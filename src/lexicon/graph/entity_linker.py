"""
LEXICON Entity Linker

Links entities in the knowledge graph and extracts relationships.
"""

import time
import json
from typing import List, Dict, Any, Optional, Tuple, Set
import logging
import re

from ..core.config import LexiconConfig
from ..core.logging import get_logger, LoggingTimer
from ..core.exceptions import ProcessingError, GraphError

from src.llm.OpenRouter import OpenRouterClient, OpenRouterConfig


class EntityLinker:
    """
    Entity linker for the knowledge graph.
    
    Links entities and extracts relationships between them.
    """
    
    def __init__(self, openrouter: OpenRouterClient, config: LexiconConfig):
        """
        Initialize the entity linker.
        
        Args:
            openrouter: OpenRouter client for LLM operations
            config: LEXICON configuration
        """
        self.openrouter = openrouter
        self.config = config
        self.logger = get_logger("graph.entity_linker")
    
    def extract_relationships(self, entities: List[Tuple[str, str]]) -> List[Dict[str, Any]]:
        """
        Extract relationships between entities.
        
        Args:
            entities: List of (entity_id, entity_text) tuples
            
        Returns:
            List of relationship dictionaries
        """
        relationships = []
        
        # Only process if we have multiple entities
        if len(entities) <= 1:
            return relationships
        
        # Limit the number of entities to avoid huge prompts
        if len(entities) > 20:
            self.logger.warning(f"Limiting relationship extraction to first 20 of {len(entities)} entities")
            entities = entities[:20]
        
        # Extract relationships using OpenRouter
        self.logger.debug(f"Extracting relationships between {len(entities)} entities")
        
        try:
            # Build prompt
            prompt = self._build_relationship_prompt(entities)
            
            # Call OpenRouter with the prompt
            model = self.config.fallback_models.get("graph_assembly", self.config.default_model)
            response = self.openrouter.simple_chat(prompt, model=model)
            
            # Parse response to extract relationships
            extracted_relationships = self._parse_relationship_response(response, entities)
            relationships.extend(extracted_relationships)
            
        except Exception as e:
            self.logger.error(f"Relationship extraction failed: {str(e)}")
        
        return relationships
    
    def _build_relationship_prompt(self, entities: List[Tuple[str, str]]) -> str:
        """
        Build prompt for relationship extraction.
        
        Args:
            entities: List of (entity_id, entity_text) tuples
            
        Returns:
            Formatted prompt
        """
        entity_list = "\n".join(f"{i+1}. {text} (ID: {entity_id})" for i, (entity_id, text) in enumerate(entities))
        
        prompt = f"""Analyze the following entities and identify meaningful relationships between them:

Entities:
{entity_list}

For each relationship you identify, include:
1. Source entity ID
2. Target entity ID
3. Relationship type
4. Confidence score (0.0-1.0)

Common relationship types include:
- "is_a" - hierarchical relationship
- "part_of" - composition relationship
- "related_to" - general association
- "same_as" - equivalence or co-reference
- "causes" - causal relationship
- "precedes" - temporal relationship
- "located_in" - spatial relationship
- "opposes" - contrasting relationship

Return your findings in this JSON format:
```json
[
  {{
    "source": "entity_id_1",
    "target": "entity_id_2",
    "type": "relationship_type",
    "confidence": 0.8,
    "metadata": {{
      "description": "Brief description of the relationship"
    }}
  }},
  ...
]
```

Only include relationships with confidence of at least 0.6. If you can't identify any clear relationships, return an empty list.
"""
        return prompt
    
    def _parse_relationship_response(self, response: str, 
                                   entities: List[Tuple[str, str]]) -> List[Dict[str, Any]]:
        """
        Parse response to extract relationships.
        
        Args:
            response: Response from OpenRouter
            entities: List of (entity_id, entity_text) tuples
            
        Returns:
            List of extracted relationships
        """
        # Extract entity IDs for validation
        entity_ids = {entity_id for entity_id, _ in entities}
        
        # Initialize results
        relationships = []
        
        try:
            # Extract JSON from response
            json_match = re.search(r'```json\s*(.*?)\s*```', response, re.DOTALL)
            if json_match:
                json_str = json_match.group(1)
            else:
                # Try to find JSON without code blocks
                json_match = re.search(r'(\[\s*\{.*\}\s*\])', response, re.DOTALL)
                if json_match:
                    json_str = json_match.group(1)
                else:
                    # Fallback to whole response if it looks like JSON
                    if response.strip().startswith('[') and response.strip().endswith(']'):
                        json_str = response
                    else:
                        return relationships  # Return empty relationships
            
            # Parse JSON
            parsed = json.loads(json_str)
            
            # Validate and collect relationships
            for item in parsed:
                # Check if required fields are present
                if "source" not in item or "target" not in item or "type" not in item:
                    continue
                    
                # Validate entity IDs
                source_id = item["source"]
                target_id = item["target"]
                
                if source_id not in entity_ids or target_id not in entity_ids:
                    continue
                
                # Check confidence threshold
                confidence = item.get("confidence", 0.0)
                if confidence < 0.6:
                    continue
                
                # Add to results
                relationships.append({
                    "source": source_id,
                    "target": target_id,
                    "type": item["type"],
                    "confidence": confidence,
                    "metadata": item.get("metadata", {"description": ""})
                })
            
        except Exception as e:
            self.logger.error(f"Failed to parse relationships: {str(e)}")
        
        return relationships
    
    def resolve_coreferences(self, text: str, entities: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        Resolve coreferences in text.
        
        Args:
            text: Text to analyze
            entities: List of entities
            
        Returns:
            List of coreferenced entities
        """
        try:
            # Build prompt for coreference resolution
            entity_list = "\n".join(f"- {e['text']}" for e in entities)
            
            prompt = f"""Resolve coreferences between pronouns and entities in this text.

Text: "{text}"

Entities:
{entity_list}

For each pronoun in the text, identify which entity it refers to.
Return your analysis in this JSON format:
```json
[
  {{
    "pronoun": "he",
    "position": 42,
    "entity": "John Smith",
    "confidence": 0.9
  }},
  ...
]
```

Only include clear coreferences with confidence of at least 0.7.
"""
            
            # Call OpenRouter with the prompt
            model = self.config.fallback_models.get("entity_extraction", self.config.default_model)
            response = self.openrouter.simple_chat(prompt, model=model)
            
            # Parse response
            json_match = re.search(r'```json\s*(.*?)\s*```', response, re.DOTALL)
            if json_match:
                coreferences = json.loads(json_match.group(1))
                return coreferences
            else:
                # Try to extract JSON without code blocks
                json_match = re.search(r'(\[\s*\{.*\}\s*\])', response, re.DOTALL)
                if json_match:
                    coreferences = json.loads(json_match.group(1))
                    return coreferences
            
        except Exception as e:
            self.logger.error(f"Coreference resolution failed: {str(e)}")
        
        return []
    
    def disambiguate_entities(self, entity1: Dict[str, Any], entity2: Dict[str, Any]) -> float:
        """
        Calculate similarity between two entities.
        
        Args:
            entity1: First entity
            entity2: Second entity
            
        Returns:
            Similarity score (0.0-1.0)
        """
        # Simple string similarity for now
        text1 = entity1.get("text", "").lower()
        text2 = entity2.get("text", "").lower()
        
        # Exact match
        if text1 == text2:
            return 1.0
        
        # Substring match
        if text1 in text2 or text2 in text1:
            return 0.8
        
        # Word overlap
        words1 = set(text1.split())
        words2 = set(text2.split())
        
        if words1 and words2:
            overlap = words1.intersection(words2)
            if overlap:
                return 0.5 * len(overlap) / min(len(words1), len(words2))
        
        # No match
        return 0.0 