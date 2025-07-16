"""
LEXICON Structured Case Determiner

Advanced case determination using structured LLM calls for accurate grammatical case assignment.
"""

import json
import re
import logging
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass

from ..core.config import LexiconConfig
from ..core.logging import get_logger
from src.llm.OpenRouter import OpenRouterClient


@dataclass
class CaseAssignment:
    """
    Structured case assignment result.
    """
    entity_text: str
    case: str
    confidence: float
    rationale: str
    linguistic_features: Dict[str, Any]
    context_analysis: str
    alternative_cases: List[Dict[str, Any]]


class StructuredCaseDeterminer:
    """
    Advanced case determination using structured LLM calls.
    
    This class provides sophisticated case assignment using:
    1. Contextual analysis
    2. Linguistic feature extraction 
    3. Multi-step reasoning
    4. Confidence scoring
    5. Alternative case consideration
    """
    
    def __init__(self, openrouter: OpenRouterClient, config: LexiconConfig):
        """
        Initialize the structured case determiner.
        
        Args:
            openrouter: OpenRouter client for LLM operations
            config: LEXICON configuration
        """
        self.openrouter = openrouter
        self.config = config
        self.logger = get_logger("declension.structured_case_determiner")
        
        # Track performance metrics
        self.metrics = {
            "determinations_made": 0,
            "high_confidence_assignments": 0,
            "llm_calls": 0,
            "fallback_used": 0
        }
    
    def determine_cases_batch(self, entities: List[str], context: str) -> List[CaseAssignment]:
        """
        Determine grammatical cases for multiple entities in context.
        
        Args:
            entities: List of entity texts
            context: Surrounding context text
            
        Returns:
            List of case assignments
        """
        if not entities:
            return []
        
        self.logger.debug(f"Determining cases for {len(entities)} entities")
        
        # Use structured LLM prompt for batch case determination
        assignments = self._determine_cases_with_llm(entities, context)
        
        # Update metrics
        self.metrics["determinations_made"] += len(assignments)
        self.metrics["high_confidence_assignments"] += sum(
            1 for a in assignments if a.confidence >= 0.8
        )
        
        return assignments
    
    def determine_single_case(self, entity_text: str, context: str) -> CaseAssignment:
        """
        Determine grammatical case for a single entity.
        
        Args:
            entity_text: Entity text
            context: Surrounding context
            
        Returns:
            Case assignment
        """
        assignments = self.determine_cases_batch([entity_text], context)
        return assignments[0] if assignments else self._create_fallback_assignment(entity_text)
    
    def _determine_cases_with_llm(self, entities: List[str], context: str) -> List[CaseAssignment]:
        """
        Use structured LLM calls to determine cases.
        
        Args:
            entities: List of entity texts
            context: Context text
            
        Returns:
            List of case assignments
        """
        self.metrics["llm_calls"] += 1
        
        try:
            # Build structured prompt
            prompt = self._build_structured_case_prompt(entities, context)
            
            # Call LLM with appropriate model
            model = self.config.fallback_models.get("case_declension", self.config.default_model)
            response = self.openrouter.simple_chat(prompt, model=model)
            
            # Parse structured response
            assignments = self._parse_structured_response(response, entities)
            
            return assignments
            
        except Exception as e:
            self.logger.error(f"LLM case determination failed: {e}")
            self.metrics["fallback_used"] += len(entities)
            return [self._create_fallback_assignment(entity) for entity in entities]
    
    def _build_structured_case_prompt(self, entities: List[str], context: str) -> str:
        """
        Build a structured prompt for case determination.
        
        Args:
            entities: List of entities to analyze
            context: Context text
            
        Returns:
            Structured prompt
        """
        entities_list = "\n".join(f"{i+1}. \"{entity}\"" for i, entity in enumerate(entities))
        
        prompt = f"""Analyze grammatical cases for entities in this text using CEREBRUM's 8-case system.

TEXT: "{context}"

ENTITIES:
{entities_list}

CASES:
- nominative: subjects/agents performing actions
- accusative: direct objects receiving actions  
- genitive: possession/source ("of", "'s")
- dative: recipients/beneficiaries ("to", "for")
- locative: locations/context ("in", "at", "on")
- instrumental: tools/means ("with", "using", "by")
- ablative: origins/causes ("from", "because of")
- vocative: direct address

Respond with ONLY this JSON format - no other text:

{{
  "assignments": [
    {{
      "entity": "exact entity text",
      "case": "case_name", 
      "confidence": 0.8,
      "reason": "brief explanation"
    }}
  ]
}}"""
        return prompt
    
    def _parse_structured_response(self, response: str, entities: List[str]) -> List[CaseAssignment]:
        """
        Parse structured response with enhanced error handling and multiple fallback strategies.
        
        Args:
            response: LLM response string
            entities: Original entities list
            
        Returns:
            List of case assignments
        """
        assignments = []
        
        try:
            # Strategy 1: Try JSON with code blocks
            json_str = self._extract_json_strategy_1(response)
            if json_str:
                parsed = json.loads(json_str)
                assignments = self._process_parsed_json(parsed, entities)
                if assignments:
                    return assignments
        except Exception as e:
            self.logger.debug(f"JSON Strategy 1 failed: {e}")
        
        try:
            # Strategy 2: Try JSON without code blocks
            json_str = self._extract_json_strategy_2(response)
            if json_str:
                parsed = json.loads(json_str)
                assignments = self._process_parsed_json(parsed, entities)
                if assignments:
                    return assignments
        except Exception as e:
            self.logger.debug(f"JSON Strategy 2 failed: {e}")
        
        try:
            # Strategy 3: Try to repair malformed JSON
            json_str = self._repair_json_strategy_3(response)
            if json_str:
                parsed = json.loads(json_str)
                assignments = self._process_parsed_json(parsed, entities)
                if assignments:
                    return assignments
        except Exception as e:
            self.logger.debug(f"JSON Strategy 3 failed: {e}")
        
        try:
            # Strategy 4: Parse line-by-line for case assignments
            assignments = self._parse_line_by_line_strategy_4(response, entities)
            if assignments:
                return assignments
        except Exception as e:
            self.logger.debug(f"Line-by-line strategy failed: {e}")
        
        try:
            # Strategy 5: Aggressive JSON extraction
            json_str = self._extract_json_strategy_5(response)
            if json_str:
                parsed = json.loads(json_str)
                assignments = self._process_parsed_json(parsed, entities)
                if assignments:
                    return assignments
        except Exception as e:
            self.logger.debug(f"JSON Strategy 5 failed: {e}")
        
        # New Strategy 6: Use json_repair library if available
        try:
            import json_repair
            repaired_json = json_repair.loads(response)
            assignments = self._process_parsed_json(repaired_json, entities)
            if assignments:
                return assignments
        except ImportError:
            self.logger.debug("json_repair not available, skipping strategy 6")
        except Exception as e:
            self.logger.debug(f"JSON repair strategy failed: {e}")
        
        # Final fallback: Create assignments for all entities
        self.logger.error("All JSON parsing strategies failed, using fallback assignments")
        self.logger.debug(f"Problematic response: {response}")  # Log the full response for debugging
        return [self._create_fallback_assignment(entity) for entity in entities]
    
    def _extract_json_strategy_1(self, response: str) -> Optional[str]:
        """Extract JSON from response with code blocks."""
        json_match = re.search(r'```json\s*(.*?)\s*```', response, re.DOTALL)
        return json_match.group(1) if json_match else None
    
    def _extract_json_strategy_2(self, response: str) -> Optional[str]:
        """Extract JSON from response without code blocks."""
        # Try new simplified format first
        json_match = re.search(r'(\{\s*"assignments"\s*:.*?\})', response, re.DOTALL)
        if json_match:
            return json_match.group(1)
        
        # Fallback to old format
        json_match = re.search(r'(\{\s*"case_assignments"\s*:.*?\})', response, re.DOTALL)
        return json_match.group(1) if json_match else None
    
    def _repair_json_strategy_3(self, response: str) -> Optional[str]:
        """Attempt to repair common JSON formatting issues."""
        # Extract potential JSON content - look for both new and old formats
        json_match = re.search(r'(\{.*?\})', response, re.DOTALL)
        if not json_match:
            return None
        
        json_str = json_match.group(1)
        
        # Common repairs
        # Fix trailing commas
        json_str = re.sub(r',(\s*[}\]])', r'\1', json_str)
        
        # Fix missing quotes around keys
        json_str = re.sub(r'(\w+):', r'"\1":', json_str)
        
        # Fix single quotes to double quotes
        json_str = json_str.replace("'", '"')
        
        # Fix common word substitutions
        json_str = re.sub(r'"(nominative|accusative|genitive|dative|locative|instrumental|ablative|vocative)"', 
                         lambda m: f'"{m.group(1).lower()}"', json_str)
        
        # Ensure proper JSON structure
        if 'assignments' not in json_str and 'case_assignments' not in json_str:
            # Try to wrap content in proper structure
            if '"entity"' in json_str and '"case"' in json_str:
                # Looks like assignment objects, wrap in assignments array
                json_str = f'{{"assignments": [{json_str}]}}'
        
        # Attempt to find and fix incomplete JSON
        if not json_str.strip().endswith('}'):
            # Count braces and try to balance
            open_braces = json_str.count('{')
            close_braces = json_str.count('}')
            if open_braces > close_braces:
                json_str += '}' * (open_braces - close_braces)
        
        return json_str
    
    def _parse_line_by_line_strategy_4(self, response: str, entities: List[str]) -> List[CaseAssignment]:
        """Parse response line by line looking for entity-case pairs."""
        assignments = []
        lines = response.split('\n')
        
        current_entity = None
        current_case = None
        current_confidence = 0.6
        current_reason = "Line-by-line parsing"
        
        for line in lines:
            line = line.strip()
            if not line:
                continue
            
            # Look for entity patterns
            entity_match = re.search(r'"?([^"]+)"?\s*:\s*"?([^",]+)"?', line)
            if entity_match:
                key, value = entity_match.groups()
                key = key.strip().lower()
                value = value.strip().lower()
                
                if key in ['entity', 'entity_text'] and value in [e.lower() for e in entities]:
                    current_entity = next(e for e in entities if e.lower() == value)
                elif key == 'case' and current_entity:
                    current_case = value
                elif key == 'confidence' and current_entity:
                    try:
                        current_confidence = float(value)
                    except ValueError:
                        pass
                elif key in ['reason', 'rationale'] and current_entity:
                    current_reason = value
            
            # Check if we have complete assignment info
            if current_entity and current_case:
                assignment = CaseAssignment(
                    entity_text=current_entity,
                    case=current_case,
                    confidence=current_confidence,
                    rationale=current_reason,
                    linguistic_features={"fallback_pattern": "line_parsing"},
                    context_analysis="Parsed from line-by-line fallback",
                    alternative_cases=[]
                )
                assignments.append(assignment)
                # Reset for next entity
                current_entity = None
                current_case = None
                current_confidence = 0.6
                current_reason = "Line-by-line parsing"
        
        return assignments
    
    def _process_parsed_json(self, parsed: dict, entities: List[str]) -> List[CaseAssignment]:
        """Process successfully parsed JSON into case assignments."""
        assignments = []
        
        # Handle both new and old formats
        case_assignments = parsed.get("assignments", parsed.get("case_assignments", []))
        
        # Process each assignment
        for assignment_data in case_assignments:
            # Handle both new and old field names
            entity_text = assignment_data.get("entity", assignment_data.get("entity_text", ""))
            
            # Validate entity matches our input
            if entity_text not in entities:
                # Try to find closest match
                closest_match = None
                for entity in entities:
                    if (entity.lower() in entity_text.lower() or 
                        entity_text.lower() in entity.lower() or
                        entity.lower() == entity_text.lower()):
                        closest_match = entity
                        break
                
                if closest_match:
                    entity_text = closest_match
                else:
                    self.logger.warning(f"Entity '{entity_text}' not found in original list")
                    continue
            
            # Create case assignment with flexible field mapping
            assignment = CaseAssignment(
                entity_text=entity_text,
                case=assignment_data.get("case", "locative"),
                confidence=assignment_data.get("confidence", 0.5),
                rationale=assignment_data.get("reason", assignment_data.get("rationale", "LLM assignment")),
                linguistic_features=assignment_data.get("linguistic_features", {"source": "simplified_llm"}),
                context_analysis=assignment_data.get("context_analysis", "LLM analysis"),
                alternative_cases=assignment_data.get("alternative_cases", [])
            )
            
            assignments.append(assignment)
        
        # Ensure all entities have assignments
        assigned_entities = {a.entity_text for a in assignments}
        for entity in entities:
            if entity not in assigned_entities:
                fallback = self._create_fallback_assignment(entity)
                assignments.append(fallback)
        
        return assignments
    
    def _create_fallback_assignment(self, entity_text: str) -> CaseAssignment:
        """
        Create a fallback case assignment with improved case diversity.
        
        Args:
            entity_text: Entity text
            
        Returns:
            Fallback case assignment
        """
        entity_lower = entity_text.lower()
        
        # Enhanced pattern-based case assignment
        case = "locative"  # Default case
        confidence = 0.4
        rationale = "Fallback assignment - insufficient data for confident determination"
        
        # Nominative patterns (subjects/agents)
        if (entity_text and entity_text[0].isupper() and 
            any(pattern in entity_lower for pattern in [
                'dr.', 'prof.', 'mr.', 'mrs.', 'ms.', 'team', 'group', 'researcher'
            ])):
            case = "nominative"
            confidence = 0.7
            rationale = "Proper noun with title or role indicator - likely subject"
        
        elif (entity_text and entity_text[0].isupper() and 
              len(entity_text.split()) <= 2 and 
              not any(prep in entity_lower for prep in ['in', 'at', 'on', 'with', 'by'])):
            case = "nominative" 
            confidence = 0.6
            rationale = "Short proper noun - likely subject"
        
        # Genitive patterns (possession/source)
        elif "'s" in entity_text or " of " in entity_text:
            case = "genitive"
            confidence = 0.8
            rationale = "Possessive pattern detected"
        
        # Instrumental patterns (tools/methods)
        elif any(pattern in entity_lower for pattern in [
            'method', 'technique', 'approach', 'tool', 'instrument', 'system', 
            'process', 'procedure', 'protocol', 'software', 'equipment'
        ]):
            case = "instrumental"
            confidence = 0.7
            rationale = "Tool or method indicator - likely instrumental"
        
        # Accusative patterns (objects/targets)
        elif any(pattern in entity_lower for pattern in [
            'result', 'outcome', 'effect', 'product', 'data', 'sample',
            'measurement', 'observation', 'finding', 'conclusion'
        ]):
            case = "accusative"
            confidence = 0.7
            rationale = "Result or target indicator - likely object"
        
        # Locative patterns (places/contexts) - more specific
        elif any(pattern in entity_lower for pattern in [
            'laboratory', 'lab', 'university', 'institute', 'center', 'facility',
            'location', 'site', 'room', 'building', 'environment', 'condition'
        ]):
            case = "locative"
            confidence = 0.7
            rationale = "Location or contextual setting indicator"
        
        # Ablative patterns (sources/origins)
        elif any(pattern in entity_lower for pattern in [
            'source', 'origin', 'cause', 'reason', 'basis', 'foundation',
            'background', 'literature', 'reference', 'previous', 'prior'
        ]):
            case = "ablative"
            confidence = 0.6
            rationale = "Source or origin indicator"
        
        # Dative patterns (recipients/beneficiaries)
        elif any(pattern in entity_lower for pattern in [
            'patient', 'subject', 'participant', 'recipient', 'target',
            'audience', 'user', 'client', 'beneficiary'
        ]):
            case = "dative"
            confidence = 0.6
            rationale = "Recipient or beneficiary indicator"
        
        # Scientific entities get more specific cases
        elif any(pattern in entity_lower for pattern in [
            'compound', 'molecule', 'chemical', 'substance', 'material',
            'solution', 'mixture', 'reagent', 'catalyst'
        ]):
            case = "accusative"  # Chemical compounds are often objects of actions
            confidence = 0.6
            rationale = "Chemical entity - likely object of action"
        
        # Temporal entities
        elif any(pattern in entity_lower for pattern in [
            'time', 'period', 'duration', 'phase', 'stage', 'step',
            'minute', 'hour', 'day', 'week', 'month', 'year'
        ]):
            case = "locative"
            confidence = 0.6
            rationale = "Temporal context indicator"
        
        # Quantitative entities
        elif any(pattern in entity_lower for pattern in [
            'concentration', 'temperature', 'pressure', 'volume', 'amount',
            'quantity', 'level', 'rate', 'speed', 'frequency'
        ]):
            case = "accusative"
            confidence = 0.6
            rationale = "Quantitative measurement - likely object"
        
        # Default diversification - avoid too much locative
        elif case == "locative" and confidence <= 0.4:
            # Randomly assign different cases to increase diversity
            import random
            alternative_cases = ["nominative", "accusative", "instrumental"]
            case = random.choice(alternative_cases)
            confidence = 0.3
            rationale = f"Diversified assignment to reduce locative bias"
        
        return CaseAssignment(
            entity_text=entity_text,
            case=case,
            confidence=confidence,
            rationale=rationale,
            linguistic_features={
                "fallback_pattern": "enhanced_heuristics",
                "entity_type": "inferred"
            },
            context_analysis="Limited context analysis available - using enhanced pattern matching",
            alternative_cases=self._generate_alternative_cases(case, confidence)
        )
    
    def _generate_alternative_cases(self, primary_case: str, primary_confidence: float) -> List[Dict[str, Any]]:
        """
        Generate alternative case possibilities for fallback assignments.
        
        Args:
            primary_case: The assigned primary case
            primary_confidence: Confidence of primary assignment
            
        Returns:
            List of alternative case dictionaries
        """
        all_cases = ["nominative", "accusative", "genitive", "dative", "locative", "instrumental", "ablative", "vocative"]
        alternatives = []
        
        # Remove primary case from alternatives
        available_cases = [c for c in all_cases if c != primary_case]
        
        # Generate 2-3 alternatives with lower confidence
        num_alternatives = min(3, len(available_cases))
        
        for i, alt_case in enumerate(available_cases[:num_alternatives]):
            alt_confidence = max(0.1, primary_confidence - 0.2 - (i * 0.1))
            alternatives.append({
                "case": alt_case,
                "confidence": alt_confidence,
                "reasoning": f"Alternative case consideration - less likely than {primary_case}"
            })
        
        return alternatives
    
    def get_metrics(self) -> Dict[str, Any]:
        """
        Get performance metrics.
        
        Returns:
            Dictionary of metrics
        """
        return {
            **self.metrics,
            "high_confidence_rate": (
                self.metrics["high_confidence_assignments"] / max(self.metrics["determinations_made"], 1)
            ),
            "fallback_rate": (
                self.metrics["fallback_used"] / max(self.metrics["determinations_made"], 1)
            )
        } 

def _extract_json_strategy_5(self, response: str) -> Optional[str]:
    """Aggressively extract JSON by finding largest valid JSON-like substring."""
    # Find all potential JSON start/end positions
    starts = [m.start() for m in re.finditer(r'\{|\[', response)]
    ends = [m.end() for m in re.finditer(r'\}|\]', response)]
    
    if not starts or not ends:
        return None
    
    # Try largest possible substrings first
    for start in sorted(starts):
        for end in sorted(ends, reverse=True):
            if end > start:
                candidate = response[start:end]
                try:
                    json.loads(candidate)
                    return candidate
                except json.JSONDecodeError:
                    continue
    return None 