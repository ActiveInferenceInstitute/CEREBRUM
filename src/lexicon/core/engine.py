"""
LEXICON Engine - Main Pipeline Orchestrator

This module provides the main LEXICON engine that orchestrates the complete
pipeline from ingestion to graph generation.
"""

import os
import time
import asyncio
from typing import Dict, List, Any, Optional, Tuple, Union
from pathlib import Path
import json
import logging
from datetime import datetime
import uuid
import traceback
import numpy as np
from dotenv import load_dotenv

# Import LEXICON components
from .config import LexiconConfig, get_default_config
from .logging import setup_logging, get_logger, LoggingTimer
from .exceptions import LexiconError, InputError, ProcessingError

# Import OpenRouter integration
from src.llm.OpenRouter.openrouter import OpenRouterClient, OpenRouterConfig
from src.llm.config import get_model_name, get_api_key

# Load environment variables from top-level .env file
env_path = Path(__file__).resolve().parent.parent.parent.parent / '.env'
load_dotenv(dotenv_path=env_path)

# Configure file logging
def log_visualization_files(output_dir: Path) -> None:
    """
    Log information about generated visualization files.
    
    Args:
        output_dir: Directory containing visualizations
    """
    logger = logging.getLogger("lexicon_engine")
    
    vis_dir = output_dir / "visualizations"
    if not vis_dir.exists():
        logger.warning(f"No visualization directory found at: {vis_dir}")
        return
        
    logger.info(f"Scanning for visualization files in: {vis_dir}")
    
    # Count files by type
    file_counts = {
        "png": 0,
        "gif": 0,
        "jpg": 0,
        "svg": 0,
        "html": 0,
        "other": 0
    }
    
    # Track total size
    total_size_kb = 0
    
    # Recursively find all files
    for path in vis_dir.glob("**/*"):
        if path.is_file():
            # Get file extension
            ext = path.suffix.lower().lstrip('.')
            if ext in file_counts:
                file_counts[ext] += 1
            else:
                file_counts["other"] += 1
                
            # Get file size
            size_kb = path.stat().st_size / 1024
            total_size_kb += size_kb
            
            # Log individual file
            logger.debug(f"Visualization file: {path.relative_to(output_dir)} ({size_kb:.2f} KB)")
    
    # Log summary
    logger.info(f"Visualization summary:")
    logger.info(f"  - PNG files: {file_counts['png']}")
    logger.info(f"  - GIF animations: {file_counts['gif']}")
    logger.info(f"  - JPG files: {file_counts['jpg']}")
    logger.info(f"  - SVG files: {file_counts['svg']}")
    logger.info(f"  - HTML files: {file_counts['html']}")
    logger.info(f"  - Other files: {file_counts['other']}")
    logger.info(f"  - Total size: {total_size_kb:.2f} KB")

class LexiconEngine:
    """
    Main LEXICON pipeline orchestrator.
    
    This class coordinates the complete pipeline:
    1. Input processing (text/audio)
    2. NLP preprocessing
    3. Case declension
    4. Graph assembly
    5. Output generation
    """
    
    def __init__(self, config: Optional[LexiconConfig] = None):
        """
        Initialize the LEXICON engine.
        
        Args:
            config: Configuration object, or None to use defaults
        """
        # Configure environment first to handle protobuf issues
        from .environment import configure_protobuf_environment, configure_transformers_environment
        configure_protobuf_environment()
        configure_transformers_environment()
        
        # Initialize configuration
        self.config = config or get_default_config()
        
        # Set up logging
        self.logger = setup_logging(self.config)
        
        # Initialize OpenRouter client
        self._setup_llm_client()
        
        # Track pipeline components (will be loaded as needed)
        self._nlp_preprocessor = None
        self._case_tagger = None
        self._graph_assembler = None
        self._paraphrase_generator = None
        
        self.entity_detection_strategies = [
            self._detect_named_entities,
            self._detect_contextual_entities,
            self._detect_relational_entities
        ]
        
        self.claim_detection_strategies = [
            self._detect_explicit_claims,
            self._detect_implicit_claims,
            self._detect_emotional_claims
        ]
        
        # Add grammatical case determination method
        self._determine_grammatical_case = self._create_case_determination_method()
        
        self.logger.info("LEXICON engine initialized")
    
    def _setup_llm_client(self):
        """Initialize the OpenRouter LLM client."""
        try:
            # Get API key from environment variables
            api_key = os.environ.get("OPENROUTER_API_KEY")
            if not api_key:
                raise ValueError("OPENROUTER_API_KEY environment variable is not set")
                
            # Use OpenRouter client with configuration
            router_config = OpenRouterConfig(
                api_key=api_key,
                default_model=get_model_name() or "anthropic/claude-3.5-sonnet",
                temperature=0.7,
                max_tokens=1000
            )
            self.llm_client = OpenRouterClient(router_config)
            logging.info("OpenRouter LLM client initialized successfully")
        except Exception as e:
            logging.error(f"Failed to initialize OpenRouter client: {e}")
            raise
    
    def _call_llm(self, prompt: str) -> str:
        """
        Call LLM with prompt using OpenRouter client.
        
        Args:
            prompt: Prompt text for LLM
            
        Returns:
            LLM response text
        """
        try:
            # Use OpenRouter client for LLM calls
            response = self.llm_client.simple_chat(prompt)
            return response
        except Exception as e:
            logging.error(f"LLM call failed: {e}")
            raise
    
    def _get_nlp_preprocessor(self):
        """Get or initialize NLP preprocessor."""
        if self._nlp_preprocessor is None:
            from ..nlp.preprocessor import NLPPreprocessor
            self._nlp_preprocessor = NLPPreprocessor(self.config)
            self.logger.info("NLP preprocessor initialized")
        return self._nlp_preprocessor
    
    def _get_case_tagger(self):
        """Get or initialize case tagger."""
        if self._case_tagger is None:
            from ..declension.tagger import CaseTagger
            self._case_tagger = CaseTagger(self.llm_client, self.config)
            self.logger.info("Case tagger initialized")
        return self._case_tagger
    
    def _get_graph_assembler(self):
        """Get or initialize graph assembler."""
        if self._graph_assembler is None:
            from ..graph.assembler import GraphAssembler
            self._graph_assembler = GraphAssembler(self.llm_client, self.config)
            self.logger.info("Graph assembler initialized")
        return self._graph_assembler
    
    def _get_paraphrase_generator(self):
        """Get or initialize paraphrase generator."""
        if self._paraphrase_generator is None:
            from ..paraphrase.generator import ParaphraseGenerator
            self._paraphrase_generator = ParaphraseGenerator(self.llm_client, self.config)
            self.logger.info("Paraphrase generator initialized")
        return self._paraphrase_generator
    
    def _detect_named_entities(self, text):
        """
        Detect named entities using advanced NLP techniques and structured case determination.
        
        Args:
            text (str): Input text
        
        Returns:
            list: Detected named entities with metadata
        """
        try:
            import spacy
            
            # Load spaCy model (try transformer first, fallback to smaller model)
            nlp = None
            for model_name in ["en_core_web_trf", "en_core_web_sm"]:
                try:
                    nlp = spacy.load(model_name)
                    break
                except IOError:
                    continue
            
            if nlp is None:
                self.logger.warning("No spaCy model available, falling back to structured LLM detection")
                return self._llm_entity_detection_with_structured_cases(text)
            
            doc = nlp(text)
            entities = []
            
            # Extract entities using spaCy
            entity_texts = []
            entity_metadata = {}
            
            for ent in doc.ents:
                # Get surrounding context for better case determination
                start_context = text[max(0, ent.start_char - 100):ent.start_char]
                end_context = text[ent.end_char:min(len(text), ent.end_char + 100)]
                context = f"{start_context} {ent.text} {end_context}".strip()
                
                entity_texts.append(ent.text)
                entity_metadata[ent.text] = {
                    "type": ent.label_,
                    "start_char": ent.start_char,
                    "end_char": ent.end_char,
                    "context": context,
                    "source": "spacy_ner"
                }
            
            # Use structured case determiner for batch case assignment
            if entity_texts:
                try:
                    from ..declension.structured_case_determiner import StructuredCaseDeterminer
                    case_determiner = StructuredCaseDeterminer(self.llm_client, self.config)
                    case_assignments = case_determiner.determine_cases_batch(entity_texts, text)
                    
                    # Build final entity list with structured case assignments
                    for assignment in case_assignments:
                        metadata = entity_metadata.get(assignment.entity_text, {})
                        
                        entities.append({
                            "text": assignment.entity_text,
                            "type": metadata.get("type", "ENTITY"),
                            "case": assignment.case,
                            "confidence": assignment.confidence,
                            "case_rationale": assignment.rationale,
                            "linguistic_features": assignment.linguistic_features,
                            "context_analysis": assignment.context_analysis,
                            "alternative_cases": assignment.alternative_cases,
                            "metadata": {
                                **metadata,
                                "structured_analysis": True
                            }
                        })
                except Exception as e:
                    self.logger.warning(f"Structured case determination failed: {e}, using fallback")
                    # Fallback to original method
                    for ent in doc.ents:
                        context = entity_metadata[ent.text]["context"]
                        case_info = self._determine_grammatical_case(ent.text, context)
                        
                        entities.append({
                            "text": ent.text,
                            "type": ent.label_,
                            "case": case_info["type"],
                            "confidence": case_info["confidence"],
                            "case_rationale": case_info["rationale"],
                            "metadata": entity_metadata[ent.text]
                        })
            
            return entities
            
        except ImportError:
            # Fallback to LLM-based detection with structured cases
            return self._llm_entity_detection_with_structured_cases(text)
    
    def _llm_entity_detection_with_structured_cases(self, text):
        """
        LLM-based entity detection with structured case determination.
        
        Args:
            text: Input text
            
        Returns:
            List of entities with structured case assignments
        """
        try:
            # First, extract entities using LLM
            entities = self._llm_entity_detection(text)
            
            # Then use structured case determination
            if entities:
                entity_texts = [e.get("text", "") for e in entities if e.get("text")]
                
                if entity_texts:
                    from ..declension.structured_case_determiner import StructuredCaseDeterminer
                    case_determiner = StructuredCaseDeterminer(self.llm_client, self.config)
                    case_assignments = case_determiner.determine_cases_batch(entity_texts, text)
                    
                    # Update entities with structured case information
                    assignment_map = {a.entity_text: a for a in case_assignments}
                    
                    for entity in entities:
                        entity_text = entity.get("text", "")
                        if entity_text in assignment_map:
                            assignment = assignment_map[entity_text]
                            entity.update({
                                "case": assignment.case,
                                "confidence": assignment.confidence,
                                "case_rationale": assignment.rationale,
                                "linguistic_features": assignment.linguistic_features,
                                "context_analysis": assignment.context_analysis,
                                "alternative_cases": assignment.alternative_cases,
                                "metadata": {
                                    **entity.get("metadata", {}),
                                    "structured_analysis": True
                                }
                            })
            
            return entities
            
        except Exception as e:
            self.logger.error(f"LLM entity detection with structured cases failed: {e}")
            return []
    
    def _detect_contextual_entities(self, text):
        """
        Detect contextual entities using LLM with structured case determination.
        
        Args:
            text: Input text
        
        Returns:
            list: Detected contextual entities with structured case assignments
        """
        prompt = f"""
        Extract all important entities from the following text. Include people, organizations, 
        locations, concepts, and other significant entities. For each entity, provide:
        
        1. The entity text
        2. The entity category (person, organization, location, concept, etc.)
        3. A confidence score between 0 and 1
        
        Format your response as a JSON array of objects, each with 'text', 'category', and 'confidence' fields.
        
        Text:
        {text}
        
        Entities (JSON array):
        """
        
        response = self._call_llm(prompt)
        
        # Parse response
        entities = []
        try:
            # Try to parse JSON response
            if response.strip().startswith('[') and response.strip().endswith(']'):
                # Direct JSON array
                parsed = json.loads(response)
                if isinstance(parsed, list):
                    entities = parsed
            elif response.strip().startswith('{') and response.strip().endswith('}'):
                # JSON object with entities field
                parsed = json.loads(response)
                if isinstance(parsed, dict) and 'entities' in parsed:
                    entities = parsed['entities']
            else:
                # Try to extract JSON from text
                import re
                json_match = re.search(r'\[\s*\{.*\}\s*\]', response, re.DOTALL)
                if json_match:
                    try:
                        entities = json.loads(json_match.group(0))
                    except json.JSONDecodeError:
                        pass
                
                # If still no entities, try line-by-line parsing
                if not entities:
                    lines = response.strip().split('\n')
                    current_entity = {}
                    
                    for line in lines:
                        line = line.strip()
                        if not line:
                            continue
                        
                        # Check for entity start/end
                        if line.startswith('- ') or line.startswith('* '):
                            # Save previous entity if exists
                            if current_entity and 'text' in current_entity:
                                entities.append(current_entity)
                            
                            # Start new entity
                            current_entity = {'text': line[2:].strip()}
                        elif ':' in line:
                            # Parse key-value pair
                            key, value = line.split(':', 1)
                            key = key.strip().lower()
                            value = value.strip()
                            
                            if key and value and current_entity:
                                current_entity[key] = value
                    
                    # Add final entity
                    if current_entity and 'text' in current_entity:
                        entities.append(current_entity)
        
        except Exception as e:
            self.logger.error(f"Failed to parse contextual entities: {e}")
        
        # Ensure all entities have required fields
        for entity in entities:
            if 'id' not in entity:
                entity['id'] = str(uuid.uuid4())
            if 'confidence' not in entity:
                entity['confidence'] = 0.8
            if 'category' not in entity:
                entity['category'] = 'unknown'
        
        # Apply structured case determination to contextual entities
        if entities:
            try:
                from ..declension.structured_case_determiner import StructuredCaseDeterminer
                case_determiner = StructuredCaseDeterminer(self.llm_client, self.config)
                entity_texts = [e.get("text", "") for e in entities if e.get("text")]
                
                if entity_texts:
                    case_assignments = case_determiner.determine_cases_batch(entity_texts, text)
                    assignment_map = {a.entity_text: a for a in case_assignments}
                    
                    # Update entities with structured case information
                    for entity in entities:
                        entity_text = entity.get("text", "")
                        if entity_text in assignment_map:
                            assignment = assignment_map[entity_text]
                            entity.update({
                                "case": assignment.case,
                                "confidence": min(entity.get("confidence", 0.8), assignment.confidence),
                                "case_rationale": assignment.rationale,
                                "linguistic_features": assignment.linguistic_features,
                                "context_analysis": assignment.context_analysis,
                                "alternative_cases": assignment.alternative_cases,
                                "metadata": {
                                    **entity.get("metadata", {}),
                                    "detection_strategy": "contextual",
                                    "structured_analysis": True
                                }
                            })
                        else:
                            # Fallback case assignment for entities not processed
                            entity.update({
                                "case": "locative",
                                "confidence": entity.get("confidence", 0.5),
                                "case_rationale": "Fallback assignment for contextual entity"
                            })
            except Exception as e:
                self.logger.warning(f"Structured case determination failed for contextual entities: {e}")
                # Fallback to simple case assignment
                for entity in entities:
                    entity.update({
                        "case": "locative",
                        "confidence": entity.get("confidence", 0.5),
                        "case_rationale": "Fallback assignment"
                    })
        
        return entities

    def _detect_relational_entities(self, text):
        """
        Detect entities based on their relationships in the text with structured case determination.
        
        Args:
            text (str): Input text
        
        Returns:
            list: Entities with relationship metadata and structured case assignments
        """
        prompt = f"""
        Analyze the following text and identify entities and their key relationships:
        - Identify entities and their interactions
        - Describe the nature of relationships between entities
        - Provide relationship confidence and type
        
        Text: {text}
        
        Output format (JSON):
        {{
            "entities": [
                {{
                    "text": "Entity Name",
                    "type": "Entity Type",
                    "relationships": [
                        {{
                            "related_entity": "Related Entity Name",
                            "relationship_type": "Interaction Type",
                            "confidence": 0.7
                        }},
                        ...
                    ]
                }},
                ...
            ]
        }}
        """
        
        response = self._call_llm(prompt)
        
        try:
            # Enhanced parsing with fallbacks
            extracted_entities = self._robust_json_parse(response)
            if not extracted_entities:
                self.logger.warning("Failed to parse relational entities, using fallback")
                return []
            
            # Extract entity texts for structured case determination
            if extracted_entities:
                entity_texts = [e.get("text", "") for e in extracted_entities if e.get("text")]
                
                if entity_texts:
                    try:
                        from ..declension.structured_case_determiner import StructuredCaseDeterminer
                        case_determiner = StructuredCaseDeterminer(self.llm_client, self.config)
                        case_assignments = case_determiner.determine_cases_batch(entity_texts, text)
                        assignment_map = {a.entity_text: a for a in case_assignments}
                        
                        # Build final entities with structured case assignments
                        for entity_data in extracted_entities:
                            entity_text = entity_data.get("text", "")
                            assignment = assignment_map.get(entity_text)
                            
                            if assignment:
                                entity_data.update({
                                    "case": assignment.case,
                                    "confidence": assignment.confidence,
                                    "case_rationale": assignment.rationale,
                                    "linguistic_features": assignment.linguistic_features,
                                    "context_analysis": assignment.context_analysis,
                                    "alternative_cases": assignment.alternative_cases
                                })
                            else:
                                entity_data.update({
                                    "case": "locative",
                                    "confidence": 0.5,
                                    "case_rationale": "Fallback assignment"
                                })
                        
                        return extracted_entities
                    except Exception as e:
                        self.logger.warning(f"Structured case determination failed: {e}", exc_info=True)
                        # Fallback to simple case
                        for entity_data in extracted_entities:
                            entity_data["case"] = "locative"
                            entity_data["confidence"] = 0.5
                        return extracted_entities
            
            return []
        except Exception as e:
            self.logger.error(f"Failed to parse relational entities: {e}", exc_info=True)
            self.logger.debug(f"Problematic response: {response}")
            return []

    def _robust_json_parse(self, response: str) -> List[Dict]:
        """Robust JSON parsing with multiple strategies."""
        try:
            return json.loads(response)
        except json.JSONDecodeError:
            try:
                import json_repair
                return json_repair.loads(response)
            except (ImportError, Exception):
                self.logger.warning("json_repair not available or failed")
                # Fallback to manual repair
                response = re.sub(r',(\s*[}\]])', r'\1', response)  # Fix trailing commas
                response = response.replace("'", '"')  # Fix single quotes
                try:
                    return json.loads(response)
                except json.JSONDecodeError:
                    return []
    
    def _detect_explicit_claims(self, text):
        """
        Detect explicit claims in the text, including scientific methodology statements.
        
        Args:
            text (str): Input text
        
        Returns:
            list: Explicit claims with metadata
        """
        # Determine if this is scientific/technical text
        is_scientific = self._is_scientific_text(text)
        
        if is_scientific:
            prompt = f"""
            Identify explicit claims and methodological statements in the following scientific text:
            - Look for clear statements of fact, methodology, or experimental procedures
            - Include statements about what was done, how it was done, and what was observed
            - Methodological statements like "were synthesized using", "was performed at", "resulted in"
            - Experimental observations and results
            - Assess the polarity and confidence of each claim
            
            Text: {text}
            
            Output format (JSON):
            [
                {{
                    "text": "Claim or methodological statement",
                    "type": "factual|methodological|observational|conclusive",
                    "polarity": "positive|negative|neutral",
                    "confidence": 0.8,
                    "case": "accusative",
                    "supporting_evidence": "Brief context",
                    "scientific_category": "method|result|observation|conclusion"
                }},
                ...
            ]
            """
        else:
            prompt = f"""
            Identify explicit claims in the following text:
            - Look for clear statements of fact or opinion
            - Assess the polarity and confidence of each claim
            - Consider the context and implications
            
            Text: {text}
            
            Output format (JSON):
            [
                {{
                    "text": "Claim statement",
                    "type": "factual|opinion|assertion",
                    "polarity": "positive|negative|neutral",
                    "confidence": 0.8,
                    "case": "accusative",
                    "supporting_evidence": "Brief context"
                }},
                ...
            ]
            """
        
        response = self._call_llm(prompt)
        
        try:
            # Try to parse JSON response
            if response.strip().startswith('[') and response.strip().endswith(']'):
                explicit_claims = json.loads(response)
            elif response.strip().startswith('{') and response.strip().endswith('}'):
                parsed = json.loads(response)
                explicit_claims = parsed.get('claims', [])
            else:
                # Try to extract JSON from text
                import re
                json_match = re.search(r'\[\s*\{.*\}\s*\]', response, re.DOTALL)
                if json_match:
                    explicit_claims = json.loads(json_match.group(0))
                else:
                    explicit_claims = []
            
            # Ensure all claims have required fields
            for claim in explicit_claims:
                if 'id' not in claim:
                    claim['id'] = str(uuid.uuid4())
                if 'confidence' not in claim:
                    claim['confidence'] = 0.7
                if 'case' not in claim:
                    claim['case'] = 'accusative'  # Claims are typically accusative (objects of assertion)
                if 'type' not in claim:
                    claim['type'] = 'factual'
            
            return explicit_claims
            
        except json.JSONDecodeError as e:
            self.logger.warning(f"Failed to parse claims JSON: {e}")
            return []
    
    def _is_scientific_text(self, text):
        """
        Determine if text appears to be scientific/technical.
        
        Args:
            text (str): Input text
            
        Returns:
            bool: True if scientific text detected
        """
        scientific_indicators = [
            # Methodology indicators
            'were synthesized', 'was performed', 'was conducted', 'was carried out',
            'was analyzed', 'was measured', 'was observed', 'was recorded',
            'using', 'via', 'through', 'by means of',
            
            # Scientific vocabulary
            'experiment', 'method', 'procedure', 'protocol', 'analysis',
            'measurement', 'observation', 'data', 'results', 'findings',
            'temperature', 'concentration', 'solution', 'sample',
            
            # Units and measurements
            '°C', 'mg/mL', 'μg', 'rpm', 'nm', 'pH', 'min', 'hrs',
            
            # Chemical/biological terms
            'synthesis', 'characterization', 'purification', 'extraction',
            'culture', 'medium', 'buffer', 'reagent', 'solvent'
        ]
        
        text_lower = text.lower()
        matches = sum(1 for indicator in scientific_indicators if indicator in text_lower)
        
        # If more than 3 scientific indicators, consider it scientific text
        return matches >= 3
    
    def _detect_implicit_claims(self, text):
        """
        Detect implicit claims that are not directly stated.
        
        Args:
            text (str): Input text
        
        Returns:
            list: Implicit claims with metadata
        """
        prompt = f"""
        Identify implicit claims in the following text:
        - Look for underlying meanings and unstated assumptions
        - Infer claims from context and subtext
        - Assess the potential impact of these implicit claims
        
        Text: {text}
        
        Output format (JSON):
        [
            {{
                "text": "Inferred claim",
                "type": "implicit",
                "polarity": "positive|negative|neutral",
                "confidence": 0.6,
                "inference_rationale": "How this claim was derived"
            }},
            ...
        ]
        """
        
        response = self._call_llm(prompt)
        
        try:
            implicit_claims = json.loads(response)
            return implicit_claims
        except json.JSONDecodeError:
            return []
    
    def _detect_emotional_claims(self, text):
        """
        Detect emotional claims and underlying sentiments.
        
        Args:
            text (str): Input text
            
        Returns:
            list: Emotional claims with metadata
        """
        prompt = f"""
        Analyze the emotional landscape of the text:
        - Identify emotional claims and sentiments
        - Assess the emotional intensity and context
        - Provide insights into underlying emotional states
        
        Text: {text}
        
        Output format (JSON):
        [
            {{
                "text": "Emotional claim or sentiment",
                "emotion_type": "primary emotion",
                "intensity": 0.7,
                "context": "Emotional context",
                "polarity": "positive|negative|neutral"
            }},
            ...
        ]
        """
        
        response = self._call_llm(prompt)
        
        try:
            emotional_claims = json.loads(response)
            return emotional_claims
        except json.JSONDecodeError:
            return []
    
    def process_text(self, text, metadata=None):
        """
        Process text content through the LEXICON pipeline.
        
        Args:
            text: Text content to process
            metadata: Optional metadata
            
        Returns:
            Processing result
        """
        try:
            self.logger.info(f"Processing text with {len(text)} characters", extra={"text_length": len(text)})
            
            # Initialize metadata if not provided
            if metadata is None:
                metadata = {}
            
            # Initialize result structure
            result = {
                "status": "success",
                "session_id": metadata.get("session_id", str(uuid.uuid4())),
                "timestamp": datetime.now().isoformat(),
                "metadata": metadata,
                "stats": {
                    "nodes": 0,
                    "edges": 0,
                    "entities": 0,
                    "claims": 0
                },
                "graph": {
                    "nodes": [],
                    "edges": []
                }
            }
            
            # Detect entities with timing and logging
            self.logger.info("Detecting entities")
            entities_start = time.time()
            entities = []
            for strategy in self.entity_detection_strategies:
                self.logger.debug(f"Applying entity detection strategy: {strategy.__name__}")
                try:
                    detected = strategy(text)
                    if detected:
                        entities.extend(detected)
                except Exception as e:
                    self.logger.error(f"Entity detection strategy {strategy.__name__} failed: {e}", exc_info=True)
                    continue
            entities_duration = time.time() - entities_start
            self.logger.info(f"Detected {len(entities)} entities in {entities_duration:.2f}s", extra={"entities_detected": len(entities), "duration": entities_duration})
            
            # Detect claims with timing and logging
            self.logger.info("Detecting claims")
            claims_start = time.time()
            claims = []
            for strategy in self.claim_detection_strategies:
                self.logger.debug(f"Applying claim detection strategy: {strategy.__name__}")
                try:
                    detected = strategy(text)
                    if detected:
                        claims.extend(detected)
                except Exception as e:
                    self.logger.error(f"Claim detection strategy {strategy.__name__} failed: {e}", exc_info=True)
                    continue
            claims_duration = time.time() - claims_start
            self.logger.info(f"Detected {len(claims)} claims in {claims_duration:.2f}s", extra={"claims_detected": len(claims), "duration": claims_duration})
            
            # Build graph
            self.logger.info("Building knowledge graph")
            graph_start = time.time()
            graph = self._build_knowledge_graph(entities, claims, text)
            graph_duration = time.time() - graph_start
            self.logger.info(f"Built graph with {len(graph['nodes'])} nodes and {len(graph['edges'])} edges in {graph_duration:.2f}s", extra={"nodes": len(graph["nodes"]), "edges": len(graph["edges"]), "duration": graph_duration})
            
            # Validate
            if not result["entities"] or not result["claims"] or not result["graph"]["nodes"]:
                self.logger.warning("Empty results detected", extra={"warning": "empty_output"})
            
            # Update result
            result["entities"] = entities
            result["claims"] = claims
            result["graph"] = graph
            result["stats"] = {
                "nodes": len(graph["nodes"]),
                "edges": len(graph["edges"]),
                "entities": len(entities),
                "claims": len(claims)
            }
            
            return result
            
        except Exception as e:
            self.logger.error(f"Error processing text: {e}", exc_info=True)
            return {
                "status": "error",
                "error": str(e),
                "session_id": metadata.get("session_id", str(uuid.uuid4())),
                "timestamp": datetime.now().isoformat(),
                "metadata": metadata
            }
    
    def _build_knowledge_graph(self, entities, claims, text=None):
        """
        Build knowledge graph from entities and claims.
        
        Args:
            entities: List of entities
            claims: List of claims
            text: Original text (optional)
            
        Returns:
            Knowledge graph
        """
        # Initialize graph
        graph = {
            "nodes": [],
            "edges": []
        }
        
        # Track node IDs for edge creation
        entity_nodes = {}
        claim_nodes = {}
        
        # Add entities as nodes
        for entity in entities:
            entity_id = entity.get("id", str(uuid.uuid4()))
            node = {
                "id": entity_id,
                "label": entity.get("text", ""),
                "type": "entity",
                "category": entity.get("type", "unknown"),
                "case": entity.get("case", "locative"),
                "case_rationale": entity.get("case_rationale", ""),
                "confidence": entity.get("confidence", 1.0),
                "data": entity
            }
            graph["nodes"].append(node)
            entity_nodes[entity.get("text", "").lower()] = entity_id
        
        # Add claims as nodes
        for claim in claims:
            claim_id = claim.get("id", str(uuid.uuid4()))
            node = {
                "id": claim_id,
                "label": claim.get("text", ""),
                "type": "claim",
                "polarity": claim.get("polarity", "neutral"),
                "confidence": claim.get("confidence", 1.0),
                "data": claim
            }
            graph["nodes"].append(node)
            claim_nodes[claim.get("text", "").lower()] = claim_id
        
        # Create relationships between entities and claims
        self._create_entity_claim_relationships(graph, entities, claims, entity_nodes, claim_nodes)
        
        # Create relationships between entities based on co-occurrence
        self._create_entity_cooccurrence_relationships(graph, entities, entity_nodes, text)
        
        # Create relationships based on grammatical cases
        self._create_case_based_relationships(graph, entities, entity_nodes)
        
        return graph
    
    def _create_entity_claim_relationships(self, graph, entities, claims, entity_nodes, claim_nodes):
        """
        Create relationships between entities and claims.
        
        Args:
            graph: Graph to add edges to
            entities: List of entities
            claims: List of claims
            entity_nodes: Mapping of entity text to node IDs
            claim_nodes: Mapping of claim text to node IDs
        """
        for claim in claims:
            claim_text = claim.get("text", "").lower()
            claim_id = claim_nodes.get(claim_text)
            
            if not claim_id:
                continue
                
            # Find entities mentioned in this claim
            for entity in entities:
                entity_text = entity.get("text", "").lower()
                entity_id = entity_nodes.get(entity_text)
                
                if not entity_id:
                    continue
                
                # Check if entity is mentioned in claim
                if entity_text in claim_text or any(word in claim_text for word in entity_text.split()):
                    edge_id = f"{entity_id}_{claim_id}_mentions"
                    edge = {
                        "id": edge_id,
                        "source": entity_id,
                        "target": claim_id,
                        "type": "mentions",
                        "case": entity.get("case", "locative"),
                        "weight": min(entity.get("confidence", 0.5), claim.get("confidence", 0.5))
                    }
                    graph["edges"].append(edge)
    
    def _create_entity_cooccurrence_relationships(self, graph, entities, entity_nodes, text):
        """
        Create relationships between entities based on co-occurrence in text.
        
        Args:
            graph: Graph to add edges to
            entities: List of entities
            entity_nodes: Mapping of entity text to node IDs
            text: Original text
        """
        if not text:
            return
        
        # Create sentence-level co-occurrence relationships
        sentences = text.split('. ')
        
        for sentence in sentences:
            sentence_lower = sentence.lower()
            sentence_entities = []
            
            # Find entities in this sentence
            for entity in entities:
                entity_text = entity.get("text", "").lower()
                if entity_text in sentence_lower:
                    sentence_entities.append(entity)
            
            # Create relationships between co-occurring entities
            for i, entity1 in enumerate(sentence_entities):
                for j, entity2 in enumerate(sentence_entities[i+1:], i+1):
                    entity1_text = entity1.get("text", "").lower()
                    entity2_text = entity2.get("text", "").lower()
                    
                    entity1_id = entity_nodes.get(entity1_text)
                    entity2_id = entity_nodes.get(entity2_text)
                    
                    if entity1_id and entity2_id and entity1_id != entity2_id:
                        # Determine relationship type based on cases
                        rel_type = self._determine_relationship_type(entity1, entity2)
                        
                        edge_id = f"{entity1_id}_{entity2_id}_{rel_type}"
                        edge = {
                            "id": edge_id,
                            "source": entity1_id,
                            "target": entity2_id,
                            "type": rel_type,
                            "case": "locative",  # Default case for relationships
                            "weight": 0.7
                        }
                        graph["edges"].append(edge)
    
    def _create_case_based_relationships(self, graph, entities, entity_nodes):
        """
        Create relationships between entities based on their grammatical cases.
        
        Args:
            graph: Graph to add edges to
            entities: List of entities  
            entity_nodes: Mapping of entity text to node IDs
        """
        # Group entities by case
        entities_by_case = {}
        for entity in entities:
            case = entity.get("case", "locative")
            if case not in entities_by_case:
                entities_by_case[case] = []
            entities_by_case[case].append(entity)
        
        # Create relationships between nominative and accusative entities (subject-object)
        if "nominative" in entities_by_case and "accusative" in entities_by_case:
            for nom_entity in entities_by_case["nominative"]:
                for acc_entity in entities_by_case["accusative"]:
                    nom_id = entity_nodes.get(nom_entity.get("text", "").lower())
                    acc_id = entity_nodes.get(acc_entity.get("text", "").lower())
                    
                    if nom_id and acc_id and nom_id != acc_id:
                        edge_id = f"{nom_id}_{acc_id}_acts_upon"
                        edge = {
                            "id": edge_id,
                            "source": nom_id,
                            "target": acc_id,
                            "type": "acts_upon",
                            "case": "accusative",
                            "weight": 0.8
                        }
                        graph["edges"].append(edge)
        
        # Create relationships between genitive and other entities (possession)
        if "genitive" in entities_by_case:
            for gen_entity in entities_by_case["genitive"]:
                for other_case, case_entities in entities_by_case.items():
                    if other_case == "genitive":
                        continue
                    
                    for other_entity in case_entities:
                        gen_id = entity_nodes.get(gen_entity.get("text", "").lower())
                        other_id = entity_nodes.get(other_entity.get("text", "").lower())
                        
                        if gen_id and other_id and gen_id != other_id:
                            edge_id = f"{gen_id}_{other_id}_possesses"
                            edge = {
                                "id": edge_id,
                                "source": gen_id,
                                "target": other_id,
                                "type": "possesses",
                                "case": "genitive",
                                "weight": 0.7
                            }
                            graph["edges"].append(edge)
    
    def _determine_relationship_type(self, entity1, entity2):
        """
        Determine the type of relationship between two entities based on their cases.
        
        Args:
            entity1: First entity
            entity2: Second entity
            
        Returns:
            str: Relationship type
        """
        case1 = entity1.get("case", "locative")
        case2 = entity2.get("case", "locative")
        
        # Define relationship patterns
        if case1 == "nominative" and case2 == "accusative":
            return "acts_upon"
        elif case1 == "accusative" and case2 == "nominative":
            return "acted_upon_by"
        elif case1 == "genitive" or case2 == "genitive":
            return "possesses"
        elif case1 == "dative" or case2 == "dative":
            return "receives_from"
        elif case1 == "instrumental" or case2 == "instrumental":
            return "uses"
        elif case1 == "ablative" or case2 == "ablative":
            return "originates_from"
        elif case1 == "locative" or case2 == "locative":
            return "co_located_with"
        else:
            return "related_to"
    
    def _detect_node_relationship(self, node1, node2):
        """
        Detect potential relationships between nodes.
        
        Args:
            node1 (dict): First node
            node2 (dict): Second node
        
        Returns:
            dict or None: Relationship details
        """
        # Implement relationship detection logic
        # This could involve text similarity, co-occurrence, semantic analysis
        
        # Example simple relationship detection
        if node1["type"] == "entity" and node2["type"] == "claim":
            # Check for textual similarity or semantic relationship
            similarity = self._calculate_semantic_similarity(node1["label"], node2["label"])
            if similarity > 0.5:
                return {
                    "type": "relates_to",
                    "confidence": similarity
                }
        
        return None
    
    def _calculate_semantic_similarity(self, text1, text2):
        """
        Calculate semantic similarity between two texts.
        
        Args:
            text1 (str): First text
            text2 (str): Second text
        
        Returns:
            float: Similarity score
        """
        # Configure environment first to handle protobuf issues
        from .environment import configure_protobuf_environment, test_transformers_import
        configure_protobuf_environment()
        
        # Test if transformers is working before attempting to use it
        if not test_transformers_import():
            self.logger.debug("Transformers not available, using simple text similarity")
            return self._simple_text_similarity(text1, text2)
        
        try:
            import sentence_transformers
            
            model = sentence_transformers.SentenceTransformer('all-MiniLM-L6-v2')
            embeddings = model.encode([text1, text2])
            
            # Calculate cosine similarity
            similarity = np.dot(embeddings[0], embeddings[1]) / (
                np.linalg.norm(embeddings[0]) * np.linalg.norm(embeddings[1]))
            
            return float(similarity)
        except (ImportError, RuntimeError, Exception) as e:
            self.logger.debug(f"Semantic similarity fallback due to error: {str(e)}")
            return self._simple_text_similarity(text1, text2)
    
    def _simple_text_similarity(self, text1, text2):
        """
        Calculate simple text similarity using word overlap.
        
        Args:
            text1 (str): First text
            text2 (str): Second text
        
        Returns:
            float: Similarity score based on word overlap
        """
        # Tokenize and normalize
        words1 = set(text1.lower().split())
        words2 = set(text2.lower().split())
        
        # Calculate Jaccard similarity
        intersection = len(words1 & words2)
        union = len(words1 | words2)
        
        if union == 0:
            return 0.0
        
        return intersection / union
    
    async def process_text_async(self, text: str, metadata: Dict[str, Any] = None) -> Dict[str, Any]:
        """
        Process raw text asynchronously through the LEXICON pipeline.
        
        Args:
            text: Raw text to process
            metadata: Optional metadata about the text
            
        Returns:
            Dict containing graph and processing information
        """
        # This is a simple wrapper that calls the sync version for now
        # In a future version, we can implement fully async processing
        loop = asyncio.get_running_loop()
        return await loop.run_in_executor(
            None, self.process_text, text, metadata
        )
    
    def process_audio(self, audio_path: Union[str, Path], 
                     metadata: Dict[str, Any] = None) -> Dict[str, Any]:
        """
        Process audio file through the LEXICON pipeline.
        
        Args:
            audio_path: Path to audio file
            metadata: Optional metadata about the audio
            
        Returns:
            Dict containing graph and processing information
        """
        # Load audio transcription module
        from ..ingest.asr_wrapper import transcribe_audio
        
        session_id = metadata.get("session_id", str(uuid.uuid4()))
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        self.logger.info(f"Starting audio processing (session={session_id}, file={audio_path})")
        
        # Check if output directory is already in metadata
        if metadata and "output_dir" in metadata:
            session_dir = Path(metadata["output_dir"])
        else:
            # Create session directory
            session_dir = self.config.output_dir / f"lexicon_{timestamp}_{session_id[:8]}"
            session_dir.mkdir(parents=True, exist_ok=True)
            
            # Create subdirectories
            logs_dir = session_dir / "logs"
            logs_dir.mkdir(parents=True, exist_ok=True)
            
            input_dir = session_dir / "input"
            input_dir.mkdir(parents=True, exist_ok=True)
            
            cache_dir = session_dir / "cache"
            cache_dir.mkdir(parents=True, exist_ok=True)
            
            vis_dir = session_dir / "visualizations"
            vis_dir.mkdir(parents=True, exist_ok=True)
        
        # Initialize metadata
        if metadata is None:
            metadata = {}
        
        metadata.update({
            "session_id": session_id,
            "timestamp": datetime.now().isoformat(),
            "audio_path": str(audio_path),
            "output_dir": str(session_dir)
        })
        
        # Copy audio file to input directory
        input_dir = session_dir / "input"
        input_copy = input_dir / Path(audio_path).name
        try:
            with open(audio_path, 'rb') as src, open(input_copy, 'wb') as dst:
                dst.write(src.read())
            self.logger.info(f"Copied audio file to {input_copy}")
        except Exception as e:
            self.logger.warning(f"Failed to copy audio file: {str(e)}")
        
        try:
            # 1. Transcribe audio
            with LoggingTimer(self.logger, "Audio transcription", session_id=session_id):
                transcript = transcribe_audio(audio_path)
                
            # Add transcript to metadata
            metadata["transcript_length"] = len(transcript)
            
            # Save transcript
            transcript_path = session_dir / "transcript.txt"
            with open(transcript_path, 'w') as f:
                f.write(transcript)
                
            # 2. Process transcript
            result = self.process_text(transcript, metadata)
            
            # Add audio-specific fields
            result["audio_path"] = str(audio_path)
            result["transcript_path"] = str(transcript_path)
            
            # Save result
            output_path = session_dir / "result.json"
            with open(output_path, 'w') as f:
                json.dump(result, f, indent=2)
            
            return result
            
        except Exception as e:
            self.logger.error(
                f"Audio processing failed (session={session_id}, file={audio_path}): {str(e)}",
                exc_info=True
            )
            
            # Save error information
            error_info = {
                "status": "error",
                "session_id": session_id,
                "metadata": metadata,
                "error": {
                    "message": str(e),
                    "type": e.__class__.__name__,
                    "traceback": traceback.format_exc()
                }
            }
            
            error_path = session_dir / "error.json"
            with open(error_path, 'w') as f:
                json.dump(error_info, f, indent=2)
            
            raise
    
    def process_file(self, file_path: Union[str, Path], 
                    metadata: Dict[str, Any] = None) -> Dict[str, Any]:
        """
        Process a file through the LEXICON pipeline.
        
        Automatically determines file type and uses appropriate processing.
        
        Args:
            file_path: Path to file
            metadata: Optional metadata about the file
            
        Returns:
            Dict containing graph and processing information
        """
        path = Path(file_path)
        session_id = str(uuid.uuid4())
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # Create session directory
        session_dir = self.config.output_dir / f"lexicon_{timestamp}_{session_id[:8]}"
        session_dir.mkdir(parents=True, exist_ok=True)
        
        # Create subdirectories
        logs_dir = session_dir / "logs"
        logs_dir.mkdir(parents=True, exist_ok=True)
        
        input_dir = session_dir / "input"
        input_dir.mkdir(parents=True, exist_ok=True)
        
        cache_dir = session_dir / "cache"
        cache_dir.mkdir(parents=True, exist_ok=True)
        
        vis_dir = session_dir / "visualizations"
        vis_dir.mkdir(parents=True, exist_ok=True)
        
        # Initialize metadata
        if metadata is None:
            metadata = {}
            
        metadata.update({
            "session_id": session_id,
            "timestamp": datetime.now().isoformat(),
            "file_path": str(path),
            "file_name": path.name,
            "file_type": path.suffix.lower(),
            "output_dir": str(session_dir)
        })
        
        # Copy input file to session directory
        input_copy = input_dir / path.name
        try:
            with open(path, 'rb') as src, open(input_copy, 'wb') as dst:
                dst.write(src.read())
            self.logger.info(f"Copied input file to {input_copy}")
        except Exception as e:
            self.logger.warning(f"Failed to copy input file: {str(e)}")
        
        # Process based on file type
        suffix = path.suffix.lower()
        
        try:
            if suffix in ['.txt', '.md', '.json', '.html', '.csv']:
                # Text file
                with open(path, 'r', encoding='utf-8') as f:
                    content = f.read()
                result = self.process_text(content, metadata)
                
            elif suffix in ['.mp3', '.wav', '.m4a', '.ogg', '.flac']:
                # Audio file
                result = self.process_audio(path, metadata)
                
            else:
                raise InputError(f"Unsupported file type: {suffix}")
            
            # Save result
            output_path = session_dir / "result.json"
            with open(output_path, 'w') as f:
                json.dump(result, f, indent=2)
            
            # Update result with session directory
            result["output_dir"] = str(session_dir)
            
            # Log visualization file information
            log_visualization_files(session_dir)
            
            return result
            
        except Exception as e:
            # Save error information
            error_info = {
                "status": "error",
                "session_id": session_id,
                "metadata": metadata,
                "error": {
                    "message": str(e),
                    "type": e.__class__.__name__,
                    "traceback": traceback.format_exc()
                }
            }
            
            error_path = session_dir / "error.json"
            with open(error_path, 'w') as f:
                json.dump(error_info, f, indent=2)
            
            raise
    
    def get_graph(self, graph_id: str) -> Dict[str, Any]:
        """
        Retrieve a previously generated graph by ID.
        
        Args:
            graph_id: ID of the graph to retrieve
            
        Returns:
            Dict containing the graph data
        """
        # Look for the graph in the output directory
        pattern = f"lexicon_*_{graph_id}*.json"
        
        for file_path in self.config.output_dir.glob(pattern):
            try:
                with open(file_path, 'r') as f:
                    return json.load(f)
            except Exception as e:
                self.logger.error(f"Failed to load graph {graph_id}: {str(e)}")
                continue
        
        # If we get here, the graph wasn't found
        raise LexiconError(f"Graph not found: {graph_id}")
    
    def get_session_status(self, session_id: str) -> Dict[str, Any]:
        """
        Get status information for a processing session.
        
        Args:
            session_id: Session ID to check
            
        Returns:
            Dict with session status information
        """
        # Look for session files
        success_path = self.config.output_dir / f"lexicon_{session_id}.json"
        error_path = self.config.output_dir / f"lexicon_{session_id}_error.json"
        
        if success_path.exists():
            try:
                with open(success_path, 'r') as f:
                    data = json.load(f)
                return {
                    "session_id": session_id,
                    "status": "complete",
                    "timestamp": data.get("metadata", {}).get("timestamp"),
                    "stats": data.get("stats", {})
                }
            except Exception as e:
                self.logger.error(f"Failed to load session data {session_id}: {str(e)}")
        
        if error_path.exists():
            try:
                with open(error_path, 'r') as f:
                    data = json.load(f)
                return {
                    "session_id": session_id,
                    "status": "error",
                    "timestamp": data.get("metadata", {}).get("timestamp"),
                    "error": data.get("error", {}).get("message")
                }
            except Exception as e:
                self.logger.error(f"Failed to load error data {session_id}: {str(e)}")
        
        # If we get here, the session wasn't found
        return {
            "session_id": session_id,
            "status": "not_found"
        } 

    def _create_case_determination_method(self):
        """
        Create a method to determine grammatical case with advanced linguistic analysis.
        
        Returns:
            callable: Method to determine grammatical case
        """
        def determine_case(text, context=""):
            """
            Determine the grammatical case of a given text using advanced linguistic techniques.
            
            Args:
                text (str): Text to determine case for
                context (str): Surrounding context for better analysis
            
            Returns:
                dict: Case information with type, confidence, and rationale
            """
            try:
                import spacy
                
                # Load spaCy model (try transformer first, fallback to smaller model)
                nlp = None
                for model_name in ["en_core_web_trf", "en_core_web_sm"]:
                    try:
                        nlp = spacy.load(model_name)
                        break
                    except IOError:
                        continue
                
                if nlp is None:
                    self.logger.warning("No spaCy model available, using rule-based case assignment")
                    return self._rule_based_case_assignment(text, context)
                
                # Analyze the text in context
                full_text = f"{context} {text}".strip() if context else text
                doc = nlp(full_text)
                
                # Find the target entity in the processed document
                target_tokens = []
                text_words = text.lower().split()
                
                for token in doc:
                    if token.text.lower() in text_words:
                        target_tokens.append(token)
                
                if not target_tokens:
                    return self._rule_based_case_assignment(text, context)
                
                # Analyze dependency relationships for the most relevant token
                primary_token = target_tokens[0]  # Start with first match
                
                # Try to find the head token if multiple matches
                for token in target_tokens:
                    if token.dep_ in ["nsubj", "nsubjpass", "dobj", "iobj", "pobj"]:
                        primary_token = token
                        break
                
                # Determine case based on dependency role
                case_info = self._analyze_dependency_case(primary_token, doc, text)
                
                # Enhance with entity type analysis
                if case_info["confidence"] < 0.7:
                    enhanced_case = self._enhance_case_with_entity_type(text, case_info, doc)
                    if enhanced_case["confidence"] > case_info["confidence"]:
                        case_info = enhanced_case
                
                return case_info
                
            except ImportError:
                self.logger.debug("spaCy not available, using rule-based case assignment")
                return self._rule_based_case_assignment(text, context)
            except Exception as e:
                self.logger.warning(f"Case determination error: {e}, using fallback")
                return self._rule_based_case_assignment(text, context)
        
        return determine_case
    
    def _analyze_dependency_case(self, token, doc, original_text):
        """
        Analyze dependency relationships to determine grammatical case.
        
        Args:
            token: spaCy token
            doc: spaCy document
            original_text: Original entity text
            
        Returns:
            dict: Case information
        """
        dep = token.dep_
        pos = token.pos_
        
        # Nominative case patterns
        if dep in ["nsubj", "nsubjpass"]:
            return {
                "type": "nominative",
                "confidence": 0.9,
                "rationale": f"Subject of sentence: '{token.text}' has dependency '{dep}'"
            }
        
        # Accusative case patterns  
        elif dep in ["dobj"]:
            return {
                "type": "accusative", 
                "confidence": 0.85,
                "rationale": f"Direct object: '{token.text}' has dependency '{dep}'"
            }
        
        # Dative case patterns
        elif dep in ["iobj", "dative"]:
            return {
                "type": "dative",
                "confidence": 0.8,
                "rationale": f"Indirect object: '{token.text}' has dependency '{dep}'"
            }
        
        # Genitive case patterns
        elif dep in ["poss", "nmod"] or "'s" in original_text:
            return {
                "type": "genitive",
                "confidence": 0.75,
                "rationale": f"Possessive relation: '{token.text}' has dependency '{dep}'"
            }
        
        # Instrumental case patterns
        elif dep == "pobj" and token.head.text.lower() in ["with", "by", "using", "through"]:
            return {
                "type": "instrumental",
                "confidence": 0.7,
                "rationale": f"Instrumental relation: '{token.text}' follows '{token.head.text}'"
            }
        
        # Ablative case patterns  
        elif dep == "pobj" and token.head.text.lower() in ["from", "of", "because"]:
            return {
                "type": "ablative",
                "confidence": 0.7,
                "rationale": f"Ablative relation: '{token.text}' follows '{token.head.text}'"
            }
        
        # Locative case patterns
        elif dep == "pobj" and token.head.text.lower() in ["in", "at", "on", "during", "within"]:
            return {
                "type": "locative",
                "confidence": 0.75,
                "rationale": f"Locative relation: '{token.text}' follows '{token.head.text}'"
            }
        
        # Vocative case patterns
        elif dep == "vocative" or (pos == "PROPN" and token.i == 0):
            return {
                "type": "vocative",
                "confidence": 0.8,
                "rationale": f"Direct address: '{token.text}' appears to be vocative"
            }
        
        # Default case based on POS and position
        else:
            return self._default_case_assignment(token, original_text)
    
    def _enhance_case_with_entity_type(self, text, current_case, doc):
        """
        Enhance case assignment using entity type information.
        
        Args:
            text: Entity text
            current_case: Current case assignment
            doc: spaCy document
            
        Returns:
            dict: Enhanced case information
        """
        # Find entity type from spaCy NER
        entity_type = None
        for ent in doc.ents:
            if text.lower() in ent.text.lower():
                entity_type = ent.label_
                break
        
        if not entity_type:
            return current_case
        
        # Enhance based on entity type patterns
        if entity_type in ["PERSON"] and current_case["confidence"] < 0.6:
            # People are often subjects (nominative) or addressed (vocative)
            if any(word in text.lower() for word in ["dr", "prof", "mr", "ms", "mrs"]):
                return {
                    "type": "nominative",
                    "confidence": 0.75,
                    "rationale": f"Person with title likely subject: {text}"
                }
        
        elif entity_type in ["ORG", "PRODUCT"] and current_case["confidence"] < 0.6:
            # Organizations and products often instrumental or genitive
            return {
                "type": "instrumental",
                "confidence": 0.65,
                "rationale": f"Organization/product entity: {text}"
            }
        
        elif entity_type in ["GPE", "LOC"] and current_case["confidence"] < 0.6:
            # Locations default to locative
            return {
                "type": "locative",
                "confidence": 0.7,
                "rationale": f"Location entity: {text}"
            }
        
        return current_case
    
    def _rule_based_case_assignment(self, text, context=""):
        """
        Rule-based case assignment when spaCy is not available.
        
        Args:
            text: Entity text
            context: Surrounding context
            
        Returns:
            dict: Case information
        """
        text_lower = text.lower()
        context_lower = context.lower()
        full_text = f"{context} {text}".lower()
        
        # Vocative patterns
        if any(pattern in full_text for pattern in ["hey ", "hi ", "hello "]):
            return {
                "type": "vocative",
                "confidence": 0.7,
                "rationale": "Direct address pattern detected"
            }
        
        # Genitive patterns
        if "'s" in text or " of " in full_text:
            return {
                "type": "genitive", 
                "confidence": 0.7,
                "rationale": "Possessive pattern detected"
            }
        
        # Instrumental patterns
        if any(prep in context_lower for prep in ["with ", "by ", "using ", "through "]):
            return {
                "type": "instrumental",
                "confidence": 0.65,
                "rationale": "Instrumental preposition context"
            }
        
        # Ablative patterns
        if any(prep in context_lower for prep in ["from ", "because of ", "due to "]):
            return {
                "type": "ablative",
                "confidence": 0.65,
                "rationale": "Ablative preposition context"
            }
        
        # Locative patterns (locations, times, contexts)
        if any(prep in context_lower for prep in ["in ", "at ", "on ", "during ", "within "]):
            return {
                "type": "locative",
                "confidence": 0.6,
                "rationale": "Locative preposition context"
            }
        
        # Default to nominative for proper nouns at sentence start
        if text[0].isupper() and len(text.split()) <= 3:
            return {
                "type": "nominative",
                "confidence": 0.6,
                "rationale": "Proper noun likely subject"
            }
        
        # Final fallback
        return {
            "type": "locative",
            "confidence": 0.4,
            "rationale": "Default assignment - insufficient context"
        }
    
    def _default_case_assignment(self, token, original_text):
        """
        Default case assignment when dependency analysis is inconclusive.
        
        Args:
            token: spaCy token
            original_text: Original entity text
            
        Returns:
            dict: Case information
        """
        pos = token.pos_
        
        # Proper nouns often nominative
        if pos == "PROPN":
            return {
                "type": "nominative",
                "confidence": 0.6,
                "rationale": f"Proper noun: '{original_text}'"
            }
        
        # Common nouns in object position
        elif pos == "NOUN":
            return {
                "type": "accusative",
                "confidence": 0.5,
                "rationale": f"Common noun: '{original_text}'"
            }
        
        # Default locative for other cases
        else:
            return {
                "type": "locative",
                "confidence": 0.4,
                "rationale": f"Default case for POS: {pos}"
            } 