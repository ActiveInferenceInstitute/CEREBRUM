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
        Detect named entities using advanced NLP techniques.
        
        Args:
            text (str): Input text
        
        Returns:
            list: Detected named entities with metadata
        """
        try:
            import spacy
            
            # Load a more advanced NER model
            nlp = spacy.load("en_core_web_trf")  # Transformer-based model
            doc = nlp(text)
            
            entities = []
            for ent in doc.ents:
                entities.append({
                    "text": ent.text,
                    "type": ent.label_,
                    "case": self._determine_grammatical_case(ent.text),
                    "confidence": 0.9,
                    "metadata": {
                        "source": "spacy_ner",
                        "start_char": ent.start_char,
                        "end_char": ent.end_char
                    }
                })
            
            return entities
        except ImportError:
            # Fallback to LLM-based detection
            return self._llm_entity_detection(text)
    
    def _detect_contextual_entities(self, text):
        """
        Detect contextual entities using LLM.
        
        Args:
            text: Input text
        
        Returns:
            list: Detected contextual entities
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
        
        return entities

    def _detect_relational_entities(self, text):
        """
        Detect entities based on their relationships in the text.
        
        Args:
            text (str): Input text
        
        Returns:
            list: Entities with relationship metadata
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
            relational_data = json.loads(response)
            return relational_data.get('entities', [])
        except json.JSONDecodeError:
            return []
    
    def _detect_explicit_claims(self, text):
        """
        Detect explicit claims in the text.
        
        Args:
            text (str): Input text
        
        Returns:
            list: Explicit claims with metadata
        """
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
                "polarity": "positive|negative|neutral",
                "confidence": 0.8,
                "case": "grammatical case",
                "supporting_evidence": "Brief context"
            }},
            ...
        ]
        """
        
        response = self._call_llm(prompt)
        
        try:
            explicit_claims = json.loads(response)
            return explicit_claims
        except json.JSONDecodeError:
            return []
    
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
            self.logger.info(f"Processing text with {len(text)} characters")
            
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
            
            # Detect entities
            self.logger.info("Detecting entities")
            entities = []
            for strategy in self.entity_detection_strategies:
                self.logger.debug(f"Applying entity detection strategy: {strategy.__name__}")
                try:
                    detected = strategy(text)
                    if detected:
                        entities.extend(detected)
                except Exception as e:
                    self.logger.error(f"Entity detection strategy {strategy.__name__} failed: {e}")
                    continue
            
            # Detect claims
            self.logger.info("Detecting claims")
            claims = []
            for strategy in self.claim_detection_strategies:
                self.logger.debug(f"Applying claim detection strategy: {strategy.__name__}")
                try:
                    detected = strategy(text)
                    if detected:
                        claims.extend(detected)
                except Exception as e:
                    self.logger.error(f"Claim detection strategy {strategy.__name__} failed: {e}")
                    continue
            
            # Build knowledge graph
            self.logger.info("Building knowledge graph")
            graph = self._build_knowledge_graph(entities, claims, text)
            
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
        
        # Add entities as nodes
        for entity in entities:
            node = {
                "id": entity.get("id", str(uuid.uuid4())),
                "label": entity.get("text", ""),
                "type": "entity",
                "category": entity.get("category", "unknown"),
                "confidence": entity.get("confidence", 1.0),
                "data": entity
            }
            graph["nodes"].append(node)
        
        # Add claims as nodes
        for claim in claims:
            node = {
                "id": claim.get("id", str(uuid.uuid4())),
                "label": claim.get("text", ""),
                "type": "claim",
                "polarity": claim.get("polarity", "neutral"),
                "confidence": claim.get("confidence", 1.0),
                "data": claim
            }
            graph["nodes"].append(node)
        
        # Create edges between entities and claims
        for claim in claims:
            claim_id = claim.get("id", "")
            
            # Connect entities mentioned in the claim
            for entity in entities:
                entity_id = entity.get("id", "")
                entity_text = entity.get("text", "").lower()
                
                # Check if entity is mentioned in claim
                if entity_text and entity_text in claim.get("text", "").lower():
                    edge = {
                        "id": f"{entity_id}_{claim_id}",
                        "source": entity_id,
                        "target": claim_id,
                        "type": "mentions",
                        "weight": 1.0
                    }
                    graph["edges"].append(edge)
        
        return graph
    
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
            similarity = self._calculate_semantic_similarity(node1["text"], node2["text"])
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
        try:
            import sentence_transformers
            
            model = sentence_transformers.SentenceTransformer('all-MiniLM-L6-v2')
            embeddings = model.encode([text1, text2])
            
            # Calculate cosine similarity
            similarity = np.dot(embeddings[0], embeddings[1]) / (
                np.linalg.norm(embeddings[0]) * np.linalg.norm(embeddings[1])
            )
            
            return float(similarity)
        except ImportError:
            # Fallback to simple text comparison
            return len(set(text1.lower().split()) & set(text2.lower().split())) / len(
                set(text1.lower().split()) | set(text2.lower().split())
            )
    
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
        Create a method to determine grammatical case.
        
        Returns:
            callable: Method to determine grammatical case
        """
        def determine_case(text):
            """
            Determine the grammatical case of a given text.
            
            This is a simple heuristic method and should be replaced with 
            a more sophisticated linguistic analysis.
            
            Args:
                text (str): Text to determine case for
            
            Returns:
                str: Grammatical case (nominative, accusative, dative, etc.)
            """
            # Simple heuristics for case determination
            text = text.lower().strip()
            
            # Nominative (subject) cases
            nominative_indicators = [
                'i', 'we', 'he', 'she', 'it', 'they', 
                'who', 'which', 'that', 
                'dr.', 'mr.', 'ms.', 'mrs.'
            ]
            
            # Accusative (direct object) cases
            accusative_indicators = [
                'me', 'us', 'him', 'her', 'it', 'them', 
                'whom'
            ]
            
            # Dative (indirect object) cases
            dative_indicators = [
                'to me', 'to us', 'to him', 'to her', 'to them',
                'for me', 'for us', 'for him', 'for her', 'for them'
            ]
            
            # Genitive (possession) cases
            genitive_indicators = [
                "'s", "s'", "of the", "of a", "of an"
            ]
            
            # Ablative (source/origin) cases
            ablative_indicators = [
                'from', 'by', 'with', 'through'
            ]
            
            # Check for specific cases
            for indicator in nominative_indicators:
                if text == indicator or text.startswith(indicator + ' '):
                    return 'nominative'
            
            for indicator in accusative_indicators:
                if text == indicator or text.startswith(indicator + ' '):
                    return 'accusative'
            
            for indicator in dative_indicators:
                if indicator in text:
                    return 'dative'
            
            for indicator in genitive_indicators:
                if indicator in text:
                    return 'genitive'
            
            for indicator in ablative_indicators:
                if text.startswith(indicator + ' '):
                    return 'ablative'
            
            # Locative (location) as default fallback
            return 'locative'
        
        return determine_case 

def _parse_contextual_entities(self, response):
    """
    Parse contextual entities from LLM response.
    
    Args:
        response: LLM response text
        
    Returns:
        List of parsed entities
    """
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
    
    return entities 