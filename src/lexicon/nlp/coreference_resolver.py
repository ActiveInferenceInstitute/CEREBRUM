"""
LEXICON Coreference Resolution

Resolves entity references across text segments.
"""

import re
from typing import List, Dict, Any, Optional, Set, Tuple
import logging

from ..core.config import LexiconConfig
from ..core.logging import get_logger, LoggingTimer
from ..core.exceptions import ProcessingError
from ..nlp.preprocessor import ProcessedSegment

try:
    import spacy
    import neuralcoref
    NEURALCOREF_AVAILABLE = True
except ImportError:
    NEURALCOREF_AVAILABLE = False


class CoreferenceResolver:
    """
    Coreference resolution for LEXICON.
    
    Resolves entity references across text segments using spaCy's neuralcoref
    if available, otherwise falls back to rule-based matching.
    """
    
    def __init__(self, config: LexiconConfig):
        """
        Initialize the coreference resolver.
        
        Args:
            config: LEXICON configuration
        """
        self.config = config
        self.logger = get_logger("nlp.coreference_resolver")
        self._spacy_nlp = None
        
        if not NEURALCOREF_AVAILABLE:
            self.logger.warning(
                "neuralcoref not available - using simple rule-based coreference resolution"
            )
    
    def _get_spacy_nlp(self, model="en_core_web_sm"):
        """Get or initialize spaCy NLP model with neuralcoref."""
        if not NEURALCOREF_AVAILABLE:
            return None
            
        if self._spacy_nlp is None:
            try:
                # Load model and add neuralcoref
                self._spacy_nlp = spacy.load(model)
                neuralcoref.add_to_pipe(self._spacy_nlp)
                self.logger.debug(f"Loaded spaCy model with neuralcoref: {model}")
            except Exception as e:
                self.logger.warning(
                    f"Failed to load spaCy model with neuralcoref: {str(e)}. "
                    "Falling back to rule-based coreference resolution."
                )
                self._spacy_nlp = None
        
        return self._spacy_nlp
    
    def _resolve_with_neuralcoref(self, segments: List[ProcessedSegment]) -> List[ProcessedSegment]:
        """
        Resolve coreferences using neuralcoref.
        
        Args:
            segments: List of processed text segments
            
        Returns:
            List of segments with resolved coreferences
        """
        nlp = self._get_spacy_nlp()
        
        # Join segments for processing
        joined_text = " ".join(segment.text for segment in segments)
        doc = nlp(joined_text)
        
        # Get coreferences
        if not doc._.has_coref:
            self.logger.debug("No coreferences found")
            return segments
        
        # Create resolved text with substitutions
        resolved_text = doc._.coref_resolved
        
        # Map resolved text back to segments
        # This is approximate and may not handle all cases correctly
        offset = 0
        resolved_segments = []
        
        for segment in segments:
            segment_len = len(segment.text)
            
            # Extract corresponding portion from resolved text
            resolved_portion = resolved_text[offset:offset + segment_len]
            
            # Create new segment with resolved text
            resolved_segment = ProcessedSegment(
                segment_id=segment.segment_id,
                text=resolved_portion,
                speaker=segment.speaker,
                timestamp=segment.timestamp,
                entities=segment.entities,
                pos_tags=segment.pos_tags,
                metadata={**segment.metadata, "coref_resolved": True}
            )
            
            resolved_segments.append(resolved_segment)
            offset += segment_len + 1  # +1 for the space we added when joining
        
        return resolved_segments
    
    def _resolve_with_rules(self, segments: List[ProcessedSegment]) -> List[ProcessedSegment]:
        """
        Resolve coreferences using simple rules.
        
        This is a very simplistic approach that only handles some basic pronoun cases.
        
        Args:
            segments: List of processed text segments
            
        Returns:
            List of segments with some resolved coreferences
        """
        # Maps of pronouns to potential entity types
        pronoun_map = {
            "he": "PERSON",
            "she": "PERSON", 
            "him": "PERSON",
            "her": "PERSON",
            "his": "PERSON",
            "hers": "PERSON",
            "it": "THING",
            "its": "THING",
            "they": "GROUP",
            "them": "GROUP",
            "their": "GROUP",
            "theirs": "GROUP"
        }
        
        # Find named entities across all segments
        all_entities = {}  # entity text -> last segment with this entity
        
        for i, segment in enumerate(segments):
            for entity in segment.entities:
                entity_type = entity.get("type", "")
                entity_text = entity.get("text", "")
                
                if entity_text:
                    # Lowercase for easier matching
                    key = entity_text.lower()
                    all_entities[key] = (i, entity_text, entity_type)
        
        # Create metadata for coreference
        for i, segment in enumerate(segments):
            # Find pronouns in segment text
            for pronoun, target_type in pronoun_map.items():
                # Look for pronouns with word boundaries
                pattern = r'\b' + re.escape(pronoun) + r'\b'
                
                # Find all occurrences of the pronoun
                for match in re.finditer(pattern, segment.text, re.IGNORECASE):
                    # Search backward for matching entities
                    found_entity = None
                    
                    for j in range(i-1, -1, -1):
                        prev_segment = segments[j]
                        
                        # Look at entities in this segment
                        for entity in prev_segment.entities:
                            entity_type = entity.get("type", "")
                            entity_text = entity.get("text", "")
                            
                            # Skip if empty or not a match for the pronoun type
                            if not entity_text:
                                continue
                                
                            # Check if entity type matches pronoun type
                            if (
                                (target_type == "PERSON" and entity_type in ["PERSON", "PROPN"]) or
                                (target_type == "THING" and entity_type not in ["PERSON", "PROPN", "ORG", "GPE"]) or
                                (target_type == "GROUP" and entity_type in ["ORG", "GPE", "NORP"])
                            ):
                                found_entity = entity_text
                                break
                                
                        if found_entity:
                            break
                    
                    # Store coreference info in metadata
                    if found_entity:
                        if "coreferences" not in segment.metadata:
                            segment.metadata["coreferences"] = []
                            
                        segment.metadata["coreferences"].append({
                            "pronoun": match.group(),
                            "start": match.start(),
                            "end": match.end(),
                            "entity": found_entity
                        })
        
        return segments
    
    def resolve(self, segments: List[ProcessedSegment]) -> List[ProcessedSegment]:
        """
        Resolve coreferences across segments.
        
        Args:
            segments: List of processed text segments
            
        Returns:
            List of segments with resolved coreferences
        """
        if not segments:
            return []
            
        try:
            # Try neuralcoref if available
            if NEURALCOREF_AVAILABLE and self._get_spacy_nlp() is not None:
                with LoggingTimer(self.logger, "Neural coreference resolution"):
                    resolved = self._resolve_with_neuralcoref(segments)
            else:
                with LoggingTimer(self.logger, "Rule-based coreference resolution"):
                    resolved = self._resolve_with_rules(segments)
            
            self.logger.debug(f"Resolved coreferences across {len(segments)} segments")
            return resolved
            
        except Exception as e:
            self.logger.error(f"Coreference resolution failed: {str(e)}")
            return segments  # Return original segments on failure 