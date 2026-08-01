"""
LEXICON Coreference Resolution

Resolves entity references across text segments.
"""

import re
from typing import List

from ..core.config import LexiconConfig
from ..core.logging import LoggingTimer, get_logger
from ..nlp.preprocessor import ProcessedSegment

try:
    import neuralcoref
    import spacy
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
        
        # Safely map the (possibly length-shifted) resolved text back onto the
        # original segments instead of slicing by original character offsets,
        # which silently corrupted segments whenever a substituted pronoun
        # changed the total character count.
        try:
            mapped_portions = self._map_segments_to_resolved(segments, joined_text, resolved_text)
        except Exception as e:
            self.logger.warning(f"Coreference re-segmentation failed ({str(e)}); returning original segments")
            return segments
        
        # Build new segments from the safe mapping.
        resolved_segments = []
        for segment, portion in zip(segments, mapped_portions):
            resolved_segment = ProcessedSegment(
                segment_id=segment.segment_id,
                text=portion,
                speaker=segment.speaker,
                timestamp=segment.timestamp,
                entities=segment.entities,
                pos_tags=segment.pos_tags,
                metadata={**segment.metadata, "coref_resolved": True}
            )
            resolved_segments.append(resolved_segment)
        
        return resolved_segments
    
    @staticmethod
    def _map_segments_to_resolved(segments: List[ProcessedSegment],
                                  joined_text: str,
                                  resolved_text: str) -> List[str]:
        """
        Re-segment resolved coreference text back onto the original segments.

        neuralcoref's ``coref_resolved`` replaces pronouns with full mentions, so
        the resolved text can be a *different length* than the joined input. The
        historical implementation sliced ``resolved_text`` using the *original*
        character offsets, which silently shifted every segment after the first
        substitution (index or garbage text corruption).

        This version re-anchors each segment in the resolved text. When lengths
        are unchanged the original offsets are exact; when they have drifted each
        segment is located by its leading word, so boundaries stay correct and a
        segment that cannot be located is returned as its original clean text
        rather than corrupted bytes.

        Args:
            segments: Original processed segments
            joined_text: Text the segments were joined into (original offsets)
            resolved_text: Corresponding coref-resolved text

        Returns:
            List of resolved text portions, one per original segment
        """
        # Fast path: same length means original offsets remain exact.
        if len(joined_text) == len(resolved_text):
            portions = []
            cursor = 0
            for seg in segments:
                portions.append(resolved_text[cursor:cursor + len(seg.text)])
                cursor += len(seg.text) + 1  # +1 for the join space
            return portions
        
        # Length drifted: re-anchor each segment by its leading word so we never
        # slice by stale offsets. No arbitrary indexing can raise IndexError here.
        portions = []
        cursor = 0
        for seg in segments:
            first_word = next((w for w in seg.text.split() if w), "")
            if not first_word:
                portions.append(seg.text)
                continue
            start = resolved_text.find(first_word, cursor)
            if start == -1:
                # Cannot locate the segment start; keep the original clean text.
                portions.append(seg.text)
                cursor += len(seg.text) + 1
                continue
            end = min(start + len(seg.text), len(resolved_text))
            portions.append(resolved_text[start:end])
            cursor = end + 1
        return portions
    
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