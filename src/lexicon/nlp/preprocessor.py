"""
LEXICON NLP Preprocessor

Main orchestration class for NLP preprocessing tasks:
- Sentence boundary detection
- Speaker diarization
- POS tagging
- Named entity recognition
- Coreference resolution
"""

import os
import re
import time
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass, field
import json
import logging
import traceback

from ..core.config import LexiconConfig
from ..core.logging import get_logger, LoggingTimer
from ..core.exceptions import ProcessingError

try:
    import spacy
    SPACY_AVAILABLE = True
except ImportError:
    SPACY_AVAILABLE = False


@dataclass
class ProcessedSegment:
    """A processed segment of text with associated metadata."""
    segment_id: str
    text: str
    speaker: Optional[str] = None
    timestamp: Optional[float] = None
    entities: List[Dict[str, Any]] = field(default_factory=list)
    pos_tags: List[Dict[str, Any]] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)


class NLPPreprocessor:
    """
    NLP preprocessing orchestrator.
    
    Coordinates the complete NLP preprocessing pipeline:
    1. Sentence boundary detection
    2. Speaker diarization (if applicable)
    3. POS tagging
    4. Named entity recognition
    5. Coreference resolution
    """
    
    def __init__(self, config: LexiconConfig):
        """
        Initialize the NLP preprocessor.
        
        Args:
            config: LEXICON configuration
        """
        self.config = config
        self.logger = get_logger("nlp.preprocessor")
        
        # Initialize components lazily
        self._sentence_splitter = None
        self._pos_tagger = None
        self._ner_extractor = None
        self._coreference_resolver = None
        self._spacy_nlp = None
        
        if not SPACY_AVAILABLE:
            self.logger.warning("spaCy not available - some NLP features will be limited")
    
    def _get_sentence_splitter(self):
        """Get or initialize sentence splitter."""
        if self._sentence_splitter is None:
            from .sentence_splitter import SentenceSplitter
            self._sentence_splitter = SentenceSplitter(self.config)
            self.logger.debug("Sentence splitter initialized")
        return self._sentence_splitter
    
    def _get_pos_tagger(self):
        """Get or initialize POS tagger."""
        if self._pos_tagger is None:
            from .pos_tagger import POSTagger
            self._pos_tagger = POSTagger(self.config)
            self.logger.debug("POS tagger initialized")
        return self._pos_tagger
    
    def _get_ner_extractor(self):
        """Get or initialize NER extractor."""
        if self._ner_extractor is None:
            from .ner_extractor import NamedEntityExtractor
            self._ner_extractor = NamedEntityExtractor(self.config)
            self.logger.debug("NER extractor initialized")
        return self._ner_extractor
    
    def _get_coreference_resolver(self):
        """Get or initialize coreference resolver."""
        if self._coreference_resolver is None:
            from .coreference_resolver import CoreferenceResolver
            self._coreference_resolver = CoreferenceResolver(self.config)
            self.logger.debug("Coreference resolver initialized")
        return self._coreference_resolver
    
    def _get_spacy_nlp(self, model="en_core_web_sm"):
        """Get or initialize spaCy NLP model."""
        if not SPACY_AVAILABLE:
            raise ProcessingError("spaCy is required for NLP processing")
            
        if self._spacy_nlp is None:
            try:
                self._spacy_nlp = spacy.load(model)
                self.logger.debug(f"Loaded spaCy model: {model}")
            except Exception as e:
                self.logger.error(f"Failed to load spaCy model {model}: {str(e)}")
                raise ProcessingError(f"Failed to initialize spaCy: {str(e)}")
        
        return self._spacy_nlp
    
    def _detect_format(self, text: str) -> str:
        """Detect the format of input text."""
        # Check for podcast VTT format
        if re.search(r'\d\d:\d\d:\d\d\.\d\d\d --> \d\d:\d\d:\d\d\.\d\d\d', text):
            return "podcast:vtt"
        
        # Check for meeting transcript format with speaker labels
        if re.search(r'^[A-Z][a-zA-Z]*\s?[A-Za-z]*\s?[A-Za-z]*:', text, re.MULTILINE):
            return "meeting:transcript"
        
        # Check for Twitter thread format
        if re.search(r'@[a-zA-Z0-9_]+:', text) or re.search(r'Tweet \d+:', text):
            return "twitter:thread"
            
        # Default to plain text
        return "plain:text"
    
    def _parse_format(self, text: str, format_type: str) -> List[ProcessedSegment]:
        """
        Parse text according to detected format.
        
        Args:
            text: Raw text content
            format_type: Detected format type
            
        Returns:
            List of processed segments
        """
        if format_type == "podcast:vtt":
            from ..ingest.format_parsers.podcast_vtt import parse_vtt
            return parse_vtt(text)
            
        elif format_type == "meeting:transcript":
            from ..ingest.format_parsers.meeting_transcript import parse_meeting
            return parse_meeting(text)
            
        elif format_type == "twitter:thread":
            from ..ingest.format_parsers.twitter_thread import parse_twitter
            return parse_twitter(text)
            
        else:  # plain:text
            # For plain text, we just use the sentence splitter
            sentences = self._get_sentence_splitter().split(text)
            
            # Convert to processed segments
            segments = []
            for i, sentence in enumerate(sentences):
                segments.append(ProcessedSegment(
                    segment_id=f"s{i+1}",
                    text=sentence.strip()
                ))
            
            return segments
    
    def process(self, text: str, format_type: Optional[str] = None) -> List[ProcessedSegment]:
        """
        Process text through the NLP pipeline.
        
        Args:
            text: Raw text to process
            format_type: Optional format hint (auto-detected if None)
            
        Returns:
            List of processed segments
        """
        self.logger.info(f"Starting NLP preprocessing (chars={len(text)})")
        start_time = time.time()
        
        try:
            # 1. Detect format if not specified
            if format_type is None:
                format_type = self._detect_format(text)
                self.logger.info(f"Detected format: {format_type}")
            
            # 2. Parse according to format
            with LoggingTimer(self.logger, "Format parsing"):
                segments = self._parse_format(text, format_type)
                self.logger.info(f"Parsed {len(segments)} segments")
            
            # Skip empty segments
            segments = [s for s in segments if s.text.strip()]
            
            # 3. Apply NLP processing
            processed_segments = []
            
            if SPACY_AVAILABLE:
                # Use spaCy for efficient batch processing
                with LoggingTimer(self.logger, "spaCy processing"):
                    nlp = self._get_spacy_nlp()
                    
                    # Process text in batches for efficiency
                    texts = [s.text for s in segments]
                    for i, doc in enumerate(nlp.pipe(texts)):
                        segment = segments[i]
                        
                        # Extract named entities
                        segment.entities = [
                            {
                                "text": ent.text,
                                "start": ent.start_char,
                                "end": ent.end_char,
                                "type": ent.label_,
                                "confidence": 1.0  # spaCy doesn't provide confidence scores
                            }
                            for ent in doc.ents
                        ]
                        
                        # Extract POS tags
                        segment.pos_tags = [
                            {
                                "text": token.text,
                                "start": token.idx,
                                "end": token.idx + len(token.text),
                                "pos": token.pos_,
                                "tag": token.tag_,
                                "dep": token.dep_
                            }
                            for token in doc
                        ]
                        
                        processed_segments.append(segment)
            else:
                # Fallback to basic processing without spaCy
                with LoggingTimer(self.logger, "Basic NLP processing"):
                    ner = self._get_ner_extractor()
                    pos = self._get_pos_tagger()
                    
                    for segment in segments:
                        # Apply NER
                        segment.entities = ner.extract(segment.text)
                        
                        # Apply POS tagging
                        segment.pos_tags = pos.tag(segment.text)
                        
                        processed_segments.append(segment)
            
            # 4. Apply coreference resolution across segments
            try:
                with LoggingTimer(self.logger, "Coreference resolution"):
                    coref = self._get_coreference_resolver()
                    processed_segments = coref.resolve(processed_segments)
            except Exception as e:
                # Coreference resolution is optional, so log error but continue
                self.logger.error(f"Coreference resolution failed: {str(e)}")
            
            # Log processing summary
            duration = time.time() - start_time
            tokens = sum(len(s.pos_tags) for s in processed_segments)
            entities = sum(len(s.entities) for s in processed_segments)
            
            self.logger.info(
                f"NLP preprocessing completed: {len(processed_segments)} segments, "
                f"{tokens} tokens, {entities} entities ({duration:.2f}s)"
            )
            
            return processed_segments
            
        except Exception as e:
            self.logger.error(f"NLP preprocessing failed: {str(e)}", exc_info=True)
            raise ProcessingError(f"NLP preprocessing failed: {str(e)}")
    
    def process_batch(self, texts: List[str]) -> List[List[ProcessedSegment]]:
        """
        Process multiple texts in batch.
        
        Args:
            texts: List of texts to process
            
        Returns:
            List of processed segment lists, one per input text
        """
        return [self.process(text) for text in texts]
    
    def serialize_processed_segments(self, segments: List[ProcessedSegment]) -> str:
        """
        Serialize processed segments to JSON.
        
        Args:
            segments: List of processed segments
            
        Returns:
            JSON string representation
        """
        # Convert dataclasses to dictionaries
        segments_dict = [
            {
                "segment_id": s.segment_id,
                "text": s.text,
                "speaker": s.speaker,
                "timestamp": s.timestamp,
                "entities": s.entities,
                "pos_tags": s.pos_tags,
                "metadata": s.metadata
            }
            for s in segments
        ]
        
        return json.dumps(segments_dict, indent=2)
    
    @classmethod
    def deserialize_processed_segments(cls, json_str: str) -> List[ProcessedSegment]:
        """
        Deserialize processed segments from JSON.
        
        Args:
            json_str: JSON string representation
            
        Returns:
            List of processed segments
        """
        segments_dict = json.loads(json_str)
        
        return [
            ProcessedSegment(
                segment_id=s["segment_id"],
                text=s["text"],
                speaker=s.get("speaker"),
                timestamp=s.get("timestamp"),
                entities=s.get("entities", []),
                pos_tags=s.get("pos_tags", []),
                metadata=s.get("metadata", {})
            )
            for s in segments_dict
        ] 