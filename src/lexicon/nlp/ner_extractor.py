"""
LEXICON Named Entity Recognition

Extracts named entities from text for the LEXICON pipeline.
"""

import re
from typing import List, Dict, Any, Optional, Tuple
import json

from ..core.config import LexiconConfig
from ..core.logging import get_logger
from ..core.exceptions import ProcessingError

try:
    import spacy
    SPACY_AVAILABLE = True
except ImportError:
    SPACY_AVAILABLE = False


class NamedEntityExtractor:
    """
    Named Entity Recognition (NER) for LEXICON.
    
    Uses spaCy if available, otherwise falls back to a rule-based approach.
    """
    
    def __init__(self, config: LexiconConfig):
        """
        Initialize the NER extractor.
        
        Args:
            config: LEXICON configuration
        """
        self.config = config
        self.logger = get_logger("nlp.ner_extractor")
        self._spacy_nlp = None
        
        if not SPACY_AVAILABLE:
            self.logger.warning(
                "spaCy not available - using simple rule-based NER"
            )
    
    def _get_spacy_nlp(self, model="en_core_web_sm"):
        """Get or initialize spaCy NLP model for NER."""
        if not SPACY_AVAILABLE:
            return None
            
        if self._spacy_nlp is None:
            try:
                # Load model with focus on NER components
                self._spacy_nlp = spacy.load(model, disable=["textcat"])
                self.logger.debug(f"Loaded spaCy model for NER: {model}")
            except Exception as e:
                self.logger.warning(
                    f"Failed to load spaCy model {model}: {str(e)}. "
                    "Falling back to rule-based NER."
                )
                self._spacy_nlp = None
        
        return self._spacy_nlp
    
    def _extract_with_spacy(self, text: str) -> List[Dict[str, Any]]:
        """
        Extract entities using spaCy.
        
        Args:
            text: Text to analyze
            
        Returns:
            List of entity dictionaries
        """
        nlp = self._get_spacy_nlp()
        doc = nlp(text)
        
        entities = []
        for ent in doc.ents:
            entity = {
                "text": ent.text,
                "start": ent.start_char,
                "end": ent.end_char,
                "type": ent.label_,
                "confidence": 1.0  # spaCy doesn't provide confidence scores
            }
            entities.append(entity)
        
        return entities
    
    def _extract_with_rules(self, text: str) -> List[Dict[str, Any]]:
        """
        Extract entities using simple rules.
        
        Args:
            text: Text to analyze
            
        Returns:
            List of entity dictionaries
        """
        entities = []
        
        # Extract potential person names (capitalized words)
        # This is a very naive approach but provides a basic fallback
        person_pattern = r'\b[A-Z][a-z]+(?:\s+[A-Z][a-z]+)*\b'
        for match in re.finditer(person_pattern, text):
            # Skip single words that are likely sentence starters
            if ' ' not in match.group() and match.start() > 0 and text[match.start()-1] == '.':
                continue
                
            entity = {
                "text": match.group(),
                "start": match.start(),
                "end": match.end(),
                "type": "PERSON",
                "confidence": 0.5  # Low confidence for rule-based matches
            }
            entities.append(entity)
        
        # Extract organizations (words ending in Inc, Corp, LLC, etc.)
        org_pattern = r'\b[A-Z][a-zA-Z]*(?:\s+[A-Z][a-zA-Z]*)*(?:\s+(?:Inc|Corp|LLC|Ltd|Limited|Company|Co))?\.?\b'
        for match in re.finditer(org_pattern, text):
            entity = {
                "text": match.group(),
                "start": match.start(),
                "end": match.end(),
                "type": "ORG",
                "confidence": 0.4
            }
            entities.append(entity)
        
        # Extract locations (simplistic)
        loc_pattern = r'\b(?:in|at|from|to)\s+([A-Z][a-z]+(?:\s+[A-Z][a-z]+)*)\b'
        for match in re.finditer(loc_pattern, text):
            entity = {
                "text": match.group(1),
                "start": match.start(1),
                "end": match.end(1),
                "type": "LOC",
                "confidence": 0.3
            }
            entities.append(entity)
        
        # Extract dates
        date_pattern = r'\b(?:\d{1,2}[/-]\d{1,2}[/-]\d{2,4})|(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)\s+\d{1,2}(?:st|nd|rd|th)?,?\s+\d{4}\b'
        for match in re.finditer(date_pattern, text):
            entity = {
                "text": match.group(),
                "start": match.start(),
                "end": match.end(),
                "type": "DATE",
                "confidence": 0.7
            }
            entities.append(entity)
        
        return entities
    
    def extract(self, text: str) -> List[Dict[str, Any]]:
        """
        Extract named entities from text.
        
        Args:
            text: Text to analyze
            
        Returns:
            List of entity dictionaries with keys:
            - text: Entity text
            - start: Start character position
            - end: End character position
            - type: Entity type
            - confidence: Confidence score (0.0-1.0)
        """
        if not text or not text.strip():
            return []
            
        try:
            # Try spaCy first if available
            if SPACY_AVAILABLE and self._get_spacy_nlp() is not None:
                entities = self._extract_with_spacy(text)
            else:
                entities = self._extract_with_rules(text)
                
            # Deduplicate entities
            seen = set()
            unique_entities = []
            
            for entity in entities:
                key = (entity["text"], entity["start"], entity["end"])
                if key not in seen:
                    seen.add(key)
                    unique_entities.append(entity)
            
            self.logger.debug(f"Extracted {len(unique_entities)} entities")
            return unique_entities
            
        except Exception as e:
            self.logger.error(f"Entity extraction failed: {str(e)}")
            return []
    
    def extract_batch(self, texts: List[str]) -> List[List[Dict[str, Any]]]:
        """
        Extract entities from multiple texts.
        
        Args:
            texts: List of texts to analyze
            
        Returns:
            List of entity lists, one per text
        """
        results = []
        
        # For spaCy, we can use pipe for efficiency
        if SPACY_AVAILABLE and self._get_spacy_nlp() is not None:
            nlp = self._get_spacy_nlp()
            docs = list(nlp.pipe(texts))
            
            for doc in docs:
                entities = []
                for ent in doc.ents:
                    entity = {
                        "text": ent.text,
                        "start": ent.start_char,
                        "end": ent.end_char,
                        "type": ent.label_,
                        "confidence": 1.0
                    }
                    entities.append(entity)
                results.append(entities)
        else:
            # Fall back to processing each text individually
            for text in texts:
                results.append(self.extract(text))
        
        return results 