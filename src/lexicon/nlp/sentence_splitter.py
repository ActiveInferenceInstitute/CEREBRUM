"""
LEXICON Sentence Splitter

Provides sentence boundary detection for the LEXICON pipeline.
"""

import re
from typing import List, Dict, Any, Optional
import logging

from ..core.config import LexiconConfig
from ..core.logging import get_logger
from ..core.exceptions import ProcessingError

try:
    import spacy
    SPACY_AVAILABLE = True
except ImportError:
    SPACY_AVAILABLE = False


class SentenceSplitter:
    """
    Sentence boundary detection.
    
    Uses spaCy if available, otherwise falls back to rule-based splitting.
    """
    
    def __init__(self, config: LexiconConfig):
        """
        Initialize the sentence splitter.
        
        Args:
            config: LEXICON configuration
        """
        self.config = config
        self.logger = get_logger("nlp.sentence_splitter")
        self._spacy_nlp = None
        
        if not SPACY_AVAILABLE:
            self.logger.warning(
                "spaCy not available - using rule-based sentence splitting"
            )
    
    def _get_spacy_nlp(self, model="en_core_web_sm"):
        """Get or initialize spaCy NLP model for sentence splitting."""
        if not SPACY_AVAILABLE:
            return None
            
        if self._spacy_nlp is None:
            try:
                # Load with only the required components for efficiency
                self._spacy_nlp = spacy.load(model, disable=["ner", "lemmatizer", "textcat"])
                self.logger.debug(f"Loaded spaCy model for sentence splitting: {model}")
            except Exception as e:
                self.logger.warning(
                    f"Failed to load spaCy model {model}: {str(e)}. "
                    "Falling back to rule-based sentence splitting."
                )
                self._spacy_nlp = None
        
        return self._spacy_nlp
    
    def _split_with_spacy(self, text: str) -> List[str]:
        """
        Split text into sentences using spaCy.
        
        Args:
            text: Text to split
            
        Returns:
            List of sentences
        """
        nlp = self._get_spacy_nlp()
        doc = nlp(text)
        return [sent.text.strip() for sent in doc.sents]
    
    def _split_with_rules(self, text: str) -> List[str]:
        """
        Split text into sentences using rule-based approach.
        
        Args:
            text: Text to split
            
        Returns:
            List of sentences
        """
        # Handle common abbreviations to avoid false sentence boundaries
        abbrevs = ['Mr.', 'Mrs.', 'Dr.', 'Prof.', 'Inc.', 'Ltd.', 'Co.', 'etc.', 
                   'vs.', 'e.g.', 'i.e.', 'a.m.', 'p.m.', 'U.S.', 'U.K.']
        
        # Replace periods in abbreviations with a special marker
        for abbrev in abbrevs:
            text = text.replace(abbrev, abbrev.replace('.', '<!PERIOD!>'))
        
        # Split on sentence boundaries
        pattern = r'(?<=[.!?])\s+(?=[A-Z])'
        sentences = re.split(pattern, text)
        
        # Handle edge cases and clean up
        clean_sentences = []
        
        for sent in sentences:
            # Skip empty sentences
            if not sent.strip():
                continue
                
            # Restore periods in abbreviations
            sent = sent.replace('<!PERIOD!>', '.')
            
            # Remove excessive whitespace
            sent = re.sub(r'\s+', ' ', sent).strip()
            
            clean_sentences.append(sent)
        
        return clean_sentences
    
    def split(self, text: str) -> List[str]:
        """
        Split text into sentences.
        
        Args:
            text: Text to split
            
        Returns:
            List of sentences
        """
        if not text or not text.strip():
            return []
            
        try:
            # Try spaCy first if available
            if SPACY_AVAILABLE and self._get_spacy_nlp() is not None:
                sentences = self._split_with_spacy(text)
            else:
                sentences = self._split_with_rules(text)
                
            # Filter out empty sentences and those without alphabetic characters
            sentences = [
                s for s in sentences 
                if s.strip() and re.search(r'[a-zA-Z]', s)
            ]
            
            self.logger.debug(f"Split text into {len(sentences)} sentences")
            return sentences
            
        except Exception as e:
            self.logger.error(f"Sentence splitting failed: {str(e)}")
            # Fall back to simple splitting on periods if everything else fails
            fallback_sentences = [s.strip() for s in re.split(r'\.+', text) if s.strip()]
            self.logger.warning(f"Used fallback sentence splitting: {len(fallback_sentences)} sentences")
            return fallback_sentences 