"""
LEXICON Part-of-Speech Tagger

Provides POS tagging functionality for the LEXICON pipeline.
"""

import re
import string
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


class POSTagger:
    """
    Part-of-Speech tagging for LEXICON.
    
    Uses spaCy if available, otherwise falls back to a simple rule-based approach.
    """
    
    def __init__(self, config: LexiconConfig):
        """
        Initialize the POS tagger.
        
        Args:
            config: LEXICON configuration
        """
        self.config = config
        self.logger = get_logger("nlp.pos_tagger")
        self._spacy_nlp = None
        
        if not SPACY_AVAILABLE:
            self.logger.warning(
                "spaCy not available - using simple rule-based POS tagging"
            )
    
    def _get_spacy_nlp(self, model="en_core_web_sm"):
        """Get or initialize spaCy NLP model for POS tagging."""
        if not SPACY_AVAILABLE:
            return None
            
        if self._spacy_nlp is None:
            try:
                # Load model with focus on POS components
                self._spacy_nlp = spacy.load(model, disable=["ner", "textcat"])
                self.logger.debug(f"Loaded spaCy model for POS tagging: {model}")
            except Exception as e:
                self.logger.warning(
                    f"Failed to load spaCy model {model}: {str(e)}. "
                    "Falling back to rule-based POS tagging."
                )
                self._spacy_nlp = None
        
        return self._spacy_nlp
    
    def _tag_with_spacy(self, text: str) -> List[Dict[str, Any]]:
        """
        Tag text with spaCy.
        
        Args:
            text: Text to tag
            
        Returns:
            List of token dictionaries
        """
        nlp = self._get_spacy_nlp()
        doc = nlp(text)
        
        tokens = []
        for token in doc:
            token_info = {
                "text": token.text,
                "start": token.idx,
                "end": token.idx + len(token.text),
                "pos": token.pos_,
                "tag": token.tag_,
                "dep": token.dep_,
                "lemma": token.lemma_
            }
            tokens.append(token_info)
        
        return tokens
    
    def _tag_with_rules(self, text: str) -> List[Dict[str, Any]]:
        """
        Tag text using simple rules.
        
        This is a very simplistic approach that only distinguishes basic POS categories.
        
        Args:
            text: Text to tag
            
        Returns:
            List of token dictionaries with minimal POS information
        """
        # Basic tokenization by whitespace and punctuation
        words = []
        current_word = ""
        current_start = 0
        
        for i, char in enumerate(text):
            if char.isalnum() or char == "'":
                if not current_word:
                    current_start = i
                current_word += char
            else:
                if current_word:
                    words.append((current_word, current_start, current_start + len(current_word)))
                    current_word = ""
                
                if not char.isspace():
                    words.append((char, i, i + 1))
        
        # Add any remaining word
        if current_word:
            words.append((current_word, current_start, current_start + len(current_word)))
        
        # Simple rules for POS tagging
        tokens = []
        for word, start, end in words:
            # Skip whitespace
            if word.isspace():
                continue
                
            # Default token
            token_info = {
                "text": word,
                "start": start,
                "end": end,
                "pos": "X",  # Unknown
                "tag": "X",
                "dep": "dep",  # Default dependency
                "lemma": word.lower()
            }
            
            # Apply simple rules
            if word in string.punctuation:
                token_info["pos"] = "PUNCT"
                token_info["tag"] = "PUNCT"
                
            elif word.isdigit():
                token_info["pos"] = "NUM"
                token_info["tag"] = "CD"
                
            elif word.lower() in ["the", "a", "an"]:
                token_info["pos"] = "DET"
                token_info["tag"] = "DT"
                
            elif word.lower() in ["in", "on", "at", "by", "with", "from", "to", "of"]:
                token_info["pos"] = "ADP"
                token_info["tag"] = "IN"
                
            elif word.lower() in ["and", "or", "but", "nor", "yet", "so"]:
                token_info["pos"] = "CCONJ"
                token_info["tag"] = "CC"
                
            elif word.lower() in ["i", "you", "he", "she", "it", "we", "they", "me", "him", "her", "us", "them"]:
                token_info["pos"] = "PRON"
                token_info["tag"] = "PRP"
                
            elif word.lower().endswith(("ed")):
                token_info["pos"] = "VERB"
                token_info["tag"] = "VBD"
                
            elif word.lower().endswith(("ing")):
                token_info["pos"] = "VERB"
                token_info["tag"] = "VBG"
                
            elif word.lower().endswith(("ly")):
                token_info["pos"] = "ADV"
                token_info["tag"] = "RB"
                
            elif word[0].isupper() and start > 0:
                token_info["pos"] = "PROPN"
                token_info["tag"] = "NNP"
                
            elif word.isalpha():
                token_info["pos"] = "NOUN"  # Default to noun for unknown words
                token_info["tag"] = "NN"
            
            tokens.append(token_info)
        
        return tokens
    
    def tag(self, text: str) -> List[Dict[str, Any]]:
        """
        Tag text with part-of-speech information.
        
        Args:
            text: Text to tag
            
        Returns:
            List of token dictionaries with keys:
            - text: Token text
            - start: Start character position
            - end: End character position
            - pos: Coarse-grained POS tag
            - tag: Fine-grained POS tag
            - dep: Dependency relation
            - lemma: Base form of the word
        """
        if not text or not text.strip():
            return []
            
        try:
            # Try spaCy first if available
            if SPACY_AVAILABLE and self._get_spacy_nlp() is not None:
                tokens = self._tag_with_spacy(text)
            else:
                tokens = self._tag_with_rules(text)
            
            self.logger.debug(f"Tagged {len(tokens)} tokens")
            return tokens
            
        except Exception as e:
            self.logger.error(f"POS tagging failed: {str(e)}")
            # Return minimal tokenization in case of failure
            words = text.split()
            position = 0
            tokens = []
            
            for word in words:
                # Find actual position in text
                word_pos = text.find(word, position)
                if word_pos >= 0:
                    position = word_pos + len(word)
                    tokens.append({
                        "text": word,
                        "start": word_pos,
                        "end": word_pos + len(word),
                        "pos": "X",
                        "tag": "X",
                        "dep": "dep",
                        "lemma": word.lower()
                    })
            
            self.logger.warning(f"Used fallback POS tagging: {len(tokens)} tokens")
            return tokens
    
    def tag_batch(self, texts: List[str]) -> List[List[Dict[str, Any]]]:
        """
        Tag multiple texts.
        
        Args:
            texts: List of texts to tag
            
        Returns:
            List of token lists, one per text
        """
        results = []
        
        # For spaCy, we can use pipe for efficiency
        if SPACY_AVAILABLE and self._get_spacy_nlp() is not None:
            nlp = self._get_spacy_nlp()
            docs = list(nlp.pipe(texts))
            
            for doc in docs:
                tokens = []
                for token in doc:
                    token_info = {
                        "text": token.text,
                        "start": token.idx,
                        "end": token.idx + len(token.text),
                        "pos": token.pos_,
                        "tag": token.tag_,
                        "dep": token.dep_,
                        "lemma": token.lemma_
                    }
                    tokens.append(token_info)
                results.append(tokens)
        else:
            # Fall back to processing each text individually
            for text in texts:
                results.append(self.tag(text))
        
        return results 