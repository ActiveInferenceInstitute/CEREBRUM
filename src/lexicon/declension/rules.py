"""
LEXICON Case Rules

Rule definitions for case declension tagging.
"""

from typing import Dict, List, Any, Optional, Tuple, Set
import re

class CaseRules:
    """
    Rule engine for applying CEREBRUM's case system.
    
    Provides pattern-based rules for assigning grammatical cases.
    """
    
    def __init__(self):
        """Initialize the case rules engine."""
        # Initialize rule definitions
        self.rules = self._build_rules()
    
    def _build_rules(self) -> Dict[str, List[Dict[str, Any]]]:
        """
        Build rule sets for each case.
        
        Returns:
            Dictionary of rules indexed by case
        """
        rules = {
            "nominative": [
                {
                    "name": "subject_position",
                    "description": "Typical subject position at start of sentence",
                    "pattern": r'^([A-Z][a-zA-Z\s]+)\s+(?:is|are|was|were|has|have|had|will|shall|may|can|do|does|did)',
                    "confidence": 0.8
                },
                {
                    "name": "clear_agent",
                    "description": "Noun performs an action",
                    "pattern": r'([A-Z][a-zA-Z\s]+)\s+(?:wrote|said|claimed|argued|suggested|proposed|implemented|created|developed|built)',
                    "confidence": 0.85
                }
            ],
            "accusative": [
                {
                    "name": "direct_object",
                    "description": "Direct object of a verb",
                    "pattern": r'(?:see|saw|found|discovered|identified|observed|analyzed|examined|studied|investigated)\s+([a-zA-Z\s]+)',
                    "confidence": 0.8
                },
                {
                    "name": "passive_subject",
                    "description": "Subject of passive voice sentence",
                    "pattern": r'([a-zA-Z\s]+)\s+(?:is|are|was|were)\s+(?:seen|found|discovered|identified|observed|analyzed|examined|studied|investigated)',
                    "confidence": 0.75
                }
            ],
            "genitive": [
                {
                    "name": "possessive_of",
                    "description": "Possessive using 'of'",
                    "pattern": r'(?:the|an?)\s+[a-zA-Z\s]+\s+of\s+([a-zA-Z\s]+)',
                    "confidence": 0.85
                },
                {
                    "name": "possessive_s",
                    "description": "Possessive using apostrophe s",
                    "pattern": r"([A-Z][a-zA-Z\s]+)'s",
                    "confidence": 0.9
                }
            ],
            "dative": [
                {
                    "name": "to_recipient",
                    "description": "Recipient with 'to'",
                    "pattern": r'(?:gave|sent|delivered|provided|offered|showed|presented|explained|described)\s+[a-zA-Z\s]+\s+to\s+([a-zA-Z\s]+)',
                    "confidence": 0.85
                },
                {
                    "name": "for_beneficiary",
                    "description": "Beneficiary with 'for'",
                    "pattern": r'(?:made|created|designed|developed|built|prepared|reserved|saved)\s+[a-zA-Z\s]+\s+for\s+([a-zA-Z\s]+)',
                    "confidence": 0.75
                }
            ],
            "locative": [
                {
                    "name": "in_location",
                    "description": "Location with 'in'",
                    "pattern": r'in\s+(?:the\s+)?([a-zA-Z\s]+)',
                    "confidence": 0.85
                },
                {
                    "name": "at_location",
                    "description": "Location with 'at'",
                    "pattern": r'at\s+(?:the\s+)?([a-zA-Z\s]+)',
                    "confidence": 0.85
                },
                {
                    "name": "on_location",
                    "description": "Location with 'on'",
                    "pattern": r'on\s+(?:the\s+)?([a-zA-Z\s]+)',
                    "confidence": 0.8
                }
            ],
            "instrumental": [
                {
                    "name": "with_instrument",
                    "description": "Instrument with 'with'",
                    "pattern": r'with\s+(?:the\s+)?([a-zA-Z\s]+)',
                    "confidence": 0.8
                },
                {
                    "name": "using_instrument",
                    "description": "Instrument with 'using'",
                    "pattern": r'using\s+(?:the\s+)?([a-zA-Z\s]+)',
                    "confidence": 0.85
                },
                {
                    "name": "by_means",
                    "description": "Means with 'by'",
                    "pattern": r'by\s+(?:means\s+of\s+)?(?:the\s+)?([a-zA-Z\s]+)',
                    "confidence": 0.75
                }
            ],
            "ablative": [
                {
                    "name": "from_source",
                    "description": "Source with 'from'",
                    "pattern": r'from\s+(?:the\s+)?([a-zA-Z\s]+)',
                    "confidence": 0.85
                },
                {
                    "name": "out_of_source",
                    "description": "Source with 'out of'",
                    "pattern": r'out\s+of\s+(?:the\s+)?([a-zA-Z\s]+)',
                    "confidence": 0.8
                }
            ],
            "vocative": [
                {
                    "name": "direct_address",
                    "description": "Direct address with comma",
                    "pattern": r'([A-Z][a-zA-Z]+),\s',
                    "confidence": 0.9
                },
                {
                    "name": "hey_address",
                    "description": "Address with 'hey'",
                    "pattern": r'(?:Hey|Hello|Hi),?\s+([A-Z][a-zA-Z]+)',
                    "confidence": 0.9
                },
                {
                    "name": "exclamation",
                    "description": "Address with exclamation",
                    "pattern": r'([A-Z][a-zA-Z]+)!',
                    "confidence": 0.7
                }
            ]
        }
        
        return rules
    
    def apply_rules(self, text: str) -> Dict[str, List[Dict[str, Any]]]:
        """
        Apply all case rules to a text.
        
        Args:
            text: Text to analyze
            
        Returns:
            Dictionary of case assignments with confidence scores
        """
        results = {
            "nominative": [],
            "accusative": [],
            "genitive": [],
            "dative": [],
            "locative": [],
            "instrumental": [],
            "ablative": [],
            "vocative": []
        }
        
        # Apply each rule to the text
        for case, rule_list in self.rules.items():
            for rule in rule_list:
                # Apply pattern matching
                for match in re.finditer(rule["pattern"], text):
                    # Extract the matched entity
                    entity_text = match.group(1).strip()
                    
                    # Add to results if not empty
                    if entity_text:
                        results[case].append({
                            "text": entity_text,
                            "confidence": rule["confidence"],
                            "rule": rule["name"]
                        })
        
        return results
    
    def apply_rule(self, case: str, rule_name: str, text: str) -> List[Dict[str, Any]]:
        """
        Apply a specific rule to text.
        
        Args:
            case: Case to apply (nominative, accusative, etc.)
            rule_name: Name of the rule to apply
            text: Text to analyze
            
        Returns:
            List of matches with confidence scores
        """
        results = []
        
        # Find the specific rule
        for rule in self.rules.get(case, []):
            if rule["name"] == rule_name:
                # Apply pattern matching
                for match in re.finditer(rule["pattern"], text):
                    # Extract the matched entity
                    entity_text = match.group(1).strip()
                    
                    # Add to results if not empty
                    if entity_text:
                        results.append({
                            "text": entity_text,
                            "confidence": rule["confidence"],
                            "rule": rule["name"]
                        })
                
                break
        
        return results 