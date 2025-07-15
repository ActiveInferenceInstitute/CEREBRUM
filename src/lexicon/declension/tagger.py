"""
LEXICON Case Tagger

Applies CEREBRUM's 8-case linguistic system to text segments.
"""

import re
import time
import json
from typing import List, Dict, Any, Optional, Tuple, Set, Union
from dataclasses import dataclass, field
import logging
from datetime import datetime

from ..core.config import LexiconConfig
from ..core.logging import get_logger, LoggingTimer
from ..core.exceptions import ProcessingError, ModelError
from ..nlp.preprocessor import ProcessedSegment
from .rules import CaseRules
from .openrouter_client import declension_prompt, extract_declensions

from src.llm.OpenRouter import OpenRouterClient, OpenRouterConfig


@dataclass
class CasedSegment:
    """A text segment with case declension applied."""
    segment_id: str
    text: str
    
    # Original properties from ProcessedSegment
    speaker: Optional[str] = None
    timestamp: Optional[float] = None
    entities: List[Dict[str, Any]] = field(default_factory=list)
    
    # Case-specific properties
    nominative: List[Dict[str, Any]] = field(default_factory=list)  # Agents/doers
    accusative: List[Dict[str, Any]] = field(default_factory=list)  # Objects/recipients
    genitive: List[Dict[str, Any]] = field(default_factory=list)    # Possessive/source
    dative: List[Dict[str, Any]] = field(default_factory=list)      # Recipients
    locative: List[Dict[str, Any]] = field(default_factory=list)    # Location context
    instrumental: List[Dict[str, Any]] = field(default_factory=list)  # Means/tools
    ablative: List[Dict[str, Any]] = field(default_factory=list)    # Source/origin
    vocative: List[Dict[str, Any]] = field(default_factory=list)    # Address/summon
    
    # Additional metadata
    metadata: Dict[str, Any] = field(default_factory=dict)


class CaseTagger:
    """
    Case declension tagger for LEXICON.
    
    Applies CEREBRUM's 8-case linguistic system to text segments:
    - NOM (Nominative): Agents/subjects in statements
    - ACC (Accusative): Objects in statements
    - GEN (Genitive): Possessive/source relations
    - DAT (Dative): Recipients of actions
    - LOC (Locative): Situational context
    - INS (Instrumental): Transformative tools/means
    - ABL (Ablative): Origins/sources
    - VOC (Vocative): Direct address/summon
    """
    
    def __init__(self, openrouter: OpenRouterClient, config: LexiconConfig):
        """
        Initialize the case tagger.
        
        Args:
            openrouter: OpenRouter client for LLM operations
            config: LEXICON configuration
        """
        self.openrouter = openrouter
        self.config = config
        self.logger = get_logger("declension.tagger")
        
        # Load rules engine
        self.rules = CaseRules()
        
        # Track performance metrics
        self.metrics = {
            "segments_processed": 0,
            "entities_tagged": 0,
            "llm_calls": 0,
            "rule_applications": 0,
            "total_duration": 0.0
        }
    
    def _preprocess_segment(self, segment: ProcessedSegment) -> Dict[str, Any]:
        """
        Prepare a segment for case tagging.
        
        Args:
            segment: Processed text segment
            
        Returns:
            Dictionary with extracted information for case tagging
        """
        # Extract entities from the segment
        entities = []
        for entity in segment.entities:
            # Skip entities without text
            if not entity.get("text"):
                continue
                
            # Extract relevant information
            entity_data = {
                "text": entity.get("text", ""),
                "type": entity.get("type", ""),
                "start": entity.get("start", 0),
                "end": entity.get("end", 0)
            }
            entities.append(entity_data)
        
        # Extract POS patterns from segment
        pos_patterns = []
        for i, tag in enumerate(segment.pos_tags):
            if i > 0:
                prev_tag = segment.pos_tags[i-1]
            else:
                prev_tag = None
                
            if i < len(segment.pos_tags) - 1:
                next_tag = segment.pos_tags[i+1]
            else:
                next_tag = None
            
            # Analyze POS patterns
            if tag.get("pos") == "VERB":
                # Look for subject-verb pattern
                subject = None
                
                # Check backward for subject
                for j in range(i-1, -1, -1):
                    prev = segment.pos_tags[j]
                    if prev.get("pos") in ["NOUN", "PROPN", "PRON"]:
                        subject = prev
                        break
                
                # Check forward for object
                object_found = None
                for j in range(i+1, len(segment.pos_tags)):
                    next_pos = segment.pos_tags[j]
                    if next_pos.get("pos") in ["NOUN", "PROPN", "PRON"]:
                        object_found = next_pos
                        break
                
                if subject:
                    pos_patterns.append({
                        "pattern": "SUBJECT_VERB",
                        "subject": subject.get("text"),
                        "verb": tag.get("text"),
                        "object": object_found.get("text") if object_found else None
                    })
            
            # Look for preposition + noun pattern (often indicates location)
            if tag.get("pos") == "ADP" and next_tag and next_tag.get("pos") in ["NOUN", "PROPN"]:
                pos_patterns.append({
                    "pattern": "PREP_NOUN",
                    "preposition": tag.get("text"),
                    "noun": next_tag.get("text")
                })
        
        # Prepare segment data
        return {
            "segment_id": segment.segment_id,
            "text": segment.text,
            "speaker": segment.speaker,
            "timestamp": segment.timestamp,
            "entities": entities,
            "pos_patterns": pos_patterns
        }
    
    def _apply_rules(self, segment_data: Dict[str, Any]) -> Dict[str, List[Dict[str, Any]]]:
        """
        Apply rule-based case tagging.
        
        Args:
            segment_data: Preprocessed segment data
            
        Returns:
            Dictionary with case assignments
        """
        text = segment_data["text"]
        entities = segment_data["entities"]
        pos_patterns = segment_data["pos_patterns"]
        
        # Initialize cases
        cases = {
            "nominative": [],
            "accusative": [],
            "genitive": [],
            "dative": [],
            "locative": [],
            "instrumental": [],
            "ablative": [],
            "vocative": []
        }
        
        # Apply rules to assign cases
        for pattern in pos_patterns:
            if pattern["pattern"] == "SUBJECT_VERB":
                # Subject is likely nominative
                if pattern["subject"]:
                    cases["nominative"].append({
                        "text": pattern["subject"],
                        "confidence": 0.8,
                        "rule": "subject_verb_pattern"
                    })
                
                # Object is likely accusative
                if pattern["object"]:
                    cases["accusative"].append({
                        "text": pattern["object"],
                        "confidence": 0.7,
                        "rule": "verb_object_pattern"
                    })
            
            elif pattern["pattern"] == "PREP_NOUN":
                # Map prepositions to cases
                prep = pattern["preposition"].lower()
                noun = pattern["noun"]
                
                if prep in ["in", "at", "on"]:
                    cases["locative"].append({
                        "text": noun,
                        "confidence": 0.8,
                        "rule": "locative_preposition"
                    })
                elif prep in ["with", "by", "using"]:
                    cases["instrumental"].append({
                        "text": noun,
                        "confidence": 0.7,
                        "rule": "instrumental_preposition"
                    })
                elif prep in ["from", "out of"]:
                    cases["ablative"].append({
                        "text": noun,
                        "confidence": 0.7,
                        "rule": "ablative_preposition"
                    })
                elif prep in ["to", "for"]:
                    cases["dative"].append({
                        "text": noun,
                        "confidence": 0.7,
                        "rule": "dative_preposition"
                    })
                elif prep in ["of"]:
                    cases["genitive"].append({
                        "text": noun,
                        "confidence": 0.7,
                        "rule": "genitive_preposition"
                    })
        
        # Additional entity-based rules
        for entity in entities:
            entity_text = entity["text"]
            entity_type = entity["type"]
            
            # Look for vocative case (direct address)
            if re.search(r'^[A-Z][a-z]+!', entity_text) or re.search(r'^Hey [A-Z][a-z]+', text):
                cases["vocative"].append({
                    "text": entity_text,
                    "confidence": 0.7,
                    "rule": "direct_address"
                })
            
            # Type-based rules
            if entity_type == "PERSON" and any(p["pattern"] == "SUBJECT_VERB" and p["subject"] == entity_text for p in pos_patterns):
                cases["nominative"].append({
                    "text": entity_text,
                    "confidence": 0.8,
                    "rule": "person_subject"
                })
            
            elif entity_type == "LOC" or entity_type == "GPE":
                cases["locative"].append({
                    "text": entity_text,
                    "confidence": 0.75,
                    "rule": "location_entity"
                })
            
            elif entity_type == "ORG" and "by " + entity_text.lower() in text.lower():
                cases["instrumental"].append({
                    "text": entity_text,
                    "confidence": 0.7,
                    "rule": "organization_by"
                })
        
        # Count rule applications
        self.metrics["rule_applications"] += sum(len(case_items) for case_items in cases.values())
        
        return cases
    
    def _apply_llm_declension(self, segment_data: Dict[str, Any]) -> Dict[str, List[Dict[str, Any]]]:
        """
        Apply case declension using OpenRouter LLM.
        
        Args:
            segment_data: Preprocessed segment data
            
        Returns:
            Dictionary with case assignments from LLM
        """
        text = segment_data["text"]
        
        # Track metrics
        self.metrics["llm_calls"] += 1
        
        # Generate prompt for the LLM
        prompt = declension_prompt(text)
        
        try:
            # Call OpenRouter with the prompt
            model = self.config.fallback_models.get("case_declension", self.config.default_model)
            response = self.openrouter.simple_chat(prompt, model=model)
            
            # Extract declensions from the response
            declensions = extract_declensions(response)
            
            return declensions
            
        except Exception as e:
            self.logger.error(f"LLM declension failed: {str(e)}")
            # Return empty declensions on failure
            return {
                "nominative": [],
                "accusative": [],
                "genitive": [],
                "dative": [],
                "locative": [],
                "instrumental": [],
                "ablative": [],
                "vocative": []
            }
    
    def _merge_declensions(self, rule_cases: Dict[str, List[Dict[str, Any]]], 
                         llm_cases: Dict[str, List[Dict[str, Any]]]) -> Dict[str, List[Dict[str, Any]]]:
        """
        Merge rule-based and LLM-based declensions.
        
        Args:
            rule_cases: Cases assigned by rules
            llm_cases: Cases assigned by LLM
            
        Returns:
            Merged case assignments
        """
        merged = {
            "nominative": [],
            "accusative": [],
            "genitive": [],
            "dative": [],
            "locative": [],
            "instrumental": [],
            "ablative": [],
            "vocative": []
        }
        
        # Track text items we've already processed
        processed = set()
        
        # Process each case
        for case in merged.keys():
            # Start with rule-based assignments
            for item in rule_cases.get(case, []):
                text = item.get("text", "").lower()
                if not text or text in processed:
                    continue
                    
                # Add to merged cases
                merged[case].append(item)
                processed.add(text)
            
            # Add LLM assignments that don't conflict
            for item in llm_cases.get(case, []):
                text = item.get("text", "").lower()
                if not text or text in processed:
                    continue
                    
                # Add to merged cases
                merged[case].append(item)
                processed.add(text)
        
        return merged
    
    def tag_segment(self, segment: ProcessedSegment) -> CasedSegment:
        """
        Apply case declension to a single segment.
        
        Args:
            segment: Processed text segment
            
        Returns:
            Segment with case declension applied
        """
        start_time = time.time()
        
        # Prepare segment data
        segment_data = self._preprocess_segment(segment)
        
        # Apply rule-based tagging
        with LoggingTimer(self.logger, "Rule-based tagging"):
            rule_cases = self._apply_rules(segment_data)
        
        # Apply LLM declension
        with LoggingTimer(self.logger, "LLM declension"):
            llm_cases = self._apply_llm_declension(segment_data)
        
        # Merge results
        with LoggingTimer(self.logger, "Merge declensions"):
            merged_cases = self._merge_declensions(rule_cases, llm_cases)
        
        # Create cased segment
        cased_segment = CasedSegment(
            segment_id=segment.segment_id,
            text=segment.text,
            speaker=segment.speaker,
            timestamp=segment.timestamp,
            entities=segment.entities,
            nominative=merged_cases["nominative"],
            accusative=merged_cases["accusative"],
            genitive=merged_cases["genitive"],
            dative=merged_cases["dative"],
            locative=merged_cases["locative"],
            instrumental=merged_cases["instrumental"],
            ablative=merged_cases["ablative"],
            vocative=merged_cases["vocative"],
            metadata={
                "processing_time": time.time() - start_time,
                "source_metadata": segment.metadata
            }
        )
        
        # Update metrics
        duration = time.time() - start_time
        self.metrics["segments_processed"] += 1
        self.metrics["entities_tagged"] += sum(len(cased_segment.__dict__[case]) for case in 
                                            ["nominative", "accusative", "genitive", "dative", 
                                             "locative", "instrumental", "ablative", "vocative"])
        self.metrics["total_duration"] += duration
        
        return cased_segment
    
    def tag(self, segments: List[ProcessedSegment]) -> List[CasedSegment]:
        """
        Apply case declension to multiple segments.
        
        Args:
            segments: List of processed text segments
            
        Returns:
            List of segments with case declension applied
        """
        start_time = time.time()
        self.logger.info(f"Starting case declension for {len(segments)} segments")
        
        # Process each segment
        cased_segments = []
        for i, segment in enumerate(segments):
            try:
                self.logger.debug(f"Processing segment {i+1}/{len(segments)}: {segment.segment_id}")
                cased_segment = self.tag_segment(segment)
                cased_segments.append(cased_segment)
                
                # Log progress for large batches
                if len(segments) > 10 and (i + 1) % 10 == 0:
                    self.logger.info(f"Processed {i + 1}/{len(segments)} segments")
                    
            except Exception as e:
                self.logger.error(f"Failed to process segment {segment.segment_id}: {str(e)}")
                # Add an empty cased segment to maintain ordering
                cased_segments.append(CasedSegment(
                    segment_id=segment.segment_id,
                    text=segment.text,
                    speaker=segment.speaker,
                    timestamp=segment.timestamp,
                    entities=segment.entities,
                    metadata={"error": str(e)}
                ))
        
        # Log summary
        duration = time.time() - start_time
        entity_count = sum(len(s.nominative) + len(s.accusative) + len(s.genitive) + 
                          len(s.dative) + len(s.locative) + len(s.instrumental) + 
                          len(s.ablative) + len(s.vocative) for s in cased_segments)
        
        self.logger.info(
            f"Case declension completed: {len(cased_segments)} segments, "
            f"{entity_count} entities tagged in {duration:.2f}s"
        )
        
        return cased_segments
    
    def get_metrics(self) -> Dict[str, Any]:
        """
        Get performance metrics for the case tagger.
        
        Returns:
            Dictionary of performance metrics
        """
        # Calculate average processing time
        avg_time = 0.0
        if self.metrics["segments_processed"] > 0:
            avg_time = self.metrics["total_duration"] / self.metrics["segments_processed"]
        
        return {
            **self.metrics,
            "avg_time_per_segment": avg_time,
            "timestamp": datetime.now().isoformat()
        }
    
    def serialize_cased_segment(self, segment: CasedSegment) -> str:
        """
        Serialize a cased segment to JSON.
        
        Args:
            segment: Cased segment to serialize
            
        Returns:
            JSON string representation
        """
        segment_dict = {
            "segment_id": segment.segment_id,
            "text": segment.text,
            "speaker": segment.speaker,
            "timestamp": segment.timestamp,
            "entities": segment.entities,
            "cases": {
                "nominative": segment.nominative,
                "accusative": segment.accusative,
                "genitive": segment.genitive,
                "dative": segment.dative,
                "locative": segment.locative,
                "instrumental": segment.instrumental,
                "ablative": segment.ablative,
                "vocative": segment.vocative
            },
            "metadata": segment.metadata
        }
        
        return json.dumps(segment_dict, indent=2)
    
    @classmethod
    def deserialize_cased_segment(cls, json_str: str) -> CasedSegment:
        """
        Deserialize a cased segment from JSON.
        
        Args:
            json_str: JSON string representation
            
        Returns:
            Deserialized cased segment
        """
        segment_dict = json.loads(json_str)
        cases = segment_dict.get("cases", {})
        
        return CasedSegment(
            segment_id=segment_dict.get("segment_id", ""),
            text=segment_dict.get("text", ""),
            speaker=segment_dict.get("speaker"),
            timestamp=segment_dict.get("timestamp"),
            entities=segment_dict.get("entities", []),
            nominative=cases.get("nominative", []),
            accusative=cases.get("accusative", []),
            genitive=cases.get("genitive", []),
            dative=cases.get("dative", []),
            locative=cases.get("locative", []),
            instrumental=cases.get("instrumental", []),
            ablative=cases.get("ablative", []),
            vocative=cases.get("vocative", []),
            metadata=segment_dict.get("metadata", {})
        ) 