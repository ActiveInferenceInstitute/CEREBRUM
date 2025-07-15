"""
LEXICON Paraphrase Generator

Generates micro-paraphrases for text segments.
"""

import time
import json
import os
from typing import List, Dict, Any, Optional, Tuple
from dataclasses import dataclass, field
import logging
import hashlib
import re # Added missing import

from ..core.config import LexiconConfig
from ..core.logging import get_logger, LoggingTimer
from ..core.exceptions import ProcessingError, ModelError
from ..declension.tagger import CasedSegment
from .prompt_templates import get_paraphrase_prompt, get_quality_prompt
from .cache import get_cache, save_to_cache

from src.llm.OpenRouter import OpenRouterClient, OpenRouterConfig


@dataclass
class ParaphrasedSegment:
    """A segment with generated paraphrases."""
    segment_id: str
    text: str
    
    # Original properties from CasedSegment
    speaker: Optional[str] = None
    timestamp: Optional[float] = None
    entities: List[Dict[str, Any]] = field(default_factory=list)
    
    # Case assignments
    nominative: List[Dict[str, Any]] = field(default_factory=list)
    accusative: List[Dict[str, Any]] = field(default_factory=list)
    genitive: List[Dict[str, Any]] = field(default_factory=list)
    dative: List[Dict[str, Any]] = field(default_factory=list)
    locative: List[Dict[str, Any]] = field(default_factory=list)
    instrumental: List[Dict[str, Any]] = field(default_factory=list)
    ablative: List[Dict[str, Any]] = field(default_factory=list)
    vocative: List[Dict[str, Any]] = field(default_factory=list)
    
    # Paraphrases
    paraphrases: List[Dict[str, Any]] = field(default_factory=list)
    
    # Additional metadata
    metadata: Dict[str, Any] = field(default_factory=dict)


class ParaphraseGenerator:
    """
    Micro-paraphrase generator for LEXICON.
    
    Generates concise alternative phrasings of text segments, focusing on
    key case-declined entities.
    """
    
    def __init__(self, openrouter: OpenRouterClient, config: LexiconConfig):
        """
        Initialize the paraphrase generator.
        
        Args:
            openrouter: OpenRouter client for LLM operations
            config: LEXICON configuration
        """
        self.openrouter = openrouter
        self.config = config
        self.logger = get_logger("paraphrase.generator")
        
        # Initialize cache directory
        self.cache_dir = config.cache_dir / "paraphrases"
        os.makedirs(self.cache_dir, exist_ok=True)
        
        # Track performance metrics
        self.metrics = {
            "segments_processed": 0,
            "paraphrases_generated": 0,
            "cache_hits": 0,
            "cache_misses": 0,
            "llm_calls": 0,
            "total_duration": 0.0
        }
    
    def _get_cache_key(self, text: str) -> str:
        """
        Generate a cache key for a text segment.
        
        Args:
            text: Text segment
            
        Returns:
            MD5 hash for cache key
        """
        hash_obj = hashlib.md5(text.encode())
        return hash_obj.hexdigest()
    
    def _generate_paraphrases_with_llm(self, segment: CasedSegment) -> List[Dict[str, Any]]:
        """
        Generate paraphrases using OpenRouter LLM.
        
        Args:
            segment: Cased segment to paraphrase
            
        Returns:
            List of paraphrase dictionaries
        """
        # Track metrics
        self.metrics["llm_calls"] += 1
        
        # Extract key entities for paraphrase focus
        key_entities = []
        
        # Prioritize nominative and accusative cases
        for entity in segment.nominative:
            key_entities.append({
                "text": entity.get("text", ""),
                "case": "nominative",
                "confidence": entity.get("confidence", 0.0)
            })
        
        for entity in segment.accusative:
            key_entities.append({
                "text": entity.get("text", ""),
                "case": "accusative",
                "confidence": entity.get("confidence", 0.0)
            })
        
        # Add high-confidence entities from other cases
        for case in ["genitive", "dative", "locative", "instrumental", "ablative", "vocative"]:
            for entity in getattr(segment, case):
                if entity.get("confidence", 0.0) >= 0.8:
                    key_entities.append({
                        "text": entity.get("text", ""),
                        "case": case,
                        "confidence": entity.get("confidence", 0.0)
                    })
        
        # Generate the prompt
        prompt = get_paraphrase_prompt(
            segment.text, 
            key_entities[:3]  # Limit to top 3 entities for focus
        )
        
        try:
            # Call OpenRouter with the prompt
            model = self.config.fallback_models.get("paraphrase", self.config.default_model)
            response = self.openrouter.simple_chat(prompt, model=model)
            
            # Parse response to extract paraphrases
            paraphrases = self._parse_paraphrase_response(response)
            
            # Check quality and filter if needed
            if paraphrases:
                quality_prompt = get_quality_prompt(segment.text, paraphrases)
                quality_response = self.openrouter.simple_chat(quality_prompt, model=model)
                filtered_paraphrases = self._filter_by_quality(paraphrases, quality_response)
                
                return filtered_paraphrases
            else:
                return []
                
        except Exception as e:
            self.logger.error(f"Paraphrase generation failed: {str(e)}")
            return []
    
    def _parse_paraphrase_response(self, response: str) -> List[Dict[str, Any]]:
        """
        Parse LLM response to extract paraphrases.
        
        Args:
            response: Raw response from OpenRouter
            
        Returns:
            List of paraphrase dictionaries
        """
        paraphrases = []
        
        try:
            # Look for JSON in response
            json_match = None
            
            # Try different JSON extraction patterns
            patterns = [
                r'```json\s*(.*?)\s*```',  # Code block JSON
                r'```\s*(.*?)\s*```',       # Any code block
                r'(\[\s*\{.*\}\s*\])'       # JSON array
            ]
            
            for pattern in patterns:
                json_match = re.search(pattern, response, re.DOTALL)
                if json_match:
                    break
            
            if json_match:
                json_str = json_match.group(1)
                parsed = json.loads(json_str)
                
                # Handle both array and object formats
                if isinstance(parsed, list):
                    for item in parsed:
                        if isinstance(item, dict) and "text" in item:
                            paraphrases.append(item)
                elif isinstance(parsed, dict) and "paraphrases" in parsed:
                    for item in parsed["paraphrases"]:
                        if isinstance(item, dict) and "text" in item:
                            paraphrases.append(item)
            else:
                # Fall back to line-by-line extraction
                for line in response.split('\n'):
                    line = line.strip()
                    # Skip empty lines and headings
                    if not line or line.startswith('#'):
                        continue
                        
                    # Remove list markers and parse as paraphrase
                    clean_line = re.sub(r'^\d+[\.\)]\s*', '', line)
                    if clean_line:
                        paraphrases.append({"text": clean_line, "quality": 0.7})
            
            # Ensure consistent format for all paraphrases
            for p in paraphrases:
                if "quality" not in p:
                    p["quality"] = 0.7
            
            return paraphrases
            
        except Exception as e:
            self.logger.error(f"Failed to parse paraphrases: {str(e)}")
            return []
    
    def _filter_by_quality(self, paraphrases: List[Dict[str, Any]], 
                         quality_response: str) -> List[Dict[str, Any]]:
        """
        Filter paraphrases by quality assessment.
        
        Args:
            paraphrases: List of paraphrases
            quality_response: Quality assessment response
            
        Returns:
            Filtered list of paraphrases
        """
        try:
            # Try to parse quality assessment as JSON
            json_match = re.search(r'```json\s*(.*?)\s*```', quality_response, re.DOTALL)
            
            if json_match:
                quality_data = json.loads(json_match.group(1))
                
                # Check for assessments array
                if "assessments" in quality_data and isinstance(quality_data["assessments"], list):
                    assessments = quality_data["assessments"]
                    
                    # Update quality scores in paraphrases
                    for i, assessment in enumerate(assessments):
                        if i < len(paraphrases) and "quality" in assessment:
                            paraphrases[i]["quality"] = assessment["quality"]
            
            # Filter out low-quality paraphrases
            return [p for p in paraphrases if p.get("quality", 0.0) >= 0.6]
            
        except Exception as e:
            self.logger.warning(f"Quality filter failed: {str(e)}")
            return paraphrases  # Return original paraphrases if filtering fails
    
    def generate_for_segment(self, segment: CasedSegment) -> ParaphrasedSegment:
        """
        Generate paraphrases for a single segment.
        
        Args:
            segment: Cased segment to paraphrase
            
        Returns:
            Segment with paraphrases
        """
        start_time = time.time()
        
        # Generate cache key
        cache_key = self._get_cache_key(segment.text)
        cache_file = self.cache_dir / f"{cache_key}.json"
        
        # Try to get from cache
        cached_paraphrases = get_cache(cache_file)
        
        if cached_paraphrases:
            self.metrics["cache_hits"] += 1
            paraphrases = cached_paraphrases
        else:
            self.metrics["cache_misses"] += 1
            # Generate new paraphrases
            with LoggingTimer(self.logger, "LLM paraphrase generation"):
                paraphrases = self._generate_paraphrases_with_llm(segment)
                
            # Save to cache
            save_to_cache(cache_file, paraphrases)
        
        # Create paraphrased segment
        paraphrased_segment = ParaphrasedSegment(
            segment_id=segment.segment_id,
            text=segment.text,
            speaker=segment.speaker,
            timestamp=segment.timestamp,
            entities=segment.entities,
            nominative=segment.nominative,
            accusative=segment.accusative,
            genitive=segment.genitive,
            dative=segment.dative,
            locative=segment.locative,
            instrumental=segment.instrumental,
            ablative=segment.ablative,
            vocative=segment.vocative,
            paraphrases=paraphrases,
            metadata={
                **segment.metadata,
                "paraphrase_time": time.time() - start_time
            }
        )
        
        # Update metrics
        duration = time.time() - start_time
        self.metrics["segments_processed"] += 1
        self.metrics["paraphrases_generated"] += len(paraphrases)
        self.metrics["total_duration"] += duration
        
        self.logger.debug(
            f"Generated {len(paraphrases)} paraphrases for segment {segment.segment_id} "
            f"in {duration:.2f}s"
        )
        
        return paraphrased_segment
    
    def generate(self, segments: List[CasedSegment]) -> List[ParaphrasedSegment]:
        """
        Generate paraphrases for multiple segments.
        
        Args:
            segments: List of cased segments
            
        Returns:
            List of segments with paraphrases
        """
        start_time = time.time()
        self.logger.info(f"Starting paraphrase generation for {len(segments)} segments")
        
        paraphrased_segments = []
        
        for i, segment in enumerate(segments):
            try:
                self.logger.debug(f"Processing segment {i+1}/{len(segments)}: {segment.segment_id}")
                paraphrased = self.generate_for_segment(segment)
                paraphrased_segments.append(paraphrased)
                
                # Log progress for large batches
                if len(segments) > 10 and (i + 1) % 10 == 0:
                    self.logger.info(f"Processed {i + 1}/{len(segments)} segments")
                    
            except Exception as e:
                self.logger.error(f"Failed to process segment {segment.segment_id}: {str(e)}")
                # Add a copy of the original segment without paraphrases to maintain ordering
                paraphrased_segments.append(ParaphrasedSegment(
                    segment_id=segment.segment_id,
                    text=segment.text,
                    speaker=segment.speaker,
                    timestamp=segment.timestamp,
                    entities=segment.entities,
                    nominative=segment.nominative,
                    accusative=segment.accusative,
                    genitive=segment.genitive,
                    dative=segment.dative,
                    locative=segment.locative,
                    instrumental=segment.instrumental,
                    ablative=segment.ablative,
                    vocative=segment.vocative,
                    metadata={"error": str(e)}
                ))
        
        # Log summary
        duration = time.time() - start_time
        paraphrase_count = sum(len(s.paraphrases) for s in paraphrased_segments)
        
        self.logger.info(
            f"Paraphrase generation completed: {len(paraphrased_segments)} segments, "
            f"{paraphrase_count} paraphrases in {duration:.2f}s"
        )
        
        return paraphrased_segments
    
    def get_metrics(self) -> Dict[str, Any]:
        """
        Get performance metrics for the paraphrase generator.
        
        Returns:
            Dictionary of performance metrics
        """
        if self.metrics["segments_processed"] > 0:
            avg_time = self.metrics["total_duration"] / self.metrics["segments_processed"]
            avg_paraphrases = self.metrics["paraphrases_generated"] / self.metrics["segments_processed"]
        else:
            avg_time = 0.0
            avg_paraphrases = 0.0
            
        return {
            **self.metrics,
            "avg_time_per_segment": avg_time,
            "avg_paraphrases_per_segment": avg_paraphrases,
        } 