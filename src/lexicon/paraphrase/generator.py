"""
LEXICON Paraphrase Generator

Generates micro-paraphrases for text segments.
"""

import hashlib
import json
import math
import os
import re
import time
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional

from src.llm.OpenRouter import OpenRouterClient

from ..core.config import LexiconConfig
from ..core.logging import LoggingTimer, get_logger
from ..declension.tagger import CasedSegment
from .cache import get_cache, save_to_cache
from .prompt_templates import get_paraphrase_prompt, get_quality_prompt


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
    
    def _get_cache_key(self, segment: CasedSegment, prompt: str, model: str) -> str:
        """Bind cached successes to both prompts and the effective request settings."""
        router = self.openrouter.config
        identity = {
            "version": 2,
            "generation_prompt": prompt,
            "quality_prompt": get_quality_prompt(segment.text, []),
            "model": model,
            "base_url": router.base_url,
            "fallback_models": router.fallback_models,
            "temperature": router.temperature,
            "max_tokens": router.max_tokens,
        }
        encoded = json.dumps(identity, sort_keys=True, ensure_ascii=False).encode("utf-8")
        return "v2-" + hashlib.sha256(encoded).hexdigest()

    def _build_paraphrase_prompt(self, segment: CasedSegment) -> str:
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
        
        return get_paraphrase_prompt(segment.text, key_entities[:3])

    def _generate_paraphrases_with_llm(
        self, segment: CasedSegment, prompt: str, model: str
    ) -> List[Dict[str, Any]]:
        """Generate and assess paraphrases; request or protocol failures propagate."""
        self.metrics["llm_calls"] += 1
        response = self.openrouter.simple_chat(prompt, model=model)
        paraphrases = self._parse_paraphrase_response(response)
        if not paraphrases:
            return []

        quality_prompt = get_quality_prompt(segment.text, paraphrases)
        self.metrics["llm_calls"] += 1
        quality_response = self.openrouter.simple_chat(quality_prompt, model=model)
        return self._filter_by_quality(paraphrases, quality_response)
    
    @staticmethod
    def _parse_json_response(response: str) -> Any:
        """Accept raw JSON or a single JSON fence, never prose as a success."""
        if not isinstance(response, str) or not response.strip():
            raise ValueError("Empty paraphrase response")
        payload = response.strip()
        fenced = re.fullmatch(r"```(?:json)?\s*(.*?)\s*```", payload, re.DOTALL)
        if fenced:
            payload = fenced.group(1)
        try:
            return json.loads(payload)
        except json.JSONDecodeError as error:
            raise ValueError("Invalid paraphrase JSON response") from error

    @staticmethod
    def _validate_quality(value: Any) -> None:
        if (
            isinstance(value, bool)
            or not isinstance(value, (int, float))
            or not math.isfinite(value)
            or not 0.0 <= value <= 1.0
        ):
            raise ValueError("Paraphrase quality must be a finite number from 0 to 1")

    def _parse_paraphrase_response(self, response: str) -> List[Dict[str, Any]]:
        parsed = self._parse_json_response(response)
        paraphrases = parsed.get("paraphrases") if isinstance(parsed, dict) else parsed
        if not isinstance(paraphrases, list):
            raise ValueError("Paraphrase response must contain an array")
        for item in paraphrases:
            if (
                not isinstance(item, dict)
                or not isinstance(item.get("text"), str)
                or not item["text"].strip()
            ):
                raise ValueError("Each paraphrase must contain nonempty text")
            if "quality" in item:
                self._validate_quality(item["quality"])
        return paraphrases

    def _filter_by_quality(
        self, paraphrases: List[Dict[str, Any]], quality_response: str
    ) -> List[Dict[str, Any]]:
        quality_data = self._parse_json_response(quality_response)
        assessments = quality_data.get("assessments") if isinstance(quality_data, dict) else None
        if not isinstance(assessments, list):
            raise ValueError("Quality response must contain assessments")

        scores = {}
        for assessment in assessments:
            if not isinstance(assessment, dict):
                raise ValueError("Each quality assessment must be an object")
            identifier = assessment.get("id")
            if (
                type(identifier) is not int
                or not 1 <= identifier <= len(paraphrases)
                or identifier in scores
            ):
                raise ValueError("Quality assessment IDs must uniquely identify paraphrases")
            quality = assessment.get("quality")
            self._validate_quality(quality)
            scores[identifier] = quality
        if len(scores) != len(paraphrases):
            raise ValueError("Quality response must assess every paraphrase")
        return [
            {**paraphrase, "quality": scores[index]}
            for index, paraphrase in enumerate(paraphrases, start=1)
            if scores[index] >= 0.6
        ]
    
    def generate_for_segment(self, segment: CasedSegment) -> ParaphrasedSegment:
        """
        Generate paraphrases for a single segment.
        
        Args:
            segment: Cased segment to paraphrase
            
        Returns:
            Segment with paraphrases
        """
        start_time = time.time()
        
        prompt = self._build_paraphrase_prompt(segment)
        model = (
            self.config.fallback_models.get("paraphrase", self.config.default_model)
            or self.openrouter.config.default_model
        )
        cache_key = self._get_cache_key(segment, prompt, model)
        cache_file = self.cache_dir / f"{cache_key}.json"
        
        # Try to get from cache.
        # Guard with ``is not None`` (not truthiness) so a legitimately-cached
        # EMPTY list counts as a hit; otherwise an empty result would be treated
        # as a miss and needlessly regenerated every call.
        cached_paraphrases = get_cache(cache_file)
        
        if cached_paraphrases is not None:
            self.metrics["cache_hits"] += 1
            paraphrases = cached_paraphrases
        else:
            self.metrics["cache_misses"] += 1
            # Generate new paraphrases
            with LoggingTimer(self.logger, "LLM paraphrase generation"):
                paraphrases = self._generate_paraphrases_with_llm(segment, prompt, model)
                
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