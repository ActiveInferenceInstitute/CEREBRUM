"""
LEXICON Enhanced Entity Deduplicator

Advanced entity deduplication using multiple similarity metrics and intelligent merging.
"""

import re
import logging
from typing import List, Dict, Any, Set, Tuple, Optional
from dataclasses import dataclass
import hashlib
from collections import defaultdict

from ..core.config import LexiconConfig
from ..core.logging import get_logger

try:
    from difflib import SequenceMatcher
    DIFFLIB_AVAILABLE = True
except ImportError:
    DIFFLIB_AVAILABLE = False

try:
    import numpy as np
    from sentence_transformers import SentenceTransformer
    SENTENCE_TRANSFORMERS_AVAILABLE = True
except ImportError:
    SENTENCE_TRANSFORMERS_AVAILABLE = False


@dataclass
class EntityMatch:
    """Represents a match between two entities with similarity metrics."""
    entity1_idx: int
    entity2_idx: int
    text_similarity: float
    semantic_similarity: float
    edit_distance: int
    overlap_ratio: float
    combined_score: float
    merge_recommendation: str  # 'merge', 'keep_both', 'needs_review'


class EntityDeduplicator:
    """
    Advanced entity deduplication using multiple similarity metrics.
    
    This class implements sophisticated deduplication strategies including:
    - Text similarity analysis
    - Semantic similarity (when available)
    - Edit distance calculation
    - Confidence-based merging
    - Alternative case preservation
    """
    
    def __init__(self, config: Optional[LexiconConfig] = None):
        """
        Initialize the entity deduplicator.
        
        Args:
            config: LEXICON configuration object
        """
        self.config = config
        self.logger = get_logger("nlp.entity_deduplicator")
        
        # Similarity thresholds
        self.text_similarity_threshold = 0.85
        self.semantic_similarity_threshold = 0.80
        self.edit_distance_threshold = 3
        self.overlap_threshold = 0.70
        
        # Weights for combined similarity score
        self.weights = {
            'text_similarity': 0.3,
            'semantic_similarity': 0.4,
            'edit_distance': 0.2,
            'overlap_ratio': 0.1
        }
        
        # Initialize semantic similarity model if available
        self._semantic_model = None
        if SENTENCE_TRANSFORMERS_AVAILABLE:
            try:
                self._semantic_model = SentenceTransformer('all-MiniLM-L6-v2')
                self.logger.debug("Loaded sentence transformer model for semantic similarity")
            except Exception as e:
                self.logger.warning(f"Failed to load sentence transformer: {e}")
        
        # Track deduplication metrics
        self.metrics = {
            "entities_processed": 0,
            "duplicates_found": 0,
            "entities_merged": 0,
            "high_confidence_merges": 0
        }
    
    def deduplicate_entities(self, entities: List[Dict[str, Any]], 
                           preserve_alternatives: bool = True) -> List[Dict[str, Any]]:
        """
        Deduplicate entities using multiple similarity metrics.
        
        Args:
            entities: List of entity dictionaries
            preserve_alternatives: Whether to preserve alternative case assignments
            
        Returns:
            List of deduplicated entities
        """
        if not entities:
            return entities
        
        self.logger.info(f"Deduplicating {len(entities)} entities")
        self.metrics["entities_processed"] = len(entities)
        
        # Find potential matches
        matches = self._find_entity_matches(entities)
        
        # Group entities into clusters based on matches
        clusters = self._cluster_entities(entities, matches)
        
        # Merge entities within each cluster
        deduplicated_entities = []
        for cluster in clusters:
            if len(cluster) == 1:
                # Single entity, no merging needed
                deduplicated_entities.append(entities[cluster[0]])
            else:
                # Multiple entities, merge them
                merged_entity = self._merge_entity_cluster(
                    [entities[idx] for idx in cluster], 
                    preserve_alternatives
                )
                deduplicated_entities.append(merged_entity)
                self.metrics["entities_merged"] += len(cluster) - 1
        
        self.metrics["duplicates_found"] = len(entities) - len(deduplicated_entities)
        
        self.logger.info(
            f"Deduplication complete: {len(entities)} -> {len(deduplicated_entities)} entities "
            f"({self.metrics['duplicates_found']} duplicates found)"
        )
        
        return deduplicated_entities
    
    def _find_entity_matches(self, entities: List[Dict[str, Any]]) -> List[EntityMatch]:
        """
        Find potential matches between entities using multiple similarity metrics.
        
        Args:
            entities: List of entities to compare
            
        Returns:
            List of entity matches with similarity scores
        """
        matches = []
        
        # Precompute semantic embeddings if available
        embeddings = None
        if self._semantic_model:
            try:
                entity_texts = [entity.get("text", "") for entity in entities]
                embeddings = self._semantic_model.encode(entity_texts)
            except Exception as e:
                self.logger.warning(f"Failed to compute semantic embeddings: {e}")
        
        # Compare each pair of entities
        for i in range(len(entities)):
            for j in range(i + 1, len(entities)):
                entity1 = entities[i]
                entity2 = entities[j]
                
                # Calculate similarity metrics
                match = self._calculate_entity_similarity(
                    entity1, entity2, i, j, embeddings
                )
                
                # Only keep matches above threshold
                if match.combined_score >= 0.5:  # Minimum threshold for consideration
                    matches.append(match)
        
        # Sort matches by combined score (highest first)
        matches.sort(key=lambda x: x.combined_score, reverse=True)
        
        return matches
    
    def _calculate_entity_similarity(self, entity1: Dict, entity2: Dict, 
                                   idx1: int, idx2: int, 
                                   embeddings: Optional[np.ndarray] = None) -> EntityMatch:
        """
        Calculate similarity between two entities using multiple metrics.
        
        Args:
            entity1: First entity
            entity2: Second entity
            idx1: Index of first entity
            idx2: Index of second entity
            embeddings: Precomputed semantic embeddings
            
        Returns:
            EntityMatch object with similarity scores
        """
        text1 = entity1.get("text", "").lower().strip()
        text2 = entity2.get("text", "").lower().strip()
        
        # Text similarity using SequenceMatcher
        text_similarity = 0.0
        if DIFFLIB_AVAILABLE and text1 and text2:
            text_similarity = SequenceMatcher(None, text1, text2).ratio()
        
        # Semantic similarity using sentence transformers
        semantic_similarity = 0.0
        if embeddings is not None and idx1 < len(embeddings) and idx2 < len(embeddings):
            try:
                semantic_similarity = float(np.dot(embeddings[idx1], embeddings[idx2]) / 
                                          (np.linalg.norm(embeddings[idx1]) * np.linalg.norm(embeddings[idx2])))
            except Exception as e:
                self.logger.debug(f"Semantic similarity calculation failed: {e}")
        
        # Edit distance (Levenshtein distance approximation)
        edit_distance = self._calculate_edit_distance(text1, text2)
        
        # Overlap ratio (Jaccard similarity for words)
        overlap_ratio = self._calculate_overlap_ratio(text1, text2)
        
        # Calculate combined score
        combined_score = self._calculate_combined_score(
            text_similarity, semantic_similarity, edit_distance, overlap_ratio, text1, text2
        )
        
        # Determine merge recommendation
        merge_recommendation = self._determine_merge_recommendation(
            text_similarity, semantic_similarity, edit_distance, overlap_ratio, combined_score
        )
        
        return EntityMatch(
            entity1_idx=idx1,
            entity2_idx=idx2,
            text_similarity=text_similarity,
            semantic_similarity=semantic_similarity,
            edit_distance=edit_distance,
            overlap_ratio=overlap_ratio,
            combined_score=combined_score,
            merge_recommendation=merge_recommendation
        )
    
    def _calculate_edit_distance(self, text1: str, text2: str) -> int:
        """
        Calculate edit distance between two texts.
        
        Args:
            text1: First text
            text2: Second text
            
        Returns:
            Edit distance (approximate Levenshtein distance)
        """
        if not text1 or not text2:
            return max(len(text1), len(text2))
        
        # Simple edit distance approximation
        if text1 == text2:
            return 0
        
        # Count character differences
        max_len = max(len(text1), len(text2))
        min_len = min(len(text1), len(text2))
        
        # Simple approximation: length difference + character mismatches
        length_diff = max_len - min_len
        char_diffs = sum(1 for i in range(min_len) if text1[i] != text2[i])
        
        return length_diff + char_diffs
    
    def _calculate_overlap_ratio(self, text1: str, text2: str) -> float:
        """
        Calculate word overlap ratio (Jaccard similarity).
        
        Args:
            text1: First text
            text2: Second text
            
        Returns:
            Overlap ratio between 0 and 1
        """
        if not text1 or not text2:
            return 0.0
        
        words1 = set(text1.split())
        words2 = set(text2.split())
        
        if not words1 or not words2:
            return 0.0
        
        intersection = len(words1 & words2)
        union = len(words1 | words2)
        
        return intersection / union if union > 0 else 0.0
    
    def _calculate_combined_score(self, text_sim: float, semantic_sim: float, 
                                edit_dist: int, overlap: float, 
                                text1: str, text2: str) -> float:
        """
        Calculate combined similarity score using weighted metrics.
        
        Args:
            text_sim: Text similarity score
            semantic_sim: Semantic similarity score
            edit_dist: Edit distance
            overlap: Overlap ratio
            text1: First text
            text2: Second text
            
        Returns:
            Combined similarity score between 0 and 1
        """
        # Normalize edit distance (lower is better)
        max_len = max(len(text1), len(text2))
        edit_norm = 1.0 - (min(edit_dist, max_len) / max_len) if max_len > 0 else 0.0
        
        # Calculate weighted score
        score = (
            self.weights['text_similarity'] * text_sim +
            self.weights['semantic_similarity'] * semantic_sim +
            self.weights['edit_distance'] * edit_norm +
            self.weights['overlap_ratio'] * overlap
        )
        
        return min(1.0, max(0.0, score))
    
    def _determine_merge_recommendation(self, text_sim: float, semantic_sim: float,
                                      edit_dist: int, overlap: float, 
                                      combined_score: float) -> str:
        """
        Determine whether entities should be merged based on similarity scores.
        
        Args:
            text_sim: Text similarity score
            semantic_sim: Semantic similarity score  
            edit_dist: Edit distance
            overlap: Overlap ratio
            combined_score: Combined similarity score
            
        Returns:
            Merge recommendation: 'merge', 'keep_both', or 'needs_review'
        """
        # High confidence merge criteria
        if (combined_score >= 0.9 or
            (text_sim >= 0.95 and edit_dist <= 2) or
            (semantic_sim >= 0.95 and overlap >= 0.8)):
            return 'merge'
        
        # Medium confidence - might need review
        elif (combined_score >= 0.7 or
              (text_sim >= 0.85 and overlap >= 0.7) or
              (semantic_sim >= 0.85 and edit_dist <= 3)):
            return 'needs_review'
        
        # Low confidence - keep separate
        else:
            return 'keep_both'
    
    def _cluster_entities(self, entities: List[Dict], matches: List[EntityMatch]) -> List[List[int]]:
        """
        Group entities into clusters based on similarity matches.
        
        Args:
            entities: List of entities
            matches: List of entity matches
            
        Returns:
            List of clusters, where each cluster is a list of entity indices
        """
        # Union-Find data structure for clustering
        parent = list(range(len(entities)))
        
        def find(x):
            if parent[x] != x:
                parent[x] = find(parent[x])
            return parent[x]
        
        def union(x, y):
            px, py = find(x), find(y)
            if px != py:
                parent[px] = py
        
        # Process high-confidence matches first
        for match in matches:
            if match.merge_recommendation == 'merge':
                union(match.entity1_idx, match.entity2_idx)
        
        # Group entities by their root parent
        clusters = defaultdict(list)
        for i, entity in enumerate(entities):
            root = find(i)
            clusters[root].append(i)
        
        return list(clusters.values())
    
    def _merge_entity_cluster(self, cluster_entities: List[Dict], 
                            preserve_alternatives: bool = True) -> Dict[str, Any]:
        """
        Merge a cluster of similar entities into a single entity.
        
        Args:
            cluster_entities: List of entities to merge
            preserve_alternatives: Whether to preserve alternative case assignments
            
        Returns:
            Merged entity dictionary
        """
        if len(cluster_entities) == 1:
            return cluster_entities[0]
        
        # Find the entity with highest confidence as base
        base_entity = max(cluster_entities, key=lambda e: e.get("confidence", 0.0))
        
        # Start with base entity
        merged_entity = base_entity.copy()
        
        # Combine confidence scores (weighted average)
        total_confidence = sum(e.get("confidence", 0.0) for e in cluster_entities)
        avg_confidence = total_confidence / len(cluster_entities)
        merged_entity["confidence"] = min(1.0, avg_confidence * 1.1)  # Slight boost for agreement
        
        # Merge alternative cases if preserving alternatives
        if preserve_alternatives:
            all_alternative_cases = []
            for entity in cluster_entities:
                if "alternative_cases" in entity:
                    all_alternative_cases.extend(entity["alternative_cases"])
            
            # Deduplicate alternative cases
            unique_alternatives = []
            seen_cases = set()
            for alt_case in all_alternative_cases:
                case_key = alt_case.get("case", "")
                if case_key not in seen_cases:
                    seen_cases.add(case_key)
                    unique_alternatives.append(alt_case)
            
            merged_entity["alternative_cases"] = unique_alternatives
        
        # Merge metadata
        merged_metadata = merged_entity.get("metadata", {})
        merged_metadata["merged_from"] = [e.get("text", "") for e in cluster_entities]
        merged_metadata["merge_confidence"] = avg_confidence
        merged_metadata["merge_count"] = len(cluster_entities)
        merged_entity["metadata"] = merged_metadata
        
        # Use the longest or most descriptive text
        candidate_texts = [e.get("text", "") for e in cluster_entities]
        merged_entity["text"] = max(candidate_texts, key=len)
        
        # Combine rationales
        rationales = [e.get("case_rationale", "") for e in cluster_entities if e.get("case_rationale")]
        if rationales:
            merged_entity["case_rationale"] = f"Merged: {'; '.join(set(rationales))}"
        
        self.metrics["high_confidence_merges"] += 1
        
        return merged_entity
    
    def get_deduplication_metrics(self) -> Dict[str, Any]:
        """
        Get metrics about the deduplication process.
        
        Returns:
            Dictionary of deduplication metrics
        """
        metrics = self.metrics.copy()
        
        if metrics["entities_processed"] > 0:
            metrics["duplicate_rate"] = metrics["duplicates_found"] / metrics["entities_processed"]
        else:
            metrics["duplicate_rate"] = 0.0
        
        return metrics
    
    def create_deduplication_report(self, original_entities: List[Dict], 
                                  deduplicated_entities: List[Dict]) -> Dict[str, Any]:
        """
        Create a detailed report of the deduplication process.
        
        Args:
            original_entities: Original entity list
            deduplicated_entities: Deduplicated entity list
            
        Returns:
            Deduplication report
        """
        report = {
            "summary": {
                "original_count": len(original_entities),
                "deduplicated_count": len(deduplicated_entities),
                "duplicates_removed": len(original_entities) - len(deduplicated_entities),
                "reduction_percentage": (len(original_entities) - len(deduplicated_entities)) / 
                                      len(original_entities) * 100 if original_entities else 0
            },
            "metrics": self.get_deduplication_metrics(),
            "merged_entities": []
        }
        
        # Find merged entities
        for entity in deduplicated_entities:
            if entity.get("metadata", {}).get("merge_count", 1) > 1:
                report["merged_entities"].append({
                    "final_text": entity.get("text", ""),
                    "merged_from": entity.get("metadata", {}).get("merged_from", []),
                    "confidence": entity.get("confidence", 0.0),
                    "case": entity.get("case", "unknown")
                })
        
        return report 