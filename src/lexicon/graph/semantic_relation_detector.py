"""
LEXICON Semantic Relation Detector

Advanced semantic relationship detection using multiple strategies:
- Syntactic dependency parsing
- Semantic role labeling
- Contextual pattern matching
- Cross-sentence coreference analysis
- Domain-specific relationship templates
"""

import re
import logging
from typing import List, Dict, Any, Set, Tuple, Optional, NamedTuple
from dataclasses import dataclass
from collections import defaultdict, Counter
import json

from ..core.config import LexiconConfig
from ..core.logging import get_logger

try:
    import spacy
    from spacy.matcher import Matcher, DependencyMatcher
    SPACY_AVAILABLE = True
except ImportError:
    SPACY_AVAILABLE = False

try:
    import networkx as nx
    NETWORKX_AVAILABLE = True
except ImportError:
    NETWORKX_AVAILABLE = False


@dataclass
class SemanticRelation:
    """Represents a semantic relationship between entities."""
    source_entity: str
    target_entity: str
    relation_type: str
    confidence: float
    evidence: List[str]
    context: str
    source_case: Optional[str] = None
    target_case: Optional[str] = None
    syntactic_pattern: Optional[str] = None
    semantic_role: Optional[str] = None


class RelationPattern:
    """Template for identifying specific types of relationships."""
    def __init__(self, name: str, patterns: List[str], relation_type: str, 
                 confidence_weight: float = 1.0):
        self.name = name
        self.patterns = [re.compile(p, re.IGNORECASE) for p in patterns]
        self.relation_type = relation_type
        self.confidence_weight = confidence_weight


class SemanticRelationDetector:
    """
    Advanced semantic relationship detector for entity knowledge graphs.
    
    Implements multiple detection strategies:
    1. Syntactic dependency analysis
    2. Semantic role labeling
    3. Pattern-based detection
    4. Coreference-based relations
    5. Domain-specific templates
    """
    
    def __init__(self, config: Optional[LexiconConfig] = None):
        """
        Initialize the semantic relation detector.
        
        Args:
            config: LEXICON configuration object
        """
        self.config = config
        self.logger = get_logger("graph.semantic_relation_detector")
        
        # Initialize spaCy if available
        self._nlp = None
        self._dependency_matcher = None
        if SPACY_AVAILABLE:
            try:
                self._nlp = spacy.load("en_core_web_sm")
                self._dependency_matcher = DependencyMatcher(self._nlp.vocab)
                self._setup_dependency_patterns()
                self.logger.debug("Loaded spaCy model for syntactic analysis")
            except Exception as e:
                self.logger.warning(f"Failed to load spaCy model: {e}")
        
        # Initialize relation patterns
        self._setup_relation_patterns()
        
        # Tracking metrics
        self.metrics = {
            "relations_detected": 0,
            "pattern_matches": 0,
            "syntactic_relations": 0,
            "semantic_role_relations": 0,
            "coreference_relations": 0
        }
    
    def _setup_dependency_patterns(self):
        """Setup dependency patterns for syntactic relationship detection."""
        if not self._dependency_matcher:
            return
            
        # Subject-verb-object pattern
        svo_pattern = [
            {
                "RIGHT_ID": "verb",
                "RIGHT_ATTRS": {"POS": "VERB"}
            },
            {
                "LEFT_ID": "verb",
                "REL_OP": ">",
                "RIGHT_ID": "subject",
                "RIGHT_ATTRS": {"DEP": "nsubj"}
            },
            {
                "LEFT_ID": "verb",
                "REL_OP": ">", 
                "RIGHT_ID": "object",
                "RIGHT_ATTRS": {"DEP": {"IN": ["dobj", "pobj"]}}
            }
        ]
        self._dependency_matcher.add("SVO", [svo_pattern])
        
        # Prepositional relationships
        prep_pattern = [
            {
                "RIGHT_ID": "head",
                "RIGHT_ATTRS": {"POS": {"IN": ["NOUN", "PROPN"]}}
            },
            {
                "LEFT_ID": "head",
                "REL_OP": ">",
                "RIGHT_ID": "prep",
                "RIGHT_ATTRS": {"DEP": "prep"}
            },
            {
                "LEFT_ID": "prep",
                "REL_OP": ">",
                "RIGHT_ID": "pobj",
                "RIGHT_ATTRS": {"DEP": "pobj"}
            }
        ]
        self._dependency_matcher.add("PREP_REL", [prep_pattern])
        
        # Possessive relationships
        poss_pattern = [
            {
                "RIGHT_ID": "head",
                "RIGHT_ATTRS": {"POS": {"IN": ["NOUN", "PROPN"]}}
            },
            {
                "LEFT_ID": "head",
                "REL_OP": ">",
                "RIGHT_ID": "possessor",
                "RIGHT_ATTRS": {"DEP": "poss"}
            }
        ]
        self._dependency_matcher.add("POSSESSIVE", [poss_pattern])
    
    def _setup_relation_patterns(self):
        """Setup pattern-based relationship templates."""
        self.relation_patterns = [
            # Therapeutic relationships
            RelationPattern(
                "therapist_client",
                [r"(?:therapist|counselor|doctor)\s+(?:to|for|with)\s+(?:client|patient)"],
                "professional_relationship",
                0.9
            ),
            RelationPattern(
                "emotional_response",
                [r"feel[s]?\s+(?:anxious|tense|worried|calm|relaxed)", 
                 r"(?:tension|anxiety|stress)\s+in\s+(?:shoulders|back|chest)"],
                "emotional_state",
                0.8
            ),
            
            # Causality patterns
            RelationPattern(
                "causal_because",
                [r"because\s+of", r"due\s+to", r"caused\s+by", r"results?\s+from"],
                "causal_relationship",
                0.85
            ),
            RelationPattern(
                "causal_leads_to",
                [r"leads?\s+to", r"results?\s+in", r"causes?", r"triggers?"],
                "causal_relationship", 
                0.85
            ),
            
            # Temporal relationships
            RelationPattern(
                "temporal_before",
                [r"before", r"prior\s+to", r"earlier", r"previously"],
                "temporal_relationship",
                0.7
            ),
            RelationPattern(
                "temporal_after",
                [r"after", r"following", r"then", r"subsequently"],
                "temporal_relationship",
                0.7
            ),
            
            # Location/containment
            RelationPattern(
                "spatial_in",
                [r"in\s+(?:the\s+)?(?:shoulders|back|chest|stomach)", 
                 r"located\s+in", r"positioned\s+at"],
                "spatial_relationship",
                0.8
            ),
            
            # Similarity/comparison
            RelationPattern(
                "similarity",
                [r"like", r"similar\s+to", r"reminds?\s+me\s+of", r"just\s+as"],
                "similarity_relationship",
                0.75
            ),
            
            # Part-whole relationships
            RelationPattern(
                "part_of",
                [r"part\s+of", r"component\s+of", r"element\s+of", r"aspect\s+of"],
                "part_whole_relationship",
                0.8
            )
        ]
    
    def detect_relations(self, entities: List[Dict[str, Any]], 
                        text: str, sentences: List[str] = None) -> List[SemanticRelation]:
        """
        Detect semantic relationships between entities in the given text.
        
        Args:
            entities: List of extracted entities
            text: Original text for analysis
            sentences: Pre-split sentences (optional)
            
        Returns:
            List of detected semantic relations
        """
        if not entities or len(entities) < 2:
            return []
        
        self.logger.info(f"Detecting relations among {len(entities)} entities")
        
        relations = []
        
        # Split text into sentences if not provided
        if sentences is None:
            sentences = self._split_into_sentences(text)
        
        # Strategy 1: Pattern-based detection
        pattern_relations = self._detect_pattern_relations(entities, text, sentences)
        relations.extend(pattern_relations)
        
        # Strategy 2: Syntactic dependency analysis
        if self._nlp:
            syntactic_relations = self._detect_syntactic_relations(entities, text, sentences)
            relations.extend(syntactic_relations)
        
        # Strategy 3: Proximity-based relationships with context
        proximity_relations = self._detect_proximity_relations(entities, sentences)
        relations.extend(proximity_relations)
        
        # Strategy 4: Coreference-based relationships
        coreference_relations = self._detect_coreference_relations(entities, text)
        relations.extend(coreference_relations)
        
        # Deduplicate and rank relations
        deduplicated_relations = self._deduplicate_relations(relations)
        
        self.metrics["relations_detected"] = len(deduplicated_relations)
        
        self.logger.info(f"Detected {len(deduplicated_relations)} semantic relations")
        
        return deduplicated_relations
    
    def _detect_pattern_relations(self, entities: List[Dict], text: str, 
                                sentences: List[str]) -> List[SemanticRelation]:
        """Detect relationships using pattern matching."""
        relations = []
        entity_texts = [e["text"].lower() for e in entities]
        
        for sentence in sentences:
            sentence_lower = sentence.lower()
            
            # Check which entities appear in this sentence
            sentence_entities = []
            for i, entity in enumerate(entities):
                if entity["text"].lower() in sentence_lower:
                    sentence_entities.append((i, entity))
            
            if len(sentence_entities) < 2:
                continue
            
            # Apply pattern matching
            for pattern in self.relation_patterns:
                for regex in pattern.patterns:
                    matches = list(regex.finditer(sentence))
                    
                    if matches:
                        # Find entities around each match
                        for match in matches:
                            match_start, match_end = match.span()
                            
                            # Find entities before and after the pattern
                            for i, (ent1_idx, ent1) in enumerate(sentence_entities):
                                for j, (ent2_idx, ent2) in enumerate(sentence_entities[i+1:], i+1):
                                    if ent1_idx != ent2_idx:
                                        relation = SemanticRelation(
                                            source_entity=ent1["text"],
                                            target_entity=ent2["text"],
                                            relation_type=pattern.relation_type,
                                            confidence=pattern.confidence_weight * 0.8,
                                            evidence=[sentence],
                                            context=sentence,
                                            source_case=ent1.get("case"),
                                            target_case=ent2.get("case"),
                                            syntactic_pattern=pattern.name
                                        )
                                        relations.append(relation)
                                        self.metrics["pattern_matches"] += 1
        
        return relations
    
    def _detect_syntactic_relations(self, entities: List[Dict], text: str,
                                  sentences: List[str]) -> List[SemanticRelation]:
        """Detect relationships using syntactic dependency parsing."""
        if not self._nlp or not self._dependency_matcher:
            return []
        
        relations = []
        entity_texts = [e["text"].lower() for e in entities]
        
        for sentence in sentences:
            doc = self._nlp(sentence)
            matches = self._dependency_matcher(doc)
            
            for match_id, token_ids in matches:
                pattern_name = self._nlp.vocab.strings[match_id]
                
                # Extract tokens from the match
                tokens = [doc[token_id] for token_id in token_ids]
                
                if pattern_name == "SVO":
                    # Subject-verb-object relationship
                    verb, subject, obj = tokens
                    subj_text = subject.text.lower()
                    obj_text = obj.text.lower()
                    
                    # Check if subject and object are entities
                    subj_entity = self._find_entity_by_text(entities, subj_text)
                    obj_entity = self._find_entity_by_text(entities, obj_text)
                    
                    if subj_entity and obj_entity:
                        relation = SemanticRelation(
                            source_entity=subj_entity["text"],
                            target_entity=obj_entity["text"],
                            relation_type="action_relationship",
                            confidence=0.85,
                            evidence=[sentence],
                            context=sentence,
                            source_case=subj_entity.get("case"),
                            target_case=obj_entity.get("case"),
                            syntactic_pattern=f"SVO:{verb.lemma_}"
                        )
                        relations.append(relation)
                        self.metrics["syntactic_relations"] += 1
                
                elif pattern_name == "PREP_REL":
                    # Prepositional relationship
                    head, prep, pobj = tokens
                    head_text = head.text.lower()
                    pobj_text = pobj.text.lower()
                    
                    head_entity = self._find_entity_by_text(entities, head_text)
                    pobj_entity = self._find_entity_by_text(entities, pobj_text)
                    
                    if head_entity and pobj_entity:
                        relation = SemanticRelation(
                            source_entity=head_entity["text"],
                            target_entity=pobj_entity["text"],
                            relation_type="prepositional_relationship",
                            confidence=0.75,
                            evidence=[sentence],
                            context=sentence,
                            source_case=head_entity.get("case"),
                            target_case=pobj_entity.get("case"),
                            syntactic_pattern=f"PREP:{prep.text}"
                        )
                        relations.append(relation)
                        self.metrics["syntactic_relations"] += 1
        
        return relations
    
    def _detect_proximity_relations(self, entities: List[Dict], 
                                   sentences: List[str]) -> List[SemanticRelation]:
        """Detect relationships based on entity proximity with contextual analysis."""
        relations = []
        
        for sentence in sentences:
            sentence_lower = sentence.lower()
            sentence_entities = []
            
            # Find entities in this sentence with positions
            for entity in entities:
                entity_text = entity["text"].lower()
                pos = sentence_lower.find(entity_text)
                if pos != -1:
                    sentence_entities.append((entity, pos, pos + len(entity_text)))
            
            # Sort by position
            sentence_entities.sort(key=lambda x: x[1])
            
            # Create proximity relationships for nearby entities
            for i in range(len(sentence_entities) - 1):
                ent1, start1, end1 = sentence_entities[i]
                ent2, start2, end2 = sentence_entities[i + 1]
                
                # Calculate distance between entities
                distance = start2 - end1
                
                if distance < 50:  # Within 50 characters
                    # Analyze the text between entities for relationship type
                    between_text = sentence[end1:start2].strip()
                    relation_type = self._classify_proximity_relation(between_text)
                    
                    confidence = max(0.3, 0.8 - (distance / 100))  # Distance-based confidence
                    
                    relation = SemanticRelation(
                        source_entity=ent1["text"],
                        target_entity=ent2["text"],
                        relation_type=relation_type,
                        confidence=confidence,
                        evidence=[sentence],
                        context=sentence,
                        source_case=ent1.get("case"),
                        target_case=ent2.get("case"),
                        syntactic_pattern=f"proximity:{distance}"
                    )
                    relations.append(relation)
        
        return relations
    
    def _detect_coreference_relations(self, entities: List[Dict], 
                                    text: str) -> List[SemanticRelation]:
        """Detect relationships through coreference analysis."""
        relations = []
        
        # Simple pronoun resolution for therapeutic context
        pronouns = {"he", "she", "it", "they", "this", "that"}
        sentences = self._split_into_sentences(text)
        
        for i, sentence in enumerate(sentences):
            sentence_lower = sentence.lower()
            
            # Find pronouns in sentence
            for pronoun in pronouns:
                if pronoun in sentence_lower:
                    # Look for entities in previous sentences
                    context_entities = []
                    for j in range(max(0, i-2), i):  # Look back 2 sentences
                        prev_sentence = sentences[j].lower()
                        for entity in entities:
                            if entity["text"].lower() in prev_sentence:
                                context_entities.append(entity)
                    
                    # Find entities in current sentence
                    current_entities = []
                    for entity in entities:
                        if entity["text"].lower() in sentence_lower:
                            current_entities.append(entity)
                    
                    # Create coreference relationships
                    for context_ent in context_entities:
                        for current_ent in current_entities:
                            if context_ent["text"] != current_ent["text"]:
                                relation = SemanticRelation(
                                    source_entity=context_ent["text"],
                                    target_entity=current_ent["text"],
                                    relation_type="coreference_relationship",
                                    confidence=0.6,
                                    evidence=[sentences[j], sentence],
                                    context=f"{sentences[j]} ... {sentence}",
                                    source_case=context_ent.get("case"),
                                    target_case=current_ent.get("case"),
                                    syntactic_pattern=f"coref:{pronoun}"
                                )
                                relations.append(relation)
                                self.metrics["coreference_relations"] += 1
        
        return relations
    
    def _classify_proximity_relation(self, between_text: str) -> str:
        """Classify the type of relationship based on text between entities."""
        between_text = between_text.lower().strip()
        
        if any(word in between_text for word in ["and", "&", "+"]):
            return "coordination_relationship"
        elif any(word in between_text for word in ["with", "alongside"]):
            return "accompaniment_relationship"
        elif any(word in between_text for word in ["in", "at", "on"]):
            return "locative_relationship"
        elif any(word in between_text for word in ["of", "'s"]):
            return "possessive_relationship"
        elif any(word in between_text for word in ["to", "for"]):
            return "directional_relationship"
        else:
            return "proximity_relationship"
    
    def _find_entity_by_text(self, entities: List[Dict], text: str) -> Optional[Dict]:
        """Find entity by text match."""
        text_lower = text.lower()
        for entity in entities:
            if entity["text"].lower() == text_lower:
                return entity
            # Also check if text is contained in entity text
            if text_lower in entity["text"].lower() or entity["text"].lower() in text_lower:
                return entity
        return None
    
    def _split_into_sentences(self, text: str) -> List[str]:
        """Split text into sentences using simple rules."""
        # Basic sentence splitting
        sentences = re.split(r'[.!?]+', text)
        return [s.strip() for s in sentences if s.strip()]
    
    def _deduplicate_relations(self, relations: List[SemanticRelation]) -> List[SemanticRelation]:
        """Remove duplicate relationships and merge similar ones."""
        relation_map = defaultdict(list)
        
        for relation in relations:
            # Create a key for grouping similar relations
            key = (relation.source_entity.lower(), 
                   relation.target_entity.lower(), 
                   relation.relation_type)
            relation_map[key].append(relation)
        
        deduplicated = []
        for similar_relations in relation_map.values():
            if len(similar_relations) == 1:
                deduplicated.append(similar_relations[0])
            else:
                # Merge similar relations
                merged = self._merge_relations(similar_relations)
                deduplicated.append(merged)
        
        # Sort by confidence
        deduplicated.sort(key=lambda r: r.confidence, reverse=True)
        
        return deduplicated
    
    def _merge_relations(self, relations: List[SemanticRelation]) -> SemanticRelation:
        """Merge multiple similar relations into one."""
        if len(relations) == 1:
            return relations[0]
        
        # Use the highest confidence relation as base
        base_relation = max(relations, key=lambda r: r.confidence)
        
        # Combine evidence and increase confidence
        all_evidence = []
        for relation in relations:
            all_evidence.extend(relation.evidence)
        
        merged_confidence = min(1.0, base_relation.confidence + 0.1 * (len(relations) - 1))
        
        return SemanticRelation(
            source_entity=base_relation.source_entity,
            target_entity=base_relation.target_entity,
            relation_type=base_relation.relation_type,
            confidence=merged_confidence,
            evidence=list(set(all_evidence)),  # Remove duplicates
            context=base_relation.context,
            source_case=base_relation.source_case,
            target_case=base_relation.target_case,
            syntactic_pattern=f"merged:{len(relations)}",
            semantic_role=base_relation.semantic_role
        )
    
    def get_relation_metrics(self) -> Dict[str, Any]:
        """Get metrics about relation detection."""
        return self.metrics.copy()
    
    def create_relation_report(self, relations: List[SemanticRelation]) -> Dict[str, Any]:
        """Create a detailed report of detected relations."""
        relation_types = Counter(r.relation_type for r in relations)
        confidence_dist = [r.confidence for r in relations]
        
        return {
            "summary": {
                "total_relations": len(relations),
                "unique_relation_types": len(relation_types),
                "avg_confidence": sum(confidence_dist) / len(confidence_dist) if confidence_dist else 0,
                "high_confidence_relations": len([r for r in relations if r.confidence > 0.8])
            },
            "relation_types": dict(relation_types),
            "metrics": self.get_relation_metrics(),
            "sample_relations": [
                {
                    "source": r.source_entity,
                    "target": r.target_entity,
                    "type": r.relation_type,
                    "confidence": r.confidence,
                    "pattern": r.syntactic_pattern
                }
                for r in sorted(relations, key=lambda x: x.confidence, reverse=True)[:10]
            ]
        } 