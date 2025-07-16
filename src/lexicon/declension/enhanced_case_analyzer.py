"""
LEXICON Enhanced Case Analyzer

Advanced case analysis system with domain-specific patterns, confidence calibration,
and reduced locative bias. Implements sophisticated linguistic analysis for accurate
case determination in therapeutic and general contexts.
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
    from spacy.matcher import Matcher
    SPACY_AVAILABLE = True
except ImportError:
    SPACY_AVAILABLE = False


@dataclass
class CaseEvidence:
    """Evidence supporting a particular case assignment."""
    case: str
    confidence: float
    evidence_type: str  # 'syntactic', 'semantic', 'pattern', 'context'
    evidence_text: str
    rule_name: str
    contextual_factors: List[str]


@dataclass
class CaseAnalysis:
    """Complete case analysis for an entity."""
    entity_text: str
    primary_case: str
    primary_confidence: float
    alternative_cases: List[Tuple[str, float]]
    evidence: List[CaseEvidence]
    context_analysis: Dict[str, Any]
    domain_indicators: List[str]


class DomainSpecificRules:
    """Domain-specific case assignment rules."""
    
    def __init__(self):
        # Therapeutic domain patterns
        self.therapeutic_patterns = {
            "nominative": [
                r"(?:client|patient|therapist|doctor)\s+(?:says?|feels?|thinks?|believes?)",
                r"(?:anxiety|tension|stress|pain)\s+(?:is|becomes|feels)",
                r"(?:shoulders|back|chest|stomach)\s+(?:are|feel|hurt)"
            ],
            "accusative": [
                r"feel[s]?\s+(?:the\s+)?(?:tension|anxiety|pain|pressure)",
                r"notice[s]?\s+(?:the\s+)?(?:sensation|feeling|tightness)",
                r"experience[s]?\s+(?:the\s+)?(?:emotion|feeling|response)"
            ],
            "genitive": [
                r"(?:client|patient)'s\s+(?:feeling|emotion|response|reaction)",
                r"(?:tension|anxiety|stress)\s+of\s+(?:the\s+)?(?:shoulders|back|work)",
                r"impact\s+of\s+(?:the\s+)?(?:deadline|expectation|pressure)"
            ],
            "dative": [
                r"(?:to|for)\s+(?:the\s+)?(?:client|patient|therapist)",
                r"(?:give|offer|provide)\s+(?:to\s+)?(?:the\s+)?(?:person|individual)",
                r"(?:helpful|beneficial|important)\s+(?:to|for)\s+(?:the\s+)?(?:healing|recovery)"
            ],
            "ablative": [
                r"(?:from|by|with)\s+(?:the\s+)?(?:therapy|treatment|session)",
                r"(?:caused|triggered|induced)\s+by\s+(?:the\s+)?(?:stress|work|deadline)",
                r"(?:relief|freedom|separation)\s+from\s+(?:the\s+)?(?:tension|anxiety)"
            ],
            "locative": [
                r"(?:in|at|on)\s+(?:the\s+)?(?:shoulders|back|chest|stomach|office|session)",
                r"(?:located|positioned|situated)\s+(?:in|at|on)",
                r"(?:during|throughout|within)\s+(?:the\s+)?(?:session|therapy|treatment)"
            ]
        }
        
        # Scientific/technical domain patterns
        self.scientific_patterns = {
            "nominative": [
                r"(?:study|research|experiment|analysis)\s+(?:shows?|reveals?|indicates?)",
                r"(?:data|results?|findings?)\s+(?:suggest|show|indicate)",
                r"(?:model|theory|hypothesis)\s+(?:predicts?|explains?)"
            ],
            "accusative": [
                r"(?:conduct|perform|carry\s+out)\s+(?:the\s+)?(?:study|experiment|analysis)",
                r"(?:measure|observe|record)\s+(?:the\s+)?(?:data|response|behavior)",
                r"(?:test|examine|evaluate)\s+(?:the\s+)?(?:hypothesis|model|theory)"
            ],
            "instrumental": [
                r"(?:using|with|through)\s+(?:the\s+)?(?:method|technique|approach|tool)",
                r"(?:measured|analyzed|processed)\s+(?:using|with|by)\s+(?:the\s+)?(?:instrument|software)",
                r"(?:via|by\s+means\s+of|through\s+the\s+use\s+of)"
            ]
        }
        
        # Business/professional domain patterns
        self.business_patterns = {
            "nominative": [
                r"(?:manager|employee|team|department)\s+(?:manages?|oversees?|coordinates?)",
                r"(?:company|organization|business)\s+(?:provides?|offers?|delivers?)",
                r"(?:project|initiative|strategy)\s+(?:aims?|targets?|focuses?)"
            ],
            "accusative": [
                r"(?:manage|oversee|coordinate)\s+(?:the\s+)?(?:project|team|department)",
                r"(?:develop|create|implement)\s+(?:the\s+)?(?:strategy|plan|solution)",
                r"(?:deliver|provide|offer)\s+(?:the\s+)?(?:service|product|solution)"
            ],
            "dative": [
                r"(?:to|for)\s+(?:the\s+)?(?:client|customer|stakeholder|user)",
                r"(?:beneficial|valuable|important)\s+(?:to|for)\s+(?:the\s+)?(?:business|organization)",
                r"(?:report|present|communicate)\s+to\s+(?:the\s+)?(?:management|board|team)"
            ]
        }
    
    def get_domain_patterns(self, domain: str) -> Dict[str, List[str]]:
        """Get patterns for a specific domain."""
        domain_map = {
            "therapeutic": self.therapeutic_patterns,
            "scientific": self.scientific_patterns,
            "business": self.business_patterns
        }
        return domain_map.get(domain, {})


class EnhancedCaseAnalyzer:
    """
    Enhanced case analyzer with domain-specific patterns and reduced locative bias.
    
    Features:
    - Domain-aware case assignment
    - Confidence calibration
    - Multi-evidence integration
    - Contextual analysis
    - Bias mitigation
    """
    
    def __init__(self, config: Optional[LexiconConfig] = None):
        """
        Initialize the enhanced case analyzer.
        
        Args:
            config: LEXICON configuration object
        """
        self.config = config
        self.logger = get_logger("declension.enhanced_case_analyzer")
        
        # Initialize spaCy if available
        self._nlp = None
        self._matcher = None
        if SPACY_AVAILABLE:
            try:
                self._nlp = spacy.load("en_core_web_sm")
                self._matcher = Matcher(self._nlp.vocab)
                self._setup_syntactic_patterns()
                self.logger.debug("Loaded spaCy model for syntactic analysis")
            except Exception as e:
                self.logger.warning(f"Failed to load spaCy model: {e}")
        
        # Initialize domain-specific rules
        self.domain_rules = DomainSpecificRules()
        
        # Case weights and biases
        self._setup_case_weights()
        
        # Tracking metrics
        self.metrics = {
            "entities_analyzed": 0,
            "high_confidence_assignments": 0,
            "alternative_cases_generated": 0,
            "domain_specific_matches": 0,
            "locative_bias_corrections": 0
        }
    
    def _setup_case_weights(self):
        """Setup case weights to reduce locative bias."""
        # Base weights for cases (reduced locative weight)
        self.case_base_weights = {
            "nominative": 1.0,
            "accusative": 1.0,
            "genitive": 0.95,
            "dative": 0.9,
            "ablative": 0.85,
            "locative": 0.7,  # Reduced from typical 1.0 to mitigate bias
            "instrumental": 0.8,
            "vocative": 0.6
        }
        
        # Context-based weight modifiers
        self.context_modifiers = {
            "therapeutic": {
                "locative": 0.9,  # Further reduce in therapeutic context
                "accusative": 1.1,  # Boost for "feeling X"
                "ablative": 1.1   # Boost for "from therapy"
            },
            "scientific": {
                "instrumental": 1.2,  # Boost for methods/tools
                "accusative": 1.1,   # Boost for "measure X"
                "nominative": 1.1    # Boost for "study shows"
            },
            "business": {
                "dative": 1.1,      # Boost for "to client"
                "accusative": 1.1,  # Boost for "manage project"
                "genitive": 1.1     # Boost for possessive
            }
        }
    
    def _setup_syntactic_patterns(self):
        """Setup syntactic patterns for spaCy matcher."""
        if not self._matcher:
            return
        
        # Nominative patterns (subject)
        nom_patterns = [
            [{"DEP": "nsubj"}],
            [{"DEP": "nsubjpass"}],
            [{"DEP": "csubj"}]
        ]
        self._matcher.add("NOMINATIVE", nom_patterns)
        
        # Accusative patterns (direct object)
        acc_patterns = [
            [{"DEP": "dobj"}],
            [{"DEP": "pobj", "HEAD": {"POS": "VERB"}}]
        ]
        self._matcher.add("ACCUSATIVE", acc_patterns)
        
        # Genitive patterns (possessive)
        gen_patterns = [
            [{"DEP": "poss"}],
            [{"TEXT": {"REGEX": r"'s$"}}],
            [{"LOWER": "of", "OP": "+"}, {"POS": "NOUN"}]
        ]
        self._matcher.add("GENITIVE", gen_patterns)
        
        # Dative patterns (indirect object, beneficiary)
        dat_patterns = [
            [{"DEP": "dative"}],
            [{"LOWER": {"IN": ["to", "for"]}}, {"POS": "NOUN"}]
        ]
        self._matcher.add("DATIVE", dat_patterns)
        
        # Ablative patterns (agent, means, separation)
        abl_patterns = [
            [{"LOWER": {"IN": ["by", "from", "with"]}}, {"POS": "NOUN"}],
            [{"DEP": "agent"}]
        ]
        self._matcher.add("ABLATIVE", abl_patterns)
        
        # Locative patterns (location, time) - with stricter conditions
        loc_patterns = [
            [{"LOWER": {"IN": ["in", "at", "on", "during"]}}, {"POS": "NOUN", "ENT_TYPE": {"IN": ["GPE", "LOC", "TIME"]}}],
            [{"DEP": "prep", "LOWER": {"IN": ["in", "at", "on"]}}, {"DEP": "pobj", "ENT_TYPE": {"IN": ["GPE", "LOC"]}}]
        ]
        self._matcher.add("LOCATIVE", loc_patterns)
        
        # Instrumental patterns (means, method)
        ins_patterns = [
            [{"LOWER": {"IN": ["with", "using", "via", "through"]}}, {"POS": "NOUN"}],
            [{"DEP": "prep", "LOWER": {"IN": ["with", "using"]}}, {"DEP": "pobj"}]
        ]
        self._matcher.add("INSTRUMENTAL", ins_patterns)
    
    def analyze_entity_case(self, entity: Dict[str, Any], 
                           full_text: str, 
                           sentence_context: str,
                           domain_hints: List[str] = None) -> CaseAnalysis:
        """
        Perform comprehensive case analysis for an entity.
        
        Args:
            entity: Entity dictionary with text and context
            full_text: Complete text for broader context
            sentence_context: Immediate sentence containing the entity
            domain_hints: Optional domain indicators
            
        Returns:
            Complete case analysis
        """
        entity_text = entity.get("text", "")
        self.logger.debug(f"Analyzing case for entity: {entity_text}")
        
        # Detect domain
        detected_domain = self._detect_domain(full_text, domain_hints)
        
        # Collect evidence from multiple sources
        evidence = []
        
        # 1. Syntactic evidence
        syntactic_evidence = self._analyze_syntactic_case(entity_text, sentence_context)
        evidence.extend(syntactic_evidence)
        
        # 2. Pattern-based evidence
        pattern_evidence = self._analyze_pattern_case(entity_text, sentence_context, detected_domain)
        evidence.extend(pattern_evidence)
        
        # 3. Contextual evidence
        contextual_evidence = self._analyze_contextual_case(entity_text, full_text, sentence_context)
        evidence.extend(contextual_evidence)
        
        # 4. Semantic role evidence
        semantic_evidence = self._analyze_semantic_role(entity_text, sentence_context)
        evidence.extend(semantic_evidence)
        
        # Aggregate evidence and determine case
        case_scores = self._aggregate_evidence(evidence, detected_domain)
        
        # Apply bias corrections
        corrected_scores = self._apply_bias_corrections(case_scores, detected_domain, sentence_context)
        
        # Determine primary case and alternatives
        primary_case, primary_confidence = max(corrected_scores.items(), key=lambda x: x[1])
        
        # Generate alternative cases
        alternative_cases = []
        for case, confidence in corrected_scores.items():
            if case != primary_case and confidence > 0.3:
                alternative_cases.append((case, confidence))
        
        alternative_cases.sort(key=lambda x: x[1], reverse=True)
        
        # Context analysis
        context_analysis = {
            "domain": detected_domain,
            "sentence_length": len(sentence_context.split()),
            "entity_position": self._get_entity_position(entity_text, sentence_context),
            "surrounding_words": self._get_surrounding_words(entity_text, sentence_context),
            "syntactic_role": self._get_syntactic_role(entity_text, sentence_context)
        }
        
        # Domain indicators
        domain_indicators = self._extract_domain_indicators(sentence_context, detected_domain)
        
        self.metrics["entities_analyzed"] += 1
        if primary_confidence > 0.8:
            self.metrics["high_confidence_assignments"] += 1
        if alternative_cases:
            self.metrics["alternative_cases_generated"] += 1
        
        return CaseAnalysis(
            entity_text=entity_text,
            primary_case=primary_case,
            primary_confidence=primary_confidence,
            alternative_cases=alternative_cases,
            evidence=evidence,
            context_analysis=context_analysis,
            domain_indicators=domain_indicators
        )
    
    def _detect_domain(self, text: str, hints: List[str] = None) -> str:
        """Detect the domain of the text."""
        text_lower = text.lower()
        
        # Check hints first
        if hints:
            for hint in hints:
                if hint.lower() in ["therapeutic", "therapy", "clinical"]:
                    return "therapeutic"
                elif hint.lower() in ["scientific", "research", "academic"]:
                    return "scientific"
                elif hint.lower() in ["business", "corporate", "professional"]:
                    return "business"
        
        # Domain keyword detection
        therapeutic_keywords = ["therapy", "therapist", "client", "session", "anxiety", "stress", "emotion", "feeling"]
        scientific_keywords = ["study", "research", "experiment", "data", "analysis", "hypothesis", "model", "theory"]
        business_keywords = ["manager", "employee", "project", "client", "business", "strategy", "meeting", "deadline"]
        
        therapeutic_score = sum(1 for keyword in therapeutic_keywords if keyword in text_lower)
        scientific_score = sum(1 for keyword in scientific_keywords if keyword in text_lower)
        business_score = sum(1 for keyword in business_keywords if keyword in text_lower)
        
        max_score = max(therapeutic_score, scientific_score, business_score)
        
        if max_score == 0:
            return "general"
        elif max_score == therapeutic_score:
            return "therapeutic"
        elif max_score == scientific_score:
            return "scientific"
        else:
            return "business"
    
    def _analyze_syntactic_case(self, entity_text: str, sentence: str) -> List[CaseEvidence]:
        """Analyze syntactic patterns for case determination."""
        evidence = []
        
        if not self._nlp or not self._matcher:
            return evidence
        
        try:
            doc = self._nlp(sentence)
            matches = self._matcher(doc)
            
            # Find the entity in the parsed sentence
            entity_token = None
            for token in doc:
                if entity_text.lower() in token.text.lower():
                    entity_token = token
                    break
            
            if not entity_token:
                return evidence
            
            # Check syntactic patterns
            for match_id, start, end in matches:
                label = self._nlp.vocab.strings[match_id]
                span = doc[start:end]
                
                # Check if our entity is part of this match
                if entity_token.i >= start and entity_token.i < end:
                    case_map = {
                        "NOMINATIVE": "nominative",
                        "ACCUSATIVE": "accusative", 
                        "GENITIVE": "genitive",
                        "DATIVE": "dative",
                        "ABLATIVE": "ablative",
                        "LOCATIVE": "locative",
                        "INSTRUMENTAL": "instrumental"
                    }
                    
                    case = case_map.get(label)
                    if case:
                        evidence.append(CaseEvidence(
                            case=case,
                            confidence=0.8,
                            evidence_type="syntactic",
                            evidence_text=span.text,
                            rule_name=f"syntactic_{label.lower()}",
                            contextual_factors=[f"dep:{entity_token.dep_}", f"pos:{entity_token.pos_}"]
                        ))
        
        except Exception as e:
            self.logger.warning(f"Syntactic analysis failed: {e}")
        
        return evidence
    
    def _analyze_pattern_case(self, entity_text: str, sentence: str, domain: str) -> List[CaseEvidence]:
        """Analyze domain-specific patterns for case determination."""
        evidence = []
        sentence_lower = sentence.lower()
        entity_lower = entity_text.lower()
        
        # Get domain-specific patterns
        domain_patterns = self.domain_rules.get_domain_patterns(domain)
        
        # Also check general therapeutic patterns if not already therapeutic domain
        if domain != "therapeutic":
            therapeutic_patterns = self.domain_rules.get_domain_patterns("therapeutic")
            for case, patterns in therapeutic_patterns.items():
                for pattern in patterns:
                    if re.search(pattern, sentence_lower) and entity_lower in sentence_lower:
                        evidence.append(CaseEvidence(
                            case=case,
                            confidence=0.7,
                            evidence_type="pattern",
                            evidence_text=sentence,
                            rule_name=f"therapeutic_{case}_pattern",
                            contextual_factors=["cross_domain_match"]
                        ))
                        break
        
        # Check domain-specific patterns
        for case, patterns in domain_patterns.items():
            for pattern in patterns:
                if re.search(pattern, sentence_lower) and entity_lower in sentence_lower:
                    confidence = 0.85 if domain != "general" else 0.75
                    evidence.append(CaseEvidence(
                        case=case,
                        confidence=confidence,
                        evidence_type="pattern",
                        evidence_text=sentence,
                        rule_name=f"{domain}_{case}_pattern",
                        contextual_factors=[f"domain:{domain}"]
                    ))
                    self.metrics["domain_specific_matches"] += 1
                    break
        
        return evidence
    
    def _analyze_contextual_case(self, entity_text: str, full_text: str, sentence: str) -> List[CaseEvidence]:
        """Analyze broader contextual clues for case determination."""
        evidence = []
        
        # Position-based analysis
        position = self._get_entity_position(entity_text, sentence)
        if position == "beginning":
            evidence.append(CaseEvidence(
                case="nominative",
                confidence=0.6,
                evidence_type="context",
                evidence_text=sentence,
                rule_name="sentence_initial_position",
                contextual_factors=["position:beginning"]
            ))
        
        # Surrounding word analysis
        surrounding = self._get_surrounding_words(entity_text, sentence, window=3)
        
        # Check for case-indicating prepositions
        prep_indicators = {
            "in": ("locative", 0.7),
            "at": ("locative", 0.6),
            "on": ("locative", 0.6),
            "to": ("dative", 0.7),
            "for": ("dative", 0.6),
            "from": ("ablative", 0.7),
            "by": ("ablative", 0.6),
            "with": ("instrumental", 0.7),
            "of": ("genitive", 0.6)
        }
        
        for word in surrounding["before"] + surrounding["after"]:
            if word.lower() in prep_indicators:
                case, confidence = prep_indicators[word.lower()]
                evidence.append(CaseEvidence(
                    case=case,
                    confidence=confidence,
                    evidence_type="context",
                    evidence_text=f"...{word} {entity_text}...",
                    rule_name=f"preposition_{word.lower()}",
                    contextual_factors=[f"prep:{word.lower()}"]
                ))
        
        return evidence
    
    def _analyze_semantic_role(self, entity_text: str, sentence: str) -> List[CaseEvidence]:
        """Analyze semantic roles for case determination."""
        evidence = []
        sentence_lower = sentence.lower()
        entity_lower = entity_text.lower()
        
        # Agent patterns (typically nominative)
        agent_patterns = [
            r"(?:^|\s)(" + re.escape(entity_lower) + r")\s+(?:does|performs|executes|initiates)",
            r"(?:^|\s)(" + re.escape(entity_lower) + r")\s+(?:says?|thinks?|believes?|feels?)"
        ]
        
        for pattern in agent_patterns:
            if re.search(pattern, sentence_lower):
                evidence.append(CaseEvidence(
                    case="nominative",
                    confidence=0.75,
                    evidence_type="semantic",
                    evidence_text=sentence,
                    rule_name="agent_role",
                    contextual_factors=["semantic_role:agent"]
                ))
                break
        
        # Patient patterns (typically accusative)
        patient_patterns = [
            r"(?:feel|notice|experience|observe)\s+(?:the\s+)?(" + re.escape(entity_lower) + r")",
            r"(?:affect|influence|impact)\s+(?:the\s+)?(" + re.escape(entity_lower) + r")"
        ]
        
        for pattern in patient_patterns:
            if re.search(pattern, sentence_lower):
                evidence.append(CaseEvidence(
                    case="accusative",
                    confidence=0.8,
                    evidence_type="semantic",
                    evidence_text=sentence,
                    rule_name="patient_role",
                    contextual_factors=["semantic_role:patient"]
                ))
                break
        
        return evidence
    
    def _aggregate_evidence(self, evidence: List[CaseEvidence], domain: str) -> Dict[str, float]:
        """Aggregate evidence from multiple sources into case scores."""
        case_scores = defaultdict(float)
        
        for ev in evidence:
            # Apply base weight
            base_weight = self.case_base_weights.get(ev.case, 0.5)
            
            # Apply domain modifier
            domain_modifier = 1.0
            if domain in self.context_modifiers:
                domain_modifier = self.context_modifiers[domain].get(ev.case, 1.0)
            
            # Calculate weighted score
            weighted_score = ev.confidence * base_weight * domain_modifier
            case_scores[ev.case] += weighted_score
        
        # Normalize scores
        if case_scores:
            max_score = max(case_scores.values())
            if max_score > 0:
                for case in case_scores:
                    case_scores[case] = case_scores[case] / max_score
        
        return dict(case_scores)
    
    def _apply_bias_corrections(self, case_scores: Dict[str, float], 
                               domain: str, sentence: str) -> Dict[str, float]:
        """Apply corrections to reduce known biases."""
        corrected_scores = case_scores.copy()
        
        # Locative bias correction
        if "locative" in corrected_scores and corrected_scores["locative"] > 0.6:
            # Check if this is a genuine locative case
            locative_indicators = ["in", "at", "on", "location", "place", "room", "office"]
            sentence_lower = sentence.lower()
            
            genuine_locative = any(indicator in sentence_lower for indicator in locative_indicators)
            
            if not genuine_locative:
                # Reduce locative score
                reduction = 0.3
                corrected_scores["locative"] *= (1 - reduction)
                self.metrics["locative_bias_corrections"] += 1
                
                # Redistribute to other cases
                remaining_cases = [c for c in corrected_scores if c != "locative"]
                if remaining_cases:
                    boost_per_case = (reduction * case_scores["locative"]) / len(remaining_cases)
                    for case in remaining_cases:
                        corrected_scores[case] += boost_per_case
        
        # Ensure scores don't exceed 1.0
        for case in corrected_scores:
            corrected_scores[case] = min(1.0, corrected_scores[case])
        
        return corrected_scores
    
    def _get_entity_position(self, entity_text: str, sentence: str) -> str:
        """Get the position of entity in sentence."""
        words = sentence.split()
        entity_words = entity_text.split()
        
        for i, word in enumerate(words):
            if entity_words[0].lower() in word.lower():
                if i == 0:
                    return "beginning"
                elif i < len(words) // 3:
                    return "early"
                elif i < 2 * len(words) // 3:
                    return "middle"
                else:
                    return "late"
        
        return "unknown"
    
    def _get_surrounding_words(self, entity_text: str, sentence: str, window: int = 2) -> Dict[str, List[str]]:
        """Get words surrounding the entity."""
        words = sentence.split()
        entity_words = entity_text.split()
        
        # Find entity position
        entity_start = -1
        for i, word in enumerate(words):
            if entity_words[0].lower() in word.lower():
                entity_start = i
                break
        
        if entity_start == -1:
            return {"before": [], "after": []}
        
        entity_end = entity_start + len(entity_words)
        
        before_words = words[max(0, entity_start - window):entity_start]
        after_words = words[entity_end:min(len(words), entity_end + window)]
        
        return {
            "before": before_words,
            "after": after_words
        }
    
    def _get_syntactic_role(self, entity_text: str, sentence: str) -> str:
        """Get syntactic role of entity if spaCy is available."""
        if not self._nlp:
            return "unknown"
        
        try:
            doc = self._nlp(sentence)
            for token in doc:
                if entity_text.lower() in token.text.lower():
                    return token.dep_
        except Exception:
            pass
        
        return "unknown"
    
    def _extract_domain_indicators(self, sentence: str, domain: str) -> List[str]:
        """Extract indicators that led to domain classification."""
        indicators = []
        sentence_lower = sentence.lower()
        
        domain_keywords = {
            "therapeutic": ["therapy", "therapist", "client", "session", "anxiety", "stress", "emotion", "feeling"],
            "scientific": ["study", "research", "experiment", "data", "analysis", "hypothesis", "model"],
            "business": ["manager", "employee", "project", "client", "business", "strategy", "meeting"]
        }
        
        if domain in domain_keywords:
            for keyword in domain_keywords[domain]:
                if keyword in sentence_lower:
                    indicators.append(keyword)
        
        return indicators
    
    def get_analysis_metrics(self) -> Dict[str, Any]:
        """Get metrics about the analysis process."""
        return self.metrics.copy()
    
    def create_analysis_report(self, analyses: List[CaseAnalysis]) -> Dict[str, Any]:
        """Create a detailed report of case analyses."""
        case_distribution = Counter(analysis.primary_case for analysis in analyses)
        confidence_dist = [analysis.primary_confidence for analysis in analyses]
        
        domain_distribution = Counter(
            analysis.context_analysis.get("domain", "unknown") 
            for analysis in analyses
        )
        
        return {
            "summary": {
                "total_entities": len(analyses),
                "avg_confidence": sum(confidence_dist) / len(confidence_dist) if confidence_dist else 0,
                "high_confidence_count": len([a for a in analyses if a.primary_confidence > 0.8]),
                "alternative_cases_provided": len([a for a in analyses if a.alternative_cases])
            },
            "case_distribution": dict(case_distribution),
            "domain_distribution": dict(domain_distribution),
            "metrics": self.get_analysis_metrics(),
            "sample_analyses": [
                {
                    "entity": analysis.entity_text,
                    "primary_case": analysis.primary_case,
                    "confidence": analysis.primary_confidence,
                    "domain": analysis.context_analysis.get("domain"),
                    "alternatives": len(analysis.alternative_cases)
                }
                for analysis in sorted(analyses, key=lambda x: x.primary_confidence, reverse=True)[:10]
            ]
        } 