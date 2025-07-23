"""
Caste Case Implementation

This module implements the [CAST] caste case for social insect caste systems,
including caste determination, role specialization, and caste-specific behaviors.
"""

from typing import Dict, Any, Optional, List, Tuple
import numpy as np
import logging
from dataclasses import dataclass, field
from enum import Enum
from collections import defaultdict

from src.core.model import Case

logger = logging.getLogger(__name__)


class CasteType(Enum):
    """Types of castes in social insects."""
    QUEEN = "queen"
    WORKER = "worker"
    SOLDIER = "soldier"
    DRONE = "drone"
    NURSE = "nurse"
    FORAGER = "forager"
    BUILDER = "builder"
    GUARD = "guard"
    REPRODUCTIVE = "reproductive"
    ALATE = "alate"


@dataclass
class CasteProfile:
    """Profile of a caste with its characteristics."""
    caste_type: CasteType
    size_factor: float  # Size relative to queen
    lifespan_factor: float  # Lifespan relative to queen
    fertility: float  # Reproductive capacity (0-1)
    aggression: float  # Aggression level (0-1)
    intelligence: float  # Cognitive capacity (0-1)
    specialization: List[str]  # Specialized behaviors
    required_nutrition: float  # Nutritional requirements


@dataclass
class CasteAssignment:
    """Assignment of an individual to a caste."""
    individual_id: str
    caste_type: CasteType
    assignment_time: float
    confidence: float
    factors: Dict[str, float]  # Factors that influenced assignment
    timestamp: float = field(default_factory=lambda: 0.0)


@dataclass
class CasteBehavior:
    """Caste-specific behavior pattern."""
    behavior_type: str
    caste_type: CasteType
    priority: float
    conditions: Dict[str, Any]
    success_rate: float
    timestamp: float = field(default_factory=lambda: 0.0)


class CasteCase:
    """
    [CAST] Caste Case for social insect caste systems.
    
    This case specializes in modeling caste determination, role specialization,
    and caste-specific behaviors in social insect societies.
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """
        Initialize caste case.
        
        Args:
            config: Configuration parameters
        """
        self.config = config or {}
        self._case = Case.NOMINATIVE  # Default case
        self.case_id = "CAST"
        
        # Caste profiles
        self.caste_profiles = self._initialize_caste_profiles()
        
        # Caste determination parameters
        self.genetic_factors = self.config.get('genetic_factors', 0.6)
        self.environmental_factors = self.config.get('environmental_factors', 0.3)
        self.nutritional_factors = self.config.get('nutritional_factors', 0.1)
        
        # Caste assignment history
        self.caste_assignments: Dict[str, CasteAssignment] = {}
        self.caste_behaviors: Dict[str, List[CasteBehavior]] = defaultdict(list)
        
        # Caste statistics
        self.caste_statistics = defaultdict(int)
        
        logger.info("Initialized CasteCase")
    
    def _initialize_caste_profiles(self) -> Dict[CasteType, CasteProfile]:
        """Initialize caste profiles with characteristics."""
        return {
            CasteType.QUEEN: CasteProfile(
                caste_type=CasteType.QUEEN,
                size_factor=1.0,
                lifespan_factor=1.0,
                fertility=1.0,
                aggression=0.3,
                intelligence=0.8,
                specialization=['reproduction', 'colony_management'],
                required_nutrition=1.0
            ),
            CasteType.WORKER: CasteProfile(
                caste_type=CasteType.WORKER,
                size_factor=0.6,
                lifespan_factor=0.3,
                fertility=0.0,
                aggression=0.4,
                intelligence=0.6,
                specialization=['foraging', 'nest_maintenance', 'brood_care'],
                required_nutrition=0.7
            ),
            CasteType.SOLDIER: CasteProfile(
                caste_type=CasteType.SOLDIER,
                size_factor=0.8,
                lifespan_factor=0.4,
                fertility=0.0,
                aggression=0.9,
                intelligence=0.5,
                specialization=['defense', 'patrol', 'attack'],
                required_nutrition=0.8
            ),
            CasteType.DRONE: CasteProfile(
                caste_type=CasteType.DRONE,
                size_factor=0.7,
                lifespan_factor=0.2,
                fertility=0.8,
                aggression=0.2,
                intelligence=0.4,
                specialization=['mating', 'sperm_production'],
                required_nutrition=0.6
            ),
            CasteType.NURSE: CasteProfile(
                caste_type=CasteType.NURSE,
                size_factor=0.5,
                lifespan_factor=0.5,
                fertility=0.0,
                aggression=0.2,
                intelligence=0.7,
                specialization=['brood_care', 'feeding', 'cleaning'],
                required_nutrition=0.6
            ),
            CasteType.FORAGER: CasteProfile(
                caste_type=CasteType.FORAGER,
                size_factor=0.6,
                lifespan_factor=0.3,
                fertility=0.0,
                aggression=0.3,
                intelligence=0.8,
                specialization=['food_collection', 'navigation', 'communication'],
                required_nutrition=0.8
            ),
            CasteType.BUILDER: CasteProfile(
                caste_type=CasteType.BUILDER,
                size_factor=0.6,
                lifespan_factor=0.4,
                fertility=0.0,
                aggression=0.2,
                intelligence=0.6,
                specialization=['construction', 'repair', 'material_gathering'],
                required_nutrition=0.7
            ),
            CasteType.GUARD: CasteProfile(
                caste_type=CasteType.GUARD,
                size_factor=0.7,
                lifespan_factor=0.4,
                fertility=0.0,
                aggression=0.7,
                intelligence=0.6,
                specialization=['nest_defense', 'intruder_detection', 'alarm'],
                required_nutrition=0.7
            ),
            CasteType.REPRODUCTIVE: CasteProfile(
                caste_type=CasteType.REPRODUCTIVE,
                size_factor=0.9,
                lifespan_factor=0.8,
                fertility=0.9,
                aggression=0.4,
                intelligence=0.7,
                specialization=['reproduction', 'colony_founding'],
                required_nutrition=0.9
            ),
            CasteType.ALATE: CasteProfile(
                caste_type=CasteType.ALATE,
                size_factor=0.8,
                lifespan_factor=0.3,
                fertility=0.7,
                aggression=0.3,
                intelligence=0.6,
                specialization=['dispersal', 'mating_flight'],
                required_nutrition=0.8
            )
        }
        
        logger.info("Initialized CasteCase")
    
    @property
    def case(self) -> Case:
        """Get the current case."""
        return self._case
        
    @case.setter
    def case(self, value: Case):
        """Set the current case."""
        if not isinstance(value, Case):
            raise TypeError(f"Expected Case enum, got {type(value)}")
        self._case = value
    
    def determine_caste(self, individual_id: str, genetic_markers: Dict[str, float],
                       environmental_conditions: Dict[str, Any], 
                       nutritional_history: List[float]) -> CasteAssignment:
        """
        Determine the caste of an individual based on various factors.
        
        Args:
            individual_id: ID of the individual
            genetic_markers: Genetic markers influencing caste
            environmental_conditions: Environmental conditions during development
            nutritional_history: History of nutritional intake
            
        Returns:
            Caste assignment for the individual
        """
        try:
            # Calculate caste probabilities
            caste_probabilities = {}
            
            for caste_type, profile in self.caste_profiles.items():
                probability = self._calculate_caste_probability(
                    caste_type, genetic_markers, environmental_conditions, nutritional_history
                )
                caste_probabilities[caste_type] = probability
            
            # Select caste with highest probability
            best_caste = max(caste_probabilities, key=caste_probabilities.get)
            confidence = caste_probabilities[best_caste]
            
            # Create caste assignment
            assignment = CasteAssignment(
                individual_id=individual_id,
                caste_type=best_caste,
                assignment_time=self._get_current_time(),
                confidence=confidence,
                factors={
                    'genetic': genetic_markers,
                    'environmental': environmental_conditions,
                    'nutritional': np.mean(nutritional_history) if nutritional_history else 0.5
                },
                timestamp=self._get_current_time()
            )
            
            # Store assignment
            self.caste_assignments[individual_id] = assignment
            self.caste_statistics[best_caste.value] += 1
            
            logger.info(f"Assigned {individual_id} to {best_caste.value} caste (confidence: {confidence:.2f})")
            
            return assignment
            
        except Exception as e:
            logger.error(f"Error determining caste: {e}")
            # Default to worker caste
            return CasteAssignment(
                individual_id=individual_id,
                caste_type=CasteType.WORKER,
                assignment_time=self._get_current_time(),
                confidence=0.5,
                factors={},
                timestamp=self._get_current_time()
            )
    
    def _calculate_caste_probability(self, caste_type: CasteType, genetic_markers: Dict[str, float],
                                   environmental_conditions: Dict[str, Any], 
                                   nutritional_history: List[float]) -> float:
        """
        Calculate probability of being assigned to a specific caste.
        
        Args:
            caste_type: Caste type to evaluate
            genetic_markers: Genetic markers
            environmental_conditions: Environmental conditions
            nutritional_history: Nutritional history
            
        Returns:
            Probability of caste assignment (0-1)
        """
        base_probability = 0.1  # Base probability for all castes
        
        # Genetic factors
        genetic_score = 0.0
        if 'caste_genes' in genetic_markers:
            genetic_score = genetic_markers['caste_genes']
        
        # Environmental factors
        environmental_score = 0.0
        temperature = environmental_conditions.get('temperature', 25.0)
        humidity = environmental_conditions.get('humidity', 0.5)
        
        # Temperature effects on caste determination
        if 20.0 <= temperature <= 30.0:
            environmental_score += 0.3
        if 0.4 <= humidity <= 0.8:
            environmental_score += 0.2
        
        # Nutritional factors
        nutritional_score = 0.0
        if nutritional_history:
            avg_nutrition = np.mean(nutritional_history)
            required_nutrition = self.caste_profiles[caste_type].required_nutrition
            
            # Higher nutrition favors reproductive castes
            if caste_type in [CasteType.QUEEN, CasteType.REPRODUCTIVE, CasteType.ALATE]:
                nutritional_score = min(avg_nutrition / required_nutrition, 1.0)
            else:
                # Lower nutrition favors worker castes
                nutritional_score = 1.0 - min(avg_nutrition / required_nutrition, 1.0)
        
        # Caste-specific adjustments
        caste_adjustments = {
            CasteType.QUEEN: 0.05,  # Rare caste
            CasteType.WORKER: 0.4,  # Common caste
            CasteType.SOLDIER: 0.15,  # Moderate frequency
            CasteType.DRONE: 0.1,  # Seasonal caste
            CasteType.NURSE: 0.2,  # Common caste
            CasteType.FORAGER: 0.25,  # Common caste
            CasteType.BUILDER: 0.2,  # Common caste
            CasteType.GUARD: 0.15,  # Moderate frequency
            CasteType.REPRODUCTIVE: 0.08,  # Rare caste
            CasteType.ALATE: 0.1  # Seasonal caste
        }
        
        # Calculate final probability
        final_probability = (
            base_probability +
            self.genetic_factors * genetic_score +
            self.environmental_factors * environmental_score +
            self.nutritional_factors * nutritional_score +
            caste_adjustments.get(caste_type, 0.1)
        )
        
        return min(final_probability, 1.0)
    
    def get_caste_behaviors(self, caste_type: CasteType, 
                           current_conditions: Dict[str, Any]) -> List[CasteBehavior]:
        """
        Get behaviors appropriate for a specific caste.
        
        Args:
            caste_type: Caste type
            current_conditions: Current environmental and colony conditions
            
        Returns:
            List of appropriate behaviors
        """
        try:
            profile = self.caste_profiles[caste_type]
            behaviors = []
            
            # Generate behaviors based on caste specialization
            for specialization in profile.specialization:
                behavior = self._create_caste_behavior(caste_type, specialization, current_conditions)
                if behavior:
                    behaviors.append(behavior)
            
            # Add caste-specific behaviors based on conditions
            if caste_type == CasteType.QUEEN:
                if current_conditions.get('colony_size', 0) < 100:
                    behaviors.append(CasteBehavior(
                        behavior_type="egg_laying",
                        caste_type=caste_type,
                        priority=0.9,
                        conditions={'colony_size': 'small'},
                        success_rate=0.8,
                        timestamp=self._get_current_time()
                    ))
            
            elif caste_type == CasteType.WORKER:
                if current_conditions.get('food_shortage', False):
                    behaviors.append(CasteBehavior(
                        behavior_type="emergency_foraging",
                        caste_type=caste_type,
                        priority=0.8,
                        conditions={'food_shortage': True},
                        success_rate=0.6,
                        timestamp=self._get_current_time()
                    ))
            
            elif caste_type == CasteType.SOLDIER:
                if current_conditions.get('threat_level', 0) > 0.5:
                    behaviors.append(CasteBehavior(
                        behavior_type="defensive_posture",
                        caste_type=caste_type,
                        priority=0.9,
                        conditions={'threat_level': 'high'},
                        success_rate=0.7,
                        timestamp=self._get_current_time()
                    ))
            
            # Sort behaviors by priority
            behaviors.sort(key=lambda b: b.priority, reverse=True)
            
            return behaviors
            
        except Exception as e:
            logger.error(f"Error getting caste behaviors: {e}")
            return []
    
    def _create_caste_behavior(self, caste_type: CasteType, specialization: str,
                              conditions: Dict[str, Any]) -> Optional[CasteBehavior]:
        """
        Create a behavior for a caste specialization.
        
        Args:
            caste_type: Caste type
            specialization: Specialization area
            conditions: Current conditions
            
        Returns:
            Caste behavior or None if not applicable
        """
        # Define behavior templates for each specialization
        behavior_templates = {
            'reproduction': {
                'priority': 0.8,
                'success_rate': 0.7,
                'conditions': {'mating_season': True}
            },
            'foraging': {
                'priority': 0.6,
                'success_rate': 0.5,
                'conditions': {'food_available': True}
            },
            'defense': {
                'priority': 0.9,
                'success_rate': 0.6,
                'conditions': {'threat_present': True}
            },
            'brood_care': {
                'priority': 0.7,
                'success_rate': 0.8,
                'conditions': {'brood_present': True}
            },
            'construction': {
                'priority': 0.5,
                'success_rate': 0.7,
                'conditions': {'materials_available': True}
            },
            'navigation': {
                'priority': 0.6,
                'success_rate': 0.6,
                'conditions': {'destination_known': True}
            },
            'communication': {
                'priority': 0.5,
                'success_rate': 0.8,
                'conditions': {'colony_members_present': True}
            }
        }
        
        template = behavior_templates.get(specialization)
        if not template:
            return None
        
        # Check if conditions are met
        conditions_met = True
        for condition_key, condition_value in template['conditions'].items():
            if conditions.get(condition_key) != condition_value:
                conditions_met = False
                break
        
        if not conditions_met:
            return None
        
        return CasteBehavior(
            behavior_type=specialization,
            caste_type=caste_type,
            priority=template['priority'],
            conditions=template['conditions'],
            success_rate=template['success_rate'],
            timestamp=self._get_current_time()
        )
    
    def get_caste_statistics(self) -> Dict[str, Any]:
        """
        Get statistics about caste distribution.
        
        Returns:
            Dictionary with caste statistics
        """
        total_individuals = sum(self.caste_statistics.values())
        
        if total_individuals == 0:
            return {'total_individuals': 0, 'caste_distribution': {}}
        
        caste_distribution = {}
        for caste_type, count in self.caste_statistics.items():
            caste_distribution[caste_type] = {
                'count': count,
                'percentage': (count / total_individuals) * 100
            }
        
        return {
            'total_individuals': total_individuals,
            'caste_distribution': caste_distribution,
            'total_assignments': len(self.caste_assignments),
            'average_confidence': np.mean([a.confidence for a in self.caste_assignments.values()]) if self.caste_assignments else 0.0
        }
    
    def get_individual_caste(self, individual_id: str) -> Optional[CasteAssignment]:
        """
        Get the caste assignment for a specific individual.
        
        Args:
            individual_id: ID of the individual
            
        Returns:
            Caste assignment or None if not found
        """
        return self.caste_assignments.get(individual_id)
    
    def update_caste_behavior(self, individual_id: str, behavior_type: str, success: bool):
        """
        Update the success rate of a caste behavior.
        
        Args:
            individual_id: ID of the individual
            behavior_type: Type of behavior performed
            success: Whether the behavior was successful
        """
        try:
            assignment = self.caste_assignments.get(individual_id)
            if not assignment:
                return
            
            # Find the behavior in the individual's behavior history
            if individual_id in self.caste_behaviors:
                for behavior in self.caste_behaviors[individual_id]:
                    if behavior.behavior_type == behavior_type:
                        # Update success rate
                        if success:
                            behavior.success_rate = min(behavior.success_rate + 0.1, 1.0)
                        else:
                            behavior.success_rate = max(behavior.success_rate - 0.1, 0.0)
                        break
            
            logger.debug(f"Updated behavior success rate for {individual_id}: {behavior_type} = {success}")
            
        except Exception as e:
            logger.error(f"Error updating caste behavior: {e}")
    
    def _get_current_time(self) -> float:
        """Get current simulation time."""
        import time
        return time.time() 