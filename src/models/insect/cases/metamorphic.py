"""
Metamorphic Case Implementation

This module implements the [MET] metamorphic case for developmental transitions
in insects, including larval, pupal, and adult stage transformations.
"""

from typing import Dict, Any, Optional, List, Tuple
import numpy as np
import logging
from dataclasses import dataclass, field
from enum import Enum

from src.core.model import Case

logger = logging.getLogger(__name__)


class DevelopmentalStage(Enum):
    """Developmental stages in insect metamorphosis."""
    EGG = "egg"
    LARVA = "larva"
    PUPA = "pupa"
    ADULT = "adult"
    IMAGO = "imago"


@dataclass
class DevelopmentalState:
    """Current developmental state of an insect."""
    stage: DevelopmentalStage
    age: float  # Age in days
    size: float  # Size relative to adult
    maturity: float  # Maturity level (0-1)
    hormone_levels: Dict[str, float]
    timestamp: float = field(default_factory=lambda: 0.0)


@dataclass
class MetamorphicTransition:
    """Transition between developmental stages."""
    from_stage: DevelopmentalStage
    to_stage: DevelopmentalStage
    transition_time: float
    success_probability: float
    required_conditions: Dict[str, Any]
    timestamp: float = field(default_factory=lambda: 0.0)


class MetamorphicCase:
    """
    [MET] Metamorphic Case for developmental transitions.
    
    This case specializes in modeling developmental changes, stage transitions,
    and metamorphic processes in insects.
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """
        Initialize metamorphic case.
        
        Args:
            config: Configuration parameters
        """
        self.config = config or {}
        self._case = Case.NOMINATIVE  # Default case
        self.case_id = "MET"
        
        # Developmental parameters
        self.stage_durations = {
            DevelopmentalStage.EGG: self.config.get('egg_duration', 3.0),
            DevelopmentalStage.LARVA: self.config.get('larva_duration', 14.0),
            DevelopmentalStage.PUPA: self.config.get('pupa_duration', 7.0),
            DevelopmentalStage.ADULT: float('inf'),  # Adult stage is indefinite
            DevelopmentalStage.IMAGO: float('inf')
        }
        
        self.growth_rates = {
            DevelopmentalStage.EGG: 0.0,  # No growth in egg stage
            DevelopmentalStage.LARVA: self.config.get('larva_growth_rate', 0.1),
            DevelopmentalStage.PUPA: self.config.get('pupa_growth_rate', 0.05),
            DevelopmentalStage.ADULT: 0.0,  # No growth in adult stage
            DevelopmentalStage.IMAGO: 0.0
        }
        
        # Hormone regulation
        self.hormone_regulation = {
            'juvenile_hormone': {
                'larva': 1.0,
                'pupa': 0.3,
                'adult': 0.0
            },
            'ecdysone': {
                'larva': 0.2,
                'pupa': 1.0,
                'adult': 0.1
            },
            'insulin': {
                'larva': 0.8,
                'pupa': 0.4,
                'adult': 0.6
            }
        }
        
        # Transition conditions
        self.transition_conditions = {
            DevelopmentalStage.LARVA: {
                'min_age': 10.0,
                'min_size': 0.7,
                'min_maturity': 0.8,
                'required_hormones': ['ecdysone']
            },
            DevelopmentalStage.PUPA: {
                'min_age': 14.0,
                'min_size': 0.9,
                'min_maturity': 0.95,
                'required_hormones': ['ecdysone', 'juvenile_hormone']
            },
            DevelopmentalStage.ADULT: {
                'min_age': 21.0,
                'min_size': 1.0,
                'min_maturity': 1.0,
                'required_hormones': ['ecdysone']
            }
        }
        
        # Developmental history
        self.developmental_history = []
        self.transition_history = []
        
        logger.info("Initialized MetamorphicCase")
    
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
    
    def update_developmental_state(self, current_state: DevelopmentalState, 
                                 time_delta: float, environmental_conditions: Dict[str, Any]) -> DevelopmentalState:
        """
        Update developmental state based on time and conditions.
        
        Args:
            current_state: Current developmental state
            time_delta: Time elapsed since last update
            environmental_conditions: Environmental conditions affecting development
            
        Returns:
            Updated developmental state
        """
        try:
            # Update age
            new_age = current_state.age + time_delta
            
            # Update size based on growth rate
            growth_rate = self.growth_rates.get(current_state.stage, 0.0)
            size_increase = growth_rate * time_delta
            new_size = min(current_state.size + size_increase, 1.0)
            
            # Update maturity based on age and stage
            stage_duration = self.stage_durations.get(current_state.stage, float('inf'))
            if stage_duration != float('inf'):
                maturity_increase = time_delta / stage_duration
                new_maturity = min(current_state.maturity + maturity_increase, 1.0)
            else:
                new_maturity = current_state.maturity
            
            # Update hormone levels
            new_hormone_levels = self._update_hormone_levels(
                current_state.hormone_levels, 
                current_state.stage, 
                time_delta,
                environmental_conditions
            )
            
            # Create updated state
            updated_state = DevelopmentalState(
                stage=current_state.stage,
                age=new_age,
                size=new_size,
                maturity=new_maturity,
                hormone_levels=new_hormone_levels,
                timestamp=self._get_current_time()
            )
            
            # Check for stage transition
            transition = self._check_stage_transition(updated_state, environmental_conditions)
            if transition:
                updated_state = self._execute_transition(updated_state, transition)
            
            # Record in history
            self.developmental_history.append(updated_state)
            
            return updated_state
            
        except Exception as e:
            logger.error(f"Error updating developmental state: {e}")
            return current_state
    
    def _update_hormone_levels(self, current_levels: Dict[str, float], stage: DevelopmentalStage,
                              time_delta: float, environmental_conditions: Dict[str, Any]) -> Dict[str, float]:
        """
        Update hormone levels based on stage and conditions.
        
        Args:
            current_levels: Current hormone levels
            stage: Current developmental stage
            time_delta: Time elapsed
            environmental_conditions: Environmental conditions
            
        Returns:
            Updated hormone levels
        """
        updated_levels = current_levels.copy()
        stage_name = stage.value
        
        # Update each hormone based on stage-specific regulation
        for hormone, regulation in self.hormone_regulation.items():
            target_level = regulation.get(stage_name, 0.0)
            current_level = current_levels.get(hormone, 0.0)
            
            # Gradual adjustment toward target level
            adjustment_rate = 0.1  # Rate of hormone level adjustment
            adjustment = (target_level - current_level) * adjustment_rate * time_delta
            updated_levels[hormone] = max(0.0, current_level + adjustment)
        
        # Environmental effects on hormone levels
        temperature = environmental_conditions.get('temperature', 25.0)
        nutrition = environmental_conditions.get('nutrition', 1.0)
        
        # Temperature effects
        if temperature > 30.0:
            # High temperature increases ecdysone
            updated_levels['ecdysone'] *= 1.2
        elif temperature < 15.0:
            # Low temperature decreases ecdysone
            updated_levels['ecdysone'] *= 0.8
        
        # Nutrition effects
        if nutrition < 0.5:
            # Poor nutrition decreases insulin
            updated_levels['insulin'] *= 0.7
        
        return updated_levels
    
    def _check_stage_transition(self, state: DevelopmentalState, 
                               environmental_conditions: Dict[str, Any]) -> Optional[MetamorphicTransition]:
        """
        Check if a stage transition should occur.
        
        Args:
            state: Current developmental state
            environmental_conditions: Environmental conditions
            
        Returns:
            Metamorphic transition if conditions are met, None otherwise
        """
        current_stage = state.stage
        
        # Define stage progression
        stage_progression = {
            DevelopmentalStage.EGG: DevelopmentalStage.LARVA,
            DevelopmentalStage.LARVA: DevelopmentalStage.PUPA,
            DevelopmentalStage.PUPA: DevelopmentalStage.ADULT,
            DevelopmentalStage.ADULT: DevelopmentalStage.ADULT  # No further progression
        }
        
        next_stage = stage_progression.get(current_stage)
        if not next_stage or next_stage == current_stage:
            return None
        
        # Check transition conditions
        conditions = self.transition_conditions.get(current_stage, {})
        
        # Check age requirement
        if state.age < conditions.get('min_age', 0.0):
            return None
        
        # Check size requirement
        if state.size < conditions.get('min_size', 0.0):
            return None
        
        # Check maturity requirement
        if state.maturity < conditions.get('min_maturity', 0.0):
            return None
        
        # Check hormone requirements
        required_hormones = conditions.get('required_hormones', [])
        for hormone in required_hormones:
            if state.hormone_levels.get(hormone, 0.0) < 0.5:  # Minimum hormone threshold
                return None
        
        # Calculate transition probability
        success_probability = self._calculate_transition_probability(state, environmental_conditions)
        
        # Create transition
        transition = MetamorphicTransition(
            from_stage=current_stage,
            to_stage=next_stage,
            transition_time=self._get_current_time(),
            success_probability=success_probability,
            required_conditions=conditions,
            timestamp=self._get_current_time()
        )
        
        return transition
    
    def _calculate_transition_probability(self, state: DevelopmentalState,
                                        environmental_conditions: Dict[str, Any]) -> float:
        """
        Calculate probability of successful transition.
        
        Args:
            state: Current developmental state
            environmental_conditions: Environmental conditions
            
        Returns:
            Probability of successful transition (0-1)
        """
        base_probability = 0.8
        
        # Environmental factors
        temperature = environmental_conditions.get('temperature', 25.0)
        humidity = environmental_conditions.get('humidity', 0.5)
        nutrition = environmental_conditions.get('nutrition', 1.0)
        
        # Temperature effects
        if 20.0 <= temperature <= 30.0:
            temp_factor = 1.0
        else:
            temp_factor = 0.5
        
        # Humidity effects
        if 0.4 <= humidity <= 0.8:
            humidity_factor = 1.0
        else:
            humidity_factor = 0.7
        
        # Nutrition effects
        nutrition_factor = min(nutrition, 1.0)
        
        # Hormone balance effects
        hormone_balance = 1.0
        if state.hormone_levels.get('ecdysone', 0.0) > 0.7:
            hormone_balance *= 1.2
        if state.hormone_levels.get('juvenile_hormone', 0.0) < 0.3:
            hormone_balance *= 1.1
        
        # Calculate final probability
        final_probability = base_probability * temp_factor * humidity_factor * nutrition_factor * hormone_balance
        
        return min(final_probability, 1.0)
    
    def _execute_transition(self, state: DevelopmentalState, 
                           transition: MetamorphicTransition) -> DevelopmentalState:
        """
        Execute a developmental transition.
        
        Args:
            state: Current developmental state
            transition: Transition to execute
            
        Returns:
            Updated developmental state after transition
        """
        try:
            # Check if transition is successful
            if np.random.random() > transition.success_probability:
                logger.warning(f"Failed transition from {transition.from_stage.value} to {transition.to_stage.value}")
                return state
            
            # Execute transition
            new_state = DevelopmentalState(
                stage=transition.to_stage,
                age=0.0,  # Reset age for new stage
                size=state.size,  # Maintain size
                maturity=0.0,  # Reset maturity for new stage
                hormone_levels=state.hormone_levels.copy(),  # Maintain hormone levels
                timestamp=self._get_current_time()
            )
            
            # Record transition
            self.transition_history.append(transition)
            
            logger.info(f"Successful transition: {transition.from_stage.value} -> {transition.to_stage.value}")
            
            return new_state
            
        except Exception as e:
            logger.error(f"Error executing transition: {e}")
            return state
    
    def get_developmental_summary(self) -> Dict[str, Any]:
        """
        Get summary of developmental progress.
        
        Returns:
            Dictionary with developmental statistics
        """
        if not self.developmental_history:
            return {'total_stages': 0, 'current_stage': 'unknown'}
        
        current_state = self.developmental_history[-1]
        total_transitions = len(self.transition_history)
        
        # Calculate time spent in each stage
        stage_times = {}
        for state in self.developmental_history:
            stage = state.stage.value
            if stage not in stage_times:
                stage_times[stage] = 0.0
            stage_times[stage] += 0.1  # Assuming 0.1 day intervals
        
        return {
            'current_stage': current_state.stage.value,
            'current_age': current_state.age,
            'current_size': current_state.size,
            'current_maturity': current_state.maturity,
            'total_transitions': total_transitions,
            'stage_times': stage_times,
            'hormone_levels': current_state.hormone_levels,
            'developmental_history_length': len(self.developmental_history)
        }
    
    def predict_next_transition(self, current_state: DevelopmentalState,
                              environmental_conditions: Dict[str, Any]) -> Optional[MetamorphicTransition]:
        """
        Predict the next developmental transition.
        
        Args:
            current_state: Current developmental state
            environmental_conditions: Environmental conditions
            
        Returns:
            Predicted transition or None if no transition expected
        """
        # Simulate future development
        simulated_state = current_state
        time_horizon = 30.0  # Look ahead 30 days
        time_step = 0.1
        
        for t in np.arange(0, time_horizon, time_step):
            simulated_state = self.update_developmental_state(
                simulated_state, time_step, environmental_conditions
            )
            
            # Check for transition
            transition = self._check_stage_transition(simulated_state, environmental_conditions)
            if transition:
                return transition
        
        return None
    
    def _get_current_time(self) -> float:
        """Get current simulation time."""
        import time
        return time.time() 