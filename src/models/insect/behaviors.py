#!/usr/bin/env python3
"""
Insect Behavioral Modules for CEREBRUM

This module provides behavioral implementations for insect models,
including foraging, navigation, communication, and social behaviors.
"""

import numpy as np
import logging
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass, field
from enum import Enum

logger = logging.getLogger(__name__)


class BehaviorType(Enum):
    """Types of insect behaviors."""
    FORAGING = "foraging"
    NAVIGATION = "navigation"
    COMMUNICATION = "communication"
    SOCIAL = "social"
    DEFENSE = "defense"
    REPRODUCTION = "reproduction"
    NEST_BUILDING = "nest_building"
    CLEANING = "cleaning"


@dataclass
class BehaviorState:
    """State of a behavioral process."""
    behavior_type: BehaviorType
    is_active: bool
    priority: float
    start_time: float
    duration: float
    success_rate: float
    energy_cost: float
    timestamp: float = field(default_factory=lambda: 0.0)


@dataclass
class BehaviorResult:
    """Result of a behavioral action."""
    success: bool
    energy_expended: float
    information_gained: Dict[str, Any]
    time_spent: float
    timestamp: float = field(default_factory=lambda: 0.0)


class ForagingBehavior:
    """Foraging behavior implementation for insects."""
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """Initialize foraging behavior."""
        self.config = config or {}
        self.search_strategy = self.config.get('search_strategy', 'random_walk')
        self.energy_threshold = self.config.get('energy_threshold', 0.3)
        self.memory_decay = self.config.get('memory_decay', 0.1)
        
        # Foraging memory
        self.food_locations = []
        self.food_qualities = {}
        self.search_history = []
        
        logger.info("Initialized ForagingBehavior")
    
    def search_for_food(self, current_position: np.ndarray, 
                       search_radius: float, energy_level: float) -> BehaviorResult:
        """Search for food in the environment."""
        if energy_level < self.energy_threshold:
            return BehaviorResult(
                success=False,
                energy_expended=0.0,
                information_gained={},
                time_spent=0.0
            )
        
        # Simple random walk search
        search_direction = np.random.randn(3)
        search_direction = search_direction / np.linalg.norm(search_direction)
        new_position = current_position + search_direction * search_radius * 0.1
        
        # Simulate finding food with some probability
        food_found = np.random.random() < 0.1
        
        if food_found:
            food_quality = np.random.random()
            self.food_locations.append(new_position)
            self.food_qualities[tuple(new_position)] = food_quality
            
            return BehaviorResult(
                success=True,
                energy_expended=0.1,
                information_gained={'food_location': new_position, 'quality': food_quality},
                time_spent=1.0
            )
        
        return BehaviorResult(
            success=False,
            energy_expended=0.05,
            information_gained={'searched_area': new_position},
            time_spent=1.0
        )
    
    def return_to_nest(self, current_position: np.ndarray, 
                      nest_position: np.ndarray) -> BehaviorResult:
        """Return to nest with food."""
        distance = np.linalg.norm(current_position - nest_position)
        energy_cost = distance * 0.01
        
        return BehaviorResult(
            success=True,
            energy_expended=energy_cost,
            information_gained={'distance_traveled': distance},
            time_spent=distance * 0.1
        )


class NavigationBehavior:
    """Navigation behavior implementation for insects."""
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """Initialize navigation behavior."""
        self.config = config or {}
        self.orientation_strategy = self.config.get('orientation_strategy', 'sun_compass')
        self.landmark_memory = self.config.get('landmark_memory', True)
        
        # Navigation memory
        self.landmarks = {}
        self.path_integration = np.zeros(3)
        self.orientation_history = []
        
        logger.info("Initialized NavigationBehavior")
    
    def navigate_to_target(self, current_position: np.ndarray, 
                          target_position: np.ndarray) -> BehaviorResult:
        """Navigate to a target location."""
        direction = target_position - current_position
        distance = np.linalg.norm(direction)
        
        if distance < 0.1:  # Close enough
            return BehaviorResult(
                success=True,
                energy_expended=0.0,
                information_gained={'arrived': True},
                time_spent=0.0
            )
        
        # Move toward target
        step_size = min(0.1, distance)
        normalized_direction = direction / distance
        new_position = current_position + normalized_direction * step_size
        
        return BehaviorResult(
            success=False,  # Not yet arrived
            energy_expended=step_size * 0.01,
            information_gained={'new_position': new_position, 'distance_remaining': distance - step_size},
            time_spent=step_size * 0.1
        )
    
    def update_path_integration(self, movement: np.ndarray):
        """Update path integration based on movement."""
        self.path_integration += movement
        self.orientation_history.append(movement.copy())


class CommunicationBehavior:
    """Communication behavior implementation for insects."""
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """Initialize communication behavior."""
        self.config = config or {}
        self.communication_range = self.config.get('communication_range', 1.0)
        self.signal_strength = self.config.get('signal_strength', 0.8)
        
        # Communication memory
        self.signal_history = []
        self.response_history = []
        
        logger.info("Initialized CommunicationBehavior")
    
    def send_signal(self, signal_type: str, position: np.ndarray, 
                   target_position: Optional[np.ndarray] = None) -> BehaviorResult:
        """Send a communication signal."""
        energy_cost = 0.1
        
        if target_position is not None:
            distance = np.linalg.norm(position - target_position)
            if distance > self.communication_range:
                return BehaviorResult(
                    success=False,
                    energy_expended=energy_cost,
                    information_gained={'error': 'out_of_range'},
                    time_spent=0.1
                )
        
        self.signal_history.append({
            'type': signal_type,
            'position': position,
            'strength': self.signal_strength,
            'timestamp': 0.0  # Would be current time
        })
        
        return BehaviorResult(
            success=True,
            energy_expended=energy_cost,
            information_gained={'signal_sent': signal_type, 'strength': self.signal_strength},
            time_spent=0.1
        )
    
    def receive_signal(self, signal_type: str, source_position: np.ndarray, 
                      receiver_position: np.ndarray) -> BehaviorResult:
        """Receive and process a communication signal."""
        distance = np.linalg.norm(source_position - receiver_position)
        
        if distance > self.communication_range:
            return BehaviorResult(
                success=False,
                energy_expended=0.0,
                information_gained={'error': 'signal_too_weak'},
                time_spent=0.0
            )
        
        # Signal strength decreases with distance
        received_strength = self.signal_strength * (1.0 - distance / self.communication_range)
        
        self.response_history.append({
            'type': signal_type,
            'source': source_position,
            'strength': received_strength,
            'timestamp': 0.0  # Would be current time
        })
        
        return BehaviorResult(
            success=True,
            energy_expended=0.01,
            information_gained={'signal_received': signal_type, 'strength': received_strength},
            time_spent=0.05
        )


class SocialBehavior:
    """Social behavior implementation for insects."""
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """Initialize social behavior."""
        self.config = config or {}
        self.social_range = self.config.get('social_range', 0.5)
        self.interaction_probability = self.config.get('interaction_probability', 0.3)
        
        # Social memory
        self.social_network = {}
        self.interaction_history = []
        
        logger.info("Initialized SocialBehavior")
    
    def interact_with_individual(self, self_id: str, other_id: str, 
                               self_position: np.ndarray, other_position: np.ndarray,
                               interaction_type: str) -> BehaviorResult:
        """Interact with another individual."""
        distance = np.linalg.norm(self_position - other_position)
        
        if distance > self.social_range:
            return BehaviorResult(
                success=False,
                energy_expended=0.0,
                information_gained={'error': 'too_far'},
                time_spent=0.0
            )
        
        # Probability of successful interaction
        if np.random.random() > self.interaction_probability:
            return BehaviorResult(
                success=False,
                energy_expended=0.05,
                information_gained={'error': 'interaction_failed'},
                time_spent=0.1
            )
        
        # Record interaction
        if self_id not in self.social_network:
            self.social_network[self_id] = {}
        
        if other_id not in self.social_network[self_id]:
            self.social_network[self_id][other_id] = 0
        
        self.social_network[self_id][other_id] += 1
        
        self.interaction_history.append({
            'self_id': self_id,
            'other_id': other_id,
            'type': interaction_type,
            'success': True,
            'timestamp': 0.0  # Would be current time
        })
        
        return BehaviorResult(
            success=True,
            energy_expended=0.1,
            information_gained={'interaction_success': interaction_type, 'relationship_strength': self.social_network[self_id][other_id]},
            time_spent=0.2
        )
    
    def get_social_connections(self, individual_id: str) -> List[str]:
        """Get list of social connections for an individual."""
        if individual_id in self.social_network:
            return list(self.social_network[individual_id].keys())
        return [] 