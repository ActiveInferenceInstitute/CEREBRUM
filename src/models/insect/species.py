#!/usr/bin/env python3
"""
Insect Species Models for CEREBRUM

This module provides species-specific implementations of insect models,
including honeybees, ants, and fruit flies with their unique characteristics.
"""

import numpy as np
import logging
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field

from .base import InsectModel, InsectActiveInferenceModel
from .neural_structures import (
    MushroomBody, CentralComplex, AntennalLobe, 
    OpticLobe, SubesophagealGanglion, VentralNerveCord
)
from .cases import (
    PheromonalCase, SwarmCase, MetamorphicCase,
    CasteCase, SubstrateCase, StigmergicCase
)
from .behaviors import (
    ForagingBehavior, NavigationBehavior, 
    CommunicationBehavior, SocialBehavior
)

logger = logging.getLogger(__name__)


@dataclass
class SpeciesCharacteristics:
    """Characteristics specific to an insect species."""
    name: str
    size: float  # Relative size
    lifespan: float  # Average lifespan in days
    social_structure: str  # 'solitary', 'eusocial', 'primitively_social'
    foraging_range: float  # Typical foraging range
    communication_modes: List[str]  # Available communication modes
    neural_complexity: float  # Relative neural complexity (0-1)
    learning_capacity: float  # Learning capacity (0-1)
    specialization: List[str]  # Specialized behaviors


class HoneybeeModel(InsectModel):
    """Honeybee (Apis mellifera) specific model."""
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """Initialize honeybee model."""
        super().__init__("Honeybee", config)
        
        # Species-specific characteristics
        self.characteristics = SpeciesCharacteristics(
            name="Apis mellifera",
            size=0.8,
            lifespan=30.0,  # Worker lifespan
            social_structure="eusocial",
            foraging_range=5.0,
            communication_modes=["waggle_dance", "pheromones", "trophallaxis"],
            neural_complexity=0.9,
            learning_capacity=0.8,
            specialization=["nectar_collection", "pollen_collection", "hive_defense"]
        )
        
        # Honeybee-specific neural structures
        self.mushroom_body = MushroomBody(
            config={'kenyon_cells': 170000, 'calyx_lobes': 2}
        )
        self.central_complex = CentralComplex(
            config={'compass_accuracy': 0.95, 'path_integration': True}
        )
        self.antennal_lobe = AntennalLobe(
            config={'glomeruli': 160, 'pheromone_sensitivity': 0.9}
        )
        self.optic_lobe = OpticLobe(
            config={'ommatidia': 5500, 'color_vision': True, 'polarization_sensitivity': True}
        )
        
        # Honeybee-specific cases
        self.pheromonal_case = PheromonalCase(
            config={'queen_pheromone_sensitivity': 0.95, 'waggle_dance_decoding': True}
        )
        self.swarm_case = SwarmCase(
            config={'swarm_decision_threshold': 0.7, 'dance_communication': True}
        )
        self.caste_case = CasteCase(
            config={'queen_development': True, 'worker_specialization': True}
        )
        
        # Behavioral modules
        self.foraging_behavior = ForagingBehavior(
            config={'waggle_dance_communication': True, 'nectar_processing': True}
        )
        self.navigation_behavior = NavigationBehavior(
            config={'sun_compass': True, 'landmark_memory': True, 'path_integration': True}
        )
        self.communication_behavior = CommunicationBehavior(
            config={'waggle_dance': True, 'pheromone_communication': True}
        )
        self.social_behavior = SocialBehavior(
            config={'trophallaxis': True, 'hive_coordination': True}
        )
        
        logger.info("Initialized HoneybeeModel")
    
    def perform_waggle_dance(self, food_location: np.ndarray, food_quality: float) -> Dict[str, Any]:
        """Perform waggle dance to communicate food location."""
        # Calculate dance parameters
        distance = np.linalg.norm(food_location - self.position)
        direction = np.arctan2(food_location[1] - self.position[1], 
                             food_location[0] - self.position[0])
        
        # Waggle duration correlates with distance
        waggle_duration = distance * 0.1
        
        # Dance angle correlates with direction
        dance_angle = direction
        
        dance_info = {
            'distance': distance,
            'direction': direction,
            'quality': food_quality,
            'waggle_duration': waggle_duration,
            'dance_angle': dance_angle,
            'success': True
        }
        
        logger.debug(f"Honeybee performed waggle dance: {dance_info}")
        return dance_info
    
    def decode_waggle_dance(self, dance_info: Dict[str, Any]) -> np.ndarray:
        """Decode waggle dance to extract food location."""
        distance = dance_info['distance']
        direction = dance_info['direction']
        
        # Convert polar coordinates to cartesian
        food_location = self.position + np.array([
            distance * np.cos(direction),
            distance * np.sin(direction),
            0.0
        ])
        
        return food_location


class AntModel(InsectModel):
    """Ant (Formicidae) specific model."""
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """Initialize ant model."""
        super().__init__("Ant", config)
        
        # Species-specific characteristics
        self.characteristics = SpeciesCharacteristics(
            name="Formicidae",
            size=0.3,
            lifespan=365.0,  # Queen lifespan
            social_structure="eusocial",
            foraging_range=10.0,
            communication_modes=["pheromones", "tandem_running", "stigmergy"],
            neural_complexity=0.7,
            learning_capacity=0.6,
            specialization=["trail_following", "nest_building", "colony_defense"]
        )
        
        # Ant-specific neural structures
        self.mushroom_body = MushroomBody(
            config={'kenyon_cells': 250000, 'calyx_lobes': 2}
        )
        self.central_complex = CentralComplex(
            config={'compass_accuracy': 0.8, 'path_integration': True}
        )
        self.antennal_lobe = AntennalLobe(
            config={'glomeruli': 400, 'pheromone_sensitivity': 0.95}
        )
        self.optic_lobe = OpticLobe(
            config={'ommatidia': 1000, 'color_vision': False, 'motion_detection': True}
        )
        
        # Ant-specific cases
        self.pheromonal_case = PheromonalCase(
            config={'trail_pheromone_sensitivity': 0.95, 'alarm_pheromone_response': True}
        )
        self.swarm_case = SwarmCase(
            config={'mass_recruitment': True, 'tandem_running': True}
        )
        self.stigmergic_case = StigmergicCase(
            config={'nest_building_coordination': True, 'trail_marking': True}
        )
        self.substrate_case = SubstrateCase(
            config={'soil_excavation': True, 'material_transport': True}
        )
        
        # Behavioral modules
        self.foraging_behavior = ForagingBehavior(
            config={'trail_following': True, 'mass_recruitment': True}
        )
        self.navigation_behavior = NavigationBehavior(
            config={'path_integration': True, 'landmark_memory': True}
        )
        self.communication_behavior = CommunicationBehavior(
            config={'pheromone_communication': True, 'tandem_running': True}
        )
        self.social_behavior = SocialBehavior(
            config={'colony_coordination': True, 'task_allocation': True}
        )
        
        logger.info("Initialized AntModel")
    
    def lay_trail_pheromone(self, position: np.ndarray, concentration: float) -> Dict[str, Any]:
        """Lay trail pheromone for navigation."""
        trail_info = {
            'position': position,
            'concentration': concentration,
            'pheromone_type': 'trail',
            'timestamp': 0.0,  # Would be current time
            'success': True
        }
        
        logger.debug(f"Ant laid trail pheromone: {trail_info}")
        return trail_info
    
    def follow_trail(self, trail_position: np.ndarray, trail_concentration: float) -> bool:
        """Follow pheromone trail."""
        if trail_concentration > 0.1:  # Threshold for detection
            # Move toward trail
            direction = trail_position - self.position
            if np.linalg.norm(direction) > 0.1:
                normalized_direction = direction / np.linalg.norm(direction)
                self.position += normalized_direction * 0.1
            return True
        return False


class FruitFlyModel(InsectModel):
    """Fruit fly (Drosophila melanogaster) specific model."""
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """Initialize fruit fly model."""
        super().__init__("FruitFly", config)
        
        # Species-specific characteristics
        self.characteristics = SpeciesCharacteristics(
            name="Drosophila melanogaster",
            size=0.1,
            lifespan=60.0,
            social_structure="solitary",
            foraging_range=1.0,
            communication_modes=["courtship_song", "pheromones", "visual_signals"],
            neural_complexity=0.8,
            learning_capacity=0.9,
            specialization=["olfactory_learning", "visual_navigation", "courtship"]
        )
        
        # Fruit fly-specific neural structures
        self.mushroom_body = MushroomBody(
            config={'kenyon_cells': 2500, 'calyx_lobes': 3}
        )
        self.central_complex = CentralComplex(
            config={'compass_accuracy': 0.7, 'path_integration': True}
        )
        self.antennal_lobe = AntennalLobe(
            config={'glomeruli': 50, 'pheromone_sensitivity': 0.8}
        )
        self.optic_lobe = OpticLobe(
            config={'ommatidia': 800, 'color_vision': True, 'motion_detection': True}
        )
        
        # Fruit fly-specific cases
        self.pheromonal_case = PheromonalCase(
            config={'courtship_pheromone_sensitivity': 0.9, 'aggregation_pheromone': True}
        )
        self.metamorphic_case = MetamorphicCase(
            config={'larval_development': True, 'pupal_transformation': True}
        )
        self.substrate_case = SubstrateCase(
            config={'fruit_oviposition': True, 'substrate_preference': True}
        )
        
        # Behavioral modules
        self.foraging_behavior = ForagingBehavior(
            config={'fruit_detection': True, 'yeast_feeding': True}
        )
        self.navigation_behavior = NavigationBehavior(
            config={'visual_navigation': True, 'wind_orientation': True}
        )
        self.communication_behavior = CommunicationBehavior(
            config={'courtship_song': True, 'visual_signals': True}
        )
        self.social_behavior = SocialBehavior(
            config={'courtship_behavior': True, 'territory_defense': True}
        )
        
        logger.info("Initialized FruitFlyModel")
    
    def perform_courtship_song(self, target_position: np.ndarray) -> Dict[str, Any]:
        """Perform courtship song."""
        distance = np.linalg.norm(target_position - self.position)
        
        if distance < 0.5:  # Close enough for courtship
            song_info = {
                'song_type': 'courtship',
                'frequency': 150,  # Hz
                'duration': 2.0,  # seconds
                'intensity': 0.8,
                'success': True
            }
            
            logger.debug(f"Fruit fly performed courtship song: {song_info}")
            return song_info
        
        return {'success': False, 'error': 'too_far'}
    
    def detect_fruit(self, fruit_position: np.ndarray, fruit_ripeness: float) -> bool:
        """Detect and approach fruit."""
        distance = np.linalg.norm(fruit_position - self.position)
        
        if distance < 0.3 and fruit_ripeness > 0.5:
            # Move toward fruit
            direction = fruit_position - self.position
            if np.linalg.norm(direction) > 0.05:
                normalized_direction = direction / np.linalg.norm(direction)
                self.position += normalized_direction * 0.05
            return True
        
        return False 