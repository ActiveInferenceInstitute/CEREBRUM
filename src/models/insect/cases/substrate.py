"""
Substrate Case Implementation

This module implements the [SUB] substrate case for environmental substrate
interactions in insects, including surface properties, material interactions,
and substrate-dependent behaviors.
"""

from typing import Dict, Any, Optional, List, Tuple
import numpy as np
import logging
from dataclasses import dataclass, field
from enum import Enum

from src.core.model import Case

logger = logging.getLogger(__name__)


class SubstrateType(Enum):
    """Types of substrates that insects interact with."""
    SOIL = "soil"
    LEAF = "leaf"
    BARK = "bark"
    STONE = "stone"
    WATER = "water"
    SAND = "sand"
    CLAY = "clay"
    WOOD = "wood"
    METAL = "metal"
    PLASTIC = "plastic"
    GLASS = "glass"
    FABRIC = "fabric"


@dataclass
class SubstrateProperties:
    """Properties of a substrate surface."""
    substrate_type: SubstrateType
    roughness: float  # Surface roughness (0-1)
    hardness: float  # Material hardness (0-1)
    porosity: float  # Porosity level (0-1)
    moisture: float  # Moisture content (0-1)
    temperature: float  # Surface temperature (Celsius)
    texture: str  # Texture description
    chemical_composition: Dict[str, float]  # Chemical composition
    timestamp: float = field(default_factory=lambda: 0.0)


@dataclass
class SubstrateInteraction:
    """Interaction between an insect and a substrate."""
    insect_id: str
    substrate_type: SubstrateType
    interaction_type: str
    duration: float
    intensity: float
    success: bool
    timestamp: float = field(default_factory=lambda: 0.0)


@dataclass
class SubstrateBehavior:
    """Substrate-dependent behavior pattern."""
    behavior_type: str
    substrate_requirements: Dict[str, Any]
    adaptation_factor: float
    energy_cost: float
    success_rate: float
    timestamp: float = field(default_factory=lambda: 0.0)


class SubstrateCase:
    """
    [SUB] Substrate Case for environmental substrate interactions.
    
    This case specializes in modeling substrate properties, material interactions,
    and substrate-dependent behaviors in insects.
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """
        Initialize substrate case.
        
        Args:
            config: Configuration parameters
        """
        self.config = config or {}
        self._case = Case.NOMINATIVE  # Default case
        self.case_id = "SUB"
        self.case_name = "substrate"
        
        # Substrate property templates
        self.substrate_templates = self._initialize_substrate_templates()
        
        # Interaction parameters
        self.adaptation_rate = self.config.get('adaptation_rate', 0.1)
        self.learning_rate = self.config.get('learning_rate', 0.05)
        self.energy_efficiency = self.config.get('energy_efficiency', 0.8)
        
        # Interaction history
        self.interaction_history: List[SubstrateInteraction] = []
        self.substrate_behaviors: Dict[str, List[SubstrateBehavior]] = {}
        
        # Substrate statistics
        self.substrate_statistics = {}
        
        logger.info("Initialized SubstrateCase")
    
    def _initialize_substrate_templates(self) -> Dict[SubstrateType, Dict[str, Any]]:
        """Initialize templates for different substrate types."""
        return {
            SubstrateType.SOIL: {
                'roughness': 0.7,
                'hardness': 0.3,
                'porosity': 0.8,
                'moisture': 0.6,
                'temperature': 20.0,
                'texture': 'granular',
                'chemical_composition': {'organic_matter': 0.3, 'minerals': 0.7}
            },
            SubstrateType.LEAF: {
                'roughness': 0.4,
                'hardness': 0.2,
                'porosity': 0.3,
                'moisture': 0.8,
                'temperature': 25.0,
                'texture': 'smooth',
                'chemical_composition': {'cellulose': 0.4, 'water': 0.5, 'nutrients': 0.1}
            },
            SubstrateType.BARK: {
                'roughness': 0.9,
                'hardness': 0.6,
                'porosity': 0.5,
                'moisture': 0.4,
                'temperature': 22.0,
                'texture': 'rough',
                'chemical_composition': {'lignin': 0.6, 'cellulose': 0.3, 'resins': 0.1}
            },
            SubstrateType.STONE: {
                'roughness': 0.8,
                'hardness': 0.9,
                'porosity': 0.1,
                'moisture': 0.2,
                'temperature': 18.0,
                'texture': 'hard',
                'chemical_composition': {'minerals': 0.9, 'water': 0.1}
            },
            SubstrateType.WATER: {
                'roughness': 0.1,
                'hardness': 0.0,
                'porosity': 1.0,
                'moisture': 1.0,
                'temperature': 20.0,
                'texture': 'fluid',
                'chemical_composition': {'water': 0.95, 'dissolved_solids': 0.05}
            },
            SubstrateType.SAND: {
                'roughness': 0.6,
                'hardness': 0.4,
                'porosity': 0.9,
                'moisture': 0.3,
                'temperature': 25.0,
                'texture': 'granular',
                'chemical_composition': {'silica': 0.8, 'minerals': 0.2}
            },
            SubstrateType.CLAY: {
                'roughness': 0.3,
                'hardness': 0.5,
                'porosity': 0.4,
                'moisture': 0.7,
                'temperature': 20.0,
                'texture': 'smooth',
                'chemical_composition': {'clay_minerals': 0.7, 'water': 0.3}
            },
            SubstrateType.WOOD: {
                'roughness': 0.5,
                'hardness': 0.4,
                'porosity': 0.6,
                'moisture': 0.5,
                'temperature': 22.0,
                'texture': 'fibrous',
                'chemical_composition': {'cellulose': 0.5, 'lignin': 0.4, 'water': 0.1}
            },
            SubstrateType.METAL: {
                'roughness': 0.2,
                'hardness': 0.9,
                'porosity': 0.0,
                'moisture': 0.1,
                'temperature': 20.0,
                'texture': 'smooth',
                'chemical_composition': {'metal': 0.9, 'oxides': 0.1}
            },
            SubstrateType.PLASTIC: {
                'roughness': 0.1,
                'hardness': 0.3,
                'porosity': 0.0,
                'moisture': 0.0,
                'temperature': 20.0,
                'texture': 'smooth',
                'chemical_composition': {'polymer': 0.9, 'additives': 0.1}
            },
            SubstrateType.GLASS: {
                'roughness': 0.0,
                'hardness': 0.8,
                'porosity': 0.0,
                'moisture': 0.0,
                'temperature': 20.0,
                'texture': 'smooth',
                'chemical_composition': {'silica': 0.8, 'soda': 0.2}
            },
            SubstrateType.FABRIC: {
                'roughness': 0.4,
                'hardness': 0.1,
                'porosity': 0.8,
                'moisture': 0.3,
                'temperature': 22.0,
                'texture': 'fibrous',
                'chemical_composition': {'cellulose': 0.7, 'synthetic': 0.3}
            }
        }
        
        logger.info("Initialized SubstrateCase")
    
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
    
    def create_substrate(self, substrate_type: SubstrateType, 
                        custom_properties: Optional[Dict[str, Any]] = None) -> SubstrateProperties:
        """
        Create a substrate with specified properties.
        
        Args:
            substrate_type: Type of substrate
            custom_properties: Custom properties to override defaults
            
        Returns:
            Substrate properties
        """
        try:
            template = self.substrate_templates.get(substrate_type, {})
            
            # Start with template properties
            properties = template.copy()
            
            # Override with custom properties
            if custom_properties:
                properties.update(custom_properties)
            
            # Create substrate properties
            substrate = SubstrateProperties(
                substrate_type=substrate_type,
                roughness=properties.get('roughness', 0.5),
                hardness=properties.get('hardness', 0.5),
                porosity=properties.get('porosity', 0.5),
                moisture=properties.get('moisture', 0.5),
                temperature=properties.get('temperature', 20.0),
                texture=properties.get('texture', 'unknown'),
                chemical_composition=properties.get('chemical_composition', {}),
                timestamp=self._get_current_time()
            )
            
            logger.debug(f"Created {substrate_type.value} substrate")
            
            return substrate
            
        except Exception as e:
            logger.error(f"Error creating substrate: {e}")
            return SubstrateProperties(
                substrate_type=SubstrateType.SOIL,
                roughness=0.5,
                hardness=0.5,
                porosity=0.5,
                moisture=0.5,
                temperature=20.0,
                texture='unknown',
                chemical_composition={},
                timestamp=self._get_current_time()
            )
    
    def interact_with_substrate(self, insect_id: str, substrate: SubstrateProperties,
                               interaction_type: str, duration: float) -> SubstrateInteraction:
        """
        Simulate interaction between an insect and a substrate.
        
        Args:
            insect_id: ID of the interacting insect
            substrate: Substrate properties
            interaction_type: Type of interaction
            duration: Duration of interaction
            
        Returns:
            Substrate interaction result
        """
        try:
            # Calculate interaction success based on substrate properties
            success_probability = self._calculate_interaction_success(
                substrate, interaction_type, duration
            )
            
            # Determine success
            success = np.random.random() < success_probability
            
            # Calculate interaction intensity
            intensity = self._calculate_interaction_intensity(
                substrate, interaction_type, duration
            )
            
            # Create interaction record
            interaction = SubstrateInteraction(
                insect_id=insect_id,
                substrate_type=substrate.substrate_type,
                interaction_type=interaction_type,
                duration=duration,
                intensity=intensity,
                success=success,
                timestamp=self._get_current_time()
            )
            
            # Record interaction
            self.interaction_history.append(interaction)
            
            # Update substrate statistics
            self._update_substrate_statistics(substrate.substrate_type, interaction)
            
            logger.debug(f"Insect {insect_id} interacted with {substrate.substrate_type.value}: {interaction_type}")
            
            return interaction
            
        except Exception as e:
            logger.error(f"Error during substrate interaction: {e}")
            return SubstrateInteraction(
                insect_id=insect_id,
                substrate_type=substrate.substrate_type,
                interaction_type=interaction_type,
                duration=duration,
                intensity=0.0,
                success=False,
                timestamp=self._get_current_time()
            )
    
    def _calculate_interaction_success(self, substrate: SubstrateProperties,
                                     interaction_type: str, duration: float) -> float:
        """
        Calculate probability of successful interaction.
        
        Args:
            substrate: Substrate properties
            interaction_type: Type of interaction
            duration: Duration of interaction
            
        Returns:
            Success probability (0-1)
        """
        base_probability = 0.5
        
        # Interaction-specific adjustments
        if interaction_type == "walking":
            # Walking success depends on roughness and hardness
            roughness_factor = 1.0 - substrate.roughness * 0.5
            hardness_factor = 1.0 - substrate.hardness * 0.3
            base_probability *= roughness_factor * hardness_factor
            
        elif interaction_type == "digging":
            # Digging success depends on hardness and porosity
            hardness_factor = 1.0 - substrate.hardness * 0.8
            porosity_factor = substrate.porosity * 0.5
            base_probability *= hardness_factor * porosity_factor
            
        elif interaction_type == "climbing":
            # Climbing success depends on roughness and texture
            roughness_factor = substrate.roughness * 0.7
            texture_factor = 1.0 if substrate.texture in ['rough', 'fibrous'] else 0.5
            base_probability *= roughness_factor * texture_factor
            
        elif interaction_type == "feeding":
            # Feeding success depends on chemical composition
            nutrient_content = substrate.chemical_composition.get('nutrients', 0.0)
            organic_content = substrate.chemical_composition.get('organic_matter', 0.0)
            base_probability *= (nutrient_content + organic_content) * 0.5
            
        elif interaction_type == "nesting":
            # Nesting success depends on porosity and moisture
            porosity_factor = substrate.porosity * 0.6
            moisture_factor = substrate.moisture * 0.4
            base_probability *= porosity_factor * moisture_factor
        
        # Duration effects
        duration_factor = min(duration / 10.0, 1.0)  # Longer interactions have higher success
        base_probability *= (0.5 + duration_factor * 0.5)
        
        return min(base_probability, 1.0)
    
    def _calculate_interaction_intensity(self, substrate: SubstrateProperties,
                                       interaction_type: str, duration: float) -> float:
        """
        Calculate intensity of interaction.
        
        Args:
            substrate: Substrate properties
            interaction_type: Type of interaction
            duration: Duration of interaction
            
        Returns:
            Interaction intensity (0-1)
        """
        base_intensity = 0.5
        
        # Duration effects
        intensity = base_intensity * (duration / 5.0)  # Normalize to 5 seconds
        
        # Substrate-specific effects
        if interaction_type == "walking":
            intensity *= (1.0 - substrate.roughness * 0.3)
        elif interaction_type == "digging":
            intensity *= substrate.hardness * 0.8
        elif interaction_type == "climbing":
            intensity *= substrate.roughness * 0.6
        elif interaction_type == "feeding":
            intensity *= 0.3  # Feeding is generally low intensity
        elif interaction_type == "nesting":
            intensity *= substrate.porosity * 0.7
        
        return min(intensity, 1.0)
    
    def get_substrate_behavior(self, substrate: SubstrateProperties,
                              insect_capabilities: Dict[str, float]) -> List[SubstrateBehavior]:
        """
        Get behaviors appropriate for a specific substrate.
        
        Args:
            substrate: Substrate properties
            insect_capabilities: Insect's capabilities
            
        Returns:
            List of appropriate behaviors
        """
        try:
            behaviors = []
            
            # Walking behavior
            if insect_capabilities.get('walking_ability', 0.0) > 0.3:
                behaviors.append(SubstrateBehavior(
                    behavior_type="walking",
                    substrate_requirements={'max_roughness': 0.8, 'max_hardness': 0.7},
                    adaptation_factor=0.8,
                    energy_cost=0.1,
                    success_rate=0.7,
                    timestamp=self._get_current_time()
                ))
            
            # Climbing behavior
            if insect_capabilities.get('climbing_ability', 0.0) > 0.5:
                behaviors.append(SubstrateBehavior(
                    behavior_type="climbing",
                    substrate_requirements={'min_roughness': 0.3, 'min_hardness': 0.2},
                    adaptation_factor=0.9,
                    energy_cost=0.3,
                    success_rate=0.6,
                    timestamp=self._get_current_time()
                ))
            
            # Digging behavior
            if insect_capabilities.get('digging_ability', 0.0) > 0.4:
                behaviors.append(SubstrateBehavior(
                    behavior_type="digging",
                    substrate_requirements={'max_hardness': 0.6, 'min_porosity': 0.3},
                    adaptation_factor=0.7,
                    energy_cost=0.5,
                    success_rate=0.5,
                    timestamp=self._get_current_time()
                ))
            
            # Feeding behavior
            if insect_capabilities.get('feeding_ability', 0.0) > 0.2:
                behaviors.append(SubstrateBehavior(
                    behavior_type="feeding",
                    substrate_requirements={'min_nutrients': 0.1},
                    adaptation_factor=0.6,
                    energy_cost=0.2,
                    success_rate=0.8,
                    timestamp=self._get_current_time()
                ))
            
            # Nesting behavior
            if insect_capabilities.get('nesting_ability', 0.0) > 0.3:
                behaviors.append(SubstrateBehavior(
                    behavior_type="nesting",
                    substrate_requirements={'min_porosity': 0.4, 'min_moisture': 0.2},
                    adaptation_factor=0.8,
                    energy_cost=0.4,
                    success_rate=0.6,
                    timestamp=self._get_current_time()
                ))
            
            # Filter behaviors based on substrate compatibility
            compatible_behaviors = []
            for behavior in behaviors:
                if self._is_behavior_compatible(behavior, substrate):
                    compatible_behaviors.append(behavior)
            
            return compatible_behaviors
            
        except Exception as e:
            logger.error(f"Error getting substrate behaviors: {e}")
            return []
    
    def _is_behavior_compatible(self, behavior: SubstrateBehavior, 
                               substrate: SubstrateProperties) -> bool:
        """
        Check if a behavior is compatible with a substrate.
        
        Args:
            behavior: Behavior to check
            substrate: Substrate properties
            
        Returns:
            True if compatible, False otherwise
        """
        requirements = behavior.substrate_requirements
        
        for req_key, req_value in requirements.items():
            if req_key == 'max_roughness':
                if substrate.roughness > req_value:
                    return False
            elif req_key == 'min_roughness':
                if substrate.roughness < req_value:
                    return False
            elif req_key == 'max_hardness':
                if substrate.hardness > req_value:
                    return False
            elif req_key == 'min_hardness':
                if substrate.hardness < req_value:
                    return False
            elif req_key == 'min_porosity':
                if substrate.porosity < req_value:
                    return False
            elif req_key == 'min_moisture':
                if substrate.moisture < req_value:
                    return False
            elif req_key == 'min_nutrients':
                nutrient_content = substrate.chemical_composition.get('nutrients', 0.0)
                if nutrient_content < req_value:
                    return False
        
        return True
    
    def _update_substrate_statistics(self, substrate_type: SubstrateType, 
                                   interaction: SubstrateInteraction):
        """Update statistics for a substrate type."""
        if substrate_type.value not in self.substrate_statistics:
            self.substrate_statistics[substrate_type.value] = {
                'total_interactions': 0,
                'successful_interactions': 0,
                'total_duration': 0.0,
                'average_intensity': 0.0
            }
        
        stats = self.substrate_statistics[substrate_type.value]
        stats['total_interactions'] += 1
        stats['total_duration'] += interaction.duration
        
        if interaction.success:
            stats['successful_interactions'] += 1
        
        # Update average intensity
        current_avg = stats['average_intensity']
        total_interactions = stats['total_interactions']
        stats['average_intensity'] = (
            (current_avg * (total_interactions - 1) + interaction.intensity) / 
            total_interactions
        )
    
    def get_substrate_statistics(self) -> Dict[str, Any]:
        """
        Get statistics about substrate interactions.
        
        Returns:
            Dictionary with substrate statistics
        """
        return {
            'substrate_statistics': self.substrate_statistics,
            'total_interactions': len(self.interaction_history),
            'successful_interactions': sum(1 for i in self.interaction_history if i.success),
            'average_duration': np.mean([i.duration for i in self.interaction_history]) if self.interaction_history else 0.0,
            'average_intensity': np.mean([i.intensity for i in self.interaction_history]) if self.interaction_history else 0.0
        }
    
    def _get_current_time(self) -> float:
        """Get current simulation time."""
        import time
        return time.time() 