"""
Base Insect Model Implementation

This module provides the foundational classes for insect cognitive models
using the CEREBRUM case framework and active inference principles.
"""

from typing import Dict, Any, Optional, List, Union, Tuple
import numpy as np
import logging
from dataclasses import dataclass, field
from enum import Enum

from src.core.active_inference import ActiveInferenceModel
from src.core.model import Case

logger = logging.getLogger(__name__)


class BehavioralState(Enum):
    """Enumeration of possible behavioral states for insects."""
    IDLE = "idle"
    FORAGING = "foraging"
    NAVIGATING = "navigating"
    COMMUNICATING = "communicating"
    SOCIAL_INTERACTING = "social_interacting"
    ESCAPING = "escaping"
    MATING = "mating"
    NEST_BUILDING = "nest_building"
    GROOMING = "grooming"
    RESTING = "resting"


@dataclass
class SensoryInput:
    """Structured sensory input for insect models."""
    visual: Optional[np.ndarray] = None
    olfactory: Optional[np.ndarray] = None
    mechanosensory: Optional[np.ndarray] = None
    gustatory: Optional[np.ndarray] = None
    thermal: Optional[np.ndarray] = None
    pheromonal: Optional[np.ndarray] = None
    timestamp: float = field(default_factory=lambda: 0.0)


@dataclass
class Action:
    """Structured action output for insect models."""
    action_type: str
    parameters: Dict[str, Any]
    confidence: float
    timestamp: float = field(default_factory=lambda: 0.0)


class InsectModel(ActiveInferenceModel):
    """
    Base class for insect cognitive models with CEREBRUM case support.
    
    This class provides the foundational functionality for modeling insect cognition,
    including case transformations, neural structure management, and behavioral
    state transitions.
    """
    
    def __init__(
        self,
        species: str,
        neural_config: Optional[Dict[str, Any]] = None,
        initial_case: Case = Case.NOMINATIVE,
        initial_state: BehavioralState = BehavioralState.IDLE
    ):
        """
        Initialize an insect model.
        
        Args:
            species: The insect species being modeled
            neural_config: Configuration for neural structures
            initial_case: Initial CEREBRUM case assignment
            initial_state: Initial behavioral state
        """
        # Initialize with default parameters for active inference
        default_parameters = {
            'transition_matrix': np.eye(5),  # 5 behavioral states
            'observation_matrix': np.eye(5),
            'n_states': 5,
            'n_actions': 3,
            'n_observations': 5
        }
        
        super().__init__(name=f"{species}_model", parameters=default_parameters)
        
        self.species = species
        self.current_case = initial_case
        self.behavioral_state = initial_state
        
        # Initialize neural structures
        self.neural_structures = self._initialize_neural_structures(neural_config or {})
        
        # Initialize case assignments
        self.case_assignments = self._initialize_case_assignments()
        
        # Initialize behavioral modules
        self.behavioral_modules = self._initialize_behavioral_modules()
        
        # State tracking
        self.sensory_history = []
        self.action_history = []
        self.case_history = []
        self.state_history = []
        
        # Performance metrics
        self.performance_metrics = {
            'total_actions': 0,
            'case_transformations': 0,
            'sensory_processing_time': [],
            'action_selection_time': []
        }
        
        logger.info(f"Initialized {species} model with case {initial_case}")
    
    def _initialize_neural_structures(self, config: Dict[str, Any]) -> Dict[str, Any]:
        """
        Initialize neural structures based on configuration.
        
        Args:
            config: Configuration dictionary for neural structures
            
        Returns:
            Dictionary of initialized neural structures
        """
        neural_structures = {}
        
        # Mushroom Body - Learning and Memory
        neural_structures['mushroom_body'] = {
            'type': 'mushroom_body',
            'activity': np.random.rand(100),  # 100 Kenyon cells
            'learning_rate': config.get('mushroom_body_learning_rate', 0.01),
            'memory_capacity': config.get('mushroom_body_memory', 1000),
            'case_assignment': Case.ACCUSATIVE,
            'connections': {
                'input': ['antennal_lobe', 'optic_lobe'],
                'output': ['central_complex', 'subesophageal_ganglion']
            }
        }
        
        # Central Complex - Navigation and Spatial Processing
        neural_structures['central_complex'] = {
            'type': 'central_complex',
            'activity': np.random.rand(50),  # 50 neurons
            'spatial_map': np.zeros((10, 10)),  # 10x10 spatial grid
            'heading_direction': 0.0,
            'case_assignment': Case.NOMINATIVE,
            'connections': {
                'input': ['mushroom_body', 'optic_lobe'],
                'output': ['ventral_nerve_cord']
            }
        }
        
        # Antennal Lobe - Olfactory Processing
        neural_structures['antennal_lobe'] = {
            'type': 'antennal_lobe',
            'activity': np.random.rand(30),  # 30 glomeruli
            'odor_responses': np.random.rand(30),
            'case_assignment': Case.DATIVE,
            'connections': {
                'input': ['sensory_input'],
                'output': ['mushroom_body', 'subesophageal_ganglion']
            }
        }
        
        # Optic Lobe - Visual Processing
        neural_structures['optic_lobe'] = {
            'type': 'optic_lobe',
            'activity': np.random.rand(200),  # 200 visual neurons
            'motion_detection': np.random.rand(20),
            'color_processing': np.random.rand(10),
            'case_assignment': Case.DATIVE,
            'connections': {
                'input': ['sensory_input'],
                'output': ['mushroom_body', 'central_complex']
            }
        }
        
        # Subesophageal Ganglion - Motor Control
        neural_structures['subesophageal_ganglion'] = {
            'type': 'subesophageal_ganglion',
            'activity': np.random.rand(40),  # 40 motor neurons
            'motor_commands': np.zeros(10),
            'case_assignment': Case.GENITIVE,
            'connections': {
                'input': ['mushroom_body', 'central_complex'],
                'output': ['motor_output']
            }
        }
        
        # Ventral Nerve Cord - Coordination
        neural_structures['ventral_nerve_cord'] = {
            'type': 'ventral_nerve_cord',
            'activity': np.random.rand(60),  # 60 coordination neurons
            'rhythm_generators': np.random.rand(5),
            'case_assignment': Case.INSTRUMENTAL,
            'connections': {
                'input': ['central_complex', 'subesophageal_ganglion'],
                'output': ['motor_output']
            }
        }
        
        # Add neural structure as attribute for visualization
        self.neural_structure = neural_structures
        
        return neural_structures
    
    def _initialize_case_assignments(self) -> Dict[str, Case]:
        """
        Initialize case assignments for different neural structures.
        
        Returns:
            Dictionary mapping neural structure names to their primary cases
        """
        return {
            'mushroom_body': Case.ACCUSATIVE,
            'central_complex': Case.NOMINATIVE,
            'antennal_lobe': Case.DATIVE,
            'optic_lobe': Case.DATIVE,
            'subesophageal_ganglion': Case.GENITIVE,
            'ventral_nerve_cord': Case.INSTRUMENTAL
        }
    
    def _initialize_behavioral_modules(self) -> Dict[str, Any]:
        """
        Initialize behavioral modules.
        
        Returns:
            Dictionary of behavioral modules
        """
        behavioral_modules = {}
        
        # Foraging Module
        behavioral_modules['foraging'] = {
            'type': 'foraging',
            'energy_level': 1.0,
            'food_memory': [],
            'search_pattern': 'spiral',
            'efficiency': 0.8,
            'case_assignment': Case.ACCUSATIVE,
            'parameters': {
                'search_radius': 10.0,
                'energy_threshold': 0.3,
                'memory_decay': 0.95
            }
        }
        
        # Navigation Module
        behavioral_modules['navigation'] = {
            'type': 'navigation',
            'current_position': np.array([0.0, 0.0, 0.0]),
            'home_position': np.array([0.0, 0.0, 0.0]),
            'path_memory': [],
            'case_assignment': Case.NOMINATIVE,
            'parameters': {
                'max_speed': 2.0,
                'turning_rate': 0.5,
                'path_smoothing': 0.8
            }
        }
        
        # Communication Module
        behavioral_modules['communication'] = {
            'type': 'communication',
            'pheromone_trail': [],
            'signal_strength': 1.0,
            'reception_sensitivity': 0.8,
            'case_assignment': Case.DATIVE,
            'parameters': {
                'signal_decay': 0.9,
                'reception_range': 5.0,
                'encoding_efficiency': 0.7
            }
        }
        
        # Social Interaction Module
        behavioral_modules['social'] = {
            'type': 'social',
            'colony_members': [],
            'hierarchy_position': 0,
            'cooperation_level': 0.6,
            'case_assignment': Case.GENITIVE,
            'parameters': {
                'interaction_range': 3.0,
                'hierarchy_weight': 0.5,
                'cooperation_threshold': 0.4
            }
        }
        
        # Motor Control Module
        behavioral_modules['motor'] = {
            'type': 'motor',
            'current_action': 'idle',
            'action_queue': [],
            'motor_commands': np.zeros(6),  # 6 degrees of freedom
            'case_assignment': Case.INSTRUMENTAL,
            'parameters': {
                'max_force': 1.0,
                'response_time': 0.1,
                'precision': 0.8
            }
        }
        
        # Sensory Processing Module
        behavioral_modules['sensory'] = {
            'type': 'sensory',
            'sensory_filters': {
                'visual': np.ones(10),
                'olfactory': np.ones(5),
                'mechanosensory': np.ones(4)
            },
            'attention_focus': 'general',
            'case_assignment': Case.DATIVE,
            'parameters': {
                'filter_decay': 0.98,
                'attention_switch_rate': 0.2,
                'sensitivity_threshold': 0.1
            }
        }
        
        return behavioral_modules
    
    def transform_case(self, target_case: Case) -> bool:
        """
        Transform the model to a different case configuration.
        
        Args:
            target_case: The target case to transform to
            
        Returns:
            True if transformation was successful, False otherwise
        """
        if target_case == self.current_case:
            return True
        
        try:
            # Validate case transformation
            if not self._validate_case_transformation(target_case):
                logger.warning(f"Invalid case transformation: {self.current_case} -> {target_case}")
                return False
            
            # Update precision parameters for the new case
            self._update_precision_for_case(target_case)
            
            # Update neural structure priorities
            self._update_neural_priorities(target_case)
            
            # Record transformation
            old_case = self.current_case
            self.current_case = target_case
            self.case_history.append((old_case, target_case, self.performance_metrics['total_actions']))
            self.performance_metrics['case_transformations'] += 1
            
            logger.info(f"Transformed case: {old_case} -> {target_case}")
            return True
            
        except Exception as e:
            logger.error(f"Error during case transformation: {e}")
            return False
    
    def _validate_case_transformation(self, target_case: Case) -> bool:
        """
        Validate if a case transformation is allowed.
        
        Args:
            target_case: The target case to validate
            
        Returns:
            True if transformation is valid, False otherwise
        """
        # Define valid case transitions based on insect behavior patterns
        valid_transitions = {
            Case.NOMINATIVE: [Case.ACCUSATIVE, Case.DATIVE, Case.INSTRUMENTAL, Case.GENITIVE],
            Case.ACCUSATIVE: [Case.NOMINATIVE, Case.ABLATIVE, Case.LOCATIVE],
            Case.DATIVE: [Case.NOMINATIVE, Case.ACCUSATIVE, Case.GENITIVE],
            Case.GENITIVE: [Case.NOMINATIVE, Case.DATIVE, Case.INSTRUMENTAL],
            Case.INSTRUMENTAL: [Case.NOMINATIVE, Case.LOCATIVE, Case.GENITIVE],
            Case.LOCATIVE: [Case.NOMINATIVE, Case.INSTRUMENTAL, Case.ABLATIVE],
            Case.ABLATIVE: [Case.ACCUSATIVE, Case.LOCATIVE, Case.NOMINATIVE],
            Case.VOCATIVE: [Case.NOMINATIVE, Case.DATIVE, Case.GENITIVE]
        }
        
        return target_case in valid_transitions.get(self.current_case, [])
    
    def _update_precision_for_case(self, target_case: Case):
        """
        Update precision parameters for the target case.
        
        Args:
            target_case: The target case to update precision for
        """
        # Case-specific precision configurations
        precision_configs = {
            Case.NOMINATIVE: {
                'action_precision': 1.0,
                'prediction_precision': 0.8,
                'sensory_precision': 0.6
            },
            Case.ACCUSATIVE: {
                'learning_precision': 1.0,
                'sensory_precision': 0.8,
                'action_precision': 0.4
            },
            Case.DATIVE: {
                'sensory_precision': 1.0,
                'input_precision': 0.9,
                'action_precision': 0.3
            },
            Case.GENITIVE: {
                'output_precision': 1.0,
                'action_precision': 0.8,
                'sensory_precision': 0.5
            },
            Case.INSTRUMENTAL: {
                'method_precision': 1.0,
                'action_precision': 0.7,
                'sensory_precision': 0.6
            },
            Case.LOCATIVE: {
                'context_precision': 1.0,
                'spatial_precision': 0.9,
                'action_precision': 0.6
            },
            Case.ABLATIVE: {
                'memory_precision': 1.0,
                'historical_precision': 0.8,
                'action_precision': 0.5
            },
            Case.VOCATIVE: {
                'identification_precision': 1.0,
                'communication_precision': 0.8,
                'action_precision': 0.6
            }
        }
        
        config = precision_configs.get(target_case, {})
        for param, value in config.items():
            if hasattr(self, param):
                setattr(self, param, value)
    
    def _update_neural_priorities(self, target_case: Case):
        """
        Update neural structure priorities based on case.
        
        Args:
            target_case: The target case to update priorities for
        """
        # Case-specific neural structure priorities
        priorities = {
            Case.NOMINATIVE: ['central_complex', 'mushroom_body', 'subesophageal_ganglion'],
            Case.ACCUSATIVE: ['mushroom_body', 'antennal_lobe', 'optic_lobe'],
            Case.DATIVE: ['antennal_lobe', 'optic_lobe', 'mushroom_body'],
            Case.GENITIVE: ['subesophageal_ganglion', 'ventral_nerve_cord', 'central_complex'],
            Case.INSTRUMENTAL: ['central_complex', 'ventral_nerve_cord', 'mushroom_body'],
            Case.LOCATIVE: ['central_complex', 'optic_lobe', 'mushroom_body'],
            Case.ABLATIVE: ['mushroom_body', 'central_complex', 'antennal_lobe'],
            Case.VOCATIVE: ['antennal_lobe', 'subesophageal_ganglion', 'central_complex']
        }
        
        # Update priorities in neural structures
        priority_list = priorities.get(target_case, [])
        for i, structure_name in enumerate(priority_list):
            if structure_name in self.neural_structure:
                # Handle both NeuralStructureProcessor objects and dictionaries
                structure = self.neural_structure[structure_name]
                if hasattr(structure, 'priority'):
                    structure.priority = len(priority_list) - i
                elif isinstance(structure, dict):
                    structure['priority'] = len(priority_list) - i
    
    def process_sensory_input(self, input_data: SensoryInput) -> Dict[str, Any]:
        """
        Process incoming sensory data through appropriate cases.
        
        Args:
            input_data: Structured sensory input
            
        Returns:
            Processed sensory information
        """
        import time
        start_time = time.time()
        
        try:
            # Add to sensory history
            self.sensory_history.append(input_data)
            
            # Process through neural structures based on current case
            processed_data = {}
            
            # Process visual input
            if input_data.visual is not None:
                if 'optic_lobe' in self.neural_structure:
                    processed_data['visual'] = self.neural_structure['optic_lobe'].process_input(input_data.visual)
            
            # Process olfactory input
            if input_data.olfactory is not None:
                if 'antennal_lobe' in self.neural_structure:
                    processed_data['olfactory'] = self.neural_structure['antennal_lobe'].process_input(input_data.olfactory)
            
            # Process pheromonal input
            if input_data.pheromonal is not None:
                if 'antennal_lobe' in self.neural_structure:
                    processed_data['pheromonal'] = self.neural_structure['antennal_lobe'].process_input(input_data.pheromonal)
            
            # Process mechanosensory input
            if input_data.mechanosensory is not None:
                if 'ventral_nerve_cord' in self.neural_structure:
                    processed_data['mechanosensory'] = self.neural_structure['ventral_nerve_cord'].process_input(input_data.mechanosensory)
            
            # Update performance metrics
            processing_time = time.time() - start_time
            self.performance_metrics['sensory_processing_time'].append(processing_time)
            
            return processed_data
            
        except Exception as e:
            logger.error(f"Error processing sensory input: {e}")
            return {}
    
    def select_action(self, context: Dict[str, Any]) -> Action:
        """
        Select appropriate action based on current case and context.
        
        Args:
            context: Current environmental and internal context
            
        Returns:
            Selected action
        """
        import time
        start_time = time.time()
        
        try:
            # Determine action based on current case and behavioral state
            action = self._select_action_by_case(context)
            
            # Add to action history
            self.action_history.append(action)
            self.performance_metrics['total_actions'] += 1
            
            # Update performance metrics
            selection_time = time.time() - start_time
            self.performance_metrics['action_selection_time'].append(selection_time)
            
            return action
            
        except Exception as e:
            logger.error(f"Error selecting action: {e}")
            # Return default idle action
            return Action(
                action_type="idle",
                parameters={},
                confidence=0.0,
                timestamp=time.time()
            )
    
    def _select_action_by_case(self, context: Dict[str, Any]) -> Action:
        """
        Select action based on current case.
        
        Args:
            context: Current context
            
        Returns:
            Selected action
        """
        case_actions = {
            Case.NOMINATIVE: self._select_nominative_action,
            Case.ACCUSATIVE: self._select_accusative_action,
            Case.DATIVE: self._select_dative_action,
            Case.GENITIVE: self._select_genitive_action,
            Case.INSTRUMENTAL: self._select_instrumental_action,
            Case.LOCATIVE: self._select_locative_action,
            Case.ABLATIVE: self._select_ablative_action,
            Case.VOCATIVE: self._select_vocative_action
        }
        
        action_selector = case_actions.get(self.current_case, self._select_default_action)
        return action_selector(context)
    
    def _select_nominative_action(self, context: Dict[str, Any]) -> Action:
        """Select action for nominative case (active agent)."""
        # Implement active behavior selection
        return Action(
            action_type="explore",
            parameters={"direction": "forward", "speed": 1.0},
            confidence=0.8,
            timestamp=context.get('timestamp', 0.0)
        )
    
    def _select_accusative_action(self, context: Dict[str, Any]) -> Action:
        """Select action for accusative case (learning target)."""
        # Implement learning-focused behavior
        return Action(
            action_type="observe",
            parameters={"focus": "sensory_input", "duration": 1.0},
            confidence=0.7,
            timestamp=context.get('timestamp', 0.0)
        )
    
    def _select_dative_action(self, context: Dict[str, Any]) -> Action:
        """Select action for dative case (sensory recipient)."""
        # Implement sensory-focused behavior
        return Action(
            action_type="sense",
            parameters={"modality": "all", "intensity": 1.0},
            confidence=0.9,
            timestamp=context.get('timestamp', 0.0)
        )
    
    def _select_genitive_action(self, context: Dict[str, Any]) -> Action:
        """Select action for genitive case (output producer)."""
        # Implement output generation
        return Action(
            action_type="produce",
            parameters={"output_type": "pheromone", "concentration": 1.0},
            confidence=0.8,
            timestamp=context.get('timestamp', 0.0)
        )
    
    def _select_instrumental_action(self, context: Dict[str, Any]) -> Action:
        """Select action for instrumental case (tool/method)."""
        # Implement method-based behavior
        return Action(
            action_type="execute_method",
            parameters={"method": "path_integration", "parameters": {}},
            confidence=0.8,
            timestamp=context.get('timestamp', 0.0)
        )
    
    def _select_locative_action(self, context: Dict[str, Any]) -> Action:
        """Select action for locative case (spatial context)."""
        # Implement spatial behavior
        return Action(
            action_type="navigate",
            parameters={"target": "home", "method": "path_integration"},
            confidence=0.9,
            timestamp=context.get('timestamp', 0.0)
        )
    
    def _select_ablative_action(self, context: Dict[str, Any]) -> Action:
        """Select action for ablative case (origin/source)."""
        # Implement memory-based behavior
        return Action(
            action_type="recall",
            parameters={"memory_type": "reward_location", "recency": 1.0},
            confidence=0.7,
            timestamp=context.get('timestamp', 0.0)
        )
    
    def _select_vocative_action(self, context: Dict[str, Any]) -> Action:
        """Select action for vocative case (addressed entity)."""
        # Implement communication behavior
        return Action(
            action_type="communicate",
            parameters={"signal_type": "alarm", "intensity": 1.0},
            confidence=0.8,
            timestamp=context.get('timestamp', 0.0)
        )
    
    def _select_default_action(self, context: Dict[str, Any]) -> Action:
        """Default action selection."""
        return Action(
            action_type="idle",
            parameters={},
            confidence=0.5,
            timestamp=context.get('timestamp', 0.0)
        )
    
    def update_behavioral_state(self, new_state: BehavioralState):
        """
        Update the behavioral state of the insect.
        
        Args:
            new_state: New behavioral state
        """
        old_state = self.behavioral_state
        self.behavioral_state = new_state
        self.state_history.append((old_state, new_state, self.performance_metrics['total_actions']))
        
        logger.info(f"Behavioral state change: {old_state} -> {new_state}")
    
    def get_performance_summary(self) -> Dict[str, Any]:
        """
        Get a summary of model performance metrics.
        
        Returns:
            Dictionary containing performance metrics
        """
        return {
            'total_actions': self.performance_metrics['total_actions'],
            'case_transformations': self.performance_metrics['case_transformations'],
            'avg_sensory_processing_time': np.mean(self.performance_metrics['sensory_processing_time']) if self.performance_metrics['sensory_processing_time'] else 0.0,
            'avg_action_selection_time': np.mean(self.performance_metrics['action_selection_time']) if self.performance_metrics['action_selection_time'] else 0.0,
            'current_case': self.current_case,
            'current_behavioral_state': self.behavioral_state,
            'sensory_history_length': len(self.sensory_history),
            'action_history_length': len(self.action_history)
        }


class InsectActiveInferenceModel(InsectModel):
    """
    Insect model with enhanced active inference capabilities.
    
    This class extends the base insect model with active inference principles,
    including free energy computation, belief updating, and action selection
    based on expected free energy minimization.
    """
    
    def __init__(
        self,
        species: str,
        neural_config: Optional[Dict[str, Any]] = None,
        initial_case: Case = Case.NOMINATIVE,
        initial_state: BehavioralState = BehavioralState.IDLE,
        free_energy_params: Optional[Dict[str, Any]] = None
    ):
        """
        Initialize an insect active inference model.
        
        Args:
            species: The insect species being modeled
            neural_config: Configuration for neural structures
            initial_case: Initial CEREBRUM case assignment
            initial_state: Initial behavioral state
            free_energy_params: Parameters for free energy computation
        """
        super().__init__(species, neural_config, initial_case, initial_state)
        
        # Active inference specific parameters
        self.free_energy_params = free_energy_params or {
            'precision_weight': 1.0,
            'complexity_weight': 1.0,
            'accuracy_weight': 1.0
        }
        
        # Belief state
        self.beliefs = {
            'environmental_state': np.zeros(10),  # Placeholder
            'internal_state': np.zeros(5),        # Placeholder
            'action_policy': np.ones(5) / 5       # Placeholder
        }
        
        # Expected free energy for action selection
        self.expected_free_energy = np.zeros(5)  # Placeholder
    
    def compute_free_energy(self, observations: np.ndarray) -> float:
        """
        Compute variational free energy for the insect model.
        
        Args:
            observations: Current observations
            
        Returns:
            Computed free energy value
        """
        try:
            # This is a simplified implementation
            # In practice, this would involve complex belief updating
            
            # Compute prediction error
            predicted_observations = self._predict_observations()
            prediction_error = np.mean((observations - predicted_observations) ** 2)
            
            # Compute complexity (KL divergence from prior)
            complexity = np.mean(self.beliefs['internal_state'] ** 2)
            
            # Compute free energy
            free_energy = (
                self.free_energy_params['accuracy_weight'] * prediction_error +
                self.free_energy_params['complexity_weight'] * complexity
            )
            
            return free_energy
            
        except Exception as e:
            logger.error(f"Error computing free energy: {e}")
            return float('inf')
    
    def _predict_observations(self) -> np.ndarray:
        """
        Predict observations based on current beliefs.
        
        Returns:
            Predicted observations
        """
        # Simplified prediction - in practice this would be more complex
        return np.zeros(10)  # Placeholder
    
    def update_beliefs(self, observations: np.ndarray):
        """
        Update beliefs through active inference.
        
        Args:
            observations: Current observations
        """
        try:
            # Update environmental state beliefs
            self.beliefs['environmental_state'] = (
                0.9 * self.beliefs['environmental_state'] + 
                0.1 * observations[:10]  # Assuming first 10 dimensions are environmental
            )
            
            # Update internal state beliefs
            self.beliefs['internal_state'] = (
                0.8 * self.beliefs['internal_state'] + 
                0.2 * observations[10:15]  # Assuming next 5 dimensions are internal
            )
            
            logger.debug("Updated beliefs through active inference")
            
        except Exception as e:
            logger.error(f"Error updating beliefs: {e}")
    
    def select_actions(self) -> List[Action]:
        """
        Select actions to minimize expected free energy.
        
        Returns:
            List of selected actions
        """
        try:
            # Compute expected free energy for different actions
            self.expected_free_energy = self._compute_expected_free_energy()
            
            # Select action with minimum expected free energy
            best_action_idx = np.argmin(self.expected_free_energy)
            
            # Create action based on selection
            action_types = ["explore", "observe", "sense", "produce", "navigate"]
            action = Action(
                action_type=action_types[best_action_idx],
                parameters={"confidence": 1.0 - self.expected_free_energy[best_action_idx]},
                confidence=1.0 - self.expected_free_energy[best_action_idx],
                timestamp=0.0  # Will be set by caller
            )
            
            return [action]
            
        except Exception as e:
            logger.error(f"Error selecting actions: {e}")
            return [Action(action_type="idle", parameters={}, confidence=0.0, timestamp=0.0)]
    
    def _compute_expected_free_energy(self) -> np.ndarray:
        """
        Compute expected free energy for different actions.
        
        Returns:
            Array of expected free energy values for each action
        """
        # Simplified implementation - in practice this would be more complex
        # This would involve forward models and belief propagation
        
        # Placeholder: random expected free energy values
        return np.random.rand(5) * 0.5 