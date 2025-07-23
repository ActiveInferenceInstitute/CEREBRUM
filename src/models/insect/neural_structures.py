"""
Neural Structures Implementation

This module provides implementations of insect neural structures mapped to CEREBRUM cases,
including mushroom bodies, central complex, antennal lobes, and other brain regions.
"""

from typing import Dict, Any, Optional, List, Tuple
import numpy as np
import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass, field

from src.core.model import Case

logger = logging.getLogger(__name__)


@dataclass
class NeuralConnection:
    """Represents a connection between neural structures."""
    source: str
    target: str
    weight: float
    connection_type: str  # excitatory, inhibitory, modulatory
    plasticity: bool = True


class NeuralStructure(ABC):
    """
    Abstract base class for neural structures.
    
    All neural structures in the insect brain inherit from this class
    and implement case-specific processing dynamics.
    """
    
    def __init__(
        self,
        structure_type: str,
        case_assignment: Case,
        input_dim: int = 100,
        output_dim: int = 50,
        config: Optional[Dict[str, Any]] = None
    ):
        """
        Initialize a neural structure.
        
        Args:
            structure_type: Type of neural structure
            case_assignment: Primary CEREBRUM case assignment
            input_dim: Input dimensionality
            output_dim: Output dimensionality
            config: Configuration parameters
        """
        self.structure_type = structure_type
        self.case_assignment = case_assignment
        self.input_dim = input_dim
        self.output_dim = output_dim
        self.config = config or {}
        
        # Initialize weights and biases
        self.weights = np.random.randn(input_dim, output_dim) * 0.1
        self.biases = np.zeros(output_dim)
        
        # Activity state
        self.current_activity = np.zeros(output_dim)
        self.previous_activity = np.zeros(output_dim)
        
        # Learning parameters
        self.learning_rate = self.config.get('learning_rate', 0.01)
        self.plasticity_enabled = self.config.get('plasticity_enabled', True)
        
        # Connections to other structures
        self.input_connections: List[NeuralConnection] = []
        self.output_connections: List[NeuralConnection] = []
        
        # Case-specific parameters
        self.case_parameters = self._initialize_case_parameters()
        
        logger.debug(f"Initialized {structure_type} with case {case_assignment}")
    
    def _initialize_case_parameters(self) -> Dict[str, Any]:
        """Initialize case-specific parameters."""
        case_params = {
            Case.NOMINATIVE: {
                'activation_threshold': 0.3,
                'output_gain': 1.0,
                'noise_level': 0.05
            },
            Case.ACCUSATIVE: {
                'activation_threshold': 0.2,
                'output_gain': 0.8,
                'noise_level': 0.1,
                'learning_rate_multiplier': 1.5
            },
            Case.DATIVE: {
                'activation_threshold': 0.1,
                'output_gain': 0.6,
                'noise_level': 0.15,
                'sensory_gain': 1.2
            },
            Case.GENITIVE: {
                'activation_threshold': 0.4,
                'output_gain': 1.2,
                'noise_level': 0.03,
                'output_stability': 0.9
            },
            Case.INSTRUMENTAL: {
                'activation_threshold': 0.25,
                'output_gain': 0.9,
                'noise_level': 0.08,
                'method_precision': 1.1
            },
            Case.LOCATIVE: {
                'activation_threshold': 0.35,
                'output_gain': 1.1,
                'noise_level': 0.06,
                'spatial_precision': 1.3
            },
            Case.ABLATIVE: {
                'activation_threshold': 0.15,
                'output_gain': 0.7,
                'noise_level': 0.12,
                'memory_stability': 0.95
            },
            Case.VOCATIVE: {
                'activation_threshold': 0.2,
                'output_gain': 1.0,
                'noise_level': 0.07,
                'identification_precision': 1.4
            }
        }
        
        return case_params.get(self.case_assignment, case_params[Case.NOMINATIVE])
    
    @abstractmethod
    def process_input(self, input_data: np.ndarray) -> np.ndarray:
        """
        Process input data according to case-specific dynamics.
        
        Args:
            input_data: Input data array
            
        Returns:
            Processed output array
        """
        pass
    
    def update_weights(self, learning_signal: np.ndarray):
        """
        Update synaptic weights based on learning signals.
        
        Args:
            learning_signal: Learning signal array
        """
        if not self.plasticity_enabled:
            return
        
        # Apply case-specific learning rate
        lr_multiplier = self.case_parameters.get('learning_rate_multiplier', 1.0)
        effective_lr = self.learning_rate * lr_multiplier
        
        # Simple Hebbian learning rule
        weight_update = effective_lr * np.outer(self.current_activity, learning_signal)
        self.weights += weight_update
        
        # Weight normalization to prevent runaway growth
        self.weights = np.clip(self.weights, -1.0, 1.0)
    
    def add_connection(self, connection: NeuralConnection, is_input: bool = True):
        """
        Add a connection to this neural structure.
        
        Args:
            connection: Neural connection to add
            is_input: Whether this is an input connection
        """
        if is_input:
            self.input_connections.append(connection)
        else:
            self.output_connections.append(connection)
    
    def get_activity(self) -> np.ndarray:
        """Get current activity state."""
        return self.current_activity.copy()
    
    def reset_activity(self):
        """Reset activity state."""
        self.previous_activity = self.current_activity.copy()
        self.current_activity = np.zeros(self.output_dim)


class MushroomBody(NeuralStructure):
    """
    Mushroom body implementation for [ACC] case.
    
    The mushroom body is the primary learning and memory center in insects,
    implementing the accusative case for associative learning.
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """
        Initialize mushroom body.
        
        Args:
            config: Configuration parameters
        """
        super().__init__(
            structure_type="mushroom_body",
            case_assignment=Case.ACCUSATIVE,
            input_dim=200,  # High input dimensionality for sensory integration
            output_dim=50,  # Match test expectation
            config=config
        )
        
        # Mushroom body specific components
        self.kenyon_cells = KenyonCellLayer(input_dim=self.input_dim, output_dim=self.output_dim)
        self.output_lobes = OutputLobeLayer(input_dim=self.output_dim, output_dim=50)
        
        # Learning state
        self.associations = {}  # Store learned associations
        self.memory_traces = np.zeros(self.output_dim)
        
        # Modulatory inputs (dopamine, octopamine)
        self.modulatory_inputs = {
            'dopamine': 0.0,
            'octopamine': 0.0,
            'serotonin': 0.0
        }
    
    def process_input(self, input_data: np.ndarray) -> np.ndarray:
        """
        Process input through Kenyon cells to output lobes.
        
        Args:
            input_data: Sensory input data
            
        Returns:
            Processed output from mushroom body
        """
        # Apply case-specific parameters
        threshold = self.case_parameters['activation_threshold']
        gain = self.case_parameters['output_gain']
        noise = self.case_parameters['noise_level']
        
        # Process through Kenyon cells (sparse coding)
        kc_output = self.kenyon_cells.process(input_data, threshold)
        
        # Apply modulatory influences
        modulated_output = self._apply_modulation(kc_output)
        
        # Update current activity
        self.current_activity = modulated_output
        
        # Process through output lobes
        final_output = self.output_lobes.process(modulated_output)
        
        # Add noise based on case parameters
        if noise > 0:
            final_output += np.random.normal(0, noise, final_output.shape)
        
        # Apply output gain
        final_output *= gain
        
        return final_output
    
    def _apply_modulation(self, activity: np.ndarray) -> np.ndarray:
        """
        Apply neuromodulatory influences to activity.
        
        Args:
            activity: Current activity
            
        Returns:
            Modulated activity
        """
        # Dopamine modulation (reward learning)
        if self.modulatory_inputs['dopamine'] > 0:
            activity *= (1.0 + self.modulatory_inputs['dopamine'])
        
        # Octopamine modulation (arousal/motivation)
        if self.modulatory_inputs['octopamine'] > 0:
            activity *= (1.0 + 0.5 * self.modulatory_inputs['octopamine'])
        
        # Serotonin modulation (behavioral state)
        if self.modulatory_inputs['serotonin'] > 0:
            activity *= (1.0 - 0.3 * self.modulatory_inputs['serotonin'])
        
        return np.clip(activity, 0, 1)
    
    def learn_association(self, stimulus: np.ndarray, outcome: np.ndarray):
        """
        Learn an association between stimulus and outcome.
        
        Args:
            stimulus: Stimulus pattern
            outcome: Outcome pattern
        """
        # Create association key
        stimulus_key = tuple(stimulus.flatten()[:10])  # Use first 10 elements as key
        
        # Store association
        self.associations[stimulus_key] = outcome.copy()
        
        # Update memory traces
        if len(outcome) == len(self.memory_traces):
            self.memory_traces += 0.1 * outcome
        else:
            # Pad or truncate outcome to match memory_traces dimension
            if len(outcome) > len(self.memory_traces):
                outcome = outcome[:len(self.memory_traces)]
            else:
                outcome = np.pad(outcome, (0, len(self.memory_traces) - len(outcome)))
            self.memory_traces += 0.1 * outcome
        
        # Normalize memory traces
        self.memory_traces = np.clip(self.memory_traces, 0, 1)
        
        logger.debug(f"Learned association in mushroom body")
    
    def recall_association(self, stimulus: np.ndarray) -> Optional[np.ndarray]:
        """
        Recall an association for a given stimulus.
        
        Args:
            stimulus: Stimulus pattern
            
        Returns:
            Recalled outcome pattern or None if not found
        """
        stimulus_key = tuple(stimulus.flatten()[:10])
        return self.associations.get(stimulus_key)


class KenyonCellLayer:
    """Layer of Kenyon cells implementing sparse coding."""
    
    def __init__(self, input_dim: int, output_dim: int):
        self.input_dim = input_dim
        self.output_dim = output_dim
        self.weights = np.random.randn(input_dim, output_dim) * 0.1
        self.sparsity = 0.1  # Only 10% of cells active at once
    
    def process(self, input_data: np.ndarray, threshold: float) -> np.ndarray:
        """Process input through sparse coding."""
        # Linear transformation
        activity = np.dot(input_data, self.weights)
        
        # Apply threshold
        activity = np.maximum(activity - threshold, 0)
        
        # Enforce sparsity (winner-take-all)
        if self.sparsity < 1.0:
            k = int(self.sparsity * self.output_dim)
            indices = np.argsort(activity)[-k:]
            sparse_activity = np.zeros_like(activity)
            sparse_activity[indices] = activity[indices]
            activity = sparse_activity
        
        return activity


class OutputLobeLayer:
    """Output lobe layer of mushroom body."""
    
    def __init__(self, input_dim: int, output_dim: int):
        self.input_dim = input_dim
        self.output_dim = output_dim
        self.weights = np.random.randn(input_dim, output_dim) * 0.1
    
    def process(self, input_data: np.ndarray) -> np.ndarray:
        """Process input through output lobes."""
        return np.dot(input_data, self.weights)


class CentralComplex(NeuralStructure):
    """
    Central complex implementation for [NOM]/[LOC] cases.
    
    The central complex is responsible for navigation, motor control,
    and spatial representation in insects.
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """
        Initialize central complex.
        
        Args:
            config: Configuration parameters
        """
        super().__init__(
            structure_type="central_complex",
            case_assignment=Case.NOMINATIVE,  # Primary case, can switch to LOCATIVE
            input_dim=150,
            output_dim=80,
            config=config
        )
        
        # Central complex components
        self.fan_shaped_body = FanShapedBody(input_dim=50, output_dim=30)
        self.ellipsoid_body = EllipsoidBody(input_dim=50, output_dim=30)
        self.protocerebral_bridge = ProtocerebralBridge(input_dim=50, output_dim=20)
        
        # Navigation state
        self.heading_direction = 0.0
        self.position = np.array([0.0, 0.0])
        self.path_integration_state = np.zeros(4)
        
        # Motor control
        self.motor_commands = np.zeros(6)  # 6 degrees of freedom
    
    def process_input(self, input_data: np.ndarray) -> np.ndarray:
        """
        Process input for navigation and motor control.
        
        Args:
            input_data: Sensory input data
            
        Returns:
            Motor commands and navigation output
        """
        # Split input for different components
        visual_input = input_data[:50]
        proprioceptive_input = input_data[50:100]
        compass_input = input_data[100:150]
        
        # Process through fan-shaped body (motor control)
        motor_output = self.fan_shaped_body.process(visual_input, proprioceptive_input)
        
        # Ensure motor_output has correct dimension
        if len(motor_output) != 30:
            if len(motor_output) > 30:
                motor_output = motor_output[:30]
            else:
                motor_output = np.pad(motor_output, (0, 30 - len(motor_output)))
        
        # Process through ellipsoid body (spatial representation)
        spatial_output = self.ellipsoid_body.process(compass_input, self.heading_direction)
        
        # Process through protocerebral bridge (path integration)
        path_output = self.protocerebral_bridge.process(spatial_output, self.path_integration_state)
        
        # Combine outputs
        combined_output = np.concatenate([motor_output, spatial_output, path_output])
        
        # Update current activity
        self.current_activity = combined_output[:self.output_dim]
        
        # Update navigation state
        self._update_navigation_state(spatial_output, path_output)
        
        # Generate motor commands
        self.motor_commands = self._generate_motor_commands(motor_output)
        
        return self.current_activity
    
    def _update_navigation_state(self, spatial_output: np.ndarray, path_output: np.ndarray):
        """Update navigation state based on outputs."""
        # Update heading direction
        heading_change = np.sum(spatial_output[:10]) * 0.1
        self.heading_direction = (self.heading_direction + heading_change) % (2 * np.pi)
        
        # Update position through path integration
        velocity = path_output[:2]
        self.position += velocity * 0.1  # Time step of 0.1
        
        # Update path integration state
        self.path_integration_state = path_output
    
    def _generate_motor_commands(self, motor_output: np.ndarray) -> np.ndarray:
        """Generate motor commands from motor output."""
        # Map motor output to 6 degrees of freedom
        commands = np.zeros(6)
        
        # Forward/backward movement
        commands[0] = motor_output[0]
        
        # Left/right turning
        commands[1] = motor_output[1]
        
        # Up/down movement
        commands[2] = motor_output[2]
        
        # Wing control (for flying insects)
        commands[3:6] = motor_output[3:6]
        
        return commands
    
    def get_navigation_state(self) -> Dict[str, Any]:
        """Get current navigation state."""
        return {
            'heading': self.heading_direction,
            'position': self.position.copy(),
            'path_integration': self.path_integration_state.copy(),
            'motor_commands': self.motor_commands.copy()
        }


class FanShapedBody:
    """Fan-shaped body for motor control."""
    
    def __init__(self, input_dim: int, output_dim: int):
        self.input_dim = input_dim
        self.output_dim = output_dim
        self.weights = np.random.randn(input_dim, output_dim) * 0.1
    
    def process(self, visual_input: np.ndarray, proprioceptive_input: np.ndarray) -> np.ndarray:
        """Process visual and proprioceptive inputs for motor control."""
        combined_input = np.concatenate([visual_input, proprioceptive_input])
        return np.dot(combined_input, self.weights)


class EllipsoidBody:
    """Ellipsoid body for spatial representation."""
    
    def __init__(self, input_dim: int, output_dim: int):
        self.input_dim = input_dim
        self.output_dim = output_dim
        self.weights = np.random.randn(input_dim, output_dim) * 0.1
        self.ring_attractor = RingAttractor(output_dim)
    
    def process(self, compass_input: np.ndarray, current_heading: float) -> np.ndarray:
        """Process compass input for spatial representation."""
        # Process through weights
        processed = np.dot(compass_input, self.weights)
        
        # Apply ring attractor dynamics
        output = self.ring_attractor.update(processed, current_heading)
        
        return output


class RingAttractor:
    """Ring attractor for heading direction representation."""
    
    def __init__(self, size: int):
        self.size = size
        self.activity = np.zeros(size)
        self.weights = self._create_ring_weights()
    
    def _create_ring_weights(self) -> np.ndarray:
        """Create ring attractor weights."""
        weights = np.zeros((self.size, self.size))
        for i in range(self.size):
            for j in range(self.size):
                distance = min(abs(i - j), self.size - abs(i - j))
                weights[i, j] = np.exp(-distance / 5.0)
        return weights
    
    def update(self, input_activity: np.ndarray, heading: float) -> np.ndarray:
        """Update ring attractor activity."""
        # Add input
        self.activity += input_activity
        
        # Apply ring attractor dynamics
        self.activity = np.dot(self.weights, self.activity)
        
        # Normalize
        self.activity = np.clip(self.activity, 0, 1)
        
        return self.activity


class ProtocerebralBridge:
    """Protocerebral bridge for path integration."""
    
    def __init__(self, input_dim: int, output_dim: int):
        self.input_dim = input_dim
        self.output_dim = output_dim
        self.weights = np.random.randn(input_dim, output_dim) * 0.1
    
    def process(self, spatial_input: np.ndarray, path_state: np.ndarray) -> np.ndarray:
        """Process spatial input for path integration."""
        combined_input = np.concatenate([spatial_input, path_state])
        return np.dot(combined_input, self.weights)


class AntennalLobe(NeuralStructure):
    """
    Antennal lobe implementation for [DAT]/[PHE] cases.
    
    The antennal lobe processes olfactory and pheromonal information,
    implementing both dative (sensory reception) and pheromonal cases.
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """
        Initialize antennal lobe.
        
        Args:
            config: Configuration parameters
        """
        super().__init__(
            structure_type="antennal_lobe",
            case_assignment=Case.DATIVE,  # Primary case, can switch to PHE
            input_dim=300,  # High input dimensionality for odor processing
            output_dim=150,
            config=config
        )
        
        # Antennal lobe components
        self.glomeruli = GlomeruliLayer(input_dim=self.input_dim, output_dim=100)
        self.projection_neurons = ProjectionNeuronLayer(input_dim=100, output_dim=self.output_dim)
        self.local_neurons = LocalNeuronLayer(input_dim=100, output_dim=100)
        
        # Odor processing state
        self.odor_receptors = np.random.randn(50, self.input_dim)  # 50 odor receptors
        self.odor_memory = {}  # Store learned odor associations
        
        # Pheromone processing
        self.pheromone_receptors = np.random.randn(20, self.input_dim)  # 20 pheromone receptors
        self.pheromone_concentrations = np.zeros(20)
    
    def process_input(self, input_data: np.ndarray) -> np.ndarray:
        """
        Process olfactory and pheromonal input.
        
        Args:
            input_data: Sensory input data
            
        Returns:
            Processed olfactory output
        """
        # Apply case-specific parameters
        threshold = self.case_parameters['activation_threshold']
        gain = self.case_parameters['output_gain']
        noise = self.case_parameters['noise_level']
        sensory_gain = self.case_parameters.get('sensory_gain', 1.0)
        
        # Apply sensory gain
        input_data *= sensory_gain
        
        # Process through glomeruli
        glomerular_output = self.glomeruli.process(input_data, self.odor_receptors)
        
        # Apply lateral inhibition through local neurons
        inhibited_output = self.local_neurons.process(glomerular_output)
        
        # Process through projection neurons
        final_output = self.projection_neurons.process(inhibited_output)
        
        # Update current activity
        self.current_activity = final_output
        
        # Process pheromones if present
        if self.case_assignment == Case.VOCATIVE:  # Pheromonal case
            pheromone_output = self._process_pheromones(input_data)
            # Ensure pheromone_output has correct dimension
            if len(pheromone_output) != 50:
                if len(pheromone_output) > 50:
                    pheromone_output = pheromone_output[:50]
                else:
                    pheromone_output = np.pad(pheromone_output, (0, 50 - len(pheromone_output)))
            final_output = np.concatenate([final_output, pheromone_output])
        
        # Add noise based on case parameters
        if noise > 0:
            final_output += np.random.normal(0, noise, final_output.shape)
        
        # Apply output gain
        final_output *= gain
        
        return final_output
    
    def _process_pheromones(self, input_data: np.ndarray) -> np.ndarray:
        """Process pheromonal signals."""
        # Calculate pheromone concentrations
        pheromone_responses = np.dot(self.pheromone_receptors, input_data)
        self.pheromone_concentrations = np.maximum(pheromone_responses, 0)
        
        return self.pheromone_concentrations
    
    def learn_odor(self, odor_pattern: np.ndarray, odor_label: str):
        """
        Learn to recognize an odor pattern.
        
        Args:
            odor_pattern: Odor pattern
            odor_label: Label for the odor
        """
        odor_key = tuple(odor_pattern.flatten()[:20])  # Use first 20 elements as key
        self.odor_memory[odor_key] = odor_label
        
        logger.debug(f"Learned odor: {odor_label}")
    
    def recognize_odor(self, odor_pattern: np.ndarray) -> Optional[str]:
        """
        Recognize an odor pattern.
        
        Args:
            odor_pattern: Odor pattern to recognize
            
        Returns:
            Recognized odor label or None
        """
        odor_key = tuple(odor_pattern.flatten()[:20])
        return self.odor_memory.get(odor_key)


class GlomeruliLayer:
    """Layer of glomeruli for odor processing."""
    
    def __init__(self, input_dim: int, output_dim: int):
        self.input_dim = input_dim
        self.output_dim = output_dim
        self.weights = np.random.randn(input_dim, output_dim) * 0.1
    
    def process(self, input_data: np.ndarray, receptors: np.ndarray) -> np.ndarray:
        """Process input through glomeruli."""
        # Apply receptor responses
        receptor_responses = np.dot(receptors, input_data)
        
        # Process through glomerular weights
        glomerular_output = np.dot(input_data, self.weights)
        
        # Combine with receptor responses
        combined = glomerular_output + receptor_responses[:self.output_dim]
        
        return np.maximum(combined, 0)


class ProjectionNeuronLayer:
    """Layer of projection neurons."""
    
    def __init__(self, input_dim: int, output_dim: int):
        self.input_dim = input_dim
        self.output_dim = output_dim
        self.weights = np.random.randn(input_dim, output_dim) * 0.1
    
    def process(self, input_data: np.ndarray) -> np.ndarray:
        """Process input through projection neurons."""
        return np.dot(input_data, self.weights)


class LocalNeuronLayer:
    """Layer of local neurons for lateral inhibition."""
    
    def __init__(self, input_dim: int, output_dim: int):
        self.input_dim = input_dim
        self.output_dim = output_dim
        self.weights = np.random.randn(input_dim, output_dim) * 0.1
        
        # Lateral inhibition weights (negative connections)
        self.lateral_weights = -0.1 * np.random.rand(input_dim, input_dim)
        np.fill_diagonal(self.lateral_weights, 0)  # No self-inhibition
    
    def process(self, input_data: np.ndarray) -> np.ndarray:
        """Process input with lateral inhibition."""
        # Apply lateral inhibition
        inhibition = np.dot(self.lateral_weights, input_data)
        inhibited = input_data + inhibition
        
        # Process through weights
        output = np.dot(inhibited, self.weights)
        
        return np.maximum(output, 0)


class OpticLobe(NeuralStructure):
    """
    Optic lobe implementation for [DAT] case.
    
    The optic lobe processes visual information from compound eyes.
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """
        Initialize optic lobe.
        
        Args:
            config: Configuration parameters
        """
        super().__init__(
            structure_type="optic_lobe",
            case_assignment=Case.DATIVE,
            input_dim=400,  # High input dimensionality for visual processing
            output_dim=200,
            config=config
        )
        
        # Optic lobe components
        self.lamina = LaminaLayer(input_dim=self.input_dim, output_dim=150)
        self.medulla = MedullaLayer(input_dim=150, output_dim=100)
        self.lobula = LobulaLayer(input_dim=100, output_dim=self.output_dim)
        
        # Visual processing state
        self.motion_detectors = np.random.randn(50, 100)  # 50 motion detectors
        self.color_channels = np.random.randn(3, 50)  # RGB color channels
    
    def process_input(self, input_data: np.ndarray) -> np.ndarray:
        """
        Process visual input through optic lobe.
        
        Args:
            input_data: Visual input data
            
        Returns:
            Processed visual output
        """
        # Apply case-specific parameters
        threshold = self.case_parameters['activation_threshold']
        gain = self.case_parameters['output_gain']
        noise = self.case_parameters['noise_level']
        sensory_gain = self.case_parameters.get('sensory_gain', 1.0)
        
        # Apply sensory gain
        input_data *= sensory_gain
        
        # Process through lamina (primary visual processing)
        lamina_output = self.lamina.process(input_data)
        
        # Process through medulla (feature extraction)
        medulla_output = self.medulla.process(lamina_output)
        
        # Process through lobula (motion detection and object recognition)
        lobula_output = self.lobula.process(medulla_output)
        
        # Update current activity
        self.current_activity = lobula_output
        
        # Add motion detection
        motion_output = self._detect_motion(lamina_output)
        
        # Add color processing
        color_output = self._process_color(input_data)
        
        # Combine outputs
        final_output = np.concatenate([lobula_output, motion_output, color_output])
        
        # Add noise based on case parameters
        if noise > 0:
            final_output += np.random.normal(0, noise, final_output.shape)
        
        # Apply output gain
        final_output *= gain
        
        return final_output
    
    def _detect_motion(self, lamina_output: np.ndarray) -> np.ndarray:
        """Detect motion in visual input."""
        motion_responses = np.dot(self.motion_detectors, lamina_output[:100])
        return np.maximum(motion_responses, 0)
    
    def _process_color(self, input_data: np.ndarray) -> np.ndarray:
        """Process color information."""
        color_responses = np.dot(self.color_channels, input_data[:50])
        return np.maximum(color_responses, 0)


class LaminaLayer:
    """Lamina layer for primary visual processing."""
    
    def __init__(self, input_dim: int, output_dim: int):
        self.input_dim = input_dim
        self.output_dim = output_dim
        self.weights = np.random.randn(input_dim, output_dim) * 0.1
    
    def process(self, input_data: np.ndarray) -> np.ndarray:
        """Process input through lamina."""
        return np.dot(input_data, self.weights)


class MedullaLayer:
    """Medulla layer for feature extraction."""
    
    def __init__(self, input_dim: int, output_dim: int):
        self.input_dim = input_dim
        self.output_dim = output_dim
        self.weights = np.random.randn(input_dim, output_dim) * 0.1
    
    def process(self, input_data: np.ndarray) -> np.ndarray:
        """Process input through medulla."""
        return np.dot(input_data, self.weights)


class LobulaLayer:
    """Lobula layer for motion detection and object recognition."""
    
    def __init__(self, input_dim: int, output_dim: int):
        self.input_dim = input_dim
        self.output_dim = output_dim
        self.weights = np.random.randn(input_dim, output_dim) * 0.1
    
    def process(self, input_data: np.ndarray) -> np.ndarray:
        """Process input through lobula."""
        return np.dot(input_data, self.weights)


class SubesophagealGanglion(NeuralStructure):
    """
    Subesophageal ganglion implementation for [GEN]/[INS] cases.
    
    The subesophageal ganglion controls feeding behavior and motor patterns.
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """
        Initialize subesophageal ganglion.
        
        Args:
            config: Configuration parameters
        """
        super().__init__(
            structure_type="subesophageal_ganglion",
            case_assignment=Case.GENITIVE,
            input_dim=100,
            output_dim=80,
            config=config
        )
        
        # Motor control components
        self.motor_patterns = {
            'feeding': np.random.randn(20),
            'grooming': np.random.randn(20),
            'walking': np.random.randn(20),
            'flying': np.random.randn(20)
        }
        
        # Feeding state
        self.hunger_level = 0.5
        self.feeding_behavior = False
    
    def process_input(self, input_data: np.ndarray) -> np.ndarray:
        """
        Process input for motor control and feeding behavior.
        
        Args:
            input_data: Sensory input data
            
        Returns:
            Motor control output
        """
        # Apply case-specific parameters
        threshold = self.case_parameters['activation_threshold']
        gain = self.case_parameters['output_gain']
        noise = self.case_parameters['noise_level']
        output_stability = self.case_parameters.get('output_stability', 0.9)
        
        # Update hunger level based on input
        self.hunger_level = np.clip(self.hunger_level + input_data[0] * 0.1, 0, 1)
        
        # Select motor pattern based on hunger and input
        if self.hunger_level > 0.7:
            motor_pattern = self.motor_patterns['feeding']
            self.feeding_behavior = True
        else:
            motor_pattern = self.motor_patterns['walking']
            self.feeding_behavior = False
        
        # Process input through weights
        processed = np.dot(input_data, self.weights)
        
        # Combine with motor pattern
        output = processed + motor_pattern[:self.output_dim]
        
        # Apply output stability
        output = output_stability * self.current_activity + (1 - output_stability) * output
        
        # Update current activity
        self.current_activity = output
        
        # Add noise based on case parameters
        if noise > 0:
            output += np.random.normal(0, noise, output.shape)
        
        # Apply output gain
        output *= gain
        
        return output
    
    def get_feeding_state(self) -> Dict[str, Any]:
        """Get current feeding state."""
        return {
            'hunger_level': self.hunger_level,
            'feeding_behavior': self.feeding_behavior,
            'motor_patterns': list(self.motor_patterns.keys())
        }


class VentralNerveCord(NeuralStructure):
    """
    Ventral nerve cord implementation for [INS] case.
    
    The ventral nerve cord coordinates motor output and signal transmission.
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """
        Initialize ventral nerve cord.
        
        Args:
            config: Configuration parameters
        """
        super().__init__(
            structure_type="ventral_nerve_cord",
            case_assignment=Case.INSTRUMENTAL,
            input_dim=120,
            output_dim=50,  # Match test expectation
            config=config
        )
        
        # Motor control components
        self.motor_neurons = np.random.randn(50, self.output_dim)
        self.sensory_neurons = np.random.randn(50, self.input_dim)
        
        # Signal transmission state
        self.transmission_efficiency = 0.95
        self.signal_delay = 0.01  # 10ms delay
    
    def process_input(self, input_data: np.ndarray) -> np.ndarray:
        """
        Process input for motor coordination and signal transmission.
        
        Args:
            input_data: Input data from brain
        
        Returns:
            Motor output and signal transmission
        """
        # Apply case-specific parameters
        threshold = self.case_parameters['activation_threshold']
        gain = self.case_parameters['output_gain']
        noise = self.case_parameters['noise_level']
        method_precision = self.case_parameters.get('method_precision', 1.0)
        
        # Process through sensory neurons
        sensory_processed = np.dot(self.sensory_neurons, input_data)
        
        # Process through main weights
        processed = np.dot(input_data, self.weights)
        
        # Apply motor neuron processing
        motor_output = np.dot(self.motor_neurons.T, processed)
        
        # Apply transmission efficiency
        motor_output *= self.transmission_efficiency
        
        # Apply method precision
        motor_output *= method_precision
        
        # Update current activity
        self.current_activity = motor_output
        
        # Add noise based on case parameters
        if noise > 0:
            motor_output += np.random.normal(0, noise, motor_output.shape)
        
        # Apply output gain
        motor_output *= gain
        
        return motor_output
    
    def get_transmission_state(self) -> Dict[str, Any]:
        """Get current transmission state."""
        return {
            'transmission_efficiency': self.transmission_efficiency,
            'signal_delay': self.signal_delay,
            'motor_neurons_active': np.sum(self.current_activity > 0.1),
            'sensory_neurons_active': np.sum(self.sensory_neurons.flatten() > 0.1)
        } 