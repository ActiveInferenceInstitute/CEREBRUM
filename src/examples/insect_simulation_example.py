"""
Comprehensive Insect Simulation Example

This example demonstrates the CEREBRUM insect models in action,
showing how different cases and neural structures work together
to create realistic insect behavior simulations.
"""

import numpy as np
import time
import logging
import os
import json
from datetime import datetime
from typing import Dict, Any, List
from dataclasses import dataclass, asdict

from src.models.insect.base import (
    InsectModel, 
    InsectActiveInferenceModel, 
    BehavioralState, 
    SensoryInput, 
    Action
)
from src.models.insect.neural_structures import (
    MushroomBody,
    CentralComplex,
    AntennalLobe,
    OpticLobe,
    SubesophagealGanglion,
    VentralNerveCord
)
from src.models.insect.cases.pheromonal import (
    PheromonalCase,
    PheromoneType,
    ChemicalSignal
)
from src.models.insect.cases.swarm import (
    SwarmCase,
    SwarmBehavior,
    SwarmMember
)
from src.models.insect.cases.metamorphic import (
    MetamorphicCase,
    DevelopmentalStage,
    DevelopmentalState
)
from src.models.insect.cases.caste import (
    CasteCase,
    CasteType
)
from src.models.insect.cases.substrate import (
    SubstrateCase,
    SubstrateType
)
from src.models.insect.cases.stigmergic import (
    StigmergicCase,
    StigmergicSignal
)
from src.core.model import Case

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


@dataclass
class SimulationEnvironment:
    """Environment for insect simulation."""
    temperature: float = 25.0
    humidity: float = 0.6
    food_sources: List[np.ndarray] = None
    threats: List[np.ndarray] = None
    substrates: Dict[str, Any] = None
    
    def __post_init__(self):
        if self.food_sources is None:
            self.food_sources = [
                np.array([5.0, 5.0, 0.0]),
                np.array([-3.0, 2.0, 0.0]),
                np.array([1.0, -4.0, 0.0])
            ]
        if self.threats is None:
            self.threats = [
                np.array([8.0, 8.0, 0.0])
            ]
        if self.substrates is None:
            self.substrates = {
                'soil': {'type': SubstrateType.SOIL, 'position': np.array([0.0, 0.0, 0.0])},
                'leaf': {'type': SubstrateType.LEAF, 'position': np.array([2.0, 2.0, 0.0])},
                'bark': {'type': SubstrateType.BARK, 'position': np.array([-2.0, -2.0, 0.0])}
            }


class InsectColony:
    """A colony of insects with different roles and behaviors."""
    
    def __init__(self, colony_id: str, environment: SimulationEnvironment):
        """
        Initialize insect colony.
        
        Args:
            colony_id: Unique identifier for the colony
            environment: Simulation environment
        """
        self.colony_id = colony_id
        self.environment = environment
        
        # Initialize cases
        self.pheromonal_case = PheromonalCase()
        self.swarm_case = SwarmCase()
        self.metamorphic_case = MetamorphicCase()
        self.caste_case = CasteCase()
        self.substrate_case = SubstrateCase()
        self.stigmergic_case = StigmergicCase()
        
        # Colony members
        self.members: Dict[str, InsectModel] = {}
        self.member_positions: Dict[str, np.ndarray] = {}
        self.member_states: Dict[str, Dict[str, Any]] = {}
        
        # Colony statistics
        self.statistics = {
            'total_food_collected': 0.0,
            'total_threats_avoided': 0,
            'total_pheromones_created': 0,
            'total_swarm_decisions': 0,
            'simulation_steps': 0
        }
        
        logger.info(f"Initialized insect colony {colony_id}")
    
    def add_member(self, member_id: str, species: str, initial_position: np.ndarray,
                   caste_type: CasteType = None) -> InsectModel:
        """
        Add a member to the colony.
        
        Args:
            member_id: Unique identifier for the member
            species: Insect species
            initial_position: Initial position in environment
            caste_type: Caste type (determined automatically if None)
            
        Returns:
            Created insect model
        """
        # Create insect model
        model = InsectModel(
            species=species,
            initial_case=Case.NOMINATIVE,
            initial_state=BehavioralState.IDLE
        )
        
        # Determine caste if not specified
        if caste_type is None:
            genetic_markers = {'caste_genes': np.random.random()}
            env_conditions = {
                'temperature': self.environment.temperature,
                'humidity': self.environment.humidity
            }
            nutrition_history = [np.random.random() for _ in range(5)]
            
            assignment = self.caste_case.determine_caste(
                member_id, genetic_markers, env_conditions, nutrition_history
            )
            caste_type = assignment.caste_type
        
        # Store member information
        self.members[member_id] = model
        self.member_positions[member_id] = initial_position.copy()
        self.member_states[member_id] = {
            'caste': caste_type,
            'energy': 1.0,
            'food_carried': 0.0,
            'age': 0.0,
            'developmental_stage': DevelopmentalStage.ADULT
        }
        
        # Add to swarm
        swarm_member = SwarmMember(
            id=member_id,
            position=initial_position,
            velocity=np.array([0.0, 0.0, 0.0]),
            state=self.member_states[member_id],
            role=caste_type.value,
            influence_radius=1.0
        )
        self.swarm_case.add_member(swarm_member)
        
        logger.info(f"Added {species} member {member_id} as {caste_type.value}")
        
        return model
    
    def simulate_step(self, step_duration: float = 1.0):
        """
        Simulate one step of the colony.
        
        Args:
            step_duration: Duration of simulation step
        """
        self.statistics['simulation_steps'] += 1
        
        # Update all members
        for member_id, model in self.members.items():
            self._update_member(member_id, model, step_duration)
        
        # Update swarm dynamics
        self.swarm_case.compute_swarm_dynamics()
        
        # Clean up expired signals
        current_time = time.time()
        self.pheromonal_case.cleanup_expired_pheromones(current_time)
        self.stigmergic_case.cleanup_expired_signals(current_time)
        
        # Log statistics periodically
        if self.statistics['simulation_steps'] % 10 == 0:
            self._log_statistics()
    
    def _update_member(self, member_id: str, model: InsectModel, step_duration: float):
        """
        Update a single colony member.
        
        Args:
            member_id: Member ID
            model: Insect model
            step_duration: Duration of simulation step
        """
        position = self.member_positions[member_id]
        state = self.member_states[member_id]
        
        # Create sensory input based on environment
        sensory_input = self._create_sensory_input(member_id, position)
        
        # Process sensory input
        processed_sensory = model.process_sensory_input(sensory_input)
        
        # Handle pheromone detection
        self._handle_pheromone_interaction(member_id, position, processed_sensory)
        
        # Handle substrate interaction
        self._handle_substrate_interaction(member_id, position)
        
        # Select action based on current case and state
        context = {
            'timestamp': time.time(),
            'energy': state['energy'],
            'food_carried': state['food_carried'],
            'caste': state['caste'].value
        }
        
        action = model.select_action(context)
        
        # Execute action
        self._execute_action(member_id, action, step_duration)
        
        # Update member state
        state['age'] += step_duration
        state['energy'] = max(0.0, state['energy'] - 0.01)  # Energy consumption
        
        # Update position in swarm
        swarm_member = self.swarm_case.members.get(member_id)
        if swarm_member:
            swarm_member.position = position.copy()
            swarm_member.state = state.copy()
    
    def _create_sensory_input(self, member_id: str, position: np.ndarray) -> SensoryInput:
        """
        Create sensory input for a member based on environment.
        
        Args:
            member_id: Member ID
            position: Current position
            
        Returns:
            Sensory input
        """
        # Visual input (distance to objects)
        visual_input = np.zeros(100)
        
        # Distance to food sources
        for i, food_pos in enumerate(self.environment.food_sources):
            distance = np.linalg.norm(position - food_pos)
            if distance < 10.0:  # Visual range
                visual_input[i] = 1.0 / (1.0 + distance)
        
        # Distance to threats
        for i, threat_pos in enumerate(self.environment.threats):
            distance = np.linalg.norm(position - threat_pos)
            if distance < 8.0:  # Threat detection range
                visual_input[50 + i] = 1.0 / (1.0 + distance)
        
        # Olfactory input (pheromone detection)
        olfactory_input = np.zeros(50)
        
        # Detect pheromones
        detected_pheromones = self.pheromonal_case.detect_signals(position, 3.0)
        for i, pheromone in enumerate(detected_pheromones):
            if i < 50:
                olfactory_input[i] = pheromone.concentration
        
        # Pheromonal input
        pheromonal_input = np.zeros(20)
        for i, pheromone in enumerate(detected_pheromones):
            if i < 20:
                pheromonal_input[i] = pheromone.concentration
        
        return SensoryInput(
            visual=visual_input,
            olfactory=olfactory_input,
            pheromonal=pheromonal_input,
            timestamp=time.time()
        )
    
    def _handle_pheromone_interaction(self, member_id: str, position: np.ndarray, 
                                    processed_sensory: Dict[str, Any]):
        """
        Handle pheromone interactions for a member.
        
        Args:
            member_id: Member ID
            position: Current position
            processed_sensory: Processed sensory input
        """
        state = self.member_states[member_id]
        
        # Check if member should create pheromones
        if state['caste'] == CasteType.FORAGER and state['food_carried'] > 0:
            # Create trail pheromone
            self.pheromonal_case.generate_pheromone(
                "trail", 0.8, position
            )
            self.statistics['total_pheromones_created'] += 1
        
        # Check if member should respond to pheromones
        if 'pheromonal' in processed_sensory:
            pheromone_strength = np.sum(processed_sensory['pheromonal'])
            if pheromone_strength > 0.5:
                # Create response pheromone
                self.pheromonal_case.generate_pheromone(
                    "recruitment", 0.6, position
                )
    
    def _handle_substrate_interaction(self, member_id: str, position: np.ndarray):
        """
        Handle substrate interactions for a member.
        
        Args:
            member_id: Member ID
            position: Current position
        """
        state = self.member_states[member_id]
        
        # Find nearest substrate
        nearest_substrate = None
        min_distance = float('inf')
        
        for substrate_name, substrate_info in self.environment.substrates.items():
            distance = np.linalg.norm(position - substrate_info['position'])
            if distance < min_distance:
                min_distance = distance
                nearest_substrate = substrate_info
        
        if nearest_substrate and min_distance < 1.0:
            # Create substrate
            substrate = self.substrate_case.create_substrate(nearest_substrate['type'])
            
            # Interact with substrate
            interaction_type = "walking"
            if state['caste'] == CasteType.BUILDER:
                interaction_type = "construction"
            elif state['caste'] == CasteType.FORAGER:
                interaction_type = "foraging"
            
            self.substrate_case.interact_with_substrate(
                member_id, substrate, interaction_type, 1.0
            )
    
    def _execute_action(self, member_id: str, action: Action, step_duration: float):
        """
        Execute an action for a member.
        
        Args:
            member_id: Member ID
            action: Action to execute
            step_duration: Duration of simulation step
        """
        position = self.member_positions[member_id]
        state = self.member_states[member_id]
        
        # Execute action based on type
        if action.action_type == "explore":
            # Random movement
            direction = np.random.randn(3)
            direction[2] = 0  # Keep z=0
            direction = direction / np.linalg.norm(direction)
            new_position = position + direction * 0.5 * step_duration
            self.member_positions[member_id] = new_position
            
        elif action.action_type == "approach":
            # Approach nearest food source
            if self.environment.food_sources:
                nearest_food = min(self.environment.food_sources, 
                                 key=lambda x: np.linalg.norm(position - x))
                direction = nearest_food - position
                direction = direction / np.linalg.norm(direction)
                new_position = position + direction * 0.3 * step_duration
                self.member_positions[member_id] = new_position
                
                # Check if reached food
                if np.linalg.norm(new_position - nearest_food) < 0.5:
                    state['food_carried'] = min(1.0, state['food_carried'] + 0.2)
                    self.statistics['total_food_collected'] += 0.2
        
        elif action.action_type == "escape":
            # Move away from threats
            if self.environment.threats:
                nearest_threat = min(self.environment.threats,
                                   key=lambda x: np.linalg.norm(position - x))
                direction = position - nearest_threat
                direction = direction / np.linalg.norm(direction)
                new_position = position + direction * 0.8 * step_duration
                self.member_positions[member_id] = new_position
                
                # Check if escaped
                if np.linalg.norm(new_position - nearest_threat) > 5.0:
                    self.statistics['total_threats_avoided'] += 1
        
        elif action.action_type == "communicate":
            # Create stigmergic signal
            signal_type = StigmergicSignal.TRAIL_MARKER
            if state['caste'] == CasteType.FORAGER:
                signal_type = StigmergicSignal.FOOD_MARKER
            
            self.stigmergic_case.create_signal(
                signal_type, position, member_id, 0.7
            )
    
    def _log_statistics(self):
        """Log colony statistics."""
        logger.info(f"Colony {self.colony_id} Statistics:")
        logger.info(f"  Members: {len(self.members)}")
        logger.info(f"  Food collected: {self.statistics['total_food_collected']:.2f}")
        logger.info(f"  Threats avoided: {self.statistics['total_threats_avoided']}")
        logger.info(f"  Pheromones created: {self.statistics['total_pheromones_created']}")
        logger.info(f"  Simulation steps: {self.statistics['simulation_steps']}")
    
    def save_simulation_data(self, output_dir: str):
        """Save simulation data to files."""
        # Create output directory if it doesn't exist
        os.makedirs(output_dir, exist_ok=True)
        
        # Save colony statistics
        stats_file = os.path.join(output_dir, "colony_statistics.json")
        with open(stats_file, 'w') as f:
            json.dump(self.statistics, f, indent=2, default=str)
        
        # Save member positions and states
        members_data = {}
        for member_id, model in self.members.items():
            # Convert enum values to strings for JSON serialization
            member_state = self.member_states[member_id].copy()
            member_state['caste'] = member_state['caste'].value
            member_state['developmental_stage'] = member_state['developmental_stage'].value
            
            members_data[member_id] = {
                'position': self.member_positions[member_id].tolist(),
                'behavioral_state': model.behavioral_state.value,
                'current_case': model.case.value,
                'species': model.species,
                'member_state': member_state
            }
        
        members_file = os.path.join(output_dir, "member_states.json")
        with open(members_file, 'w') as f:
            json.dump(members_data, f, indent=2)
        
        # Save case-specific data
        case_data = {
            'pheromonal': self.pheromonal_case.get_pheromone_environment(),
            'swarm': self.swarm_case.get_swarm_statistics(),
            'substrate': self.substrate_case.get_substrate_statistics(),
            'stigmergic': self.stigmergic_case.get_stigmergic_statistics()
        }
        
        case_file = os.path.join(output_dir, "case_statistics.json")
        with open(case_file, 'w') as f:
            json.dump(case_data, f, indent=2, default=str)
        
        # Save environment data
        env_data = {
            'food_sources': [pos.tolist() for pos in self.environment.food_sources],
            'threats': [pos.tolist() for pos in self.environment.threats],
            'temperature': self.environment.temperature,
            'humidity': self.environment.humidity
        }
        
        env_file = os.path.join(output_dir, "environment_data.json")
        with open(env_file, 'w') as f:
            json.dump(env_data, f, indent=2)
        
        logger.info(f"Simulation data saved to {output_dir}")


def run_insect_simulation():
    """Run a comprehensive insect simulation."""
    logger.info("Starting CEREBRUM Insect Simulation")
    
    # Create environment
    environment = SimulationEnvironment()
    
    # Create colony
    colony = InsectColony("colony_1", environment)
    
    # Add different types of members
    member_types = [
        (CasteType.QUEEN, 1),
        (CasteType.WORKER, 5),
        (CasteType.FORAGER, 3),
        (CasteType.BUILDER, 2),
        (CasteType.GUARD, 2)
    ]
    
    member_id = 0
    for caste_type, count in member_types:
        for _ in range(count):
            # Random initial position
            initial_pos = np.random.randn(3) * 2.0
            initial_pos[2] = 0.0  # Keep z=0
            
            colony.add_member(
                f"member_{member_id}",
                "honeybee",
                initial_pos,
                caste_type
            )
            member_id += 1
    
    # Run simulation
    simulation_steps = 50
    step_duration = 1.0
    
    logger.info(f"Running simulation for {simulation_steps} steps")
    
    for step in range(simulation_steps):
        logger.debug(f"Simulation step {step + 1}/{simulation_steps}")
        colony.simulate_step(step_duration)
        
        # Add some randomness to environment
        if step % 10 == 0:
            # Move food sources slightly
            for i, food_pos in enumerate(environment.food_sources):
                environment.food_sources[i] += np.random.randn(3) * 0.1
                environment.food_sources[i][2] = 0.0
    
    # Create timestamped output directory
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    output_dir = f"output/insect_simulation_{timestamp}"
    
    # Final statistics
    logger.info("Simulation completed!")
    colony._log_statistics()
    
    # Print case-specific statistics
    logger.info("Pheromonal Case Statistics:")
    pheromone_stats = colony.pheromonal_case.get_pheromone_environment()
    logger.info(f"  Active pheromones: {pheromone_stats['active_pheromones']}")
    
    logger.info("Swarm Case Statistics:")
    swarm_stats = colony.swarm_case.get_swarm_statistics()
    logger.info(f"  Total members: {swarm_stats['total_members']}")
    logger.info(f"  Cohesion: {swarm_stats['cohesion']:.3f}")
    logger.info(f"  Alignment: {swarm_stats['alignment']:.3f}")
    
    logger.info("Substrate Case Statistics:")
    substrate_stats = colony.substrate_case.get_substrate_statistics()
    logger.info(f"  Total interactions: {substrate_stats['total_interactions']}")
    logger.info(f"  Successful interactions: {substrate_stats['successful_interactions']}")
    
    logger.info("Stigmergic Case Statistics:")
    stigmergic_stats = colony.stigmergic_case.get_stigmergic_statistics()
    logger.info(f"  Active signals: {stigmergic_stats['active_signals']}")
    logger.info(f"  Total responses: {stigmergic_stats['total_responses']}")
    
    # Save simulation data to files
    colony.save_simulation_data(output_dir)


if __name__ == "__main__":
    run_insect_simulation() 