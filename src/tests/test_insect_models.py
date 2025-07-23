"""
Comprehensive Tests for Insect Models

This module provides comprehensive tests for the CEREBRUM insect models,
including base models, neural structures, and insect-specific cases.
"""

import pytest
import numpy as np
from typing import Dict, Any
import tempfile
import os

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
    ChemicalSignal,
    ProcessedSignal
)
from src.models.insect.cases.swarm import (
    SwarmCase,
    SwarmBehavior,
    SwarmMember,
    SwarmState
)
from src.models.insect.cases.metamorphic import (
    MetamorphicCase,
    DevelopmentalStage,
    DevelopmentalState
)
from src.models.insect.cases.caste import (
    CasteCase,
    CasteType,
    CasteProfile,
    CasteAssignment
)
from src.models.insect.cases.substrate import (
    SubstrateCase,
    SubstrateType,
    SubstrateProperties
)
from src.models.insect.cases.stigmergic import (
    StigmergicCase,
    StigmergicSignal,
    EnvironmentalModification
)
from src.core.model import Case


class TestInsectBaseModels:
    """Test the base insect model classes."""
    
    def test_insect_model_initialization(self):
        """Test basic insect model initialization."""
        model = InsectModel(
            species="honeybee",
            initial_case=Case.NOMINATIVE,
            initial_state=BehavioralState.IDLE
        )
        
        assert model.species == "honeybee"
        assert model.current_case == Case.NOMINATIVE
        assert model.behavioral_state == BehavioralState.IDLE
        assert len(model.neural_structures) == 0
        assert len(model.case_assignments) > 0
    
    def test_case_transformation(self):
        """Test case transformation functionality."""
        model = InsectModel("ant", initial_case=Case.NOMINATIVE)
        
        # Test valid transformation
        success = model.transform_case(Case.ACCUSATIVE)
        assert success
        assert model.current_case == Case.ACCUSATIVE
        
        # Test invalid transformation
        success = model.transform_case(Case.VOCATIVE)
        assert not success
        assert model.current_case == Case.ACCUSATIVE  # Should remain unchanged
    
    def test_sensory_processing(self):
        """Test sensory input processing."""
        model = InsectModel("bee")
        
        # Create sensory input
        sensory_input = SensoryInput(
            visual=np.random.rand(100),
            olfactory=np.random.rand(50),
            pheromonal=np.random.rand(20),
            timestamp=0.0
        )
        
        # Process sensory input
        processed = model.process_sensory_input(sensory_input)
        
        assert isinstance(processed, dict)
        assert len(model.sensory_history) == 1
    
    def test_action_selection(self):
        """Test action selection by case."""
        model = InsectModel("wasp", initial_case=Case.NOMINATIVE)
        
        context = {"timestamp": 0.0}
        action = model.select_action(context)
        
        assert isinstance(action, Action)
        assert action.action_type in ["explore", "observe", "sense", "produce", "navigate"]
        assert 0.0 <= action.confidence <= 1.0
    
    def test_behavioral_state_update(self):
        """Test behavioral state transitions."""
        model = InsectModel("fly", initial_state=BehavioralState.IDLE)
        
        model.update_behavioral_state(BehavioralState.FORAGING)
        assert model.behavioral_state == BehavioralState.FORAGING
        assert len(model.state_history) == 1
    
    def test_performance_summary(self):
        """Test performance summary generation."""
        model = InsectModel("mosquito")
        
        # Perform some actions
        for _ in range(5):
            model.select_action({"timestamp": 0.0})
        
        summary = model.get_performance_summary()
        
        assert summary['total_actions'] == 5
        assert summary['current_case'] == Case.NOMINATIVE
        assert summary['current_behavioral_state'] == BehavioralState.IDLE


class TestNeuralStructures:
    """Test neural structure implementations."""
    
    def test_mushroom_body(self):
        """Test mushroom body functionality."""
        mb = MushroomBody()
        
        # Test input processing
        input_data = np.random.rand(200)
        output = mb.process_input(input_data)
        
        assert isinstance(output, np.ndarray)
        assert len(output) == 50  # Output dimension
        
        # Test learning
        stimulus = np.random.rand(200)
        outcome = np.random.rand(50)
        mb.learn_association(stimulus, outcome)
        
        # Test recall
        recalled = mb.recall_association(stimulus)
        assert recalled is not None
    
    def test_central_complex(self):
        """Test central complex functionality."""
        cc = CentralComplex()
        
        # Test input processing
        input_data = np.random.rand(150)
        output = cc.process_input(input_data)
        
        assert isinstance(output, np.ndarray)
        assert len(output) == 80  # Output dimension
        
        # Test navigation state
        nav_state = cc.get_navigation_state()
        assert 'heading' in nav_state
        assert 'position' in nav_state
        assert 'motor_commands' in nav_state
    
    def test_antennal_lobe(self):
        """Test antennal lobe functionality."""
        al = AntennalLobe()
        
        # Test input processing
        input_data = np.random.rand(300)
        output = al.process_input(input_data)
        
        assert isinstance(output, np.ndarray)
        assert len(output) == 150  # Output dimension
        
        # Test odor learning
        odor_pattern = np.random.rand(300)
        al.learn_odor(odor_pattern, "test_odor")
        
        # Test odor recognition
        recognized = al.recognize_odor(odor_pattern)
        assert recognized == "test_odor"
    
    def test_optic_lobe(self):
        """Test optic lobe functionality."""
        ol = OpticLobe()
        
        # Test input processing
        input_data = np.random.rand(400)
        output = ol.process_input(input_data)
        
        assert isinstance(output, np.ndarray)
        assert len(output) == 200  # Output dimension
    
    def test_subesophageal_ganglion(self):
        """Test subesophageal ganglion functionality."""
        sg = SubesophagealGanglion()
        
        # Test input processing
        input_data = np.random.rand(100)
        output = sg.process_input(input_data)
        
        assert isinstance(output, np.ndarray)
        assert len(output) == 80  # Output dimension
        
        # Test feeding state
        feeding_state = sg.get_feeding_state()
        assert 'hunger_level' in feeding_state
        assert 'feeding_behavior' in feeding_state
    
    def test_ventral_nerve_cord(self):
        """Test ventral nerve cord functionality."""
        vnc = VentralNerveCord()
        
        # Test input processing
        input_data = np.random.rand(120)
        output = vnc.process_input(input_data)
        
        assert isinstance(output, np.ndarray)
        assert len(output) == 100  # Output dimension
        
        # Test transmission state
        transmission_state = vnc.get_transmission_state()
        assert 'transmission_efficiency' in transmission_state
        assert 'signal_delay' in transmission_state


class TestPheromonalCase:
    """Test pheromonal case functionality."""
    
    def test_pheromonal_case_initialization(self):
        """Test pheromonal case initialization."""
        pc = PheromonalCase()
        
        assert pc.case_id == "PHE"
        assert pc.case_name == "pheromonal"
        assert len(pc.volatility_rates) > 0
        assert len(pc.concentration_thresholds) > 0
    
    def test_chemical_signal_processing(self):
        """Test chemical signal processing."""
        pc = PheromonalCase()
        
        # Create chemical signal
        signal = ChemicalSignal(
            pheromone_type=PheromoneType.TRAIL,
            concentration=0.8,
            volatility=0.1,
            source_position=np.array([1.0, 2.0, 0.0]),
            timestamp=0.0
        )
        
        # Process signal
        processed = pc.process_chemical_signal(signal)
        
        assert isinstance(processed, ProcessedSignal)
        assert processed.signal_strength > 0
        assert processed.behavioral_response == "follow_trail"
    
    def test_pheromone_generation(self):
        """Test pheromone generation."""
        pc = PheromonalCase()
        
        # Generate pheromone
        output = pc.generate_pheromone("trail", 0.7, np.array([1.0, 2.0, 0.0]))
        
        assert output.pheromone_type == PheromoneType.TRAIL
        assert output.concentration == 0.7
        assert output.duration > 0
    
    def test_pheromone_environment(self):
        """Test pheromone environment management."""
        pc = PheromonalCase()
        
        # Generate some pheromones
        pc.generate_pheromone("trail", 0.5)
        pc.generate_pheromone("alarm", 0.8)
        
        # Get environment state
        env_state = pc.get_pheromone_environment()
        
        assert env_state['active_pheromones'] == 2
        assert 'trail' in env_state['pheromone_types']
        assert 'alarm' in env_state['pheromone_types']


class TestSwarmCase:
    """Test swarm case functionality."""
    
    def test_swarm_case_initialization(self):
        """Test swarm case initialization."""
        sc = SwarmCase()
        
        assert sc.case_id == "SWARM"
        assert sc.case_name == "swarm"
        assert len(sc.members) == 0
    
    def test_swarm_member_management(self):
        """Test swarm member management."""
        sc = SwarmCase()
        
        # Create swarm member
        member = SwarmMember(
            id="member_1",
            position=np.array([0.0, 0.0, 0.0]),
            velocity=np.array([1.0, 0.0, 0.0]),
            state={"energy": 0.8},
            role="forager",
            influence_radius=1.0
        )
        
        # Add member
        sc.add_member(member)
        assert len(sc.members) == 1
        assert "member_1" in sc.members
        
        # Remove member
        sc.remove_member("member_1")
        assert len(sc.members) == 0
    
    def test_swarm_dynamics(self):
        """Test swarm dynamics computation."""
        sc = SwarmCase()
        
        # Add multiple members
        for i in range(3):
            member = SwarmMember(
                id=f"member_{i}",
                position=np.array([float(i), 0.0, 0.0]),
                velocity=np.array([1.0, 0.0, 0.0]),
                state={},
                role="worker",
                influence_radius=1.0
            )
            sc.add_member(member)
        
        # Compute dynamics
        swarm_state = sc.compute_swarm_dynamics()
        
        assert swarm_state.total_members == 3
        assert swarm_state.cohesion > 0
        assert swarm_state.alignment >= 0
    
    def test_collective_decision_making(self):
        """Test collective decision making."""
        sc = SwarmCase()
        
        # Add members
        for i in range(5):
            member = SwarmMember(
                id=f"member_{i}",
                position=np.array([0.0, 0.0, 0.0]),
                velocity=np.array([0.0, 0.0, 0.0]),
                state={},
                role="worker",
                influence_radius=1.0
            )
            sc.add_member(member)
        
        # Make decision
        options = ["option_a", "option_b", "option_c"]
        preferences = {
            "member_0": "option_a",
            "member_1": "option_a",
            "member_2": "option_b",
            "member_3": "option_a",
            "member_4": "option_c"
        }
        
        decision = sc.make_collective_decision("decision_1", options, preferences)
        
        assert decision.decision == "option_a"  # Should be the most preferred
        assert decision.confidence > 0
        assert decision.consensus_level > 0


class TestMetamorphicCase:
    """Test metamorphic case functionality."""
    
    def test_metamorphic_case_initialization(self):
        """Test metamorphic case initialization."""
        mc = MetamorphicCase()
        
        assert mc.case_id == "MET"
        assert mc.case_name == "metamorphic"
        assert len(mc.stage_durations) > 0
        assert len(mc.growth_rates) > 0
    
    def test_developmental_state_update(self):
        """Test developmental state updates."""
        mc = MetamorphicCase()
        
        # Create initial state
        initial_state = DevelopmentalState(
            stage=DevelopmentalStage.LARVA,
            age=5.0,
            size=0.3,
            maturity=0.4,
            hormone_levels={
                'juvenile_hormone': 0.8,
                'ecdysone': 0.2,
                'insulin': 0.6
            }
        )
        
        # Environmental conditions
        env_conditions = {
            'temperature': 25.0,
            'humidity': 0.6,
            'nutrition': 0.8
        }
        
        # Update state
        updated_state = mc.update_developmental_state(
            initial_state, 1.0, env_conditions
        )
        
        assert updated_state.age > initial_state.age
        assert updated_state.size >= initial_state.size
        assert updated_state.maturity >= initial_state.maturity
    
    def test_stage_transition(self):
        """Test developmental stage transitions."""
        mc = MetamorphicCase()
        
        # Create mature larva state
        larva_state = DevelopmentalState(
            stage=DevelopmentalStage.LARVA,
            age=15.0,  # Old enough for transition
            size=0.8,  # Large enough
            maturity=0.9,  # Mature enough
            hormone_levels={
                'juvenile_hormone': 0.3,
                'ecdysone': 0.8,  # High ecdysone
                'insulin': 0.7
            }
        )
        
        env_conditions = {
            'temperature': 25.0,
            'humidity': 0.6,
            'nutrition': 0.8
        }
        
        # Update state (should trigger transition)
        updated_state = mc.update_developmental_state(
            larva_state, 1.0, env_conditions
        )
        
        # Check if transition occurred
        if updated_state.stage == DevelopmentalStage.PUPA:
            assert len(mc.transition_history) > 0
    
    def test_developmental_summary(self):
        """Test developmental summary generation."""
        mc = MetamorphicCase()
        
        # Create and update state multiple times
        state = DevelopmentalState(
            stage=DevelopmentalStage.LARVA,
            age=0.0,
            size=0.1,
            maturity=0.0,
            hormone_levels={'juvenile_hormone': 1.0, 'ecdysone': 0.1, 'insulin': 0.5}
        )
        
        env_conditions = {'temperature': 25.0, 'humidity': 0.6, 'nutrition': 0.8}
        
        for _ in range(10):
            state = mc.update_developmental_state(state, 0.1, env_conditions)
        
        summary = mc.get_developmental_summary()
        
        assert summary['current_stage'] == 'larva'
        assert summary['current_age'] > 0
        assert summary['total_transitions'] >= 0


class TestCasteCase:
    """Test caste case functionality."""
    
    def test_caste_case_initialization(self):
        """Test caste case initialization."""
        cc = CasteCase()
        
        assert cc.case_id == "CAST"
        assert cc.case_name == "caste"
        assert len(cc.caste_profiles) > 0
    
    def test_caste_determination(self):
        """Test caste determination."""
        cc = CasteCase()
        
        # Genetic markers
        genetic_markers = {'caste_genes': 0.7}
        
        # Environmental conditions
        env_conditions = {
            'temperature': 25.0,
            'humidity': 0.6
        }
        
        # Nutritional history
        nutrition_history = [0.8, 0.7, 0.9, 0.6, 0.8]
        
        # Determine caste
        assignment = cc.determine_caste(
            "individual_1",
            genetic_markers,
            env_conditions,
            nutrition_history
        )
        
        assert assignment.individual_id == "individual_1"
        assert assignment.caste_type in CasteType
        assert assignment.confidence > 0
        assert assignment.confidence <= 1.0
    
    def test_caste_behaviors(self):
        """Test caste-specific behaviors."""
        cc = CasteCase()
        
        # Get behaviors for worker caste
        conditions = {
            'food_available': True,
            'brood_present': True,
            'materials_available': True
        }
        
        behaviors = cc.get_caste_behaviors(CasteType.WORKER, conditions)
        
        assert len(behaviors) > 0
        for behavior in behaviors:
            assert behavior.caste_type == CasteType.WORKER
            assert behavior.priority > 0
            assert behavior.success_rate > 0
    
    def test_caste_statistics(self):
        """Test caste statistics."""
        cc = CasteCase()
        
        # Determine some castes
        for i in range(10):
            assignment = cc.determine_caste(
                f"individual_{i}",
                {'caste_genes': np.random.random()},
                {'temperature': 25.0, 'humidity': 0.6},
                [0.8] * 5
            )
        
        stats = cc.get_caste_statistics()
        
        assert stats['total_individuals'] == 10
        assert len(stats['caste_distribution']) > 0
        assert stats['total_assignments'] == 10


class TestSubstrateCase:
    """Test substrate case functionality."""
    
    def test_substrate_case_initialization(self):
        """Test substrate case initialization."""
        sc = SubstrateCase()
        
        assert sc.case_id == "SUB"
        assert sc.case_name == "substrate"
        assert len(sc.substrate_templates) > 0
    
    def test_substrate_creation(self):
        """Test substrate creation."""
        sc = SubstrateCase()
        
        # Create soil substrate
        soil = sc.create_substrate(SubstrateType.SOIL)
        
        assert soil.substrate_type == SubstrateType.SOIL
        assert 0.0 <= soil.roughness <= 1.0
        assert 0.0 <= soil.hardness <= 1.0
        assert 0.0 <= soil.porosity <= 1.0
        assert 0.0 <= soil.moisture <= 1.0
        
        # Create custom substrate
        custom_soil = sc.create_substrate(
            SubstrateType.SOIL,
            {'roughness': 0.9, 'hardness': 0.8}
        )
        
        assert custom_soil.roughness == 0.9
        assert custom_soil.hardness == 0.8
    
    def test_substrate_interaction(self):
        """Test substrate interactions."""
        sc = SubstrateCase()
        
        # Create substrate
        substrate = sc.create_substrate(SubstrateType.LEAF)
        
        # Interact with substrate
        interaction = sc.interact_with_substrate(
            "insect_1",
            substrate,
            "walking",
            5.0
        )
        
        assert interaction.insect_id == "insect_1"
        assert interaction.substrate_type == SubstrateType.LEAF
        assert interaction.interaction_type == "walking"
        assert interaction.duration == 5.0
        assert 0.0 <= interaction.intensity <= 1.0
        assert isinstance(interaction.success, bool)
    
    def test_substrate_behaviors(self):
        """Test substrate-dependent behaviors."""
        sc = SubstrateCase()
        
        # Create substrate
        substrate = sc.create_substrate(SubstrateType.BARK)
        
        # Insect capabilities
        capabilities = {
            'walking_ability': 0.8,
            'climbing_ability': 0.9,
            'digging_ability': 0.3,
            'feeding_ability': 0.6,
            'nesting_ability': 0.7
        }
        
        # Get behaviors
        behaviors = sc.get_substrate_behavior(substrate, capabilities)
        
        assert len(behaviors) > 0
        for behavior in behaviors:
            assert behavior.behavior_type in ['walking', 'climbing', 'digging', 'feeding', 'nesting']
            assert behavior.adaptation_factor > 0
            assert behavior.energy_cost >= 0
            assert behavior.success_rate > 0


class TestStigmergicCase:
    """Test stigmergic case functionality."""
    
    def test_stigmergic_case_initialization(self):
        """Test stigmergic case initialization."""
        sc = StigmergicCase()
        
        assert sc.case_id == "STIG"
        assert sc.case_name == "stigmergic"
        assert len(sc.signal_templates) > 0
    
    def test_signal_creation(self):
        """Test stigmergic signal creation."""
        sc = StigmergicCase()
        
        # Create trail marker
        position = np.array([1.0, 2.0, 0.0])
        signal = sc.create_signal(
            StigmergicSignal.TRAIL_MARKER,
            position,
            "insect_1",
            0.8
        )
        
        assert signal.signal_type == StigmergicSignal.TRAIL_MARKER
        assert np.array_equal(signal.position, position)
        assert signal.intensity == 0.8
        assert signal.creator_id == "insect_1"
        assert len(sc.active_modifications) == 1
    
    def test_signal_detection(self):
        """Test signal detection."""
        sc = StigmergicCase()
        
        # Create signals at different positions
        sc.create_signal(StigmergicSignal.TRAIL_MARKER, np.array([0.0, 0.0, 0.0]), "insect_1")
        sc.create_signal(StigmergicSignal.FOOD_MARKER, np.array([5.0, 5.0, 0.0]), "insect_2")
        
        # Detect signals from origin
        detected = sc.detect_signals(np.array([0.0, 0.0, 0.0]), 2.0)
        
        assert len(detected) == 1  # Only trail marker should be detected
        assert detected[0].signal_type == StigmergicSignal.TRAIL_MARKER
    
    def test_signal_response(self):
        """Test signal response generation."""
        sc = StigmergicCase()
        
        # Create signal
        signal = sc.create_signal(
            StigmergicSignal.FOOD_MARKER,
            np.array([1.0, 0.0, 0.0]),
            "insect_1"
        )
        
        # Respond to signal
        response = sc.respond_to_signal("insect_2", signal, "approach")
        
        assert response.insect_id == "insect_2"
        assert response.signal_type == StigmergicSignal.FOOD_MARKER
        assert response.response_type == "approach"
        assert response.intensity > 0
        assert response.confidence > 0
    
    def test_collective_project(self):
        """Test collective construction projects."""
        sc = StigmergicCase()
        
        # Start project
        target_structure = {
            'type': 'nest',
            'size': 10.0,
            'position': np.array([0.0, 0.0, 0.0])
        }
        
        project = sc.start_collective_project(
            "project_1",
            "nest_building",
            target_structure,
            "insect_1"
        )
        
        assert project.project_id == "project_1"
        assert project.project_type == "nest_building"
        assert project.current_progress == 0.0
        assert "insect_1" in project.participants
        
        # Contribute to project
        success = sc.contribute_to_project("project_1", "insect_2", 0.3)
        assert success
        
        # Check progress
        updated_project = sc.active_projects["project_1"]
        assert updated_project.current_progress > 0.0
        assert "insect_2" in updated_project.participants


class TestInsectIntegration:
    """Test integration between different insect components."""
    
    def test_insect_model_with_neural_structures(self):
        """Test insect model with neural structures."""
        # Create model with neural configuration
        neural_config = {
            'mushroom_body': {'learning_rate': 0.01},
            'central_complex': {'activation_threshold': 0.3},
            'antennal_lobe': {'sensory_gain': 1.2}
        }
        
        model = InsectModel("honeybee", neural_config=neural_config)
        
        # Test that neural structures can be initialized
        assert isinstance(model.neural_structures, dict)
    
    def test_case_transformation_with_behavior(self):
        """Test case transformation affecting behavior."""
        model = InsectModel("ant", initial_case=Case.NOMINATIVE)
        
        # Get action in nominative case
        context = {"timestamp": 0.0}
        nominative_action = model.select_action(context)
        
        # Transform to accusative case
        model.transform_case(Case.ACCUSATIVE)
        accusative_action = model.select_action(context)
        
        # Actions should be different due to case change
        assert nominative_action.action_type != accusative_action.action_type
    
    def test_pheromonal_swarm_integration(self):
        """Test integration between pheromonal and swarm cases."""
        pc = PheromonalCase()
        sc = SwarmCase()
        
        # Create pheromone signal
        signal = ChemicalSignal(
            pheromone_type=PheromoneType.RECRUITMENT,
            concentration=0.8,
            volatility=0.6,
            timestamp=0.0
        )
        
        # Process signal
        processed = pc.process_chemical_signal(signal)
        
        # If signal indicates recruitment, add to swarm
        if processed.behavioral_response == "join_recruitment":
            member = SwarmMember(
                id="recruited_insect",
                position=np.array([0.0, 0.0, 0.0]),
                velocity=np.array([0.0, 0.0, 0.0]),
                state={"recruited": True},
                role="forager",
                influence_radius=1.0
            )
            sc.add_member(member)
            
            assert len(sc.members) == 1
            assert "recruited_insect" in sc.members


if __name__ == "__main__":
    # Run all tests
    pytest.main([__file__, "-v"]) 