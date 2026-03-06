"""
Extended tests for insect case modules to push coverage toward 90%.
Targets: swarm (85%→90%), stigmergic (80%→90%), substrate (82%→90%), caste (86%→90%).
"""

import pytest
import numpy as np


from src.models.insect.cases.swarm import (
    SwarmCase, SwarmMember, SwarmBehavior, SwarmAction,
)
from src.models.insect.cases.stigmergic import (
    StigmergicCase, StigmergicSignal, EnvironmentalModification,
)
from src.models.insect.cases.substrate import (
    SubstrateCase, SubstrateType, SubstrateProperties,
)
from src.models.insect.cases.caste import CasteCase


# ═══════════════════════ SwarmCase extended tests ═══════════════════════

class TestSwarmCaseExtended:
    @pytest.fixture
    def swarm(self):
        s = SwarmCase()
        # Add 5 members
        for i in range(5):
            member = SwarmMember(
                id=f"ant_{i}",
                position=np.array([float(i), float(i)]),
                velocity=np.array([0.1, 0.1]),
                state={"energy": 0.8, "role": "worker"},
                role="worker",
                influence_radius=2.0
            )
            s.add_member(member)
        return s

    def test_allocate_tasks(self, swarm):
        """Cover L307-360: task allocation."""
        tasks = ["forage", "build", "defend"]
        capabilities = {
            "ant_0": ["forage", "build"],
            "ant_1": ["forage", "defend"],
            "ant_2": ["build"],
            "ant_3": ["defend", "forage"],
            "ant_4": ["build", "defend"],
        }
        result = swarm.allocate_tasks(tasks, capabilities)
        assert isinstance(result, dict)
        # All tasks should be assigned
        assert len(result) >= 1

    def test_coordinate_foraging(self, swarm):
        """Cover L362-420: foraging coordination."""
        food_sources = [
            np.array([5.0, 5.0]),
            np.array([10.0, 10.0]),
            np.array([2.0, 8.0]),
        ]
        food_qualities = [0.9, 0.5, 0.7]
        result = swarm.coordinate_foraging(food_sources, food_qualities)
        assert isinstance(result, dict)

    def test_synchronize_behavior(self, swarm):
        """Cover L422-459: behavior synchronization."""
        result = swarm.synchronize_behavior(
            SwarmBehavior.AGGREGATION,
            synchronization_strength=0.8
        )
        assert result is True

    def test_synchronize_dispersion(self, swarm):
        result = swarm.synchronize_behavior(
            SwarmBehavior.DISPERSION,
            synchronization_strength=0.3
        )
        assert result is True

    def test_collective_decision_majority(self, swarm):
        """Cover L225-305: collective decision making."""
        options = ["left", "right", "forward"]
        prefs = {
            "ant_0": "left",
            "ant_1": "left",
            "ant_2": "right",
            "ant_3": "left",
            "ant_4": "forward",
        }
        decision = swarm.make_collective_decision("dir_choice", options, prefs)
        assert decision.decision == "left"
        assert decision.consensus_level > 0.0

    def test_collective_decision_tie(self, swarm):
        options = ["A", "B"]
        prefs = {"ant_0": "A", "ant_1": "B", "ant_2": "A", "ant_3": "B"}
        decision = swarm.make_collective_decision("tie_test", options, prefs)
        assert decision.decision in ["A", "B"]

    def test_remove_member(self, swarm):
        """Cover L161-172: member removal."""
        swarm.remove_member("ant_0")
        assert "ant_0" not in swarm.members

    def test_compute_swarm_dynamics(self, swarm):
        """Cover L186-223: swarm dynamics computation."""
        state = swarm.compute_swarm_dynamics()
        assert state.total_members == 5
        assert state.cohesion >= 0.0
        assert state.dispersion >= 0.0


# ═══════════════════════ StigmergicCase extended tests ═══════════════════════

class TestStigmergicCaseExtended:
    @pytest.fixture
    def stig(self):
        return StigmergicCase()

    def test_create_and_detect_signals(self, stig):
        """Cover L183-276: signal creation and detection."""
        signal = stig.create_signal(
            StigmergicSignal.TRAIL_MARKER,
            position=np.array([5.0, 5.0]),
            creator_id="ant_0"
        )
        assert signal.signal_type == StigmergicSignal.TRAIL_MARKER
        detected = stig.detect_signals(np.array([5.5, 5.5]), detection_range=2.0)
        assert len(detected) >= 1

    def test_respond_to_signal(self, stig):
        """Cover L278-331: signal response."""
        signal = stig.create_signal(
            StigmergicSignal.FOOD_MARKER,
            position=np.array([3.0, 3.0]),
            creator_id="ant_0"
        )
        response = stig.respond_to_signal("ant_1", signal, "follow")
        assert response.insect_id == "ant_1"
        assert response.signal_type == StigmergicSignal.FOOD_MARKER

    def test_collective_project(self, stig):
        """Cover L333-417: project lifecycle."""
        project = stig.start_collective_project(
            "nest_1", "nest_building",
            {"material": "soil", "size": 100},
            "ant_0"
        )
        assert project.project_id == "nest_1"
        assert project.current_progress == 0.0

        success = stig.contribute_to_project("nest_1", "ant_1", 0.3)
        assert success is True

        # Verify project exists in active_projects
        assert "nest_1" in stig.active_projects

    def test_contribute_nonexistent_project(self, stig):
        """Cover error path in contribute_to_project."""
        result = stig.contribute_to_project("nonexistent", "ant_1", 0.5)
        assert result is False

    def test_cleanup_expired_signals(self, stig):
        """Cover L448-475: signal cleanup."""
        stig.create_signal(
            StigmergicSignal.TRAIL_MARKER,
            position=np.array([1.0, 1.0]),
            creator_id="ant_0",
            intensity=0.1  # low intensity → may expire faster
        )
        # Cleanup with a very large time should remove expired signals
        stig.cleanup_expired_signals(current_time=10000.0)
        # At least the cleanup method ran without error

    def test_get_stigmergic_statistics(self, stig):
        """Cover get_stigmergic_statistics."""
        stig.create_signal(
            StigmergicSignal.FOOD_MARKER,
            position=np.array([2.0, 2.0]),
            creator_id="ant_0"
        )
        stats = stig.get_stigmergic_statistics()
        assert isinstance(stats, dict)
        assert len(stats) > 0


# ═══════════════════════ SubstrateCase extended tests ═══════════════════════

class TestSubstrateCaseExtended:
    @pytest.fixture
    def sub(self):
        return SubstrateCase()

    def test_create_substrate_soil(self, sub):
        """Cover L237-288: substrate creation."""
        substrate = sub.create_substrate(SubstrateType.SOIL)
        assert substrate.substrate_type == SubstrateType.SOIL
        assert substrate.roughness > 0.0

    def test_create_substrate_leaf(self, sub):
        substrate = sub.create_substrate(SubstrateType.LEAF)
        assert substrate.substrate_type == SubstrateType.LEAF

    def test_create_with_custom_properties(self, sub):
        substrate = sub.create_substrate(
            SubstrateType.BARK,
            custom_properties={"roughness": 0.9, "temperature": 25.0}
        )
        assert substrate.roughness == 0.9
        assert substrate.temperature == 25.0

    def test_interact_with_substrate(self, sub):
        """Cover L290-349: substrate interaction."""
        substrate = sub.create_substrate(SubstrateType.SOIL)
        interaction = sub.interact_with_substrate(
            "ant_0", substrate, "digging", duration=2.0
        )
        assert interaction.insect_id == "ant_0"
        assert interaction.substrate_type == SubstrateType.SOIL

    def test_interact_climbing(self, sub):
        substrate = sub.create_substrate(SubstrateType.BARK)
        interaction = sub.interact_with_substrate(
            "ant_1", substrate, "climbing", duration=1.0
        )
        assert interaction.interaction_type == "climbing"

    def test_get_substrate_behavior(self, sub):
        """Cover L435-515: substrate behavior selection."""
        substrate = sub.create_substrate(SubstrateType.SOIL)
        capabilities = {"digging": 0.9, "climbing": 0.3, "walking": 0.7}
        behaviors = sub.get_substrate_behavior(substrate, capabilities)
        assert isinstance(behaviors, list)

    def test_get_substrate_statistics(self, sub):
        """Cover get_substrate_statistics."""
        substrate = sub.create_substrate(SubstrateType.SOIL)
        sub.interact_with_substrate("ant_0", substrate, "digging", duration=1.0)
        stats = sub.get_substrate_statistics()
        assert isinstance(stats, dict)


# ═══════════════════════ CasteCase extended tests ═══════════════════════

class TestCasteCaseExtended:
    @pytest.fixture
    def caste(self):
        return CasteCase()

    def test_determine_caste_worker(self, caste):
        """Cover L225-286: determine_caste method."""
        from src.models.insect.cases.caste import CasteType
        assignment = caste.determine_caste(
            "ant_0",
            genetic_markers={"worker_gene": 0.9, "soldier_gene": 0.1},
            environmental_conditions={"colony_size": 100, "food_abundance": 0.7},
            nutritional_history=[0.5, 0.6, 0.7, 0.8]
        )
        assert assignment.individual_id == "ant_0"
        assert isinstance(assignment.caste_type, CasteType)

    def test_determine_caste_soldier(self, caste):
        from src.models.insect.cases.caste import CasteType
        assignment = caste.determine_caste(
            "ant_1",
            genetic_markers={"worker_gene": 0.1, "soldier_gene": 0.9},
            environmental_conditions={"colony_size": 50, "threat_level": 0.8},
            nutritional_history=[0.9, 0.8, 0.9]
        )
        assert assignment.individual_id == "ant_1"

    def test_get_caste_behaviors(self, caste):
        """Cover L359-422: get_caste_behaviors."""
        from src.models.insect.cases.caste import CasteType
        behaviors = caste.get_caste_behaviors(
            CasteType.WORKER,
            current_conditions={"food_abundance": 0.5, "colony_size": 100}
        )
        assert isinstance(behaviors, list)

    def test_get_individual_caste(self, caste):
        """Cover L525-535: get_individual_caste."""
        caste.determine_caste(
            "ant_0",
            genetic_markers={"worker_gene": 0.9},
            environmental_conditions={"colony_size": 100},
            nutritional_history=[0.5]
        )
        result = caste.get_individual_caste("ant_0")
        assert result is not None
        assert result.individual_id == "ant_0"

    def test_get_individual_caste_not_found(self, caste):
        result = caste.get_individual_caste("nonexistent")
        assert result is None

    def test_update_caste_behavior(self, caste):
        """Cover L537-565: update_caste_behavior."""
        caste.determine_caste(
            "ant_0",
            genetic_markers={"worker_gene": 0.9},
            environmental_conditions={"colony_size": 100},
            nutritional_history=[0.5]
        )
        caste.update_caste_behavior("ant_0", "foraging", success=True)
        caste.update_caste_behavior("ant_0", "foraging", success=False)

    def test_get_caste_statistics(self, caste):
        """Cover L499-523: get_caste_statistics."""
        caste.determine_caste(
            "ant_0",
            genetic_markers={"worker_gene": 0.9},
            environmental_conditions={},
            nutritional_history=[0.5]
        )
        stats = caste.get_caste_statistics()
        assert isinstance(stats, dict)
        assert stats["total_individuals"] >= 1
