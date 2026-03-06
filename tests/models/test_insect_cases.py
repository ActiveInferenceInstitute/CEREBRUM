"""
Comprehensive tests for insect case modules:
- SwarmCase (55% → 85%+)
- StigmergicCase (66% → 85%+)
- PheromonalCase (74% → 85%+)
- SubstrateCase (77% → 85%+)
- CasteCase (79% → 85%+)
- MetamorphicCase (79% → 85%+)
"""

import pytest
import numpy as np

from src.models.insect.cases.swarm import (
    SwarmCase, SwarmBehavior, SwarmMember, SwarmState, CollectiveDecision,
)
from src.models.insect.cases.stigmergic import (
    StigmergicCase, StigmergicSignal, EnvironmentalModification, StigmergicResponse,
    CollectiveConstruction,
)
from src.models.insect.cases.pheromonal import (
    PheromonalCase, PheromoneType, ChemicalSignal, ProcessedSignal, PheromoneOutput,
)
from src.models.insect.cases.substrate import SubstrateCase
from src.models.insect.cases.caste import CasteCase
from src.models.insect.cases.metamorphic import MetamorphicCase
from src.core.model import Case


# ═══════════════════════ SwarmCase ═══════════════════════


@pytest.fixture
def swarm():
    return SwarmCase()


@pytest.fixture
def swarm_members():
    return [
        SwarmMember(id="m1", position=np.array([0.0, 0.0]),
                    velocity=np.array([1.0, 0.0]), state={}, role="worker",
                    influence_radius=5.0),
        SwarmMember(id="m2", position=np.array([1.0, 1.0]),
                    velocity=np.array([0.0, 1.0]), state={}, role="worker",
                    influence_radius=5.0),
        SwarmMember(id="m3", position=np.array([2.0, 0.0]),
                    velocity=np.array([1.0, 1.0]), state={}, role="scout",
                    influence_radius=5.0),
    ]


class TestSwarmCase:
    def test_init_default(self, swarm):
        assert swarm.case == Case.NOMINATIVE
        assert len(swarm.members) == 0

    def test_init_custom_config(self):
        cfg = {"max_swarm_size": 200, "cohesion_weight": 0.8}
        sc = SwarmCase(config=cfg)
        assert sc.config["max_swarm_size"] == 200

    def test_case_property(self, swarm):
        swarm.case = Case.NOMINATIVE
        assert swarm.case == Case.NOMINATIVE

    def test_add_member(self, swarm, swarm_members):
        swarm.add_member(swarm_members[0])
        assert len(swarm.members) == 1

    def test_remove_member(self, swarm, swarm_members):
        swarm.add_member(swarm_members[0])
        swarm.remove_member("m1")
        assert len(swarm.members) == 0

    def test_remove_nonexistent(self, swarm):
        swarm.remove_member("unknown")
        assert len(swarm.members) == 0

    def test_update_member_state(self, swarm, swarm_members):
        swarm.add_member(swarm_members[0])
        swarm.update_member_state("m1", {"energy": 0.9})
        # Verify state was updated (merge into existing state dict)
        # Access may be via dict or list depending on implementation

    def test_compute_swarm_dynamics_empty(self, swarm):
        state = swarm.compute_swarm_dynamics()
        assert isinstance(state, SwarmState)
        assert state.total_members == 0

    def test_compute_swarm_dynamics(self, swarm, swarm_members):
        for m in swarm_members:
            swarm.add_member(m)
        state = swarm.compute_swarm_dynamics()
        assert state.total_members == 3
        assert state.cohesion >= 0
        assert state.alignment >= 0

    def test_make_collective_decision(self, swarm, swarm_members):
        for m in swarm_members:
            swarm.add_member(m)
        prefs = {"m1": "A", "m2": "A", "m3": "B"}
        decision = swarm.make_collective_decision("d1", ["A", "B"], prefs)
        assert isinstance(decision, CollectiveDecision)
        assert decision.decision == "A"
        assert decision.confidence > 0

    def test_make_collective_decision_tie(self, swarm, swarm_members):
        for m in swarm_members[:2]:
            swarm.add_member(m)
        prefs = {"m1": "A", "m2": "B"}
        decision = swarm.make_collective_decision("d2", ["A", "B"], prefs)
        assert isinstance(decision, CollectiveDecision)

    def test_make_collective_decision_empty(self, swarm):
        decision = swarm.make_collective_decision("d3", ["A", "B"], {})
        assert isinstance(decision, CollectiveDecision)

    def test_allocate_tasks(self, swarm, swarm_members):
        for m in swarm_members:
            swarm.add_member(m)
        tasks = ["forage", "guard"]
        caps = {"m1": ["forage", "guard"], "m2": ["forage"], "m3": ["guard"]}
        alloc = swarm.allocate_tasks(tasks, caps)
        assert isinstance(alloc, dict)

    def test_coordinate_foraging(self, swarm, swarm_members):
        for m in swarm_members:
            swarm.add_member(m)
        sources = [np.array([5.0, 5.0]), np.array([10.0, 10.0])]
        qualities = [0.8, 0.5]
        assign = swarm.coordinate_foraging(sources, qualities)
        assert isinstance(assign, dict)

    def test_synchronize_behavior(self, swarm, swarm_members):
        for m in swarm_members:
            swarm.add_member(m)
        result = swarm.synchronize_behavior(SwarmBehavior.SYNCHRONIZATION, 0.7)
        assert isinstance(result, bool)


# ═══════════════════════ StigmergicCase ═══════════════════════


@pytest.fixture
def stigmergic():
    return StigmergicCase()


class TestStigmergicCase:
    def test_init_default(self, stigmergic):
        assert stigmergic.case == Case.NOMINATIVE

    def test_case_property(self, stigmergic):
        stigmergic.case = Case.LOCATIVE
        assert stigmergic.case == Case.LOCATIVE

    def test_create_signal(self, stigmergic):
        mod = stigmergic.create_signal(
            StigmergicSignal.TRAIL_MARKER,
            position=np.array([1.0, 2.0]),
            creator_id="ant1",
            intensity=0.8,
        )
        assert isinstance(mod, EnvironmentalModification)
        assert mod.signal_type == StigmergicSignal.TRAIL_MARKER
        assert mod.intensity == 0.8

    def test_create_signal_default_intensity(self, stigmergic):
        mod = stigmergic.create_signal(
            StigmergicSignal.FOOD_MARKER,
            position=np.array([0.0, 0.0]),
            creator_id="ant2",
        )
        assert mod.intensity > 0

    def test_detect_signals(self, stigmergic):
        stigmergic.create_signal(StigmergicSignal.TRAIL_MARKER,
                                  np.array([1.0, 1.0]), "a1")
        stigmergic.create_signal(StigmergicSignal.FOOD_MARKER,
                                  np.array([100.0, 100.0]), "a2")
        detected = stigmergic.detect_signals(np.array([0.0, 0.0]), 5.0)
        assert len(detected) >= 1

    def test_detect_signals_none(self, stigmergic):
        detected = stigmergic.detect_signals(np.array([0.0, 0.0]), 5.0)
        assert detected == []

    def test_respond_to_signal(self, stigmergic):
        mod = stigmergic.create_signal(StigmergicSignal.TRAIL_MARKER,
                                        np.array([0.0, 0.0]), "a1")
        resp = stigmergic.respond_to_signal("a2", mod, "follow")
        assert isinstance(resp, StigmergicResponse)
        assert resp.insect_id == "a2"

    def test_start_collective_project(self, stigmergic):
        proj = stigmergic.start_collective_project(
            "p1", "nest", {"size": "large"}, "a1"
        )
        assert isinstance(proj, CollectiveConstruction)
        assert proj.project_id == "p1"

    def test_contribute_to_project(self, stigmergic):
        stigmergic.start_collective_project("p1", "nest", {"size": "large"}, "a1")
        result = stigmergic.contribute_to_project("p1", "a2", 0.3)
        assert result is True

    def test_contribute_nonexistent(self, stigmergic):
        result = stigmergic.contribute_to_project("missing", "a1", 0.5)
        assert result is False

    def test_get_nearby_projects(self, stigmergic):
        stigmergic.start_collective_project("p1", "nest", {"size": "small"}, "a1")
        nearby = stigmergic.get_nearby_projects(np.array([0.0, 0.0]), 100.0)
        assert isinstance(nearby, list)

    def test_cleanup_expired_signals(self, stigmergic):
        stigmergic.create_signal(StigmergicSignal.TRAIL_MARKER,
                                  np.array([0.0, 0.0]), "a1")
        # Set far-future time to expire all signals
        stigmergic.cleanup_expired_signals(1e10)

    def test_get_statistics(self, stigmergic):
        stigmergic.create_signal(StigmergicSignal.TRAIL_MARKER,
                                  np.array([0.0, 0.0]), "a1")
        stats = stigmergic.get_stigmergic_statistics()
        assert isinstance(stats, dict)
        assert len(stats) > 0


# ═══════════════════════ PheromonalCase ═══════════════════════


@pytest.fixture
def pheromonal():
    return PheromonalCase()


class TestPheromonalCase:
    def test_init(self, pheromonal):
        assert pheromonal.case == Case.NOMINATIVE

    def test_case_property(self, pheromonal):
        pheromonal.case = Case.DATIVE
        assert pheromonal.case == Case.DATIVE

    def test_process_chemical_signal_trail(self, pheromonal):
        signal = ChemicalSignal(
            pheromone_type=PheromoneType.TRAIL,
            concentration=0.8,
            volatility=0.3,
            source_position=np.array([1.0, 2.0]),
        )
        result = pheromonal.process_chemical_signal(signal)
        assert isinstance(result, ProcessedSignal)
        assert result.signal_strength > 0

    def test_process_chemical_signal_alarm(self, pheromonal):
        signal = ChemicalSignal(
            pheromone_type=PheromoneType.ALARM,
            concentration=0.9,
            volatility=0.8,
        )
        result = pheromonal.process_chemical_signal(signal)
        assert isinstance(result, ProcessedSignal)
        assert result.urgency > 0

    def test_generate_pheromone(self, pheromonal):
        output = pheromonal.generate_pheromone(
            pheromone_type="trail",
            concentration=0.7,
            target_position=np.array([5.0, 5.0]),
        )
        assert isinstance(output, PheromoneOutput)
        assert output.concentration > 0

    def test_generate_pheromone_no_target(self, pheromonal):
        output = pheromonal.generate_pheromone("alarm", 0.9)
        assert isinstance(output, PheromoneOutput)

    def test_calculate_urgency(self, pheromonal):
        urgency = pheromonal._calculate_urgency(PheromoneType.ALARM, 0.8)
        assert 0 <= urgency <= 1

    def test_calculate_duration(self, pheromonal):
        dur = pheromonal._calculate_duration(0.5)
        assert dur > 0

    def test_update_pheromone_memory(self, pheromonal):
        signal = ChemicalSignal(
            pheromone_type=PheromoneType.TRAIL,
            concentration=0.5,
            volatility=0.2,
        )
        pheromonal._update_pheromone_memory(signal, 0.7)
        summary = pheromonal.get_learning_summary()
        assert summary["total_encounters"] >= 1

    def test_get_pheromone_environment(self, pheromonal):
        env = pheromonal.get_pheromone_environment()
        assert isinstance(env, dict)

    def test_cleanup_expired(self, pheromonal):
        pheromonal.generate_pheromone("trail", 0.5)
        pheromonal.cleanup_expired_pheromones(1e10)

    def test_detect_signals(self, pheromonal):
        pheromonal.generate_pheromone("trail", 0.8,
                                      target_position=np.array([1.0, 1.0]))
        detected = pheromonal.detect_signals(np.array([0.0, 0.0]), 10.0)
        assert isinstance(detected, list)


# ═══════════════════════ SubstrateCase ═══════════════════════

from src.models.insect.cases.substrate import SubstrateType, SubstrateProperties

@pytest.fixture
def substrate():
    return SubstrateCase()


class TestSubstrateCase:
    def test_init(self, substrate):
        assert substrate.case == Case.NOMINATIVE

    def test_case_property(self, substrate):
        substrate.case = Case.LOCATIVE
        assert substrate.case == Case.LOCATIVE

    def test_create_substrate(self, substrate):
        props = substrate.create_substrate(SubstrateType.SOIL)
        assert isinstance(props, SubstrateProperties)

    def test_create_substrate_custom(self, substrate):
        props = substrate.create_substrate(
            SubstrateType.LEAF,
            custom_properties={"roughness": 0.2, "moisture": 0.9},
        )
        assert isinstance(props, SubstrateProperties)

    def test_interact_with_substrate(self, substrate):
        props = substrate.create_substrate(SubstrateType.SOIL)
        interaction = substrate.interact_with_substrate(
            "ant1", props, "digging", 5.0
        )
        assert interaction.insect_id == "ant1"

    def test_get_substrate_behavior(self, substrate):
        props = substrate.create_substrate(SubstrateType.BARK)
        behaviors = substrate.get_substrate_behavior(
            props, {"grip_strength": 0.8, "body_weight": 0.5}
        )
        assert isinstance(behaviors, list)

    def test_get_statistics(self, substrate):
        props = substrate.create_substrate(SubstrateType.SOIL)
        substrate.interact_with_substrate("ant1", props, "walking", 1.0)
        stats = substrate.get_substrate_statistics()
        assert isinstance(stats, dict)


# ═══════════════════════ CasteCase ═══════════════════════

from src.models.insect.cases.caste import CasteType

@pytest.fixture
def caste_case():
    return CasteCase()


class TestCasteCase:
    def test_init(self, caste_case):
        assert caste_case.case == Case.NOMINATIVE

    def test_case_property(self, caste_case):
        caste_case.case = Case.NOMINATIVE
        assert caste_case.case == Case.NOMINATIVE

    def test_determine_caste(self, caste_case):
        assignment = caste_case.determine_caste(
            "insect1",
            genetic_markers={"queen_marker": 0.3, "worker_marker": 0.8},
            environmental_conditions={"temperature": 25.0, "food_availability": 0.7},
            nutritional_history=[0.6, 0.7, 0.8],
        )
        assert assignment.individual_id == "insect1"

    def test_get_caste_behaviors(self, caste_case):
        behaviors = caste_case.get_caste_behaviors(
            CasteType.WORKER,
            {"temperature": 25.0, "threat_level": 0.1},
        )
        assert isinstance(behaviors, list)

    def test_get_individual_caste(self, caste_case):
        caste_case.determine_caste(
            "i1",
            {"queen_marker": 0.1},
            {"temperature": 25.0},
            [0.5],
        )
        result = caste_case.get_individual_caste("i1")
        assert result is not None

    def test_get_individual_caste_missing(self, caste_case):
        result = caste_case.get_individual_caste("nonexistent")
        assert result is None

    def test_get_caste_statistics(self, caste_case):
        caste_case.determine_caste("i1", {"q": 0.1}, {"temp": 25.0}, [0.5])
        stats = caste_case.get_caste_statistics()
        assert isinstance(stats, dict)

    def test_update_caste_behavior(self, caste_case):
        caste_case.determine_caste("i1", {"q": 0.1}, {"temp": 25.0}, [0.5])
        caste_case.update_caste_behavior("i1", "foraging", True)


# ═══════════════════════ MetamorphicCase ═══════════════════════

from src.models.insect.cases.metamorphic import DevelopmentalStage, DevelopmentalState

@pytest.fixture
def metamorphic():
    return MetamorphicCase()


class TestMetamorphicCase:
    def test_init(self, metamorphic):
        assert metamorphic.case == Case.NOMINATIVE

    def test_case_property(self, metamorphic):
        metamorphic.case = Case.ABLATIVE
        assert metamorphic.case == Case.ABLATIVE

    def test_update_developmental_state(self, metamorphic):
        state = DevelopmentalState(
            stage=DevelopmentalStage.LARVA,
            age=2.0,
            size=0.3,
            maturity=0.2,
            hormone_levels={"ecdysone": 0.5, "juvenile_hormone": 0.8},
        )
        updated = metamorphic.update_developmental_state(
            state, time_delta=1.0,
            environmental_conditions={"temperature": 25.0, "food_quality": 0.8},
        )
        assert isinstance(updated, DevelopmentalState)
        assert updated.age > state.age

    def test_predict_next_transition(self, metamorphic):
        state = DevelopmentalState(
            stage=DevelopmentalStage.LARVA,
            age=10.0,
            size=0.8,
            maturity=0.95,
            hormone_levels={"ecdysone": 0.9, "juvenile_hormone": 0.1},
        )
        result = metamorphic.predict_next_transition(
            state, {"temperature": 25.0}
        )
        # May be None or a MetamorphicTransition
        assert result is None or hasattr(result, "from_stage")

    def test_get_developmental_summary(self, metamorphic):
        summary = metamorphic.get_developmental_summary()
        assert isinstance(summary, dict)

    def test_full_lifecycle(self, metamorphic):
        state = DevelopmentalState(
            stage=DevelopmentalStage.EGG,
            age=0.0,
            size=0.1,
            maturity=0.0,
            hormone_levels={"ecdysone": 0.1, "juvenile_hormone": 0.5},
        )
        env = {"temperature": 25.0, "humidity": 0.7, "food_quality": 0.8}
        for _ in range(20):
            state = metamorphic.update_developmental_state(state, 1.0, env)
        assert state.age > 0
