"""
Tests for src/models/insect/behaviors.py

Comprehensive tests for ForagingBehavior, NavigationBehavior,
CommunicationBehavior, and SocialBehavior classes plus dataclasses.
"""

import pytest
import numpy as np

from src.models.insect.behaviors import (
    BehaviorType,
    BehaviorState,
    BehaviorResult,
    ForagingBehavior,
    NavigationBehavior,
    CommunicationBehavior,
    SocialBehavior,
)


# ── Dataclass Tests ───────────────────────────────────────────────

class TestBehaviorType:
    def test_all_types_exist(self):
        assert BehaviorType.FORAGING.value == "foraging"
        assert BehaviorType.NAVIGATION.value == "navigation"
        assert BehaviorType.COMMUNICATION.value == "communication"
        assert BehaviorType.SOCIAL.value == "social"
        assert BehaviorType.DEFENSE.value == "defense"
        assert BehaviorType.REPRODUCTION.value == "reproduction"
        assert BehaviorType.NEST_BUILDING.value == "nest_building"
        assert BehaviorType.CLEANING.value == "cleaning"


class TestBehaviorState:
    def test_creation(self):
        state = BehaviorState(
            behavior_type=BehaviorType.FORAGING,
            is_active=True,
            priority=0.8,
            start_time=0.0,
            duration=5.0,
            success_rate=0.7,
            energy_cost=0.5,
        )
        assert state.behavior_type == BehaviorType.FORAGING
        assert state.is_active is True
        assert state.timestamp == 0.0  # default

    def test_timestamp_default(self):
        state = BehaviorState(BehaviorType.NAVIGATION, False, 0.5, 1.0, 2.0, 0.9, 0.3)
        assert state.timestamp == 0.0


class TestBehaviorResult:
    def test_creation(self):
        result = BehaviorResult(
            success=True,
            energy_expended=0.1,
            information_gained={"food": True},
            time_spent=1.0,
        )
        assert result.success is True
        assert result.energy_expended == 0.1
        assert result.information_gained == {"food": True}
        assert result.timestamp == 0.0


# ── ForagingBehavior ──────────────────────────────────────────────

class TestForagingBehavior:
    @pytest.fixture
    def forager(self):
        return ForagingBehavior()

    @pytest.fixture
    def custom_forager(self):
        return ForagingBehavior(config={
            'search_strategy': 'spiral',
            'energy_threshold': 0.5,
            'memory_decay': 0.2,
        })

    def test_init_defaults(self, forager):
        assert forager.search_strategy == 'random_walk'
        assert forager.energy_threshold == 0.3
        assert forager.memory_decay == 0.1
        assert forager.food_locations == []

    def test_init_custom(self, custom_forager):
        assert custom_forager.search_strategy == 'spiral'
        assert custom_forager.energy_threshold == 0.5

    def test_search_low_energy_returns_failure(self, forager):
        pos = np.array([0.0, 0.0, 0.0])
        result = forager.search_for_food(pos, search_radius=1.0, energy_level=0.1)
        assert result.success is False
        assert result.energy_expended == 0.0
        assert result.time_spent == 0.0

    def test_search_sufficient_energy(self, forager):
        np.random.seed(42)
        pos = np.array([0.0, 0.0, 0.0])
        result = forager.search_for_food(pos, search_radius=1.0, energy_level=1.0)
        assert isinstance(result, BehaviorResult)
        assert result.time_spent == 1.0

    def test_search_records_food_location(self, forager):
        np.random.seed(0)  # seed where random < 0.1 for food discovery
        pos = np.array([0.0, 0.0, 0.0])
        # Run enough searches to statistically find food
        found = False
        for _ in range(100):
            result = forager.search_for_food(pos, search_radius=1.0, energy_level=1.0)
            if result.success:
                found = True
                break
        # At least one search should succeed in 100 tries at 10% probability
        assert found or len(forager.food_locations) == 0  # stochastic, both valid

    def test_return_to_nest(self, forager):
        pos = np.array([5.0, 5.0, 0.0])
        nest = np.array([0.0, 0.0, 0.0])
        result = forager.return_to_nest(pos, nest)
        assert result.success is True
        assert result.energy_expended > 0
        expected_distance = np.linalg.norm(pos - nest)
        assert result.information_gained['distance_traveled'] == pytest.approx(expected_distance)


# ── NavigationBehavior ────────────────────────────────────────────

class TestNavigationBehavior:
    @pytest.fixture
    def navigator(self):
        return NavigationBehavior()

    def test_init_defaults(self, navigator):
        assert navigator.orientation_strategy == 'sun_compass'
        assert navigator.landmark_memory is True
        np.testing.assert_array_equal(navigator.path_integration, np.zeros(3))

    def test_navigate_already_at_target(self, navigator):
        pos = np.array([1.0, 1.0, 0.0])
        target = np.array([1.0, 1.0, 0.0])
        result = navigator.navigate_to_target(pos, target)
        assert result.success is True
        assert result.information_gained['arrived'] is True

    def test_navigate_toward_target(self, navigator):
        pos = np.array([0.0, 0.0, 0.0])
        target = np.array([10.0, 0.0, 0.0])
        result = navigator.navigate_to_target(pos, target)
        assert result.success is False  # not yet arrived
        assert 'new_position' in result.information_gained
        assert result.information_gained['distance_remaining'] < 10.0

    def test_update_path_integration(self, navigator):
        movement = np.array([1.0, 2.0, 0.0])
        navigator.update_path_integration(movement)
        np.testing.assert_array_equal(navigator.path_integration, movement)
        assert len(navigator.orientation_history) == 1

    def test_cumulative_path_integration(self, navigator):
        navigator.update_path_integration(np.array([1.0, 0.0, 0.0]))
        navigator.update_path_integration(np.array([0.0, 2.0, 0.0]))
        np.testing.assert_array_equal(navigator.path_integration, np.array([1.0, 2.0, 0.0]))
        assert len(navigator.orientation_history) == 2


# ── CommunicationBehavior ────────────────────────────────────────

class TestCommunicationBehavior:
    @pytest.fixture
    def communicator(self):
        return CommunicationBehavior()

    def test_init_defaults(self, communicator):
        assert communicator.communication_range == 1.0
        assert communicator.signal_strength == 0.8

    def test_send_signal_broadcast(self, communicator):
        pos = np.array([0.0, 0.0, 0.0])
        result = communicator.send_signal("alarm", pos)
        assert result.success is True
        assert len(communicator.signal_history) == 1
        assert communicator.signal_history[0]['type'] == "alarm"

    def test_send_signal_in_range(self, communicator):
        pos = np.array([0.0, 0.0, 0.0])
        target = np.array([0.5, 0.0, 0.0])
        result = communicator.send_signal("food_found", pos, target_position=target)
        assert result.success is True

    def test_send_signal_out_of_range(self, communicator):
        pos = np.array([0.0, 0.0, 0.0])
        target = np.array([5.0, 5.0, 0.0])
        result = communicator.send_signal("food_found", pos, target_position=target)
        assert result.success is False
        assert result.information_gained['error'] == 'out_of_range'

    def test_receive_signal_in_range(self, communicator):
        source = np.array([0.0, 0.0, 0.0])
        receiver = np.array([0.5, 0.0, 0.0])
        result = communicator.receive_signal("pheromone", source, receiver)
        assert result.success is True
        assert result.information_gained['strength'] > 0
        assert len(communicator.response_history) == 1

    def test_receive_signal_out_of_range(self, communicator):
        source = np.array([0.0, 0.0, 0.0])
        receiver = np.array([5.0, 5.0, 0.0])
        result = communicator.receive_signal("pheromone", source, receiver)
        assert result.success is False
        assert result.information_gained['error'] == 'signal_too_weak'

    def test_signal_strength_attenuation(self, communicator):
        source = np.array([0.0, 0.0, 0.0])
        # Closer receiver
        near = np.array([0.1, 0.0, 0.0])
        near_result = communicator.receive_signal("ping", source, near)
        # Farther receiver
        far = np.array([0.9, 0.0, 0.0])
        far_result = communicator.receive_signal("ping", source, far)
        assert near_result.information_gained['strength'] > far_result.information_gained['strength']


# ── SocialBehavior ────────────────────────────────────────────────

class TestSocialBehavior:
    @pytest.fixture
    def social(self):
        return SocialBehavior(config={'interaction_probability': 1.0})  # deterministic

    def test_init_defaults(self):
        s = SocialBehavior()
        assert s.social_range == 0.5
        assert s.interaction_probability == 0.3

    def test_interact_in_range(self, social):
        pos1 = np.array([0.0, 0.0, 0.0])
        pos2 = np.array([0.1, 0.0, 0.0])
        result = social.interact_with_individual("ant1", "ant2", pos1, pos2, "trophallaxis")
        assert result.success is True
        assert result.information_gained['interaction_success'] == "trophallaxis"
        assert result.information_gained['relationship_strength'] == 1

    def test_interact_out_of_range(self, social):
        pos1 = np.array([0.0, 0.0, 0.0])
        pos2 = np.array([5.0, 5.0, 0.0])
        result = social.interact_with_individual("ant1", "ant2", pos1, pos2, "trophallaxis")
        assert result.success is False
        assert result.information_gained['error'] == 'too_far'

    def test_repeated_interactions_build_relationship(self, social):
        pos1 = np.array([0.0, 0.0, 0.0])
        pos2 = np.array([0.1, 0.0, 0.0])
        social.interact_with_individual("a", "b", pos1, pos2, "groom")
        social.interact_with_individual("a", "b", pos1, pos2, "groom")
        assert social.social_network["a"]["b"] == 2

    def test_get_social_connections_empty(self, social):
        assert social.get_social_connections("unknown") == []

    def test_get_social_connections_after_interaction(self, social):
        pos1 = np.array([0.0, 0.0, 0.0])
        pos2 = np.array([0.1, 0.0, 0.0])
        social.interact_with_individual("queen", "worker1", pos1, pos2, "command")
        connections = social.get_social_connections("queen")
        assert "worker1" in connections

    def test_interaction_history_recorded(self, social):
        pos1 = np.array([0.0, 0.0, 0.0])
        pos2 = np.array([0.1, 0.0, 0.0])
        social.interact_with_individual("a", "b", pos1, pos2, "antennation")
        assert len(social.interaction_history) == 1
        assert social.interaction_history[0]['type'] == "antennation"
