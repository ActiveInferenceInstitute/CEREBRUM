"""
Tests for src/models/insect/species.py

Tests HoneybeeModel, AntModel, FruitFlyModel species-specific
implementations and their unique behaviors.
"""

import pytest
import numpy as np

from src.models.insect.species import (
    SpeciesCharacteristics,
    HoneybeeModel,
    AntModel,
    FruitFlyModel,
)


# ── SpeciesCharacteristics ────────────────────────────────────────

class TestSpeciesCharacteristics:
    def test_creation(self):
        chars = SpeciesCharacteristics(
            name="Test", size=0.5, lifespan=30.0,
            social_structure="solitary", foraging_range=1.0,
            communication_modes=["visual"], neural_complexity=0.5,
            learning_capacity=0.5, specialization=["foraging"]
        )
        assert chars.name == "Test"
        assert chars.size == 0.5


# ── HoneybeeModel ────────────────────────────────────────────────

class TestHoneybeeModel:
    @pytest.fixture
    def bee(self):
        b = HoneybeeModel()
        b.position = np.zeros(3)
        return b

    def test_init(self, bee):
        assert bee.species == "Honeybee"
        assert bee.characteristics.name == "Apis mellifera"
        assert bee.characteristics.social_structure == "eusocial"
        assert bee.characteristics.neural_complexity == 0.9

    def test_has_neural_structures(self, bee):
        assert bee.mushroom_body is not None
        assert bee.central_complex is not None
        assert bee.antennal_lobe is not None
        assert bee.optic_lobe is not None

    def test_has_case_objects(self, bee):
        assert bee.pheromonal_case is not None
        assert bee.swarm_case is not None
        assert bee.caste_case is not None

    def test_has_behaviors(self, bee):
        assert bee.foraging_behavior is not None
        assert bee.navigation_behavior is not None
        assert bee.communication_behavior is not None
        assert bee.social_behavior is not None

    def test_perform_waggle_dance(self, bee):
        food_loc = np.array([5.0, 5.0, 0.0])
        result = bee.perform_waggle_dance(food_loc, food_quality=0.9)
        assert result['success'] is True
        assert result['quality'] == 0.9
        assert result['distance'] > 0
        assert 'waggle_duration' in result

    def test_decode_waggle_dance(self, bee):
        food_loc = np.array([5.0, 5.0, 0.0])
        dance_info = bee.perform_waggle_dance(food_loc, food_quality=0.8)
        decoded = bee.decode_waggle_dance(dance_info)
        # Should decode back close to original food location  
        assert decoded.shape == (3,)
        np.testing.assert_allclose(decoded[:2], food_loc[:2], atol=0.5)

    def test_waggle_dance_roundtrip(self, bee):
        food = np.array([3.0, 4.0, 0.0])
        dance = bee.perform_waggle_dance(food, food_quality=0.95)
        decoded = bee.decode_waggle_dance(dance)
        # Distance should approximately match
        expected_dist = np.linalg.norm(food - bee.position)
        actual_dist = np.linalg.norm(decoded - bee.position)
        assert abs(expected_dist - actual_dist) < 1.0


# ── AntModel ──────────────────────────────────────────────────────

class TestAntModel:
    @pytest.fixture
    def ant(self):
        a = AntModel()
        a.position = np.zeros(3)
        return a

    def test_init(self, ant):
        assert ant.species == "Ant"
        assert ant.characteristics.name == "Formicidae"
        assert ant.characteristics.social_structure == "eusocial"

    def test_has_unique_cases(self, ant):
        assert ant.stigmergic_case is not None
        assert ant.substrate_case is not None

    def test_lay_trail_pheromone(self, ant):
        pos = np.array([1.0, 2.0, 0.0])
        result = ant.lay_trail_pheromone(pos, concentration=0.8)
        assert result['success'] is True
        assert result['pheromone_type'] == 'trail'
        assert result['concentration'] == 0.8

    def test_follow_trail_strong(self, ant):
        trail_pos = np.array([1.0, 0.0, 0.0])
        old_pos = ant.position.copy()
        result = ant.follow_trail(trail_pos, trail_concentration=0.5)
        assert result is True
        # Position should have changed toward trail
        assert not np.array_equal(ant.position, old_pos)

    def test_follow_trail_weak(self, ant):
        trail_pos = np.array([1.0, 0.0, 0.0])
        result = ant.follow_trail(trail_pos, trail_concentration=0.05)
        assert result is False  # Below detection threshold


# ── FruitFlyModel ─────────────────────────────────────────────────

class TestFruitFlyModel:
    @pytest.fixture
    def fly(self):
        f = FruitFlyModel()
        f.position = np.zeros(3)
        return f

    def test_init(self, fly):
        assert fly.species == "FruitFly"
        assert fly.characteristics.name == "Drosophila melanogaster"
        assert fly.characteristics.social_structure == "solitary"

    def test_has_metamorphic_case(self, fly):
        assert fly.metamorphic_case is not None

    def test_courtship_song_close(self, fly):
        target = fly.position + np.array([0.1, 0.0, 0.0])
        result = fly.perform_courtship_song(target)
        assert result['success'] is True
        assert result['song_type'] == 'courtship'
        assert result['frequency'] == 150

    def test_courtship_song_too_far(self, fly):
        target = fly.position + np.array([5.0, 5.0, 0.0])
        result = fly.perform_courtship_song(target)
        assert result['success'] is False

    def test_detect_fruit_close_and_ripe(self, fly):
        fruit_pos = fly.position + np.array([0.1, 0.0, 0.0])
        result = fly.detect_fruit(fruit_pos, fruit_ripeness=0.9)
        assert result is True

    def test_detect_fruit_unripe(self, fly):
        fruit_pos = fly.position + np.array([0.1, 0.0, 0.0])
        result = fly.detect_fruit(fruit_pos, fruit_ripeness=0.1)
        assert result is False

    def test_detect_fruit_too_far(self, fly):
        fruit_pos = fly.position + np.array([5.0, 5.0, 0.0])
        result = fly.detect_fruit(fruit_pos, fruit_ripeness=0.9)
        assert result is False
