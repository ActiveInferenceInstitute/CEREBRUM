"""
Tests for src/cases/animal_cases.py

Tests the AnimalCaseManager with real AnimalAgent instances,
verifying transformations (goal seeker, explorer, follower),
formations (V, line, circle), and role swapping.
"""

import pytest
import numpy as np

from src.core.model import Case
from src.cases.animal_cases import AnimalCaseManager
from src.examples.animal_agent import AnimalAgent


@pytest.fixture
def animal_manager():
    """Create an AnimalCaseManager instance."""
    return AnimalCaseManager()


@pytest.fixture
def animal():
    """Create a basic animal agent."""
    return AnimalAgent(name="TestAnimal", position=np.array([0.0, 0.0]))


@pytest.fixture
def animals():
    """Create a list of animal agents."""
    return [
        AnimalAgent(name=f"Animal_{i}", position=np.array([float(i), 0.0]))
        for i in range(4)
    ]


class TestAnimalCaseManagerInit:
    """Test AnimalCaseManager initialization."""

    def test_inherits_case_manager(self, animal_manager):
        from src.cases.case_manager import CaseManager
        assert isinstance(animal_manager, CaseManager)


class TestTransformToGoalSeeker:
    """Test goal seeker transformation."""

    def test_returns_animal(self, animal_manager, animal):
        animal_manager.register_model(animal)
        result = animal_manager.transform_to_goal_seeker(animal)
        assert result is animal

    def test_sets_nominative_case(self, animal_manager, animal):
        animal_manager.register_model(animal)
        animal_manager.transform_to_goal_seeker(animal)
        assert animal.case == Case.NOMINATIVE


class TestTransformToExplorer:
    """Test explorer transformation."""

    def test_returns_animal(self, animal_manager, animal):
        animal_manager.register_model(animal)
        result = animal_manager.transform_to_explorer(animal)
        assert result is animal

    def test_sets_nominative_case_with_exploration(self, animal_manager, animal):
        """transform_to_explorer uses NOMINATIVE case with exploration parameters."""
        animal_manager.register_model(animal)
        animal_manager.transform_to_explorer(animal)
        assert animal.case == Case.NOMINATIVE
        assert getattr(animal, '_exploration_mode', False) is True


class TestTransformToFollower:
    """Test follower transformation."""

    def test_returns_follower(self, animal_manager, animals):
        for a in animals:
            animal_manager.register_model(a)
        follower, leader = animals[0], animals[1]
        result = animal_manager.transform_to_follower(follower, leader)
        assert result is follower

    def test_sets_dative_case(self, animal_manager, animals):
        for a in animals:
            animal_manager.register_model(a)
        follower, leader = animals[0], animals[1]
        animal_manager.transform_to_follower(follower, leader)
        assert follower.case == Case.DATIVE


class TestFormations:
    """Test formation creation."""

    def test_v_formation_returns_dict(self, animal_manager, animals):
        for a in animals:
            animal_manager.register_model(a)
        result = animal_manager.create_v_formation(animals)
        assert isinstance(result, dict)

    def test_line_formation_returns_dict(self, animal_manager, animals):
        for a in animals:
            animal_manager.register_model(a)
        result = animal_manager.create_line_formation(animals)
        assert isinstance(result, dict)

    def test_circle_formation_returns_dict(self, animal_manager, animals):
        for a in animals:
            animal_manager.register_model(a)
        result = animal_manager.create_circle_formation(animals)
        assert isinstance(result, dict)


class TestSwapAnimalRoles:
    """Test role swapping between animals."""

    def test_swaps_roles(self, animal_manager, animals):
        for a in animals:
            animal_manager.register_model(a)
        a1, a2 = animals[0], animals[1]
        animal_manager.transform_to_goal_seeker(a1)  # NOMINATIVE
        animal_manager.transform_to_follower(a2, a1)  # DATIVE
        
        original_a1_case = a1.case
        original_a2_case = a2.case
        
        result = animal_manager.swap_animal_roles(a1, a2)
        
        assert isinstance(result, tuple)
        assert len(result) == 2
        assert a1.case == original_a2_case
        assert a2.case == original_a1_case
