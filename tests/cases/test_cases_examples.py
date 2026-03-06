"""
Tests for src/cases/examples.py

Tests the example functions that demonstrate the case system:
basic_case_example, animal_formation_example, case_update_example,
and relationship_example.
"""


from src.examples.case_examples import (
    basic_case_example,
    animal_formation_example,
    case_update_example,
    relationship_example,
)
from src.core.model import Model, Case


class TestBasicCaseExample:
    """Test the basic_case_example function."""

    def test_returns_model_and_manager(self):
        model, manager = basic_case_example()
        assert isinstance(model, Model)
        assert model.name == "TestModel"

    def test_model_has_parameters(self):
        model, _ = basic_case_example()
        assert "speed" in model.parameters
        assert "size" in model.parameters


class TestAnimalFormationExample:
    """Test the animal_formation_example function."""

    def test_returns_animals_and_manager(self):
        animals, manager = animal_formation_example()
        assert isinstance(animals, list)
        assert len(animals) == 4

    def test_all_animals_have_names(self):
        animals, _ = animal_formation_example()
        names = [a.name for a in animals]
        assert "Leader" in names
        assert "Follower1" in names


class TestCaseUpdateExample:
    """Test the case_update_example function."""

    def test_returns_three_models(self):
        active, passive, recipient = case_update_example()
        assert isinstance(active, Model)
        assert isinstance(passive, Model)
        assert isinstance(recipient, Model)

    def test_models_have_correct_cases(self):
        active, passive, recipient = case_update_example()
        assert active.case == Case.NOMINATIVE
        assert passive.case == Case.ACCUSATIVE
        assert recipient.case == Case.DATIVE


class TestRelationshipExample:
    """Test the relationship_example function."""

    def test_returns_manager(self):
        manager = relationship_example()
        from src.cases.case_manager import CaseManager
        assert isinstance(manager, CaseManager)
