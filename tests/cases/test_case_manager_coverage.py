"""
Tests for src/cases/case_manager.py

Comprehensive tests for CaseManager covering model registration,
case transformation, relationship creation, process_update,
model queries, and free energy calculation.
"""

import pytest

from src.core.model import Model, Case
from src.cases.case_manager import CaseManager


@pytest.fixture
def mgr():
    return CaseManager()


@pytest.fixture
def model_a():
    return Model(name="ModelA")


@pytest.fixture
def model_b():
    return Model(name="ModelB")


# ── Initialization ───────────────────────────────────────────────

class TestCaseManagerInit:
    def test_has_all_handlers(self, mgr):
        for case in Case:
            assert case in mgr.case_handlers

    def test_default_case_is_nominative(self, mgr):
        assert mgr.default_case == Case.NOMINATIVE

    def test_empty_tracked_models(self, mgr):
        assert len(mgr.tracked_models) == 0


# ── Registration ──────────────────────────────────────────────────

class TestRegistration:
    def test_register_model(self, mgr, model_a):
        mgr.register_model(model_a)
        assert model_a.id in mgr.tracked_models
        assert mgr.tracked_models[model_a.id] is model_a

    def test_unregister_model(self, mgr, model_a):
        mgr.register_model(model_a)
        assert mgr.unregister_model(model_a) is True
        assert model_a.id not in mgr.tracked_models

    def test_unregister_nonexistent(self, mgr, model_a):
        assert mgr.unregister_model(model_a) is False

    def test_unregister_removes_relationships(self, mgr, model_a, model_b):
        mgr.register_model(model_a)
        mgr.register_model(model_b)
        mgr.create_relationship(model_a, model_b, "generates")
        assert len(mgr.case_relationships) == 1
        mgr.unregister_model(model_a)
        assert len(mgr.case_relationships) == 0


# ── Case Transformation ──────────────────────────────────────────

class TestTransformCase:
    def test_transform_to_nominative(self, mgr, model_a):
        result = mgr.transform_case(model_a, Case.NOMINATIVE)
        assert result.case == Case.NOMINATIVE

    def test_transform_to_accusative(self, mgr, model_a):
        result = mgr.transform_case(model_a, Case.ACCUSATIVE)
        assert result.case == Case.ACCUSATIVE

    def test_transform_all_cases(self, mgr):
        for case in Case:
            model = Model(name=f"test_{case.value}")
            result = mgr.transform_case(model, case)
            assert result.case == case


# ── Relationships ─────────────────────────────────────────────────

class TestRelationships:
    def test_create_generates(self, mgr, model_a, model_b):
        src, tgt = mgr.create_relationship(model_a, model_b, "generates")
        assert len(mgr.case_relationships) == 1

    def test_create_updates(self, mgr, model_a, model_b):
        mgr.create_relationship(model_a, model_b, "updates")
        assert len(mgr.case_relationships) == 1

    def test_invalid_relationship(self, mgr, model_a, model_b):
        with pytest.raises(ValueError):
            mgr.create_relationship(model_a, model_b, "teleports")

    def test_get_related_models(self, mgr, model_a, model_b):
        mgr.create_relationship(model_a, model_b, "generates")
        related = mgr.get_related_models(model_a)
        assert len(related) == 1
        assert related[0][0] is model_b

    def test_get_related_models_inverse(self, mgr, model_a, model_b):
        mgr.create_relationship(model_a, model_b, "generates")
        related = mgr.get_related_models(model_b)
        assert len(related) == 1
        assert "inverse_" in related[0][1]

    def test_get_related_models_filtered(self, mgr, model_a, model_b):
        mgr.create_relationship(model_a, model_b, "generates")
        related = mgr.get_related_models(model_a, relationship_type="generates")
        assert len(related) == 1
        related_none = mgr.get_related_models(model_a, relationship_type="updates")
        assert len(related_none) == 0


# ── Process Update ────────────────────────────────────────────────

class TestProcessUpdate:
    def test_process_update_nominative(self, mgr, model_a):
        model_a.case = Case.NOMINATIVE
        result = mgr.process_update(model_a, {"value": 1.0})
        assert isinstance(result, dict)


# ── Model Queries ─────────────────────────────────────────────────

class TestModelQueries:
    def test_get_models_by_case(self, mgr, model_a, model_b):
        mgr.register_model(model_a)
        mgr.register_model(model_b)
        model_a.case = Case.NOMINATIVE
        model_b.case = Case.DATIVE
        nom_models = mgr.get_models_by_case(Case.NOMINATIVE)
        assert len(nom_models) == 1
        assert nom_models[0] is model_a

    def test_get_models_by_case_empty(self, mgr):
        result = mgr.get_models_by_case(Case.VOCATIVE)
        assert result == []


# ── Free Energy ───────────────────────────────────────────────────

class TestFreeEnergy:
    def test_calculate_free_energy(self, mgr, model_a):
        model_a.case = Case.NOMINATIVE
        fe = mgr.calculate_free_energy(model_a)
        assert isinstance(fe, (int, float))
