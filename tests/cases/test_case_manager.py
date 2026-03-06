#!/usr/bin/env python3
"""
Tests for case_manager module.
Tests the centralized case transformation manager for CEREBRUM.
"""

import pytest
import sys
from pathlib import Path

# Ensure src is in path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from src.cases.case_manager import CaseManager
from src.models.base import Model, Case


class TestCaseManagerInit:
    """Tests for CaseManager initialization."""
    
    def test_initialization(self):
        """Test CaseManager initializes with empty state."""
        manager = CaseManager()
        assert manager is not None
        assert manager.tracked_models == {}
        assert manager.case_relationships == []
        assert manager.default_case == Case.NOMINATIVE
    
    def test_has_case_handlers(self):
        """Test CaseManager has case handlers registered."""
        manager = CaseManager()
        assert Case.NOMINATIVE in manager.case_handlers
        assert Case.ACCUSATIVE in manager.case_handlers


class TestModelRegistration:
    """Tests for model registration functionality."""
    
    @pytest.fixture
    def manager(self):
        """Create a fresh CaseManager."""
        return CaseManager()
    
    @pytest.fixture
    def sample_model(self):
        """Create a sample model."""
        return Model(name="test_model", case=Case.NOMINATIVE)
    
    def test_register_model(self, manager, sample_model):
        """Test registering a model."""
        manager.register_model(sample_model)
        assert sample_model.id in manager.tracked_models
        assert manager.tracked_models[sample_model.id] == sample_model
    
    def test_register_multiple_models(self, manager):
        """Test registering multiple models."""
        model1 = Model(name="model1")
        model2 = Model(name="model2")
        manager.register_model(model1)
        manager.register_model(model2)
        assert len(manager.tracked_models) == 2
    
    def test_unregister_model(self, manager, sample_model):
        """Test unregistering a model."""
        manager.register_model(sample_model)
        result = manager.unregister_model(sample_model)
        assert result is True
        assert sample_model.id not in manager.tracked_models
    
    def test_unregister_nonexistent_model(self, manager, sample_model):
        """Test unregistering a model that's not registered."""
        result = manager.unregister_model(sample_model)
        assert result is False


class TestCaseTransformation:
    """Tests for case transformation functionality."""
    
    @pytest.fixture
    def manager(self):
        """Create a fresh CaseManager."""
        return CaseManager()
    
    @pytest.fixture
    def sample_model(self):
        """Create a sample model."""
        return Model(name="test_model", case=Case.NOMINATIVE)
    
    def test_transform_to_accusative(self, manager, sample_model):
        """Test transforming model to accusative case."""
        result = manager.transform_case(sample_model, Case.ACCUSATIVE)
        assert result is not None
        assert result.case == Case.ACCUSATIVE
    
    def test_transform_to_unhandled_case(self, manager, sample_model):
        """Test transforming to case without handler falls back to basic."""
        result = manager.transform_case(sample_model, Case.LOCATIVE)
        assert result.case == Case.LOCATIVE


class TestGetModelsByCase:
    """Tests for filtering models by case."""
    
    @pytest.fixture
    def manager_with_models(self):
        """Create a manager with registered models."""
        manager = CaseManager()
        model1 = Model(name="nom1", case=Case.NOMINATIVE)
        model2 = Model(name="nom2", case=Case.NOMINATIVE)
        model3 = Model(name="acc1", case=Case.ACCUSATIVE)
        manager.register_model(model1)
        manager.register_model(model2)
        manager.register_model(model3)
        return manager
    
    def test_get_nominative_models(self, manager_with_models):
        """Test getting models with nominative case."""
        models = manager_with_models.get_models_by_case(Case.NOMINATIVE)
        assert len(models) == 2
        assert all(m.case == Case.NOMINATIVE for m in models)
    
    def test_get_accusative_models(self, manager_with_models):
        """Test getting models with accusative case."""
        models = manager_with_models.get_models_by_case(Case.ACCUSATIVE)
        assert len(models) == 1
    
    def test_get_empty_case(self, manager_with_models):
        """Test getting models with case that has none."""
        models = manager_with_models.get_models_by_case(Case.DATIVE)
        assert len(models) == 0


class TestCalculateFreeEnergy:
    """Tests for free energy calculation."""
    
    @pytest.fixture
    def manager(self):
        """Create a fresh CaseManager."""
        return CaseManager()
    
    def test_calculate_free_energy_default(self, manager):
        """Test default free energy calculation returns 1.0."""
        model = Model(name="test_model")
        result = manager.calculate_free_energy(model)
        assert isinstance(result, float)
        assert result == 1.0  # Default fallback
