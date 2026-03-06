#!/usr/bin/env python3
"""
Tests for model_registry module.
Tests the centralized model management system for CEREBRUM.
"""

import pytest
import os
import tempfile
import shutil
import sys
from pathlib import Path
from datetime import datetime

# Ensure src is in path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from src.core.model_registry import (
    ModelMetadata,
    ModelRegistry,
    get_global_registry
)
from src.models.base import Model, Case


class TestModelMetadata:
    """Tests for ModelMetadata dataclass."""
    
    def test_creation(self):
        """Test creating ModelMetadata."""
        now = datetime.now()
        metadata = ModelMetadata(
            model_id="test_model",
            model_type="LinearRegression",
            case=Case.NOMINATIVE,
            created_at=now,
            last_accessed=now
        )
        assert metadata.model_id == "test_model"
        assert metadata.model_type == "LinearRegression"
        assert metadata.case == Case.NOMINATIVE
        assert metadata.is_fitted is False
    
    def test_to_dict(self):
        """Test converting ModelMetadata to dictionary."""
        now = datetime.now()
        metadata = ModelMetadata(
            model_id="test_model",
            model_type="LinearRegression",
            case=Case.NOMINATIVE,
            created_at=now,
            last_accessed=now,
            tags=["test", "example"]
        )
        result = metadata.to_dict()
        assert isinstance(result, dict)
        assert result["model_id"] == "test_model"
        assert result["tags"] == ["test", "example"]
    
    def test_from_dict(self):
        """Test creating ModelMetadata from dictionary."""
        data = {
            "model_id": "test_model",
            "model_type": "LinearRegression",
            "case": "NOM",  # Case enum uses abbreviated values
            "created_at": datetime.now().isoformat(),
            "last_accessed": datetime.now().isoformat(),
            "is_fitted": True,
            "tags": ["test"],
            "description": "Test model"
        }
        metadata = ModelMetadata.from_dict(data)
        assert metadata.model_id == "test_model"
        assert metadata.is_fitted is True


class TestModelRegistry:
    """Tests for ModelRegistry class."""
    
    @pytest.fixture
    def temp_registry_dir(self):
        """Create a temporary directory for registry tests."""
        temp_dir = tempfile.mkdtemp()
        yield temp_dir
        shutil.rmtree(temp_dir, ignore_errors=True)
    
    @pytest.fixture
    def registry(self, temp_registry_dir):
        """Create a fresh ModelRegistry for testing."""
        return ModelRegistry(registry_path=temp_registry_dir)
    
    @pytest.fixture
    def sample_model(self):
        """Create a sample model for testing."""
        return Model(
            name="test_model",
            case=Case.NOMINATIVE
        )
    
    def test_initialization(self, temp_registry_dir):
        """Test ModelRegistry initialization."""
        registry = ModelRegistry(registry_path=temp_registry_dir)
        assert registry is not None
        assert os.path.exists(temp_registry_dir)
    
    def test_register_model(self, registry, sample_model):
        """Test registering a model."""
        model_id = registry.register_model(
            model=sample_model,
            tags=["test"],
            description="Test model",
            persist=False  # Don't persist to disk for this test
        )
        assert model_id == "test_model"
        assert "test_model" in registry._models
    
    def test_register_model_custom_id(self, registry, sample_model):
        """Test registering a model with custom ID."""
        model_id = registry.register_model(
            model=sample_model,
            model_id="custom_id",
            persist=False
        )
        assert model_id == "custom_id"
        assert "custom_id" in registry._models
    
    def test_get_model(self, registry, sample_model):
        """Test retrieving a model."""
        registry.register_model(sample_model, persist=False)
        retrieved = registry.get_model("test_model")
        assert retrieved is not None
        assert retrieved.name == "test_model"
    
    def test_get_model_not_found(self, registry):
        """Test getting a non-existent model."""
        result = registry.get_model("nonexistent")
        assert result is None
    
    def test_list_models_empty(self, registry):
        """Test listing models when registry is empty."""
        models = registry.list_models()
        assert models == []
    
    def test_list_models_with_filter(self, registry, sample_model):
        """Test listing models with case filter."""
        registry.register_model(sample_model, tags=["test"], persist=False)
        
        # Filter by matching case
        models = registry.list_models(case=Case.NOMINATIVE)
        assert len(models) == 1
        
        # Filter by non-matching case
        models = registry.list_models(case=Case.ACCUSATIVE)
        assert len(models) == 0
    
    def test_list_models_by_tags(self, registry, sample_model):
        """Test listing models filtered by tags."""
        registry.register_model(sample_model, tags=["ml", "test"], persist=False)
        
        # Filter by matching tag
        models = registry.list_models(tags=["ml"])
        assert len(models) == 1
        
        # Filter by non-matching tag
        models = registry.list_models(tags=["production"])
        assert len(models) == 0
    
    def test_update_model_metrics(self, registry, sample_model):
        """Test updating model metrics."""
        registry.register_model(sample_model, persist=False)
        success = registry.update_model_metrics(
            "test_model",
            metrics={"accuracy": 0.95, "loss": 0.05}
        )
        assert success is True
        
        # Check metrics were stored
        metadata = registry._metadata["test_model"]
        assert metadata.performance_metrics["accuracy"] == 0.95
    
    def test_update_metrics_nonexistent(self, registry):
        """Test updating metrics for non-existent model."""
        success = registry.update_model_metrics("nonexistent", {"accuracy": 0.5})
        assert success is False
    
    def test_remove_model(self, registry, sample_model):
        """Test removing a model."""
        registry.register_model(sample_model, persist=False)
        success = registry.remove_model("test_model", delete_files=False)
        assert success is True
        assert "test_model" not in registry._models
    
    def test_remove_nonexistent_model(self, registry):
        """Test removing a non-existent model."""
        success = registry.remove_model("nonexistent")
        assert success is False
    
    def test_get_statistics(self, registry, sample_model):
        """Test getting registry statistics."""
        registry.register_model(sample_model, persist=False)
        stats = registry.get_statistics()
        
        assert "total_models" in stats
        assert stats["total_models"] == 1
        assert "models_by_case" in stats
        assert "NOM" in stats["models_by_case"]
    
    def test_export_registry(self, registry, sample_model, temp_registry_dir):
        """Test exporting registry to file."""
        registry.register_model(sample_model, persist=False)
        export_path = os.path.join(temp_registry_dir, "export.json")
        
        success = registry.export_registry(export_path)
        assert success is True
        assert os.path.exists(export_path)


class TestGlobalRegistry:
    """Tests for global registry functions."""
    
    def test_get_global_registry(self):
        """Test getting the global registry."""
        registry = get_global_registry()
        assert registry is not None
        assert isinstance(registry, ModelRegistry)
    
    def test_global_registry_singleton(self):
        """Test that global registry is a singleton."""
        registry1 = get_global_registry()
        registry2 = get_global_registry()
        assert registry1 is registry2
