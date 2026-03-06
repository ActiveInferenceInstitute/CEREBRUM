"""
Tests for src/core/model_registry.py

Comprehensive tests for ModelMetadata, ModelRegistry lifecycle
(register, get, list, update, remove, export), and statistics.
"""

import json
import pytest

from src.core.model import Model, Case
from src.core.model_registry import (
    ModelMetadata,
    ModelRegistry,
)
from datetime import datetime


# ── ModelMetadata ─────────────────────────────────────────────────

class TestModelMetadata:
    def test_to_dict(self):
        now = datetime.now()
        md = ModelMetadata(
            model_id="m1", model_type="Model", case=Case.NOMINATIVE,
            created_at=now, last_accessed=now, tags=["test"],
            description="A test model"
        )
        d = md.to_dict()
        assert d['model_id'] == 'm1'
        assert d['case'] == Case.NOMINATIVE.value
        assert isinstance(d['created_at'], str)

    def test_roundtrip(self):
        now = datetime.now()
        md = ModelMetadata(
            model_id="m2", model_type="Model", case=Case.DATIVE,
            created_at=now, last_accessed=now
        )
        d = md.to_dict()
        md2 = ModelMetadata.from_dict(d)
        assert md2.model_id == 'm2'
        assert md2.case == Case.DATIVE


# ── ModelRegistry ─────────────────────────────────────────────────

class TestModelRegistry:
    @pytest.fixture
    def registry(self, tmp_path):
        return ModelRegistry(registry_path=str(tmp_path / "registry"))

    @pytest.fixture
    def model(self):
        return Model(name="TestModel")

    def test_init_creates_dirs(self, registry):
        assert registry.registry_path.exists()
        assert registry.models_path.exists()

    def test_register_and_get(self, registry, model):
        model_id = registry.register_model(model, model_id="m1")
        assert model_id == "m1"
        retrieved = registry.get_model("m1")
        assert retrieved is model

    def test_register_uses_model_name(self, registry, model):
        model_id = registry.register_model(model)
        assert model_id == "TestModel"

    def test_register_with_tags(self, registry, model):
        registry.register_model(model, tags=["core", "test"])
        models = registry.list_models(tags=["core"])
        assert len(models) == 1
        assert "core" in models[0].tags

    def test_register_overwrite(self, registry, model):
        registry.register_model(model, model_id="dup")
        m2 = Model(name="Other")
        registry.register_model(m2, model_id="dup")
        assert registry.get_model("dup") is m2

    def test_get_nonexistent(self, registry):
        assert registry.get_model("ghost") is None

    def test_get_loads_from_disk(self, registry, model, tmp_path):
        registry.register_model(model, model_id="persist_test")
        # Clear memory
        del registry._models["persist_test"]
        retrieved = registry.get_model("persist_test")
        assert retrieved is not None
        assert retrieved.name == "TestModel"

    def test_list_all(self, registry):
        registry.register_model(Model(name="A"), model_id="a")
        registry.register_model(Model(name="B"), model_id="b")
        models = registry.list_models()
        assert len(models) == 2

    def test_list_filter_by_case(self, registry):
        m1 = Model(name="M1")
        m1.case = Case.NOMINATIVE
        m2 = Model(name="M2")
        m2.case = Case.DATIVE
        registry.register_model(m1, model_id="nom")
        registry.register_model(m2, model_id="dat")
        nom_models = registry.list_models(case=Case.NOMINATIVE)
        assert len(nom_models) == 1

    def test_list_filter_by_type(self, registry):
        registry.register_model(Model(name="X"), model_id="x")
        models = registry.list_models(model_type="Model")
        assert len(models) == 1

    def test_update_metrics(self, registry, model):
        registry.register_model(model, model_id="m")
        result = registry.update_model_metrics("m", {"accuracy": 0.95})
        assert result is True
        assert registry._metadata["m"].performance_metrics["accuracy"] == 0.95

    def test_update_metrics_nonexistent(self, registry):
        assert registry.update_model_metrics("nope", {"a": 1}) is False

    def test_remove_model(self, registry, model):
        registry.register_model(model, model_id="rm")
        assert registry.remove_model("rm") is True
        assert registry.get_model("rm") is None

    def test_remove_nonexistent(self, registry):
        assert registry.remove_model("nope") is False

    def test_get_statistics_empty(self, registry):
        stats = registry.get_statistics()
        assert stats['total_models'] == 0

    def test_get_statistics_populated(self, registry):
        registry.register_model(Model(name="A"), model_id="a")
        registry.register_model(Model(name="B"), model_id="b")
        stats = registry.get_statistics()
        assert stats['total_models'] == 2
        assert stats['newest_model'] == 'b'

    def test_export_registry(self, registry, model, tmp_path):
        registry.register_model(model, model_id="exp")
        export_path = str(tmp_path / "export.json")
        assert registry.export_registry(export_path) is True
        with open(export_path) as f:
            data = json.load(f)
        assert 'exp' in data['models']

    def test_persistence_survives_reload(self, tmp_path):
        path = str(tmp_path / "persist_reg")
        reg1 = ModelRegistry(registry_path=path)
        reg1.register_model(Model(name="Keep"), model_id="keep")
        # Create new instance pointing to same path
        reg2 = ModelRegistry(registry_path=path)
        assert "keep" in reg2._metadata
