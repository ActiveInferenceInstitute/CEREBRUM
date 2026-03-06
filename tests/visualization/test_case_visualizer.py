"""
Tests for src/visualization/insect/case_visualizer.py
"""

import pytest
import numpy as np
import os
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from src.visualization.insect.case_visualizer import (
    InsectCaseVisualizer,
    CaseTransitionVisualizer,
    CaseEffectivenessVisualizer
)
from src.models.insect.base import InsectModel
from src.core.model import Case

class _InsectCompat:
    def __init__(self, model):
        self._model = model
        self.species = model.species
        self.internal_state = {"energy": 0.8}

    def __getattr__(self, name):
        return getattr(self._model, name)

    @property
    def current_case(self):
        return Case.ACCUSATIVE

@pytest.fixture
def insect():
    return _InsectCompat(InsectModel(species="TestAnt"))

@pytest.fixture
def case_viz():
    return InsectCaseVisualizer(figsize=(6, 4), dpi=50)

@pytest.fixture
def trans_viz():
    return CaseTransitionVisualizer(figsize=(6, 4), dpi=50)

@pytest.fixture
def effect_viz():
    return CaseEffectivenessVisualizer(figsize=(6, 4), dpi=50)

class TestInsectCaseVisualizer:
    def test_track_case_usage(self, case_viz, insect):
        case_viz.track_case_usage(insect, {"food_detected": True})
        assert len(case_viz.case_history) == 1

    def test_visualize_case_distribution(self, case_viz, insect, tmp_path):
        case_viz.track_case_usage(insect, {})
        fig = case_viz.visualize_case_distribution(save_path=str(tmp_path / "dist.png"))
        assert fig is not None
        assert os.path.exists(tmp_path / "dist.png")
        plt.close(fig)

    def test_visualize_case_effectiveness(self, case_viz, insect):
        case_viz.track_case_usage(insect, {})
        fig = case_viz.visualize_case_effectiveness(insect, {})
        assert fig is not None
        plt.close(fig)

    def test_generate_comprehensive_visualizations(self, case_viz, insect, tmp_path):
        case_viz.track_case_usage(insect, {})
        case_viz.generate_comprehensive_visualizations(str(tmp_path))
        target_dir = tmp_path / "visualizations" / "case_analysis"
        assert os.path.exists(target_dir / "case_distribution_pie.png")

class TestCaseTransitionVisualizer:
    def test_track_transition(self, trans_viz):
        trans_viz.track_transition(Case.NOMINATIVE, Case.ACCUSATIVE, {}, True)
        assert len(trans_viz.transition_history) == 1

    def test_visualize_transition_patterns(self, trans_viz, tmp_path):
        trans_viz.track_transition(Case.NOMINATIVE, Case.ACCUSATIVE, {}, True)
        fig = trans_viz.visualize_transition_patterns(save_path=str(tmp_path / "trans.png"))
        assert fig is not None
        assert os.path.exists(tmp_path / "trans.png")
        plt.close(fig)

class TestCaseEffectivenessVisualizer:
    def test_track_effectiveness(self, effect_viz):
        effect_viz.track_effectiveness(Case.ACCUSATIVE, {"score": 0.9}, {"health": 1})
        assert Case.ACCUSATIVE in effect_viz.effectiveness_data
        assert len(effect_viz.effectiveness_data[Case.ACCUSATIVE]) == 1

    def test_visualize_effectiveness_trends(self, effect_viz, tmp_path):
        effect_viz.track_effectiveness(Case.ACCUSATIVE, {"score": 0.9}, {"health": 1})
        fig = effect_viz.visualize_effectiveness_trends(save_path=str(tmp_path / "eff.png"))
        assert fig is not None
        assert os.path.exists(tmp_path / "eff.png")
        plt.close(fig)
