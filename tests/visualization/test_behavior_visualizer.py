"""
Tests for src/visualization/insect/behavior_visualizer.py
"""

import pytest
import numpy as np
import os
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from src.visualization.insect.behavior_visualizer import (
    BehaviorPatternVisualizer,
    SwarmBehaviorVisualizer
)
from src.models.insect.base import InsectModel, BehavioralState
from src.core.model import Case

class _InsectCompat:
    def __init__(self, model):
        self._model = model
        self.species = model.species
        self.internal_state = {"energy": 0.8, "health": 1.0}

    def __getattr__(self, name):
        return getattr(self._model, name)

    @property
    def current_case(self):
        return self._model.case

    @property
    def behavioral_state(self):
        # Base insect defaults to IDLE which isn't in color maps, return FORAGING
        return BehavioralState.FORAGING

    @property
    def position(self):
        return np.array([1.0, 2.0, 0.0])

@pytest.fixture
def insect():
    return _InsectCompat(InsectModel(species="TestAnt"))

@pytest.fixture
def behavior_viz():
    return BehaviorPatternVisualizer(figsize=(6, 4), dpi=50)

@pytest.fixture
def swarm_viz():
    return SwarmBehaviorVisualizer(figsize=(6, 4), dpi=50)

class TestBehaviorPatternVisualizer:
    def test_init(self, behavior_viz):
        assert behavior_viz is not None

    def test_track_behavior(self, behavior_viz, insect):
        event = {"step": 1, "context": {"food": True}}
        behavior_viz.track_behavior(insect, event)
        assert len(behavior_viz.behavior_history) > 0

    def test_visualize_behavior_timeline(self, behavior_viz, insect, tmp_path):
        for i in range(5):
            behavior_viz.track_behavior(insect, {"step": i})
            # mock changing states
        fig = behavior_viz.visualize_behavior_timeline(save_path=str(tmp_path / "timeline.png"))
        assert fig is not None
        assert os.path.exists(tmp_path / "timeline.png")
        plt.close(fig)

    def test_generate_comprehensive_visualizations(self, behavior_viz, insect, tmp_path):
        for i in range(5):
            behavior_viz.track_behavior(insect, {"step": i})
        # This will create a visualizations/swarm_analysis folder
        behavior_viz.generate_comprehensive_visualizations(str(tmp_path))
        target_dir = tmp_path / "visualizations" / "swarm_analysis"
        assert os.path.exists(target_dir / "behavior_and_case_timeline.png")

    def test_visualize_behavior_performance(self, behavior_viz, insect, tmp_path):
        behavior_viz.track_behavior(insect, {"step": 1, "performance": 0.8})
        fig = behavior_viz.visualize_behavior_performance(save_path=str(tmp_path / "perf.png"))
        assert fig is not None
        plt.close(fig)

    def test_visualize_behavior_transitions(self, behavior_viz, insect, tmp_path):
        behavior_viz.track_behavior(insect, {"step": 1})
        fig = behavior_viz.visualize_behavior_transitions(save_path=str(tmp_path / "trans.png"))
        assert fig is not None
        plt.close(fig)


class TestSwarmBehaviorVisualizer:
    def test_init(self, swarm_viz):
        assert swarm_viz is not None

    def test_track_swarm_state(self, swarm_viz, insect):
        swarm_viz.track_swarm_state([insect, insect], np.array([0,0,0]), 1.0, 0.8)
        assert len(swarm_viz.swarm_history) > 0

    def test_visualize_swarm_dynamics(self, swarm_viz, insect, tmp_path):
        for _ in range(3):
            swarm_viz.track_swarm_state([insect, insect], np.array([0,0,0]), 1.0, 0.8)
        fig = swarm_viz.visualize_swarm_dynamics(save_path=str(tmp_path / "dyn.png"))
        assert fig is not None
        plt.close(fig)

    def test_generate_comprehensive_visualizations(self, swarm_viz, insect, tmp_path):
        for _ in range(3):
            swarm_viz.track_swarm_state([insect, insect], np.array([0,0,0]), 1.0, 0.8)
        swarm_viz.generate_comprehensive_visualizations(str(tmp_path))
        target_dir = tmp_path / "visualizations" / "swarm_analysis"
        assert os.path.exists(target_dir / "swarm_dynamics_analysis.png")

    def test_visualize_swarm_state(self, swarm_viz, insect):
        env = {"food_sources": [{"position": [1,2,0]}]}
        fig = swarm_viz.visualize_swarm_state([insect, insect], env)
        assert fig is not None
        plt.close(fig)

    def test_create_swarm_animation(self, swarm_viz, insect):
        # We don't save to avoid ffmpeg dependency issues, just create it
        for _ in range(3):
            swarm_viz.track_swarm_state([insect, insect], np.array([0,0,0]), 1.0, 0.8)
        
        # Test just the generation of the animation object
        anim = swarm_viz.create_swarm_animation(duration=1.0, fps=2)
        assert anim is not None
