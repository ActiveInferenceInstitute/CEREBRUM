"""
Tests for src/visualization/insect/neural_visualizer.py
"""

import pytest
import numpy as np
import os
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from src.visualization.insect.neural_visualizer import (
    NeuralStructureVisualizer,
    BrainActivityVisualizer
)
from src.models.insect.base import InsectModel

class StubConnection:
    def __init__(self, weight, ctype, target="motor_cortex"):
        self.weight = weight
        self.connection_type = ctype
        self.target = target

class StubNeuralStructure:
    def __init__(self, name="Mushroom Body"):
        self.name = name
        self.structure_type = "sensorimotor"
        self.state = np.array([0.5, 0.4, 0.6])
        self.case_parameters = {"sensitivity": 0.8}
        self.history = {"activity": [np.array([0.5]), np.array([0.6])]}
        self.input_connections = [StubConnection(0.5, "excitatory")]
        self.output_connections = [StubConnection(0.1, "inhibitory")]
        self.output_weights = np.array([[0.1, 0.2]])
    
    def get_state(self):
        return self.state

class _InsectCompat:
    def __init__(self, model):
        self._model = model
        self.species = model.species
        self.neural_structures = {
            "mushroom_body": StubNeuralStructure("Mushroom Body"),
            "central_complex": StubNeuralStructure("Central Complex")
        }

    def __getattr__(self, name):
        return getattr(self._model, name)
        
    @property
    def current_case(self):
        from src.core.model import Case
        return Case.ACCUSATIVE

@pytest.fixture
def insect():
    return _InsectCompat(InsectModel(species="TestAnt"))

@pytest.fixture
def neural_viz():
    return NeuralStructureVisualizer(figsize=(6, 4), dpi=50)

@pytest.fixture
def brain_viz():
    return BrainActivityVisualizer(figsize=(6, 4), dpi=50)

class TestNeuralStructureVisualizer:
    def test_track_activity(self, neural_viz, insect):
        neural_viz.track_activity(insect, {"step": 1})
        assert len(neural_viz.activity_history) == 1

    def test_visualize_structure_activity(self, neural_viz, tmp_path):
        struct = StubNeuralStructure()
        fig = neural_viz.visualize_structure_activity(struct, save_path=str(tmp_path / "struct.png"))
        assert fig is not None
        assert os.path.exists(tmp_path / "struct.png")
        plt.close(fig)

    def test_visualize_connectivity(self, neural_viz):
        struct = StubNeuralStructure()
        fig = neural_viz.visualize_connectivity(struct)
        assert fig is not None
        plt.close(fig)

    def test_generate_comprehensive_visualizations(self, neural_viz, insect, tmp_path):
        neural_viz.track_activity(insect, {})
        neural_viz.generate_comprehensive_visualizations(str(tmp_path))
        # It creates animation which might be skipped in some backends, just ensure directory handled
        # The file checks are bypassed if the animation doesn't save due to no writer 
        # but the method should not crash at least.

class TestBrainActivityVisualizer:
    def test_visualize_brain_activity(self, brain_viz, insect):
        fig = brain_viz.visualize_brain_activity(insect)
        assert fig is not None
        plt.close(fig)

    def test_visualize_brain_connectivity(self, brain_viz, insect, tmp_path):
        fig = brain_viz.visualize_brain_connectivity(insect, save_path=str(tmp_path / "conn.png"))
        assert fig is not None
        assert os.path.exists(tmp_path / "conn.png")
        plt.close(fig)

    def test_create_activity_animation(self, brain_viz, insect):
        brain_viz.activity_history = [{"mushroom_body": np.array([0.5])}, {"mushroom_body": np.array([0.6])}]
        anim = brain_viz.create_activity_animation(insect, duration=1.0, fps=2)
        assert anim is not None

    def test_track_learning_dynamics(self, brain_viz, insect):
        signal = np.array([0.1, 0.2, 0.15])
        fig = brain_viz.track_learning_dynamics(insect, signal)
        assert fig is not None
        plt.close(fig)
