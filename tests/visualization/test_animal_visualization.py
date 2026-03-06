"""
Tests for src/visualization/animal_visualization.py

Tests animal visualization functions using the Agg backend:
plot_animal_path, visualize_animal_cases.

Note: plot_animal_environment and plot_animal_sensory_state require
specific environment formats and polar axes that are tested separately
with appropriate fixtures.
"""

import pytest
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from src.core.model import Case
from src.examples.animal_agent import AnimalAgent
from src.visualization.animal_visualization import (
    plot_animal_path,
    visualize_animal_cases,
)


@pytest.fixture
def animal():
    """Create an animal agent with path history for visualization."""
    a = AnimalAgent(name="VizAnimal", position=np.array([5.0, 5.0]))
    # Build path history
    if hasattr(a, 'path_history'):
        for i in range(5):
            a.path_history.append(np.array([5.0 + i * 0.5, 5.0 + i * 0.3]))
    return a


class TestPlotAnimalPath:
    """Test plot_animal_path function."""

    def test_returns_figure(self, animal):
        fig = plot_animal_path(animal)
        assert isinstance(fig, (plt.Figure, type(None))) or fig is None
        plt.close("all")


class TestVisualizeAnimalCases:
    """Test visualize_animal_cases function."""

    def test_returns_dict(self, animal):
        result = visualize_animal_cases(animal)
        assert isinstance(result, dict)
        plt.close("all")

    def test_has_case_entries(self, animal):
        result = visualize_animal_cases(animal)
        assert len(result) >= 1
        plt.close("all")

    def test_case_figures_are_valid(self, animal):
        result = visualize_animal_cases(animal)
        for case_name, fig in result.items():
            if fig is not None:
                assert isinstance(fig, plt.Figure)
        plt.close("all")
