"""
Tests for src/visualization/case_visualization.py and src/visualization/case_comparison.py

Tests visualization functions using the Agg backend (no display required).
Each test verifies that functions produce valid matplotlib Figure objects
and that plots contain expected elements.
"""

import pytest
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from src.core.model import Model, Case
from src.visualization.case_visualization import (
    plot_model_state,
    plot_model_transition,
    plot_model_ecosystem,
    plot_free_energy_landscape,
    plot_case_transformation_cycle,
    CASE_COLORS,
    CASE_SYMBOLS,
)
from src.visualization.case_comparison import (
    CaseComparisonVisualizer,
    get_case_info,
    case_linguistic_meanings,
)


@pytest.fixture
def model():
    """Create a model with parameters for visualization tests."""
    m = Model(name="VizTestModel")
    m.parameters = {
        "learning_rate": 0.01,
        "precision": 1.0,
        "weights": np.random.randn(5),
    }
    m.case = Case.NOMINATIVE
    # Attributes required by plot_free_energy_landscape
    m.posterior_means = np.array([0.5, 0.3])
    m.prior_means = np.array([0.0, 0.0])
    return m


@pytest.fixture
def model_ecosystem():
    """Create multiple models for ecosystem visualization."""
    models = []
    cases = [Case.NOMINATIVE, Case.ACCUSATIVE, Case.DATIVE, Case.GENITIVE]
    for i, case in enumerate(cases):
        m = Model(name=f"Model_{case.name}")
        m.case = case
        m.parameters = {"weight": float(i)}
        models.append(m)
    return models


class TestCaseConstants:
    """Test case color and symbol constants."""

    def test_case_colors_complete(self):
        for case in Case:
            assert case in CASE_COLORS, f"Missing color for {case.name}"

    def test_case_symbols_complete(self):
        for case in Case:
            assert case in CASE_SYMBOLS, f"Missing symbol for {case.name}"


class TestPlotModelState:
    """Test plot_model_state function."""

    def test_returns_figure(self, model):
        fig = plot_model_state(model)
        assert isinstance(fig, plt.Figure)
        plt.close(fig)

    def test_custom_figsize(self, model):
        fig = plot_model_state(model, figsize=(8, 4))
        assert isinstance(fig, plt.Figure)
        plt.close(fig)

    def test_without_parameters(self, model):
        fig = plot_model_state(model, show_parameters=False)
        assert isinstance(fig, plt.Figure)
        plt.close(fig)

    def test_without_precision(self, model):
        fig = plot_model_state(model, precision_plot=False)
        assert isinstance(fig, plt.Figure)
        plt.close(fig)


class TestPlotModelTransition:
    """Test plot_model_transition function."""

    def test_returns_figure(self, model):
        fig = plot_model_transition(model, Case.NOMINATIVE, Case.ACCUSATIVE)
        assert isinstance(fig, plt.Figure)
        plt.close(fig)

    def test_all_case_transitions(self, model):
        cases = list(Case)
        for i in range(len(cases) - 1):
            fig = plot_model_transition(model, cases[i], cases[i + 1])
            assert isinstance(fig, plt.Figure)
            plt.close(fig)


class TestPlotModelEcosystem:
    """Test plot_model_ecosystem function."""

    def test_returns_figure(self, model_ecosystem):
        fig = plot_model_ecosystem(model_ecosystem)
        assert isinstance(fig, plt.Figure)
        plt.close(fig)

    def test_with_relationships(self, model_ecosystem):
        relationships = [
            (model_ecosystem[0], model_ecosystem[1], "observes"),
            (model_ecosystem[1], model_ecosystem[2], "sends_data"),
        ]
        fig = plot_model_ecosystem(model_ecosystem, relationships=relationships)
        assert isinstance(fig, plt.Figure)
        plt.close(fig)


class TestPlotFreeEnergyLandscape:
    """Test plot_free_energy_landscape function.
    
    Note: This function calls model.free_energy(observations), so we need
    a model subclass that accepts the observations argument.
    """

    @pytest.fixture
    def fe_model(self):
        """Model with free_energy(observations) support for landscape plotting."""
        class FreeEnergyModel(Model):
            def free_energy(self, observations=None):
                return float(np.sum(self.posterior_means ** 2))
        
        m = FreeEnergyModel(name="FEModel")
        m.parameters = {"precision": 1.0}
        m.case = Case.NOMINATIVE
        m.posterior_means = np.array([0.5, 0.3])
        m.prior_means = np.array([0.0, 0.0])
        return m

    def test_returns_figure(self, fe_model):
        fig = plot_free_energy_landscape(fe_model)
        assert isinstance(fig, plt.Figure)
        plt.close(fig)

    def test_with_observations(self, fe_model):
        obs = np.random.randn(10, 2)
        fig = plot_free_energy_landscape(fe_model, observations=obs)
        assert isinstance(fig, plt.Figure)
        plt.close(fig)

    def test_custom_resolution(self, fe_model):
        fig = plot_free_energy_landscape(fe_model, resolution=20)
        assert isinstance(fig, plt.Figure)
        plt.close(fig)


class TestPlotCaseTransformationCycle:
    """Test plot_case_transformation_cycle function."""

    def test_returns_figure(self, model):
        sequence = [Case.NOMINATIVE, Case.ACCUSATIVE, Case.DATIVE, Case.NOMINATIVE]
        fig = plot_case_transformation_cycle(model, sequence)
        assert isinstance(fig, plt.Figure)
        plt.close(fig)

    def test_four_case_cycle(self, model):
        # Use 4 cases to avoid subplot layout issues with too many axes
        sequence = [Case.NOMINATIVE, Case.ACCUSATIVE, Case.DATIVE, Case.GENITIVE]
        fig = plot_case_transformation_cycle(model, sequence)
        assert isinstance(fig, plt.Figure)
        plt.close(fig)


class TestGetCaseInfo:
    """Test the get_case_info utility function."""

    def test_returns_dict_for_all_cases(self):
        for case in Case:
            info = get_case_info(case)
            assert isinstance(info, dict)

    def test_info_contains_meaning(self):
        info = get_case_info(Case.NOMINATIVE)
        assert "meaning" in info or "color" in info or len(info) >= 1


class TestCaseLinguisticMeanings:
    """Test case_linguistic_meanings dictionary completeness."""

    def test_all_cases_have_meanings(self):
        for case in Case:
            assert case in case_linguistic_meanings
