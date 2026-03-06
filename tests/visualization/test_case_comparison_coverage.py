"""
Tests for src/visualization/case_comparison.py

Tests CaseComparisonVisualizer, get_case_info, and the case
linguistic/statistical mapping dictionaries.
"""

import os
import pytest
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

from src.core.model import Model, Case
from src.visualization.case_comparison import (
    CaseComparisonVisualizer,
    get_case_info,
    case_linguistic_meanings,
    case_statistical_roles,
)


# ── Dictionaries ──────────────────────────────────────────────────

class TestCaseDictionaries:
    def test_all_cases_have_linguistic_meanings(self):
        for case in Case:
            assert case in case_linguistic_meanings
            assert isinstance(case_linguistic_meanings[case], str)

    def test_all_cases_have_statistical_roles(self):
        for case in Case:
            assert case in case_statistical_roles
            assert isinstance(case_statistical_roles[case], str)


# ── get_case_info ─────────────────────────────────────────────────

class TestGetCaseInfo:
    def test_returns_complete_info(self):
        for case in Case:
            info = get_case_info(case)
            assert 'name' in info
            assert 'linguistic_meaning' in info
            assert 'statistical_role' in info
            assert 'color' in info
            assert 'symbol' in info

    def test_nominative_info(self):
        info = get_case_info(Case.NOMINATIVE)
        assert info['name'] == Case.NOMINATIVE.value
        assert 'agent' in info['linguistic_meaning']


# ── CaseComparisonVisualizer ─────────────────────────────────────

class TestCaseComparisonVisualizer:
    def test_create_comparison_grid_no_models(self, tmp_path):
        output_path = CaseComparisonVisualizer.create_comparison_grid(
            case=Case.NOMINATIVE,
            output_dir=str(tmp_path),
        )
        assert os.path.exists(output_path)
        assert output_path.endswith('.png')

    def test_create_comparison_grid_with_model(self, tmp_path):
        model = Model(name="TestModel")
        output_path = CaseComparisonVisualizer.create_comparison_grid(
            case=Case.ACCUSATIVE,
            output_dir=str(tmp_path),
            nn_model=model,
        )
        assert os.path.exists(output_path)

    def test_create_comparison_grid_all_cases(self, tmp_path):
        for case in Case:
            path = CaseComparisonVisualizer.create_comparison_grid(
                case=case, output_dir=str(tmp_path)
            )
            assert os.path.exists(path)

    def test_create_all_case_comparisons(self, tmp_path):
        paths = CaseComparisonVisualizer.create_all_case_comparisons(
            output_dir=str(tmp_path)
        )
        assert len(paths) == len(Case) + 1  # all cases + summary
        for p in paths:
            assert os.path.exists(p)

    def test_plot_pomdp_nominative_no_belief(self, tmp_path):
        fig, ax = plt.subplots()
        model = Model(name="POMDP")
        CaseComparisonVisualizer._plot_pomdp_representation(ax, model, Case.NOMINATIVE)
        plt.close(fig)

    def test_plot_pomdp_nominative_with_belief(self, tmp_path):
        fig, ax = plt.subplots()
        model = Model(name="POMDP")
        model.belief_state = [0.2, 0.3, 0.5]
        CaseComparisonVisualizer._plot_pomdp_representation(ax, model, Case.NOMINATIVE)
        plt.close(fig)

    def test_plot_pomdp_accusative_no_matrix(self):
        fig, ax = plt.subplots()
        model = Model(name="POMDP")
        CaseComparisonVisualizer._plot_pomdp_representation(ax, model, Case.ACCUSATIVE)
        plt.close(fig)

    def test_plot_pomdp_accusative_with_matrix(self):
        fig, ax = plt.subplots()
        model = Model(name="POMDP")
        model.parameters['transition_matrix'] = np.eye(3)
        CaseComparisonVisualizer._plot_pomdp_representation(ax, model, Case.ACCUSATIVE)
        plt.close(fig)

    def test_plot_pomdp_instrumental(self):
        fig, ax = plt.subplots()
        model = Model(name="POMDP")
        CaseComparisonVisualizer._plot_pomdp_representation(ax, model, Case.INSTRUMENTAL)
        plt.close(fig)

    def test_plot_pomdp_default_case(self):
        fig, ax = plt.subplots()
        model = Model(name="POMDP")
        CaseComparisonVisualizer._plot_pomdp_representation(ax, model, Case.DATIVE)
        plt.close(fig)

    def test_plot_nn_nominative(self):
        fig, ax = plt.subplots()
        model = Model(name="NN")
        CaseComparisonVisualizer._plot_nn_representation(ax, model, Case.NOMINATIVE)
        plt.close(fig)

    def test_plot_nn_accusative(self):
        fig, ax = plt.subplots()
        model = Model(name="NN")
        CaseComparisonVisualizer._plot_nn_representation(ax, model, Case.ACCUSATIVE)
        plt.close(fig)

    def test_plot_nn_dative(self):
        fig, ax = plt.subplots()
        model = Model(name="NN")
        CaseComparisonVisualizer._plot_nn_representation(ax, model, Case.DATIVE)
        plt.close(fig)

    def test_plot_nn_default(self):
        fig, ax = plt.subplots()
        model = Model(name="NN")
        CaseComparisonVisualizer._plot_nn_representation(ax, model, Case.GENITIVE)
        plt.close(fig)

    def test_plot_linear_nominative_no_coeff(self):
        fig, ax = plt.subplots()
        model = Model(name="LR")
        CaseComparisonVisualizer._plot_linear_representation(ax, model, Case.NOMINATIVE)
        plt.close(fig)

    def test_plot_linear_nominative_1d(self):
        fig, ax = plt.subplots()
        model = Model(name="LR")
        model.coefficients_ = [1.5]
        model.intercept_ = 0.3
        CaseComparisonVisualizer._plot_linear_representation(ax, model, Case.NOMINATIVE)
        plt.close(fig)

    def test_plot_linear_nominative_nd(self):
        fig, ax = plt.subplots()
        model = Model(name="LR")
        model.coefficients_ = [1.5, 2.0, -0.5]
        model.intercept_ = 0.3
        CaseComparisonVisualizer._plot_linear_representation(ax, model, Case.NOMINATIVE)
        plt.close(fig)

    def test_plot_linear_instrumental(self):
        fig, ax = plt.subplots()
        model = Model(name="LR")
        CaseComparisonVisualizer._plot_linear_representation(ax, model, Case.INSTRUMENTAL)
        plt.close(fig)

    def test_plot_linear_default(self):
        fig, ax = plt.subplots()
        model = Model(name="LR")
        CaseComparisonVisualizer._plot_linear_representation(ax, model, Case.DATIVE)
        plt.close(fig)
