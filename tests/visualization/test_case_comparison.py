"""
Tests for src/visualization/case_comparison.py

Tests CaseComparisonVisualizer and case_linguistic_meanings / case_statistical_roles
constants with real matplotlib and temporary directories.
"""

import pytest
import os
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from src.core.model import Model, Case
from src.visualization.case_comparison import (
    CaseComparisonVisualizer,
    case_linguistic_meanings,
    case_statistical_roles,
    get_case_info,
)
from src.visualization.case_visualization import CASE_COLORS, CASE_SYMBOLS


class TestCaseLinguisticMeanings:
    def test_all_cases_covered(self):
        for case in Case:
            assert case in case_linguistic_meanings

    def test_all_values_are_strings(self):
        for case, meaning in case_linguistic_meanings.items():
            assert isinstance(meaning, str)


class TestCaseStatisticalRoles:
    def test_all_cases_covered(self):
        for case in Case:
            assert case in case_statistical_roles


class TestGetCaseInfo:
    def test_returns_dict(self):
        info = get_case_info(Case.NOMINATIVE)
        assert isinstance(info, dict)

    def test_has_all_keys(self):
        info = get_case_info(Case.ACCUSATIVE)
        assert "name" in info
        assert "linguistic_meaning" in info
        assert "statistical_role" in info
        assert "color" in info
        assert "symbol" in info

    @pytest.mark.parametrize("case", list(Case))
    def test_all_cases(self, case):
        info = get_case_info(case)
        assert info["name"] == case.value


class TestCreateComparisonGrid:
    def test_creates_file(self, tmp_path):
        path = CaseComparisonVisualizer.create_comparison_grid(
            case=Case.NOMINATIVE,
            output_dir=str(tmp_path),
        )
        assert os.path.exists(path)
        assert path.endswith(".png")
        plt.close("all")

    def test_with_model(self, tmp_path):
        model = Model(name="TestModel")
        model.case = Case.NOMINATIVE
        path = CaseComparisonVisualizer.create_comparison_grid(
            case=Case.NOMINATIVE,
            output_dir=str(tmp_path),
            nn_model=model,
        )
        assert os.path.exists(path)
        plt.close("all")
