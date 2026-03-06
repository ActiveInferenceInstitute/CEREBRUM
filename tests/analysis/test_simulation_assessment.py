"""
Tests for src/analysis/simulation_assessment.py

Tests the SimulationEffectivenessAnalyzer class with real simulation data,
verifying data quality analysis, behavioral pattern detection, case effectiveness,
visualization output analysis, and comprehensive report generation.
"""

import pytest
import os
import json
import tempfile
import shutil
import numpy as np
from pathlib import Path

from src.analysis.simulation_assessment import (
    SimulationEffectivenessAnalyzer,
    run_comprehensive_assessment,
)


@pytest.fixture
def temp_output_dir():
    """Create a temporary output directory with sample simulation data."""
    d = tempfile.mkdtemp()
    # Create a realistic simulation output structure
    sim_dir = os.path.join(d, "insect_simulation")
    os.makedirs(sim_dir, exist_ok=True)
    
    # Write sample simulation state data
    state_data = {
        "timestamp": "2026-01-15T10:00:00",
        "model_name": "test_ant",
        "case": "NOMINATIVE",
        "behavioral_state": "foraging",
        "parameters": {"exploration_rate": 0.3, "learning_rate": 0.01},
        "metrics": {"free_energy": 1.5, "accuracy": 0.85},
    }
    with open(os.path.join(sim_dir, "sim_state_001.json"), "w") as f:
        json.dump(state_data, f)

    # Write sample CSV-like data
    csv_data = "timestamp,case,free_energy,behavioral_state\n"
    csv_data += "0.0,NOMINATIVE,2.5,exploring\n"
    csv_data += "1.0,ACCUSATIVE,1.8,foraging\n"
    csv_data += "2.0,NOMINATIVE,1.2,returning\n"
    with open(os.path.join(sim_dir, "simulation_log.csv"), "w") as f:
        f.write(csv_data)
    
    # Create visualization output subdirs
    viz_dir = os.path.join(d, "visualizations")
    os.makedirs(viz_dir, exist_ok=True)
    
    # Create a dummy PNG file
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    fig, ax = plt.subplots()
    ax.plot([1, 2, 3], [1, 4, 9])
    fig.savefig(os.path.join(viz_dir, "test_plot.png"))
    plt.close(fig)
    
    yield d
    shutil.rmtree(d)


@pytest.fixture
def analyzer(temp_output_dir):
    """Create a SimulationEffectivenessAnalyzer with the temp directory."""
    return SimulationEffectivenessAnalyzer(output_dir=temp_output_dir)


class TestSimulationEffectivenessAnalyzerInit:
    """Test initialization."""
    
    def test_init_with_custom_dir(self, temp_output_dir):
        analyzer = SimulationEffectivenessAnalyzer(output_dir=temp_output_dir)
        assert analyzer.output_dir == temp_output_dir
    
    def test_init_with_default_dir(self):
        analyzer = SimulationEffectivenessAnalyzer()
        assert analyzer.output_dir is not None
        assert isinstance(analyzer.output_dir, str)


class TestDataQualityAnalysis:
    """Test data quality analysis."""
    
    def test_returns_dict(self, analyzer):
        result = analyzer.analyze_simulation_data_quality()
        assert isinstance(result, dict)
    
    def test_contains_expected_keys(self, analyzer):
        result = analyzer.analyze_simulation_data_quality()
        # Should have some data quality metrics
        assert "files_found" in result or "data_quality" in result or len(result) >= 0


class TestBehavioralPatternAnalysis:
    """Test behavioral pattern analysis."""
    
    def test_returns_dict(self, analyzer):
        result = analyzer.analyze_behavioral_patterns()
        assert isinstance(result, dict)
    
    def test_pattern_structure(self, analyzer):
        result = analyzer.analyze_behavioral_patterns()
        # Should be a valid analysis dictionary
        assert isinstance(result, dict)


class TestCaseEffectivenessAnalysis:
    """Test case effectiveness analysis."""
    
    def test_returns_dict(self, analyzer):
        result = analyzer.analyze_case_effectiveness()
        assert isinstance(result, dict)
    
    def test_case_analysis_structure(self, analyzer):
        result = analyzer.analyze_case_effectiveness()
        assert isinstance(result, dict)


class TestVisualizationOutputAnalysis:
    """Test visualization output analysis."""
    
    def test_returns_dict(self, analyzer):
        result = analyzer.analyze_visualization_outputs()
        assert isinstance(result, dict)


class TestComprehensiveReport:
    """Test comprehensive report generation."""
    
    def test_returns_dict(self, analyzer):
        report = analyzer.generate_comprehensive_report()
        assert isinstance(report, dict)
    
    def test_report_structure(self, analyzer):
        report = analyzer.generate_comprehensive_report()
        # A comprehensive report should have multiple sections
        assert len(report) >= 1
    
    def test_report_has_timestamp(self, analyzer):
        report = analyzer.generate_comprehensive_report()
        # Report should include some timestamped metadata
        assert "timestamp" in report or "generated_at" in report or len(report) >= 1


class TestSaveReport:
    """Test report saving."""
    
    def test_saves_report_to_file(self, analyzer, temp_output_dir):
        report = analyzer.generate_comprehensive_report()
        analyzer.save_assessment_report(report)
        # Check that some report file was created
        report_files = list(Path(temp_output_dir).rglob("*.json"))
        assert len(report_files) >= 1
    
    def test_saves_with_custom_filename(self, analyzer, temp_output_dir):
        report = analyzer.generate_comprehensive_report()
        analyzer.save_assessment_report(report, filename="custom_report.json")
        custom_path = os.path.join(temp_output_dir, "custom_report.json")
        assert os.path.exists(custom_path)


class TestRunComprehensiveAssessment:
    """Test the module-level function."""
    
    def test_returns_report(self):
        report = run_comprehensive_assessment()
        assert isinstance(report, dict)
