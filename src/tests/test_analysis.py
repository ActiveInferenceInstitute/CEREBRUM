"""
Tests for the analysis module.
"""

import pytest
import numpy as np
from unittest.mock import patch, MagicMock
import tempfile
import os

# Skip tests if optional dependencies are not available
pytest.importorskip("imageio", reason="imageio required for analysis tests")
pytest.importorskip("seaborn", reason="seaborn required for analysis tests")

from src.analysis.simulation_assessment import SimulationEffectivenessAnalyzer


class TestSimulationEffectivenessAnalyzer:
    """Test the SimulationEffectivenessAnalyzer class."""
    
    def test_initialization_default(self):
        """Test analyzer initialization with default output directory."""
        analyzer = SimulationEffectivenessAnalyzer()
        assert analyzer.output_dir is not None
    
    def test_initialization_custom_dir(self):
        """Test analyzer initialization with custom output directory."""
        with tempfile.TemporaryDirectory() as tmpdir:
            analyzer = SimulationEffectivenessAnalyzer(output_dir=tmpdir)
            assert analyzer.output_dir == tmpdir
    
    def test_analyze_simulation_data_quality(self):
        """Test data quality analysis."""
        with tempfile.TemporaryDirectory() as tmpdir:
            analyzer = SimulationEffectivenessAnalyzer(output_dir=tmpdir)
            result = analyzer.analyze_simulation_data_quality()
            
            assert isinstance(result, dict)
            # Should have basic structure even with empty directory
            assert 'total_files' in result or 'status' in result
    
    def test_analyze_behavioral_patterns(self):
        """Test behavioral pattern analysis."""
        with tempfile.TemporaryDirectory() as tmpdir:
            analyzer = SimulationEffectivenessAnalyzer(output_dir=tmpdir)
            result = analyzer.analyze_behavioral_patterns()
            
            assert isinstance(result, dict)
    
    def test_analyze_case_effectiveness(self):
        """Test case effectiveness analysis."""
        with tempfile.TemporaryDirectory() as tmpdir:
            analyzer = SimulationEffectivenessAnalyzer(output_dir=tmpdir)
            result = analyzer.analyze_case_effectiveness()
            
            assert isinstance(result, dict)
    
    def test_generate_comprehensive_report(self):
        """Test comprehensive report generation."""
        with tempfile.TemporaryDirectory() as tmpdir:
            analyzer = SimulationEffectivenessAnalyzer(output_dir=tmpdir)
            report = analyzer.generate_comprehensive_report()
            
            assert isinstance(report, dict)
            # Report should have standard sections
            assert 'timestamp' in report or 'summary' in report or 'status' in report
    
    def test_save_assessment_report(self):
        """Test saving assessment report to file."""
        with tempfile.TemporaryDirectory() as tmpdir:
            analyzer = SimulationEffectivenessAnalyzer(output_dir=tmpdir)
            
            test_report = {
                'test_key': 'test_value',
                'metrics': {'accuracy': 0.95}
            }
            
            analyzer.save_assessment_report(test_report, 'test_report.json')
            
            # Check file was created
            report_path = os.path.join(tmpdir, 'test_report.json')
            assert os.path.exists(report_path)
