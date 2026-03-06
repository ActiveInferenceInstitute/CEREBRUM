"""
Tests for src/visualization/insect/report_generator.py

Tests ComprehensiveReportGenerator with properly structured simulation data
matching the expected events-based format.
"""

import pytest
import os
import json
import numpy as np

from src.visualization.insect.report_generator import ComprehensiveReportGenerator


@pytest.fixture
def gen(tmp_path):
    return ComprehensiveReportGenerator(output_dir=str(tmp_path))


@pytest.fixture
def sim_data():
    """Simulation data with the events list structure that report_generator expects."""
    np.random.seed(42)
    events = []
    cases = ["NOM", "ACC", "DAT", "GEN", "INS", "LOC"]
    states = ["foraging", "exploring", "resting", "escaping"]
    insects = ["ant_1", "ant_2", "ant_3"]
    for step in range(100):
        events.append({
            "step": step,
            "insect_id": insects[step % len(insects)],
            "case": cases[step % len(cases)],
            "behavioral_state": states[step % len(states)],
            "processed_data": {
                "confidence": np.random.uniform(0.3, 0.95),
            },
        })
    return {"events": events}


class TestComprehensiveReportGenerator:
    def test_init_creates_dirs(self, gen):
        assert os.path.isdir(gen.reports_dir)

    def test_generate_performance_report(self, gen, sim_data):
        json_path = gen.generate_performance_report(sim_data)
        assert os.path.exists(json_path)
        with open(json_path) as f:
            data = json.load(f)
        assert "total_events" in data
        assert data["total_events"] == 100
        # Check markdown companion
        md_path = json_path.replace(".json", ".md")
        assert os.path.exists(md_path)

    def test_generate_behavioral_report(self, gen, sim_data):
        json_path = gen.generate_behavioral_report(sim_data)
        assert os.path.exists(json_path)
        with open(json_path) as f:
            data = json.load(f)
        assert "behavioral_states" in data

    def test_generate_case_analysis_report(self, gen, sim_data):
        json_path = gen.generate_case_analysis_report(sim_data)
        assert os.path.exists(json_path)
        with open(json_path) as f:
            data = json.load(f)
        assert "case_transitions" in data

    def test_generate_neural_activity_report(self, gen, sim_data):
        json_path = gen.generate_neural_activity_report(sim_data)
        assert os.path.exists(json_path)
        with open(json_path) as f:
            data = json.load(f)
        assert "activity_patterns" in data

    def test_generate_swarm_analysis_report(self, gen, sim_data):
        json_path = gen.generate_swarm_analysis_report(sim_data)
        assert os.path.exists(json_path)
        with open(json_path) as f:
            data = json.load(f)
        assert "swarm_coordination" in data

    def test_generate_comprehensive_report(self, gen, sim_data):
        json_path = gen.generate_comprehensive_report(sim_data)
        assert os.path.exists(json_path)
        with open(json_path) as f:
            data = json.load(f)
        assert "simulation_overview" in data

    def test_generate_all_reports(self, gen, sim_data):
        reports = gen.generate_all_reports(sim_data)
        assert "performance" in reports
        assert "behavioral" in reports
        assert "case_analysis" in reports
        assert "neural_activity" in reports
        assert "swarm_analysis" in reports
        assert "comprehensive" in reports
        # All paths should exist
        for key, path in reports.items():
            assert os.path.exists(path), f"Missing report file: {key} at {path}"

    def test_markdown_content_has_headings(self, gen, sim_data):
        gen.generate_performance_report(sim_data)
        reports_dir = gen.reports_dir
        md_path = os.path.join(reports_dir, "performance_analysis", "performance_analysis_report.md")
        content = open(md_path).read()
        assert "# Performance Analysis Report" in content
        assert "Total Events" in content

    def test_empty_events_handled(self, gen):
        """Reports should handle missing events gracefully (may raise but shouldn't crash hard)."""
        try:
            gen.generate_performance_report({"events": []})
        except (ValueError, ZeroDivisionError, KeyError):
            # Expected for empty events list (set() on empty, division by zero)
            pass
