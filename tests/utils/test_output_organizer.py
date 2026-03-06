"""
Tests for src/utils/output_organizer.py

Tests SimulationOutputOrganizer with real temporary directories,
verifying directory creation, event saving, data saving, report saving,
summary generation, and cleanup functionality.
"""

import pytest
import os
import json

from src.utils.output_organizer import SimulationOutputOrganizer


@pytest.fixture
def organizer(tmp_path, monkeypatch):
    """Create a SimulationOutputOrganizer using a temp directory."""
    monkeypatch.setattr(
        "src.utils.output_organizer.get_output_dir",
        lambda: str(tmp_path)
    )
    return SimulationOutputOrganizer(simulation_name="test_sim")


class TestOrganizerInit:
    """Test initialization and directory creation."""

    def test_creates_simulation_directory(self, organizer):
        sim_path = organizer.get_simulation_path()
        assert os.path.isdir(sim_path)

    def test_creates_logs_dir(self, organizer):
        assert os.path.isdir(organizer.get_logs_path())

    def test_creates_visualizations_dir(self, organizer):
        assert os.path.isdir(organizer.get_visualizations_path())

    def test_creates_data_dir(self, organizer):
        assert os.path.isdir(organizer.get_data_path())

    def test_creates_reports_dir(self, organizer):
        assert os.path.isdir(organizer.get_reports_path())

    def test_simulation_name_in_path(self, organizer):
        assert "test_sim" in organizer.get_simulation_path()


class TestSaveEvents:
    """Test event saving functionality."""

    def test_saves_event_file(self, organizer):
        event = {"type": "action", "data": "foraging_started"}
        organizer.save_simulation_event(event)
        # Check that a JSON file was created somewhere under simulation_dir
        json_files = list(_find_json_files(organizer.simulation_dir))
        assert len(json_files) >= 1

    def test_saves_event_with_custom_name(self, organizer):
        event = {"type": "observation"}
        organizer.save_simulation_event(event, filename="custom_event.json")
        json_files = list(_find_json_files(organizer.simulation_dir))
        custom = [f for f in json_files if "custom_event" in f]
        assert len(custom) >= 1


class TestSaveCasePerformance:
    """Test case performance saving."""

    def test_saves_case_data(self, organizer):
        data = {"case": "NOMINATIVE", "free_energy": 1.5}
        organizer.save_case_performance(data)
        json_files = list(_find_json_files(organizer.simulation_dir))
        assert len(json_files) >= 1


class TestSaveBehavioralData:
    """Test behavioral data saving."""

    def test_saves_behavior_data(self, organizer):
        data = {"state": "foraging", "transitions": 5}
        organizer.save_behavioral_data(data)
        json_files = list(_find_json_files(organizer.simulation_dir))
        assert len(json_files) >= 1


class TestSaveReport:
    """Test report saving."""

    def test_saves_report(self, organizer):
        report = {"summary": "Test passed", "metrics": {"accuracy": 0.95}}
        organizer.save_report(report, report_type="test_report")
        json_files = list(_find_json_files(organizer.simulation_dir))
        assert len(json_files) >= 1


class TestSaveData:
    """Test data saving."""

    def test_saves_data(self, organizer):
        data = {"values": [1, 2, 3]}
        organizer.save_data(data, data_type="test_data")
        json_files = list(_find_json_files(organizer.simulation_dir))
        assert len(json_files) >= 1


class TestSimulationSummary:
    """Test simulation summary creation."""

    def test_creates_summary(self, organizer):
        summary = {"total_steps": 100, "final_free_energy": 0.8}
        organizer.create_simulation_summary(summary)
        summary_path = os.path.join(organizer.simulation_dir, "simulation_summary.json")
        assert os.path.exists(summary_path)
        with open(summary_path) as f:
            saved = json.load(f)
        # Summary wraps user data under "summary_data"
        assert saved["summary_data"]["total_steps"] == 100


class TestSaveVisualization:
    """Test visualization saving."""

    def test_saves_matplotlib_figure(self, organizer):
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
        fig, ax = plt.subplots()
        ax.plot([1, 2, 3], [1, 4, 9])
        organizer.save_visualization(fig, "test_viz.png")
        _has_png = any(
            f.endswith(".png")
            for root, _, files in os.walk(organizer.simulation_dir)
            for f in files
        )
        assert _has_png
        plt.close(fig)


def _find_json_files(root_dir):
    """Recursively find all .json files under root_dir."""
    for root, _, files in os.walk(root_dir):
        for f in files:
            if f.endswith(".json"):
                yield os.path.join(root, f)


class TestCleanupOldOutputs:
    """Test cleanup_old_outputs functionality (L168-185)."""

    def test_cleanup_keeps_n_most_recent(self, tmp_path, monkeypatch):
        monkeypatch.setattr(
            "src.utils.output_organizer.get_output_dir",
            lambda: str(tmp_path)
        )
        # Pre-create 6 old simulation dirs
        for i in range(6):
            d = tmp_path / f"test_sim_2026010{i}_120000"
            d.mkdir()
        # Create organizer (adds 1 more dir)
        org = SimulationOutputOrganizer(simulation_name="test_sim")
        # Cleanup, keeping last 3
        org.cleanup_old_outputs(keep_last_n=3)
        remaining = [d for d in os.listdir(str(tmp_path))
                     if d.startswith("test_sim") and os.path.isdir(tmp_path / d)]
        assert len(remaining) <= 3

    def test_cleanup_no_extra_dirs(self, tmp_path, monkeypatch):
        monkeypatch.setattr(
            "src.utils.output_organizer.get_output_dir",
            lambda: str(tmp_path)
        )
        org = SimulationOutputOrganizer(simulation_name="test_sim")
        # Only 1 dir exists, keeping 5 → nothing to remove
        org.cleanup_old_outputs(keep_last_n=5)
        remaining = [d for d in os.listdir(str(tmp_path))
                     if d.startswith("test_sim") and os.path.isdir(tmp_path / d)]
        assert len(remaining) == 1


class TestOrganizeExistingOutputs:
    """Test organize_existing_outputs function (L194-223)."""

    def test_organizes_png_files(self, tmp_path, monkeypatch):
        from src.utils.output_organizer import organize_existing_outputs
        monkeypatch.setattr(
            "src.utils.output_organizer.get_output_dir",
            lambda: str(tmp_path)
        )
        # Create loose files
        (tmp_path / "viz_plot.png").write_bytes(b"\x89PNG\r\n")
        (tmp_path / "data.json").write_text('{"x": 1}')
        insect_dir = tmp_path / "insect_logs"
        insect_dir.mkdir()
        (insect_dir / "log.txt").write_text("entry")

        result = organize_existing_outputs()
        assert os.path.isdir(result)
        assert "organized_outputs" in result
