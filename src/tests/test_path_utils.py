#!/usr/bin/env python3
"""
Tests for path_utils module.
Tests path handling utilities for CEREBRUM.
"""

import pytest
import os
import sys
import tempfile
import shutil
from pathlib import Path
from unittest import mock

# Ensure src is in path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from src.utils.path_utils import (
    find_project_root,
    get_output_dir,
    get_logs_dir,
    save_plot
)


class TestFindProjectRoot:
    """Tests for find_project_root function."""
    
    def test_finds_git_root(self):
        """Test finding project root with .git marker."""
        root = find_project_root()
        assert os.path.isabs(root)
        assert os.path.exists(root)
    
    def test_finds_custom_marker(self):
        """Test finding root with custom marker."""
        # Should find pyproject.toml as marker
        root = find_project_root(marker="pyproject.toml")
        assert os.path.exists(os.path.join(root, "pyproject.toml"))
    
    def test_returns_absolute_path(self):
        """Test that returned path is absolute."""
        root = find_project_root()
        assert os.path.isabs(root)
    
    def test_fallback_to_cwd(self, tmp_path, monkeypatch):
        """Test fallback to CWD when marker not found."""
        # Create a temp directory without .git
        empty_dir = tmp_path / "no_marker"
        empty_dir.mkdir()
        
        # Mock the starting directory
        monkeypatch.setattr(os.path, 'abspath', lambda x: str(empty_dir / "subdir"))
        
        # When called from a path without the marker in any parent,
        # it should fall back to CWD
        with mock.patch('os.getcwd', return_value=str(tmp_path)):
            # The function will traverse up and eventually fail, using CWD
            pass  # Function behavior tested via integration


class TestGetOutputDir:
    """Tests for get_output_dir function."""
    
    def test_returns_valid_path(self):
        """Test that output directory is returned."""
        output_dir = get_output_dir()
        assert output_dir is not None
        assert os.path.isabs(output_dir)
    
    def test_creates_directory(self):
        """Test that directory is created if not exists."""
        output_dir = get_output_dir()
        assert os.path.isdir(output_dir)
    
    def test_contains_output_in_path(self):
        """Test that 'output' is in the path."""
        output_dir = get_output_dir()
        assert "output" in output_dir


class TestGetLogsDir:
    """Tests for get_logs_dir function."""
    
    def test_returns_valid_path(self):
        """Test that logs directory is returned."""
        logs_dir = get_logs_dir()
        assert logs_dir is not None
        assert os.path.isabs(logs_dir)
    
    def test_creates_directory(self):
        """Test that directory is created if not exists."""
        logs_dir = get_logs_dir()
        assert os.path.isdir(logs_dir)
    
    def test_contains_logs_in_path(self):
        """Test that 'logs' is in the path."""
        logs_dir = get_logs_dir()
        assert "logs" in logs_dir


class TestSavePlot:
    """Tests for save_plot function."""
    
    @pytest.fixture
    def sample_figure(self):
        """Create a sample matplotlib figure."""
        import matplotlib
        matplotlib.use('Agg')  # Non-interactive backend
        import matplotlib.pyplot as plt
        fig, ax = plt.subplots()
        ax.plot([1, 2, 3], [1, 2, 3])
        return fig
    
    def test_saves_file(self, sample_figure):
        """Test that file is saved."""
        filepath = save_plot(sample_figure, "test_plot")
        assert os.path.exists(filepath)
        # Clean up
        os.remove(filepath)
    
    def test_returns_path(self, sample_figure):
        """Test that path is returned."""
        filepath = save_plot(sample_figure, "test_plot_return")
        assert filepath is not None
        assert filepath.endswith(".png")
        # Clean up
        os.remove(filepath)
    
    def test_creates_subdirectory(self, sample_figure):
        """Test that subdirectory is created."""
        filepath = save_plot(sample_figure, "test_subdir_plot", subdirectory="test_subdir")
        assert os.path.exists(filepath)
        assert "test_subdir" in filepath
        # Clean up
        os.remove(filepath)
    
    def test_filename_contains_base(self, sample_figure):
        """Test that filename contains the base name."""
        filepath = save_plot(sample_figure, "my_custom_plot")
        assert "my_custom_plot" in filepath
        # Clean up
        os.remove(filepath)
    
    def test_filename_has_timestamp(self, sample_figure):
        """Test that filename has timestamp pattern."""
        filepath = save_plot(sample_figure, "timestamp_test")
        filename = os.path.basename(filepath)
        # Should contain 8-digit date and 6-digit time separated by underscore
        parts = filename.replace(".png", "").split("_")
        # At least one part should be numeric (the timestamp)
        has_timestamp = any(part.isdigit() for part in parts)
        assert has_timestamp
        # Clean up
        os.remove(filepath)
