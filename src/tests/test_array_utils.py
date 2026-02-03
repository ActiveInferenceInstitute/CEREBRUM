#!/usr/bin/env python3
"""
Tests for array_utils module.
Tests array handling utilities for CEREBRUM.
"""

import pytest
import numpy as np
import sys
from pathlib import Path

# Ensure src is in path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from src.utils.array_utils import (
    safe_array_index,
    get_xy_values,
    ensure_1d,
    ensure_2d,
    validate_regression_data,
    safe_predict,
    create_residual_lines_data,
    safe_metrics_calculation
)


class TestSafeArrayIndex:
    """Tests for safe_array_index function."""
    
    def test_1d_array(self):
        """Test indexing 1D array."""
        arr = np.array([1.0, 2.0, 3.0, 4.0])
        assert safe_array_index(arr, 0) == 1.0
        assert safe_array_index(arr, 2) == 3.0
    
    def test_2d_array_single_column(self):
        """Test indexing 2D array with single column."""
        arr = np.array([[1.0], [2.0], [3.0]])
        assert safe_array_index(arr, 0) == 1.0
        assert safe_array_index(arr, 2) == 3.0
    
    def test_2d_array_multiple_columns(self):
        """Test indexing 2D array with multiple columns."""
        arr = np.array([[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]])
        result = safe_array_index(arr, 1)
        np.testing.assert_array_equal(result, [3.0, 4.0])
    
    def test_2d_array_column_axis(self):
        """Test indexing along column axis."""
        arr = np.array([[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]])
        result = safe_array_index(arr, 1, axis=1)
        np.testing.assert_array_equal(result, [2.0, 4.0, 6.0])


class TestGetXYValues:
    """Tests for get_xy_values function."""
    
    def test_1d_arrays(self):
        """Test with 1D arrays."""
        X = np.array([1.0, 2.0, 3.0])
        y = np.array([4.0, 5.0, 6.0])
        x_val, y_val = get_xy_values(X, y, 1)
        assert x_val == 2.0
        assert y_val == 5.0
    
    def test_2d_arrays(self):
        """Test with 2D arrays."""
        X = np.array([[1.0], [2.0], [3.0]])
        y = np.array([[4.0], [5.0], [6.0]])
        x_val, y_val = get_xy_values(X, y, 0)
        assert x_val == 1.0
        assert y_val == 4.0
    
    def test_returns_floats(self):
        """Test that return values are floats."""
        X = np.array([1, 2, 3])
        y = np.array([4, 5, 6])
        x_val, y_val = get_xy_values(X, y, 0)
        assert isinstance(x_val, float)
        assert isinstance(y_val, float)


class TestEnsure1D:
    """Tests for ensure_1d function."""
    
    def test_already_1d(self):
        """Test array already 1D."""
        arr = np.array([1.0, 2.0, 3.0])
        result = ensure_1d(arr)
        assert result.ndim == 1
        np.testing.assert_array_equal(result, arr)
    
    def test_2d_single_column(self):
        """Test 2D array with single column."""
        arr = np.array([[1.0], [2.0], [3.0]])
        result = ensure_1d(arr)
        assert result.ndim == 1
        np.testing.assert_array_equal(result, [1.0, 2.0, 3.0])
    
    def test_2d_multiple_columns(self):
        """Test 2D array with multiple columns flattens."""
        arr = np.array([[1.0, 2.0], [3.0, 4.0]])
        result = ensure_1d(arr)
        assert result.ndim == 1
        assert len(result) == 4


class TestEnsure2D:
    """Tests for ensure_2d function."""
    
    def test_1d_to_2d(self):
        """Test 1D array to 2D."""
        arr = np.array([1.0, 2.0, 3.0])
        result = ensure_2d(arr)
        assert result.ndim == 2
        assert result.shape == (3, 1)
    
    def test_already_2d(self):
        """Test array already 2D."""
        arr = np.array([[1.0, 2.0], [3.0, 4.0]])
        result = ensure_2d(arr)
        assert result.ndim == 2
        np.testing.assert_array_equal(result, arr)


class TestValidateRegressionData:
    """Tests for validate_regression_data function."""
    
    def test_valid_data(self):
        """Test with valid data."""
        X = np.array([[1.0], [2.0], [3.0]])
        y = np.array([4.0, 5.0, 6.0])
        X_out, y_out = validate_regression_data(X, y)
        assert X_out.ndim == 2
        assert y_out.ndim == 1
        assert X_out.shape[0] == y_out.shape[0]
    
    def test_shape_mismatch_raises(self):
        """Test that shape mismatch raises ValueError."""
        X = np.array([[1.0], [2.0], [3.0]])
        y = np.array([4.0, 5.0])
        with pytest.raises(ValueError, match="same number of samples"):
            validate_regression_data(X, y)
    
    def test_nan_in_x_raises(self):
        """Test that NaN in X raises ValueError."""
        X = np.array([[1.0], [np.nan], [3.0]])
        y = np.array([4.0, 5.0, 6.0])
        with pytest.raises(ValueError, match="NaN or infinite"):
            validate_regression_data(X, y)
    
    def test_inf_in_y_raises(self):
        """Test that infinite in y raises ValueError."""
        X = np.array([[1.0], [2.0], [3.0]])
        y = np.array([4.0, np.inf, 6.0])
        with pytest.raises(ValueError, match="NaN or infinite"):
            validate_regression_data(X, y)


class TestSafeMetricsCalculation:
    """Tests for safe_metrics_calculation function."""
    
    def test_perfect_predictions(self):
        """Test with perfect predictions."""
        y_true = np.array([1.0, 2.0, 3.0])
        y_pred = np.array([1.0, 2.0, 3.0])
        metrics = safe_metrics_calculation(y_true, y_pred)
        assert metrics['mse'] == 0.0
        assert metrics['rmse'] == 0.0
        assert metrics['mae'] == 0.0
        assert metrics['r2'] == 1.0
    
    def test_returns_dict_with_keys(self):
        """Test that returns dict with expected keys."""
        y_true = np.array([1.0, 2.0, 3.0])
        y_pred = np.array([1.1, 2.1, 3.1])
        metrics = safe_metrics_calculation(y_true, y_pred)
        assert 'mse' in metrics
        assert 'rmse' in metrics
        assert 'mae' in metrics
        assert 'r2' in metrics
    
    def test_length_mismatch_raises(self):
        """Test that length mismatch raises ValueError."""
        y_true = np.array([1.0, 2.0, 3.0])
        y_pred = np.array([1.0, 2.0])
        with pytest.raises(ValueError, match="same length"):
            safe_metrics_calculation(y_true, y_pred)


class TestCreateResidualLinesData:
    """Tests for create_residual_lines_data function."""
    
    def test_creates_correct_format(self):
        """Test output format is correct."""
        X = np.array([[1.0], [2.0], [3.0]])
        y_true = np.array([4.0, 5.0, 6.0])
        y_pred = np.array([4.1, 5.1, 6.1])
        result = create_residual_lines_data(X, y_true, y_pred)
        assert len(result) == 3
        assert len(result[0]) == 3  # (x_val, y_true_val, y_pred_val)
    
    def test_values_extracted_correctly(self):
        """Test values are extracted correctly."""
        X = np.array([[1.0], [2.0]])
        y_true = np.array([3.0, 4.0])
        y_pred = np.array([3.5, 4.5])
        result = create_residual_lines_data(X, y_true, y_pred)
        assert result[0] == (1.0, 3.0, 3.5)
        assert result[1] == (2.0, 4.0, 4.5)
