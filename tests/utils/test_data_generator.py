#!/usr/bin/env python3
"""
Tests for data_generator module.
Tests data generation utilities for CEREBRUM.
"""

import pytest
import numpy as np
import sys
from pathlib import Path

# Ensure src is in path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from src.utils.data_generator import DataGenerator


class TestLinearData:
    """Tests for linear data generation."""
    
    def test_generates_correct_shape(self):
        """Test that linear_data generates correct array shapes."""
        X, y = DataGenerator.linear_data(n_samples=50)
        assert X.shape == (50, 1)
        assert y.shape == (50,)
    
    def test_return_1d_x(self):
        """Test generating 1D X array."""
        X, y = DataGenerator.linear_data(n_samples=50, return_2d=False)
        assert X.shape == (50,)
        assert y.shape == (50,)
    
    def test_reproducibility_with_seed(self):
        """Test that same seed produces same data."""
        X1, y1 = DataGenerator.linear_data(random_seed=42)
        X2, y2 = DataGenerator.linear_data(random_seed=42)
        np.testing.assert_array_equal(X1, X2)
        np.testing.assert_array_equal(y1, y2)
    
    def test_different_seeds_different_data(self):
        """Test that different seeds produce different data."""
        X1, y1 = DataGenerator.linear_data(random_seed=42)
        X2, y2 = DataGenerator.linear_data(random_seed=123)
        assert not np.array_equal(X1, X2)
    
    def test_x_range_respected(self):
        """Test that X values are within specified range."""
        X, _ = DataGenerator.linear_data(x_range=(-10, 10), return_2d=False)
        assert X.min() >= -10
        assert X.max() <= 10
    
    def test_zero_noise(self):
        """Test with zero noise produces exact linear relationship."""
        X, y = DataGenerator.linear_data(
            n_samples=10, 
            slope=2.0, 
            intercept=1.0, 
            noise_level=0.0,
            return_2d=False
        )
        expected_y = 1.0 + 2.0 * X
        np.testing.assert_array_almost_equal(y, expected_y)


class TestPolynomialData:
    """Tests for polynomial data generation."""
    
    def test_generates_correct_shape(self):
        """Test that polynomial_data generates correct shapes."""
        X, y = DataGenerator.polynomial_data(n_samples=75)
        assert X.shape == (75, 1)
        assert y.shape == (75,)
    
    def test_return_1d_x(self):
        """Test generating 1D X array."""
        X, y = DataGenerator.polynomial_data(n_samples=50, return_2d=False)
        assert X.shape == (50,)
    
    def test_linear_coefficients(self):
        """Test that linear coefficients produce linear data."""
        X, y = DataGenerator.polynomial_data(
            n_samples=10,
            coefficients=(3.0, 2.0),  # y = 3 + 2x
            noise_level=0.0,
            return_2d=False
        )
        expected_y = 3.0 + 2.0 * X
        np.testing.assert_array_almost_equal(y, expected_y)
    
    def test_quadratic_coefficients(self):
        """Test quadratic polynomial generation."""
        X, y = DataGenerator.polynomial_data(
            n_samples=10,
            coefficients=(1.0, 0.0, 1.0),  # y = 1 + x^2
            noise_level=0.0,
            return_2d=False
        )
        expected_y = 1.0 + X**2
        np.testing.assert_array_almost_equal(y, expected_y)


class TestNormalizeArrays:
    """Tests for array normalization."""
    
    def test_1d_to_2d(self):
        """Test converting 1D X to 2D."""
        X = np.array([1, 2, 3, 4, 5])
        y = np.array([2, 4, 6, 8, 10])
        X_out, y_out = DataGenerator.normalize_arrays(X, y)
        assert X_out.shape == (5, 1)
        assert y_out.shape == (5,)
    
    def test_2d_unchanged(self):
        """Test 2D X array remains unchanged."""
        X = np.array([[1], [2], [3]])
        y = np.array([2, 4, 6])
        X_out, y_out = DataGenerator.normalize_arrays(X, y)
        assert X_out.shape == (3, 1)
    
    def test_2d_y_flattened(self):
        """Test 2D y is flattened."""
        X = np.array([1, 2, 3])
        y = np.array([[2], [4], [6]])
        X_out, y_out = DataGenerator.normalize_arrays(X, y)
        assert y_out.shape == (3,)
    
    def test_shape_mismatch_raises(self):
        """Test that mismatched shapes raise ValueError."""
        X = np.array([1, 2, 3])
        y = np.array([2, 4])
        with pytest.raises(ValueError, match="same number of samples"):
            DataGenerator.normalize_arrays(X, y)


class TestClassificationData:
    """Tests for classification data generation."""
    
    def test_generates_correct_shape(self):
        """Test that classification_data generates correct shapes."""
        X, y = DataGenerator.classification_data(n_samples=100, n_features=4, n_classes=3)
        assert X.shape == (99, 4)  # May be slightly less due to integer division
        assert y.shape == (99,)
    
    def test_correct_number_of_classes(self):
        """Test that correct number of classes are generated."""
        X, y = DataGenerator.classification_data(n_samples=100, n_classes=5)
        unique_classes = np.unique(y)
        assert len(unique_classes) == 5
    
    def test_reproducibility(self):
        """Test that same seed produces same data."""
        X1, y1 = DataGenerator.classification_data(random_seed=42)
        X2, y2 = DataGenerator.classification_data(random_seed=42)
        np.testing.assert_array_equal(X1, X2)
        np.testing.assert_array_equal(y1, y2)


class TestMultivariateData:
    """Tests for multivariate regression data generation."""
    
    def test_generates_correct_shape(self):
        """Test that multivariate_data generates correct shapes."""
        X, y = DataGenerator.multivariate_data(n_samples=50, n_features=5, n_outputs=1)
        assert X.shape == (50, 5)
        assert y.shape == (50,)
    
    def test_multiple_outputs(self):
        """Test generating multiple output dimensions."""
        X, y = DataGenerator.multivariate_data(n_samples=50, n_features=3, n_outputs=2)
        assert X.shape == (50, 3)
        assert y.shape == (50, 2)
    
    def test_reproducibility(self):
        """Test that same seed produces same data."""
        X1, y1 = DataGenerator.multivariate_data(random_seed=42)
        X2, y2 = DataGenerator.multivariate_data(random_seed=42)
        np.testing.assert_array_equal(X1, X2)
        np.testing.assert_array_equal(y1, y2)


class TestTimeSeriesData:
    """Tests for time series data generation."""
    
    def test_generates_correct_shape(self):
        """Test that time_series_data generates correct shapes."""
        t, y = DataGenerator.time_series_data(n_samples=100)
        assert t.shape == (100,)
        assert y.shape == (100,)
    
    def test_multivariate_time_series(self):
        """Test generating multivariate time series."""
        t, y = DataGenerator.time_series_data(n_samples=100, n_features=3)
        assert t.shape == (100,)
        assert y.shape == (100, 3)
    
    def test_time_indices(self):
        """Test that time indices are sequential."""
        t, y = DataGenerator.time_series_data(n_samples=50)
        np.testing.assert_array_equal(t, np.arange(50))
    
    def test_reproducibility(self):
        """Test that same seed produces same data."""
        t1, y1 = DataGenerator.time_series_data(random_seed=42)
        t2, y2 = DataGenerator.time_series_data(random_seed=42)
        np.testing.assert_array_equal(y1, y2)


class TestSplitData:
    """Tests for data splitting functionality."""
    
    def test_split_sizes(self):
        """Test that split produces correct sizes."""
        X = np.random.randn(100, 5)
        y = np.random.randn(100)
        X_train, X_test, y_train, y_test = DataGenerator.split_data(X, y, test_size=0.2)
        assert len(X_train) == 80
        assert len(X_test) == 20
        assert len(y_train) == 80
        assert len(y_test) == 20
    
    def test_different_test_size(self):
        """Test with different test size."""
        X = np.random.randn(100, 3)
        y = np.random.randn(100)
        X_train, X_test, y_train, y_test = DataGenerator.split_data(X, y, test_size=0.3)
        assert len(X_train) == 70
        assert len(X_test) == 30
    
    def test_reproducibility(self):
        """Test that same seed produces same split."""
        X = np.random.randn(50, 2)
        y = np.random.randn(50)
        X_tr1, X_te1, y_tr1, y_te1 = DataGenerator.split_data(X, y, random_seed=42)
        X_tr2, X_te2, y_tr2, y_te2 = DataGenerator.split_data(X, y, random_seed=42)
        np.testing.assert_array_equal(X_tr1, X_tr2)
        np.testing.assert_array_equal(X_te1, X_te2)
    
    def test_no_overlap(self):
        """Test that train and test sets don't overlap."""
        X = np.arange(100).reshape(-1, 1)
        y = np.arange(100)
        X_train, X_test, y_train, y_test = DataGenerator.split_data(X, y, test_size=0.2)
        # Check no values in test are in train
        train_set = set(X_train.flatten())
        test_set = set(X_test.flatten())
        assert len(train_set.intersection(test_set)) == 0
