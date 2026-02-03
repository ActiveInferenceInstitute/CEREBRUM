#!/usr/bin/env python3
"""
Data Generation Module for CEREBRUM
Provides utilities for generating test data for regression models
"""

from typing import Tuple, Optional
import numpy as np

class DataGenerator:
    """Data generation utilities for regression tests."""
    
    @staticmethod
    def linear_data(
        n_samples: int = 100,
        slope: float = 2.0,
        intercept: float = 5.0,
        noise_level: float = 1.0,
        x_range: Tuple[float, float] = (-5, 5),
        random_seed: Optional[int] = 42,
        return_2d: bool = True
    ) -> Tuple[np.ndarray, np.ndarray]:
        """Generate linear regression data with specified parameters.
        
        Args:
            n_samples: Number of data points to generate
            slope: True slope of the relationship
            intercept: True intercept of the relationship  
            noise_level: Standard deviation of noise to add
            x_range: Range for X values (min, max)
            random_seed: Random seed for reproducibility
            return_2d: If True, return X as 2D array (n_samples, 1), else 1D
            
        Returns:
            X: Feature array, shape (n_samples, 1) if return_2d=True, else (n_samples,)
            y: Target array, shape (n_samples,)
        """
        if random_seed is not None:
            np.random.seed(random_seed)
            
        # Generate X values within the specified range
        X = np.random.uniform(x_range[0], x_range[1], n_samples)
        
        # Generate y values with noise
        y = intercept + slope * X + np.random.normal(0, noise_level, n_samples)
        
        # Reshape X to 2D if requested (standard for sklearn)
        if return_2d:
            X = X.reshape(-1, 1)
        
        return X, y
    
    @staticmethod
    def polynomial_data(
        n_samples: int = 100,
        coefficients: Tuple[float, ...] = (1.0, 2.0, -0.5),
        noise_level: float = 1.0,
        x_range: Tuple[float, float] = (-3, 3),
        random_seed: Optional[int] = 42,
        return_2d: bool = True
    ) -> Tuple[np.ndarray, np.ndarray]:
        """Generate polynomial regression data.
        
        Args:
            n_samples: Number of data points to generate
            coefficients: Polynomial coefficients (constant, linear, quadratic, ...)
            noise_level: Standard deviation of noise to add
            x_range: Range for X values (min, max)
            random_seed: Random seed for reproducibility
            return_2d: If True, return X as 2D array (n_samples, 1), else 1D
            
        Returns:
            X: Feature array
            y: Target array following polynomial relationship
        """
        if random_seed is not None:
            np.random.seed(random_seed)
            
        # Generate X values
        X = np.random.uniform(x_range[0], x_range[1], n_samples)
        
        # Generate polynomial y values
        y = np.zeros(n_samples)
        for i, coef in enumerate(coefficients):
            y += coef * (X ** i)
            
        # Add noise
        y += np.random.normal(0, noise_level, n_samples)
        
        # Reshape X to 2D if requested
        if return_2d:
            X = X.reshape(-1, 1)
            
        return X, y
    
    @staticmethod
    def normalize_arrays(X: np.ndarray, y: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
        """Ensure arrays have consistent shapes for regression models.
        
        Args:
            X: Feature array (any shape)
            y: Target array (any shape)
            
        Returns:
            X: Feature array with shape (n_samples, n_features)
            y: Target array with shape (n_samples,)
        """
        # Ensure X is 2D
        if X.ndim == 1:
            X = X.reshape(-1, 1)
        elif X.ndim > 2:
            X = X.reshape(X.shape[0], -1)
            
        # Ensure y is 1D
        if y.ndim > 1:
            y = y.flatten()
            
        # Ensure same number of samples
        if X.shape[0] != y.shape[0]:
            raise ValueError(f"X and y must have same number of samples: {X.shape[0]} vs {y.shape[0]}")
            
        return X, y
    
    @staticmethod
    def classification_data(
        n_samples: int = 100,
        n_features: int = 2,
        n_classes: int = 2,
        class_sep: float = 1.0,
        random_seed: Optional[int] = 42
    ) -> Tuple[np.ndarray, np.ndarray]:
        """Generate classification data with specified parameters.
        
        Args:
            n_samples: Number of data points to generate
            n_features: Number of features per sample
            n_classes: Number of classes
            class_sep: Separation between class centers
            random_seed: Random seed for reproducibility
            
        Returns:
            X: Feature array, shape (n_samples, n_features)
            y: Class labels, shape (n_samples,)
        """
        if random_seed is not None:
            np.random.seed(random_seed)
            
        samples_per_class = n_samples // n_classes
        X_list = []
        y_list = []
        
        for class_idx in range(n_classes):
            # Generate class center
            center = np.zeros(n_features)
            center[class_idx % n_features] = class_sep * class_idx
            
            # Generate samples around center
            samples = np.random.randn(samples_per_class, n_features) + center
            X_list.append(samples)
            y_list.append(np.full(samples_per_class, class_idx))
        
        X = np.vstack(X_list)
        y = np.hstack(y_list)
        
        # Shuffle
        indices = np.random.permutation(len(y))
        return X[indices], y[indices]
    
    @staticmethod
    def multivariate_data(
        n_samples: int = 100,
        n_features: int = 3,
        n_outputs: int = 1,
        noise_level: float = 0.1,
        random_seed: Optional[int] = 42
    ) -> Tuple[np.ndarray, np.ndarray]:
        """Generate multivariate regression data.
        
        Args:
            n_samples: Number of data points to generate
            n_features: Number of input features
            n_outputs: Number of output dimensions
            noise_level: Standard deviation of noise to add
            random_seed: Random seed for reproducibility
            
        Returns:
            X: Feature array, shape (n_samples, n_features)
            y: Target array, shape (n_samples, n_outputs) or (n_samples,) if n_outputs=1
        """
        if random_seed is not None:
            np.random.seed(random_seed)
            
        # Generate random features
        X = np.random.randn(n_samples, n_features)
        
        # Generate random weights
        weights = np.random.randn(n_features, n_outputs)
        
        # Generate targets
        y = X @ weights + np.random.randn(n_samples, n_outputs) * noise_level
        
        if n_outputs == 1:
            y = y.flatten()
            
        return X, y
    
    @staticmethod
    def time_series_data(
        n_samples: int = 100,
        n_features: int = 1,
        trend: float = 0.1,
        seasonality_period: int = 12,
        seasonality_amplitude: float = 1.0,
        noise_level: float = 0.5,
        random_seed: Optional[int] = 42
    ) -> Tuple[np.ndarray, np.ndarray]:
        """Generate time series data with trend and seasonality.
        
        Args:
            n_samples: Number of time steps
            n_features: Number of features (multivariate time series)
            trend: Linear trend coefficient
            seasonality_period: Period of seasonal component
            seasonality_amplitude: Amplitude of seasonal component
            noise_level: Standard deviation of noise
            random_seed: Random seed for reproducibility
            
        Returns:
            t: Time indices, shape (n_samples,)
            y: Time series values, shape (n_samples, n_features) or (n_samples,)
        """
        if random_seed is not None:
            np.random.seed(random_seed)
            
        t = np.arange(n_samples)
        
        # Generate components for each feature
        y_list = []
        for _ in range(n_features):
            # Trend + Seasonality + Noise
            y_feature = (
                trend * t +
                seasonality_amplitude * np.sin(2 * np.pi * t / seasonality_period) +
                np.random.randn(n_samples) * noise_level
            )
            y_list.append(y_feature)
        
        if n_features == 1:
            return t, y_list[0]
        else:
            return t, np.column_stack(y_list)
    
    @staticmethod
    def split_data(
        X: np.ndarray,
        y: np.ndarray,
        test_size: float = 0.2,
        random_seed: Optional[int] = 42
    ) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
        """Split data into training and test sets.
        
        Args:
            X: Feature array
            y: Target array
            test_size: Fraction of data to use for testing (0.0 to 1.0)
            random_seed: Random seed for reproducibility
            
        Returns:
            X_train, X_test, y_train, y_test
        """
        if random_seed is not None:
            np.random.seed(random_seed)
            
        n_samples = len(X)
        n_test = int(n_samples * test_size)
        
        # Shuffle indices
        indices = np.random.permutation(n_samples)
        
        test_indices = indices[:n_test]
        train_indices = indices[n_test:]
        
        return X[train_indices], X[test_indices], y[train_indices], y[test_indices] 