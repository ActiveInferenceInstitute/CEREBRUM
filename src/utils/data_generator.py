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