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
        random_seed: Optional[int] = 42
    ) -> Tuple[np.ndarray, np.ndarray]:
        """Generate linear regression data with specified parameters."""
        if random_seed is not None:
            np.random.seed(random_seed)
            
        # Generate X values within the specified range
        X = np.random.uniform(x_range[0], x_range[1], n_samples).reshape(-1, 1)
        
        # Generate y values with noise
        y = intercept + slope * X.flatten() + np.random.normal(0, noise_level, n_samples)
        
        return X, y 