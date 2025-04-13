"""
Data generation utilities for linear regression tests.
"""
import numpy as np
from typing import Tuple, Optional


class DataGenerator:
    """Generates synthetic data for linear regression tests with various properties."""
    
    @staticmethod
    def linear_data(
        n_samples: int = 100,
        slope: float = 2.0,
        intercept: float = 5.0,
        noise_level: float = 1.0,
        x_range: Tuple[float, float] = (-5, 5),
        random_seed: Optional[int] = 42
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate synthetic linear data with specified parameters.
        
        Args:
            n_samples: Number of data points to generate
            slope: True slope of the linear relationship
            intercept: True intercept of the linear relationship
            noise_level: Standard deviation of Gaussian noise
            x_range: Range of x values (min, max)
            random_seed: Random seed for reproducibility
            
        Returns:
            Tuple of (X, y) arrays
        """
        if random_seed is not None:
            np.random.seed(random_seed)
            
        # Generate X values
        X = np.random.uniform(x_range[0], x_range[1], size=n_samples)
        
        # Reshape X to column vector
        X = X.reshape(-1, 1)
        
        # Generate y values: y = intercept + slope * X + noise
        y = intercept + slope * X + np.random.normal(0, noise_level, size=X.shape)
        
        return X, y
    
    @staticmethod
    def nonlinear_data(
        n_samples: int = 100,
        function_type: str = 'quadratic',
        noise_level: float = 1.0,
        x_range: Tuple[float, float] = (-5, 5),
        random_seed: Optional[int] = 42
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate synthetic nonlinear data.
        
        Args:
            n_samples: Number of data points to generate
            function_type: Type of nonlinear function ('quadratic', 'exponential', 'sinusoidal')
            noise_level: Standard deviation of Gaussian noise
            x_range: Range of x values (min, max)
            random_seed: Random seed for reproducibility
            
        Returns:
            Tuple of (X, y) arrays
        """
        if random_seed is not None:
            np.random.seed(random_seed)
        
        # Generate X values
        X = np.random.uniform(x_range[0], x_range[1], size=n_samples)
        X = X.reshape(-1, 1)
        
        # Generate y values based on function type
        if function_type == 'quadratic':
            y = 1 + 2 * X + 3 * X**2 + np.random.normal(0, noise_level, size=X.shape)
        elif function_type == 'exponential':
            y = np.exp(X) + np.random.normal(0, noise_level, size=X.shape)
        elif function_type == 'sinusoidal':
            y = np.sin(X) + np.random.normal(0, noise_level / 3, size=X.shape)
        else:
            raise ValueError(f"Unknown function type: {function_type}")
        
        return X, y
    
    @staticmethod
    def heteroskedastic_data(
        n_samples: int = 100,
        slope: float = 2.0,
        intercept: float = 5.0,
        base_noise: float = 0.5,
        x_range: Tuple[float, float] = (-5, 5),
        random_seed: Optional[int] = 42
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate synthetic data with heteroskedasticity (non-constant variance).
        
        Args:
            n_samples: Number of data points to generate
            slope: True slope of the linear relationship
            intercept: True intercept of the linear relationship
            base_noise: Base level of noise (increases with X)
            x_range: Range of x values (min, max)
            random_seed: Random seed for reproducibility
            
        Returns:
            Tuple of (X, y) arrays
        """
        if random_seed is not None:
            np.random.seed(random_seed)
        
        # Generate X values
        X = np.random.uniform(x_range[0], x_range[1], size=n_samples)
        X = X.reshape(-1, 1)
        
        # Noise level increases with X - heteroskedasticity
        noise = np.random.normal(0, base_noise * (1 + np.abs(X)), size=X.shape)
        
        # Generate y values with heteroskedastic noise
        y = intercept + slope * X + noise
        
        return X, y
    
    @staticmethod
    def multivariate_data(
        n_samples: int = 100,
        n_features: int = 3,
        coefficients: Optional[np.ndarray] = None,
        intercept: float = 5.0,
        noise_level: float = 1.0,
        x_range: Tuple[float, float] = (-5, 5),
        correlated: bool = False,
        correlation: float = 0.7,
        random_seed: Optional[int] = 42
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate synthetic multivariate data.
        
        Args:
            n_samples: Number of data points to generate
            n_features: Number of features
            coefficients: Array of feature coefficients (default: random)
            intercept: True intercept
            noise_level: Standard deviation of Gaussian noise
            x_range: Range of x values (min, max)
            correlated: Whether to generate correlated features
            correlation: Correlation coefficient between features (if correlated=True)
            random_seed: Random seed for reproducibility
            
        Returns:
            Tuple of (X, y) arrays where X has shape (n_samples, n_features)
        """
        if random_seed is not None:
            np.random.seed(random_seed)
        
        if coefficients is None:
            coefficients = np.random.uniform(-3, 3, size=n_features)
        
        if correlated:
            # Generate correlated features
            # Start with uncorrelated data
            X_uncorr = np.random.uniform(x_range[0], x_range[1], size=(n_samples, n_features))
            
            # Create a common factor for correlation
            common_factor = np.random.uniform(x_range[0], x_range[1], size=(n_samples, 1))
            
            # Mix in the common factor to create correlation
            X = X_uncorr * (1 - correlation) + common_factor * correlation
        else:
            # Generate uncorrelated features
            X = np.random.uniform(x_range[0], x_range[1], size=(n_samples, n_features))
        
        # Generate y values: y = intercept + X * coefficients + noise
        y = intercept + X.dot(coefficients.reshape(-1, 1)) + np.random.normal(0, noise_level, size=(n_samples, 1))
        
        return X, y 