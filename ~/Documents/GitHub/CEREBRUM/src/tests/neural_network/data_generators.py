import numpy as np
from typing import Tuple

# Data generator for Neural Network test data
class DataGenerator:
    """Generates synthetic data for Neural Network tests."""
    
    @staticmethod
    def regression_data(
        n_samples: int = 200,
        input_dim: int = 1,
        output_dim: int = 1,
        noise_level: float = 0.1,
        random_seed: int = 42
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate synthetic regression data.
        
        Args:
            n_samples: Number of data points
            input_dim: Input dimension
            output_dim: Output dimension
            noise_level: Noise level in the data
            random_seed: Random seed for reproducibility
            
        Returns:
            Tuple of (X, y) arrays
        """
        np.random.seed(random_seed)
        
        # Generate random inputs
        X = np.random.randn(n_samples, input_dim)
        
        # Generate true weights and biases
        W = np.random.randn(input_dim, output_dim)
        b = np.random.randn(output_dim)
        
        # Generate outputs with noise
        y_true = np.dot(X, W) + b
        noise = np.random.normal(0, noise_level, (n_samples, output_dim))
        y = y_true + noise
        
        return X, y
    
    @staticmethod
    def classification_data(
        n_samples: int = 200,
        n_classes: int = 2,
        n_features: int = 2,
        class_sep: float = 1.0,
        random_seed: int = 42
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate synthetic classification data.
        
        Args:
            n_samples: Number of data points
            n_classes: Number of classes
            n_features: Number of features
            class_sep: Class separation
            random_seed: Random seed for reproducibility
            
        Returns:
            Tuple of (X, y) arrays
        """
        np.random.seed(random_seed)
        
        # Generate class centers
        centers = np.random.randn(n_classes, n_features) * class_sep
        
        # Initialize data arrays
        X = np.zeros((n_samples, n_features))
        y = np.zeros(n_samples, dtype=int)
        
        # Generate data for each class
        samples_per_class = n_samples // n_classes
        for i in range(n_classes):
            start_idx = i * samples_per_class
            end_idx = (i + 1) * samples_per_class if i < n_classes - 1 else n_samples
            n_class_samples = end_idx - start_idx
            
            # Generate samples around the class center
            X[start_idx:end_idx] = centers[i] + np.random.randn(n_class_samples, n_features)
            y[start_idx:end_idx] = i
        
        # Shuffle the data
        idx = np.random.permutation(n_samples)
        X = X[idx]
        y = y[idx]
        
        # Convert to one-hot encoding if more than 2 classes
        if n_classes > 2:
            y_one_hot = np.zeros((n_samples, n_classes))
            for i in range(n_samples):
                y_one_hot[i, y[i]] = 1
            return X, y_one_hot
        
        return X, y.reshape(-1, 1) 