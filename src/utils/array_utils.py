#!/usr/bin/env python3
"""
Array Utilities for CEREBRUM
Provides consistent array handling across different test cases
"""

import numpy as np
from typing import Union, Tuple, Any

def safe_array_index(arr: np.ndarray, index: int, axis: int = 0) -> Union[float, np.ndarray]:
    """Safely index into arrays of different dimensions.
    
    Args:
        arr: Array to index into
        index: Index to extract
        axis: Axis to index along (0 for rows, 1 for columns)
        
    Returns:
        The indexed value or subarray
    """
    if arr.ndim == 1:
        return arr[index]
    elif arr.ndim == 2:
        if axis == 0:
            return arr[index, 0] if arr.shape[1] == 1 else arr[index]
        else:
            return arr[:, index]
    else:
        # For higher dimensional arrays, flatten and index
        return arr.flatten()[index]

def get_xy_values(X: np.ndarray, y: np.ndarray, index: int) -> Tuple[float, float]:
    """Extract x and y values at a given index, handling different array shapes.
    
    Args:
        X: Feature array (1D or 2D)
        y: Target array (1D or 2D)
        index: Index to extract
        
    Returns:
        Tuple of (x_value, y_value) as floats
    """
    x_val = safe_array_index(X, index)
    y_val = safe_array_index(y, index)
    
    # Ensure we return scalars
    if isinstance(x_val, np.ndarray):
        x_val = x_val.item() if x_val.size == 1 else x_val[0]
    if isinstance(y_val, np.ndarray):
        y_val = y_val.item() if y_val.size == 1 else y_val[0]
        
    return float(x_val), float(y_val)

def ensure_1d(arr: np.ndarray) -> np.ndarray:
    """Ensure array is 1D.
    
    Args:
        arr: Input array
        
    Returns:
        1D array
    """
    if arr.ndim == 1:
        return arr
    elif arr.ndim == 2 and arr.shape[1] == 1:
        return arr.flatten()
    else:
        return arr.flatten()

def ensure_2d(arr: np.ndarray) -> np.ndarray:
    """Ensure array is 2D with shape (n_samples, n_features).
    
    Args:
        arr: Input array
        
    Returns:
        2D array with shape (n_samples, n_features)
    """
    if arr.ndim == 1:
        return arr.reshape(-1, 1)
    elif arr.ndim == 2:
        return arr
    else:
        # For higher dimensions, reshape to 2D
        return arr.reshape(arr.shape[0], -1)

def validate_regression_data(X: np.ndarray, y: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
    """Validate and normalize regression data.
    
    Args:
        X: Feature array
        y: Target array
        
    Returns:
        Validated and normalized (X, y) arrays
        
    Raises:
        ValueError: If arrays have incompatible shapes
    """
    # Ensure proper shapes
    X = ensure_2d(X)
    y = ensure_1d(y)
    
    # Check sample count consistency
    if X.shape[0] != y.shape[0]:
        raise ValueError(f"X and y must have same number of samples: {X.shape[0]} vs {y.shape[0]}")
    
    # Check for NaN or infinite values
    if np.any(np.isnan(X)) or np.any(np.isinf(X)):
        raise ValueError("X contains NaN or infinite values")
    if np.any(np.isnan(y)) or np.any(np.isinf(y)):
        raise ValueError("y contains NaN or infinite values")
        
    return X, y

def safe_predict(model: Any, X: np.ndarray) -> np.ndarray:
    """Safely make predictions with a model, handling different array shapes.
    
    Args:
        model: Model with predict method
        X: Input features
        
    Returns:
        Predictions as 1D array
    """
    X = ensure_2d(X)
    predictions = model.predict(X)
    return ensure_1d(predictions)

def create_residual_lines_data(X: np.ndarray, y_true: np.ndarray, y_pred: np.ndarray) -> list:
    """Create data for plotting residual lines.
    
    Args:
        X: Feature array
        y_true: True target values
        y_pred: Predicted values
        
    Returns:
        List of tuples (x_val, y_true_val, y_pred_val) for each sample
    """
    X, y_true = validate_regression_data(X, y_true)
    y_pred = ensure_1d(y_pred)
    
    residual_data = []
    for i in range(len(X)):
        x_val, y_true_val = get_xy_values(X, y_true, i)
        y_pred_val = float(y_pred[i])
        residual_data.append((x_val, y_true_val, y_pred_val))
    
    return residual_data

def safe_metrics_calculation(y_true: np.ndarray, y_pred: np.ndarray) -> dict:
    """Safely calculate regression metrics.
    
    Args:
        y_true: True target values
        y_pred: Predicted values
        
    Returns:
        Dictionary of metrics
    """
    from sklearn.metrics import mean_squared_error, r2_score, mean_absolute_error
    
    y_true = ensure_1d(y_true)
    y_pred = ensure_1d(y_pred)
    
    if len(y_true) != len(y_pred):
        raise ValueError(f"y_true and y_pred must have same length: {len(y_true)} vs {len(y_pred)}")
    
    return {
        'mse': float(mean_squared_error(y_true, y_pred)),
        'rmse': float(np.sqrt(mean_squared_error(y_true, y_pred))),
        'mae': float(mean_absolute_error(y_true, y_pred)),
        'r2': float(r2_score(y_true, y_pred))
    } 