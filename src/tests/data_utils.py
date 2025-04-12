"""
Utilities for loading and working with test data.
"""
import os
import json
import numpy as np
from pathlib import Path
from typing import Dict, Any, List, Optional, Union, Tuple

def get_test_data_dir() -> Path:
    """
    Get the path to the test data directory.
    
    Returns:
        Path to the test data directory
    """
    return Path(__file__).parent / "data"

def load_json_test_data(filename: str) -> Dict[str, Any]:
    """
    Load test data from a JSON file.
    
    Args:
        filename: Name of the JSON file in the test data directory
        
    Returns:
        Dictionary containing the loaded JSON data
    """
    data_path = get_test_data_dir() / filename
    with open(data_path, 'r') as f:
        return json.load(f)

def load_sample_model_data() -> Dict[str, Any]:
    """
    Load the sample model data.
    
    Returns:
        Dictionary containing model test data
    """
    return load_json_test_data("sample_model_data.json")

def get_test_input_vectors() -> np.ndarray:
    """
    Get sample input vectors for testing.
    
    Returns:
        NumPy array of input vectors
    """
    data = load_sample_model_data()
    return np.array(data["test_vectors"]["input"])

def get_test_output_vectors() -> np.ndarray:
    """
    Get sample expected output vectors for testing.
    
    Returns:
        NumPy array of expected output vectors
    """
    data = load_sample_model_data()
    return np.array(data["test_vectors"]["expected_output"])

def get_model_configs() -> List[Dict[str, Any]]:
    """
    Get sample model configurations for testing.
    
    Returns:
        List of model configuration dictionaries
    """
    data = load_sample_model_data()
    return data["models"]

def get_connection_configs() -> List[Dict[str, Any]]:
    """
    Get sample connection configurations for testing.
    
    Returns:
        List of connection configuration dictionaries
    """
    data = load_sample_model_data()
    return data["connections"]

def save_json_test_data(data: Dict[str, Any], filename: str) -> None:
    """
    Save test data to a JSON file.
    
    Args:
        data: Data to save
        filename: Name of the file to save to
    """
    data_path = get_test_data_dir() / filename
    with open(data_path, 'w') as f:
        json.dump(data, f, indent=2)

def create_test_input_matrix(rows: int, cols: int, seed: Optional[int] = None) -> np.ndarray:
    """
    Create a test input matrix of random values.
    
    Args:
        rows: Number of rows
        cols: Number of columns
        seed: Random seed for reproducibility
        
    Returns:
        NumPy array of shape (rows, cols)
    """
    if seed is not None:
        np.random.seed(seed)
    return np.random.randn(rows, cols) 