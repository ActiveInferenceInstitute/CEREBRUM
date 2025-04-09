"""
Path utility functions for the CEREBRUM project.

This module provides common utilities for handling paths, file locations,
and output directories across the project.
"""

import os
import sys
import logging

def find_project_root(marker: str = ".git") -> str:
    """
    Find the project root directory by searching for a marker file/directory.
    
    Args:
        marker: Marker file or directory to identify the root (default: ".git")
        
    Returns:
        Absolute path to project root
    """
    current_dir = os.path.dirname(os.path.abspath(__file__))
    project_root = current_dir
    
    # Traverse up until we find the marker
    while not os.path.exists(os.path.join(project_root, marker)):
        parent_dir = os.path.dirname(project_root)
        if parent_dir == project_root:  # Reached filesystem root
            # Fallback: Assume current working directory
            project_root = os.getcwd()
            print(f"Warning: Could not find project root marker '{marker}'. "
                 f"Assuming CWD '{project_root}' is project root.", file=sys.stderr)
            break
        project_root = parent_dir
    
    return project_root

def get_output_dir() -> str:
    """
    Get or create the standard output directory for the project.
    
    Returns:
        Path to the output directory
    """
    project_root = find_project_root()
    output_dir = os.path.join(project_root, "src", "tests", "example_outputs")
    os.makedirs(output_dir, exist_ok=True)
    return output_dir

def get_logs_dir() -> str:
    """
    Get or create the standard logs directory for the project.
    
    Returns:
        Path to the logs directory
    """
    project_root = find_project_root()
    logs_dir = os.path.join(project_root, "src", "tests", "logs")
    os.makedirs(logs_dir, exist_ok=True)
    return logs_dir

def save_plot(fig, filename_base: str, subdirectory: str = None) -> str:
    """
    Save a matplotlib figure to the output directory with a timestamp.
    
    Args:
        fig: Matplotlib figure to save
        filename_base: Base filename (without extension)
        subdirectory: Optional subdirectory within the output directory
        
    Returns:
        Path to the saved file
    """
    import time
    import matplotlib.pyplot as plt
    
    timestamp = time.strftime("%Y%m%d_%H%M%S")
    output_dir = get_output_dir()
    
    # Add subdirectory if provided
    if subdirectory:
        output_dir = os.path.join(output_dir, subdirectory)
        os.makedirs(output_dir, exist_ok=True)
    
    filename = os.path.join(output_dir, f"{filename_base}_{timestamp}.png")
    fig.savefig(filename)
    logging.info(f"Saved plot to: {filename}")
    plt.close(fig)  # Close the figure to free memory
    
    return filename 