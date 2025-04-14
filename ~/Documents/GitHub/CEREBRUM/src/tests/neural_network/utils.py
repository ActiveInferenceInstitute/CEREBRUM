import os
import logging
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import numpy as np
from typing import Dict, List, Any, Tuple

# Output directory for visualizations
OUTPUT_DIR = os.path.join(os.path.dirname(os.path.dirname(__file__)), "output", "neural_network")

# Configure logging
def setup_logger(name: str, level=logging.INFO) -> logging.Logger:
    """
    Set up a logger with the specified name and level.
    
    Args:
        name: The name for the logger
        level: The logging level (default: INFO)
        
    Returns:
        Logger instance
    """
    logger = logging.getLogger(name)
    logger.setLevel(level)
    
    # Create formatter
    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    
    # Create console handler
    console_handler = logging.StreamHandler()
    console_handler.setFormatter(formatter)
    logger.addHandler(console_handler)
    
    return logger

def ensure_directory(directory_path: str) -> None:
    """
    Ensure that the specified directory exists.
    
    Args:
        directory_path: Path to the directory
    """
    os.makedirs(directory_path, exist_ok=True)

def get_case_dir(case_name: str) -> str:
    """
    Get the output directory for a specific case.
    
    Args:
        case_name: The name of the case
        
    Returns:
        Path to the case output directory
    """
    case_dir = os.path.join(OUTPUT_DIR, case_name.lower())
    ensure_directory(case_dir)
    return case_dir

def create_animation(
    fig: plt.Figure,
    update_func,
    init_func=None,
    frames: int = 100,
    interval: int = 200,
    save_path: str = None,
    fps: int = 2,
    logger=None
) -> animation.FuncAnimation:
    """
    Create and save a matplotlib animation.
    
    Args:
        fig: The matplotlib figure
        update_func: The function to update the plot for each frame
        init_func: The function to initialize the plot (optional)
        frames: Number of frames
        interval: Interval between frames in milliseconds
        save_path: Path to save the animation
        fps: Frames per second for the saved animation
        logger: Logger instance for logging
        
    Returns:
        The FuncAnimation object
    """
    anim = animation.FuncAnimation(
        fig=fig,
        func=update_func,
        frames=frames,
        init_func=init_func,
        blit=True,
        interval=interval
    )
    
    if save_path:
        anim.save(save_path, writer='pillow', fps=fps)
        if logger:
            logger.info(f"Saved animation to {save_path}")
    
    return anim

def generate_report(
    case_name: str,
    case_info: Dict[str, str],
    model_info: Dict[str, Any],
    visualizations: List[str],
    sections: Dict[str, str] = None,
    save_path: str = None,
    logger=None
) -> None:
    """
    Generate a markdown report for a case test.
    
    Args:
        case_name: The name of the case
        case_info: Information about the case
        model_info: Information about the model
        visualizations: List of visualization file names
        sections: Additional sections to include (optional)
        save_path: Path to save the report
        logger: Logger instance for logging
    """
    if save_path is None:
        return
    
    with open(save_path, "w") as f:
        f.write(f"# Neural Network in {case_name} Case\n\n")
        f.write(f"## {case_info['linguistic_meaning']}\n\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n\n")
        f.write(f"### Context\n\n")
        f.write(f"{case_info['neural_network_context']}\n\n")
        f.write("### Mathematical Representation\n\n")
        f.write(f"```\n{case_info['formula']}\n```\n\n")
        f.write("### Primary Methods\n\n")
        f.write(f"{case_info['primary_methods']}\n\n")
        f.write("### Example\n\n")
        f.write(f"{case_info['example']}\n\n")
        f.write("### Analysis Results\n\n")
        
        # Add model information
        if model_info:
            for key, value in model_info.items():
                f.write(f"* {key}: {value}\n")
        
        # Add additional sections
        if sections:
            f.write("\n")
            for section_title, section_content in sections.items():
                f.write(f"### {section_title}\n\n")
                f.write(f"{section_content}\n\n")
        
        # Add visualizations
        f.write("### Visualizations\n\n")
        for i, viz in enumerate(visualizations):
            f.write(f"{i+1}. [{os.path.splitext(viz)[0].replace('_', ' ').title()}]({viz})\n")
    
    if logger:
        logger.info(f"Generated report at {save_path}") 