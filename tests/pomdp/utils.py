import os
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from typing import Dict, Any, List, Callable, Optional

def get_case_dir(case_name: str) -> str:
    """
    Get the directory path for a specific grammatical case test output.
    
    Args:
        case_name: Name of the grammatical case
        
    Returns:
        Directory path for the case's test output
    """
    # Create base output directory
    output_dir = os.path.join("output", "pomdp", case_name.lower())
    os.makedirs(output_dir, exist_ok=True)
    return output_dir

def create_animation(
    fig: plt.Figure, 
    update_func: Callable, 
    init_func: Callable, 
    frames: int, 
    save_path: str, 
    fps: int = 5,
    logger: Optional[Any] = None
) -> None:
    """
    Create and save an animation.
    
    Args:
        fig: Matplotlib figure to animate
        update_func: Function to update the figure for each frame
        init_func: Function to initialize the figure
        frames: Number of frames in the animation
        save_path: Path to save the animation to
        fps: Frames per second for the animation
        logger: Optional logger for logging progress
    """
    if logger:
        logger.info(f"Creating animation with {frames} frames at {fps} fps")
    
    anim = animation.FuncAnimation(
        fig, update_func, frames=frames, init_func=init_func, blit=True
    )
    
    try:
        # Save the animation
        anim.save(save_path, writer='pillow', fps=fps)
        if logger:
            logger.info(f"Animation saved to {save_path}")
    except Exception as e:
        if logger:
            logger.error(f"Failed to save animation: {e}")
        print(f"Failed to save animation: {e}")

def generate_report(
    case_name: str,
    case_info: Dict[str, Any],
    model_info: Dict[str, Any],
    visualizations: List[str],
    save_path: str,
    logger: Optional[Any] = None
) -> None:
    """
    Generate a report for a case test.
    
    Args:
        case_name: Name of the grammatical case
        case_info: Dictionary with information about the case
        model_info: Dictionary with information about the model
        visualizations: List of visualization file names
        save_path: Path to save the report to
        logger: Optional logger for logging progress
    """
    if logger:
        logger.info(f"Generating report for {case_name} case")
    
    # Report content
    content = f"# {case_name} Case Test Report\n\n"
    
    # Case Information
    content += "## Case Information\n\n"
    content += f"- **Linguistic Meaning**: {case_info['linguistic_meaning']}\n"
    content += f"- **Statistical Role**: {case_info['statistical_role']}\n"
    
    if 'pomdp_context' in case_info:
        content += f"- **POMDP Context**: {case_info['pomdp_context']}\n"
    
    if 'primary_methods' in case_info:
        content += f"- **Primary Methods**: {case_info['primary_methods']}\n"
    
    content += "\n"
    
    # Model Information
    content += "## Model Information\n\n"
    for key, value in model_info.items():
        content += f"- **{key}**: {value}\n"
    content += "\n"
    
    # Visualizations
    content += "## Visualizations\n\n"
    for vis in visualizations:
        content += f"### {vis.replace('.png', '').replace('.gif', '').replace('_', ' ').title()}\n\n"
        content += f"![{vis}]({vis})\n\n"
    
    # Save the report
    with open(save_path, "w") as f:
        f.write(content)
    
    if logger:
        logger.info(f"Report saved to {save_path}") 