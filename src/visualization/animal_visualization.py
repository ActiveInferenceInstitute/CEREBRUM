"""
Visualization utilities for the animal agent model.

This module provides functions for visualizing the animal agent's
state, environment, and navigation behavior.
"""

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Circle, Wedge, Arrow
from typing import Dict, Any, List, Optional, Tuple, Union
import io
from PIL import Image
import logging
import os
import time
import sys

# Handle imports
from ..core.model import Case
from ..examples.animal_agent import AnimalAgent
from ..transformations.case_transformations import transform_case
from ..utils.path_utils import get_output_dir

# Get output directory
OUTPUT_DIR = get_output_dir()

def plot_animal_environment(
    animal: AnimalAgent,
    environment: Dict[str, Any] = None,
    ax: Optional[plt.Axes] = None,
    figsize: Tuple[int, int] = (10, 10),
    show_vision: bool = True,
    show_path: bool = True,
    show_goal: bool = True,
    margin: float = 2.0
) -> plt.Figure:
    """
    Visualize the animal agent's environment.
    
    Args:
        animal: The animal agent
        environment: Optional environment data
        ax: Optional matplotlib axis
        figsize: Figure size if ax is None
        show_vision: Whether to show vision cone
        show_path: Whether to show path history
        show_goal: Whether to show goal position
        margin: Margin around the plot area
        
    Returns:
        The matplotlib figure
    """
    # Create figure if needed
    if ax is None:
        fig, ax = plt.subplots(figsize=figsize)
    else:
        fig = ax.figure
    
    # Get environment data
    if environment is None:
        environment = {}
        if hasattr(animal, 'environment') and animal.environment is not None:
            environment = animal.environment
    
    # Plot obstacles
    obstacles = environment.get('obstacles', [])
    for obs_pos in obstacles:
        circle = Circle(obs_pos, 0.5, fill=True, color='gray', alpha=0.7)
        ax.add_patch(circle)
    
    # Plot goal
    goal_pos = animal.goal_position
    if show_goal:
        goal_circle = Circle(goal_pos, 0.7, fill=True, color='green', alpha=0.5)
        ax.add_patch(goal_circle)
        ax.plot(goal_pos[0], goal_pos[1], 'g*', markersize=15)
        
        # Draw a line from animal to goal
        ax.plot([animal.position[0], goal_pos[0]], 
                [animal.position[1], goal_pos[1]], 
                'g--', alpha=0.3)
    
    # Plot vision cone
    if show_vision:
        vision_range = animal.parameters['vision_range']
        vision_angle = animal.parameters['vision_angle']
        
        # Create vision cone
        theta1 = np.degrees(animal.orientation - vision_angle/2)
        theta2 = np.degrees(animal.orientation + vision_angle/2)
        
        wedge = Wedge(animal.position, vision_range, theta1, theta2, 
                     alpha=0.15, color='blue')
        ax.add_patch(wedge)
        
        # Draw faint circle for max vision range
        vision_circle = Circle(animal.position, vision_range, 
                              fill=False, linestyle='--', color='blue', alpha=0.5)
        ax.add_patch(vision_circle)
    
    # Plot path history
    if show_path and len(animal.position_history) > 1:
        positions = np.array(animal.position_history)
        ax.plot(positions[:, 0], positions[:, 1], 'b-', alpha=0.6)
        
        # Mark start position
        ax.plot(positions[0, 0], positions[0, 1], 'bo', markersize=8)
    
    # Plot the animal
    animal_marker = Circle(animal.position, 0.5, fill=True, color='blue')
    ax.add_patch(animal_marker)
    
    # Draw orientation indicator
    direction = np.array([np.cos(animal.orientation), np.sin(animal.orientation)])
    head_length = 1.0
    ax.arrow(animal.position[0], animal.position[1],
             direction[0] * head_length, direction[1] * head_length,
             head_width=0.3, head_length=0.3, fc='blue', ec='blue')
    
    # Set plot limits with margin
    all_points = [animal.position]
    if show_goal:
        all_points.append(goal_pos)
    if show_path and len(animal.position_history) > 0:
        all_points.extend(animal.position_history)
    all_points.extend(obstacles)
    
    all_points = np.array(all_points)
    if all_points.size > 0:
        min_x, min_y = np.min(all_points, axis=0) - margin
        max_x, max_y = np.max(all_points, axis=0) + margin
        ax.set_xlim(min_x, max_x)
        ax.set_ylim(min_y, max_y)
    else:
        # Default limits
        ax.set_xlim(-10, 20)
        ax.set_ylim(-10, 20)
    
    # Set equal aspect ratio
    ax.set_aspect('equal')
    
    # Add grid and labels
    ax.grid(True, alpha=0.3)
    ax.set_xlabel('X position')
    ax.set_ylabel('Y position')
    
    # Add title based on case
    case_titles = {
        Case.NOMINATIVE: "Animal Navigating [NOM]",
        Case.ACCUSATIVE: "Animal Receiving Updates [ACC]",
        Case.GENITIVE: "Animal Generating Reports [GEN]",
        Case.DATIVE: "Animal Receiving Sensory Data [DAT]",
        Case.INSTRUMENTAL: "Animal as Navigation Tool [INS]",
        Case.LOCATIVE: "Animal Providing Spatial Context [LOC]",
        Case.ABLATIVE: "Animal as Behavioral Source [ABL]",
        Case.VOCATIVE: "Animal Responding to Commands [VOC]"
    }
    
    title = case_titles.get(animal.case, f"Animal Agent [{animal.case.value}]")
    ax.set_title(title)
    
    return fig


def plot_animal_sensory_state(
    animal: AnimalAgent,
    ax: Optional[plt.Axes] = None,
    figsize: Tuple[int, int] = (10, 5)
) -> plt.Figure:
    """
    Plot the animal agent's sensory state.
    
    Args:
        animal: The animal agent
        ax: Matplotlib axes to plot on (optional)
        figsize: Figure size
        
    Returns:
        Figure containing the plot
    """
    if ax is None:
        fig, ax = plt.subplots(figsize=figsize)
    else:
        fig = ax.figure
    
    # Plot vision field sectors
    num_sectors = animal.parameters["num_vision_sectors"]
    vision_angle = animal.parameters["vision_angle"]
    sector_angles = np.linspace(
        -vision_angle/2, vision_angle/2, num_sectors + 1
    )
    # Convert agent orientation and sector angles to world coordinates for plotting
    theta = animal.orientation + sector_angles
    
    # Use a larger radius for visualization clarity
    radius = animal.parameters["vision_range"] * 1.2 
    
    # Plot sectors based on vision field values
    for i in range(num_sectors):
        # Intensity mapped to color (e.g., red for blocked, green for clear)
        color_intensity = animal.vision_field[i] # 1.0 is clear, 0.0 is blocked
        color = plt.cm.RdYlGn(color_intensity) # Red-Yellow-Green colormap
        
        # Create wedge patch
        wedge = Wedge(
            center=(0, 0), # Plot relative to agent center
            r=radius, 
            theta1=np.degrees(theta[i]), 
            theta2=np.degrees(theta[i+1]), 
            width=radius*0.8, # Make it a thick arc
            color=color,
            alpha=0.6
        )
        ax.add_patch(wedge)
    
    # Plot goal direction (if available and agent knows it)
    # Use goal_direction_relative which is angle relative to agent's orientation
    if hasattr(animal, 'goal_distance') and animal.goal_distance < np.inf:
        goal_theta_relative = animal.goal_direction_relative
        # Convert relative angle to world angle for plotting consistency if needed, but polar plot handles relative
        # goal_theta_world = animal.orientation + goal_theta_relative
        ax.plot([goal_theta_relative], [radius*0.9], 'g*', markersize=15, label='Goal Direction')

    # Configure polar plot
    ax.set_theta_zero_location("N") # 0 degrees is North (agent's forward)
    ax.set_theta_direction(-1) # Clockwise angles
    ax.set_ylim(0, radius)
    ax.set_yticks([]) # Hide radial ticks
    ax.set_xticks(np.radians(np.linspace(-90, 90, 5))) # Ticks for FoV
    ax.set_xticklabels([f'{deg:.0f}°' for deg in np.linspace(-vision_angle/2 * 180/np.pi, vision_angle/2 * 180/np.pi, 5)])
    ax.set_title("Sensory State (Vision & Goal Direction)")
    if ax.get_legend() is None:
         ax.legend(loc='upper right', bbox_to_anchor=(1.3, 1.1))
         
    return fig


def plot_animal_path(
    animal: AnimalAgent,
    ax: Optional[plt.Axes] = None,
    figsize: Tuple[int, int] = (10, 10),
    include_orientations: bool = True,
    color_by_time: bool = True,
    margin: float = 2.0
) -> plt.Figure:
    """
    Visualize the animal agent's path history.
    
    Args:
        animal: The animal agent
        ax: Optional matplotlib axis
        figsize: Figure size if ax is None
        include_orientations: Whether to show orientation markers
        color_by_time: Whether to color the path by time
        margin: Margin around the plot area
        
    Returns:
        The matplotlib figure
    """
    # Create figure if needed
    if ax is None:
        fig, ax = plt.subplots(figsize=figsize)
    else:
        fig = ax.figure
    
    # Get path data
    positions = np.array(animal.position_history)
    
    if len(positions) <= 1:
        ax.text(0.5, 0.5, "Insufficient path data", 
               ha='center', va='center', transform=ax.transAxes)
        return fig
    
    # Plot the path
    if color_by_time:
        # Color path segments by time (older=lighter, newer=darker)
        points = np.array(positions)
        segments = np.array([points[i:i+2] for i in range(len(points)-1)])
        
        # Create line collection with time-based colormap
        from matplotlib.collections import LineCollection
        from matplotlib.colors import LinearSegmentedColormap
        
        # Custom colormap: light blue to dark blue
        cmap = LinearSegmentedColormap.from_list('BlueTime', ['#CCDDFF', '#0000AA'])
        
        # Normalized time values
        t = np.linspace(0, 1, len(segments))
        
        # Create line collection
        lc = LineCollection(segments, cmap=cmap, array=t)
        ax.add_collection(lc)
        
        # Add colorbar
        cbar = plt.colorbar(lc, ax=ax)
        cbar.set_label('Time')
    else:
        # Simple path line
        ax.plot(positions[:, 0], positions[:, 1], 'b-', alpha=0.8)
    
    # Mark start and end points
    ax.plot(positions[0, 0], positions[0, 1], 'go', markersize=10, label='Start')
    ax.plot(positions[-1, 0], positions[-1, 1], 'ro', markersize=10, label='Current')
    
    # Plot orientation markers if available and requested
    if include_orientations and len(animal.orientation_history) > 0:
        # Sample some points to avoid overcrowding
        n_points = min(len(positions), 10)
        indices = np.linspace(0, len(positions)-1, n_points, dtype=int)
        
        for i in indices:
            if i < len(animal.orientation_history):
                pos = positions[i]
                orient = animal.orientation_history[i]
                
                # Draw orientation arrow
                direction = np.array([np.cos(orient), np.sin(orient)])
                ax.arrow(pos[0], pos[1],
                        direction[0] * 0.8, direction[1] * 0.8,
                        head_width=0.2, head_length=0.2, 
                        fc='black', ec='black', alpha=0.5)
    
    # Add goal marker if available
    if hasattr(animal, 'goal_position'):
        ax.plot(animal.goal_position[0], animal.goal_position[1], 
               'g*', markersize=15, label='Goal')
        
        # Draw a line from start to goal
        ax.plot([positions[0, 0], animal.goal_position[0]], 
                [positions[0, 1], animal.goal_position[1]], 
                'g--', alpha=0.4, label='Direct Path')
    
    # Calculate path statistics
    path_length = 0
    for i in range(len(positions)-1):
        path_length += np.linalg.norm(positions[i+1] - positions[i])
    
    # Calculate direct distance
    direct_distance = np.linalg.norm(positions[-1] - positions[0])
    
    # Efficiency ratio (1.0 = perfect straight line)
    efficiency = direct_distance / path_length if path_length > 0 else 1.0
    
    # Add statistics text
    stat_text = (
        f"Path Length: {path_length:.2f}\n"
        f"Direct Distance: {direct_distance:.2f}\n"
        f"Efficiency: {efficiency:.2f}\n"
        f"Steps: {len(positions)}"
    )
    
    ax.text(0.02, 0.97, stat_text, transform=ax.transAxes,
           verticalalignment='top',
           bbox=dict(facecolor='white', alpha=0.7))
    
    # Set plot limits with margin
    min_x, min_y = np.min(positions, axis=0) - margin
    max_x, max_y = np.max(positions, axis=0) + margin
    
    # Include goal in limits if available
    if hasattr(animal, 'goal_position'):
        min_x = min(min_x, animal.goal_position[0] - margin)
        min_y = min(min_y, animal.goal_position[1] - margin)
        max_x = max(max_x, animal.goal_position[0] + margin)
        max_y = max(max_y, animal.goal_position[1] + margin)
    
    ax.set_xlim(min_x, max_x)
    ax.set_ylim(min_y, max_y)
    
    # Set equal aspect ratio
    ax.set_aspect('equal')
    
    # Add grid and labels
    ax.grid(True, alpha=0.3)
    ax.set_xlabel('X position')
    ax.set_ylabel('Y position')
    ax.set_title(f'Animal Path History [{animal.case.value}]')
    ax.legend()
    
    return fig


def create_animal_animation(
    animal: AnimalAgent,
    environment: Dict[str, Any],
    steps: int = 20,
    fig_size: Tuple[int, int] = (10, 10)
) -> List[np.ndarray]:
    """
    Create a series of animation frames showing animal navigation.
    
    Args:
        animal: The animal agent
        environment: Environment data
        steps: Number of simulation steps
        fig_size: Figure size
        
    Returns:
        List of frames as numpy arrays
    """
    frames = []
    
    # Set animal to NOMINATIVE case for active navigation
    original_case = animal.case
    
    # Save the initial state
    initial_position = animal.position.copy()
    initial_orientation = animal.orientation
    initial_history = {
        'position': animal.position_history.copy(),
        'orientation': animal.orientation_history.copy()
    }
    
    try:
        # Set to NOMINATIVE case
        animal.case = Case.NOMINATIVE
        
        for step in range(steps):
            # Create figure for this frame
            fig, ax = plt.subplots(figsize=fig_size)
            
            # Update the animal
            animal.update(environment)
            
            # Plot the current state
            plot_animal_environment(animal, environment, ax=ax)
            
            # Convert figure to image
            fig.canvas.draw()
            frame = np.array(fig.canvas.renderer.buffer_rgba())
            frames.append(frame)
            
            plt.close(fig)
            
            # Stop if goal reached
            if animal.goal_reached:
                break
    
    finally:
        # Restore original case
        animal.case = original_case
    
    return frames


def animate_animal_navigation(
    animal: AnimalAgent,
    environment: Dict[str, Any],
    steps: int = 20,
    fig_size: Tuple[int, int] = (10, 10),
    save_path: Optional[str] = None,
    fps: int = 5
) -> Union[str, List[np.ndarray]]:
    """
    Animate the animal agent navigating in the environment.
    
    Args:
        animal: The animal agent
        environment: Environment data
        steps: Number of simulation steps
        fig_size: Figure size
        save_path: Optional path to save animation
        fps: Frames per second
        
    Returns:
        Either save path if saved or list of frames
    """
    try:
        import matplotlib.animation as animation
    except ImportError:
        print("Matplotlib animation module not available.")
        return []
    
    # Create a copy of the animal to avoid modifying the original
    from copy import deepcopy
    animal_copy = deepcopy(animal)
    
    # Set animal to NOMINATIVE case for active navigation
    animal_copy.case = Case.NOMINATIVE
    
    # Initialize figure
    fig, ax = plt.subplots(figsize=fig_size)
    
    # Plot initial state
    plot_animal_environment(animal_copy, environment, ax=ax)
    
    # Animation update function
    def update(frame):
        ax.clear()
        animal_copy.update(environment)
        plot_animal_environment(animal_copy, environment, ax=ax)
        ax.set_title(f"Animal Navigation - Step {frame+1}")
        
        # Stop if goal reached
        if animal_copy.goal_reached:
            return []
    
    # Create animation
    anim = animation.FuncAnimation(
        fig, update, frames=steps, interval=1000//fps, blit=False)
    
    if save_path:
        anim.save(save_path, writer='pillow', fps=fps)
        plt.close(fig)
        return save_path
    
    # Convert to frames
    frames = []
    for i in range(steps):
        update(i)
        fig.canvas.draw()
        frame = np.array(fig.canvas.renderer.buffer_rgba())
        frames.append(frame)
        
        # Stop if goal reached
        if animal_copy.goal_reached:
            break
    
    plt.close(fig)
    return frames


def visualize_animal_cases(animal: AnimalAgent) -> Dict[str, plt.Figure]:
    """
    Visualize the animal in all different cases.
    
    Args:
        animal: The animal agent
        
    Returns:
        Dictionary mapping case names to figures
    """
    # Create simple environment for visualization
    environment = {
        "obstacles": [
            [5.0, 5.0],
            [7.0, 3.0],
            [3.0, 8.0]
        ],
        "goal": animal.goal_position.tolist()
    }
    
    # Save original case to restore later
    original_case = animal.case
    
    figures = {}
    
    try:
        # Create visualizations for each case
        for case in Case:
            transform_case(animal, case)
            
            # Create figure with two subplots
            fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(18, 8))
            
            # Plot environment view
            plot_animal_environment(animal, environment, ax=ax1)
            
            # Plot case-specific visualization
            if case == Case.NOMINATIVE:
                # For NOMINATIVE, show sensory state using a polar plot
                # Create polar axes in the second subplot position
                fig.delaxes(ax2) # Remove the original axes
                ax_polar = fig.add_subplot(1, 2, 2, projection='polar') # Add polar axes
                plot_animal_sensory_state(animal, ax=ax_polar)
                ax_polar.set_title("Sensory State [NOM]") # Title on polar axes
            
            elif case == Case.ACCUSATIVE:
                # For ACCUSATIVE, show parameter update visualization
                ax2.bar(['Speed', 'Vision Range', 'Max Rotation'], 
                       [animal.parameters.get('speed', 0), # Use .get for safety
                        animal.parameters.get('vision_range', 0),
                        animal.parameters.get('max_rotation', 0)])
                ax2.set_title("Updatable Parameters [ACC]")
                
            elif case == Case.GENITIVE:
                # For GENITIVE, show report generation
                plot_animal_path(animal, ax=ax2)
                ax2.set_title("Path Report [GEN]")
                
            elif case == Case.DATIVE:
                # For DATIVE, show sensory inputs using a polar plot
                fig.delaxes(ax2) # Remove the original axes
                ax_polar = fig.add_subplot(1, 2, 2, projection='polar') # Add polar axes
                plot_animal_sensory_state(animal, ax=ax_polar)
                ax_polar.set_title("Received Sensory Data [DAT]")
                
            elif case == Case.INSTRUMENTAL:
                # For INSTRUMENTAL, show path planning
                # Simulate a path planning calculation
                from copy import deepcopy
                animal_sim = deepcopy(animal)
                
                # Run a few steps of navigation
                positions = [animal_sim.position.copy()]
                for _ in range(5):
                    animal_sim.perceive(environment)
                    rot, speed = animal_sim.compute_action()
                    animal_sim.move(rot, speed)
                    positions.append(animal_sim.position.copy())
                
                # Plot planned path
                positions = np.array(positions)
                ax2.plot(positions[:, 0], positions[:, 1], 'b-o')
                ax2.set_title("Path Planning Tool [INS]")
                ax2.set_aspect('equal')
                ax2.grid(True, alpha=0.3)
                
            elif case == Case.LOCATIVE:
                # For LOCATIVE, show spatial context
                # Create polar plot for relative positions
                from matplotlib.projections import PolarAxes
                ax2 = fig.add_subplot(122, projection='polar')
                
                # Add obstacles in polar coordinates
                for obs_pos in environment["obstacles"]:
                    # Calculate relative position
                    rel_pos = np.array(obs_pos) - animal.position
                    distance = np.linalg.norm(rel_pos)
                    angle = np.arctan2(rel_pos[1], rel_pos[0]) - animal.orientation
                    
                    if distance <= animal.parameters["vision_range"]:
                        ax2.plot(angle, distance, 'o', color='gray', markersize=10)
                
                # Add goal in polar coordinates
                goal_vec = animal.goal_position - animal.position
                goal_dist = np.linalg.norm(goal_vec)
                goal_angle = np.arctan2(goal_vec[1], goal_vec[0]) - animal.orientation
                ax2.plot(goal_angle, goal_dist, '*', color='green', markersize=15)
                
                # Configure polar plot
                ax2.set_title("Spatial Context [LOC]")
                ax2.set_rticks([1, 2, 3, 4, 5])
                ax2.set_rlim(0, animal.parameters["vision_range"])
                ax2.grid(True)
                
            elif case == Case.ABLATIVE:
                # For ABLATIVE, show historical data
                if len(animal.position_history) > 1:
                    plot_animal_path(animal, ax=ax2)
                    ax2.set_title("Historical Path [ABL]")
                else:
                    ax2.text(0.5, 0.5, "Insufficient history data", 
                           ha='center', va='center', transform=ax2.transAxes)
                
            elif case == Case.VOCATIVE:
                # For VOCATIVE, show command interface
                commands = [
                    "status", "stop", "turn_left", 
                    "turn_right", "forward", "help"
                ]
                
                # Create command buttons visualization
                y_pos = np.arange(len(commands))
                ax2.barh(y_pos, [1] * len(commands), align='center', color='skyblue')
                ax2.set_yticks(y_pos)
                ax2.set_yticklabels(commands)
                ax2.set_xlabel('Available Commands')
                ax2.set_title("Command Interface [VOC]")
                ax2.invert_yaxis()  # Display from top
            
            fig.suptitle(f"Animal Agent in {case.name} Case", fontsize=16)
            fig.tight_layout()
            
            figures[case.name] = fig
    
    finally:
        # Restore original case
        animal.case = original_case
    
    return figures 