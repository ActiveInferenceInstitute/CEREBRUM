"""
Environment module for the animal agent.

This module provides a simple environment implementation for the animal agent
to navigate in. It includes obstacles, goals, and methods to interact with
the environment.
"""

import numpy as np
from typing import List, Tuple, Dict, Any, Optional
import matplotlib.pyplot as plt
from matplotlib.patches import Circle, Rectangle, Polygon, Arc
from matplotlib.figure import Figure
from matplotlib.axes import Axes
import logging
import os
import time
import sys

# --- Output Directory Configuration ---
# Outputs will be saved inside the tests directory
project_root_marker = ".git" # Or another marker file/dir in your project root
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = current_dir
while not os.path.exists(os.path.join(project_root, project_root_marker)):
    parent_dir = os.path.dirname(project_root)
    if parent_dir == project_root: # Reached filesystem root
        # Fallback: Assume running from project root if marker not found
        project_root = os.getcwd()
        print(f"Warning: Could not find project root marker '{project_root_marker}'. Assuming CWD '{project_root}' is project root.", file=sys.stderr)
        break
    project_root = parent_dir

OUTPUT_DIR = os.path.join(project_root, "src", "tests", "example_outputs")
os.makedirs(OUTPUT_DIR, exist_ok=True)

# Configure logging (use root logger settings)
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

class Environment:
    """
    A 2D environment for the animal agent to navigate in.
    
    Features:
    - Rectangular boundaries
    - Circular obstacles
    - Goal position
    - Collision detection
    """
    
    def __init__(
        self,
        width: float = 20.0,
        height: float = 20.0,
        obstacles: List[List[float]] = None,
        goal_position: List[float] = None
    ):
        """
        Initialize the environment.
        
        Args:
            width: Width of the environment
            height: Height of the environment
            obstacles: List of obstacle positions [[x1, y1], [x2, y2], ...]
            goal_position: Position of the goal [x, y]
        """
        self.width = width
        self.height = height
        
        # Set default obstacles if none provided
        self.obstacles = obstacles if obstacles is not None else []
        self.obstacle_radius = 0.5  # Default obstacle radius
        
        # Set default goal position if none provided
        self.goal_position = np.array(goal_position) if goal_position is not None else np.array([width - 2, height - 2])
        self.goal_radius = 1.0  # Radius to consider goal reached
    
    def is_in_bounds(self, position: np.ndarray) -> bool:
        """
        Check if a position is within the environment boundaries.
        
        Args:
            position: [x, y] position to check
            
        Returns:
            True if position is in bounds, False otherwise
        """
        x, y = position
        return 0 <= x <= self.width and 0 <= y <= self.height
    
    def is_collision(self, position: np.ndarray, radius: float = 0.0) -> bool:
        """
        Check if a position collides with any obstacle.
        
        Args:
            position: [x, y] position to check
            radius: Radius of the agent
            
        Returns:
            True if collision, False otherwise
        """
        # Check boundary collision
        x, y = position
        if (x - radius < 0 or x + radius > self.width or 
            y - radius < 0 or y + radius > self.height):
            return True
        
        # Check obstacle collision
        for obstacle in self.obstacles:
            obstacle_pos = np.array(obstacle)
            distance = np.linalg.norm(position - obstacle_pos)
            if distance < (radius + self.obstacle_radius):
                return True
        
        return False
    
    def is_goal_reached(self, position: np.ndarray, threshold: float = None) -> bool:
        """
        Check if the goal is reached.
        
        Args:
            position: [x, y] position to check
            threshold: Distance threshold to consider goal reached (optional)
            
        Returns:
            True if goal reached, False otherwise
        """
        if threshold is None:
            threshold = self.goal_radius
            
        distance = np.linalg.norm(position - self.goal_position)
        return distance <= threshold
    
    def get_state_dict(self) -> Dict[str, Any]:
        """
        Get the environment as a dictionary for the animal agent.
        
        Returns:
            Dictionary representation of the environment
        """
        return {
            "width": self.width,
            "height": self.height,
            "obstacles": self.obstacles.copy(),
            "goal": self.goal_position.tolist()
        }
    
    def add_obstacle(self, position: List[float]) -> None:
        """
        Add an obstacle to the environment.
        
        Args:
            position: [x, y] position of the obstacle
        """
        self.obstacles.append(position)
    
    def set_goal_position(self, position: List[float]) -> None:
        """
        Set the goal position.
        
        Args:
            position: [x, y] position of the goal
        """
        self.goal_position = np.array(position)
    
    def plot(self, ax: Optional[Axes] = None, figsize: Tuple[int, int] = (8, 8)) -> Tuple[Figure, Axes]:
        """
        Plot the environment.
        
        Args:
            ax: Matplotlib axes to plot on (optional)
            figsize: Figure size (width, height) in inches
            
        Returns:
            (figure, axes) tuple
        """
        if ax is None:
            fig, ax = plt.subplots(figsize=figsize)
        else:
            fig = ax.figure
        
        # Plot boundaries
        ax.set_xlim(0, self.width)
        ax.set_ylim(0, self.height)
        
        # Plot obstacles
        for obstacle in self.obstacles:
            circle = Circle(obstacle, self.obstacle_radius, 
                           fill=True, color='gray', alpha=0.7)
            ax.add_patch(circle)
        
        # Plot goal
        goal_circle = Circle(self.goal_position, self.goal_radius, 
                            fill=True, color='green', alpha=0.5)
        ax.add_patch(goal_circle)
        
        ax.set_aspect('equal')
        ax.set_xlabel('X')
        ax.set_ylabel('Y')
        ax.set_title('Environment')
        
        return fig, ax


def create_sample_environment() -> Environment:
    """
    Create a sample environment with obstacles.
    
    Returns:
        Sample environment
    """
    # Create environment
    env = Environment(width=20.0, height=20.0)
    
    # Add some obstacles
    obstacles = [
        [5.0, 5.0],
        [10.0, 8.0],
        [15.0, 5.0],
        [7.0, 12.0],
        [12.0, 15.0]
    ]
    
    for obs in obstacles:
        env.add_obstacle(obs)
    
    # Set goal position
    env.set_goal_position([18.0, 18.0])
    
    return env


def create_maze_environment() -> Environment:
    """
    Create a maze-like environment with obstacles.
    
    Returns:
        Maze environment
    """
    # Create environment
    env = Environment(width=20.0, height=20.0)
    
    # Create a maze pattern
    obstacles = []
    
    # Horizontal walls
    for x in range(2, 16, 2):
        obstacles.append([x, 5.0])
    
    for x in range(4, 18, 2):
        obstacles.append([x, 10.0])
    
    for x in range(2, 16, 2):
        obstacles.append([x, 15.0])
    
    # Vertical walls
    for y in range(2, 8, 2):
        obstacles.append([5.0, y])
    
    for y in range(12, 18, 2):
        obstacles.append([5.0, y])
    
    for y in range(2, 8, 2):
        obstacles.append([15.0, y])
    
    for y in range(12, 18, 2):
        obstacles.append([15.0, y])
    
    # Add all obstacles
    for obs in obstacles:
        env.add_obstacle(obs)
    
    # Set goal position
    env.set_goal_position([18.0, 18.0])
    
    return env


if __name__ == "__main__":
    # Create and visualize a sample environment
    env = create_sample_environment()
    fig, ax = env.plot()
    plt.show()  # Uncommented to display plot interactively
    timestamp = time.strftime("%Y%m%d_%H%M%S")
    filename = os.path.join(OUTPUT_DIR, f"environment_sample_{timestamp}.png")
    fig.savefig(filename)
    logging.info(f"Saved plot to: {filename}")
    plt.close(fig) # Close the figure to free memory
    
    # Create and visualize a maze environment
    maze_env = create_maze_environment()
    fig, ax = maze_env.plot()
    plt.show()  # Uncommented to display plot interactively
    timestamp = time.strftime("%Y%m%d_%H%M%S")
    filename = os.path.join(OUTPUT_DIR, f"environment_maze_{timestamp}.png")
    fig.savefig(filename)
    logging.info(f"Saved plot to: {filename}")
    plt.close(fig) # Close the figure to free memory 