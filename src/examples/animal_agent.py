"""
Animal Agent Model - A case-bearing model for navigating animals.

This module implements a navigating animal agent as an example of a
case-bearing generative model in the CEREBRUM framework.
"""

import numpy as np
import matplotlib.pyplot as plt
from enum import Enum
from typing import Dict, Any, Optional, List, Callable, Tuple, Union
import uuid
import logging
import os
import time
import sys

# Handle imports properly whether file is run directly or imported as a module
try:
    # Try relative import (works when imported as a module)
    from ..core.model import Model, Case
except ImportError:
    # Fall back to absolute import (works when run directly)
    from src.core.model import Model, Case


# --- Output Directory Configuration ---
project_root_marker = ".git"
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = current_dir
while not os.path.exists(os.path.join(project_root, project_root_marker)):
    parent_dir = os.path.dirname(project_root)
    if parent_dir == project_root:
        project_root = os.getcwd()
        print(f"Warning: Could not find project root marker '{project_root_marker}'. Assuming CWD '{project_root}' is project root.", file=sys.stderr)
        break
    project_root = parent_dir
OUTPUT_DIR = os.path.join(project_root, "src", "tests", "example_outputs")
os.makedirs(OUTPUT_DIR, exist_ok=True)

# Configure basic logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

class AnimalAgent(Model):
    """
    Represents an animal agent navigating a 2D environment.
    Uses case-based reasoning derived from the base Model class.
    """
    
    # --- Initialization ---
    def __init__(
        self,
        name: str = "Animal",
        position: np.ndarray = np.array([0.0, 0.0]),
        orientation: float = 0.0,
        speed: float = 0.5,
        vision_range: float = 10.0,
        vision_angle: float = np.pi/2,  # 90 degrees field of view
        num_vision_sectors: int = 5,
        max_rotation: float = np.pi/6,  # 30 degrees max rotation per step
        goal_position: np.ndarray = np.array([10.0, 10.0]),
        goal_distance_threshold: float = 1.0
    ):
        """
        Initialize the Animal Agent.
        """
        super().__init__(name=name)
        
        # State variables
        self.position = np.array(position)
        self.orientation = float(orientation)
        self.goal_position = np.array(goal_position)
        self.goal_distance_threshold = float(goal_distance_threshold)
        self.goal_reached = False
        
        # Parameters dictionary (as required by Model base class)
        self.parameters = {
            "speed": float(speed),
            "vision_range": float(vision_range),
            "vision_angle": float(vision_angle),
            "num_vision_sectors": int(num_vision_sectors),
            "max_rotation": float(max_rotation),
        }
        
        # Sensory state (internal)
        self.vision_field = np.zeros(self.parameters["num_vision_sectors"])
        self.goal_direction_relative = 0.0 # Angle relative to agent orientation
        self.goal_distance = np.inf
        
        # History tracking for visualization
        self.position_history = [self.position.copy()]
        self.orientation_history = [self.orientation]
        
        # Reference to environment (set during update)
        self.environment = None 
        
        # No need for _configure_case_behaviors if using _update_* methods
    
    # --- Core Methods (required by Model) ---
    def free_energy(self) -> float:
        """Placeholder Free Energy: Lower when closer to goal."""
        return np.linalg.norm(self.position - self.goal_position)

    # --- Case-Specific Update Methods (implementing Model's abstract methods) ---
    def _update_nominative(self, environment: Dict[str, Any]) -> Dict[str, Any]:
        """Handle NOMINATIVE case: active agent making decisions."""
        if self.goal_reached: return {"action": "idle", "status": "goal_reached"}
        
        self.perceive(environment)
        rotation, speed_factor = self.compute_action()
        actual_speed = self.parameters["speed"] * speed_factor
        self.move(rotation, actual_speed)
        
        return {
            "action": "move",
            "rotation": rotation,
            "speed": actual_speed,
            "new_position": self.position.copy().tolist(),
            "new_orientation": self.orientation
        }
    
    def _update_accusative(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Handle ACCUSATIVE case: receive updates to parameters."""
        updates = data if isinstance(data, dict) else {}
        updated_params = {}
        for param, value in updates.items():
            if param in self.parameters:
                old_value = self.parameters[param]
                self.parameters[param] = value
                updated_params[param] = {"old": old_value, "new": value}
                logging.info(f"Agent '{self.name}' parameter updated: {param}={value}")
        # Re-calculate dependent things if needed, e.g., vision sectors
        if "num_vision_sectors" in updated_params:
             self.vision_field = np.zeros(self.parameters["num_vision_sectors"])
             
        return {"action": "update_parameters", "updated_params": updated_params}
    
    def _update_genitive(self, data: Any) -> Dict[str, Any]:
        """Handle GENITIVE case: generate reports/data possession."""
        report = {
            "name": self.name,
            "id": self.id,
            "case": self.case.value,
            "position": self.position.tolist(),
            "orientation": self.orientation,
            "goal_position": self.goal_position.tolist(),
            "goal_distance": self.goal_distance,
            "goal_reached": self.goal_reached,
            "parameters": self.parameters.copy(),
            "vision_field": self.vision_field.tolist(),
            "history_len": len(self.position_history)
        }
        return {"action": "generate_report", "report": report}
    
    def _update_dative(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Handle DATIVE case: receive sensory data or goal info."""
        result = {"action": "receive_data"}
        if isinstance(data, dict):
            if "goal_position" in data:
                self.goal_position = np.array(data["goal_position"])
                self.goal_reached = False # Reset goal status
                logging.info(f"Agent '{self.name}' received new goal: {self.goal_position}")
                result["new_goal"] = self.goal_position.tolist()
            # Could also process direct sensory input here if needed
            result["data_processed"] = list(data.keys())
        return result
    
    def _update_instrumental(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Handle INSTRUMENTAL case: be used as navigation tool (e.g., path planning)."""
        # Example: Plan path to a target specified in data
        if isinstance(data, dict) and "target" in data:
            target_pos = np.array(data["target"])
            # Basic path: straight line (no obstacle avoidance in this stub)
            path = [self.position.tolist(), target_pos.tolist()]
            return {"action": "plan_path", "target": target_pos.tolist(), "path": path}
        return {"action": "instrumental_idle", "status": "no_task"}
    
    def _update_locative(self, data: Any) -> Dict[str, Any]:
        """Handle LOCATIVE case: provide spatial context."""
        # Assume perception is up-to-date or call self.perceive() if needed
        context = {
            "position": self.position.tolist(),
            "orientation": self.orientation,
            "goal_distance": self.goal_distance,
            "vision_summary": self.vision_field.tolist() 
        }
        return {"action": "provide_context", "context": context}
    
    def _update_ablative(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Handle ABLATIVE case: source of behavior/history."""
        query = data if isinstance(data, dict) else {}
        history_type = query.get("type", "position")
        max_items = query.get("max_items", 10)
        
        historical_data = {}
        if history_type == "position":
            history = self.position_history
            items = [p.tolist() for p in history]
        elif history_type == "orientation":
            history = self.orientation_history
            items = list(history)
        else:
            return {"action": "history_query", "status": "error", "message": "Unknown history type"}

        # Limit items
        if len(items) > max_items:
             indices = np.linspace(0, len(items)-1, max_items, dtype=int)
             items = [items[i] for i in indices]
             
        historical_data[history_type] = items
        return {"action": "history_query", "data": historical_data}
    
    def _update_vocative(self, data: Any) -> Dict[str, Any]:
        """Handle VOCATIVE case: respond to direct commands/address."""
        command = data if isinstance(data, str) else "status"
        response = {"action": "command_response", "command": command}
        if command == "status":
            response["status_report"] = self._update_genitive({})["report"]
        elif command == "reset_goal":
            # Example command - reset goal to default (needs definition)
            response["status"] = "Goal reset not implemented"
        else:
            response["status"] = "Unknown command"
        return response

    # --- Agent Specific Methods ---
    def perceive(self, environment: Dict[str, Any]):
        """Update vision field based on environment obstacles and goal."""
        self.environment = environment # Store env reference
        num_sectors = self.parameters["num_vision_sectors"]
        vision_range = self.parameters["vision_range"]
        vision_angle = self.parameters["vision_angle"]
        sector_angle = vision_angle / num_sectors
        
        # Reset vision field (1.0 = clear, 0.0 = blocked)
        self.vision_field.fill(1.0)
        min_dist_in_sector = np.full(num_sectors, vision_range)

        obstacles = environment.get("obstacles", [])
        for obs_pos_list in obstacles:
            obs_pos = np.array(obs_pos_list)
            vec_to_obs = obs_pos - self.position
            dist_to_obs = np.linalg.norm(vec_to_obs)

            if 0 < dist_to_obs < vision_range:
                # Angle of obstacle relative to world X-axis
                world_angle_obs = np.arctan2(vec_to_obs[1], vec_to_obs[0])
                # Angle relative to agent's orientation
                relative_angle = world_angle_obs - self.orientation
                # Normalize angle to [-pi, pi]
                relative_angle = (relative_angle + np.pi) % (2 * np.pi) - np.pi

                # Check if obstacle is within vision field angle
                if abs(relative_angle) < vision_angle / 2:
                    # Determine which sector the obstacle falls into
                    # Offset angle to start sectors from the left edge of vision field
                    offset_angle = relative_angle + vision_angle / 2
                    sector_index = int(offset_angle // sector_angle)
                    sector_index = max(0, min(num_sectors - 1, sector_index))
                    
                    # Update minimum distance in that sector
                    min_dist_in_sector[sector_index] = min(min_dist_in_sector[sector_index], dist_to_obs)

        # Update vision field based on minimum distances (normalized)
        self.vision_field = min_dist_in_sector / vision_range
        self.vision_field = np.clip(self.vision_field, 0.0, 1.0)

        # Update goal perception
        vec_to_goal = self.goal_position - self.position
        self.goal_distance = np.linalg.norm(vec_to_goal)
        if self.goal_distance > 0:
            world_angle_goal = np.arctan2(vec_to_goal[1], vec_to_goal[0])
            self.goal_direction_relative = (world_angle_goal - self.orientation + np.pi) % (2 * np.pi) - np.pi
        else:
            self.goal_direction_relative = 0.0
            
        # Check goal reached
        if self.goal_distance < self.goal_distance_threshold:
            self.goal_reached = True
            
    def compute_action(self) -> Tuple[float, float]:
        """Compute rotation and speed factor based on perception."""
        if self.goal_reached: return 0.0, 0.0

        num_sectors = self.parameters["num_vision_sectors"]
        vision_angle = self.parameters["vision_angle"]
        sector_angle = vision_angle / num_sectors

        # --- Obstacle Avoidance --- 
        # Simple avoidance: turn away from the most blocked sector (lowest value)
        # More weight to closer obstacles (lower vision_field value)
        turn_avoidance = 0.0
        weights = 1.0 - self.vision_field # Higher weight for blocked sectors
        if np.sum(weights) > 0:
            # Calculate weighted average angle of blocked sectors
            # Angle of the center of each sector relative to agent orientation
            sector_centers_relative = np.linspace(-vision_angle/2 + sector_angle/2, 
                                                 vision_angle/2 - sector_angle/2, 
                                                 num_sectors)
            # Weighted average direction to turn *away* from obstacles
            avg_obstacle_angle = np.sum(weights * sector_centers_relative) / np.sum(weights)
            turn_avoidance = -avg_obstacle_angle * 0.8 # Turn away proportionally
            
        # --- Goal Seeking --- 
        turn_goal = self.goal_direction_relative * 0.5 # Turn towards goal proportionally
        
        # --- Combine Behaviors --- 
        # Simple blending based on average blockage
        avg_blockage = 1.0 - np.mean(self.vision_field)
        rotation = (1.0 - avg_blockage) * turn_goal + avg_blockage * turn_avoidance
        
        # Limit rotation speed
        max_rot = self.parameters["max_rotation"]
        rotation = np.clip(rotation, -max_rot, max_rot)
        
        # Speed factor: slow down if turning sharply or obstacles are very close
        speed_factor = 1.0 - abs(rotation / max_rot) * 0.5 # Slow down when turning
        min_vision_val = np.min(self.vision_field)
        if min_vision_val < 0.2: # If something is very close
            speed_factor *= min_vision_val / 0.2 # Slow down significantly
            
        return rotation, max(0.0, speed_factor) # Ensure speed factor is non-negative

    def move(self, rotation: float, speed: float, timestep: float = 0.1):
        """Update agent position and orientation."""
        self.orientation += rotation * timestep
        self.orientation = (self.orientation + np.pi) % (2 * np.pi) - np.pi # Normalize to [-pi, pi]
        
        direction_vector = np.array([np.cos(self.orientation), np.sin(self.orientation)])
        new_position = self.position + direction_vector * speed * timestep

        # Basic collision check with environment boundaries (if env exists)
        env_width = self.environment.get("width", np.inf) if self.environment else np.inf
        env_height = self.environment.get("height", np.inf) if self.environment else np.inf
        body_radius = 0.3 # Assume a small radius for boundary checks
        
        valid_move = True
        if not (body_radius <= new_position[0] <= env_width - body_radius and 
                body_radius <= new_position[1] <= env_height - body_radius):
            valid_move = False
            
        # Basic collision with obstacles (if env exists)
        if valid_move and self.environment:
            obstacles = self.environment.get("obstacles", [])
            for obs_pos_list in obstacles:
                 obs_pos = np.array(obs_pos_list)
                 # Simple circle collision (assuming obstacle radius = 0.5)
                 if np.linalg.norm(new_position - obs_pos) < body_radius + 0.5:
                     valid_move = False
                     break 
                     
        if valid_move:
            self.position = new_position
            # Update history only if moved (relevant for NOMINATIVE)
            if self.case == Case.NOMINATIVE:
                self.position_history.append(self.position.copy())
                self.orientation_history.append(self.orientation)
        else:
             # Optionally log collision or boundary hit
             pass
             
    def _calculate_trajectory_length(self) -> float:
        """Calculate the total length of the path traveled."""
        if len(self.position_history) < 2:
            return 0.0
        diffs = np.diff(np.array(self.position_history), axis=0)
        distances = np.linalg.norm(diffs, axis=1)
        return np.sum(distances)

# --- Main Execution Block (Example Usage) ---
if __name__ == "__main__":
    logging.info("Starting Animal Agent Example")
    
    # Create environment
    env_details = {
        "width": 20.0,
        "height": 20.0,
        "obstacles": [
            [5.0, 5.0], [10.0, 8.0], [15.0, 5.0],
            [7.0, 12.0], [12.0, 15.0]
        ],
        "goal": [18.0, 18.0]
    }

    # Create agent
    agent = AnimalAgent(
        position=np.array([2.0, 2.0]),
        goal_position=np.array(env_details["goal"])
    )
    
    max_steps = 200
    history = []

    logging.info("Running simulation...")
    for step in range(max_steps):
        # Set agent to NOMINATIVE for this step
        agent.case = Case.NOMINATIVE 
        update_result = agent.update(env_details) # Pass environment details
        history.append(agent.position.copy())
        
        if agent.goal_reached:
            logging.info(f"Goal reached at step {step+1}!")
            break
            
        # Example: Occasionally change goal using DATIVE case
        # if step == 50:
        #     agent.case = Case.DATIVE
        #     agent.update({"goal_position": [2.0, 18.0]})
            
    if not agent.goal_reached:
         logging.info(f"Simulation ended after {max_steps} steps, goal not reached.")

    # --- Visualization ---
    logging.info("Generating visualization...")
    fig, ax = plt.subplots(figsize=(8, 8))
    ax.set_xlim(0, env_details["width"])
    ax.set_ylim(0, env_details["height"])
    ax.set_aspect('equal')
    ax.set_title("Animal Agent Path")
    ax.set_xlabel("X")
    ax.set_ylabel("Y")

    # Plot obstacles
    for obs in env_details["obstacles"]:
        circle = plt.Circle(obs, 0.5, color='gray', alpha=0.7)
        ax.add_patch(circle)

    # Plot goal
    goal_circle = plt.Circle(env_details["goal"], agent.goal_distance_threshold, color='green', alpha=0.5)
    ax.add_patch(goal_circle)

    # Plot path
    path = np.array(agent.position_history)
    ax.plot(path[:, 0], path[:, 1], marker='o', linestyle='-', color='blue', markersize=3, label="Path")
    ax.plot(path[0, 0], path[0, 1], 'go', markersize=8, label="Start") # Start
    ax.plot(path[-1, 0], path[-1, 1], 'ro', markersize=8, label="End")   # End
    
    ax.legend()
    ax.grid(True, alpha=0.3)
    
    # Save the plot
    timestamp = time.strftime("%Y%m%d_%H%M%S")
    filename = os.path.join(OUTPUT_DIR, f"animal_agent_path_{timestamp}.png")
    fig.savefig(filename)
    logging.info(f"Saved agent path plot to: {filename}")
    plt.close(fig) 