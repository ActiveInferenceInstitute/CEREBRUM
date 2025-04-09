"""
Streamlined Animal Agent Model

This module implements a streamlined version of the animal agent model using the
CEREBRUM framework. The animal agent uses active inference for decision-making
and navigation.

This version simplifies the original implementation while maintaining the core
case-based reasoning functionality.
"""

import numpy as np
import matplotlib.pyplot as plt
from typing import Dict, List, Tuple, Any, Optional
from matplotlib.patches import Circle, Arrow
import math
import logging
import os
import time
import io
import sys

# Handle imports properly whether file is run directly or imported as a module
try:
    # Try relative imports (work when imported as a module)
    from ..core.model import Case, Model 
    from .environment import Environment, create_sample_environment
    from ..transformations.case_transformations import transform_case
except ImportError:
    # Fall back to absolute imports (work when run directly)
    from src.core.model import Case, Model 
    from src.examples.environment import Environment, create_sample_environment
    from src.transformations.case_transformations import transform_case

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

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

class StreamlinedAnimalAgent(Model):
    """
    A streamlined version of the animal agent demonstrating basic movement,
    perception, and case handling within an environment.
    Inherits from the base Model class.
    """
    
    def __init__(
        self,
        position: List[float] = None,
        orientation: float = 0.0,
        speed: float = 0.5,
        vision_range: float = 5.0,
        body_radius: float = 0.5,
        goal_position: List[float] = None,
        environment: Optional[Environment] = None
    ):
        """
        Initialize the streamlined animal agent.
        
        Args:
            position: Initial [x, y] position
            orientation: Initial orientation angle (radians)
            speed: Movement speed
            vision_range: Range of agent's vision
            body_radius: Radius of the agent's body for collision
            goal_position: Target goal position [x, y]
            environment: The environment object the agent exists in
        """
        super().__init__(name="StreamlinedAnimal")
        
        self.environment = environment if environment is not None else Environment()
        
        self.position = np.array(position) if position is not None else np.array([self.environment.width / 2, self.environment.height / 2])
        self.orientation = orientation
        self.body_radius = body_radius
        
        self.parameters["speed"] = speed
        self.parameters["vision_range"] = vision_range
        
        self.goal_position = np.array(goal_position) if goal_position is not None else self.environment.goal_position
        
        self.sensory_state = {
            "visible_obstacles": [],
            "goal_visible": False,
            "goal_distance": np.inf,
            "goal_direction": np.zeros(2)
        }
        
    def update_sensory_state(self) -> Dict[str, Any]:
        """
        Update the agent's sensory state based on the environment.
        
        Returns:
            Updated sensory state dictionary
        """
        self.sensory_state["position"] = self.position.copy()
        self.sensory_state["orientation"] = self.orientation
        
        goal_vector = self.goal_position - self.position
        self.sensory_state["goal_distance"] = np.linalg.norm(goal_vector)
        
        if self.sensory_state["goal_distance"] > 0:
            self.sensory_state["goal_direction"] = goal_vector / self.sensory_state["goal_distance"]
        else:
            self.sensory_state["goal_direction"] = np.zeros(2)
        
        self.sensory_state["goal_visible"] = self.sensory_state["goal_distance"] <= self.parameters["vision_range"]
        
        self.sensory_state["visible_obstacles"] = self.perceive_obstacles()
        
        return self.sensory_state
    
    def perceive_obstacles(self) -> List[Dict[str, Any]]:
        """
        Detect obstacles within the agent's vision range.
        
        Returns:
            List of visible obstacles with position and distance
        """
        visible_obstacles = []
        
        for obstacle in self.environment.obstacles:
            obstacle_pos = np.array(obstacle)
            distance = np.linalg.norm(self.position - obstacle_pos)
            
            if distance <= self.parameters["vision_range"]:
                direction = (obstacle_pos - self.position)
                if distance > 0:
                    direction = direction / distance
                
                visible_obstacles.append({
                    "position": obstacle_pos,
                    "distance": distance,
                    "direction": direction
                })
        
        return visible_obstacles
    
    def compute_action(self) -> Tuple[float, float]:
        """
        Compute the next action based on the current sensory state.
        
        This is the main decision function that combines all cases.
        
        Returns:
            (move_direction, turn_angle) tuple for the next action
        """
        move_direction = 0.0
        turn_angle = 0.0
        
        self_state = self.handle_nominative()
        
        target_info = self.handle_accusative()
        
        avoid_vector = self.handle_genitive()
        
        if target_info["has_goal"]:
            goal_angle = math.atan2(target_info["goal_direction"][1], 
                                    target_info["goal_direction"][0])
            absolute_goal_angle = goal_angle
            relative_goal_angle = absolute_goal_angle - self.orientation
            
            relative_goal_angle = ((relative_goal_angle + math.pi) % (2 * math.pi)) - math.pi
            
            turn_angle = relative_goal_angle * 0.5
            
            if len(avoid_vector) > 0 and np.linalg.norm(avoid_vector) > 0:
                avoidance_angle = math.atan2(avoid_vector[1], avoid_vector[0])
                absolute_avoid_angle = avoidance_angle
                relative_avoid_angle = absolute_avoid_angle - self.orientation
                
                relative_avoid_angle = ((relative_avoid_angle + math.pi) % (2 * math.pi)) - math.pi
                
                min_obstacle_dist = float('inf')
                for obs in self.sensory_state["visible_obstacles"]:
                    min_obstacle_dist = min(min_obstacle_dist, obs["distance"])
                
                if min_obstacle_dist < self.parameters["vision_range"]:
                    avoidance_weight = 1.0 - (min_obstacle_dist / self.parameters["vision_range"])
                    avoidance_weight = min(0.8, avoidance_weight)
                    goal_weight = 1.0 - avoidance_weight
                    
                    turn_angle = (goal_weight * relative_goal_angle + 
                                 avoidance_weight * relative_avoid_angle)
        
        move_direction = 1.0 - (abs(turn_angle) / math.pi) * 0.5
        
        return move_direction, turn_angle
    
    def move(self, timestep: float = 0.1) -> None:
        """
        Move the agent based on the computed action.
        
        Args:
            timestep: Time step for the movement simulation
        """
        move_direction, turn_angle = self.compute_action()
        
        self.orientation += turn_angle * timestep
        self.orientation = self.orientation % (2 * math.pi)
        
        direction_vector = np.array([
            math.cos(self.orientation),
            math.sin(self.orientation)
        ])
        
        new_position = self.position + direction_vector * move_direction * self.parameters["speed"] * timestep
        
        if self.environment.is_in_bounds(new_position) and not self.environment.is_collision(new_position, self.body_radius):
            self.position = new_position
        
        self.update_sensory_state()
    
    def handle_nominative(self) -> Dict[str, Any]:
        """
        Nominative case: Identifies the agent's own state.
        
        Returns:
            Dictionary containing the agent's self-identification
        """
        return {
            "position": self.position.copy(),
            "orientation": self.orientation,
            "speed": self.parameters["speed"]
        }
    
    def handle_accusative(self) -> Dict[str, Any]:
        """
        Accusative case: Identifies the target/goal.
        
        Returns:
            Dictionary containing goal information
        """
        goal_direction = np.zeros(2)
        has_goal = False
        
        if self.sensory_state["goal_visible"]:
            goal_direction = self.sensory_state["goal_direction"]
            has_goal = True
        
        return {
            "has_goal": has_goal,
            "goal_position": self.goal_position.copy(),
            "goal_distance": self.sensory_state["goal_distance"],
            "goal_direction": goal_direction
        }
    
    def handle_genitive(self) -> np.ndarray:
        """
        Genitive case: Handles relationships and obstacles.
        
        Returns:
            Avoidance vector (direction to move away from obstacles)
        """
        avoid_vector = np.zeros(2)
        
        for obstacle in self.sensory_state["visible_obstacles"]:
            distance = max(obstacle["distance"] - self.body_radius, 0.1)
            
            force_magnitude = (self.parameters["vision_range"] / distance) ** 2
            
            force_direction = -obstacle["direction"]
            
            avoid_vector += force_direction * force_magnitude
        
        if np.linalg.norm(avoid_vector) > 0:
            avoid_vector = avoid_vector / np.linalg.norm(avoid_vector)
        
        return avoid_vector
    
    def handle_dative(self, target_position: np.ndarray) -> np.ndarray:
        """
        Dative case: Handles movement toward a target.
        
        Args:
            target_position: Position to move toward
            
        Returns:
            Direction vector toward the target
        """
        direction = target_position - self.position
        distance = np.linalg.norm(direction)
        
        if distance > 0:
            direction = direction / distance
        
        return direction
    
    def handle_instrumental(self, action_params: Dict[str, Any]) -> Dict[str, Any]:
        """
        Instrumental case: Handles means or instruments for actions.
        
        Args:
            action_params: Parameters for the action
            
        Returns:
            Modified action parameters
        """
        if "speed" in action_params:
            min_obstacle_dist = float('inf')
            for obs in self.sensory_state["visible_obstacles"]:
                min_obstacle_dist = min(min_obstacle_dist, obs["distance"])
            
            if min_obstacle_dist < self.parameters["vision_range"]:
                speed_factor = min_obstacle_dist / self.parameters["vision_range"]
                action_params["speed"] = action_params["speed"] * speed_factor
        
        return action_params
    
    def handle_locative(self) -> Dict[str, Any]:
        """
        Locative case: Handles spatial location information.
        
        Returns:
            Dictionary with spatial relationship data
        """
        surroundings = {
            "position": self.position.copy(),
            "in_bounds": self.environment.is_in_bounds(self.position),
            "near_goal": self.environment.is_goal_reached(
                self.position, threshold=self.parameters["vision_range"]*0.5
            ),
            "at_goal": self.environment.is_goal_reached(self.position),
            "obstacle_distances": []
        }
        
        for obs in self.sensory_state["visible_obstacles"]:
            surroundings["obstacle_distances"].append(obs["distance"])
        
        return surroundings
    
    def is_goal_reached(self) -> bool:
        """
        Check if the agent has reached the goal.
        
        Returns:
            True if goal reached, False otherwise
        """
        return self.environment.is_goal_reached(self.position)
    
    def visualize(self, ax=None):
        """
        Visualize the agent in its environment.
        
        Args:
            ax: Optional matplotlib axes to plot on
            
        Returns:
            (figure, axes) tuple
        """
        if ax is None:
            fig, ax = self.environment.plot()
        else:
            fig = ax.figure
        
        agent_circle = Circle(
            self.position, 
            self.body_radius, 
            fill=True, 
            color='blue', 
            alpha=0.7
        )
        ax.add_patch(agent_circle)
        
        arrow_length = self.body_radius * 2
        arrow_end = self.position + arrow_length * np.array([
            np.cos(self.orientation), 
            np.sin(self.orientation)
        ])
        
        arrow = Arrow(
            self.position[0], 
            self.position[1],
            arrow_end[0] - self.position[0],
            arrow_end[1] - self.position[1],
            width=0.2,
            color='black'
        )
        ax.add_patch(arrow)
        
        vision_circle = Circle(
            self.position,
            self.parameters["vision_range"],
            fill=False,
            color='lightblue',
            linestyle='--',
            alpha=0.3
        )
        ax.add_patch(vision_circle)
        
        return fig, ax

    def _update_nominative(self, data: Any) -> Dict[str, Any]:
        """NOMINATIVE: Act - perceive, decide, move."""
        self.update_sensory_state()
        move_dir, turn_angle = self.compute_action()
        self.move(turn_angle, move_dir)
        return {"action": "move", "turn": turn_angle, "move_dir": move_dir}

    def _update_accusative(self, data: Any) -> Dict[str, Any]:
        """ACCUSATIVE: Be updated - e.g., change parameters."""
        updated_params = {}
        if isinstance(data, dict):
            for key, value in data.items():
                if key in self.parameters:
                    self.parameters[key] = value
                    updated_params[key] = value
        return {"action": "update_params", "updated": updated_params}

    def _update_genitive(self, data: Any) -> Dict[str, Any]:
        """GENITIVE: Report state/possess info."""
        return {"action": "report", "state": self.get_state_dict()}

    def _update_dative(self, data: Any) -> Dict[str, Any]:
        """DATIVE: Receive info - e.g., new goal."""
        if isinstance(data, dict) and "goal_position" in data:
            self.goal_position = np.array(data["goal_position"])
            return {"action": "set_goal", "new_goal": self.goal_position.tolist()}
        return {"action": "receive_data", "data_received": data}

    def _update_instrumental(self, data: Any) -> Dict[str, Any]:
        """INSTRUMENTAL: Act as tool - e.g., calculate path."""
        return {"action": "capability_report", "can_calculate_path": True}

    def _update_locative(self, data: Any) -> Dict[str, Any]:
        """LOCATIVE: Provide context - e.g., report surroundings."""
        self.update_sensory_state()
        return {"action": "report_context", "sensory_state": self.sensory_state}

    def _update_ablative(self, data: Any) -> Dict[str, Any]:
        """ABLATIVE: Report origin/history (Not implemented fully here)."""
        return {"action": "report_history", "history_available": False}

    def _update_vocative(self, data: Any) -> Dict[str, Any]:
        """VOCATIVE: Respond to address - identify self."""
        return {"action": "identify", "name": self.name, "id": self.id}

    def free_energy(self) -> float:
        dist = np.linalg.norm(self.position - self.goal_position)
        return dist

    def get_state_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "id": self.id,
            "case": self.case.value,
            "position": self.position.tolist(),
            "orientation": self.orientation,
            "parameters": self.parameters.copy(),
            "goal_position": self.goal_position.tolist(),
            "sensory_state": self.sensory_state
        }

def demonstrate_animal_agent():
    """
    Demonstrate the animal agent navigating toward a goal and visualize case transformations.
    """
    logging.info("Starting case transformation visualization demo.")
    env = create_sample_environment()
    agent = StreamlinedAnimalAgent(
        position=[10.0, 10.0],
        orientation=np.pi/4,
        environment=env
    )
    
    cases_to_visualize = [
        Case.NOMINATIVE, 
        Case.ACCUSATIVE, 
        Case.GENITIVE, 
        Case.INSTRUMENTAL,
        Case.LOCATIVE
    ]
    num_cases = len(cases_to_visualize)
    
    summary_fig, summary_axs = plt.subplots(1, num_cases, figsize=(5 * num_cases, 5))
    if num_cases == 1:
        summary_axs = [summary_axs]
        
    summary_fig.suptitle("Animal Agent Case Transformations")

    vis_results = {}

    for i, case in enumerate(cases_to_visualize):
        logging.info(f"Visualizing agent in case: {case.name}")
        
        transform_case(agent, case)
        
        temp_fig, temp_ax = plt.subplots() 
        agent.visualize(ax=temp_ax)
        temp_ax.set_title(f"Case: {case.name} ({case.value})")
        
        buf = io.BytesIO()
        temp_fig.savefig(buf, format='png', bbox_inches='tight')
        buf.seek(0)
        img = plt.imread(buf)
        buf.close()
        plt.close(temp_fig)
        
        vis_results[case] = img

        ax = summary_axs[i]
        ax.imshow(img)
        ax.axis('off')
        ax.set_title(f"Case: {case.name}")
        
    plt.tight_layout(rect=[0, 0.03, 1, 0.95])
    
    timestamp = time.strftime("%Y%m%d_%H%M%S")
    filename = os.path.join(OUTPUT_DIR, f"animal_agent_case_summary_{timestamp}.png")
    summary_fig.savefig(filename)
    logging.info(f"Saved case transformation summary plot to: {filename}")
    plt.close(summary_fig)

if __name__ == "__main__":
    demonstrate_animal_agent() 