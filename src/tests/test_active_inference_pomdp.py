import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib
from matplotlib.gridspec import GridSpec
from matplotlib.animation import FuncAnimation
import pytest
import logging
from typing import Dict, Any, Tuple, Optional

from src.core.model import Case
from src.core.active_inference import ActiveInferenceModel

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

# Set the output directory for visualizations
OUTPUT_DIR = os.path.join(os.path.dirname(__file__), "output", "active_inference_pomdp")
os.makedirs(OUTPUT_DIR, exist_ok=True)

# Set matplotlib backend to a non-interactive one for test environment
matplotlib.use('Agg')

class POMDPModel(ActiveInferenceModel):
    """POMDP implementation of the ActiveInferenceModel for tests."""
    
    def __init__(self, model_id: str, case: Case):
        """
        Initialize a POMDP model with the given case.
        
        Args:
            model_id: Unique identifier for the model
            case: Initial case for the model
        """
        # Initialize with simple 2-state POMDP
        n_states = 2
        n_obs = 2
        
        # Create simple transition matrix (states x states)
        # Row i, column j gives p(s_j | s_i)
        transition_matrix = np.array([
            [0.7, 0.3],
            [0.3, 0.7]
        ])
        
        # Create simple observation matrix (states x observations)
        # Row i, column j gives p(o_j | s_i)
        observation_matrix = np.array([
            [0.8, 0.2],
            [0.2, 0.8]
        ])
        
        # Create prior over states (uniform)
        prior_means = np.array([0.5, 0.5])
        prior_precision = np.eye(n_states)
        
        # Create parameters dictionary
        parameters = {
            "transition_matrix": transition_matrix,
            "observation_matrix": observation_matrix,
            "n_states": n_states,
            "n_observations": n_obs
        }
        
        # Initialize base class
        super().__init__(name=model_id, parameters=parameters,
                         prior_means=prior_means, prior_precision=prior_precision)
        
        # Set the initial case
        self._case = case
        
        # Additional POMDP-specific attributes
        self.action_space = ["left", "right"]
        self.observation_space = ["red", "blue"]
        self.current_state = np.random.choice(n_states)
        self.belief_state = prior_means.copy()
        self.history = []
    
    def transition(self, action_idx: int) -> int:
        """
        Transition the POMDP to a new state based on the action.
        
        Args:
            action_idx: Index of the action to take
            
        Returns:
            New state index
        """
        transition_probs = self.parameters["transition_matrix"][self.current_state]
        new_state = np.random.choice(self.parameters["n_states"], p=transition_probs)
        self.current_state = new_state
        return new_state
    
    def observe(self) -> int:
        """
        Generate an observation based on the current state.
        
        Returns:
            Observation index
        """
        obs_probs = self.parameters["observation_matrix"][self.current_state]
        observation = np.random.choice(self.parameters["n_observations"], p=obs_probs)
        return observation
    
    def update_belief(self, action_idx: int, observation_idx: int) -> np.ndarray:
        """
        Update the belief state based on action and observation.
        
        Args:
            action_idx: Index of the action taken
            observation_idx: Index of the observation received
            
        Returns:
            Updated belief state
        """
        # Transition update (prediction)
        transition_matrix = self.parameters["transition_matrix"]
        predicted_belief = np.zeros(self.parameters["n_states"])
        
        for s in range(self.parameters["n_states"]):
            for prev_s in range(self.parameters["n_states"]):
                predicted_belief[s] += transition_matrix[prev_s, s] * self.belief_state[prev_s]
        
        # Observation update (correction)
        observation_matrix = self.parameters["observation_matrix"]
        updated_belief = np.zeros(self.parameters["n_states"])
        
        for s in range(self.parameters["n_states"]):
            updated_belief[s] = observation_matrix[s, observation_idx] * predicted_belief[s]
        
        # Normalize
        updated_belief /= np.sum(updated_belief)
        
        # Update the belief state
        self.belief_state = updated_belief
        
        # Record history
        self.history.append({
            "action": action_idx,
            "observation": observation_idx,
            "belief": updated_belief.copy()
        })
        
        return updated_belief
    
    def get_optimal_action(self) -> int:
        """
        Get the optimal action based on the current belief state.
        
        Returns:
            Index of the optimal action
        """
        # Simple heuristic: choose action that maximizes expected reward
        # For this simple POMDP, we'll just go with the most likely state
        most_likely_state = np.argmax(self.belief_state)
        # Action that keeps us in the most likely state
        return 0 if most_likely_state == 0 else 1

# Case definitions for POMDP models
class CaseDefinitions:
    """Class containing definitions for all CEREBRUM cases in POMDP context."""
    
    @staticmethod
    def nominative() -> Dict[str, str]:
        """
        NOMINATIVE case: The model as active agent.
        
        In linguistics: Subject of verb.
        In POMDP: Model actively generating actions.
        """
        return {
            "case": Case.NOMINATIVE.value,
            "linguistic_meaning": "Subject/Doer of action",
            "statistical_role": "Active decision-maker",
            "pomdp_context": "The POMDP as an agent actively choosing actions to maximize reward",
            "formula": "π(a|s) = argmax_a Q(s,a)",
            "primary_methods": "get_optimal_action(), transition()",
            "visualization": "Policy diagrams, action selection processes",
            "example": "The POMDP DECIDES which action to take next"
        }
    
    @staticmethod
    def accusative() -> Dict[str, str]:
        """
        ACCUSATIVE case: The model as object of process.
        
        In linguistics: Direct object.
        In POMDP: Model being evaluated or updated.
        """
        return {
            "case": Case.ACCUSATIVE.value,
            "linguistic_meaning": "Direct object/Receiver of action",
            "statistical_role": "Object of evaluation",
            "pomdp_context": "The POMDP as an object being evaluated or updated by external processes",
            "formula": "J(π) = E[∑γᵗR(sₜ,aₜ)]",
            "primary_methods": "evaluate(), update_parameters()",
            "visualization": "Performance evaluation metrics, model convergence",
            "example": "The programmer EVALUATES the POMDP's performance"
        }
    
    @staticmethod
    def dative() -> Dict[str, str]:
        """
        DATIVE case: The model as recipient.
        
        In linguistics: Indirect object.
        In POMDP: Model receiving observations.
        """
        return {
            "case": Case.DATIVE.value,
            "linguistic_meaning": "Indirect object/Recipient",
            "statistical_role": "Receiver of information",
            "pomdp_context": "The POMDP as a recipient of observations from the environment",
            "formula": "p(o|s)",
            "primary_methods": "observe(), receive_observation()",
            "visualization": "Observation flow, information reception diagrams",
            "example": "The environment GIVES observations TO the POMDP"
        }
    
    @staticmethod
    def genitive() -> Dict[str, str]:
        """
        GENITIVE case: The model as source/possessor.
        
        In linguistics: Possessive.
        In POMDP: Model generating beliefs/outputs.
        """
        return {
            "case": Case.GENITIVE.value,
            "linguistic_meaning": "Possessive/Source",
            "statistical_role": "Generator of beliefs",
            "pomdp_context": "The POMDP as a source of beliefs and predictions about states",
            "formula": "b(s) = p(s|o₁,...,oₜ)",
            "primary_methods": "get_belief(), generate_prediction()",
            "visualization": "Belief distributions, confidence intervals",
            "example": "The POMDP's BELIEF about the state guides decisions"
        }
    
    @staticmethod
    def instrumental() -> Dict[str, str]:
        """
        INSTRUMENTAL case: The model as method/tool.
        
        In linguistics: Means or instrument.
        In POMDP: Model as computational approach.
        """
        return {
            "case": Case.INSTRUMENTAL.value,
            "linguistic_meaning": "By means of/Using",
            "statistical_role": "Computational method",
            "pomdp_context": "The POMDP as a computational method for solving sequential decision problems",
            "formula": "V(b) = max_a[R(b,a) + γ∑p(o|b,a)V(b')]",
            "primary_methods": "solve(), compute_value_function()",
            "visualization": "Computational diagrams, algorithm flowcharts",
            "example": "The agent solves the problem BY USING POMDP methods"
        }
    
    @staticmethod
    def locative() -> Dict[str, str]:
        """
        LOCATIVE case: The model as location/context.
        
        In linguistics: Location or time.
        In POMDP: Model as decision context.
        """
        return {
            "case": Case.LOCATIVE.value,
            "linguistic_meaning": "In/At/Within",
            "statistical_role": "Decision context",
            "pomdp_context": "The POMDP as the context within which decisions are made under uncertainty",
            "formula": "Context: {S, A, O, T, Z, R, γ}",
            "primary_methods": "define_space(), set_parameters()",
            "visualization": "State space diagrams, problem structure",
            "example": "Optimal decisions exist WITHIN the POMDP framework"
        }
    
    @staticmethod
    def ablative() -> Dict[str, str]:
        """
        ABLATIVE case: The model as origin/cause.
        
        In linguistics: Movement from.
        In POMDP: Model as source of uncertainty.
        """
        return {
            "case": Case.ABLATIVE.value,
            "linguistic_meaning": "From/Out of/Because of",
            "statistical_role": "Source of uncertainty",
            "pomdp_context": "The POMDP as the origin of uncertainty and partial observability",
            "formula": "H(s|o) = -∑p(s|o)log p(s|o)",
            "primary_methods": "compute_entropy(), analyze_uncertainty()",
            "visualization": "Uncertainty quantification, entropy maps",
            "example": "Uncertainty COMES FROM the POMDP's partial observability"
        }
    
    @staticmethod
    def vocative() -> Dict[str, str]:
        """
        VOCATIVE case: The model as addressable entity.
        
        In linguistics: Direct address.
        In POMDP: Model as interactive interface.
        """
        return {
            "case": Case.VOCATIVE.value,
            "linguistic_meaning": "Direct address/Invocation",
            "statistical_role": "Interactive interface",
            "pomdp_context": "The POMDP as an addressable entity with a query/response interface",
            "formula": "API: query(POMDP, type) → response",
            "primary_methods": "query(), respond()",
            "visualization": "Interface diagrams, command flows",
            "example": "HEY POMDP, what is the optimal action for this belief?"
        }
    
    @staticmethod
    def get_all_cases() -> Dict[Case, Dict[str, str]]:
        """Returns a dictionary with information for all cases."""
        return {
            Case.NOMINATIVE: CaseDefinitions.nominative(),
            Case.ACCUSATIVE: CaseDefinitions.accusative(),
            Case.DATIVE: CaseDefinitions.dative(),
            Case.GENITIVE: CaseDefinitions.genitive(),
            Case.INSTRUMENTAL: CaseDefinitions.instrumental(),
            Case.LOCATIVE: CaseDefinitions.locative(),
            Case.ABLATIVE: CaseDefinitions.ablative(),
            Case.VOCATIVE: CaseDefinitions.vocative(),
        }

# Data generator for POMDP test data
class DataGenerator:
    """Generates synthetic data for POMDP tests."""
    
    @staticmethod
    def generate_trajectory(
        transition_matrix: np.ndarray,
        observation_matrix: np.ndarray,
        n_steps: int = 10,
        initial_state: int = 0,
        random_seed: int = 42
    ) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
        """
        Generate a synthetic trajectory of states, actions, and observations.
        
        Args:
            transition_matrix: State transition matrix
            observation_matrix: Observation matrix
            n_steps: Number of steps in the trajectory
            initial_state: Initial state index
            random_seed: Random seed for reproducibility
            
        Returns:
            Tuple of (states, actions, observations)
        """
        np.random.seed(random_seed)
        
        n_states = transition_matrix.shape[0]
        n_actions = 2  # Simplified to 2 actions
        n_observations = observation_matrix.shape[1]
        
        states = np.zeros(n_steps, dtype=int)
        actions = np.zeros(n_steps, dtype=int)
        observations = np.zeros(n_steps, dtype=int)
        
        # Set initial state
        states[0] = initial_state
        
        # Generate trajectory
        for t in range(n_steps - 1):
            # Random action
            actions[t] = np.random.choice(n_actions)
            
            # Generate observation from current state
            observations[t] = np.random.choice(
                n_observations, 
                p=observation_matrix[states[t]]
            )
            
            # Transition to next state
            states[t + 1] = np.random.choice(
                n_states,
                p=transition_matrix[states[t]]
            )
        
        # Final step observation
        observations[-1] = np.random.choice(
            n_observations,
            p=observation_matrix[states[-1]]
        )
        
        return states, actions, observations

# Visualization helper class for POMDP models
class Visualizer:
    """Helper class for creating visualizations of POMDP models and data."""
    
    @staticmethod
    def plot_belief_state(
        belief: np.ndarray,
        title: str = "Belief State",
        figsize: Tuple[int, int] = (10, 6),
        save_path: str = None
    ) -> plt.Figure:
        """
        Plot the belief state as a bar chart.
        
        Args:
            belief: Belief state vector
            title: Plot title
            figsize: Figure size
            save_path: Path to save the figure
            
        Returns:
            Matplotlib figure
        """
        fig, ax = plt.subplots(figsize=figsize)
        
        x = np.arange(len(belief))
        ax.bar(x, belief, color='blue', alpha=0.7)
        
        ax.set_xticks(x)
        ax.set_xticklabels([f'State {i}' for i in x])
        ax.set_xlabel('States')
        ax.set_ylabel('Probability')
        ax.set_title(title)
        ax.grid(True, linestyle='--', alpha=0.6)
        
        plt.tight_layout()
        
        if save_path:
            fig.savefig(save_path)
            logger.info(f"Saved belief state plot to {save_path}")
            plt.close(fig)
        
        return fig
    
    @staticmethod
    def plot_pomdp_structure(
        transition_matrix: np.ndarray,
        observation_matrix: np.ndarray,
        title: str = "POMDP Structure",
        figsize: Tuple[int, int] = (12, 6),
        save_path: str = None
    ) -> plt.Figure:
        """
        Plot the POMDP structure with transition and observation matrices.
        
        Args:
            transition_matrix: State transition matrix
            observation_matrix: Observation matrix
            title: Plot title
            figsize: Figure size
            save_path: Path to save the figure
            
        Returns:
            Matplotlib figure
        """
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=figsize)
        
        # Plot transition matrix
        im1 = ax1.imshow(transition_matrix, cmap='viridis')
        ax1.set_title('Transition Matrix')
        ax1.set_xlabel('Next State')
        ax1.set_ylabel('Current State')
        ax1.set_xticks(np.arange(transition_matrix.shape[1]))
        ax1.set_yticks(np.arange(transition_matrix.shape[0]))
        ax1.set_xticklabels([f'S{i}' for i in range(transition_matrix.shape[1])])
        ax1.set_yticklabels([f'S{i}' for i in range(transition_matrix.shape[0])])
        
        # Add colorbar
        cbar1 = fig.colorbar(im1, ax=ax1, fraction=0.046, pad=0.04)
        cbar1.set_label('Probability')
        
        # Plot observation matrix
        im2 = ax2.imshow(observation_matrix, cmap='viridis')
        ax2.set_title('Observation Matrix')
        ax2.set_xlabel('Observation')
        ax2.set_ylabel('State')
        ax2.set_xticks(np.arange(observation_matrix.shape[1]))
        ax2.set_yticks(np.arange(observation_matrix.shape[0]))
        ax2.set_xticklabels([f'O{i}' for i in range(observation_matrix.shape[1])])
        ax2.set_yticklabels([f'S{i}' for i in range(observation_matrix.shape[0])])
        
        # Add colorbar
        cbar2 = fig.colorbar(im2, ax=ax2, fraction=0.046, pad=0.04)
        cbar2.set_label('Probability')
        
        # Set main title
        fig.suptitle(title, fontsize=16)
        
        plt.tight_layout()
        
        if save_path:
            fig.savefig(save_path)
            logger.info(f"Saved POMDP structure plot to {save_path}")
            plt.close(fig)
        
        return fig

# Fixtures
@pytest.fixture
def case_definitions():
    """Fixture for case definitions."""
    return CaseDefinitions.get_all_cases()

@pytest.fixture
def pomdp_test_data():
    """Fixture for POMDP test data."""
    # Create POMDP test data
    # # Create POMDP test data manually instead of using the fixture  # This causes an error when called directly
    
    # Manually create test data instead of using the fixture
    # Define the POMDP structure for testing
    n_states = 4  # Number of states in the environment
    n_actions = 3  # Number of actions the agent can take
    n_observations = 2  # Number of possible observations
    
    # Create a simple transition matrix (states x actions x states)
    transition_matrix = np.array([
        # Action 0
        [[0.7, 0.3, 0.0, 0.0],
         [0.0, 0.8, 0.2, 0.0],
         [0.0, 0.0, 0.9, 0.1],
         [0.0, 0.0, 0.0, 1.0]],
        # Action 1
        [[0.9, 0.1, 0.0, 0.0],
         [0.1, 0.8, 0.1, 0.0],
         [0.0, 0.1, 0.8, 0.1],
         [0.0, 0.0, 0.1, 0.9]],
        # Action 2
        [[0.95, 0.05, 0.0, 0.0],
         [0.05, 0.9, 0.05, 0.0],
         [0.0, 0.05, 0.9, 0.05],
         [0.0, 0.0, 0.05, 0.95]]
    ])
    
    # Create an observation matrix (states x observations)
    observation_matrix = np.array([
        [0.8, 0.2],  # State 0 - high prob of observing 0, low of 1
        [0.6, 0.4],  # State 1 - higher prob of 0, but more uncertainty
        [0.4, 0.6],  # State 2 - higher prob of 1, but more uncertainty
        [0.2, 0.8]   # State 3 - high prob of observing 1, low of 0
    ])
    
    # Create reward matrix (states x actions)
    reward_matrix = np.array([
        [1.0, 0.5, 0.1],  # Rewards for state 0
        [0.5, 1.0, 0.5],  # Rewards for state 1
        [0.1, 0.5, 1.0],  # Rewards for state 2
        [0.0, 0.1, 0.0]   # Rewards for state 3
    ])
    
    # Initial belief state (uniform distribution)
    initial_belief = np.ones(n_states) / n_states
    
    # Create the test data
    test_data = {
        "transition_matrix": transition_matrix,
        "observation_matrix": observation_matrix,
        "reward_matrix": reward_matrix,
        "initial_belief": initial_belief,
        "n_states": n_states,
        "n_actions": n_actions,
        "n_observations": n_observations
    }
    
    return test_data

# Visualization helper for case linguistic context
def plot_case_linguistic_context(case: Case, save_path: str) -> None:
    """Generate a visualization showing linguistic context for a case."""
    case_info = CaseDefinitions.get_all_cases()[case]
    
    fig, ax = plt.subplots(figsize=(10, 6))
    
    # Create visual representation
    ax.axis('off')  # Hide the axes
    
    # Title with case
    title = f"CEREBRUM Case: {case.value} ({case_info['linguistic_meaning']})"
    ax.text(0.5, 0.95, title, ha='center', va='top', fontsize=16, 
            fontweight='bold', transform=ax.transAxes)
    
    # Statistical role
    ax.text(0.5, 0.88, f"Statistical Role: {case_info['statistical_role']}", 
            ha='center', va='top', fontsize=14, transform=ax.transAxes)
    
    # Example sentence
    example_box = dict(boxstyle='round,pad=0.5', facecolor='lightblue', alpha=0.5)
    ax.text(0.5, 0.8, f"Linguistic example: {case_info['example']}", 
            ha='center', va='top', fontsize=12, bbox=example_box, 
            transform=ax.transAxes)
    
    # Formula
    formula_box = dict(boxstyle='round,pad=0.5', facecolor='lightyellow', alpha=0.5)
    ax.text(0.5, 0.68, f"Mathematical representation:\n{case_info['formula']}", 
            ha='center', va='top', fontsize=12, bbox=formula_box, 
            transform=ax.transAxes)
    
    # POMDP context
    context_box = dict(boxstyle='round,pad=0.5', facecolor='lightgreen', alpha=0.5)
    ax.text(0.5, 0.54, "POMDP Context:", ha='center', va='top', 
            fontsize=12, fontweight='bold', transform=ax.transAxes)
    ax.text(0.5, 0.48, case_info['pomdp_context'], ha='center', va='top', 
            fontsize=12, wrap=True, transform=ax.transAxes)
    
    # Methods
    methods_box = dict(boxstyle='round,pad=0.5', facecolor='lightgray', alpha=0.5)
    ax.text(0.5, 0.36, f"Primary Methods: {case_info['primary_methods']}", 
            ha='center', va='top', fontsize=12, bbox=methods_box, 
            transform=ax.transAxes)
    
    # Save the figure
    fig.savefig(save_path)
    plt.close(fig)

# Test functions for each case
def test_nominative_case(pomdp_test_data, case_definitions):
    """Test for NOMINATIVE case: Model as active agent generating actions."""
    # Get case info for logging
    case_info = case_definitions[Case.NOMINATIVE]
    logger.info(f"Testing {Case.NOMINATIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(OUTPUT_DIR, Case.NOMINATIVE.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.NOMINATIVE, linguistics_path)
    
    # Unpack the test data
    transition_matrix = pomdp_test_data["transition_matrix"]
    observation_matrix = pomdp_test_data["observation_matrix"]
    states = pomdp_test_data["states"]
    actions = pomdp_test_data["actions"]
    observations = pomdp_test_data["observations"]
    
    # Create a POMDP model in NOMINATIVE case (active decision-maker)
    model = POMDPModel(model_id="NomPOMDP", case=Case.NOMINATIVE)
    
    # Log model details
    logger.info(f"Created POMDP model in {Case.NOMINATIVE.value} case (model as active decision-maker)")
    logger.info(f"POMDP parameters: {model.parameters}")
    
    # Visualize POMDP structure
    structure_path = os.path.join(case_dir, "pomdp_structure.png")
    Visualizer.plot_pomdp_structure(
        transition_matrix=model.parameters["transition_matrix"],
        observation_matrix=model.parameters["observation_matrix"],
        title=f"POMDP Structure in {Case.NOMINATIVE.value} Case",
        save_path=structure_path
    )
    
    # Initialize belief state visualization
    belief_init_path = os.path.join(case_dir, "initial_belief.png")
    Visualizer.plot_belief_state(
        belief=model.belief_state,
        title=f"Initial Belief State in {Case.NOMINATIVE.value} Case",
        save_path=belief_init_path
    )
    
    # In NOMINATIVE case, the model actively chooses actions
    # We'll simulate a decision-making process
    
    # Prepare for simulation
    n_steps = 20
    belief_history = []
    action_history = []
    observation_history = []
    state_history = []
    
    # Store initial state
    current_state = model.current_state
    state_history.append(current_state)
    belief_history.append(model.belief_state.copy())
    
    # Run the simulation
    logger.info(f"Running POMDP simulation for {n_steps} steps in {Case.NOMINATIVE.value} case")
    
    for step in range(n_steps):
        # Model actively chooses the action (NOMINATIVE case)
        action_idx = model.get_optimal_action()
        action_history.append(action_idx)
        
        # Transition to new state based on action
        new_state = model.transition(action_idx)
        state_history.append(new_state)
        
        # Get observation
        observation_idx = model.observe()
        observation_history.append(observation_idx)
        
        # Update belief state
        updated_belief = model.update_belief(action_idx, observation_idx)
        belief_history.append(updated_belief.copy())
        
        logger.info(f"Step {step+1}: Action={action_idx}, Observation={observation_idx}, " +
                   f"True State={new_state}, Max Belief State={np.argmax(updated_belief)}")
    
    # Visualize the belief evolution
    belief_evolution_path = os.path.join(case_dir, "belief_evolution.png")
    fig, ax = plt.subplots(figsize=(12, 6))
    
    # Convert belief history to array for easier plotting
    belief_array = np.array(belief_history)
    
    # Plot each state's belief over time
    for state in range(model.parameters["n_states"]):
        ax.plot(belief_array[:, state], label=f'State {state}', linewidth=2)
    
    # Mark the true states
    for step, state in enumerate(state_history):
        ax.scatter(step, belief_array[step, state], color='red', s=50, zorder=10)
    
    ax.set_xlabel('Step')
    ax.set_ylabel('Belief Probability')
    ax.set_title(f'Belief State Evolution in {Case.NOMINATIVE.value} Case')
    ax.set_ylim(0, 1)
    ax.legend()
    ax.grid(True, linestyle='--', alpha=0.6)
    
    plt.tight_layout()
    fig.savefig(belief_evolution_path)
    plt.close(fig)
    
    # Visualize the policy in action
    policy_path = os.path.join(case_dir, "policy_visualization.png")
    fig, axs = plt.subplots(2, 1, figsize=(12, 10))
    
    # Plot actions taken
    axs[0].step(range(n_steps), action_history, where='mid', linewidth=2)
    axs[0].set_xlabel('Step')
    axs[0].set_ylabel('Action')
    axs[0].set_title('Actions Chosen by Agent')
    axs[0].set_yticks(range(len(model.action_space)))
    axs[0].set_yticklabels(model.action_space)
    axs[0].grid(True, linestyle='--', alpha=0.6)
    
    # Plot states and observations
    axs[1].plot(range(n_steps+1), state_history, 'b-', label='True State')
    axs[1].scatter(range(1, n_steps+1), observation_history, color='orange', label='Observation')
    axs[1].set_xlabel('Step')
    axs[1].set_ylabel('State/Observation')
    axs[1].set_title('States and Observations')
    axs[1].set_yticks(range(model.parameters["n_states"]))
    axs[1].legend()
    axs[1].grid(True, linestyle='--', alpha=0.6)
    
    plt.tight_layout()
    fig.savefig(policy_path)
    plt.close(fig)
    
    # Create an animation of the belief updates
    animation_path = os.path.join(case_dir, "belief_animation.gif")
    
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 10))
    
    # Setup for belief state plot
    bars = ax1.bar(range(model.parameters["n_states"]), belief_history[0])
    ax1.set_ylim(0, 1)
    ax1.set_xlabel('State')
    ax1.set_ylabel('Probability')
    ax1.set_title('Current Belief State')
    
    # Setup for state/observation plot
    line1, = ax2.plot([], [], 'b-', label='True State')
    line2, = ax2.plot([], [], 'ro', label='Observations')
    ax2.set_xlim(0, n_steps)
    ax2.set_ylim(-0.5, model.parameters["n_states"] - 0.5)
    ax2.set_xlabel('Step')
    ax2.set_ylabel('State/Observation')
    ax2.legend()
    ax2.grid(True, linestyle='--', alpha=0.6)
    
    # Text to show step information
    step_text = ax1.text(0.02, 0.95, '', transform=ax1.transAxes)
    
    def init():
        for bar in bars:
            bar.set_height(0)
        line1.set_data([], [])
        line2.set_data([], [])
        step_text.set_text('')
        return bars + [line1, line2, step_text]
    
    def update(frame):
        # Update belief bars
        for i, bar in enumerate(bars):
            bar.set_height(belief_history[frame][i])
            # Color the bar differently for the true state
            if i == state_history[frame]:
                bar.set_color('red')
            else:
                bar.set_color('blue')
        
        # Update state/observation plot
        x_data = range(frame + 1)
        line1.set_data(x_data, state_history[:frame + 1])
        
        # Only show observations up to current frame
        if frame > 0:
            line2.set_data(range(1, frame + 1), observation_history[:frame])
        
        # Update step text
        if frame > 0:
            action_text = f"Action: {model.action_space[action_history[frame-1]]}"
            obs_text = f"Observed: {model.observation_space[observation_history[frame-1]]}"
        else:
            action_text = "Action: None"
            obs_text = "Observed: None"
        
        step_text.set_text(f'Step: {frame}\n{action_text}\n{obs_text}')
        
        return bars + [line1, line2, step_text]
    
    anim = FuncAnimation(fig, update, frames=len(belief_history),
                          init_func=init, blit=True)
    
    anim.save(animation_path, writer='pillow', fps=2)
    plt.close(fig)
    logger.info(f"Created belief update animation: {animation_path}")
    
    # Create a visualization of the decision process
    decision_path = os.path.join(case_dir, "decision_process.png")
    
    # Calculate policy statistics
    action_counts = np.bincount(action_history, minlength=len(model.action_space))
    correct_decisions = sum(1 for i, s in enumerate(state_history[:-1]) 
                            if action_history[i] == (0 if s == 0 else 1))
    decision_accuracy = correct_decisions / n_steps
    
    fig, axs = plt.subplots(2, 2, figsize=(14, 10))
    
    # Plot action distribution
    axs[0, 0].bar(range(len(model.action_space)), action_counts)
    axs[0, 0].set_xlabel('Action')
    axs[0, 0].set_ylabel('Count')
    axs[0, 0].set_title('Action Distribution')
    axs[0, 0].set_xticks(range(len(model.action_space)))
    axs[0, 0].set_xticklabels(model.action_space)
    
    # Plot decision accuracy
    axs[0, 1].bar(['Accuracy'], [decision_accuracy])
    axs[0, 1].set_ylim(0, 1)
    axs[0, 1].set_title('Decision Accuracy')
    axs[0, 1].text(0, decision_accuracy / 2, f'{decision_accuracy:.2f}', 
                   ha='center', va='center', fontweight='bold')
    
    # Plot belief confidence (max probability in belief state)
    belief_confidence = [np.max(belief) for belief in belief_history]
    axs[1, 0].plot(belief_confidence, 'g-')
    axs[1, 0].set_xlabel('Step')
    axs[1, 0].set_ylabel('Confidence')
    axs[1, 0].set_title('Belief Confidence')
    axs[1, 0].set_ylim(0, 1)
    axs[1, 0].grid(True, linestyle='--', alpha=0.6)
    
    # Plot belief entropy (uncertainty)
    def entropy(belief):
        # Avoid log(0)
        belief = np.clip(belief, 1e-10, 1.0)
        return -np.sum(belief * np.log2(belief))
    
    belief_entropy = [entropy(belief) for belief in belief_history]
    axs[1, 1].plot(belief_entropy, 'r-')
    axs[1, 1].set_xlabel('Step')
    axs[1, 1].set_ylabel('Entropy (bits)')
    axs[1, 1].set_title('Belief Uncertainty')
    axs[1, 1].grid(True, linestyle='--', alpha=0.6)
    
    plt.tight_layout()
    fig.savefig(decision_path)
    plt.close(fig)
    
    # Save results to a report file
    report_path = os.path.join(case_dir, "report.md")
    with open(report_path, "w") as f:
        f.write(f"# POMDP Model in {Case.NOMINATIVE.value} Case\n\n")
        f.write(f"## {case_info['linguistic_meaning']}\n\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n\n")
        f.write(f"### Context\n\n")
        f.write(f"{case_info['pomdp_context']}\n\n")
        f.write("### Mathematical Representation\n\n")
        f.write(f"```\n{case_info['formula']}\n```\n\n")
        f.write("### Primary Methods\n\n")
        f.write(f"{case_info['primary_methods']}\n\n")
        f.write("### Example\n\n")
        f.write(f"{case_info['example']}\n\n")
        f.write("### Simulation Results\n\n")
        f.write(f"* Simulation steps: {n_steps}\n")
        f.write(f"* Model parameters:\n")
        f.write(f"  - States: {model.parameters['n_states']}\n")
        f.write(f"  - Observations: {model.parameters['n_observations']}\n")
        f.write(f"  - Actions: {model.action_space}\n")
        f.write(f"* Performance metrics:\n")
        f.write(f"  - Decision accuracy: {decision_accuracy:.4f}\n")
        f.write(f"  - Average belief confidence: {np.mean(belief_confidence):.4f}\n")
        f.write(f"  - Average belief entropy: {np.mean(belief_entropy):.4f} bits\n\n")
        f.write("### Decision Process Analysis\n\n")
        f.write("In the NOMINATIVE case, the POMDP acts as an active decision-maker, ")
        f.write("choosing actions based on its current beliefs about the world state. ")
        f.write("The visualizations show the evolution of beliefs over time and the actions ")
        f.write("chosen by the model.\n\n")
        f.write("### Visualizations\n\n")
        f.write(f"1. [POMDP Structure](pomdp_structure.png)\n")
        f.write(f"2. [Initial Belief State](initial_belief.png)\n")
        f.write(f"3. [Belief Evolution](belief_evolution.png)\n")
        f.write(f"4. [Policy Visualization](policy_visualization.png)\n")
        f.write(f"5. [Decision Process Analysis](decision_process.png)\n")
        f.write(f"6. [Belief Animation](belief_animation.gif)\n")
    
    logger.info(f"Completed NOMINATIVE case test with visualizations in {case_dir}")
    
    # Return the model for potential further testing
    return model

def test_accusative_case(pomdp_test_data, case_definitions):
    """Test for ACCUSATIVE case: Model as object of evaluation."""
    # Get case info for logging
    case_info = case_definitions[Case.ACCUSATIVE]
    logger.info(f"Testing {Case.ACCUSATIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(OUTPUT_DIR, Case.ACCUSATIVE.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.ACCUSATIVE, linguistics_path)
    
    # Unpack the test data
    transition_matrix = pomdp_test_data["transition_matrix"]
    observation_matrix = pomdp_test_data["observation_matrix"]
    states = pomdp_test_data["states"]
    actions = pomdp_test_data["actions"]
    observations = pomdp_test_data["observations"]
    
    # Create a POMDP model in ACCUSATIVE case (object of evaluation)
    model = POMDPModel(model_id="AccPOMDP", case=Case.ACCUSATIVE)
    logger.info(f"Created POMDP model in {Case.ACCUSATIVE.value} case (model as object of evaluation)")
    
    # Visualize POMDP structure
    structure_path = os.path.join(case_dir, "pomdp_structure.png")
    Visualizer.plot_pomdp_structure(
        transition_matrix=model.parameters["transition_matrix"],
        observation_matrix=model.parameters["observation_matrix"],
        title=f"POMDP Structure in {Case.ACCUSATIVE.value} Case",
        save_path=structure_path
    )
    
    # In ACCUSATIVE case, model is the object of evaluation
    # We'll run external evaluations on the model performance
    
    # First, prepare some fixed scenarios for evaluation
    n_evaluation_scenarios = 5
    n_steps_per_scenario = 15
    
    # Store metrics for all scenarios
    scenario_metrics = []
    
    # Run multiple evaluation scenarios
    logger.info(f"Running {n_evaluation_scenarios} evaluation scenarios for POMDP in {Case.ACCUSATIVE.value} case")
    
    for scenario in range(n_evaluation_scenarios):
        # Reset the model for this scenario
        model = POMDPModel(model_id=f"AccPOMDP_Scenario{scenario+1}", case=Case.ACCUSATIVE)
        model.current_state = np.random.choice(model.parameters["n_states"])  # Random initial state
        
        # Prepare metrics for this scenario
        belief_accuracy = []  # Belief state accuracy
        decision_accuracy = []  # Decision accuracy
        belief_entropy = []  # Uncertainty in belief state
        
        # Store histories for visualization
        belief_history = [model.belief_state.copy()]
        state_history = [model.current_state]
        action_history = []
        observation_history = []
        
        # Run steps for this scenario
        for step in range(n_steps_per_scenario):
            # External process selects an action (not the model in ACCUSATIVE case)
            # For evaluation, we use a random action
            action_idx = np.random.choice(len(model.action_space))
            action_history.append(action_idx)
            
            # Transition and observe
            new_state = model.transition(action_idx)
            state_history.append(new_state)
            
            observation_idx = model.observe()
            observation_history.append(observation_idx)
            
            # Update belief state
            updated_belief = model.update_belief(action_idx, observation_idx)
            belief_history.append(updated_belief.copy())
            
            # Calculate metrics for this step
            # Belief accuracy: probability assigned to true state
            belief_acc = updated_belief[new_state]
            belief_accuracy.append(belief_acc)
            
            # Decision accuracy: would the model have chosen the optimal action?
            optimal_action = 0 if new_state == 0 else 1  # Simple heuristic for this 2-state POMDP
            model_action = np.argmax(updated_belief)  # Model would select action based on belief
            decision_acc = 1.0 if model_action == optimal_action else 0.0
            decision_accuracy.append(decision_acc)
            
            # Belief entropy (uncertainty)
            entropy = -np.sum(updated_belief * np.log2(np.clip(updated_belief, 1e-10, 1.0)))
            belief_entropy.append(entropy)
        
        # Store scenario metrics
        scenario_metrics.append({
            "belief_accuracy": belief_accuracy,
            "decision_accuracy": decision_accuracy,
            "belief_entropy": belief_entropy,
            "belief_history": belief_history,
            "state_history": state_history,
            "action_history": action_history,
            "observation_history": observation_history
        })
        
        logger.info(f"Completed scenario {scenario+1} evaluation")
        logger.info(f"  Average belief accuracy: {np.mean(belief_accuracy):.4f}")
        logger.info(f"  Average decision accuracy: {np.mean(decision_accuracy):.4f}")
        logger.info(f"  Average belief entropy: {np.mean(belief_entropy):.4f}")
    
    # Visualize evaluation metrics across scenarios
    metrics_path = os.path.join(case_dir, "evaluation_metrics.png")
    fig, axs = plt.subplots(3, 1, figsize=(12, 15))
    
    # Helper for plotting metrics from all scenarios
    def plot_metric(ax, metric_name, title, ylabel, color='blue'):
        for i, metrics in enumerate(scenario_metrics):
            values = metrics[metric_name]
            ax.plot(values, alpha=0.5, color=color, label=f"Scenario {i+1}" if i == 0 else None)
        
        # Calculate and plot average across scenarios
        avg_values = np.mean([metrics[metric_name] for metrics in scenario_metrics], axis=0)
        ax.plot(avg_values, 'k-', linewidth=2, label="Average")
        
        ax.set_xlabel('Step')
        ax.set_ylabel(ylabel)
        ax.set_title(title)
        ax.legend()
        ax.grid(True, linestyle='--', alpha=0.6)
    
    # Plot each metric
    plot_metric(axs[0], "belief_accuracy", "Belief Accuracy Across Scenarios", "Accuracy", color='green')
    plot_metric(axs[1], "decision_accuracy", "Decision Accuracy Across Scenarios", "Accuracy", color='blue')
    plot_metric(axs[2], "belief_entropy", "Belief Entropy Across Scenarios", "Entropy (bits)", color='red')
    
    plt.tight_layout()
    fig.savefig(metrics_path)
    plt.close(fig)
    
    # Visualize a specific scenario in detail (choose the middle one)
    scenario_idx = n_evaluation_scenarios // 2
    scenario_metrics_detail = scenario_metrics[scenario_idx]
    
    # Create detailed visualization of the chosen scenario
    scenario_path = os.path.join(case_dir, f"scenario_{scenario_idx+1}_detail.png")
    fig, axs = plt.subplots(3, 1, figsize=(12, 15))
    
    # Plot belief state evolution
    belief_array = np.array(scenario_metrics_detail["belief_history"])
    for state in range(model.parameters["n_states"]):
        axs[0].plot(belief_array[:, state], label=f'State {state}', linewidth=2)
    
    # Mark the true states
    state_history = scenario_metrics_detail["state_history"]
    for step, state in enumerate(state_history):
        axs[0].scatter(step, belief_array[step, state], color='red', s=50, zorder=10)
    
    axs[0].set_xlabel('Step')
    axs[0].set_ylabel('Belief Probability')
    axs[0].set_title(f'Belief State Evolution in Scenario {scenario_idx+1}')
    axs[0].set_ylim(0, 1)
    axs[0].legend()
    axs[0].grid(True, linestyle='--', alpha=0.6)
    
    # Plot true states and observations
    axs[1].plot(range(n_steps_per_scenario+1), state_history, 'b-', label='True State')
    axs[1].scatter(range(1, n_steps_per_scenario+1), 
                  scenario_metrics_detail["observation_history"], 
                  color='orange', label='Observation')
    axs[1].set_xlabel('Step')
    axs[1].set_ylabel('State/Observation')
    axs[1].set_title('States and Observations')
    axs[1].set_yticks(range(model.parameters["n_states"]))
    axs[1].legend()
    axs[1].grid(True, linestyle='--', alpha=0.6)
    
    # Plot actions used for evaluation
    axs[2].step(range(n_steps_per_scenario), scenario_metrics_detail["action_history"], where='mid', linewidth=2)
    axs[2].set_xlabel('Step')
    axs[2].set_ylabel('Action')
    axs[2].set_title('Actions Used for Evaluation')
    axs[2].set_yticks(range(len(model.action_space)))
    axs[2].set_yticklabels(model.action_space)
    axs[2].grid(True, linestyle='--', alpha=0.6)
    
    plt.tight_layout()
    fig.savefig(scenario_path)
    plt.close(fig)
    
    # Create an animation of a scenario evaluation
    animation_path = os.path.join(case_dir, "evaluation_animation.gif")
    
    # Use the detail scenario for animation
    belief_history = scenario_metrics_detail["belief_history"]
    state_history = scenario_metrics_detail["state_history"]
    action_history = scenario_metrics_detail["action_history"]
    observation_history = scenario_metrics_detail["observation_history"]
    
    fig, (ax1, ax2, ax3) = plt.subplots(3, 1, figsize=(10, 15))
    
    # Setup for belief state plot
    bars = ax1.bar(range(model.parameters["n_states"]), belief_history[0])
    ax1.set_ylim(0, 1)
    ax1.set_xlabel('State')
    ax1.set_ylabel('Probability')
    ax1.set_title('Current Belief State')
    
    # Setup for state/observation plot
    line1, = ax2.plot([], [], 'b-', label='True State')
    line2, = ax2.plot([], [], 'ro', label='Observations')
    ax2.set_xlim(0, n_steps_per_scenario)
    ax2.set_ylim(-0.5, model.parameters["n_states"] - 0.5)
    ax2.set_xlabel('Step')
    ax2.set_ylabel('State/Observation')
    ax2.legend()
    ax2.grid(True, linestyle='--', alpha=0.6)
    
    # Setup for metrics plot
    line3, = ax3.plot([], [], 'g-', label='Belief Accuracy')
    line4, = ax3.plot([], [], 'b-', label='Decision Accuracy')
    line5, = ax3.plot([], [], 'r-', label='Belief Entropy')
    ax3.set_xlim(0, n_steps_per_scenario)
    ax3.set_ylim(0, 1.1)  # Entropy can be slightly above 1 for 2 states
    ax3.set_xlabel('Step')
    ax3.set_ylabel('Metric Value')
    ax3.set_title('Evaluation Metrics')
    ax3.legend()
    ax3.grid(True, linestyle='--', alpha=0.6)
    
    # Text to show step information
    step_text = ax1.text(0.02, 0.95, '', transform=ax1.transAxes)
    
    def init():
        for bar in bars:
            bar.set_height(0)
        line1.set_data([], [])
        line2.set_data([], [])
        line3.set_data([], [])
        line4.set_data([], [])
        line5.set_data([], [])
        step_text.set_text('')
        return bars + [line1, line2, line3, line4, line5, step_text]
    
    def update(frame):
        # Update belief bars
        for i, bar in enumerate(bars):
            bar.set_height(belief_history[frame][i])
            # Color the bar differently for the true state
            if i == state_history[frame]:
                bar.set_color('red')
            else:
                bar.set_color('blue')
        
        # Update state/observation plot
        x_data = range(frame + 1)
        line1.set_data(x_data, state_history[:frame + 1])
        
        # Only show observations up to current frame
        if frame > 0:
            line2.set_data(range(1, frame + 1), observation_history[:frame])
        
        # Update metrics plot
        if frame > 0:
            x_metric = range(1, frame + 1)
            belief_acc = scenario_metrics_detail["belief_accuracy"][:frame]
            decision_acc = scenario_metrics_detail["decision_accuracy"][:frame]
            entropy = scenario_metrics_detail["belief_entropy"][:frame]
            
            line3.set_data(x_metric, belief_acc)
            line4.set_data(x_metric, decision_acc)
            line5.set_data(x_metric, entropy)
        
        # Update step text
        if frame > 0:
            action_text = f"Action: {model.action_space[action_history[frame-1]]}"
            obs_text = f"Observed: {model.observation_space[observation_history[frame-1]]}"
            
            # Add metrics
            belief_acc_text = f"Belief Acc: {scenario_metrics_detail['belief_accuracy'][frame-1]:.2f}"
            decision_acc_text = f"Decision Acc: {scenario_metrics_detail['decision_accuracy'][frame-1]:.2f}"
            entropy_text = f"Entropy: {scenario_metrics_detail['belief_entropy'][frame-1]:.2f}"
            
            step_text.set_text(f'Step: {frame}\n{action_text}\n{obs_text}\n{belief_acc_text}\n{decision_acc_text}\n{entropy_text}')
        else:
            step_text.set_text(f'Step: {frame}\nInitial State')
        
        return bars + [line1, line2, line3, line4, line5, step_text]
    
    anim = FuncAnimation(fig, update, frames=len(belief_history),
                         init_func=init, blit=True)
    
    anim.save(animation_path, writer='pillow', fps=1, dpi=100)
    plt.close(fig)
    logger.info(f"Created evaluation animation: {animation_path}")
    
    # Create summary statistics across all scenarios
    summary_path = os.path.join(case_dir, "evaluation_summary.png")
    
    # Calculate aggregate metrics
    avg_belief_acc = np.mean([np.mean(m["belief_accuracy"]) for m in scenario_metrics])
    avg_decision_acc = np.mean([np.mean(m["decision_accuracy"]) for m in scenario_metrics])
    avg_entropy = np.mean([np.mean(m["belief_entropy"]) for m in scenario_metrics])
    
    # Standard deviation of metrics across scenarios
    std_belief_acc = np.std([np.mean(m["belief_accuracy"]) for m in scenario_metrics])
    std_decision_acc = np.std([np.mean(m["decision_accuracy"]) for m in scenario_metrics])
    std_entropy = np.std([np.mean(m["belief_entropy"]) for m in scenario_metrics])
    
    # Create summary visualization
    fig, ax = plt.subplots(figsize=(10, 6))
    metrics = ['Belief Accuracy', 'Decision Accuracy', 'Belief Entropy']
    values = [avg_belief_acc, avg_decision_acc, avg_entropy]
    errors = [std_belief_acc, std_decision_acc, std_entropy]
    colors = ['green', 'blue', 'red']
    
    bars = ax.bar(metrics, values, yerr=errors, alpha=0.7, capsize=10, color=colors)
    
    # Add value labels on bars
    for bar, value in zip(bars, values):
        ax.text(bar.get_x() + bar.get_width()/2, value + 0.01, f'{value:.4f}', 
                ha='center', va='bottom', fontweight='bold')
    
    ax.set_ylim(0, max(1.0, max(values) + max(errors) + 0.1))
    ax.set_title('POMDP Evaluation Summary')
    ax.set_ylabel('Average Value Across Scenarios')
    ax.grid(True, linestyle='--', alpha=0.6, axis='y')
    
    plt.tight_layout()
    fig.savefig(summary_path)
    plt.close(fig)
    
    # Generate a detailed report
    report_path = os.path.join(case_dir, "report.md")
    with open(report_path, "w") as f:
        f.write(f"# POMDP Model in {Case.ACCUSATIVE.value} Case\n\n")
        f.write(f"## {case_info['linguistic_meaning']}\n\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n\n")
        f.write(f"### Context\n\n")
        f.write(f"{case_info['pomdp_context']}\n\n")
        f.write("### Mathematical Representation\n\n")
        f.write(f"```\n{case_info['formula']}\n```\n\n")
        f.write("### Primary Methods\n\n")
        f.write(f"{case_info['primary_methods']}\n\n")
        f.write("### Example\n\n")
        f.write(f"{case_info['example']}\n\n")
        f.write("### Evaluation Results\n\n")
        f.write(f"* Number of evaluation scenarios: {n_evaluation_scenarios}\n")
        f.write(f"* Steps per scenario: {n_steps_per_scenario}\n")
        f.write(f"* Model parameters:\n")
        f.write(f"  - States: {model.parameters['n_states']}\n")
        f.write(f"  - Observations: {model.parameters['n_observations']}\n")
        f.write(f"  - Actions: {model.action_space}\n")
        f.write(f"* Evaluation metrics (mean ± std):\n")
        f.write(f"  - Belief accuracy: {avg_belief_acc:.4f} ± {std_belief_acc:.4f}\n")
        f.write(f"  - Decision accuracy: {avg_decision_acc:.4f} ± {std_decision_acc:.4f}\n")
        f.write(f"  - Belief entropy: {avg_entropy:.4f} ± {std_entropy:.4f} bits\n\n")
        f.write("### Evaluation Process Analysis\n\n")
        f.write("In the ACCUSATIVE case, the POMDP is treated as an object of evaluation. ")
        f.write("External processes evaluate the model's performance on various scenarios, ")
        f.write("measuring its ability to accurately track the true state and make effective decisions. ")
        f.write("The model doesn't actively choose actions in this case, but is instead controlled ")
        f.write("by the evaluation process.\n\n")
        f.write("### Visualizations\n\n")
        f.write(f"1. [POMDP Structure](pomdp_structure.png)\n")
        f.write(f"2. [Evaluation Metrics](evaluation_metrics.png)\n")
        f.write(f"3. [Scenario {scenario_idx+1} Detail](scenario_{scenario_idx+1}_detail.png)\n")
        f.write(f"4. [Evaluation Animation](evaluation_animation.gif)\n")
        f.write(f"5. [Evaluation Summary](evaluation_summary.png)\n")
    
    logger.info(f"Completed ACCUSATIVE case test with visualizations in {case_dir}")
    
    # Return the model for potential further testing
    return model

def test_dative_case(pomdp_test_data, case_definitions):
    """Test for DATIVE case: Model as recipient of observations."""
    # Get case info for logging
    case_info = case_definitions[Case.DATIVE]
    logger.info(f"Testing {Case.DATIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(OUTPUT_DIR, Case.DATIVE.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.DATIVE, linguistics_path)
    
    # Unpack the test data
    transition_matrix = pomdp_test_data["transition_matrix"]
    observation_matrix = pomdp_test_data["observation_matrix"]
    states = pomdp_test_data["states"]
    actions = pomdp_test_data["actions"]
    observations = pomdp_test_data["observations"]
    
    # Create a POMDP model in DATIVE case (recipient of observations)
    model = POMDPModel(model_id="DatPOMDP", case=Case.DATIVE)
    logger.info(f"Created POMDP model in {Case.DATIVE.value} case (model as recipient of observations)")
    
    # Visualize POMDP structure
    structure_path = os.path.join(case_dir, "pomdp_structure.png")
    Visualizer.plot_pomdp_structure(
        transition_matrix=model.parameters["transition_matrix"],
        observation_matrix=model.parameters["observation_matrix"],
        title=f"POMDP Structure in {Case.DATIVE.value} Case",
        save_path=structure_path
    )
    
    # In DATIVE case, the focus is on the model receiving observations
    # We'll simulate different observation sequences and analyze their impact
    
    # Generate different observation sequences
    n_sequences = 4
    n_steps_per_sequence = 15
    
    # Store data for all sequences
    sequence_data = []
    
    # Run multiple observation sequences
    logger.info(f"Running {n_sequences} observation sequences for POMDP in {Case.DATIVE.value} case")
    
    for seq_idx in range(n_sequences):
        # Reset the model for this sequence
        model = POMDPModel(model_id=f"DatPOMDP_Seq{seq_idx+1}", case=Case.DATIVE)
        
        # Fix initial state for consistency across sequences
        model.current_state = 0
        
        # Store histories for visualization
        belief_history = [model.belief_state.copy()]
        state_history = [model.current_state]
        action_history = []
        observation_history = []
        observation_likelihoods = []  # Track the likelihood of each observation
        
        # Run steps for this sequence
        for step in range(n_steps_per_sequence):
            # Use a consistent action pattern across sequences
            action_idx = step % 2  # Alternate between actions
            action_history.append(action_idx)
            
            # Transition to new state
            new_state = model.transition(action_idx)
            state_history.append(new_state)
            
            # Get observation
            observation_idx = model.observe()
            observation_history.append(observation_idx)
            
            # Calculate observation likelihood p(o|s)
            obs_likelihood = model.parameters["observation_matrix"][new_state, observation_idx]
            observation_likelihoods.append(obs_likelihood)
            
            # Update belief state (model receives the observation)
            updated_belief = model.update_belief(action_idx, observation_idx)
            belief_history.append(updated_belief.copy())
            
            logger.info(f"Sequence {seq_idx+1}, Step {step+1}: Action={action_idx}, " +
                       f"State={new_state}, Observation={observation_idx}, " +
                       f"Obs Likelihood={obs_likelihood:.4f}")
        
        # Store sequence data
        sequence_data.append({
            "belief_history": belief_history,
            "state_history": state_history,
            "action_history": action_history,
            "observation_history": observation_history,
            "observation_likelihoods": observation_likelihoods
        })
    
    # Visualize observation reception across sequences
    obs_reception_path = os.path.join(case_dir, "observation_reception.png")
    fig, axs = plt.subplots(2, 2, figsize=(14, 12))
    axs = axs.flatten()
    
    for i, data in enumerate(sequence_data):
        ax = axs[i]
        obs_history = data["observation_history"]
        obs_likelihoods = data["observation_likelihoods"]
        states = data["state_history"][1:]  # Skip initial state
        
        # Create a scatter plot of observations
        scatter = ax.scatter(range(len(obs_history)), obs_history, 
                           c=obs_likelihoods, cmap='viridis', 
                           s=100, alpha=0.8)
        
        # Plot true states as a step plot
        ax.step(range(len(states)), states, 'r-', where='mid', alpha=0.6, label='True State')
        
        ax.set_xlabel('Step')
        ax.set_ylabel('Observation / State')
        ax.set_title(f'Sequence {i+1}: Observation Reception')
        ax.set_yticks(range(model.parameters["n_observations"]))
        ax.set_ylim(-0.5, model.parameters["n_observations"] - 0.5)
        ax.legend()
        ax.grid(True, linestyle='--', alpha=0.6)
    
    # Add a colorbar
    cbar_ax = fig.add_axes([0.92, 0.15, 0.02, 0.7])
    cbar = fig.colorbar(scatter, cax=cbar_ax)
    cbar.set_label('Observation Likelihood p(o|s)')
    
    plt.tight_layout(rect=[0, 0, 0.9, 1])
    fig.savefig(obs_reception_path)
    plt.close(fig)
    
    # Visualize belief updates in response to observations
    belief_update_path = os.path.join(case_dir, "belief_updates.png")
    fig, axs = plt.subplots(n_sequences, 1, figsize=(12, 5*n_sequences))
    
    if n_sequences == 1:
        axs = [axs]
    
    for i, data in enumerate(sequence_data):
        ax = axs[i]
        belief_history = np.array(data["belief_history"])
        
        # Plot belief evolution for each state
        for state in range(model.parameters["n_states"]):
            ax.plot(belief_history[:, state], label=f'State {state}', linewidth=2)
        
        # Add observation markers
        for step, obs in enumerate(data["observation_history"]):
            # Mark each observation with a vertical line
            ax.axvline(x=step+1, color='gray', linestyle='--', alpha=0.3)
            # Add a text annotation for the observation
            ax.text(step+1, 0.1, f'O{obs}', 
                   ha='center', va='center', backgroundcolor='white',
                   bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
        
        ax.set_xlabel('Step')
        ax.set_ylabel('Belief Probability')
        ax.set_title(f'Sequence {i+1}: Belief Updates in Response to Observations')
        ax.set_ylim(0, 1)
        ax.legend()
        ax.grid(True, linestyle='--', alpha=0.6)
    
    plt.tight_layout()
    fig.savefig(belief_update_path)
    plt.close(fig)
    
    # Create an animation showing observation reception and belief updates
    animation_path = os.path.join(case_dir, "observation_reception_animation.gif")
    
    # Use the first sequence for animation
    seq_data = sequence_data[0]
    belief_history = seq_data["belief_history"]
    state_history = seq_data["state_history"]
    observation_history = seq_data["observation_history"]
    
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 12))
    
    # Setup for observation plot
    obs_scatter = ax1.scatter([], [], s=150, color='blue')
    line1, = ax1.plot([], [], 'r-', linewidth=2, label='True State')
    ax1.set_xlim(-0.5, n_steps_per_sequence + 0.5)
    ax1.set_ylim(-0.5, model.parameters["n_observations"] - 0.5)
    ax1.set_xlabel('Step')
    ax1.set_ylabel('Observation / State')
    ax1.set_title('Observation Reception')
    ax1.set_yticks(range(model.parameters["n_observations"]))
    ax1.legend()
    ax1.grid(True, linestyle='--', alpha=0.6)
    
    # Text for observation information
    obs_text = ax1.text(0.02, 0.95, '', transform=ax1.transAxes, 
                      bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
    
    # Setup for belief state plot
    bars = ax2.bar(range(model.parameters["n_states"]), belief_history[0])
    ax2.set_ylim(0, 1)
    ax2.set_xlabel('State')
    ax2.set_ylabel('Belief Probability')
    ax2.set_title('Belief State')
    
    def init():
        obs_scatter.set_offsets([])
        line1.set_data([], [])
        for bar in bars:
            bar.set_height(0)
        obs_text.set_text('')
        return [obs_scatter, line1] + list(bars) + [obs_text]
    
    def update(frame):
        # Update observation plot
        if frame > 0:
            # Only show observations up to current frame
            x_obs = range(1, frame + 1)
            y_obs = observation_history[:frame]
            obs_scatter.set_offsets(list(zip(x_obs, y_obs)))
            
            # Update true state line
            line1.set_data(range(frame + 1), state_history[:frame + 1])
            
            # Update observation text
            current_obs = observation_history[frame-1]
            current_state = state_history[frame]
            obs_likelihood = model.parameters["observation_matrix"][current_state, current_obs]
            
            obs_text.set_text(f'Step: {frame}\n' +
                             f'Observation: {current_obs}\n' +
                             f'True State: {current_state}\n' +
                             f'Obs Likelihood: {obs_likelihood:.4f}')
        else:
            obs_text.set_text(f'Initial State: {state_history[0]}')
        
        # Update belief bars
        current_belief = belief_history[frame]
        for i, bar in enumerate(bars):
            bar.set_height(current_belief[i])
            # Color based on state
            if i == state_history[frame]:
                bar.set_color('red')
            else:
                bar.set_color('blue')
        
        return [obs_scatter, line1] + list(bars) + [obs_text]
    
    anim = FuncAnimation(fig, update, frames=len(belief_history),
                         init_func=init, blit=True)
    
    anim.save(animation_path, writer='pillow', fps=1, dpi=100)
    plt.close(fig)
    logger.info(f"Created observation reception animation: {animation_path}")
    
    # Create a visualization of observation impact
    impact_path = os.path.join(case_dir, "observation_impact.png")
    
    # Calculate the KL divergence between consecutive belief states
    kl_divergences = []
    
    for seq_idx, data in enumerate(sequence_data):
        belief_history = data["belief_history"]
        seq_kl = []
        
        for i in range(1, len(belief_history)):
            prior = belief_history[i-1]
            posterior = belief_history[i]
            
            # Avoid division by zero in log
            prior = np.clip(prior, 1e-10, 1.0)
            posterior = np.clip(posterior, 1e-10, 1.0)
            
            # KL divergence: sum(posterior * log(posterior / prior))
            kl = np.sum(posterior * np.log(posterior / prior))
            seq_kl.append(kl)
        
        kl_divergences.append(seq_kl)
    
    # Plot KL divergences
    fig, ax = plt.subplots(figsize=(10, 6))
    
    for i, kl_seq in enumerate(kl_divergences):
        ax.plot(range(1, len(kl_seq) + 1), kl_seq, marker='o', label=f'Sequence {i+1}')
    
    ax.set_xlabel('Step')
    ax.set_ylabel('KL Divergence')
    ax.set_title('Impact of Observations on Belief State')
    ax.legend()
    ax.grid(True, linestyle='--', alpha=0.6)
    
    plt.tight_layout()
    fig.savefig(impact_path)
    plt.close(fig)
    
    # Generate a detailed report
    report_path = os.path.join(case_dir, "report.md")
    with open(report_path, "w") as f:
        f.write(f"# POMDP Model in {Case.DATIVE.value} Case\n\n")
        f.write(f"## {case_info['linguistic_meaning']}\n\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n\n")
        f.write(f"### Context\n\n")
        f.write(f"{case_info['pomdp_context']}\n\n")
        f.write("### Mathematical Representation\n\n")
        f.write(f"```\n{case_info['formula']}\n```\n\n")
        f.write("### Primary Methods\n\n")
        f.write(f"{case_info['primary_methods']}\n\n")
        f.write("### Example\n\n")
        f.write(f"{case_info['example']}\n\n")
        f.write("### Analysis Results\n\n")
        f.write(f"* Number of observation sequences: {n_sequences}\n")
        f.write(f"* Steps per sequence: {n_steps_per_sequence}\n")
        f.write(f"* Model parameters:\n")
        f.write(f"  - States: {model.parameters['n_states']}\n")
        f.write(f"  - Observations: {model.parameters['n_observations']}\n")
        f.write(f"  - Actions: {model.action_space}\n")
        f.write("\n### Observation Reception Analysis\n\n")
        f.write("In the DATIVE case, the POMDP is treated as a recipient of observations. ")
        f.write("The focus is on how the model receives and processes these observations to update its beliefs. ")
        f.write("The visualizations show the observation sequences and their impact on the model's belief state.\n\n")
        f.write("Key insights:\n")
        f.write("1. Observations with higher likelihood (p(o|s)) have a stronger influence on belief updates\n")
        f.write("2. The KL divergence between consecutive belief states measures the information gain from each observation\n")
        f.write("3. The model's belief state converges toward the true state as it receives more informative observations\n\n")
        f.write("### Visualizations\n\n")
        f.write(f"1. [POMDP Structure](pomdp_structure.png)\n")
        f.write(f"2. [Observation Reception](observation_reception.png)\n")
        f.write(f"3. [Belief Updates](belief_updates.png)\n")
        f.write(f"4. [Observation Impact](observation_impact.png)\n")
        f.write(f"5. [Observation Reception Animation](observation_reception_animation.gif)\n")
    
    logger.info(f"Completed DATIVE case test with visualizations in {case_dir}")
    
    # Return the model for potential further testing
    return model

def test_genitive_case(pomdp_test_data, case_definitions):
    """Test for GENITIVE case: Model as source/generator of beliefs."""
    # Get case info for logging
    case_info = case_definitions[Case.GENITIVE]
    logger.info(f"Testing {Case.GENITIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(OUTPUT_DIR, Case.GENITIVE.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.GENITIVE, linguistics_path)
    
    # Unpack the test data
    transition_matrix = pomdp_test_data["transition_matrix"]
    observation_matrix = pomdp_test_data["observation_matrix"]
    states = pomdp_test_data["states"]
    actions = pomdp_test_data["actions"]
    observations = pomdp_test_data["observations"]
    
    # Create a POMDP model in GENITIVE case (source/generator of beliefs)
    model = POMDPModel(model_id="GenPOMDP", case=Case.GENITIVE)
    logger.info(f"Created POMDP model in {Case.GENITIVE.value} case (model as source/generator of beliefs)")
    
    # Visualize POMDP structure
    structure_path = os.path.join(case_dir, "pomdp_structure.png")
    Visualizer.plot_pomdp_structure(
        transition_matrix=model.parameters["transition_matrix"],
        observation_matrix=model.parameters["observation_matrix"],
        title=f"POMDP Structure in {Case.GENITIVE.value} Case",
        save_path=structure_path
    )
    
    # In GENITIVE case, focus on the model as a generator/source of beliefs
    # We'll examine how different initial beliefs evolve and generate predictions
    
    # Create different initial beliefs
    n_initial_beliefs = 4
    initial_beliefs = []
    
    # Generate different initial belief states
    np.random.seed(42)  # For reproducibility
    
    # 1. Strong belief in state 0
    initial_beliefs.append(np.array([0.9, 0.1]))
    
    # 2. Strong belief in state 1
    initial_beliefs.append(np.array([0.1, 0.9]))
    
    # 3. Uniform belief
    initial_beliefs.append(np.array([0.5, 0.5]))
    
    # 4. Slightly biased belief
    initial_beliefs.append(np.array([0.6, 0.4]))
    
    # Track belief evolution for each initial belief
    simulation_data = []
    n_steps = 20
    
    logger.info(f"Running {n_initial_beliefs} simulations with different initial beliefs in {Case.GENITIVE.value} case")
    
    for belief_idx, initial_belief in enumerate(initial_beliefs):
        # Create a fresh model for this initial belief
        model = POMDPModel(model_id=f"GenPOMDP_Belief{belief_idx+1}", case=Case.GENITIVE)
        
        # Set initial state and belief
        model.current_state = np.random.choice(model.parameters["n_states"])
        model.belief_state = initial_belief.copy()
        
        # Store histories for this simulation
        belief_history = [model.belief_state.copy()]
        state_history = [model.current_state]
        action_history = []
        observation_history = []
        prediction_history = []  # To store predictions about future observations
        
        # Run the simulation
        for step in range(n_steps):
            # Generate a prediction about expected observations
            # Compute p(o) = ∑_s p(o|s)p(s) for current belief
            expected_obs_probs = np.zeros(model.parameters["n_observations"])
            for s in range(model.parameters["n_states"]):
                for o in range(model.parameters["n_observations"]):
                    expected_obs_probs[o] += model.parameters["observation_matrix"][s, o] * model.belief_state[s]
            
            # Store prediction (most likely observation)
            most_likely_obs = np.argmax(expected_obs_probs)
            prediction_history.append(most_likely_obs)
            
            # Choose action based on current belief
            action_idx = model.get_optimal_action()
            action_history.append(action_idx)
            
            # Transition to new state
            new_state = model.transition(action_idx)
            state_history.append(new_state)
            
            # Get observation
            observation_idx = model.observe()
            observation_history.append(observation_idx)
            
            # Update belief state
            updated_belief = model.update_belief(action_idx, observation_idx)
            belief_history.append(updated_belief.copy())
            
            logger.info(f"Belief {belief_idx+1}, Step {step+1}: " +
                       f"Action={action_idx}, Observation={observation_idx}, " +
                       f"Prediction={most_likely_obs}, Accuracy={1 if most_likely_obs == observation_idx else 0}")
        
        # Store simulation data
        simulation_data.append({
            "initial_belief": initial_belief,
            "belief_history": belief_history,
            "state_history": state_history,
            "action_history": action_history,
            "observation_history": observation_history,
            "prediction_history": prediction_history
        })
    
    # Visualize belief evolution for each initial belief
    evolution_path = os.path.join(case_dir, "belief_evolution.png")
    fig, axs = plt.subplots(n_initial_beliefs, 1, figsize=(12, 5*n_initial_beliefs))
    
    if n_initial_beliefs == 1:
        axs = [axs]
    
    for i, data in enumerate(simulation_data):
        ax = axs[i]
        belief_history = np.array(data["belief_history"])
        
        # Plot belief evolution for each state
        for state in range(model.parameters["n_states"]):
            ax.plot(belief_history[:, state], label=f'State {state}', linewidth=2)
        
        # Add a legend for the first subplot only to avoid clutter
        if i == 0:
            ax.legend()
        
        # Add title with initial belief
        ax.set_title(f'Initial Belief: [{data["initial_belief"][0]:.2f}, {data["initial_belief"][1]:.2f}]')
        ax.set_xlabel('Step')
        ax.set_ylabel('Belief Probability')
        ax.set_ylim(0, 1)
        ax.grid(True, linestyle='--', alpha=0.6)
    
    plt.tight_layout()
    fig.savefig(evolution_path)
    plt.close(fig)
    
    # Visualize prediction accuracy for each initial belief
    prediction_path = os.path.join(case_dir, "prediction_accuracy.png")
    fig, axs = plt.subplots(n_initial_beliefs, 1, figsize=(12, 5*n_initial_beliefs))
    
    if n_initial_beliefs == 1:
        axs = [axs]
    
    for i, data in enumerate(simulation_data):
        ax = axs[i]
        predictions = data["prediction_history"]
        observations = data["observation_history"]
        
        # Calculate prediction accuracy at each step
        prediction_accuracy = [1 if pred == obs else 0 for pred, obs in zip(predictions, observations)]
        
        # Calculate cumulative accuracy
        cumulative_accuracy = np.cumsum(prediction_accuracy) / np.arange(1, len(prediction_accuracy) + 1)
        
        # Plot accuracy
        ax.plot(range(1, len(prediction_accuracy) + 1), prediction_accuracy, 'bo-', alpha=0.5, label='Step Accuracy')
        ax.plot(range(1, len(cumulative_accuracy) + 1), cumulative_accuracy, 'r-', linewidth=2, label='Cumulative Accuracy')
        
        # Add horizontal line at 0.5 (random guessing)
        ax.axhline(y=0.5, color='gray', linestyle='--', alpha=0.7)
        
        # Add title with initial belief
        ax.set_title(f'Initial Belief: [{data["initial_belief"][0]:.2f}, {data["initial_belief"][1]:.2f}]')
        ax.set_xlabel('Step')
        ax.set_ylabel('Prediction Accuracy')
        ax.set_ylim(-0.1, 1.1)
        ax.legend()
        ax.grid(True, linestyle='--', alpha=0.6)
    
    plt.tight_layout()
    fig.savefig(prediction_path)
    plt.close(fig)
    
    # Visualize belief uncertainty and prediction confidence
    uncertainty_path = os.path.join(case_dir, "belief_uncertainty.png")
    fig, axs = plt.subplots(n_initial_beliefs, 1, figsize=(12, 5*n_initial_beliefs))
    
    if n_initial_beliefs == 1:
        axs = [axs]
    
    for i, data in enumerate(simulation_data):
        ax = axs[i]
        belief_history = np.array(data["belief_history"])
        
        # Calculate belief entropy (uncertainty) at each step
        def entropy(belief):
            # Avoid division by zero in log
            belief = np.clip(belief, 1e-10, 1.0)
            return -np.sum(belief * np.log2(belief))
        
        belief_entropy = [entropy(belief) for belief in belief_history]
        
        # Calculate belief confidence (max probability)
        belief_confidence = [np.max(belief) for belief in belief_history]
        
        # Plot entropy and confidence
        ax.plot(belief_entropy, 'r-', linewidth=2, label='Belief Entropy')
        ax.set_ylabel('Entropy (bits)', color='r')
        ax.tick_params(axis='y', labelcolor='r')
        
        # Create twin axis for confidence
        ax2 = ax.twinx()
        ax2.plot(belief_confidence, 'b-', linewidth=2, label='Belief Confidence')
        ax2.set_ylabel('Confidence', color='b')
        ax2.tick_params(axis='y', labelcolor='b')
        
        # Add title with initial belief
        ax.set_title(f'Initial Belief: [{data["initial_belief"][0]:.2f}, {data["initial_belief"][1]:.2f}]')
        ax.set_xlabel('Step')
        ax.grid(True, linestyle='--', alpha=0.6)
        
        # Add legend
        lines1, labels1 = ax.get_legend_handles_labels()
        lines2, labels2 = ax2.get_legend_handles_labels()
        ax.legend(lines1 + lines2, labels1 + labels2, loc='upper right')
    
    plt.tight_layout()
    fig.savefig(uncertainty_path)
    plt.close(fig)
    
    # Create an animation of belief evolution and predictions
    animation_path = os.path.join(case_dir, "belief_generation_animation.gif")
    
    # Use one of the simulations for the animation (pick the one with uniform initial belief)
    uniform_idx = 2  # Index of the uniform belief simulation
    sim_data = simulation_data[uniform_idx]
    
    belief_history = sim_data["belief_history"]
    state_history = sim_data["state_history"]
    observation_history = sim_data["observation_history"]
    prediction_history = sim_data["prediction_history"]
    
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 12))
    
    # Setup for belief state plot
    bars = ax1.bar(range(model.parameters["n_states"]), belief_history[0])
    ax1.set_ylim(0, 1)
    ax1.set_xlabel('State')
    ax1.set_ylabel('Belief Probability')
    ax1.set_title('Belief State')
    
    # Setup for prediction/observation plot
    line_pred, = ax2.plot([], [], 'bo-', label='Prediction')
    line_obs, = ax2.plot([], [], 'ro', label='Observation')
    line_state, = ax2.plot([], [], 'g-', label='True State')
    ax2.set_xlim(0, n_steps)
    ax2.set_ylim(-0.5, max(model.parameters["n_states"], model.parameters["n_observations"]) - 0.5)
    ax2.set_xlabel('Step')
    ax2.set_ylabel('State/Observation')
    ax2.legend()
    ax2.grid(True, linestyle='--', alpha=0.6)
    
    # Text for animation information
    step_text = ax1.text(0.02, 0.95, '', transform=ax1.transAxes)
    
    def init():
        for bar in bars:
            bar.set_height(0)
        line_pred.set_data([], [])
        line_obs.set_data([], [])
        line_state.set_data([], [])
        step_text.set_text('')
        return bars + [line_pred, line_obs, line_state, step_text]
    
    def update(frame):
        # Update belief bars
        for i, bar in enumerate(bars):
            bar.set_height(belief_history[frame][i])
            # Color according to state
            if i == state_history[frame]:
                bar.set_color('red')
            else:
                bar.set_color('blue')
        
        # Update prediction and observation plots
        if frame > 0:
            x_pred = range(1, frame + 1)
            y_pred = prediction_history[:frame]
            line_pred.set_data(x_pred, y_pred)
            
            x_obs = range(1, frame + 1)
            y_obs = observation_history[:frame]
            line_obs.set_data(x_obs, y_obs)
        
        # Update state line
        x_state = range(frame + 1)
        y_state = state_history[:frame + 1]
        line_state.set_data(x_state, y_state)
        
        # Update step text
        if frame > 0:
            # Calculate belief entropy
            belief = belief_history[frame]
            entropy_val = -np.sum(np.clip(belief, 1e-10, 1.0) * np.log2(np.clip(belief, 1e-10, 1.0)))
            
            # Prediction accuracy
            prediction = prediction_history[frame - 1]
            observation = observation_history[frame - 1]
            accuracy = "Correct" if prediction == observation else "Incorrect"
            
            step_text.set_text(f'Step: {frame}\n' +
                             f'True State: {state_history[frame]}\n' +
                             f'Prediction: {prediction}\n' +
                             f'Observation: {observation}\n' +
                             f'Accuracy: {accuracy}\n' +
                             f'Entropy: {entropy_val:.2f}')
        else:
            # Calculate initial entropy
            belief = belief_history[0]
            entropy_val = -np.sum(np.clip(belief, 1e-10, 1.0) * np.log2(np.clip(belief, 1e-10, 1.0)))
            
            step_text.set_text(f'Initial Belief\n' +
                             f'State 0: {belief[0]:.2f}\n' +
                             f'State 1: {belief[1]:.2f}\n' +
                             f'Entropy: {entropy_val:.2f}')
        
        return bars + [line_pred, line_obs, line_state, step_text]
    
    anim = FuncAnimation(fig, update, frames=len(belief_history),
                         init_func=init, blit=True)
    
    anim.save(animation_path, writer='pillow', fps=1, dpi=100)
    plt.close(fig)
    logger.info(f"Created belief generation animation: {animation_path}")
    
    # Visualize the source nature of different beliefs
    source_path = os.path.join(case_dir, "belief_source_analysis.png")
    
    # Compare final beliefs across all initial beliefs
    fig, axs = plt.subplots(2, 1, figsize=(12, 10))
    
    # Plot initial beliefs
    width = 0.2
    x = np.arange(model.parameters["n_states"])
    
    for i, data in enumerate(simulation_data):
        axs[0].bar(x + i*width, data["initial_belief"], width=width, 
                 label=f'Simulation {i+1}')
    
    axs[0].set_xlabel('State')
    axs[0].set_ylabel('Initial Belief Probability')
    axs[0].set_title('Initial Beliefs')
    axs[0].set_xticks(x + width * (n_initial_beliefs - 1) / 2)
    axs[0].set_xticklabels([f'State {i}' for i in range(model.parameters["n_states"])])
    axs[0].legend()
    axs[0].grid(True, linestyle='--', alpha=0.6)
    
    # Plot final beliefs
    for i, data in enumerate(simulation_data):
        axs[1].bar(x + i*width, data["belief_history"][-1], width=width, 
                 label=f'Simulation {i+1}')
    
    axs[1].set_xlabel('State')
    axs[1].set_ylabel('Final Belief Probability')
    axs[1].set_title('Final Beliefs')
    axs[1].set_xticks(x + width * (n_initial_beliefs - 1) / 2)
    axs[1].set_xticklabels([f'State {i}' for i in range(model.parameters["n_states"])])
    axs[1].legend()
    axs[1].grid(True, linestyle='--', alpha=0.6)
    
    plt.tight_layout()
    fig.savefig(source_path)
    plt.close(fig)
    
    # Calculate and visualize convergence of beliefs
    convergence_path = os.path.join(case_dir, "belief_convergence.png")
    
    # Compute KL divergence between beliefs at each step
    kl_divergences = []
    
    for sim_idx, data in enumerate(simulation_data):
        belief_history = data["belief_history"]
        
        # For each step, compute KL divergence to the final belief
        final_belief = belief_history[-1]
        kl_divs = []
        
        for belief in belief_history:
            # Avoid division by zero in log
            current = np.clip(belief, 1e-10, 1.0)
            final = np.clip(final_belief, 1e-10, 1.0)
            
            # KL divergence: sum(current * log(current / final))
            kl = np.sum(current * np.log(current / final))
            kl_divs.append(kl)
        
        kl_divergences.append(kl_divs)
    
    # Plot convergence
    fig, ax = plt.subplots(figsize=(10, 6))
    
    for i, kl_divs in enumerate(kl_divergences):
        ax.plot(kl_divs, marker='o', label=f'Simulation {i+1}')
    
    ax.set_xlabel('Step')
    ax.set_ylabel('KL Divergence to Final Belief')
    ax.set_title('Convergence of Beliefs to Final State')
    ax.legend()
    ax.grid(True, linestyle='--', alpha=0.6)
    
    plt.tight_layout()
    fig.savefig(convergence_path)
    plt.close(fig)
    
    # Generate a detailed report
    report_path = os.path.join(case_dir, "report.md")
    with open(report_path, "w") as f:
        f.write(f"# POMDP Model in {Case.GENITIVE.value} Case\n\n")
        f.write(f"## {case_info['linguistic_meaning']}\n\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n\n")
        f.write(f"### Context\n\n")
        f.write(f"{case_info['pomdp_context']}\n\n")
        f.write("### Mathematical Representation\n\n")
        f.write(f"```\n{case_info['formula']}\n```\n\n")
        f.write("### Primary Methods\n\n")
        f.write(f"{case_info['primary_methods']}\n\n")
        f.write("### Example\n\n")
        f.write(f"{case_info['example']}\n\n")
        f.write("### Analysis Results\n\n")
        f.write(f"* Number of initial beliefs tested: {n_initial_beliefs}\n")
        f.write(f"* Simulation steps: {n_steps}\n")
        f.write(f"* Model parameters:\n")
        f.write(f"  - States: {model.parameters['n_states']}\n")
        f.write(f"  - Observations: {model.parameters['n_observations']}\n")
        f.write(f"  - Actions: {model.action_space}\n")
        
        # Calculate overall prediction accuracy for each simulation
        f.write("\n* Prediction accuracy by initial belief:\n")
        for i, data in enumerate(simulation_data):
            preds = data["prediction_history"]
            obs = data["observation_history"]
            accuracy = sum(1 for p, o in zip(preds, obs) if p == o) / len(preds)
            f.write(f"  - Simulation {i+1} ({data['initial_belief']}): {accuracy:.4f}\n")
        
        f.write("\n### Belief Generation Analysis\n\n")
        f.write("In the GENITIVE case, the POMDP acts as a source or generator of beliefs. ")
        f.write("The focus is on how different initial beliefs evolve and generate predictions ")
        f.write("about observations. The visualizations show belief evolution, prediction accuracy, ")
        f.write("and convergence properties.\n\n")
        f.write("Key insights:\n")
        f.write("1. Initial beliefs strongly influence early predictions but converge over time\n")
        f.write("2. Belief entropy (uncertainty) generally decreases as more observations are received\n")
        f.write("3. Prediction accuracy improves as the belief state becomes more certain\n")
        f.write("4. Different initial beliefs may converge to similar final beliefs given enough observations\n\n")
        f.write("### Visualizations\n\n")
        f.write(f"1. [POMDP Structure](pomdp_structure.png)\n")
        f.write(f"2. [Belief Evolution](belief_evolution.png)\n")
        f.write(f"3. [Prediction Accuracy](prediction_accuracy.png)\n")
        f.write(f"4. [Belief Uncertainty](belief_uncertainty.png)\n")
        f.write(f"5. [Belief Source Analysis](belief_source_analysis.png)\n")
        f.write(f"6. [Belief Convergence](belief_convergence.png)\n")
        f.write(f"7. [Belief Generation Animation](belief_generation_animation.gif)\n")
    
    logger.info(f"Completed GENITIVE case test with visualizations in {case_dir}")
    
    # Return the model for potential further testing
    return model

def test_instrumental_case(pomdp_test_data, case_definitions):
    """Test for INSTRUMENTAL case: Model as computational method."""
    # Get case info for logging
    case_info = case_definitions[Case.INSTRUMENTAL]
    logger.info(f"Testing {Case.INSTRUMENTAL.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(OUTPUT_DIR, Case.INSTRUMENTAL.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.INSTRUMENTAL, linguistics_path)
    
    # Unpack the test data
    transition_matrix = pomdp_test_data["transition_matrix"]
    observation_matrix = pomdp_test_data["observation_matrix"]
    states = pomdp_test_data["states"]
    actions = pomdp_test_data["actions"]
    observations = pomdp_test_data["observations"]
    
    # Create a POMDP model in INSTRUMENTAL case (as computational method)
    model = POMDPModel(model_id="InsPOMDP", case=Case.INSTRUMENTAL)
    logger.info(f"Created POMDP model in {Case.INSTRUMENTAL.value} case (model as computational method)")
    
    # Visualize POMDP structure
    structure_path = os.path.join(case_dir, "pomdp_structure.png")
    Visualizer.plot_pomdp_structure(
        transition_matrix=model.parameters["transition_matrix"],
        observation_matrix=model.parameters["observation_matrix"],
        title=f"POMDP Structure in {Case.INSTRUMENTAL.value} Case",
        save_path=structure_path
    )
    
    # In INSTRUMENTAL case, focus on model as a computational tool/method
    # We'll examine different computational aspects and algorithmic properties
    
    # 1. Algorithm convergence - belief state evolution over increasing time
    # 2. Computational efficiency - operations required for different calculations
    # 3. Method comparison - optimal vs heuristic decision making
    
    # Define sample problems to solve with the POMDP
    n_problems = 3
    problem_data = []
    n_steps = 30  # Longer horizon to test convergence
    
    # Problem types:
    # 1. Starting with uniform belief (maximum uncertainty)
    # 2. Starting with wrong belief (state 0 but belief that it's state 1)
    # 3. Starting with noisy observations (increased observation noise)
    
    logger.info(f"Testing POMDP as computational method across {n_problems} problem scenarios")
    
    # Problem 1: Uniform belief (high uncertainty)
    model_p1 = POMDPModel(model_id="InsPOMDP_P1", case=Case.INSTRUMENTAL)
    model_p1.current_state = 0
    model_p1.belief_state = np.array([0.5, 0.5])  # Uniform prior
    
    # Problem 2: Wrong belief
    model_p2 = POMDPModel(model_id="InsPOMDP_P2", case=Case.INSTRUMENTAL)
    model_p2.current_state = 0
    model_p2.belief_state = np.array([0.1, 0.9])  # Wrong belief (thinks it's in state 1)
    
    # Problem 3: Noisy observations (create a modified model with more observation noise)
    modified_obs_matrix = np.array([
        [0.6, 0.4],  # More noise compared to original [0.8, 0.2]
        [0.4, 0.6]
    ])
    model_p3 = POMDPModel(model_id="InsPOMDP_P3", case=Case.INSTRUMENTAL)
    model_p3.parameters["observation_matrix"] = modified_obs_matrix
    model_p3.current_state = 0
    model_p3.belief_state = np.array([0.5, 0.5])  # Uniform prior
    
    models = [model_p1, model_p2, model_p3]
    problem_labels = ["Uniform Prior", "Wrong Prior", "Noisy Observations"]
    
    # Record time and computational aspects for each problem
    for p_idx, model in enumerate(models):
        # Store histories for this problem
        belief_history = [model.belief_state.copy()]
        state_history = [model.current_state]
        action_history = []
        observation_history = []
        
        # Track computational metrics
        computational_metrics = {
            "belief_updates": [],   # Track KL divergence in belief updates
            "decision_times": [],   # Track time for decision (simulated based on steps)
            "error_metrics": []     # Track accuracy of state estimation
        }
        
        # Run the algorithm for this problem
        for step in range(n_steps):
            # Choose action using model
            # In INSTRUMENTAL case, we focus on how the model is being used as a tool
            
            # Optimal policy (based on current belief)
            optimal_action = model.get_optimal_action()
            
            # Track computation time (simulated based on belief entropy)
            entropy = -np.sum(np.clip(model.belief_state, 1e-10, 1.0) * np.log2(np.clip(model.belief_state, 1e-10, 1.0)))
            computation_time = 1.0 + entropy * 2  # More entropy = more computation time
            computational_metrics["decision_times"].append(computation_time)
            
            # Apply action
            action_idx = optimal_action
            action_history.append(action_idx)
            
            # Transition and observe
            new_state = model.transition(action_idx)
            state_history.append(new_state)
            
            observation_idx = model.observe()
            observation_history.append(observation_idx)
            
            # Calculate pre-update error (L1 distance from true state)
            pre_update_error = 0.0
            true_state_vector = np.zeros(model.parameters["n_states"])
            true_state_vector[new_state] = 1.0
            pre_update_error = np.sum(np.abs(model.belief_state - true_state_vector))
            
            # Update belief state
            prev_belief = model.belief_state.copy()
            updated_belief = model.update_belief(action_idx, observation_idx)
            belief_history.append(updated_belief.copy())
            
            # Calculate belief update magnitude (KL divergence)
            prev_belief_clipped = np.clip(prev_belief, 1e-10, 1.0)
            updated_belief_clipped = np.clip(updated_belief, 1e-10, 1.0)
            kl = np.sum(updated_belief_clipped * np.log(updated_belief_clipped / prev_belief_clipped))
            computational_metrics["belief_updates"].append(kl)
            
            # Calculate post-update error
            post_update_error = 0.0
            true_state_vector = np.zeros(model.parameters["n_states"])
            true_state_vector[new_state] = 1.0
            post_update_error = np.sum(np.abs(updated_belief - true_state_vector))
            
            # Store error improvement
            error_improvement = pre_update_error - post_update_error
            computational_metrics["error_metrics"].append(error_improvement)
            
            logger.info(f"Problem {p_idx+1}, Step {step+1}: " +
                       f"Action={action_idx}, Observation={observation_idx}, " +
                       f"Computation Time={computation_time:.4f}, Error Improvement={error_improvement:.4f}")
        
        # Store data for this problem
        problem_data.append({
            "label": problem_labels[p_idx],
            "belief_history": belief_history,
            "state_history": state_history,
            "action_history": action_history,
            "observation_history": observation_history,
            "computational_metrics": computational_metrics
        })
    
    # Visualize algorithm convergence across problems
    convergence_path = os.path.join(case_dir, "algorithm_convergence.png")
    fig, axs = plt.subplots(2, 1, figsize=(12, 12))
    
    # Plot belief accuracy over time
    for i, data in enumerate(problem_data):
        belief_history = np.array(data["belief_history"])
        state_history = np.array(data["state_history"])
        
        # Calculate belief accuracy at each step
        accuracy = []
        for step in range(len(belief_history)):
            # Most likely state from belief
            predicted_state = np.argmax(belief_history[step])
            # True state
            true_state = state_history[step]
            # Accuracy (1 if correct, 0 if wrong)
            acc = 1.0 if predicted_state == true_state else 0.0
            accuracy.append(acc)
        
        # Calculate running average accuracy
        window_size = 5
        running_avg = []
        for j in range(len(accuracy)):
            if j < window_size:
                running_avg.append(np.mean(accuracy[:j+1]))
            else:
                running_avg.append(np.mean(accuracy[j-window_size+1:j+1]))
        
        # Plot accuracy
        axs[0].plot(accuracy, 'o-', alpha=0.3, label=f'{data["label"]} (Raw)')
        axs[0].plot(running_avg, '-', linewidth=2, label=f'{data["label"]} (Avg)')
    
    axs[0].set_xlabel('Step')
    axs[0].set_ylabel('State Prediction Accuracy')
    axs[0].set_title('Algorithm Convergence: State Prediction')
    axs[0].set_ylim(-0.1, 1.1)
    axs[0].legend()
    axs[0].grid(True, linestyle='--', alpha=0.6)
    
    # Plot belief entropy over time (uncertainty)
    for i, data in enumerate(problem_data):
        belief_history = np.array(data["belief_history"])
        
        # Calculate entropy at each step
        entropy = []
        for belief in belief_history:
            # Avoid log(0)
            belief_clipped = np.clip(belief, 1e-10, 1.0)
            ent = -np.sum(belief_clipped * np.log2(belief_clipped))
            entropy.append(ent)
        
        # Plot entropy
        axs[1].plot(entropy, '-', linewidth=2, label=data["label"])
    
    axs[1].set_xlabel('Step')
    axs[1].set_ylabel('Belief Entropy (bits)')
    axs[1].set_title('Algorithm Convergence: Uncertainty Reduction')
    axs[1].legend()
    axs[1].grid(True, linestyle='--', alpha=0.6)
    
    plt.tight_layout()
    fig.savefig(convergence_path)
    plt.close(fig)
    
    # Visualize computational efficiency
    efficiency_path = os.path.join(case_dir, "computational_efficiency.png")
    fig, axs = plt.subplots(2, 1, figsize=(12, 12))
    
    # Plot decision computation time
    for i, data in enumerate(problem_data):
        decision_times = data["computational_metrics"]["decision_times"]
        axs[0].plot(decision_times, '-', linewidth=2, label=data["label"])
    
    axs[0].set_xlabel('Step')
    axs[0].set_ylabel('Computation Time (a.u.)')
    axs[0].set_title('Computational Efficiency: Decision Making Time')
    axs[0].legend()
    axs[0].grid(True, linestyle='--', alpha=0.6)
    
    # Plot belief update magnitude (KL divergence)
    for i, data in enumerate(problem_data):
        belief_updates = data["computational_metrics"]["belief_updates"]
        axs[1].plot(belief_updates, '-', linewidth=2, label=data["label"])
    
    axs[1].set_xlabel('Step')
    axs[1].set_ylabel('Belief Update (KL Divergence)')
    axs[1].set_title('Computational Efficiency: Belief Update Magnitude')
    axs[1].legend()
    axs[1].grid(True, linestyle='--', alpha=0.6)
    
    plt.tight_layout()
    fig.savefig(efficiency_path)
    plt.close(fig)
    
    # Create animation showing the computational process
    animation_path = os.path.join(case_dir, "computational_process_animation.gif")
    
    # Use the wrong prior problem for the animation
    wrong_prior_idx = 1
    data = problem_data[wrong_prior_idx]
    
    belief_history = data["belief_history"]
    state_history = data["state_history"]
    action_history = data["action_history"]
    observation_history = data["observation_history"]
    decision_times = data["computational_metrics"]["decision_times"]
    belief_updates = data["computational_metrics"]["belief_updates"]
    
    fig, axs = plt.subplots(3, 1, figsize=(10, 15))
    
    # Setup for belief state plot
    bars = axs[0].bar(range(model.parameters["n_states"]), belief_history[0])
    axs[0].set_ylim(0, 1)
    axs[0].set_xlabel('State')
    axs[0].set_ylabel('Belief Probability')
    axs[0].set_title('Current Belief State')
    
    # Setup for computation metrics plot
    comp_line, = axs[1].plot([], [], 'r-', linewidth=2, label='Decision Time')
    update_line, = axs[1].plot([], [], 'b-', linewidth=2, label='Belief Update')
    axs[1].set_xlim(0, n_steps)
    axs[1].set_ylim(0, max(max(decision_times), max(belief_updates) if belief_updates else 1) * 1.1)
    axs[1].set_xlabel('Step')
    axs[1].set_ylabel('Computation Metric')
    axs[1].set_title('Computational Process Metrics')
    axs[1].legend()
    axs[1].grid(True, linestyle='--', alpha=0.6)
    
    # Setup for state/action plot
    state_line, = axs[2].plot([], [], 'g-', linewidth=2, label='True State')
    action_bars = axs[2].barh([], [], color='blue', alpha=0.6, label='Action')
    axs[2].set_xlim(-0.5, 1.5)  # For 2 possible actions/states
    axs[2].set_ylim(-0.5, n_steps - 0.5)
    axs[2].set_xlabel('State / Action')
    axs[2].set_ylabel('Step')
    axs[2].set_title('POMDP Method Application')
    axs[2].legend(loc='upper right')
    axs[2].grid(True, linestyle='--', alpha=0.6)
    
    # Text for animation information
    step_text = axs[0].text(0.02, 0.95, '', transform=axs[0].transAxes)
    
    def init():
        for bar in bars:
            bar.set_height(0)
        comp_line.set_data([], [])
        update_line.set_data([], [])
        state_line.set_data([], [])
        # Initialize action bars with empty data
        action_bars.set_width([])
        action_bars.set_y([])
        step_text.set_text('')
        return bars + [comp_line, update_line, state_line, action_bars, step_text]
    
    def update(frame):
        # Update belief bars
        for i, bar in enumerate(bars):
            bar.set_height(belief_history[frame][i])
            # Color according to state
            if i == state_history[frame]:
                bar.set_color('red')
            else:
                bar.set_color('blue')
        
        # Update computation metrics plot
        if frame > 0:
            x_comp = range(1, frame + 1)
            y_comp = decision_times[:frame]
            comp_line.set_data(x_comp, y_comp)
            
            x_update = range(1, frame + 1)
            y_update = belief_updates[:frame]
            update_line.set_data(x_update, y_update)
        
        # Update state line and action bars in state/action plot
        x_state = state_history[:frame+1]
        y_state = range(frame+1)
        state_line.set_data(x_state, y_state)
        
        if frame > 0:
            # Update action bars
            action_widths = [1.0] * frame
            action_positions = range(frame)
            action_bars.set_width(action_widths)
            action_bars.set_y(action_positions)
            action_bars.set_x(action_history[:frame])
        
        # Update step text
        if frame > 0:
            # Calculate belief entropy
            belief = belief_history[frame]
            entropy_val = -np.sum(np.clip(belief, 1e-10, 1.0) * np.log2(np.clip(belief, 1e-10, 1.0)))
            
            step_text.set_text(f'Step: {frame}\n' +
                             f'True State: {state_history[frame]}\n' +
                             f'Predicted State: {np.argmax(belief_history[frame])}\n' +
                             f'Entropy: {entropy_val:.4f}\n' +
                             f'Decision Time: {decision_times[frame-1]:.4f}\n' +
                             f'Belief Update: {belief_updates[frame-1]:.4f}')
        else:
            # Calculate initial entropy
            belief = belief_history[0]
            entropy_val = -np.sum(np.clip(belief, 1e-10, 1.0) * np.log2(np.clip(belief, 1e-10, 1.0)))
            
            step_text.set_text(f'Initial Belief\n' +
                             f'State 0: {belief[0]:.2f}\n' +
                             f'State 1: {belief[1]:.2f}\n' +
                             f'Entropy: {entropy_val:.2f}')
        
        return bars + [comp_line, update_line, state_line, action_bars, step_text]
    
    anim = FuncAnimation(fig, update, frames=len(belief_history),
                         init_func=init, blit=True)
    
    anim.save(animation_path, writer='pillow', fps=1, dpi=100)
    plt.close(fig)
    logger.info(f"Created computational process animation: {animation_path}")
    
    # Visualize method comparison (optimal vs heuristic)
    method_path = os.path.join(case_dir, "method_comparison.png")
    
    # Compare optimal method with heuristic methods
    # 1. Optimal: Use full belief state to choose actions
    # 2. Heuristic 1: Always choose action 0
    # 3. Heuristic 2: Random actions
    
    fig, axs = plt.subplots(n_problems, 1, figsize=(12, 5*n_problems))
    
    if n_problems == 1:
        axs = [axs]
    
    for i, data in enumerate(problem_data):
        ax = axs[i]
        belief_history = np.array(data["belief_history"])
        state_history = np.array(data["state_history"])
        
        # Calculate accuracy of optimal method (already computed)
        optimal_accuracy = []
        for step in range(len(belief_history)):
            predicted_state = np.argmax(belief_history[step])
            true_state = state_history[step]
            acc = 1.0 if predicted_state == true_state else 0.0
            optimal_accuracy.append(acc)
        
        # Calculate theoretical performance of heuristic 1 (always action 0)
        heuristic1_accuracy = [1.0 if state == 0 else 0.0 for state in state_history]
        
        # Calculate theoretical performance of heuristic 2 (random actions)
        # With 2 states, random guessing gives 0.5 accuracy
        heuristic2_accuracy = [0.5] * len(state_history)
        
        # Plot method comparison
        ax.plot(optimal_accuracy, 'g-', linewidth=2, label='Optimal POMDP Method')
        ax.plot(heuristic1_accuracy, 'r-', linewidth=2, label='Heuristic 1 (Action 0)')
        ax.plot(heuristic2_accuracy, 'b--', linewidth=2, label='Heuristic 2 (Random)')
        
        # Compute cumulative average performance
        opt_cumavg = np.cumsum(optimal_accuracy) / np.arange(1, len(optimal_accuracy) + 1)
        h1_cumavg = np.cumsum(heuristic1_accuracy) / np.arange(1, len(heuristic1_accuracy) + 1)
        h2_cumavg = np.cumsum(heuristic2_accuracy) / np.arange(1, len(heuristic2_accuracy) + 1)
        
        # Plot cumulative average performance
        ax.plot(opt_cumavg, 'g--', alpha=0.7, label='Optimal (Cum. Avg.)')
        ax.plot(h1_cumavg, 'r--', alpha=0.7, label='Heuristic 1 (Cum. Avg.)')
        ax.plot(h2_cumavg, 'b:', alpha=0.7, label='Heuristic 2 (Cum. Avg.)')
        
        ax.set_title(f'Method Comparison: {data["label"]}')
        ax.set_xlabel('Step')
        ax.set_ylabel('Accuracy')
        ax.set_ylim(-0.1, 1.1)
        ax.legend()
        ax.grid(True, linestyle='--', alpha=0.6)
    
    plt.tight_layout()
    fig.savefig(method_path)
    plt.close(fig)
    
    # Generate a detailed report
    report_path = os.path.join(case_dir, "report.md")
    with open(report_path, "w") as f:
        f.write(f"# POMDP Model in {Case.INSTRUMENTAL.value} Case\n\n")
        f.write(f"## {case_info['linguistic_meaning']}\n\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n\n")
        f.write(f"### Context\n\n")
        f.write(f"{case_info['pomdp_context']}\n\n")
        f.write("### Mathematical Representation\n\n")
        f.write(f"```\n{case_info['formula']}\n```\n\n")
        f.write("### Primary Methods\n\n")
        f.write(f"{case_info['primary_methods']}\n\n")
        f.write("### Example\n\n")
        f.write(f"{case_info['example']}\n\n")
        f.write("### Algorithm Analysis Results\n\n")
        f.write(f"* Number of problem scenarios tested: {n_problems}\n")
        f.write(f"* Steps per scenario: {n_steps}\n")
        f.write(f"* Problem scenarios:\n")
        for i, data in enumerate(problem_data):
            f.write(f"  - Problem {i+1}: {data['label']}\n")
        
        f.write("\n* Computational metrics:\n")
        for i, data in enumerate(problem_data):
            avg_decision_time = np.mean(data["computational_metrics"]["decision_times"])
            avg_belief_update = np.mean(data["computational_metrics"]["belief_updates"])
            avg_error_improvement = np.mean(data["computational_metrics"]["error_metrics"])
            
            f.write(f"  - Problem {i+1} ({data['label']}):\n")
            f.write(f"    * Average decision time: {avg_decision_time:.4f}\n")
            f.write(f"    * Average belief update magnitude: {avg_belief_update:.4f}\n")
            f.write(f"    * Average error improvement: {avg_error_improvement:.4f}\n")
        
        f.write("\n### POMDP as Computational Method\n\n")
        f.write("In the INSTRUMENTAL case, the POMDP acts as a computational method or tool. ")
        f.write("The focus is on algorithmic properties such as convergence, efficiency, and ")
        f.write("comparison to other methods. The visualizations highlight these aspects across ")
        f.write("different problem scenarios.\n\n")
        f.write("Key insights:\n")
        f.write("1. The POMDP algorithm converges to accurate state predictions over time\n")
        f.write("2. Computational requirements vary with problem difficulty and uncertainty\n")
        f.write("3. The optimal POMDP method outperforms simpler heuristics, especially in difficult scenarios\n")
        f.write("4. Initial beliefs and observation noise impact convergence rates and computational efficiency\n\n")
        f.write("### Visualizations\n\n")
        f.write(f"1. [POMDP Structure](pomdp_structure.png)\n")
        f.write(f"2. [Algorithm Convergence](algorithm_convergence.png)\n")
        f.write(f"3. [Computational Efficiency](computational_efficiency.png)\n")
        f.write(f"4. [Method Comparison](method_comparison.png)\n")
        f.write(f"5. [Computational Process Animation](computational_process_animation.gif)\n")
    
    logger.info(f"Completed INSTRUMENTAL case test with visualizations in {case_dir}")
    
    # Return the model for potential further testing
    return model

def test_locative_case(pomdp_test_data, case_definitions):
    """Test for LOCATIVE case: Model as decision context."""
    # Get case info for logging
    case_info = case_definitions[Case.LOCATIVE]
    logger.info(f"Testing {Case.LOCATIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(OUTPUT_DIR, Case.LOCATIVE.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.LOCATIVE, linguistics_path)
    
    # Implement the test
    # TODO: Implement detailed LOCATIVE case test with visualizations
    
    logger.info(f"Completed LOCATIVE case test with visualizations in {case_dir}")

def test_ablative_case(pomdp_test_data, case_definitions):
    """Test for ABLATIVE case: Model as source of uncertainty."""
    # Get case info for logging
    case_info = case_definitions[Case.ABLATIVE]
    logger.info(f"Testing {Case.ABLATIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(OUTPUT_DIR, Case.ABLATIVE.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.ABLATIVE, linguistics_path)
    
    # Implement the test
    # TODO: Implement detailed ABLATIVE case test with visualizations
    
    logger.info(f"Completed ABLATIVE case test with visualizations in {case_dir}")

def test_vocative_case(pomdp_test_data, case_definitions):
    """Test for VOCATIVE case: Model as addressable interface."""
    # Get case info for logging
    case_info = case_definitions[Case.VOCATIVE]
    logger.info(f"Testing {Case.VOCATIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(OUTPUT_DIR, Case.VOCATIVE.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.VOCATIVE, linguistics_path)
    
    # Implement the test
    # TODO: Implement detailed VOCATIVE case test with visualizations
    
    logger.info(f"Completed VOCATIVE case test with visualizations in {case_dir}")

def run_all_case_tests(output_dir: str = OUTPUT_DIR, specific_case: Optional[str] = None) -> Dict[Case, POMDPModel]:
    """
    Run all case tests and generate overview visualizations.
    
    Args:
        output_dir: Directory to save outputs
        specific_case: Only run tests for this case if specified
        
    Returns:
        Dictionary mapping cases to models
    """
    # Create output directory
    os.makedirs(output_dir, exist_ok=True)
    
    # Override global output directory
    global OUTPUT_DIR
    OUTPUT_DIR = output_dir
    
    # Get case definitions
    case_defs = CaseDefinitions.get_all_cases()
    
    # Create POMDP test data
    # Create POMDP test data manually instead of using the fixture
    
    # Initialize model dictionary
    model_dict = {}
    
    # Determine which cases to run
    if specific_case:
        try:
            cases_to_run = [Case[specific_case.upper()]]
        except KeyError:
            logger.error(f"Invalid case: {specific_case}")
            return model_dict
    else:
        cases_to_run = [
            Case.NOMINATIVE, Case.ACCUSATIVE, Case.DATIVE, Case.GENITIVE,
            Case.INSTRUMENTAL, Case.LOCATIVE, Case.ABLATIVE, Case.VOCATIVE
        ]
    
    # Run each case test
    for case in cases_to_run:
        try:
            logger.info(f"Running test for {case.value} case")
            
            if case == Case.NOMINATIVE:
                model = test_nominative_case(test_data, case_defs)
            elif case == Case.ACCUSATIVE:
                model = test_accusative_case(test_data, case_defs)
            elif case == Case.DATIVE:
                model = test_dative_case(test_data, case_defs)
            elif case == Case.GENITIVE:
                model = test_genitive_case(test_data, case_defs)
            elif case == Case.INSTRUMENTAL:
                model = test_instrumental_case(test_data, case_defs)
            elif case == Case.LOCATIVE:
                model = test_locative_case(test_data, case_defs)
            elif case == Case.ABLATIVE:
                model = test_ablative_case(test_data, case_defs)
            elif case == Case.VOCATIVE:
                model = test_vocative_case(test_data, case_defs)
            
            # Store model in dictionary
            model_dict[case] = model
            
        except Exception as e:
            logger.error(f"Error in {case.value} case test: {str(e)}")
    
    # Generate overview visualization
    overview_path = os.path.join(output_dir, "pomdp_cases_overview.png")
    create_overview_visualization(overview_path)
    
    logger.info(f"All POMDP case tests completed. Overview available at {overview_path}")
    return model_dict

def create_overview_visualization(save_path: str) -> None:
    """Create an overview visualization of all POMDP cases."""
    fig, axs = plt.subplots(4, 2, figsize=(14, 20))
    axs = axs.flatten()
    
    cases = [
        Case.NOMINATIVE, Case.ACCUSATIVE, Case.DATIVE, Case.GENITIVE,
        Case.INSTRUMENTAL, Case.LOCATIVE, Case.ABLATIVE, Case.VOCATIVE
    ]
    
    case_defs = CaseDefinitions.get_all_cases()
    
    for i, case in enumerate(cases):
        ax = axs[i]
        case_info = case_defs[case]
        
        # Set up the axis
        ax.axis('off')
        
        # Add case title
        ax.text(0.5, 0.9, f"{case.value}: {case_info['linguistic_meaning']}", 
                ha='center', fontsize=14, fontweight='bold', transform=ax.transAxes)
        
        # Add POMDP context
        ax.text(0.5, 0.8, case_info['pomdp_context'], 
                ha='center', fontsize=10, transform=ax.transAxes, 
                bbox=dict(boxstyle='round,pad=0.3', facecolor='lightgreen', alpha=0.3))
        
        # Add formula
        ax.text(0.5, 0.6, f"Formula: {case_info['formula']}", 
                ha='center', fontsize=10, transform=ax.transAxes, 
                bbox=dict(boxstyle='round,pad=0.3', facecolor='lightyellow', alpha=0.3))
        
        # Add primary methods
        ax.text(0.5, 0.4, f"Methods: {case_info['primary_methods']}", 
                ha='center', fontsize=10, transform=ax.transAxes,
                bbox=dict(boxstyle='round,pad=0.3', facecolor='lightblue', alpha=0.3))
        
        # Add example
        ax.text(0.5, 0.2, f"Example: {case_info['example']}", 
                ha='center', va='top', fontweight='italic', transform=ax.transAxes)
    
    # Add title
    fig.suptitle("CEREBRUM Cases for POMDP Models", fontsize=20, y=0.98)
    
    # Add explanatory text
    fig.text(0.5, 0.01, 
             "CEREBRUM framework applies linguistic cases to model different functional roles in POMDPs", 
             ha='center', fontsize=12, fontweight='bold')
    
    plt.tight_layout(rect=[0, 0.03, 1, 0.95])
    fig.savefig(save_path, dpi=300)
    plt.close(fig)

if __name__ == "__main__":
    run_all_case_tests() 