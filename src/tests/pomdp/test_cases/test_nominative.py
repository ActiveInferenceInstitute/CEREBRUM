import pytest
import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from typing import Dict, Any, List, Tuple

from src.core.active_inference import ActiveInferenceModel
from src.models.base import Case
from src.models.pomdp import NominativePOMDP
from src.tests.pomdp.utils.test_utils import PomdpTestEnvironment
from src.tests.pomdp.utils.test_models import create_simple_pomdp

from src.tests.pomdp.utils import get_case_dir, create_animation, generate_report
from src.tests.pomdp.visualizers import Visualizer, plot_case_linguistic_context

def test_nominative_case(pomdp_test_data, case_definitions, logger=None):
    """
    Test for NOMINATIVE case: Model as active agent generating actions in POMDP context.
    
    Args:
        pomdp_test_data: Dictionary containing POMDP test data
        case_definitions: Dictionary of case definitions
        logger: Logger instance for logging
        
    Returns:
        Trained POMDP model instance
    """
    # Get case info for logging
    case_info = case_definitions[Case.NOMINATIVE]
    if logger:
        logger.info(f"Testing {Case.NOMINATIVE.value} case: {case_info['linguistic_meaning']}")
        logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = get_case_dir(Case.NOMINATIVE.value)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.NOMINATIVE, linguistics_path, logger)
    
    # Unpack the test data
    transition_matrix = pomdp_test_data["transition_matrix"]
    observation_matrix = pomdp_test_data["observation_matrix"]
    states = pomdp_test_data["states"]
    actions = pomdp_test_data["actions"]
    observations = pomdp_test_data["observations"]
    
    # Create a POMDP model in NOMINATIVE case (active decision-maker)
    model = ActiveInferenceModel(
        name="NomPOMDP",
        parameters={
            "transition_matrix": transition_matrix,
            "observation_matrix": observation_matrix,
            "states": states,
            "actions": actions,
            "n_states": len(states),
            "n_actions": len(actions),
            "n_observations": len(observations)
        }
    )
    model.case = Case.NOMINATIVE  # Explicitly set to NOMINATIVE case
    
    # Log model details
    if logger:
        logger.info(f"Created POMDP model in {Case.NOMINATIVE.value} case (model as active decision-maker)")
        logger.info(f"POMDP parameters: {model.parameters}")
    
    # Visualize POMDP structure
    structure_path = os.path.join(case_dir, "pomdp_structure.png")
    Visualizer.plot_pomdp_structure(
        transition_matrix=model.parameters["transition_matrix"],
        observation_matrix=model.parameters["observation_matrix"],
        title=f"POMDP Structure in {Case.NOMINATIVE.value} Case",
        save_path=structure_path,
        logger=logger
    )
    
    # Initialize belief state visualization
    belief_init_path = os.path.join(case_dir, "initial_belief.png")
    Visualizer.plot_belief_state(
        belief=model.posterior_means,
        title=f"Initial Belief State in {Case.NOMINATIVE.value} Case",
        save_path=belief_init_path,
        logger=logger
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
    current_state = 0  # Starting state
    state_history.append(current_state)
    belief_history.append(model.posterior_means.copy())
    
    # Run the simulation
    if logger:
        logger.info(f"Running POMDP simulation for {n_steps} steps in {Case.NOMINATIVE.value} case")
    
    for step in range(n_steps):
        # Model actively chooses the action (NOMINATIVE case)
        # Make sure action_idx is within valid range
        action_idx = np.argmax(model.posterior_means) % len(actions)
        action_history.append(action_idx)
        
        # Transition to new state based on action
        transition_probs = transition_matrix[current_state, action_idx, :]
        new_state = np.random.choice(len(states), p=transition_probs)
        current_state = new_state
        state_history.append(new_state)
        
        # Get observation
        observation_probs = observation_matrix[new_state, :]
        observation_idx = np.random.choice(len(observations), p=observation_probs)
        observation_history.append(observation_idx)
        
        # Update model's posterior (belief)
        observation_data = np.zeros(len(observations))
        observation_data[observation_idx] = 1.0
        model.update_posterior(observation_data)
        belief_history.append(model.posterior_means.copy())
        
        if logger:
            logger.info(f"Step {step+1}: Action={action_idx}, Observation={observation_idx}, " +
                       f"True State={new_state}, Max Belief State={np.argmax(model.posterior_means)}")
    
    # Visualize the belief evolution
    belief_evolution_path = os.path.join(case_dir, "belief_evolution.png")
    fig, ax = plt.subplots(figsize=(12, 6))
    
    # Convert belief history to array for easier plotting
    belief_array = np.array(belief_history)
    
    # Plot each state's belief over time
    for state in range(len(states)):
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
    axs[0].set_yticks(range(len(actions)))
    axs[0].set_yticklabels(actions)
    axs[0].grid(True, linestyle='--', alpha=0.6)
    
    # Plot states and observations
    axs[1].plot(range(n_steps+1), state_history, 'b-', label='True State')
    axs[1].scatter(range(1, n_steps+1), observation_history, color='orange', label='Observation')
    axs[1].set_xlabel('Step')
    axs[1].set_ylabel('State/Observation')
    axs[1].set_title('States and Observations')
    axs[1].set_yticks(range(len(states)))
    axs[1].legend()
    axs[1].grid(True, linestyle='--', alpha=0.6)

    plt.tight_layout()
    fig.savefig(policy_path)
    plt.close(fig)
    
    # Create an animation of the belief updates
    animation_path = os.path.join(case_dir, "belief_animation.gif")
    
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 10))

    # Setup for belief state plot
    bars = ax1.bar(range(len(states)), belief_history[0])
    ax1.set_ylim(0, 1)
    ax1.set_xlabel('State')
    ax1.set_ylabel('Probability')
    ax1.set_title('Current Belief State')

    # Setup for state/observation plot
    line1, = ax2.plot([], [], 'b-', label='True State')
    line2, = ax2.plot([], [], 'ro', label='Observations')
    ax2.set_xlim(0, n_steps)
    ax2.set_ylim(-0.5, len(states) - 0.5)
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
        return tuple(bars) + (line1, line2, step_text)
    
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
        step_info = f"Step: {frame}"
        if frame > 0:
            step_info += f" | Action: {action_history[frame-1]}"
            step_info += f" | Observation: {observation_history[frame-1]}"
        step_text.set_text(step_info)
        
        return tuple(bars) + (line1, line2, step_text)
    
    # Create and save animation
    create_animation(
        fig=fig, 
        update_func=update, 
        init_func=init, 
        frames=len(belief_history),
        save_path=animation_path,
        fps=2,
        logger=logger
    )
    plt.close(fig)
    
    if logger:
        logger.info(f"Created learning animation: {animation_path}")
    
    # Save final metrics
    metrics_file = os.path.join(case_dir, "metrics.txt")
    with open(metrics_file, "w") as f:
        f.write(f"Case: {Case.NOMINATIVE.value} - {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical Role: {case_info['statistical_role']}\n\n")
        f.write(f"POMDP Context: {case_info.get('pomdp_context', 'Active decision-maker')}\n\n")
        f.write("Simulation Metrics:\n")
        f.write(f"  Number of Steps: {n_steps}\n")
        f.write(f"  Number of States: {len(states)}\n")
        f.write(f"  Number of Actions: {len(actions)}\n")
        f.write(f"  Number of Observations: {len(observations)}\n")
        f.write(f"  Final State: {state_history[-1]}\n")
        f.write(f"  Accuracy: {sum(np.argmax(belief_history[i]) == state_history[i] for i in range(len(state_history))) / len(state_history):.2f}\n")
    
    # Prepare visualizations list for the report
    visualizations = [
        "linguistic_context.png", 
        "pomdp_structure.png", 
        "initial_belief.png", 
        "belief_evolution.png", 
        "policy_visualization.png"
    ]
    if os.path.exists(animation_path):
        visualizations.append(os.path.basename(animation_path))
    
    # Prepare model info for the report
    model_info = {
        "Model type": "POMDP (Partially Observable Markov Decision Process)",
        "Number of states": f"{len(states)}",
        "Number of actions": f"{len(actions)}",
        "Number of observations": f"{len(observations)}",
        "Case function": "Active decision-maker generating actions"
    }
    
    # Generate a report
    report_path = os.path.join(case_dir, "report.md")
    generate_report(
        case_name=Case.NOMINATIVE.value,
        case_info=case_info,
        model_info=model_info,
        visualizations=visualizations,
        save_path=report_path,
        logger=logger
    )
    
    if logger:
        logger.info(f"Completed NOMINATIVE case test with visualizations in {case_dir}")
    
    # Return the model for potential further testing
    return model 