import pytest
import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from typing import Dict, Any, List, Tuple

from src.core.active_inference import ActiveInferenceModel
from src.models.base import Case
from src.models.pomdp import POMDP, AblativePOMDP
from src.tests.pomdp.utils.test_utils import PomdpTestEnvironment
from src.tests.pomdp.utils.test_models import create_simple_pomdp

from src.tests.pomdp.utils import get_case_dir, create_animation, generate_report
from src.tests.pomdp.visualizers import Visualizer, plot_case_linguistic_context

def test_ablative_case(pomdp_test_data, case_definitions, logger=None):
    """
    Test for ABLATIVE case: Model as origin or source of information/signals.
    
    Args:
        pomdp_test_data: Dictionary containing POMDP test data
        case_definitions: Dictionary of case definitions
        logger: Logger instance for logging
        
    Returns:
        Trained POMDP model instance as a source
    """
    # Get case info for logging
    case_info = case_definitions[Case.ABLATIVE]
    if logger:
        logger.info(f"Testing {Case.ABLATIVE.value} case: {case_info['linguistic_meaning']}")
        logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = get_case_dir(Case.ABLATIVE.value)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.ABLATIVE, linguistics_path, logger)
    
    # Unpack the test data
    transition_matrix = pomdp_test_data["transition_matrix"]
    observation_matrix = pomdp_test_data["observation_matrix"]
    states = pomdp_test_data["states"]
    actions = pomdp_test_data["actions"]
    observations = pomdp_test_data["observations"]
    
    # Create a POMDP model in ABLATIVE case (as source/origin)
    model = ActiveInferenceModel(
        name="AblPOMDP",
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
    model.case = Case.ABLATIVE  # Explicitly set to ABLATIVE case
    
    # Log model details
    if logger:
        logger.info(f"Created POMDP model in {Case.ABLATIVE.value} case (model as source/origin)")
        logger.info(f"POMDP parameters: {model.parameters}")
    
    # Visualize POMDP structure
    structure_path = os.path.join(case_dir, "pomdp_structure.png")
    Visualizer.plot_pomdp_structure(
        transition_matrix=model.parameters["transition_matrix"],
        observation_matrix=model.parameters["observation_matrix"],
        title=f"POMDP Structure in {Case.ABLATIVE.value} Case",
        save_path=structure_path,
        logger=logger
    )
    
    # Initialize state distribution - for ABLATIVE case, this represents
    # the source distribution that generates observations
    source_distribution = np.zeros(len(states))
    source_distribution[0] = 1.0  # Start from state 0
    
    # Visualize the source distribution
    source_path = os.path.join(case_dir, "source_distribution.png")
    Visualizer.plot_belief_state(
        belief=source_distribution,
        title=f"Initial Source Distribution in {Case.ABLATIVE.value} Case",
        save_path=source_path,
        logger=logger
    )
    
    # In ABLATIVE case, the model serves as a source/origin of observations
    # We'll simulate how different receivers update their beliefs from this source
    
    # Prepare for simulation
    n_steps = 20
    source_states = []  # True states of the source POMDP
    source_observations = []  # Observations generated from the source
    
    # We'll track multiple receivers with different learning rates
    receivers = [
        {"name": "Fast Learner", "learning_rate": 0.8, "beliefs": [], "color": "green"},
        {"name": "Medium Learner", "learning_rate": 0.4, "beliefs": [], "color": "blue"},
        {"name": "Slow Learner", "learning_rate": 0.1, "beliefs": [], "color": "red"}
    ]
    
    # Initialize receivers with uniform belief
    initial_belief = np.ones(len(states)) / len(states)
    for receiver in receivers:
        receiver["beliefs"].append(initial_belief.copy())
    
    # Store initial source state
    current_state = 0  # Starting state
    source_states.append(current_state)
    
    # Run the simulation
    if logger:
        logger.info(f"Running POMDP simulation for {n_steps} steps in {Case.ABLATIVE.value} case")
    
    for step in range(n_steps):
        # Source generates observation from current state (ABLATIVE case - origin of signal)
        observation_probs = observation_matrix[current_state, :]
        observation_idx = np.random.choice(len(observations), p=observation_probs)
        source_observations.append(observation_idx)
        
        # All receivers observe the same signal from the source
        observation_data = np.zeros(len(observations))
        observation_data[observation_idx] = 1.0
        
        # Each receiver updates its belief based on the observation with different learning rates
        for receiver in receivers:
            current_belief = receiver["beliefs"][-1]
            learning_rate = receiver["learning_rate"]
            
            # Compute observation likelihood for each state
            likelihood = np.zeros(len(states))
            for s in range(len(states)):
                likelihood[s] = observation_matrix[s, observation_idx]
            
            # Bayes rule with learning rate factor
            new_belief = current_belief * (1 + learning_rate * (likelihood - 1))
            new_belief /= np.sum(new_belief)  # Normalize
            
            receiver["beliefs"].append(new_belief)
        
        # Source transitions to a new state 
        action_idx = 0  # Fixed action for source transition
        transition_probs = transition_matrix[current_state, action_idx, :]
        new_state = np.random.choice(len(states), p=transition_probs)
        current_state = new_state
        source_states.append(current_state)
        
        if logger:
            logger.info(f"Step {step+1}: Source State={current_state}, " +
                       f"Generated Observation={observation_idx}")
            for receiver in receivers:
                max_belief_state = np.argmax(receiver["beliefs"][-1])
                confidence = receiver["beliefs"][-1][max_belief_state]
                logger.info(f"  {receiver['name']}: Max Belief State={max_belief_state}, " +
                          f"Confidence={confidence:.2f}")
    
    # Visualize the belief evolution for all receivers
    belief_evolution_path = os.path.join(case_dir, "belief_evolution.png")
    fig, axs = plt.subplots(len(receivers), 1, figsize=(12, 4*len(receivers)), sharex=True)
    
    # Plot each receiver's belief evolution
    for i, receiver in enumerate(receivers):
        ax = axs[i]
        belief_array = np.array(receiver["beliefs"])
        
        # Plot each state's belief over time
        for state in range(len(states)):
            ax.plot(belief_array[:, state], label=f'State {state}', linewidth=2)
        
        # Mark the true source states
        for step, state in enumerate(source_states):
            ax.scatter(step, belief_array[step, state], color='red', s=50, zorder=10)
        
        ax.set_ylabel('Belief Probability')
        ax.set_title(f'{receiver["name"]} Belief Evolution (LR={receiver["learning_rate"]})')
        ax.set_ylim(0, 1)
        ax.legend()
        ax.grid(True, linestyle='--', alpha=0.6)
    
    axs[-1].set_xlabel('Step')
    plt.tight_layout()
    fig.savefig(belief_evolution_path)
    plt.close(fig)
    
    # Visualize accuracy of each receiver
    accuracy_path = os.path.join(case_dir, "receiver_accuracy.png")
    fig, ax = plt.subplots(figsize=(12, 6))
    
    # Calculate accuracy over time for each receiver
    accuracy_data = []
    for receiver in receivers:
        beliefs = np.array(receiver["beliefs"])
        accuracy = np.zeros(len(source_states))
        for t in range(len(source_states)):
            max_belief_state = np.argmax(beliefs[t])
            accuracy[t] = 1.0 if max_belief_state == source_states[t] else 0.0
        accuracy_data.append(accuracy)
    
    # Plot accuracy
    for i, receiver in enumerate(receivers):
        ax.plot(np.arange(len(source_states)), accuracy_data[i], 
                label=receiver["name"], color=receiver["color"], linewidth=2)
    
    ax.set_xlabel('Step')
    ax.set_ylabel('Accuracy (1 = Correct Belief)')
    ax.set_title(f'Receiver Accuracy in {Case.ABLATIVE.value} Case')
    ax.set_yticks([0, 1])
    ax.set_yticklabels(["Incorrect", "Correct"])
    ax.legend()
    ax.grid(True, linestyle='--', alpha=0.6)
    
    plt.tight_layout()
    fig.savefig(accuracy_path)
    plt.close(fig)
    
    # Create a visualization of source and observations
    source_obs_path = os.path.join(case_dir, "source_observations.png")
    fig, axs = plt.subplots(2, 1, figsize=(12, 8))
    
    # Plot source states
    axs[0].plot(range(n_steps+1), source_states, 'g-o', label='Source State')
    axs[0].set_xlabel('Step')
    axs[0].set_ylabel('State')
    axs[0].set_title('Source State Evolution')
    axs[0].set_yticks(range(len(states)))
    axs[0].grid(True, linestyle='--', alpha=0.6)
    axs[0].legend()
    
    # Plot observations
    axs[1].scatter(range(1, n_steps+1), source_observations, color='orange', s=100, label='Observations')
    axs[1].set_xlabel('Step')
    axs[1].set_ylabel('Observation')
    axs[1].set_title('Observations from Source')
    axs[1].set_yticks(range(len(observations)))
    axs[1].grid(True, linestyle='--', alpha=0.6)
    axs[1].legend()
    
    plt.tight_layout()
    fig.savefig(source_obs_path)
    plt.close(fig)
    
    # Create an animation of the belief updates from the source
    animation_path = os.path.join(case_dir, "source_receiver_animation.gif")
    
    fig, (ax1, ax2, ax3) = plt.subplots(3, 1, figsize=(10, 12))
    
    # Setup for source state plot
    line_source, = ax1.plot([], [], 'g-o', label='Source State')
    ax1.set_xlim(0, n_steps)
    ax1.set_ylim(-0.5, len(states) - 0.5)
    ax1.set_xlabel('Step')
    ax1.set_ylabel('State')
    ax1.set_title('Source State (ABLATIVE Case)')
    ax1.grid(True, linestyle='--', alpha=0.6)
    ax1.legend()
    
    # Setup for observation plot
    line_obs, = ax2.plot([], [], 'ro', label='Observations')
    ax2.set_xlim(0, n_steps)
    ax2.set_ylim(-0.5, len(observations) - 0.5)
    ax2.set_xlabel('Step')
    ax2.set_ylabel('Observation')
    ax2.set_title('Observations from Source')
    ax2.grid(True, linestyle='--', alpha=0.6)
    ax2.legend()
    
    # Setup for receivers' belief bars
    width = 0.2
    n_receivers = len(receivers)
    bar_positions = []
    for i in range(n_receivers):
        offset = width * (i - (n_receivers-1)/2)
        bar_positions.append(np.arange(len(states)) + offset)
    
    # Create initial empty bars for each receiver
    receiver_bars = []
    for i, receiver in enumerate(receivers):
        bars = ax3.bar(bar_positions[i], np.zeros(len(states)), width, 
                     label=receiver["name"], color=receiver["color"], alpha=0.7)
        receiver_bars.append(bars)
    
    ax3.set_xlabel('State')
    ax3.set_ylabel('Belief Probability')
    ax3.set_title('Receivers\' Beliefs')
    ax3.set_xticks(np.arange(len(states)))
    ax3.set_ylim(0, 1)
    ax3.legend()
    
    # Text for step information
    step_text = ax1.text(0.02, 0.95, '', transform=ax1.transAxes)
    
    def init():
        line_source.set_data([], [])
        line_obs.set_data([], [])
        for bars in receiver_bars:
            for bar in bars:
                bar.set_height(0)
        step_text.set_text('')
        return (line_source, line_obs, step_text) + tuple(bar for bars in receiver_bars for bar in bars)
    
    def update(frame):
        # Update source state line
        x_data = range(frame + 1)
        line_source.set_data(x_data, source_states[:frame + 1])
        
        # Update observations
        if frame > 0:
            line_obs.set_data(range(1, frame + 1), source_observations[:frame])
        
        # Update receiver belief bars
        for i, receiver in enumerate(receivers):
            for j, bar in enumerate(receiver_bars[i]):
                bar.set_height(receiver["beliefs"][frame][j])
                # Highlight the bar for the true state
                if j == source_states[frame]:
                    bar.set_edgecolor('black')
                    bar.set_linewidth(2)
                else:
                    bar.set_edgecolor(receiver["color"])
                    bar.set_linewidth(1)
        
        # Update step text
        step_info = f"Step: {frame}"
        if frame > 0:
            step_info += f" | Observation: {source_observations[frame-1]}"
            step_info += f" | Source State: {source_states[frame]}"
        step_text.set_text(step_info)
        
        return (line_source, line_obs, step_text) + tuple(bar for bars in receiver_bars for bar in bars)
    
    # Create and save animation
    create_animation(
        fig=fig, 
        update_func=update, 
        init_func=init, 
        frames=len(source_states),
        save_path=animation_path,
        fps=2,
        logger=logger
    )
    plt.close(fig)
    
    if logger:
        logger.info(f"Created source-receiver animation: {animation_path}")
    
    # Save final metrics
    metrics_file = os.path.join(case_dir, "metrics.txt")
    with open(metrics_file, "w") as f:
        f.write(f"Case: {Case.ABLATIVE.value} - {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical Role: {case_info['statistical_role']}\n\n")
        f.write(f"POMDP Context: {case_info.get('pomdp_context', 'Origin/source of observations')}\n\n")
        f.write("Simulation Metrics:\n")
        f.write(f"  Number of Steps: {n_steps}\n")
        f.write(f"  Number of States: {len(states)}\n")
        f.write(f"  Number of Actions: {len(actions)}\n")
        f.write(f"  Number of Observations: {len(observations)}\n")
        f.write(f"  Final Source State: {source_states[-1]}\n\n")
        f.write("Receiver Performance:\n")
        for receiver in receivers:
            # Calculate final accuracy
            correct_beliefs = sum(np.argmax(receiver["beliefs"][i]) == source_states[i] 
                                 for i in range(len(source_states)))
            accuracy = correct_beliefs / len(source_states)
            f.write(f"  {receiver['name']} (LR={receiver['learning_rate']}):\n")
            f.write(f"    - Final Accuracy: {accuracy:.2f}\n")
            f.write(f"    - Confidence in Final Belief: {np.max(receiver['beliefs'][-1]):.2f}\n")
    
    # Prepare visualizations list for the report
    visualizations = [
        "linguistic_context.png", 
        "pomdp_structure.png", 
        "source_distribution.png",
        "belief_evolution.png", 
        "receiver_accuracy.png",
        "source_observations.png"
    ]
    if os.path.exists(animation_path):
        visualizations.append(os.path.basename(animation_path))
    
    # Prepare model info for the report
    model_info = {
        "Model type": "POMDP (Partially Observable Markov Decision Process)",
        "Number of states": f"{len(states)}",
        "Number of actions": f"{len(actions)}",
        "Number of observations": f"{len(observations)}",
        "Case function": "Origin/source of information and observations"
    }
    
    # Generate a report
    report_path = os.path.join(case_dir, "report.md")
    generate_report(
        case_name=Case.ABLATIVE.value,
        case_info=case_info,
        model_info=model_info,
        visualizations=visualizations,
        save_path=report_path,
        logger=logger
    )
    
    if logger:
        logger.info(f"Completed ABLATIVE case test with visualizations in {case_dir}")
    
    # Return the model for potential further testing
    return model 