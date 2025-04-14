import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from typing import Dict, Any, List, Tuple

from src.core.model import Case
from src.core.active_inference import ActiveInferenceModel

from src.tests.pomdp.utils import get_case_dir, create_animation, generate_report
from src.tests.pomdp.visualizers import Visualizer, plot_case_linguistic_context

def test_locative_case(pomdp_test_data, case_definitions, logger=None):
    """
    Test for LOCATIVE case: Model as context or container for POMDP processes.
    
    Args:
        pomdp_test_data: Dictionary containing POMDP test data
        case_definitions: Dictionary of case definitions
        logger: Logger instance for logging
        
    Returns:
        Trained POMDP model instance within a specific context
    """
    # Get case info for logging
    case_info = case_definitions[Case.LOCATIVE]
    if logger:
        logger.info(f"Testing {Case.LOCATIVE.value} case: {case_info['linguistic_meaning']}")
        logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = get_case_dir(Case.LOCATIVE.value)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.LOCATIVE, linguistics_path, logger)
    
    # Unpack the test data
    transition_matrix = pomdp_test_data["transition_matrix"]
    observation_matrix = pomdp_test_data["observation_matrix"]
    states = pomdp_test_data["states"]
    actions = pomdp_test_data["actions"]
    observations = pomdp_test_data["observations"]
    
    # Create a POMDP model in LOCATIVE case (as a context/environment)
    model = ActiveInferenceModel(
        name="LocPOMDP",
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
    model.case = Case.LOCATIVE  # Explicitly set to LOCATIVE case
    
    # Log model details
    if logger:
        logger.info(f"Created POMDP model in {Case.LOCATIVE.value} case (model as context/environment)")
        logger.info(f"POMDP parameters: {model.parameters}")
    
    # Visualize POMDP structure
    structure_path = os.path.join(case_dir, "pomdp_structure.png")
    Visualizer.plot_pomdp_structure(
        transition_matrix=model.parameters["transition_matrix"],
        observation_matrix=model.parameters["observation_matrix"],
        title=f"POMDP Structure in {Case.LOCATIVE.value} Case",
        save_path=structure_path,
        logger=logger
    )
    
    # Initialize belief state visualization
    belief_init_path = os.path.join(case_dir, "initial_belief.png")
    Visualizer.plot_belief_state(
        belief=model.posterior_means,
        title=f"Initial Belief State in {Case.LOCATIVE.value} Case",
        save_path=belief_init_path,
        logger=logger
    )
    
    # In LOCATIVE case, the model serves as a context for other processes
    # We'll simulate a process occurring within this context/environment
    
    # Prepare for simulation
    n_steps = 20
    belief_history = []
    action_history = []
    observation_history = []
    state_history = []
    external_actions = []  # Actions chosen by external agent
    
    # Store initial state
    current_state = 0  # Starting state
    state_history.append(current_state)
    belief_history.append(model.posterior_means.copy())
    
    # Run the simulation with multiple external agents exploring the environment
    if logger:
        logger.info(f"Running POMDP simulation for {n_steps} steps in {Case.LOCATIVE.value} case")
    
    # Simulate different exploration strategies in this environment
    exploration_phases = [
        {"name": "Random", "steps": 5},
        {"name": "Greedy", "steps": 5},
        {"name": "Balanced", "steps": 10}
    ]
    
    current_step = 0
    for phase in exploration_phases:
        phase_name = phase["name"]
        phase_steps = phase["steps"]
        
        for step in range(phase_steps):
            # External agent chooses action based on current phase
            if phase_name == "Random":
                action_idx = np.random.choice(len(actions))
            elif phase_name == "Greedy":
                # Take action that maximizes expected observation value
                expected_obs = np.zeros(len(actions))
                for a in range(len(actions)):
                    for s_next in range(len(states)):
                        # Probability of next state given current state and action
                        s_prob = transition_matrix[current_state, a, s_next]
                        # Expected observation value for this state
                        expected_obs[a] += s_prob * np.mean(np.arange(len(observations)) * observation_matrix[s_next, :])
                action_idx = np.argmax(expected_obs)
            else:  # Balanced
                # Mix of exploration and exploitation
                if np.random.random() < 0.7:  # 70% exploit
                    action_idx = np.argmax(model.posterior_means) % len(actions)  # Ensure valid action index
                else:  # 30% explore
                    action_idx = np.random.choice(len(actions))
            
            external_actions.append((phase_name, action_idx))
            action_history.append(action_idx)
            
            # Transition to new state based on action (environment response)
            transition_probs = transition_matrix[current_state, action_idx, :]
            new_state = np.random.choice(len(states), p=transition_probs)
            current_state = new_state
            state_history.append(new_state)
            
            # Get observation from environment
            observation_probs = observation_matrix[new_state, :]
            observation_idx = np.random.choice(len(observations), p=observation_probs)
            observation_history.append(observation_idx)
            
            # Update model's posterior (belief) about the environment state
            observation_data = np.zeros(len(observations))
            observation_data[observation_idx] = 1.0
            model.update_posterior(observation_data)
            belief_history.append(model.posterior_means.copy())
            
            if logger:
                logger.info(f"Step {current_step+1} ({phase_name}): Action={action_idx}, " +
                           f"Observation={observation_idx}, True State={new_state}, " +
                           f"Max Belief State={np.argmax(model.posterior_means)}")
            
            current_step += 1
    
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
    
    # Add phase separation lines
    current_x = 0
    for phase in exploration_phases:
        current_x += phase["steps"]
        if current_x < n_steps:
            ax.axvline(x=current_x, color='black', linestyle='--', alpha=0.5)
            ax.text(current_x - phase["steps"]/2, 1.05, phase["name"], 
                   horizontalalignment='center', transform=ax.get_xaxis_transform())
    ax.text(current_x - exploration_phases[-1]["steps"]/2, 1.05, exploration_phases[-1]["name"], 
           horizontalalignment='center', transform=ax.get_xaxis_transform())
    
    ax.set_xlabel('Step')
    ax.set_ylabel('Belief Probability')
    ax.set_title(f'Belief State Evolution in {Case.LOCATIVE.value} Case')
    ax.set_ylim(0, 1)
    ax.legend()
    ax.grid(True, linestyle='--', alpha=0.6)
    
    plt.tight_layout()
    fig.savefig(belief_evolution_path)
    plt.close(fig)
    
    # Visualize the environmental dynamics
    env_path = os.path.join(case_dir, "environment_visualization.png")
    fig, axs = plt.subplots(3, 1, figsize=(12, 12))
    
    # Plot phase information
    phase_colors = {'Random': 'lightblue', 'Greedy': 'lightsalmon', 'Balanced': 'lightgreen'}
    for i, (phase_name, _) in enumerate(external_actions):
        axs[0].axvspan(i, i+1, color=phase_colors[phase_name], alpha=0.3)
    
    # Plot actions taken by external agents
    action_y = action_history  # action_history is already a list of action indices
    axs[0].step(range(n_steps), action_y, where='mid', linewidth=2, color='blue')
    axs[0].set_xlabel('Step')
    axs[0].set_ylabel('Action')
    axs[0].set_title('Actions Taken in Environment')
    axs[0].set_yticks(range(len(actions)))
    axs[0].set_yticklabels(actions)
    axs[0].grid(True, linestyle='--', alpha=0.6)
    
    # Plot states of the environment
    axs[1].plot(range(n_steps+1), state_history, 'g-', label='Environment State')
    axs[1].set_xlabel('Step')
    axs[1].set_ylabel('State')
    axs[1].set_title('Environment States')
    axs[1].set_yticks(range(len(states)))
    axs[1].grid(True, linestyle='--', alpha=0.6)
    
    # Plot observations from the environment
    axs[2].scatter(range(1, n_steps+1), observation_history, color='orange', s=70)
    axs[2].set_xlabel('Step')
    axs[2].set_ylabel('Observation')
    axs[2].set_title('Observations from Environment')
    axs[2].set_yticks(range(len(observations)))
    axs[2].grid(True, linestyle='--', alpha=0.6)

    plt.tight_layout()
    fig.savefig(env_path)
    plt.close(fig)
    
    # Create an animation of the environment exploration
    animation_path = os.path.join(case_dir, "environment_exploration.gif")
    
    fig, (ax1, ax2, ax3) = plt.subplots(3, 1, figsize=(10, 12))

    # Setup for belief state plot
    bars = ax1.bar(range(len(states)), belief_history[0])
    ax1.set_ylim(0, 1)
    ax1.set_xlabel('State')
    ax1.set_ylabel('Probability')
    ax1.set_title('Current Belief About Environment')

    # Setup for state plot
    line1, = ax2.plot([], [], 'g-', label='Environment State')
    ax2.set_xlim(0, n_steps)
    ax2.set_ylim(-0.5, len(states) - 0.5)
    ax2.set_xlabel('Step')
    ax2.set_ylabel('State')
    ax2.legend()
    ax2.grid(True, linestyle='--', alpha=0.6)
    
    # Setup for observation plot
    line2, = ax3.plot([], [], 'ro', label='Observations')
    ax3.set_xlim(0, n_steps)
    ax3.set_ylim(-0.5, len(observations) - 0.5)
    ax3.set_xlabel('Step')
    ax3.set_ylabel('Observation')
    ax3.legend()
    ax3.grid(True, linestyle='--', alpha=0.6)

    # Text to show step and phase information
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
                bar.set_color('green')
            else:
                bar.set_color('blue')
        
        # Update state plot
        x_data = range(frame + 1)
        line1.set_data(x_data, state_history[:frame + 1])
        
        # Only show observations up to current frame
        if frame > 0:
            line2.set_data(range(1, frame + 1), observation_history[:frame])
        
        # Update step text with phase information
        if frame > 0:
            phase_name = external_actions[frame-1][0]
            action_idx = external_actions[frame-1][1]
            step_info = f"Step: {frame} | Phase: {phase_name}"
            step_info += f" | Action: {action_idx}"
            step_info += f" | Observation: {observation_history[frame-1]}"
        else:
            step_info = "Initial Environment State"
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
        logger.info(f"Created environment exploration animation: {animation_path}")
    
    # Save final metrics
    metrics_file = os.path.join(case_dir, "metrics.txt")
    with open(metrics_file, "w") as f:
        f.write(f"Case: {Case.LOCATIVE.value} - {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical Role: {case_info['statistical_role']}\n\n")
        f.write(f"POMDP Context: {case_info.get('pomdp_context', 'Environmental context')}\n\n")
        f.write("Simulation Metrics:\n")
        f.write(f"  Number of Steps: {n_steps}\n")
        f.write(f"  Number of States: {len(states)}\n")
        f.write(f"  Number of Actions: {len(actions)}\n")
        f.write(f"  Number of Observations: {len(observations)}\n")
        f.write(f"  Final State: {state_history[-1]}\n")
        f.write("  Exploration Phases:\n")
        for phase in exploration_phases:
            f.write(f"    - {phase['name']}: {phase['steps']} steps\n")
    
    # Prepare visualizations list for the report
    visualizations = [
        "linguistic_context.png", 
        "pomdp_structure.png", 
        "initial_belief.png", 
        "belief_evolution.png", 
        "environment_visualization.png"
    ]
    if os.path.exists(animation_path):
        visualizations.append(os.path.basename(animation_path))
    
    # Prepare model info for the report
    model_info = {
        "Model type": "POMDP (Partially Observable Markov Decision Process)",
        "Number of states": f"{len(states)}",
        "Number of actions": f"{len(actions)}",
        "Number of observations": f"{len(observations)}",
        "Case function": "Environmental context where processes occur"
    }
    
    # Generate a report
    report_path = os.path.join(case_dir, "report.md")
    generate_report(
        case_name=Case.LOCATIVE.value,
        case_info=case_info,
        model_info=model_info,
        visualizations=visualizations,
        save_path=report_path,
        logger=logger
    )
    
    if logger:
        logger.info(f"Completed LOCATIVE case test with visualizations in {case_dir}")
    
    # Return the model for potential further testing
    return model 