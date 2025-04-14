import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from typing import Dict, Any, List, Tuple

from src.core.model import Case
from src.core.active_inference import ActiveInferenceModel

from src.tests.pomdp.utils import get_case_dir, create_animation, generate_report
from src.tests.pomdp.visualizers import Visualizer, plot_case_linguistic_context

def test_vocative_case(pomdp_test_data, case_definitions, logger=None):
    """
    Test for VOCATIVE case: Model as an addressable entity that can be directly queried or invoked.
    
    Args:
        pomdp_test_data: Dictionary containing POMDP test data
        case_definitions: Dictionary of case definitions
        logger: Logger instance for logging
        
    Returns:
        Trained POMDP model instance that responds to queries
    """
    # Get case info for logging
    case_info = case_definitions[Case.VOCATIVE]
    if logger:
        logger.info(f"Testing {Case.VOCATIVE.value} case: {case_info['linguistic_meaning']}")
        logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = get_case_dir(Case.VOCATIVE.value)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.VOCATIVE, linguistics_path, logger)
    
    # Unpack the test data
    transition_matrix = pomdp_test_data["transition_matrix"]
    observation_matrix = pomdp_test_data["observation_matrix"]
    states = pomdp_test_data["states"]
    actions = pomdp_test_data["actions"]
    observations = pomdp_test_data["observations"]
    
    # Create a POMDP model in VOCATIVE case (as addressable entity)
    model = ActiveInferenceModel(
        name="VocPOMDP",
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
    model.case = Case.VOCATIVE  # Explicitly set to VOCATIVE case
    
    # Log model details
    if logger:
        logger.info(f"Created POMDP model in {Case.VOCATIVE.value} case (model as addressable entity)")
        logger.info(f"POMDP parameters: {model.parameters}")
    
    # Visualize POMDP structure
    structure_path = os.path.join(case_dir, "pomdp_structure.png")
    Visualizer.plot_pomdp_structure(
        transition_matrix=model.parameters["transition_matrix"],
        observation_matrix=model.parameters["observation_matrix"],
        title=f"POMDP Structure in {Case.VOCATIVE.value} Case",
        save_path=structure_path,
        logger=logger
    )
    
    # Initialize belief state visualization
    belief_init_path = os.path.join(case_dir, "initial_belief.png")
    Visualizer.plot_belief_state(
        belief=model.posterior_means,
        title=f"Initial Belief State in {Case.VOCATIVE.value} Case",
        save_path=belief_init_path,
        logger=logger
    )
    
    # In VOCATIVE case, the model is directly addressed and queried
    # We'll simulate a question-answering interaction with the model
    
    # Define query types
    query_types = [
        {"name": "State Query", 
         "description": "What state are you in?",
         "response_fn": lambda m, true_state: 
             f"My belief distribution is {np.round(m.posterior_means, 2)}, " +
             f"most likely state: {np.argmax(m.posterior_means)}, " +
             f"true state: {true_state}"},
        {"name": "Action Query", 
         "description": "What action would you take?",
         "response_fn": lambda m, true_state: 
             f"I would take action {np.argmax(m.posterior_means)}"},
        {"name": "Prediction Query", 
         "description": "What observation do you expect?",
         "response_fn": lambda m, true_state: 
             f"I expect observation {np.argmax(np.sum(m.posterior_means[:, np.newaxis] * m.parameters['observation_matrix'], axis=0))}"},
        {"name": "Confidence Query", 
         "description": "How certain are you?",
         "response_fn": lambda m, true_state: 
             f"My confidence is {np.max(m.posterior_means):.2f} in state {np.argmax(m.posterior_means)}"}
    ]
    
    # Prepare for simulation
    n_steps = 20
    true_states = []
    query_history = []
    response_history = []
    observation_history = []
    belief_history = []
    
    # Store initial state
    current_state = 0  # Starting state
    true_states.append(current_state)
    belief_history.append(model.posterior_means.copy())
    
    # Run the simulation
    if logger:
        logger.info(f"Running POMDP simulation for {n_steps} steps in {Case.VOCATIVE.value} case")
    
    for step in range(n_steps):
        # Choose a random query type
        query_idx = np.random.randint(0, len(query_types))
        query = query_types[query_idx]
        query_history.append(query)
        
        # Get model's response
        response = query["response_fn"](model, current_state)
        response_history.append(response)
        
        if logger:
            logger.info(f"Step {step+1}: Query='{query['description']}', Response='{response}'")
        
        # Environment generates observation based on current state
        observation_probs = observation_matrix[current_state, :]
        observation_idx = np.random.choice(len(observations), p=observation_probs)
        observation_history.append(observation_idx)
        
        # Update model's posterior based on new observation
        observation_data = np.zeros(len(observations))
        observation_data[observation_idx] = 1.0
        model.update_posterior(observation_data)
        belief_history.append(model.posterior_means.copy())
        
        # State transition (environment changes)
        action_idx = np.random.randint(0, len(actions))  # Random action from environment
        transition_probs = transition_matrix[current_state, action_idx, :]
        new_state = np.random.choice(len(states), p=transition_probs)
        current_state = new_state
        true_states.append(current_state)
        
        if logger:
            logger.info(f"  Observation={observation_idx}, New True State={current_state}")
            logger.info(f"  Updated Belief: {np.round(model.posterior_means, 2)}")
    
    # Visualize the query-response interactions
    interaction_path = os.path.join(case_dir, "interaction_history.png")
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(12, 10))
    
    # Plot true states and observations
    ax1.plot(range(n_steps+1), true_states, 'b-', label='True State')
    ax1.scatter(range(1, n_steps+1), observation_history, color='orange', label='Observation')
    ax1.set_xlabel('Step')
    ax1.set_ylabel('State/Observation')
    ax1.set_title('Environment States and Observations')
    ax1.set_yticks(range(max(len(states), len(observations))))
    ax1.legend()
    ax1.grid(True, linestyle='--', alpha=0.6)
    
    # Plot query types over time
    query_colors = {'State Query': 'blue', 'Action Query': 'green', 
                   'Prediction Query': 'red', 'Confidence Query': 'purple'}
    
    # Create colored background regions for query types
    for i, query in enumerate(query_history):
        ax2.axvspan(i, i+1, color=query_colors[query["name"]], alpha=0.3)
    
    # Plot model's confidence over time
    belief_array = np.array(belief_history)
    confidence = np.max(belief_array, axis=1)
    ax2.plot(range(n_steps+1), confidence, 'k-', linewidth=2, label='Confidence')
    
    # Add query labels
    for i, query in enumerate(query_history):
        if i % 3 == 0:  # Show every 3rd query to avoid clutter
            ax2.text(i + 0.5, 0.05, query["name"], rotation=90,
                   verticalalignment='bottom', horizontalalignment='center')
    
    ax2.set_xlabel('Step')
    ax2.set_ylabel('Confidence')
    ax2.set_title('Query Types and Model Confidence')
    ax2.set_xlim(0, n_steps)
    ax2.set_ylim(0, 1)
    ax2.legend()
    ax2.grid(True, linestyle='--', alpha=0.6)
    
    plt.tight_layout()
    fig.savefig(interaction_path)
    plt.close(fig)
    
    # Visualize the belief evolution
    belief_evolution_path = os.path.join(case_dir, "belief_evolution.png")
    fig, ax = plt.subplots(figsize=(12, 6))
    
    # Convert belief history to array for easier plotting
    belief_array = np.array(belief_history)
    
    # Plot each state's belief over time
    for state in range(len(states)):
        ax.plot(belief_array[:, state], label=f'State {state}', linewidth=2)
    
    # Mark the true states
    for step, state in enumerate(true_states):
        ax.scatter(step, belief_array[step, state], color='red', s=50, zorder=10)
    
    ax.set_xlabel('Step')
    ax.set_ylabel('Belief Probability')
    ax.set_title(f'Belief State Evolution in {Case.VOCATIVE.value} Case')
    ax.set_ylim(0, 1)
    ax.legend()
    ax.grid(True, linestyle='--', alpha=0.6)
    
    plt.tight_layout()
    fig.savefig(belief_evolution_path)
    plt.close(fig)
    
    # Create a dialog visualization showing query-response pairs
    dialog_path = os.path.join(case_dir, "dialog_visualization.png")
    fig, ax = plt.subplots(figsize=(14, 10))
    
    # Set up dialog display
    ax.axis('off')
    ax.set_xlim(0, 10)
    ax.set_ylim(0, n_steps + 1)
    
    # Add title
    ax.text(5, n_steps + 0.5, f"Dialog with POMDP in {Case.VOCATIVE.value} Case", 
           horizontalalignment='center', fontsize=16, fontweight='bold')
    
    # Draw dialog bubbles
    for i, (query, response) in enumerate(zip(query_history, response_history)):
        # Position from bottom to top
        y_pos = n_steps - i
        
        # Draw query on the left
        query_text = f"Query: {query['description']}"
        ax.text(0.2, y_pos, query_text, fontsize=10, 
               bbox=dict(boxstyle="round,pad=0.8", facecolor='lightblue', alpha=0.8))
        
        # Draw response on the right
        ax.text(9.8, y_pos, response, fontsize=10, 
               horizontalalignment='right', 
               bbox=dict(boxstyle="round,pad=0.8", facecolor='lightgreen', alpha=0.8))
        
        # Add observation information
        if i < len(observation_history):
            obs_text = f"Observation: {observation_history[i]}"
            ax.text(5, y_pos - 0.4, obs_text, fontsize=8, 
                  horizontalalignment='center', 
                  bbox=dict(boxstyle="round,pad=0.3", facecolor='lightyellow', alpha=0.8))
    
    plt.tight_layout()
    fig.savefig(dialog_path)
    plt.close(fig)
    
    # Create an animation of the dialog interaction
    animation_path = os.path.join(case_dir, "dialog_animation.gif")
    
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 10))

    # Setup for belief state plot
    bars = ax1.bar(range(len(states)), belief_history[0])
    ax1.set_ylim(0, 1)
    ax1.set_xlabel('State')
    ax1.set_ylabel('Probability')
    ax1.set_title('Model Belief State')

    # Setup for query/response display
    ax2.axis('off')
    query_text = ax2.text(0.5, 0.8, '', fontsize=12, horizontalalignment='center',
                      bbox=dict(boxstyle="round,pad=0.8", facecolor='lightblue', alpha=0.8),
                      transform=ax2.transAxes)
    response_text = ax2.text(0.5, 0.5, '', fontsize=12, horizontalalignment='center', 
                         bbox=dict(boxstyle="round,pad=0.8", facecolor='lightgreen', alpha=0.8),
                         transform=ax2.transAxes)
    observation_text = ax2.text(0.5, 0.2, '', fontsize=10, horizontalalignment='center',
                           bbox=dict(boxstyle="round,pad=0.5", facecolor='lightyellow', alpha=0.8),
                           transform=ax2.transAxes)
    
    def init():
        for bar in bars:
            bar.set_height(0)
        query_text.set_text('')
        response_text.set_text('')
        observation_text.set_text('')
        return tuple(bars) + (query_text, response_text, observation_text)
    
    def update(frame):
        # Update belief bars
        for i, bar in enumerate(bars):
            bar.set_height(belief_history[frame][i])
            # Color the true state differently
            if i == true_states[frame]:
                bar.set_color('red')
            else:
                bar.set_color('blue')
        
        # Update dialog texts
        if frame > 0:
            query_idx = frame - 1
            if query_idx < len(query_history):
                query_text.set_text(f"Query: {query_history[query_idx]['description']}")
                response_text.set_text(f"Response: {response_history[query_idx]}")
                observation_text.set_text(f"Observation: {observation_history[query_idx]}")
            else:
                query_text.set_text('')
                response_text.set_text('')
                observation_text.set_text('')
        else:
            query_text.set_text('')
            response_text.set_text('')
            observation_text.set_text('')
        
        return tuple(bars) + (query_text, response_text, observation_text)
    
    # Create and save animation
    create_animation(
        fig=fig, 
        update_func=update, 
        init_func=init, 
        frames=len(belief_history),
        save_path=animation_path,
        fps=1,  # Slower to read dialog
        logger=logger
    )
    plt.close(fig)
    
    if logger:
        logger.info(f"Created dialog animation: {animation_path}")
    
    # Save interaction transcript
    transcript_file = os.path.join(case_dir, "dialog_transcript.txt")
    with open(transcript_file, "w") as f:
        f.write(f"Dialog with POMDP in {Case.VOCATIVE.value} Case\n")
        f.write("="*50 + "\n\n")
        for i, (query, response) in enumerate(zip(query_history, response_history)):
            f.write(f"Step {i+1}:\n")
            f.write(f"Query: {query['description']}\n")
            f.write(f"Response: {response}\n")
            f.write(f"Observation: {observation_history[i]}\n")
            f.write(f"True State: {true_states[i+1]}\n\n")
    
    # Save final metrics
    metrics_file = os.path.join(case_dir, "metrics.txt")
    with open(metrics_file, "w") as f:
        f.write(f"Case: {Case.VOCATIVE.value} - {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical Role: {case_info['statistical_role']}\n\n")
        f.write(f"POMDP Context: {case_info.get('pomdp_context', 'Addressable entity that responds to queries')}\n\n")
        f.write("Simulation Metrics:\n")
        f.write(f"  Number of Steps: {n_steps}\n")
        f.write(f"  Number of States: {len(states)}\n")
        f.write(f"  Number of Actions: {len(actions)}\n")
        f.write(f"  Number of Observations: {len(observations)}\n")
        f.write(f"  Final State: {true_states[-1]}\n")
        
        # Calculate accuracy metrics for each query type
        f.write("\nQuery Response Accuracy:\n")
        query_accuracy = {}
        for q_type in [q["name"] for q in query_types]:
            query_accuracy[q_type] = {"count": 0, "correct": 0}
        
        for i, query in enumerate(query_history):
            q_name = query["name"]
            query_accuracy[q_name]["count"] += 1
            
            # For state queries, check if max belief matches true state
            if q_name == "State Query":
                if np.argmax(belief_history[i+1]) == true_states[i+1]:
                    query_accuracy[q_name]["correct"] += 1
        
        for q_type, stats in query_accuracy.items():
            if stats["count"] > 0:
                accuracy = stats["correct"] / stats["count"] if stats["count"] > 0 else 0
                f.write(f"  {q_type}: {accuracy:.2f} ({stats['correct']}/{stats['count']})\n")
    
    # Prepare visualizations list for the report
    visualizations = [
        "linguistic_context.png", 
        "pomdp_structure.png", 
        "initial_belief.png", 
        "belief_evolution.png", 
        "interaction_history.png",
        "dialog_visualization.png"
    ]
    if os.path.exists(animation_path):
        visualizations.append(os.path.basename(animation_path))
    
    # Prepare model info for the report
    model_info = {
        "Model type": "POMDP (Partially Observable Markov Decision Process)",
        "Number of states": f"{len(states)}",
        "Number of actions": f"{len(actions)}",
        "Number of observations": f"{len(observations)}",
        "Case function": "Addressable entity that responds to direct queries"
    }
    
    # Generate a report
    report_path = os.path.join(case_dir, "report.md")
    generate_report(
        case_name=Case.VOCATIVE.value,
        case_info=case_info,
        model_info=model_info,
        visualizations=visualizations,
        save_path=report_path,
        logger=logger
    )
    
    if logger:
        logger.info(f"Completed VOCATIVE case test with visualizations in {case_dir}")
    
    # Return the model for potential further testing
    return model 