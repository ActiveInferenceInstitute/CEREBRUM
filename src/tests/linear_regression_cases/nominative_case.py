#!/usr/bin/env python3
"""
NOMINATIVE Case Test Module for CEREBRUM
Tests the NOMINATIVE case in the linear regression context
"""

import os
import logging
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import pandas as pd
from sklearn.metrics import mean_squared_error, r2_score

from src.models.base import Case
from src.models.case_definitions import CaseDefinitions
from src.models.linear_regression import LinearRegressionModel
from src.utils.visualization import plot_case_linguistic_context
from src.utils.animation import save_animation

# Setup logging
logger = logging.getLogger("cerebrum-nominative-test")

def test_nominative_case(linear_test_data, output_dir):
    """
    Test NOMINATIVE case: Model actively fitting parameters to data.
    
    In linguistics: The subject performing the action.
    In regression: Model as an active agent performing parameter estimation.
    """
    # Get case definitions
    case_definitions = CaseDefinitions.get_all_cases()
    
    # Get case info for logging
    case_info = case_definitions[Case.NOMINATIVE]
    logger.info(f"Testing {Case.NOMINATIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(output_dir, "nominative")
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.NOMINATIVE, linguistics_path)
    
    # Get data
    X, y = linear_test_data
    
    # Create a NOMINATIVE case model - actively fitting parameters
    model = LinearRegressionModel(model_id=f"{Case.NOMINATIVE.value}_model", case=Case.NOMINATIVE)
    
    # 1. First, create a plain visualization of the model actively fitting data
    active_fit_path = os.path.join(case_dir, "active_fitting.png")
    
    # Fit the model
    model.fit(X, y)
    
    # Extract fitted parameters
    intercept = model._params['intercept']
    slope = model._params['coefficients'][0]
    
    # Generate predictions
    y_pred = model.predict(X)
    mse = mean_squared_error(y, y_pred)
    r2 = r2_score(y, y_pred)
    
    # Create a visualization showing the model actively fitting data
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Plot actual data points
    ax.scatter(X, y, alpha=0.7, color='blue', label='Data Points')
    
    # Plot the fitted line
    x_range = np.linspace(X.min(), X.max(), 100).reshape(-1, 1)
    y_range = model.predict(x_range)
    ax.plot(x_range, y_range, 'r-', linewidth=2, label=f'Fitted Model: y = {intercept:.4f} + {slope:.4f}x')
    
    # Add error lines to visualize fit
    for i in range(len(X)):
        # Check if arrays are 2D or 1D and index accordingly
        if len(X.shape) > 1:
            x_val = X[i, 0]
        else:
            x_val = X[i]
            
        if len(y.shape) > 1:
            y_true = y[i, 0]
            y_predicted = y_pred[i, 0]
        else:
            y_true = y[i]
            y_predicted = y_pred[i]
            
        ax.plot([x_val, x_val], [y_true, y_predicted], 'k--', alpha=0.3)
    
    # Add text annotation explaining the active nature of the model
    active_text = (
        "NOMINATIVE CASE: MODEL ACTIVELY FITTING DATA\n\n"
        f"The model acts as the subject, actively finding parameters:\n"
        f"Slope = {slope:.4f}, Intercept = {intercept:.4f}\n"
        f"MSE = {mse:.4f}, R² = {r2:.4f}\n\n"
        f"In the NOMINATIVE case, the model performs the action of parameter estimation,"
        f"\nsearching for the optimal values that minimize error."
    )
    
    ax.text(0.5, -0.15, active_text, ha='center', va='center', 
            transform=ax.transAxes, fontsize=12, 
            bbox=dict(facecolor='white', alpha=0.9, boxstyle='round,pad=0.5'))
    
    # Style the plot
    ax.set_title("NOMINATIVE Case: Model Actively Fitting Data", fontsize=16)
    ax.set_xlabel("X", fontsize=12)
    ax.set_ylabel("Y", fontsize=12)
    ax.legend()
    ax.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig(active_fit_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # 2. Now, create an animation showing the iterative gradient descent process
    # This demonstrates the "active agent" nature of the model in the NOMINATIVE case
    
    # Define a function to manually implement gradient descent to show the activity
    def gradient_descent(X, y, learning_rate=0.01, iterations=30):
        m = X.shape[0]
        # Initialize parameters randomly
        theta = np.random.randn(2, 1) * 0.5
        
        # Prepare design matrix (add column of 1s for intercept)
        X_b = np.c_[np.ones((m, 1)), X]
        
        # Storage for parameters and metrics history
        theta_history = [theta.copy()]
        cost_history = []
        
        # Run gradient descent iterations
        for i in range(iterations):
            # Compute predictions
            y_pred = X_b.dot(theta)
            
            # Compute gradients
            gradients = (2/m) * X_b.T.dot(y_pred - y)
            
            # Update parameters
            theta = theta - learning_rate * gradients
            
            # Store parameters
            theta_history.append(theta.copy())
            
            # Compute cost (MSE)
            cost = np.mean((y_pred - y) ** 2)
            cost_history.append(cost)
        
        return theta_history, cost_history
    
    # Run gradient descent
    theta_history, cost_history = gradient_descent(X, y, learning_rate=0.01, iterations=30)
    
    # Create an animation showing the gradient descent process
    animation_path = os.path.join(case_dir, "gradient_descent_animation.gif")
    
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 8))
    
    # Set explicit figure background color for better animation
    fig.patch.set_facecolor('white')
    
    # Sort X for smooth line plotting
    X_sorted = np.sort(X, axis=0)
    
    # Plot settings
    ax1.set_xlabel("X")
    ax1.set_ylabel("Y")
    ax1.set_title("Model Fitting Process (NOMINATIVE Case)")
    ax1.grid(True, alpha=0.3)
    
    ax2.set_xlabel("Iteration")
    ax2.set_ylabel("Cost (MSE)")
    ax2.set_title("Cost Function")
    ax2.grid(True, alpha=0.3)
    
    # Plot data
    scatter = ax1.scatter(X, y, color='blue', alpha=0.6, label='Data')
    
    # Set fixed axis limits for better animation
    x_min, x_max = X.min()-0.5, X.max()+0.5
    y_min, y_max = y.min()-0.5, y.max()+0.5
    ax1.set_xlim(x_min, x_max)
    ax1.set_ylim(y_min, y_max)
    
    # Set fixed cost axis limits
    cost_min, cost_max = min(cost_history), max(cost_history)
    margin = (cost_max - cost_min) * 0.1
    ax2.set_xlim(0, len(cost_history))
    ax2.set_ylim(max(0, cost_min - margin), cost_max + margin)
    
    # Initialize animated elements
    line, = ax1.plot([], [], 'r-', lw=2, label='Model')
    cost_line, = ax2.plot([], [], 'g-', lw=2)
    
    # Initialize text elements for parameter updates
    theta_text = ax1.text(0.02, 0.95, '', transform=ax1.transAxes,
                     bbox=dict(facecolor='white', alpha=0.8))
    
    # Initialize annotation for "active model" representation
    agent_annotation = ax1.annotate("", xy=(0, 0), xytext=(0, 0),
                               arrowprops=dict(arrowstyle="->", color='green', lw=2, alpha=0))
    
    # Add legends for both axes
    ax1.legend(loc='lower right')
    
    # Animation initialization function
    def init():
        line.set_data([], [])
        cost_line.set_data([], [])
        theta_text.set_text("")
        agent_annotation.set_alpha(0)
        return line, cost_line, theta_text, agent_annotation
    
    # Animation update function with safeguards
    def update(frame):
        # Ensure frame index is valid
        frame_idx = min(frame, len(theta_history) - 1)
        
        # Get current parameters
        theta = theta_history[frame_idx]
        intercept, slope = theta[0, 0], theta[1, 0]
        
        # Update model line
        x_line = np.linspace(x_min, x_max, 100)
        y_line = intercept + slope * x_line
        line.set_data(x_line, y_line)
        
        # Update cost plot
        if frame_idx > 0:
            iterations = range(frame_idx + 1)
            costs = cost_history[:frame_idx + 1]
            cost_line.set_data(iterations, costs)
        
        # Update parameters text
        cost_val = cost_history[frame_idx] if frame_idx < len(cost_history) else cost_history[-1]
        theta_text.set_text(f"Iteration: {frame_idx}\nIntercept: {intercept:.4f}\nSlope: {slope:.4f}\nMSE: {cost_val:.4f}")
        
        # Update agent annotation - make the model "move" around the parameter space
        if frame_idx > 0 and frame_idx < len(theta_history):
            prev_idx = max(0, frame_idx-1)
            prev_theta = theta_history[prev_idx]
            curr_theta = theta
            
            # Project this movement onto the data space for visualization
            mid_x = np.mean(X)
            prev_y = prev_theta[0, 0] + prev_theta[1, 0] * mid_x
            curr_y = curr_theta[0, 0] + curr_theta[1, 0] * mid_x
            
            # Make arrow indicating model movement direction
            agent_annotation.xy = (mid_x, curr_y)
            agent_annotation.xytext = (mid_x, prev_y)
            agent_annotation.set_alpha(0.7)
        else:
            agent_annotation.set_alpha(0)
        
        return line, cost_line, theta_text, agent_annotation
    
    # Create and save animation
    num_frames = min(20, len(theta_history))  # Reduce number of frames for better compatibility
    frame_indices = np.linspace(0, len(theta_history)-1, num_frames, dtype=int)
    
    anim = FuncAnimation(fig, update, frames=frame_indices, 
                      init_func=init, blit=True)
    
    # Save using our improved animation utility
    logger.info(f"Creating NOMINATIVE case gradient descent animation with {num_frames} frames")
    animation_success = save_animation(anim, animation_path, fps=3, dpi=120)
    
    if not animation_success:
        logger.warning(f"Failed to create animation at {animation_path}. Creating a static visualization instead.")
        # Create a final static frame as fallback
        static_path = os.path.join(case_dir, "gradient_descent_static.png")
        
        # Get final parameters
        final_theta = theta_history[-1]
        final_intercept, final_slope = final_theta[0, 0], final_theta[1, 0]
        
        # Create static visualization
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 8))
        
        # Plot data and final model
        ax1.scatter(X, y, color='blue', alpha=0.6, label='Data')
        x_line = np.linspace(X.min()-0.5, X.max()+0.5, 100)
        y_line = final_intercept + final_slope * x_line
        ax1.plot(x_line, y_line, 'r-', lw=2, label='Final Model')
        
        # Plot cost history
        ax2.plot(range(len(cost_history)), cost_history, 'g-', lw=2)
        
        # Add text and annotations
        ax1.text(0.02, 0.95, f"Final Parameters:\nIntercept: {final_intercept:.4f}\nSlope: {final_slope:.4f}\nMSE: {cost_history[-1]:.4f}",
                transform=ax1.transAxes, bbox=dict(facecolor='white', alpha=0.5))
        
        # Add warning about animation failure
        fig.suptitle("Gradient Descent Optimization (Static Version - Animation Failed)", fontsize=14)
        
        # Set labels and grid
        ax1.set_xlabel("X")
        ax1.set_ylabel("Y")
        ax1.set_title("Final Model Fit")
        ax1.grid(True, alpha=0.3)
        ax1.legend()
        
        ax2.set_xlabel("Iteration")
        ax2.set_ylabel("Cost (MSE)")
        ax2.set_title("Cost Function History")
        ax2.grid(True, alpha=0.3)
        
        plt.tight_layout()
        plt.savefig(static_path, dpi=100, bbox_inches='tight')
        plt.close(fig)
    
    plt.close(fig)
    
    # 3. Create a visualization showing the "subject -> verb -> object" structure
    # This emphasizes the linguistic meaning of the NOMINATIVE case
    linguistic_vis_path = os.path.join(case_dir, "nominative_linguistic.png")
    
    fig, ax = plt.subplots(figsize=(12, 8))
    ax.set_xlim(0, 10)
    ax.set_ylim(0, 10)
    ax.axis('off')
    
    # Draw "subject -> verb -> object" structure
    subject_pos = (2, 7)
    verb_pos = (5, 7)
    object_pos = (8, 7)
    
    # Draw nodes
    model_circle = plt.Circle(subject_pos, 1.2, fc='lightblue', ec='blue', alpha=0.7)
    verb_circle = plt.Circle(verb_pos, 0.8, fc='lightgreen', ec='green', alpha=0.7)
    data_circle = plt.Circle(object_pos, 1.2, fc='lightyellow', ec='orange', alpha=0.7)
    
    ax.add_artist(model_circle)
    ax.add_artist(verb_circle)
    ax.add_artist(data_circle)
    
    # Add text
    ax.text(subject_pos[0], subject_pos[1], "MODEL\n(Subject)", ha='center', va='center', fontweight='bold')
    ax.text(verb_pos[0], verb_pos[1], "FITS\n(Verb)", ha='center', va='center', fontweight='bold')
    ax.text(object_pos[0], object_pos[1], "DATA\n(Object)", ha='center', va='center', fontweight='bold')
    
    # Draw arrows
    ax.annotate("", xy=(verb_pos[0]-0.9, verb_pos[1]), xytext=(subject_pos[0]+1.3, subject_pos[1]),
               arrowprops=dict(arrowstyle="->", color='blue', lw=2))
    
    ax.annotate("", xy=(object_pos[0]-1.3, object_pos[1]), xytext=(verb_pos[0]+0.9, verb_pos[1]),
               arrowprops=dict(arrowstyle="->", color='green', lw=2))
    
    # Add examples of model's active actions
    actions = [
        "Model COMPUTES predictions",
        "Model MINIMIZES error",
        "Model UPDATES parameters",
        "Model PERFORMS optimization",
        "Model LEARNS from data",
        "Model PRODUCES coefficients",
        "Model FITS regression line"
    ]
    
    # Position actions below the main diagram
    for i, action in enumerate(actions):
        y_pos = 4 - (i * 0.6)
        ax.text(5, y_pos, action, ha='center', fontsize=12)
    
    # Add title and explanation
    ax.text(5, 9, "NOMINATIVE CASE STRUCTURE", ha='center', fontsize=16, fontweight='bold')
    
    explanation = (
        "The NOMINATIVE case represents the model as the subject performing an action.\n"
        "In regression, the model actively works to estimate parameters and fit the data.\n"
        "This mirrors the linguistic subject-verb-object structure, where the model is the acting agent."
    )
    
    ax.text(5, 1.5, explanation, ha='center', fontsize=12, 
           bbox=dict(facecolor='white', alpha=0.9, boxstyle='round,pad=0.5'))
    
    plt.tight_layout()
    plt.savefig(linguistic_vis_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # 4. Document all results for this case
    with open(os.path.join(case_dir, "nominative_results.txt"), 'w') as f:
        f.write(f"NOMINATIVE CASE RESULTS\n")
        f.write(f"======================\n\n")
        f.write(f"Linguistic meaning: {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n")
        f.write(f"Regression context: {case_info['regression_context']}\n\n")
        
        f.write(f"Model Parameters (from active fitting):\n")
        f.write(f"- Intercept: {intercept:.6f}\n")
        f.write(f"- Slope: {slope:.6f}\n")
        f.write(f"- Model Formula: y = {intercept:.6f} + {slope:.6f}x\n\n")
        
        f.write(f"Performance Metrics:\n")
        f.write(f"- Mean Squared Error (MSE): {mse:.6f}\n")
        f.write(f"- R-squared: {r2:.6f}\n\n")
        
        f.write(f"Gradient Descent Analysis:\n")
        f.write(f"- Initial cost: {cost_history[0]:.6f}\n")
        f.write(f"- Final cost: {cost_history[-1]:.6f}\n")
        f.write(f"- Reduction: {(1 - cost_history[-1]/cost_history[0])*100:.2f}%\n")
        f.write(f"- Iterations: {len(cost_history)}\n\n")
        
        f.write(f"Linguistic Analysis (NOMINATIVE Case):\n")
        f.write(f"- The model acts as the SUBJECT performing the action\n")
        f.write(f"- Active verbs: Computes, Measures, Adjusts, Reduces, Fits, Learns\n")
        f.write(f"- Objects: Parameters, Errors, Predictions, Data\n\n")
        
        f.write(f"Linguistic Formula Example:\n")
        f.write(f"{case_info['example']}\n\n")
    
    logger.info(f"Completed NOMINATIVE case test with visualizations in {case_dir}")
    
    return model 