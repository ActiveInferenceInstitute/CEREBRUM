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
from sklearn.metrics import mean_squared_error, r2_score

from src.models.base import Case
from src.models.case_definitions import CaseDefinitions
from src.models.linear_regression import LinearRegressionModel
from src.utils.visualization import plot_case_linguistic_context

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
    case_dir = os.path.join(output_dir, Case.NOMINATIVE.value.lower())
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
    
    # Define animation update function
    def update(frame):
        # Clear the axes
        ax1.clear()
        ax2.clear()
        
        # Get current parameters
        theta = theta_history[frame]
        intercept_val = theta[0, 0]
        slope_val = theta[1, 0]
        
        # Plot data
        ax1.scatter(X, y, alpha=0.7, color='blue', label='Data Points')
        
        # Plot current line
        x_line = np.linspace(X.min(), X.max(), 100).reshape(-1, 1)
        X_line_b = np.c_[np.ones((100, 1)), x_line]
        y_line = X_line_b.dot(theta)
        ax1.plot(x_line, y_line, 'r-', linewidth=2, 
                label=f'Iteration {frame}: y = {intercept_val:.4f} + {slope_val:.4f}x')
        
        # Add active verbs to show model is the subject performing actions
        iter_text = (
            f"Iteration {frame}:\n"
            f"Model COMPUTES predictions\n"
            f"Model MEASURES error\n"
            f"Model ADJUSTS parameters\n"
            f"Model REDUCES cost\n"
        )
        
        ax1.text(0.02, 0.98, iter_text, transform=ax1.transAxes, fontsize=10,
                va='top', ha='left', bbox=dict(facecolor='white', alpha=0.8))
        
        # Style plot 1
        ax1.set_title("NOMINATIVE Case: Model Actively Fitting Data", fontsize=14)
        ax1.set_xlabel("X", fontsize=12)
        ax1.set_ylabel("Y", fontsize=12)
        ax1.legend()
        ax1.grid(True, alpha=0.3)
        
        # Plot cost history
        if frame > 0:
            iterations = list(range(frame))
            costs = cost_history[:frame]
            ax2.plot(iterations, costs, 'b-', linewidth=2)
            ax2.scatter(iterations, costs, color='blue')
            
            # Highlight the current point
            if frame > 0:
                ax2.scatter([frame-1], [cost_history[frame-1]], color='red', s=100, zorder=10)
                
                # Add arrow to show the direction of progress
                if frame > 1:
                    ax2.annotate('', xy=(frame-1, cost_history[frame-1]), 
                                xytext=(frame-2, cost_history[frame-2]),
                                arrowprops=dict(arrowstyle='->', color='green', lw=2))
        
        # Add cost info
        if frame > 0:
            cost_info = (
                f"Current cost: {cost_history[frame-1]:.4f}\n"
                f"Initial cost: {cost_history[0]:.4f}\n"
                f"Improvement: {(1 - cost_history[frame-1]/cost_history[0])*100:.1f}%"
            )
            ax2.text(0.02, 0.98, cost_info, transform=ax2.transAxes, fontsize=10,
                    va='top', ha='left', bbox=dict(facecolor='white', alpha=0.8))
        
        # Style plot 2
        ax2.set_title("Cost Reduction over Iterations", fontsize=14)
        ax2.set_xlabel("Iteration", fontsize=12)
        ax2.set_ylabel("Cost (MSE)", fontsize=12)
        ax2.grid(True, alpha=0.3)
        
        if frame > 0:
            # Try to set y-axis limits reasonably
            y_min = min(cost_history[:frame])
            y_max = max(cost_history[:frame])
            buffer = (y_max - y_min) * 0.1
            ax2.set_ylim([max(0, y_min - buffer), y_max + buffer])
        
        return [ax1, ax2]
    
    # Create animation
    anim = FuncAnimation(fig, update, frames=len(theta_history), interval=200, blit=True)
    
    # Save animation
    logger.info(f"Creating NOMINATIVE case animation with {len(theta_history)} frames")
    anim.save(animation_path, writer='pillow', fps=3, dpi=100)
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