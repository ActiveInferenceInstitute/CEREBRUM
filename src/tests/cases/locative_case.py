#!/usr/bin/env python3
"""
LOCATIVE Case Test Module for CEREBRUM
Tests the LOCATIVE case in the linear regression context
"""

import os
import logging
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

from src.models.base import Case
from src.models.case_definitions import CaseDefinitions
from src.models.linear_regression import LinearRegressionModel
from src.utils.visualization import plot_case_linguistic_context

# Setup logging
logger = logging.getLogger("cerebrum-locative-test")

def test_locative_case(linear_test_data, output_dir):
    """
    Test LOCATIVE case: Position in the parameter space.
    
    In linguistics: Used to denote location or place.
    In regression: Exploring where the model is in parameter space.
    """
    # Get case definitions
    case_definitions = CaseDefinitions.get_all_cases()
    
    # Get case info for logging
    case_info = case_definitions[Case.LOCATIVE]
    logger.info(f"Testing {Case.LOCATIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(output_dir, Case.LOCATIVE.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.LOCATIVE, linguistics_path)
    
    # Get data
    X, y = linear_test_data
    
    # Create a LOCATIVE case model
    model = LinearRegressionModel(model_id=f"{Case.LOCATIVE.value}_model", case=Case.LOCATIVE)
    
    # Generate a parameter space grid to explore
    n_grid = 20
    slope_range = np.linspace(-2, 3, n_grid)
    intercept_range = np.linspace(-2, 3, n_grid)
    
    # Calculate error landscape for different parameter values
    error_landscape = np.zeros((n_grid, n_grid))
    
    for i, slope in enumerate(slope_range):
        for j, intercept in enumerate(intercept_range):
            # Set model parameters manually
            model._model.coef_ = np.array([slope])
            model._model.intercept_ = intercept
            model._is_fitted = True
            
            # Calculate predictions
            y_pred = model.predict(X)
            
            # Calculate MSE
            mse = np.mean((y - y_pred) ** 2)
            error_landscape[i, j] = mse
    
    # Find optimal parameters
    min_idx = np.unravel_index(np.argmin(error_landscape), error_landscape.shape)
    optimal_slope = slope_range[min_idx[0]]
    optimal_intercept = intercept_range[min_idx[1]]
    
    logger.info(f"Optimal parameters found: slope={optimal_slope:.4f}, intercept={optimal_intercept:.4f}")
    
    # Visualize parameter space
    param_space_path = os.path.join(case_dir, "parameter_space.png")
    
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Create contour plot of error landscape
    contour = ax.contourf(slope_range, intercept_range, error_landscape.T, 
                         levels=20, cmap='viridis', alpha=0.7)
    
    # Add color bar
    cbar = plt.colorbar(contour, ax=ax)
    cbar.set_label('Mean Squared Error')
    
    # Mark the optimal parameters
    ax.scatter(optimal_slope, optimal_intercept, color='red', s=100, 
             marker='*', label=f'Optimal parameters\nSlope: {optimal_slope:.4f}\nIntercept: {optimal_intercept:.4f}')
    
    # Fit the model using proper method to get true params
    model_true = LinearRegressionModel(model_id="true_model", case=Case.NOMINATIVE)
    model_true.fit(X, y)
    true_slope = model_true._model.coef_[0]
    true_intercept = model_true._model.intercept_
    
    # Mark the true parameters
    ax.scatter(true_slope, true_intercept, color='green', s=100, 
             marker='o', label=f'True parameters\nSlope: {true_slope:.4f}\nIntercept: {true_intercept:.4f}')
    
    # Set up plot labels and title
    ax.set_xlabel('Slope')
    ax.set_ylabel('Intercept')
    ax.set_title(f'LOCATIVE Case: Parameter Space for Linear Regression\nWhere is the model located?')
    ax.legend(loc='upper right')
    
    # Add grid lines
    ax.grid(True, alpha=0.3, linestyle='--')
    
    plt.tight_layout()
    plt.savefig(param_space_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # Create an animation of navigating the parameter space
    parameter_path = os.path.join(case_dir, "parameter_navigation.gif")
    
    # Generate a path from a random start point to the optimal parameters
    np.random.seed(42)  # For reproducibility
    
    # Random starting point within the grid
    start_idx = (np.random.randint(0, n_grid), np.random.randint(0, n_grid))
    start_slope = slope_range[start_idx[0]]
    start_intercept = intercept_range[start_idx[1]]
    
    # Path finding with gradient descent (simplified)
    n_steps = 15
    path_slopes = np.zeros(n_steps)
    path_intercepts = np.zeros(n_steps)
    
    current_slope = start_slope
    current_intercept = start_intercept
    
    learning_rate = 0.2
    
    for step in range(n_steps):
        path_slopes[step] = current_slope
        path_intercepts[step] = current_intercept
        
        # Small perturbations to calculate gradient
        delta = 0.01
        
        # Calculate current MSE
        model._model.coef_ = np.array([current_slope])
        model._model.intercept_ = current_intercept
        y_pred = model.predict(X)
        current_mse = np.mean((y - y_pred) ** 2)
        
        # Calculate gradient for slope
        model._model.coef_ = np.array([current_slope + delta])
        y_pred = model.predict(X)
        slope_plus_mse = np.mean((y - y_pred) ** 2)
        
        # Calculate gradient for intercept
        model._model.coef_ = np.array([current_slope])
        model._model.intercept_ = current_intercept + delta
        y_pred = model.predict(X)
        intercept_plus_mse = np.mean((y - y_pred) ** 2)
        
        # Compute gradients
        slope_gradient = (slope_plus_mse - current_mse) / delta
        intercept_gradient = (intercept_plus_mse - current_mse) / delta
        
        # Update parameters
        current_slope -= learning_rate * slope_gradient
        current_intercept -= learning_rate * intercept_gradient
        
        # Ensure parameters stay within our grid
        current_slope = np.clip(current_slope, slope_range.min(), slope_range.max())
        current_intercept = np.clip(current_intercept, intercept_range.min(), intercept_range.max())
    
    # Create animation
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 8))
    
    # Function to update the plot for each frame
    def update(frame):
        # Clear axes
        ax1.clear()
        ax2.clear()
        
        # Current parameters
        current_slope = path_slopes[frame]
        current_intercept = path_intercepts[frame]
        
        # Set model parameters
        model._model.coef_ = np.array([current_slope])
        model._model.intercept_ = current_intercept
        
        # Calculate predictions and error
        y_pred = model.predict(X)
        mse = np.mean((y - y_pred) ** 2)
        r2 = 1 - np.sum((y - y_pred) ** 2) / np.sum((y - np.mean(y)) ** 2)
        
        # Left plot: Parameter space
        # Create contour plot of error landscape
        contour = ax1.contourf(slope_range, intercept_range, error_landscape.T, 
                              levels=20, cmap='viridis', alpha=0.7)
        
        # Plot path so far
        ax1.plot(path_slopes[:frame+1], path_intercepts[:frame+1], 'r-', linewidth=2)
        
        # Mark current position
        ax1.scatter(current_slope, current_intercept, color='red', s=150, 
                  marker='o', label=f'Current\nSlope: {current_slope:.4f}\nIntercept: {current_intercept:.4f}')
        
        # Mark optimal position
        ax1.scatter(optimal_slope, optimal_intercept, color='green', s=100, 
                  marker='*', label=f'Optimal\nSlope: {optimal_slope:.4f}\nIntercept: {optimal_intercept:.4f}')
        
        # Mark starting position
        ax1.scatter(start_slope, start_intercept, color='blue', s=100, 
                  marker='o', label=f'Start\nSlope: {start_slope:.4f}\nIntercept: {start_intercept:.4f}')
        
        # Add grid lines
        ax1.grid(True, alpha=0.3, linestyle='--')
        
        # Set up plot labels and title
        ax1.set_xlabel('Slope')
        ax1.set_ylabel('Intercept')
        ax1.set_title('LOCATIVE Case: Parameter Space Navigation')
        ax1.legend(loc='upper right')
        
        # Right plot: Model fit with current parameters
        # Plot data points
        ax2.scatter(X, y, color='blue', alpha=0.6, label='Data')
        
        # Plot prediction line
        x_range = np.linspace(X.min(), X.max(), 100)
        y_range = model._model.intercept_ + model._model.coef_[0] * x_range
        ax2.plot(x_range, y_range, 'r-', linewidth=2, 
                label=f'Model: y = {current_intercept:.4f} + {current_slope:.4f}x')
        
        # Add MSE and R² information
        ax2.text(0.05, 0.95, f'MSE: {mse:.4f}\nR²: {r2:.4f}', 
                transform=ax2.transAxes, fontsize=12,
                bbox=dict(facecolor='white', alpha=0.7))
        
        # Set up plot labels and title
        ax2.set_xlabel('X')
        ax2.set_ylabel('y')
        ax2.set_title('Current Model Fit')
        ax2.legend(loc='upper left')
        ax2.grid(True, alpha=0.3)
        
        # Return the artists that need to be redrawn
        return [ax1, ax2]
    
    # Create and save the animation
    anim = FuncAnimation(fig, update, frames=n_steps, blit=True)
    anim.save(parameter_path, writer='pillow', fps=3, dpi=100)
    plt.close(fig)
    
    # Create a zoomed-in visualization of the parameter space vicinity around the optimal parameters
    vicinity_path = os.path.join(case_dir, "parameter_vicinity.png")
    
    # Define a smaller range around the optimal parameters
    zoom_factor = 0.3
    slope_vicinity = np.linspace(
        optimal_slope - zoom_factor, 
        optimal_slope + zoom_factor, 
        n_grid
    )
    intercept_vicinity = np.linspace(
        optimal_intercept - zoom_factor, 
        optimal_intercept + zoom_factor, 
        n_grid
    )
    
    # Calculate error landscape for the vicinity
    vicinity_error = np.zeros((n_grid, n_grid))
    
    for i, slope in enumerate(slope_vicinity):
        for j, intercept in enumerate(intercept_vicinity):
            # Set model parameters manually
            model._model.coef_ = np.array([slope])
            model._model.intercept_ = intercept
            
            # Calculate predictions
            y_pred = model.predict(X)
            
            # Calculate MSE
            mse = np.mean((y - y_pred) ** 2)
            vicinity_error[i, j] = mse
    
    # Visualize the vicinity
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Create contour plot of error landscape
    contour = ax.contourf(slope_vicinity, intercept_vicinity, vicinity_error.T, 
                         levels=20, cmap='viridis', alpha=0.7)
    
    # Add color bar
    cbar = plt.colorbar(contour, ax=ax)
    cbar.set_label('Mean Squared Error')
    
    # Mark the optimal parameters
    ax.scatter(optimal_slope, optimal_intercept, color='red', s=150, 
             marker='*', label=f'Optimal parameters\nSlope: {optimal_slope:.4f}\nIntercept: {optimal_intercept:.4f}')
    
    # Mark the true parameters if they are within the vicinity
    if (slope_vicinity.min() <= true_slope <= slope_vicinity.max() and 
        intercept_vicinity.min() <= true_intercept <= intercept_vicinity.max()):
        ax.scatter(true_slope, true_intercept, color='green', s=100, 
                 marker='o', label=f'True parameters\nSlope: {true_slope:.4f}\nIntercept: {true_intercept:.4f}')
    
    # Set up plot labels and title
    ax.set_xlabel('Slope')
    ax.set_ylabel('Intercept')
    ax.set_title('LOCATIVE Case: Parameter Space Vicinity\nZoomed-in view around optimal parameters')
    ax.legend(loc='upper right')
    
    # Add grid lines
    ax.grid(True, alpha=0.3, linestyle='--')
    
    plt.tight_layout()
    plt.savefig(vicinity_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # Document all results for this case
    with open(os.path.join(case_dir, "locative_results.txt"), 'w') as f:
        f.write(f"LOCATIVE CASE RESULTS\n")
        f.write(f"====================\n\n")
        
        f.write(f"Linguistic meaning: {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n")
        f.write(f"Regression context: {case_info['regression_context']}\n\n")
        
        f.write(f"Parameter Space Analysis:\n")
        f.write(f"- Range explored: Slope [{slope_range.min()}, {slope_range.max()}], ")
        f.write(f"Intercept [{intercept_range.min()}, {intercept_range.max()}]\n")
        f.write(f"- Optimal parameters: Slope = {optimal_slope:.6f}, Intercept = {optimal_intercept:.6f}\n")
        f.write(f"- True parameters: Slope = {true_slope:.6f}, Intercept = {true_intercept:.6f}\n")
        f.write(f"- Difference: Slope = {abs(optimal_slope - true_slope):.6f}, ")
        f.write(f"Intercept = {abs(optimal_intercept - true_intercept):.6f}\n\n")
        
        f.write(f"Path Analysis:\n")
        f.write(f"- Starting point: Slope = {start_slope:.6f}, Intercept = {start_intercept:.6f}\n")
        f.write(f"- Number of steps: {n_steps}\n")
        f.write(f"- Final position: Slope = {path_slopes[-1]:.6f}, Intercept = {path_intercepts[-1]:.6f}\n")
        f.write(f"- Euclidean distance to optimal: ")
        
        final_distance = np.sqrt((path_slopes[-1] - optimal_slope)**2 + 
                                 (path_intercepts[-1] - optimal_intercept)**2)
        f.write(f"{final_distance:.6f}\n\n")
        
        f.write(f"Linguistic Formula Example:\n")
        f.write(f"{case_info['example']}\n\n")
    
    logger.info(f"Completed LOCATIVE case test with visualizations in {case_dir}")
    
    # Fit the model at the optimal parameters for return
    model._model.coef_ = np.array([optimal_slope])
    model._model.intercept_ = optimal_intercept
    model._is_fitted = True
    model._params = {
        'intercept': optimal_intercept,
        'coefficients': model._model.coef_
    }
    
    return model 