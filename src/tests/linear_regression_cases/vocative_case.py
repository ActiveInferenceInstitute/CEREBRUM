#!/usr/bin/env python3
"""
VOCATIVE Case Test Module for CEREBRUM
Tests the VOCATIVE case in the linear regression context
"""

import os
import logging
import numpy as np
import matplotlib.pyplot as plt
import imageio
from matplotlib.animation import FuncAnimation
from scipy.stats import norm
import pandas as pd
import seaborn as sns
from sklearn.metrics import mean_squared_error, r2_score

from src.models.base import Case
from src.models.case_definitions import CaseDefinitions
from src.models.linear_regression import LinearRegressionModel
from src.utils.visualization import plot_case_linguistic_context
from src.utils.animation import ensure_scalar, save_animation

# Setup logging
logger = logging.getLogger("cerebrum-vocative-test")

def test_vocative_case(linear_test_data, output_dir):
    """
    Test VOCATIVE case: Communication between model components and data.
    
    In linguistics: Used to address someone directly.
    In regression: Used to examine direct communication between model parts and data.
    """
    # Get case definitions
    case_definitions = CaseDefinitions.get_all_cases()
    
    # Get case info for logging
    case_info = case_definitions[Case.VOCATIVE]
    logger.info(f"Testing {Case.VOCATIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(output_dir, "vocative")
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.VOCATIVE, linguistics_path)
    
    # Define animation output path
    animation_path = os.path.join(case_dir, "vocative_communication_animation.gif")
    
    # Get data
    X, y = linear_test_data
    
    # Fit a VOCATIVE case model - "addressing" components directly
    model = LinearRegressionModel(model_id=f"{Case.VOCATIVE.value}_model", case=Case.VOCATIVE)
    model.fit(X, y)
    
    # Extract parameters and ensure they are scalar values, not numpy arrays
    intercept = ensure_scalar(model._params['intercept'])
    coefficients = model._params['coefficients']
    
    # Handle coefficients - ensure we get a scalar value for the slope
    if isinstance(coefficients, np.ndarray):
        if coefficients.ndim > 0:
            slope = ensure_scalar(coefficients[0])
        else:
            slope = ensure_scalar(coefficients)
    else:
        slope = coefficients
        
    logger.info(f"Fitted VOCATIVE case model with parameters: {{'intercept': np.float64({intercept}), 'coefficients': array([{slope}])}}")
    
    # 1. Data points addressing the model (vocative visualization)
    vocative_path = os.path.join(case_dir, "data_addressing_model.png")
    
    # Create a visualization showing "communication" from data to model
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Generate predictions for visualization
    x_range = np.linspace(X.min() - 0.2, X.max() + 0.2, 100).reshape(-1, 1)
    y_pred_line = model.predict(x_range)
    
    # Plot the regression line
    ax.plot(x_range, y_pred_line, 'r-', linewidth=2, label='Model')
    
    # Plot the data points with "speech bubbles"
    ax.scatter(X, y, color='blue', alpha=0.7, label='Data Points')
    
    # Add arrows from data points to regression line (representing "addressing")
    for i in range(len(X)):
        # Check if arrays are 2D or 1D and index accordingly
        if len(X.shape) > 1:
            xi = X[i, 0]
        else:
            xi = X[i]
            
        if len(y.shape) > 1:
            yi = y[i, 0]
        else:
            yi = y[i]
        
        y_model = intercept + slope * xi
        
        # Calculate perpendicular point on the line for proper addressing
        dx = 1.0
        dy = slope * dx
        norm_factor = np.sqrt(dx**2 + dy**2)
        dx, dy = dx / norm_factor, dy / norm_factor
        
        # Distance from point to line
        dist = abs(yi - y_model) / np.sqrt(slope**2 + 1)
        
        # Adjust starting point to be at data point
        x_arrow_start = xi
        y_arrow_start = yi
        
        # Calculate end point on the line
        if yi > y_model:
            x_arrow_end = xi - dy * dist
            y_arrow_end = yi - dx * dist
        else:
            x_arrow_end = xi + dy * dist
            y_arrow_end = yi + dx * dist
            
        # Draw arrow
        ax.annotate("", xy=(x_arrow_end, y_arrow_end), xytext=(x_arrow_start, y_arrow_start),
                   arrowprops=dict(arrowstyle="->", color='gray', lw=1, alpha=0.6))
        
        # Add "speech bubble" text showing residual
        residual = yi - y_model
        bubble_text = f"{residual:.2f}"
        
        # Position the bubble based on the residual direction
        offset_y = 0.15 if residual > 0 else -0.15
        ax.text(xi + 0.05, yi + offset_y, bubble_text, 
                bbox=dict(boxstyle="round,pad=0.3", fc="white", ec="gray", alpha=0.7),
                ha='center', va='center', fontsize=9)
    
    # Style the visualization
    ax.set_title("VOCATIVE Case: Data Points Addressing the Model", fontsize=14)
    ax.set_xlabel("X", fontsize=12)
    ax.set_ylabel("Y", fontsize=12)
    ax.legend()
    ax.grid(True, alpha=0.3)
    
    # Add explanatory text
    vocative_text = (
        "VOCATIVE CASE VISUALIZATION\n\n"
        "In the VOCATIVE case, data points directly address the model.\n"
        "Each point communicates its residual value to the model line.\n"
        "This direct communication visualizes the 'conversation' between\n"
        "data and model, similar to how we might address someone by name.\n\n"
        f"Model equation: y = {intercept:.4f} + {slope:.4f}x"
    )
    
    ax.text(0.5, -0.15, vocative_text, ha='center', va='center', 
            transform=ax.transAxes, fontsize=11, 
            bbox=dict(facecolor='white', alpha=0.9, boxstyle='round,pad=0.5'))
    
    plt.tight_layout()
    plt.savefig(vocative_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # 2. Communication between model components
    components_path = os.path.join(case_dir, "model_component_communication.png")
    
    # Create visualization showing "communication" between model components
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Define model components with their positions
    components = {
        'X': (0.2, 0.5),
        'Slope': (0.5, 0.7),
        'Intercept': (0.5, 0.3),
        'Prediction': (0.8, 0.5)
    }
    
    # Draw components as nodes
    for name, (x, y) in components.items():
        circle = plt.Circle((x, y), 0.08, fc='skyblue', ec='blue', alpha=0.7)
        ax.add_artist(circle)
        ax.text(x, y, name, ha='center', va='center', fontweight='bold')
    
    # Draw connections with mathematical operations
    # X to Prediction
    ax.annotate("", xy=components['Prediction'], xytext=components['X'],
               arrowprops=dict(arrowstyle="->", color='black', lw=1.5, 
                              connectionstyle="arc3,rad=0.3"))
    
    # Operation text for X → Prediction
    midpoint_x1 = (components['X'][0] + components['Prediction'][0]) / 2
    midpoint_y1 = (components['X'][1] + components['Prediction'][1]) / 2
    ax.text(midpoint_x1, midpoint_y1 + 0.05, f"× {slope:.2f}", 
            ha='center', va='center', bbox=dict(fc='white', ec='gray', alpha=0.7))
    
    # X to Slope
    ax.annotate("", xy=components['Slope'], xytext=components['X'],
               arrowprops=dict(arrowstyle="->", color='black', lw=1.5))
    
    # Slope to Prediction
    ax.annotate("", xy=components['Prediction'], xytext=components['Slope'],
               arrowprops=dict(arrowstyle="->", color='black', lw=1.5))
    
    # Intercept to Prediction
    ax.annotate("", xy=components['Prediction'], xytext=components['Intercept'],
               arrowprops=dict(arrowstyle="->", color='black', lw=1.5))
    
    # Operation text for Intercept → Prediction
    midpoint_x2 = (components['Intercept'][0] + components['Prediction'][0]) / 2
    midpoint_y2 = (components['Intercept'][1] + components['Prediction'][1]) / 2
    ax.text(midpoint_x2, midpoint_y2 - 0.05, f"+ {intercept:.2f}", 
            ha='center', va='center', bbox=dict(fc='white', ec='gray', alpha=0.7))
    
    # Add speech bubbles for vocative communication
    # X "calling" slope
    ax.text(0.28, 0.6, "Hey Slope, multiply me!", 
            bbox=dict(boxstyle="round,pad=0.3", fc="#FFF9D9", ec="gold", alpha=0.8),
            ha='left', va='center', fontsize=9)
    
    # Slope "addressing" X
    ax.text(0.4, 0.73, f"I'll scale you by {slope:.2f}", 
            bbox=dict(boxstyle="round,pad=0.3", fc="#E3F4F4", ec="skyblue", alpha=0.8),
            ha='left', va='center', fontsize=9)
    
    # Intercept "calling" Prediction
    ax.text(0.6, 0.25, f"Add {intercept:.2f} to complete the prediction!", 
            bbox=dict(boxstyle="round,pad=0.3", fc="#F8E8EE", ec="pink", alpha=0.8),
            ha='center', va='center', fontsize=9)
    
    # Prediction response
    ax.text(0.9, 0.45, "I'm the final answer\nafter combining inputs", 
            bbox=dict(boxstyle="round,pad=0.3", fc="#E5F9DB", ec="lightgreen", alpha=0.8),
            ha='center', va='center', fontsize=9)
    
    # Remove axis ticks and spines
    ax.set_xlim(0, 1)
    ax.set_ylim(0, 1)
    ax.axis('off')
    
    # Add explanatory text
    title = "VOCATIVE Case: Communication Between Model Components"
    ax.set_title(title, fontsize=14, pad=20)
    
    explanation = (
        "The VOCATIVE case represents how components of the model address each other.\n"
        "Input data addresses the slope component for transformation.\n"
        "The intercept addresses the final prediction to be added.\n"
        "Each component 'calls out' to other components with specific operations."
    )
    
    ax.text(0.5, 0.05, explanation, ha='center', va='center', 
            transform=ax.transAxes, fontsize=11, 
            bbox=dict(facecolor='white', alpha=0.9, boxstyle='round,pad=0.5'))
    
    plt.tight_layout()
    plt.savefig(components_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # 3. Model addressing the data (residual analysis)
    addressing_path = os.path.join(case_dir, "model_addressing_data.png")
    
    # Generate predictions for the original data
    y_pred = model.predict(X)
    residuals = y - y_pred
    
    # Create visualization
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Sort data by X for better visualization
    indices = np.argsort(X.flatten())
    X_sorted = X[indices].flatten() if hasattr(X, 'shape') and len(X.shape) > 1 else np.array(X).flatten()[indices]
    
    # Handle y and y_pred based on their dimensions
    if hasattr(y, 'shape') and len(y.shape) > 1:
        y_sorted = y[indices].flatten()
    else:
        # If y is 1D array or a list
        y_arr = np.array(y).flatten()
        if len(y_arr) == len(indices):
            y_sorted = y_arr[indices]
        else:
            # Handle the case where dimensions don't match
            y_sorted = y_arr
    
    if hasattr(y_pred, 'shape') and len(y_pred.shape) > 1:
        y_pred_sorted = y_pred[indices].flatten()
    else:
        # If y_pred is 1D array or a list
        y_pred_arr = np.array(y_pred).flatten()
        if len(y_pred_arr) == len(indices):
            y_pred_sorted = y_pred_arr[indices]
        else:
            # Handle the case where dimensions don't match
            y_pred_sorted = y_pred_arr
    
    # Ensure arrays are the same length
    min_len = min(len(X_sorted), len(y_sorted), len(y_pred_sorted))
    X_sorted = X_sorted[:min_len]
    y_sorted = y_sorted[:min_len]
    y_pred_sorted = y_pred_sorted[:min_len]
    
    # Calculate residuals after sorting
    residuals_sorted = y_sorted - y_pred_sorted
    
    # Plot data and predictions
    ax.scatter(X_sorted, y_sorted, color='blue', alpha=0.7, label='Actual Data')
    ax.scatter(X_sorted, y_pred_sorted, color='red', alpha=0.7, label='Model Predictions')
    ax.plot(X_sorted, y_pred_sorted, 'r-', alpha=0.5)
    
    # Draw residual lines
    for i in range(len(X_sorted)):
        ax.plot([X_sorted[i], X_sorted[i]], [y_sorted[i], y_pred_sorted[i]], 
               'k-', alpha=0.3)
    
    # Add speech bubbles from model to data (model addressing the data)
    for i in range(min(10, len(X_sorted))):  # Limit to prevent overcrowding
        if i % 2 == 0:  # Only add to every other point for clarity
            resid = residuals_sorted[i]
            direction = "over" if resid < 0 else "under"
            
            # Calculate position for the bubble - halfway up the residual line
            x_bubble = X_sorted[i]
            y_bubble = (y_sorted[i] + y_pred_sorted[i]) / 2
            
            # Add model response 
            bubble_text = f"I'm {abs(resid):.2f} {direction} your true value!"
            
            # Offset bubble position to the right of the residual line
            ax.text(x_bubble + 0.15, y_bubble, bubble_text, 
                   bbox=dict(boxstyle="round,pad=0.3", fc="#E5F9DB", ec="lightgreen", alpha=0.8),
                   ha='left', va='center', fontsize=9)
            
            # Add an arrow from the model line to the bubble
            ax.annotate("", xy=(x_bubble + 0.14, y_bubble), 
                       xytext=(x_bubble, y_pred_sorted[i]),
                       arrowprops=dict(arrowstyle="->", color='green', lw=1, alpha=0.6))
    
    # Style the visualization
    ax.set_title("VOCATIVE Case: Model Addressing the Data Points", fontsize=14)
    ax.set_xlabel("X", fontsize=12)
    ax.set_ylabel("Y", fontsize=12)
    ax.legend()
    ax.grid(True, alpha=0.3)
    
    # Add explanatory text
    addressing_text = (
        "VOCATIVE CASE: MODEL ADDRESSING DATA\n\n"
        "In this visualization, the model directly addresses data points,\n"
        "telling them how much their predictions differ from actual values.\n"
        "This represents the vocative case where the model 'calls out' to\n"
        "each data point with information about their residuals."
    )
    
    ax.text(0.5, -0.15, addressing_text, ha='center', va='center', 
            transform=ax.transAxes, fontsize=11, 
            bbox=dict(facecolor='white', alpha=0.9, boxstyle='round,pad=0.5'))
    
    plt.tight_layout()
    plt.savefig(addressing_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # 4. Animation of communication in the model
    animation_path = os.path.join(case_dir, "vocative_communication.gif")
    
    # Create a function for animation frames
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Ensure X and y are properly formatted as arrays
    X_anim = np.array(X)
    y_anim = np.array(y) if not np.isscalar(y) else np.array([y])
    
    # Handle shape for comparison
    X_shape = X_anim.shape[0] if hasattr(X_anim, 'shape') else len(X_anim)
    y_shape = y_anim.shape[0] if hasattr(y_anim, 'shape') else len(y_anim)
    
    # Ensure arrays are the same size for animation
    if X_shape != y_shape:
        # Find minimum size
        min_size = min(X_shape, y_shape)
        X_anim = X_anim[:min_size]
        y_anim = y_anim[:min_size]
    
    # Calculate margins for plot limits
    if X_anim.size > 0:
        x_min, x_max = X_anim.min(), X_anim.max()
        x_margin = (x_max - x_min) * 0.2 if x_max > x_min else 1.0
    else:
        x_min, x_max, x_margin = -1, 1, 0.4

    if y_anim.size > 0:
        y_min, y_max = y_anim.min(), y_anim.max()
        y_margin = (y_max - y_min) * 0.2 if y_max > y_min else 1.0
    else:
        y_min, y_max, y_margin = -1, 1, 0.4
    
    # Plot data points and initial regression line
    scatter = ax.scatter(X_anim, y_anim, color='blue', alpha=0.7, label='Data')
    line, = ax.plot([], [], 'r-', linewidth=2, label='Model')
    residual_lines = [ax.plot([], [], 'k-', alpha=0.3)[0] for _ in range(len(X_anim))]
    
    # Create text elements for speech bubbles
    speech_bubbles = []
    for i in range(len(X_anim)):
        # Create a text object for each data point
        bubble = ax.text(0, 0, '', bbox=dict(boxstyle="round,pad=0.3", fc="white", 
                                           ec="gray", alpha=0.0), visible=False)
        speech_bubbles.append(bubble)
    
    # Model response bubble
    model_bubble = ax.text(0.5, 0.9, '', transform=ax.transAxes,
                          bbox=dict(boxstyle="round,pad=0.5", fc="#E3F4F4", ec="blue", alpha=0.0),
                          ha='center', va='center', fontsize=12, visible=False)
    
    # Set axis limits with safe values and expansion
    ax.set_xlim(x_min - x_margin, x_max + x_margin)
    ax.set_ylim(y_min - y_margin, y_max + y_margin)
    
    # Add labels and grid
    ax.set_title("VOCATIVE Case: Communication Animation", fontsize=14)
    ax.set_xlabel("X", fontsize=12)
    ax.set_ylabel("Y", fontsize=12)
    ax.grid(True, alpha=0.3)
    ax.legend()
    
    # Initialize empty scatter points for predicted values
    pred_scatter = ax.scatter([], [], color='red', alpha=0.7, label='Predictions')
    
    def init():
        # Initialize the animation
        line.set_data([], [])
        for residual_line in residual_lines:
            residual_line.set_data([], [])
        model_bubble.set_visible(False)
        for bubble in speech_bubbles:
            bubble.set_visible(False)
        pred_scatter.set_offsets(np.empty((0, 2)))
        return [line, *residual_lines, model_bubble, *speech_bubbles, pred_scatter]
    
    def animate(i):
        if i < 20:
            # Phase 1: Draw the regression line gradually
            progress = i / 19
            x_line = np.linspace(x_min - x_margin, x_min + (x_max - x_min + 2*x_margin) * progress, 100)
            y_line = intercept + slope * x_line
            line.set_data(x_line, y_line)
            
            # Hide other elements
            for residual_line in residual_lines:
                residual_line.set_data([], [])
            model_bubble.set_visible(False)
            for bubble in speech_bubbles:
                bubble.set_visible(False)
            pred_scatter.set_offsets(np.empty((0, 2)))
            
            if i == 19:
                # At the end of phase 1, show model introduction
                model_bubble.set_text("Hello data points! I'm your linear model.")
                model_bubble.set_bbox(dict(boxstyle="round,pad=0.5", fc="#E3F4F4", ec="blue", alpha=0.9))
                model_bubble.set_visible(True)
                
        elif i < 25:
            # Phase 2: Model introduction
            # Keep the regression line complete
            x_line = np.linspace(x_min - x_margin, x_max + x_margin, 100)
            y_line = intercept + slope * x_line
            line.set_data(x_line, y_line)
            
            # Keep model bubble visible
            model_bubble.set_visible(True)
            
        elif i < 25 + len(X_anim):
            # Phase 3: Add predictions one by one
            point_idx = i - 25
            
            # Keep the regression line complete
            x_line = np.linspace(x_min - x_margin, x_max + x_margin, 100)
            y_line = intercept + slope * x_line
            line.set_data(x_line, y_line)
            
            # Get current data point
            if len(X_anim.shape) > 1:
                x_val = X_anim[point_idx, 0]
            else:
                x_val = X_anim[point_idx]
                
            if len(y_anim.shape) > 1:
                y_true = y_anim[point_idx, 0]
            else:
                y_true = y_anim[point_idx]
                
            y_pred_val = intercept + slope * x_val
            
            # Update the model speech bubble
            model_bubble.set_text(f"Point at x={x_val:.2f}, I predict y={y_pred_val:.2f}")
            model_bubble.set_visible(True)
            
            # Show predictions up to current point
            pred_data = []
            for j in range(point_idx + 1):
                if len(X_anim.shape) > 1:
                    x_j = X_anim[j, 0]
                else:
                    x_j = X_anim[j]
                y_j_pred = intercept + slope * x_j
                pred_data.append([x_j, y_j_pred])
            
            pred_scatter.set_offsets(np.array(pred_data))
            
            # Draw residual line for current point
            residual_lines[point_idx].set_data([x_val, x_val], [y_true, y_pred_val])
            
            # Show speech bubble for current data point
            residual = y_true - y_pred_val
            bubble_text = f"I'm actually {y_true:.2f}, off by {residual:.2f}"
            speech_bubbles[point_idx].set_position((x_val + 0.15, y_true))
            speech_bubbles[point_idx].set_text(bubble_text)
            speech_bubbles[point_idx].set_bbox(dict(boxstyle="round,pad=0.3", fc="#F8E8EE", ec="pink", alpha=0.9))
            speech_bubbles[point_idx].set_visible(True)
            
        elif i < 25 + len(X_anim) + 10:
            # Phase 4: All predictions shown, model summarizes
            # Keep the regression line complete
            x_line = np.linspace(x_min - x_margin, x_max + x_margin, 100)
            y_line = intercept + slope * x_line
            line.set_data(x_line, y_line)
            
            # Show all predictions
            pred_data = []
            for j in range(len(X_anim)):
                if len(X_anim.shape) > 1:
                    x_j = X_anim[j, 0]
                else:
                    x_j = X_anim[j]
                y_j_pred = intercept + slope * x_j
                pred_data.append([x_j, y_j_pred])
            
            pred_scatter.set_offsets(np.array(pred_data))
            
            # Draw all residual lines
            for j, residual_line in enumerate(residual_lines):
                if len(X_anim.shape) > 1:
                    x_val = X_anim[j, 0]
                else:
                    x_val = X_anim[j]
                    
                if len(y_anim.shape) > 1:
                    y_true = y_anim[j, 0]
                else:
                    y_true = y_anim[j]
                    
                y_pred_val = intercept + slope * x_val
                residual_line.set_data([x_val, x_val], [y_true, y_pred_val])
            
            # Calculate residuals manually
            residuals = []
            for j in range(len(X_anim)):
                if len(X_anim.shape) > 1:
                    x_j = X_anim[j, 0]
                else:
                    x_j = X_anim[j]
                    
                if len(y_anim.shape) > 1:
                    y_true_j = y_anim[j, 0]
                else:
                    y_true_j = y_anim[j]
                    
                y_pred_j = intercept + slope * x_j
                residuals.append(y_true_j - y_pred_j)
            
            mse = np.mean(np.array(residuals)**2)
            
            # Update model speech bubble with summary
            model_bubble.set_text(f"Thanks for the feedback! My MSE is {mse:.4f}")
            model_bubble.set_visible(True)
            
            # Hide individual speech bubbles
            for bubble in speech_bubbles:
                bubble.set_visible(False)
                
        else:
            # Phase 5: Final frame - keep everything visible but change model message
            # Keep the regression line complete
            x_line = np.linspace(x_min - x_margin, x_max + x_margin, 100)
            y_line = intercept + slope * x_line
            line.set_data(x_line, y_line)
            
            # Show all predictions
            pred_data = []
            for j in range(len(X_anim)):
                if len(X_anim.shape) > 1:
                    x_j = X_anim[j, 0]
                else:
                    x_j = X_anim[j]
                y_j_pred = intercept + slope * x_j
                pred_data.append([x_j, y_j_pred])
            
            pred_scatter.set_offsets(np.array(pred_data))
            
            # Draw all residual lines
            for j, residual_line in enumerate(residual_lines):
                if len(X_anim.shape) > 1:
                    x_val = X_anim[j, 0]
                else:
                    x_val = X_anim[j]
                    
                if len(y_anim.shape) > 1:
                    y_true = y_anim[j, 0]
                else:
                    y_true = y_anim[j]
                    
                y_pred_val = intercept + slope * x_val
                residual_line.set_data([x_val, x_val], [y_true, y_pred_val])
            
            # Final model message
            model_bubble.set_text(f"My equation is y = {intercept:.2f} + {slope:.2f}x")
            model_bubble.set_visible(True)
            
            # Add a few representative speech bubbles back
            for j in range(min(5, len(speech_bubbles))):
                idx = j * len(speech_bubbles) // 5
                if idx < len(speech_bubbles):
                    if len(X_anim.shape) > 1:
                        x_val = X_anim[idx, 0]
                    else:
                        x_val = X_anim[idx]
                        
                    if len(y_anim.shape) > 1:
                        y_true = y_anim[idx, 0]
                    else:
                        y_true = y_anim[idx]
                        
                    y_pred_val = intercept + slope * x_val
                    residual = y_true - y_pred_val
                    bubble_text = f"Residual: {residual:.2f}"
                    speech_bubbles[idx].set_position((x_val + 0.15, y_true))
                    speech_bubbles[idx].set_text(bubble_text)
                    speech_bubbles[idx].set_bbox(dict(boxstyle="round,pad=0.3", fc="#F8E8EE", ec="pink", alpha=0.9))
                    speech_bubbles[idx].set_visible(True)
        
        return [line, *residual_lines, model_bubble, *speech_bubbles, pred_scatter]
    
    # Create animation with fewer frames to avoid issues
    num_frames = 50  # Reduced frame count
    
    # Create and save animation
    logger.info(f"Creating VOCATIVE case animation with {num_frames} frames")
    anim = FuncAnimation(fig, animate, frames=num_frames, init_func=init, blit=True)
    animation_success = save_animation(anim, animation_path, fps=2, dpi=120)
    
    # If animation fails, create a series of static images instead
    if not animation_success:
        logger.warning(f"Failed to create animation at {animation_path}. Creating static images instead.")
        # Create a directory for static frames
        frames_dir = os.path.join(case_dir, "dialogue_frames")
        os.makedirs(frames_dir, exist_ok=True)
        
        # Save key frames as static images
        for i in [0, 10, 20, 30, 40]:
            frame_path = os.path.join(frames_dir, f"frame_{i:02d}.png")
            # Temporarily clear the axis
            plt.figure(fig.number)
            animate(i)
            plt.savefig(frame_path, dpi=120, bbox_inches='tight')
            logger.info(f"Saved static frame to {frame_path}")
            
        # Add a note about static frames
        with open(os.path.join(case_dir, "animation_note.txt"), 'w') as f:
            f.write("Animation could not be generated as a GIF. Please see the 'dialogue_frames' directory for static images.")
            f.write("\n\nTo view the animation sequence, open the numbered frame images in order.")
    
    plt.close(fig)
    
    # 5. Document all results for this case
    with open(os.path.join(case_dir, "vocative_results.txt"), 'w') as f:
        f.write(f"VOCATIVE CASE RESULTS\n")
        f.write(f"====================\n\n")
        
        f.write(f"Linguistic meaning: {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n")
        f.write(f"Regression context: {case_info['regression_context']}\n\n")
        
        f.write(f"Model Parameters:\n")
        f.write(f"- Intercept: {intercept:.6f}\n")
        f.write(f"- Slope: {slope:.6f}\n")
        f.write(f"- Model Formula: y = {intercept:.6f} + {slope:.6f}x\n\n")
        
        # Calculate residuals and statistics
        y_pred = model.predict(X)
        y_pred_array = np.array(y_pred)
        y_array = np.array(y)
        residuals = y_array - y_pred_array
        mse = np.mean(residuals**2)
        rmse = np.sqrt(mse)
        mae = np.mean(np.abs(residuals))
        
        # Handle potential division by zero in R²
        ss_total = np.sum((y_array - np.mean(y_array))**2)
        if ss_total > 0:
            r_squared = 1 - np.sum(residuals**2) / ss_total
        else:
            r_squared = 0  # Default if total sum of squares is zero
        
        f.write(f"Communication Analysis (VOCATIVE Case):\n")
        f.write(f"- Data-to-Model Communication: Residuals represent 'messages' from data to model\n")
        f.write(f"- Mean Squared Error (MSE): {mse:.6f}\n")
        f.write(f"- Root Mean Squared Error (RMSE): {rmse:.6f}\n")
        f.write(f"- Mean Absolute Error (MAE): {mae:.6f}\n")
        f.write(f"- R-squared: {r_squared:.6f}\n\n")
        
        f.write(f"Linguistic Formula Example:\n")
        f.write(f"{case_info['example']}\n\n")
        
        f.write(f"VOCATIVE Case Interpretation:\n")
        f.write(f"In the VOCATIVE case, we explore how model components 'address' each other\n")
        f.write(f"and how data and model communicate. Key insights include:\n\n")
        
        f.write(f"1. Data-to-Model Communication:\n")
        f.write(f"   - Each data point 'addresses' the model with its true value\n")
        f.write(f"   - The model responds with its prediction\n")
        f.write(f"   - The residual represents the 'conversation' between data and model\n\n")
        
        f.write(f"2. Model Component Communication:\n")
        f.write(f"   - Input features address the coefficients (slope)\n")
        f.write(f"   - Coefficients address the prediction function\n")
        f.write(f"   - Intercept addresses the prediction independently\n\n")
    
    return {
        'model': model,
        'intercept': intercept,
        'slope': slope,
        'case_dir': case_dir
    } 