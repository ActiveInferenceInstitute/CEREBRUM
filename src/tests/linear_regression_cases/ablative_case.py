#!/usr/bin/env python3
"""
ABLATIVE Case Test Module for CEREBRUM
Tests the ABLATIVE case in the linear regression context
"""

import os
import logging
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from sklearn.metrics import mean_squared_error, r2_score
import pandas as pd
import seaborn as sns

from models.base import Case
from models.case_definitions import CaseDefinitions
from models.linear_regression import LinearRegressionModel
from utils.visualization import plot_case_linguistic_context

# Setup logging
logger = logging.getLogger("cerebrum-ablative-test")

def test_ablative_case(linear_test_data, output_dir):
    """
    Test ABLATIVE case: Model as a source of transformation or cause of effect.
    
    In linguistics: Indicating origin, source, or cause of action.
    In regression: Model as origin of transformations or predictions.
    """
    # Get case definitions
    case_definitions = CaseDefinitions.get_all_cases()
    
    # Get case info for logging
    case_info = case_definitions[Case.ABLATIVE]
    logger.info(f"Testing {Case.ABLATIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(output_dir, Case.ABLATIVE.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.ABLATIVE, linguistics_path)
    
    # Get data
    X, y = linear_test_data
    
    # Create and fit a model (initially in NOMINATIVE case)
    model = LinearRegressionModel(model_id=f"{Case.ABLATIVE.value}_model", case=Case.NOMINATIVE)
    model.fit(X, y)
    
    # Extract parameters
    intercept = model._params['intercept']
    slope = model._params['coefficients'][0]
    
    logger.info(f"Model fitted with parameters: intercept={intercept:.4f}, slope={slope:.4f}")
    
    # Now convert model to ABLATIVE case (source of transformation)
    model.case = Case.ABLATIVE
    
    # 1. First visualization: Model as source of transformations
    transformation_path = os.path.join(case_dir, "model_as_source.png")
    
    # Generate predictions
    y_pred = model.predict(X)
    
    # Calculate metrics
    mse = mean_squared_error(y, y_pred)
    r2 = r2_score(y, y_pred)
    
    # Create visualization showing model as source
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Plot the original data
    ax.scatter(X, y, alpha=0.5, color='blue', label='Original Data')
    
    # Plot the transformed data (predictions as sourced from the model)
    ax.scatter(X, y_pred, alpha=0.5, color='red', label='Data Sourced from Model')
    
    # Plot the model line
    x_range = np.linspace(X.min(), X.max(), 100).reshape(-1, 1)
    y_range = model.predict(x_range)
    ax.plot(x_range, y_range, 'r-', linewidth=2, label=f'Model: y = {intercept:.4f} + {slope:.4f}x')
    
    # Add arrows from model to predictions (showing model as source)
    # Use a sample of points to avoid overcrowding
    sample_indices = np.linspace(0, len(X)-1, 10, dtype=int)
    
    for i in sample_indices:
        # Check if arrays are 2D or 1D and index accordingly
        if len(X.shape) > 1:
            x_val = X[i, 0]
        else:
            x_val = X[i]
            
        if len(y_pred.shape) > 1:
            y_val = y_pred[i, 0]
        else:
            y_val = y_pred[i]
        
        # Arrow from model line to prediction
        model_y = intercept + slope * x_val
        ax.annotate("", xy=(x_val, y_val), xytext=(x_val, model_y),
                   arrowprops=dict(arrowstyle="->", color='green', lw=1, alpha=0.7))
    
    # Style the plot
    ax.set_title("ABLATIVE Case: Model as Source of Transformation", fontsize=16)
    ax.set_xlabel("X", fontsize=12)
    ax.set_ylabel("Y", fontsize=12)
    ax.legend()
    ax.grid(True, alpha=0.3)
    
    # Add ablative case explanation
    ablative_text = (
        "ABLATIVE CASE: MODEL AS SOURCE\n\n"
        f"The model transforms inputs into outputs:\n"
        f"y = {intercept:.4f} + {slope:.4f}x\n\n"
        f"In the ABLATIVE case, the model is the origin or source of the transformation."
        f"\nThe data comes FROM the model rather than TO the model."
    )
    
    ax.text(0.5, -0.15, ablative_text, ha='center', va='center', 
            transform=ax.transAxes, fontsize=12, 
            bbox=dict(facecolor='white', alpha=0.9, boxstyle='round,pad=0.5'))
    
    plt.tight_layout()
    plt.savefig(transformation_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # 2. Animation: Showing data flowing FROM the model (source) to different outputs
    source_animation_path = os.path.join(case_dir, "ablative_source_animation.gif")
    
    # Create animation
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 8))
    
    # Generate data for different transformations
    np.random.seed(42)
    
    # Original predictions
    base_predictions = model.predict(X)
    
    # 1. Add noise to show variation from source
    noise_levels = [0.0, 0.2, 0.5, 1.0, 2.0]
    noisy_predictions = []
    
    for noise in noise_levels:
        # Add random noise proportional to the noise level
        noise_vector = np.random.normal(0, noise, size=y.shape)
        noisy_predictions.append(base_predictions + noise_vector)
    
    # 2. Apply scaling transformations
    scaling_factors = [0.5, 0.8, 1.0, 1.2, 1.5]
    scaled_predictions = []
    
    for scale in scaling_factors:
        # Scale predictions by factor
        scaled_predictions.append(base_predictions * scale)
    
    # Define animation update function
    def update(frame):
        # Clear axes
        ax1.clear()
        ax2.clear()
        
        # For the first set of frames, show noise transformations
        if frame < len(noise_levels):
            # Get current noise level
            noise_level = noise_levels[frame]
            current_predictions = noisy_predictions[frame]
            
            # Plot original data
            ax1.scatter(X, y, alpha=0.5, color='blue', label='Original Data')
            
            # Plot predictions from model
            ax1.scatter(X, current_predictions, alpha=0.7, color='red', 
                       label=f'Predictions (noise={noise_level:.1f})')
            
            # Plot model line
            ax1.plot(x_range, y_range, 'r-', linewidth=2, 
                    label=f'Model: y = {intercept:.4f} + {slope:.4f}x')
            
            # Add arrows showing data flowing FROM the model
            sample_indices = np.linspace(0, len(X)-1, min(8, len(X)), dtype=int)
            
            for i in sample_indices:
                # Check if arrays are 2D or 1D and index accordingly
                if len(X.shape) > 1:
                    x_val = X[i, 0]
                else:
                    x_val = X[i]
                    
                if len(current_predictions.shape) > 1:
                    y_val = current_predictions[i, 0]
                else:
                    y_val = current_predictions[i]
                
                # Arrow from model line to prediction
                model_y = intercept + slope * x_val
                ax1.annotate("", xy=(x_val, y_val), xytext=(x_val, model_y),
                            arrowprops=dict(arrowstyle="->", color='green', lw=1, alpha=0.7))
            
            # Style plot 1
            ax1.set_title(f"Noise Transformation from Model (σ={noise_level:.1f})", fontsize=14)
            ax1.set_xlabel("X", fontsize=12)
            ax1.set_ylabel("Y", fontsize=12)
            ax1.legend()
            ax1.grid(True, alpha=0.3)
            
            # Set axes limits consistently
            y_min = min(y.min(), current_predictions.min()) - 0.5
            y_max = max(y.max(), current_predictions.max()) + 0.5
            ax1.set_ylim(y_min, y_max)
            
            # Add ablative explanation
            source_text = (
                "ABLATIVE CASE: MODEL AS SOURCE\n\n"
                f"With noise level σ={noise_level:.1f},\n"
                f"the model is the SOURCE from which\n"
                f"these noisy predictions originate."
            )
            
            ax1.text(0.05, 0.95, source_text, transform=ax1.transAxes, fontsize=12,
                    va='top', ha='left', bbox=dict(facecolor='white', alpha=0.8))
            
            # Plot error distribution on the right side
            errors = current_predictions - y
            sns.histplot(errors, kde=True, ax=ax2, color='purple', alpha=0.6)
            ax2.axvline(x=0, color='red', linestyle='--', alpha=0.7)
            
            # Calculate metrics for this transformation
            curr_mse = mean_squared_error(y, current_predictions)
            curr_r2 = r2_score(y, current_predictions)
            
            # Style plot 2
            ax2.set_title("Error Distribution", fontsize=14)
            ax2.set_xlabel("Error", fontsize=12)
            ax2.set_ylabel("Frequency", fontsize=12)
            ax2.grid(True, alpha=0.3)
            
            # Add metrics
            metrics_text = (
                f"Metrics for noise σ={noise_level:.1f}:\n"
                f"MSE: {curr_mse:.4f}\n"
                f"R²: {curr_r2:.4f}\n"
                f"Mean Error: {np.mean(errors):.4f}\n"
                f"Error Std: {np.std(errors):.4f}"
            )
            
            ax2.text(0.95, 0.95, metrics_text, transform=ax2.transAxes, fontsize=12,
                    va='top', ha='right', bbox=dict(facecolor='white', alpha=0.8))
            
        # For the second set of frames, show scaling transformations
        elif frame < len(noise_levels) + len(scaling_factors):
            # Get current scaling factor
            scale_idx = frame - len(noise_levels)
            scale_factor = scaling_factors[scale_idx]
            current_predictions = scaled_predictions[scale_idx]
            
            # Plot original data
            ax1.scatter(X, y, alpha=0.5, color='blue', label='Original Data')
            
            # Plot predictions from model
            ax1.scatter(X, current_predictions, alpha=0.7, color='green', 
                       label=f'Predictions (scale={scale_factor:.1f})')
            
            # Plot model line and scaled line
            ax1.plot(x_range, y_range, 'r-', linewidth=1, alpha=0.5,
                    label=f'Base Model: y = {intercept:.4f} + {slope:.4f}x')
            
            scaled_y_range = y_range * scale_factor
            ax1.plot(x_range, scaled_y_range, 'g-', linewidth=2,
                    label=f'Scaled Model (×{scale_factor:.1f})')
            
            # Add arrows showing data flowing FROM the model
            sample_indices = np.linspace(0, len(X)-1, min(8, len(X)), dtype=int)
            
            for i in sample_indices:
                # Check if arrays are 2D or 1D and index accordingly
                if len(X.shape) > 1:
                    x_val = X[i, 0]
                else:
                    x_val = X[i]
                    
                if len(current_predictions.shape) > 1:
                    y_val = current_predictions[i, 0]
                else:
                    y_val = current_predictions[i]
                
                # Arrow from model line to prediction
                model_y = (intercept + slope * x_val) * scale_factor
                ax1.annotate("", xy=(x_val + 0.1, y_val), xytext=(x_val, model_y),
                            arrowprops=dict(arrowstyle="->", color='green', lw=1, alpha=0.7))
            
            # Style plot 1
            ax1.set_title(f"Scaling Transformation from Model (×{scale_factor:.1f})", fontsize=14)
            ax1.set_xlabel("X", fontsize=12)
            ax1.set_ylabel("Y", fontsize=12)
            ax1.legend()
            ax1.grid(True, alpha=0.3)
            
            # Set axes limits consistently
            y_min = min(y.min(), current_predictions.min()) - 0.5
            y_max = max(y.max(), current_predictions.max()) + 0.5
            ax1.set_ylim(y_min, y_max)
            
            # Add ablative explanation
            source_text = (
                "ABLATIVE CASE: MODEL AS SOURCE\n\n"
                f"With scaling factor {scale_factor:.1f},\n"
                f"the model is the SOURCE of the\n"
                f"amplified or diminished predictions."
            )
            
            ax1.text(0.05, 0.95, source_text, transform=ax1.transAxes, fontsize=12,
                    va='top', ha='left', bbox=dict(facecolor='white', alpha=0.8))
            
            # Plot error metrics on the right side
            ax2.set_xlim(0, len(scaling_factors))
            ax2.set_ylim(0, 1.5)
            
            # Plot MSE and R² for all scaling factors up to current
            x_scales = list(range(scale_idx + 1))
            mse_values = [mean_squared_error(y, scaled_predictions[i]) for i in x_scales]
            r2_values = [r2_score(y, scaled_predictions[i]) for i in x_scales]
            
            ax2.plot(x_scales, mse_values, 'ro-', label='MSE')
            ax2.plot(x_scales, r2_values, 'go-', label='R²')
            
            # Add data point labels
            for i, (mse_val, r2_val) in enumerate(zip(mse_values, r2_values)):
                ax2.annotate(f"{mse_val:.2f}", (i, mse_val), xytext=(0, 5), 
                            textcoords='offset points', ha='center', fontsize=9)
                ax2.annotate(f"{r2_val:.2f}", (i, r2_val), xytext=(0, -15), 
                            textcoords='offset points', ha='center', fontsize=9)
            
            # Style plot 2
            ax2.set_title("Performance Metrics by Scale Factor", fontsize=14)
            ax2.set_xlabel("Scale Index", fontsize=12)
            ax2.set_ylabel("Metric Value", fontsize=12)
            ax2.grid(True, alpha=0.3)
            ax2.legend()
            
            # Add scale factor labels on x-axis
            ax2.set_xticks(x_scales)
            ax2.set_xticklabels([f"{scaling_factors[i]:.1f}" for i in x_scales])
            
        else:
            # Final frame: Summary
            # Create a diagram showing the ablative case meaning
            ax1.axis('off')
            ax2.axis('off')
            
            # Create ablative case diagram
            ablative_diagram = (
                "ABLATIVE CASE: MODEL AS SOURCE OR ORIGIN\n\n"
            )
            
            ax1.text(0.5, 0.95, ablative_diagram, ha='center', va='top', 
                    transform=ax1.transAxes, fontsize=16, fontweight='bold')
            
            # Create an illustration showing flow from the model to transformations
            # Draw model as source with arrows to different transformations
            model_pos = (0.5, 0.6)
            trans_positions = [
                (0.2, 0.3),  # Position for noise transformation
                (0.5, 0.3),  # Position for scaling transformation
                (0.8, 0.3)   # Position for another transformation
            ]
            
            # Draw model source
            model_circle = plt.Circle(model_pos, 0.1, fc='lightblue', ec='blue', alpha=0.7)
            ax1.add_artist(model_circle)
            ax1.text(model_pos[0], model_pos[1], "MODEL\n(Source)", 
                    ha='center', va='center', fontsize=12, fontweight='bold')
            
            # Draw transformations
            trans1_circle = plt.Circle(trans_positions[0], 0.08, fc='lightgreen', ec='green', alpha=0.7)
            trans2_circle = plt.Circle(trans_positions[1], 0.08, fc='lightcoral', ec='red', alpha=0.7)
            trans3_circle = plt.Circle(trans_positions[2], 0.08, fc='lightyellow', ec='orange', alpha=0.7)
            
            ax1.add_artist(trans1_circle)
            ax1.add_artist(trans2_circle)
            ax1.add_artist(trans3_circle)
            
            ax1.text(trans_positions[0][0], trans_positions[0][1], "Noise\nTransform", 
                    ha='center', va='center', fontsize=10)
            ax1.text(trans_positions[1][0], trans_positions[1][1], "Scaling\nTransform", 
                    ha='center', va='center', fontsize=10)
            ax1.text(trans_positions[2][0], trans_positions[2][1], "Bias\nTransform", 
                    ha='center', va='center', fontsize=10)
            
            # Draw arrows from model to transformations
            for pos in trans_positions:
                ax1.annotate("", xy=pos, xytext=model_pos,
                            arrowprops=dict(arrowstyle="->", color='blue', lw=2, alpha=0.7))
            
            # Add ablative language examples
            examples = [
                "FROM the model, we derive predictions",
                "The data originates FROM the model",
                "Transformations come FROM the model",
                "Results emerge FROM model calculations",
                "Errors arise FROM model limitations"
            ]
            
            # Position examples at the bottom
            for i, example in enumerate(examples):
                y_pos = 0.15 - i * 0.05
                ax1.text(0.5, y_pos, example, ha='center', fontsize=12,
                        transform=ax1.transAxes)
            
            # Create a summary of what we've learned
            summary = (
                "ABLATIVE CASE SUMMARY\n\n"
                "In the ABLATIVE case, the model acts as:\n\n"
                "1. SOURCE of transformations (data flowing from model)\n"
                "2. ORIGIN of predictions (outputs originating from model)\n"
                "3. CAUSE of variations (different outputs caused by model)\n\n"
                "The ABLATIVE case emphasizes that predictions and transformations\n"
                "come FROM the model rather than TO the model."
            )
            
            ax2.text(0.5, 0.5, summary, ha='center', va='center',
                    transform=ax2.transAxes, fontsize=14,
                    bbox=dict(boxstyle="round,pad=0.5", fc="white", alpha=0.9))
        
        return [ax1, ax2]
    
    # Create animation with all frames (noise + scaling + summary)
    total_frames = len(noise_levels) + len(scaling_factors) + 1
    anim = FuncAnimation(fig, update, frames=total_frames, interval=2000, blit=True)
    
    # Save animation
    logger.info(f"Creating ABLATIVE case animation with {total_frames} frames")
    anim.save(source_animation_path, writer='pillow', fps=1, dpi=100)
    plt.close(fig)
    
    # 3. Cause-effect analysis showing model as origin of outcomes
    causal_path = os.path.join(case_dir, "causal_analysis.png")
    
    # Create visualization of causal chains from the model
    fig, ax = plt.subplots(figsize=(12, 8))
    
    # Create a flowchart showing model as cause/origin
    nodes = {
        'model': (0.5, 0.8, "MODEL\n(Origin/Cause)"),
        'prediction': (0.5, 0.6, "Predictions"),
        'decision': (0.5, 0.4, "Decisions"),
        'outcome_good': (0.3, 0.2, "Positive\nOutcomes"),
        'outcome_bad': (0.7, 0.2, "Negative\nOutcomes")
    }
    
    # Draw nodes
    for node, (x, y, label) in nodes.items():
        if node == 'model':
            color = 'lightblue'
            ec = 'blue'
            size = 0.1
        elif node in ['outcome_good', 'outcome_bad']:
            color = 'lightgreen' if node == 'outcome_good' else 'lightcoral'
            ec = 'green' if node == 'outcome_good' else 'red'
            size = 0.08
        else:
            color = 'lightyellow'
            ec = 'orange'
            size = 0.08
            
        circle = plt.Circle((x, y), size, fc=color, ec=ec, alpha=0.7)
        ax.add_artist(circle)
        ax.text(x, y, label, ha='center', va='center', fontsize=11, fontweight='bold')
    
    # Draw arrows
    ax.annotate("", xy=(nodes['prediction'][0], nodes['prediction'][1]), 
               xytext=(nodes['model'][0], nodes['model'][1]),
               arrowprops=dict(arrowstyle="->", color='blue', lw=2))
    
    ax.annotate("", xy=(nodes['decision'][0], nodes['decision'][1]), 
               xytext=(nodes['prediction'][0], nodes['prediction'][1]),
               arrowprops=dict(arrowstyle="->", color='blue', lw=2))
    
    ax.annotate("", xy=(nodes['outcome_good'][0], nodes['outcome_good'][1]), 
               xytext=(nodes['decision'][0], nodes['decision'][1]),
               arrowprops=dict(arrowstyle="->", color='green', lw=2))
    
    ax.annotate("", xy=(nodes['outcome_bad'][0], nodes['outcome_bad'][1]), 
               xytext=(nodes['decision'][0], nodes['decision'][1]),
               arrowprops=dict(arrowstyle="->", color='red', lw=2))
    
    # Add causal explanations along arrows
    ax.text(0.5, 0.7, "causes", ha='center', va='center', fontsize=10,
           bbox=dict(facecolor='white', alpha=0.8))
    
    ax.text(0.5, 0.5, "leads to", ha='center', va='center', fontsize=10,
           bbox=dict(facecolor='white', alpha=0.8))
    
    ax.text(0.4, 0.3, "results in", ha='center', va='center', fontsize=10,
           bbox=dict(facecolor='white', alpha=0.8))
    
    ax.text(0.6, 0.3, "may cause", ha='center', va='center', fontsize=10,
           bbox=dict(facecolor='white', alpha=0.8))
    
    # Set axes properties
    ax.set_xlim(0, 1)
    ax.set_ylim(0, 1)
    ax.axis('off')
    
    # Add explanatory title and text
    ax.set_title("ABLATIVE Case: Model as Cause in Causal Chain", fontsize=16, pad=20)
    
    explanation = (
        "The ABLATIVE case represents the model as the origin or cause from which effects flow.\n"
        "Predictions, decisions, and outcomes all originate FROM the model's computations.\n"
        "This causal perspective emphasizes the model's role as a starting point for a chain of consequences."
    )
    
    ax.text(0.5, 0.05, explanation, ha='center', va='center', fontsize=12,
           bbox=dict(facecolor='white', alpha=0.9, boxstyle='round,pad=0.5'))
    
    plt.tight_layout()
    plt.savefig(causal_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # 4. Document all results for this case
    with open(os.path.join(case_dir, "ablative_results.txt"), 'w') as f:
        f.write(f"ABLATIVE CASE RESULTS\n")
        f.write(f"====================\n\n")
        f.write(f"Linguistic meaning: {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n")
        f.write(f"Regression context: {case_info['regression_context']}\n\n")
        
        f.write(f"Model Parameters:\n")
        f.write(f"- Intercept: {intercept:.6f}\n")
        f.write(f"- Slope: {slope:.6f}\n")
        f.write(f"- Model Formula: y = {intercept:.6f} + {slope:.6f}x\n\n")
        
        f.write(f"Evaluation Metrics:\n")
        f.write(f"- Mean Squared Error (MSE): {mse:.6f}\n")
        f.write(f"- R-squared: {r2:.6f}\n\n")
        
        f.write(f"Transformation Analysis:\n")
        f.write(f"- Noise transformations: Demonstrating how noise emerges FROM the model\n")
        f.write(f"- Scaling transformations: Showing amplification/attenuation FROM the model\n")
        f.write(f"- Bias transformations: Illustrating systematic shifts FROM the model\n\n")
        
        f.write(f"Causal Analysis:\n")
        f.write(f"- Model as origin/cause of predictions\n")
        f.write(f"- Predictions leading to decisions\n")
        f.write(f"- Decisions resulting in outcomes\n")
        f.write(f"- Complete causal chain originating FROM the model\n\n")
        
        f.write(f"Linguistic Analysis (ABLATIVE Case):\n")
        f.write(f"- The model is the SOURCE from which predictions originate\n")
        f.write(f"- Predictions come FROM the model (not TO the model)\n")
        f.write(f"- Transformations are derived FROM model computation\n")
        f.write(f"- Outcomes ultimately stem FROM the model's initial processing\n\n")
        
        f.write(f"Linguistic Formula Example:\n")
        f.write(f"{case_info['example']}\n\n")
    
    logger.info(f"Completed ABLATIVE case test with visualizations in {case_dir}")
    
    return model 