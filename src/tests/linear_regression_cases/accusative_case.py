#!/usr/bin/env python3
"""
ACCUSATIVE Case Test Module for CEREBRUM
Tests the ACCUSATIVE case in the linear regression context
"""

import os
import logging
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import seaborn as sns
from sklearn.metrics import mean_squared_error, r2_score, mean_absolute_error
from sklearn.model_selection import KFold

from src.models.base import Case
from src.models.case_definitions import CaseDefinitions
from src.models.linear_regression import LinearRegressionModel
from src.utils.animation import save_animation
from src.utils.visualization import plot_case_linguistic_context
from src.utils.array_utils import (
    validate_regression_data, get_xy_values, create_residual_lines_data,
    safe_metrics_calculation, ensure_1d, ensure_2d
)

# Prevent interactive display
plt.ioff()

# Setup logging
logger = logging.getLogger("cerebrum-accusative-test")

def test_accusative_case(linear_test_data, output_dir):
    """
    Test ACCUSATIVE case: Model as the object undergoing evaluation.
    
    In linguistics: The direct object receiving the action.
    In regression: Model as the object being evaluated, validated, or tested.
    """
    # Get case definitions
    case_definitions = CaseDefinitions.get_all_cases()
    
    # Get case info for logging
    case_info = case_definitions[Case.ACCUSATIVE]
    logger.info(f"Testing {Case.ACCUSATIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(output_dir, "accusative")
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.ACCUSATIVE, linguistics_path)
    
    # Get data and validate
    X, y = linear_test_data
    X, y = validate_regression_data(X, y)
    
    # Create and fit a model (initially in NOMINATIVE case)
    model = LinearRegressionModel(model_id=f"{Case.ACCUSATIVE.value}_model", case=Case.NOMINATIVE)
    model.fit(X, y)
    
    # Extract parameters
    intercept = float(model._params['intercept'])
    slope = float(model._params['coefficients'][0])
    
    logger.info(f"Model fitted with parameters: intercept={intercept:.4f}, slope={slope:.4f}")
    
    # Now convert model to ACCUSATIVE case (object receiving evaluation)
    model.case = Case.ACCUSATIVE
    
    # 1. First visualization: Model undergoing evaluation
    evaluation_path = os.path.join(case_dir, "model_evaluation.png")
    
    # Generate predictions
    y_pred = model.predict(X)
    
    # Calculate metrics using safe utility
    metrics = safe_metrics_calculation(y, y_pred)
    mse = metrics['mse']
    rmse = metrics['rmse'] 
    mae = metrics['mae']
    r2 = metrics['r2']
    
    # Create visualization
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Plot the data and model predictions
    ax.scatter(X, y, alpha=0.7, color='blue', label='Actual Data')
    
    # Plot the model line
    x_range = np.linspace(X.min(), X.max(), 100).reshape(-1, 1)
    y_range = model.predict(x_range)
    ax.plot(x_range, y_range, 'r-', linewidth=2, label=f'Model: y = {float(intercept):.4f} + {float(slope):.4f}x')
    
    # Add residual lines using safe utility
    residual_data = create_residual_lines_data(X, y, y_pred)
    for x_val, y_true, y_predicted in residual_data:
        ax.plot([x_val, x_val], [y_true, y_predicted], 'k--', alpha=0.3)
    
    # Style the plot
    ax.set_title("ACCUSATIVE Case: Model Being Evaluated", fontsize=16)
    ax.set_xlabel("X", fontsize=12)
    ax.set_ylabel("Y", fontsize=12)
    ax.legend()
    ax.grid(True, alpha=0.3)
    
    # Add metrics as text
    metrics_text = (
        "ACCUSATIVE CASE: MODEL RECEIVING EVALUATION\n\n"
        f"Evaluation Metrics:\n"
        f"MSE: {mse:.4f}\n"
        f"RMSE: {rmse:.4f}\n"
        f"MAE: {mae:.4f}\n"
        f"R²: {r2:.4f}\n\n"
        f"In the ACCUSATIVE case, the model is the direct object receiving the action of evaluation."
    )
    
    ax.text(0.5, -0.15, metrics_text, ha='center', va='center', 
            transform=ax.transAxes, fontsize=12, 
            bbox=dict(facecolor='white', alpha=0.9, boxstyle='round,pad=0.5'))
    
    plt.tight_layout()
    plt.savefig(evaluation_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # 2. Animation: Cross-validation showing model undergoing multiple evaluations
    cv_animation_path = os.path.join(case_dir, "cross_validation_animation.gif")
    
    # Setup the animation figure
    fig, ax = plt.subplots(figsize=(10, 8))
    fig.patch.set_facecolor('white')  # Set explicit background for better animation
    
    # Set plot limits
    ax.set_xlim(X.min() - 0.5, X.max() + 0.5)
    ax.set_ylim(y.min() - 0.5, y.max() + 0.5)
    
    # Plot all data points
    scatter = ax.scatter(X, y, color='blue', alpha=0.5, label='Data')
    
    # Initialize elements for animation
    train_scatter = ax.scatter([], [], color='green', alpha=0.7, marker='o', s=80, label='Training')
    test_scatter = ax.scatter([], [], color='red', alpha=0.7, marker='x', s=80, label='Testing')
    line, = ax.plot([], [], 'r-', linewidth=2, label='Fold Model')
    
    # Add text for fold information
    fold_text = ax.text(0.02, 0.98, '', transform=ax.transAxes, fontsize=12,
                      va='top', ha='left', bbox=dict(facecolor='white', alpha=0.8))
    
    # Add legend
    ax.legend(loc='upper right')
    ax.set_title('ACCUSATIVE Case: Cross-Validation Analysis', fontsize=14)
    ax.set_xlabel('X', fontsize=12)
    ax.set_ylabel('y', fontsize=12)
    ax.grid(True, alpha=0.3)
    
    # Prepare cross-validation
    n_splits = 5
    kf = KFold(n_splits=n_splits, shuffle=True, random_state=42)
    
    # Storage for fold data and models
    fold_indices = []
    fold_models = []
    fold_metrics = []
    
    # Run cross-validation and collect data for animation
    for fold, (train_idx, test_idx) in enumerate(kf.split(X)):
        # Split data
        X_train, X_test = X[train_idx], X[test_idx]
        y_train, y_test = y[train_idx], y[test_idx]
        
        # Create and fit a model on this fold
        fold_model = LinearRegressionModel(model_id=f"fold_{fold}_model", case=Case.NOMINATIVE)
        fold_model.fit(X_train, y_train)
        
        # Get test predictions
        y_pred_test = fold_model.predict(X_test)
        
        # Calculate metrics
        fold_mse = mean_squared_error(y_test, y_pred_test)
        fold_r2 = r2_score(y_test, y_pred_test)
        
        # Store results
        fold_indices.append((train_idx, test_idx))
        fold_models.append(fold_model)
        fold_metrics.append({
            'fold': fold + 1,
            'mse': fold_mse,
            'r2': fold_r2
        })
    
    logger.info(f"Completed {n_splits}-fold cross-validation for ACCUSATIVE case")
    
    # Animation update function
    def update(frame):
        # For frames before the fold count, show intro animation
        if frame == 0:
            # Just show all data points
            train_scatter.set_offsets(np.empty((0, 2)))
            test_scatter.set_offsets(np.empty((0, 2)))
            line.set_data([], [])
            fold_text.set_text("Cross-Validation: ACCUSATIVE Case\nModel is being evaluated")
            return train_scatter, test_scatter, line, fold_text
        
        # Show each fold
        fold_idx = min(frame - 1, len(fold_indices) - 1)  # Ensure index is valid
        
        # Get train and test indices for this fold
        train_idx, test_idx = fold_indices[fold_idx]
        
        # Get the model for this fold
        fold_model = fold_models[fold_idx]
        
        # Set train/test scatter points
        train_data = np.column_stack((X[train_idx], y[train_idx]))
        test_data = np.column_stack((X[test_idx], y[test_idx]))
        
        train_scatter.set_offsets(train_data)
        test_scatter.set_offsets(test_data)
        
        # Get model parameters
        intercept = float(fold_model._params['intercept'])
        slope = float(fold_model._params['coefficients'][0])
        
        # Set model line
        x_line = np.linspace(X.min() - 0.5, X.max() + 0.5, 100)
        y_line = intercept + slope * x_line
        line.set_data(x_line, y_line)
        
        # Update text
        # Get metrics for this fold
        test_predictions = fold_model.predict(X[test_idx])
        test_mse = float(mean_squared_error(y[test_idx], test_predictions))
        test_r2 = float(r2_score(y[test_idx], test_predictions))
            
        fold_text.set_text(
            f"Fold {fold_idx+1} of {len(fold_indices)}\n"
            f"MSE: {test_mse:.4f}\n"
            f"R²: {test_r2:.4f}\n"
            f"Model being evaluated (ACCUSATIVE case)"
        )
        
        return train_scatter, test_scatter, line, fold_text
    
    # Create animation
    logger.info(f"Creating ACCUSATIVE case cross-validation animation with {len(fold_indices) + 1} frames")
    anim = FuncAnimation(fig, update, frames=len(fold_indices) + 1, blit=True)
    
    # Save animation
    animation_success = save_animation(anim, cv_animation_path, fps=1, dpi=120)
    
    # Always create static images to ensure reliable output
    logger.info(f"Creating static cross-validation images as reliable output")
    # Create a directory for static frames
    frames_dir = os.path.join(case_dir, "cv_frames")
    os.makedirs(frames_dir, exist_ok=True)
    
    # Save key frames as static images
    for i in range(len(fold_indices) + 1):
        frame_path = os.path.join(frames_dir, f"fold_{i:02d}.png")
        # Create a new figure for each frame to avoid state issues
        new_fig, new_ax = plt.subplots(figsize=(10, 8))
        new_fig.patch.set_facecolor('white')
        
        # Copy the plot setup
        new_ax.set_xlim(X.min() - 0.5, X.max() + 0.5)
        new_ax.set_ylim(y.min() - 0.5, y.max() + 0.5)
        new_ax.scatter(X, y, color='blue', alpha=0.5, label='Data')
        new_ax.set_title('ACCUSATIVE Case: Cross-Validation Analysis', fontsize=14)
        new_ax.set_xlabel('X', fontsize=12)
        new_ax.set_ylabel('y', fontsize=12)
        new_ax.grid(True, alpha=0.3)
        
        # If not the intro frame, add the fold-specific elements
        if i > 0:
            fold_idx = i - 1
            train_idx, test_idx = fold_indices[fold_idx]
            fold_model = fold_models[fold_idx]
            
            # Plot train/test points
            new_ax.scatter(X[train_idx], y[train_idx], color='green', alpha=0.7, 
                         marker='o', s=80, label='Training')
            new_ax.scatter(X[test_idx], y[test_idx], color='red', alpha=0.7, 
                         marker='x', s=80, label='Testing')
            
            # Plot model line
            intercept = float(fold_model._params['intercept'])
            slope = float(fold_model._params['coefficients'][0])
            x_line = np.linspace(X.min() - 0.5, X.max() + 0.5, 100)
            y_line = intercept + slope * x_line
            new_ax.plot(x_line, y_line, 'r-', linewidth=2, label='Fold Model')
            
            # Get metrics
            test_predictions = fold_model.predict(X[test_idx])
            test_mse = float(mean_squared_error(y[test_idx], test_predictions))
            test_r2 = float(r2_score(y[test_idx], test_predictions))
            
            # Add text
            new_ax.text(0.02, 0.98, 
                      f"Fold {fold_idx+1} of {len(fold_indices)}\n"
                      f"MSE: {test_mse:.4f}\n"
                      f"R²: {test_r2:.4f}\n"
                      f"Model being evaluated (ACCUSATIVE case)",
                      transform=new_ax.transAxes, fontsize=12,
                      va='top', ha='left', bbox=dict(facecolor='white', alpha=0.8))
        else:
            # Intro frame
            new_ax.text(0.02, 0.98, 
                      "Cross-Validation: ACCUSATIVE Case\nModel is being evaluated",
                      transform=new_ax.transAxes, fontsize=12,
                      va='top', ha='left', bbox=dict(facecolor='white', alpha=0.8))
        
        new_ax.legend(loc='upper right')
        new_fig.tight_layout()
        plt.savefig(frame_path, dpi=120, bbox_inches='tight')
        plt.close(new_fig)
    
    logger.info(f"Saved static cross-validation frames to {frames_dir}")
    plt.close(fig)
    
    # Calculate cross-validation metrics for use in the residuals animation
    all_mse = [fold['mse'] for fold in fold_metrics]
    all_r2 = [fold['r2'] for fold in fold_metrics]
    avg_mse = np.mean(all_mse)
    avg_r2 = np.mean(all_r2)
    std_mse = np.std(all_mse)
    std_r2 = np.std(all_r2)
    
    # 3. Residuals analysis animation - another form of model evaluation
    residuals_animation_path = os.path.join(case_dir, "residuals_analysis_animation.gif")
    
    # Calculate residuals for the full dataset model
    y_pred = model.predict(X)
    residuals = y - y_pred
    
    # Create a directory for frames
    frames_dir = os.path.join(case_dir, "residuals_frames")
    os.makedirs(frames_dir, exist_ok=True)
    
    # Create each frame individually for reliability
    logger.info(f"Creating residual analysis frames")
    frame_paths = []
    
    # Generate Frame 1: Residuals vs Predicted
    frame1_path = os.path.join(frames_dir, "residuals_frame_0.png")
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 8))
    
    # 1. Residuals vs Predicted
    ax1.scatter(y_pred, residuals, alpha=0.7, color='blue')
    ax1.axhline(y=0, color='red', linestyle='-', alpha=0.7)
    
    ax1.set_title("Residuals vs Predicted Values", fontsize=14)
    ax1.set_xlabel("Predicted Values", fontsize=12)
    ax1.set_ylabel("Residuals", fontsize=12)
    ax1.grid(True, alpha=0.3)
    
    # Add evaluative text
    eval_text = (
        "ANALYSIS 1: RESIDUAL PATTERNS\n\n"
        "We are evaluating the MODEL's residual distribution\n"
        "looking for patterns that indicate bias or heteroscedasticity."
    )
    
    ax1.text(0.5, -0.15, eval_text, ha='center', va='center', 
            transform=ax1.transAxes, fontsize=12, 
            bbox=dict(facecolor='white', alpha=0.9, boxstyle='round,pad=0.5'))
    
    # Plot residual histogram on the right
    ax2.hist(residuals, bins=15, alpha=0.7, color='purple')
    ax2.axvline(x=0, color='red', linestyle='-', alpha=0.7)
    
    ax2.set_title("Residuals Distribution", fontsize=14)
    ax2.set_xlabel("Residual Value", fontsize=12)
    ax2.set_ylabel("Frequency", fontsize=12)
    ax2.grid(True, alpha=0.3)
    
    # Add distribution metrics
    dist_text = (
        f"Mean of residuals: {float(np.mean(residuals)):.4f}\n"
        f"Standard deviation: {float(np.std(residuals)):.4f}\n"
        f"Skewness: {float(np.mean(residuals**3)/(np.std(residuals)**3)):.4f}"
    )
    
    ax2.text(0.05, 0.95, dist_text, transform=ax2.transAxes, fontsize=10,
            va='top', ha='left', bbox=dict(facecolor='white', alpha=0.8))
    
    fig.tight_layout()
    plt.savefig(frame1_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    frame_paths.append(frame1_path)
    logger.info(f"Created frame 1/3: Residuals vs Predicted")
    
    # Generate Frame 2: QQ Plot
    frame2_path = os.path.join(frames_dir, "residuals_frame_1.png")
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 8))
    
    # 2. QQ Plot
    from scipy import stats
    
    # Generate theoretical quantiles
    sorted_residuals = np.sort(residuals.flatten())
    theoretical_quantiles = stats.norm.ppf(np.linspace(0.01, 0.99, len(sorted_residuals)))
    
    # Plot QQ plot
    ax1.scatter(theoretical_quantiles, sorted_residuals, alpha=0.7, color='blue')
    ax1.plot([-3, 3], [-3, 3], 'r--', alpha=0.7)  # Reference line
    
    ax1.set_title("Q-Q Plot of Residuals", fontsize=14)
    ax1.set_xlabel("Theoretical Quantiles", fontsize=12)
    ax1.set_ylabel("Sample Quantiles", fontsize=12)
    ax1.grid(True, alpha=0.3)
    
    # Add evaluative text
    eval_text = (
        "ANALYSIS 2: NORMALITY CHECK\n\n"
        "We are evaluating if the MODEL's residuals follow a normal distribution,\n"
        "which is an assumption in linear regression."
    )
    
    ax1.text(0.5, -0.15, eval_text, ha='center', va='center', 
            transform=ax1.transAxes, fontsize=12, 
            bbox=dict(facecolor='white', alpha=0.9, boxstyle='round,pad=0.5'))
    
    # Shapiro-Wilk test
    from scipy.stats import shapiro
    stat, p_value = shapiro(residuals)
    
    # Plot statistical test results
    ax2.axis('off')
    test_result = (
        "NORMALITY TEST RESULTS\n\n"
        f"Shapiro-Wilk Test:\n"
        f"Test statistic: {float(stat):.4f}\n"
        f"p-value: {float(p_value):.4f}\n\n"
        f"{'Residuals appear to be normally distributed' if p_value > 0.05 else 'Residuals do not appear to be normally distributed'}\n"
        f"(at significance level α=0.05)"
    )
    
    ax2.text(0.5, 0.5, test_result, ha='center', va='center', 
            transform=ax2.transAxes, fontsize=12, 
            bbox=dict(boxstyle="round,pad=0.5", fc="white", alpha=0.9))
    
    fig.tight_layout()
    plt.savefig(frame2_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    frame_paths.append(frame2_path)
    logger.info(f"Created frame 2/3: QQ Plot Analysis")
    
    # Generate Frame 3: Summary
    frame3_path = os.path.join(frames_dir, "residuals_frame_2.png")
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 8))
    
    # 3. Summary and linguistic interpretation
    ax1.axis('off')
    ax2.axis('off')
    
    # Create linguistic diagram for ACCUSATIVE case
    accusative_diagram = (
        "ACCUSATIVE CASE: MODEL AS DIRECT OBJECT\n\n"
        "Subject → Verb → Object\n"
        "  We    →  Test →  MODEL\n"
        "  We    → Evaluate → MODEL\n"
        "  We    → Validate → MODEL\n\n"
        "The model is the OBJECT that RECEIVES the action."
    )
    
    ax1.text(0.5, 0.5, accusative_diagram, ha='center', va='center', 
            transform=ax1.transAxes, fontsize=14, 
            bbox=dict(boxstyle="round,pad=0.5", fc="lightyellow", alpha=0.9))
    
    # Create model evaluation summary
    eval_summary = (
        "MODEL EVALUATION SUMMARY\n\n"
        f"MSE: {float(mse):.4f}\n"
        f"RMSE: {float(rmse):.4f}\n"
        f"MAE: {float(mae):.4f}\n"
        f"R²: {float(r2):.4f}\n\n"
        f"Cross-validation Mean MSE: {float(avg_mse):.4f}\n"
        f"Cross-validation Mean R²: {float(avg_r2):.4f}\n\n"
        "In the ACCUSATIVE case, all these evaluations are actions\n"
        "being performed ON the model rather than BY the model."
    )
    
    ax2.text(0.5, 0.5, eval_summary, ha='center', va='center', 
            transform=ax2.transAxes, fontsize=12, 
            bbox=dict(boxstyle="round,pad=0.5", fc="white", alpha=0.9))
    
    fig.tight_layout()
    plt.savefig(frame3_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    frame_paths.append(frame3_path)
    logger.info(f"Created frame 3/3: Summary")
    
    # Create the GIF from the frames using imageio
    try:
        from PIL import Image
        import imageio
        
        logger.info(f"Creating GIF from {len(frame_paths)} frames")
        
        # Read all frames and ensure they have the same dimensions
        images = []
        target_size = None
        
        # First pass: determine the size of the first valid image
        for frame_path in frame_paths:
            try:
                img = Image.open(frame_path)
                if target_size is None:
                    target_size = img.size
                    logger.info(f"Using target size for frames: {target_size}")
                    break
            except Exception as e:
                logger.warning(f"Error opening image {frame_path}: {str(e)}")
        
        # If no valid images were found, use a default size
        if target_size is None:
            target_size = (800, 400)
            logger.warning(f"No valid images found, using default size: {target_size}")
        
        # Second pass: resize all images to the same dimensions
        for frame_path in frame_paths:
            try:
                img = Image.open(frame_path)
                # Resize to target dimensions if different
                if img.size != target_size:
                    logger.info(f"Resizing frame {frame_path} from {img.size} to {target_size}")
                    img = img.resize(target_size, Image.LANCZOS)
                # Convert to RGB if needed (in case of RGBA)
                if img.mode != 'RGB':
                    img = img.convert('RGB')
                # Convert to numpy array
                img_array = np.array(img)
                images.append(img_array)
            except Exception as e:
                logger.warning(f"Error processing image {frame_path}: {str(e)}")
        
        # Only create GIF if we have images
        if images:
            logger.info(f"Creating GIF with {len(images)} processed frames")
            # Save as GIF
            imageio.mimsave(residuals_animation_path, images, fps=0.33, loop=0)
            
            # Verify the file was created
            if os.path.exists(residuals_animation_path):
                file_size = os.path.getsize(residuals_animation_path) / 1024.0  # KB
                logger.info(f"Successfully created animation at {residuals_animation_path} ({file_size:.1f} KB)")
            else:
                logger.error(f"Failed to create animation at {residuals_animation_path}")
        else:
            logger.error("No valid images to create GIF")
            
    except Exception as e:
        logger.error(f"Error creating GIF: {str(e)}")
        logger.info(f"Static frames are still available in {frames_dir}")
    
    # No need to remove the FuncAnimation code completely as it might affect other parts
    # Just make it a placeholder that doesn't actually run
    def init_residuals():
        return []
        
    def update_residuals(frame):
        return []
        
    # Create empty animation object (will not be used)
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 8))
    anim = FuncAnimation(fig, update_residuals, frames=3, init_func=init_residuals, interval=3000, blit=True)
    plt.close(fig)
    
    # 4. Document all results for this case
    with open(os.path.join(case_dir, "accusative_results.txt"), 'w') as f:
        f.write(f"ACCUSATIVE CASE RESULTS\n")
        f.write(f"======================\n\n")
        f.write(f"Linguistic meaning: {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n")
        f.write(f"Regression context: {case_info['regression_context']}\n\n")
        
        f.write(f"Model Parameters:\n")
        f.write(f"- Intercept: {float(intercept):.6f}\n")
        f.write(f"- Slope: {float(slope):.6f}\n")
        f.write(f"- Model Formula: y = {float(intercept):.6f} + {float(slope):.6f}x\n\n")
        
        f.write(f"Evaluation Metrics:\n")
        f.write(f"- Mean Squared Error (MSE): {float(mse):.6f}\n")
        f.write(f"- Root Mean Squared Error (RMSE): {float(rmse):.6f}\n")
        f.write(f"- Mean Absolute Error (MAE): {float(mae):.6f}\n")
        f.write(f"- R-squared: {float(r2):.6f}\n\n")
        
        f.write(f"Cross-Validation Results ({n_splits} folds):\n")
        for i, fold_data in enumerate(fold_metrics):
            f.write(f"- Fold {i+1}: MSE={fold_data['mse']:.6f}, R²={fold_data['r2']:.6f}\n")
        f.write(f"- Average MSE: {avg_mse:.6f} ± {std_mse:.6f}\n")
        f.write(f"- Average R²: {avg_r2:.6f} ± {std_r2:.6f}\n\n")
        
        f.write(f"Residual Analysis:\n")
        f.write(f"- Mean of residuals: {float(np.mean(residuals)):.6f}\n")
        f.write(f"- Standard deviation: {float(np.std(residuals)):.6f}\n")
        f.write(f"- Range: [{float(np.min(residuals)):.6f}, {float(np.max(residuals)):.6f}]\n\n")
        
        f.write(f"Linguistic Analysis (ACCUSATIVE Case):\n")
        f.write(f"- The model is the OBJECT receiving the action of evaluation\n")
        f.write(f"- Verbs acting on the model: Test, Evaluate, Validate, Assess\n")
        f.write(f"- Subjects performing the action: Researchers, Analysts, Tests\n\n")
        
        f.write(f"Linguistic Formula Example:\n")
        f.write(f"{case_info['example']}\n\n")
    
    logger.info(f"Completed ACCUSATIVE case test with visualizations in {case_dir}")
    
    return model 