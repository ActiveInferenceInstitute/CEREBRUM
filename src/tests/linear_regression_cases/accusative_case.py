#!/usr/bin/env python3
"""
ACCUSATIVE Case Test Module for CEREBRUM
Tests the ACCUSATIVE case in the linear regression context
"""

import os
import logging
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from sklearn.metrics import mean_squared_error, r2_score, mean_absolute_error
from sklearn.model_selection import KFold

from src.models.base import Case
from src.models.case_definitions import CaseDefinitions
from src.models.linear_regression import LinearRegressionModel
from src.utils.visualization import plot_case_linguistic_context

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
    case_dir = os.path.join(output_dir, Case.ACCUSATIVE.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.ACCUSATIVE, linguistics_path)
    
    # Get data
    X, y = linear_test_data
    
    # Create and fit a model (initially in NOMINATIVE case)
    model = LinearRegressionModel(model_id=f"{Case.ACCUSATIVE.value}_model", case=Case.NOMINATIVE)
    model.fit(X, y)
    
    # Extract parameters
    intercept = model._params['intercept']
    slope = model._params['coefficients'][0]
    
    logger.info(f"Model fitted with parameters: intercept={intercept:.4f}, slope={slope:.4f}")
    
    # Now convert model to ACCUSATIVE case (object receiving evaluation)
    model.case = Case.ACCUSATIVE
    
    # 1. First visualization: Model undergoing evaluation
    evaluation_path = os.path.join(case_dir, "model_evaluation.png")
    
    # Generate predictions
    y_pred = model.predict(X)
    
    # Calculate metrics
    mse = mean_squared_error(y, y_pred)
    rmse = np.sqrt(mse)
    mae = mean_absolute_error(y, y_pred)
    r2 = r2_score(y, y_pred)
    
    # Create visualization
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Plot the data and model predictions
    ax.scatter(X, y, alpha=0.7, color='blue', label='Actual Data')
    
    # Plot the model line
    x_range = np.linspace(X.min(), X.max(), 100).reshape(-1, 1)
    y_range = model.predict(x_range)
    ax.plot(x_range, y_range, 'r-', linewidth=2, label=f'Model: y = {intercept:.4f} + {slope:.4f}x')
    
    # Add residual lines
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
    
    # Prepare cross-validation
    n_splits = 5
    kf = KFold(n_splits=n_splits, shuffle=True, random_state=42)
    
    # Storage for validation results
    fold_metrics = []
    
    # Run cross-validation (just to collect metrics, we'll animate them later)
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
        fold_metrics.append({
            'fold': fold + 1,
            'train_indices': train_idx,
            'test_indices': test_idx,
            'mse': fold_mse,
            'r2': fold_r2,
            'model': fold_model
        })
    
    logger.info(f"Completed {n_splits}-fold cross-validation for ACCUSATIVE case")
    
    # Create animation
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 8))
    
    # Define animation update function
    def update(frame):
        # For frames before the fold count, show intro animation
        if frame < n_splits:
            # Clear axes
            ax1.clear()
            ax2.clear()
            
            # Get current fold data
            fold_data = fold_metrics[frame]
            fold_num = fold_data['fold']
            train_idx = fold_data['train_indices']
            test_idx = fold_data['test_indices']
            fold_model = fold_data['model']
            
            # Plot training and validation sets
            ax1.scatter(X[train_idx], y[train_idx], color='blue', alpha=0.7, label='Training Data')
            ax1.scatter(X[test_idx], y[test_idx], color='red', alpha=0.7, label='Validation Data')
            
            # Plot the fold model's prediction line
            x_range = np.linspace(X.min(), X.max(), 100).reshape(-1, 1)
            y_range = fold_model.predict(x_range)
            ax1.plot(x_range, y_range, 'g-', linewidth=2, 
                     label=f'Fold {fold_num} Model')
            
            # Add arrows pointing to the validation points (showing model being validated)
            for idx in test_idx:
                # Check if arrays are 2D or 1D and index accordingly
                if len(X.shape) > 1:
                    x_val = X[idx, 0]
                else:
                    x_val = X[idx]
                    
                if len(y.shape) > 1:
                    y_true = y[idx, 0]
                else:
                    y_true = y[idx]
                
                # Get prediction for this point
                if len(X.shape) > 1:
                    x_input = X[idx].reshape(1, -1)
                else:
                    x_input = np.array([X[idx]]).reshape(1, -1)
                
                # Get prediction
                y_pred_val = fold_model.predict(x_input)
                
                # Extract prediction value based on its shape
                if len(y_pred_val.shape) > 1:
                    y_pred = y_pred_val[0, 0]
                else:
                    y_pred = y_pred_val[0]
                
                residual = y_true - y_pred
                
                # Add arrow pointing to validation point
                ax1.annotate("", xy=(x_val, y_true), 
                            xytext=(x_val - 0.1, y_true + 0.1 * np.sign(residual)),
                            arrowprops=dict(arrowstyle="->", color='orange', lw=1))
            
            # Style plot 1
            ax1.set_title(f"Fold {fold_num}/{n_splits}: Model Being Evaluated", fontsize=14)
            ax1.set_xlabel("X", fontsize=12)
            ax1.set_ylabel("Y", fontsize=12)
            ax1.legend()
            ax1.grid(True, alpha=0.3)
            
            # Set axes limits consistently
            ax1.set_xlim(X.min() - 0.1, X.max() + 0.1)
            ax1.set_ylim(y.min() - 0.5, y.max() + 0.5)
            
            # Add accusative case text pointing to the model
            validation_text = (
                f"MODEL is being\nEVALUATED on\nfold {fold_num} data"
            )
            
            # Position the text consistently
            text_x = X.min() + (X.max() - X.min()) * 0.15
            text_y = y.max() - (y.max() - y.min()) * 0.2
            
            ax1.text(text_x, text_y, validation_text, 
                    bbox=dict(boxstyle="round,pad=0.3", fc="#E3F4F4", ec="blue", alpha=0.8),
                    ha='center', va='center', fontsize=12)
            
            # Plot metrics on the right side
            # Bar chart showing current fold metrics and previous folds
            fold_nums = list(range(1, fold_num + 1))
            mse_values = [fold_metrics[i]['mse'] for i in range(fold_num)]
            r2_values = [fold_metrics[i]['r2'] for i in range(fold_num)]
            
            # Plot MSE bars
            bar_width = 0.35
            ax2.bar(np.array(fold_nums) - bar_width/2, mse_values, bar_width, 
                   alpha=0.7, color='red', label='MSE')
            
            # Add R² on the second y-axis
            ax2_r2 = ax2.twinx()
            ax2_r2.bar(np.array(fold_nums) + bar_width/2, r2_values, bar_width, 
                      alpha=0.7, color='green', label='R²')
            
            # Add fold average lines if we have multiple folds
            if fold_num > 1:
                avg_mse = np.mean(mse_values)
                avg_r2 = np.mean(r2_values)
                ax2.axhline(y=avg_mse, color='red', linestyle='--', alpha=0.7, 
                          label=f'Avg MSE: {avg_mse:.4f}')
                ax2_r2.axhline(y=avg_r2, color='green', linestyle='--', alpha=0.7,
                             label=f'Avg R²: {avg_r2:.4f}')
            
            # Style plot 2
            ax2.set_title("Cross-Validation Metrics", fontsize=14)
            ax2.set_xlabel("Fold", fontsize=12)
            ax2.set_ylabel("Mean Squared Error", color='red', fontsize=12)
            ax2_r2.set_ylabel("R² Score", color='green', fontsize=12)
            
            # Set y-axis limits
            if fold_num > 0:
                max_mse = max(mse_values) * 1.2
                ax2.set_ylim(0, max_mse)
                ax2_r2.set_ylim(0, 1.1)
            
            # Add legend for both axes
            lines1, labels1 = ax2.get_legend_handles_labels()
            lines2, labels2 = ax2_r2.get_legend_handles_labels()
            ax2.legend(lines1 + lines2, labels1 + labels2, loc='upper right')
            
            # Show fold information
            fold_info = (
                f"Fold {fold_num}/{n_splits}:\n"
                f"MSE: {fold_data['mse']:.4f}\n"
                f"R²: {fold_data['r2']:.4f}"
            )
            
            ax2.text(0.05, 0.95, fold_info, transform=ax2.transAxes, fontsize=12,
                    va='top', ha='left', bbox=dict(facecolor='white', alpha=0.8))
            
        else:
            # Summary frame after showing all folds
            ax1.clear()
            ax2.clear()
            
            # Create a visual summary in ax1
            ax1.text(0.5, 0.5, "ACCUSATIVE CASE SUMMARY\nModel as Object of Evaluation", 
                    ha='center', va='center', fontsize=16, fontweight='bold',
                    transform=ax1.transAxes)
            
            # Create subject-verb-object diagram
            ax1.text(0.2, 0.7, "Evaluator\n(Subject)", ha='center', va='center', 
                    fontsize=14, fontweight='bold',
                    bbox=dict(boxstyle="round,pad=0.3", fc="lightblue", ec="blue", alpha=0.8))
            
            ax1.text(0.5, 0.7, "Evaluates\n(Verb)", ha='center', va='center', 
                    fontsize=14, fontweight='bold',
                    bbox=dict(boxstyle="round,pad=0.3", fc="lightgreen", ec="green", alpha=0.8))
            
            ax1.text(0.8, 0.7, "MODEL\n(Object)", ha='center', va='center', 
                    fontsize=14, fontweight='bold',
                    bbox=dict(boxstyle="round,pad=0.3", fc="#F8E8EE", ec="pink", alpha=0.8))
            
            # Add arrows
            ax1.annotate("", xy=(0.35, 0.7), xytext=(0.2, 0.7), 
                        xycoords='axes fraction', textcoords='axes fraction',
                        arrowprops=dict(arrowstyle="->", color='blue', lw=2))
            
            ax1.annotate("", xy=(0.65, 0.7), xytext=(0.5, 0.7), 
                        xycoords='axes fraction', textcoords='axes fraction',
                        arrowprops=dict(arrowstyle="->", color='green', lw=2))
            
            # Add explanation
            accusative_explanation = (
                "In the ACCUSATIVE case, the model is the DIRECT OBJECT receiving the action.\n"
                "The model is being evaluated, tested, or validated by an external agent.\n"
                "The model experiences the action but does not perform it."
            )
            
            ax1.text(0.5, 0.3, accusative_explanation, ha='center', va='center', 
                    fontsize=12, transform=ax1.transAxes,
                    bbox=dict(boxstyle="round,pad=0.5", fc="white", alpha=0.9))
            
            # Show overall performance in ax2
            all_mse = [fold['mse'] for fold in fold_metrics]
            all_r2 = [fold['r2'] for fold in fold_metrics]
            avg_mse = np.mean(all_mse)
            avg_r2 = np.mean(all_r2)
            std_mse = np.std(all_mse)
            std_r2 = np.std(all_r2)
            
            # Create a summary table
            summary_text = (
                "CROSS-VALIDATION SUMMARY\n\n"
                f"Mean MSE: {avg_mse:.4f} ± {std_mse:.4f}\n"
                f"Mean R²: {avg_r2:.4f} ± {std_r2:.4f}\n\n"
                f"Fold-by-fold results:\n"
            )
            
            for i, fold_data in enumerate(fold_metrics):
                summary_text += f"Fold {i+1}: MSE={fold_data['mse']:.4f}, R²={fold_data['r2']:.4f}\n"
            
            ax2.text(0.5, 0.5, summary_text, ha='center', va='center', 
                    fontsize=12, transform=ax2.transAxes,
                    bbox=dict(boxstyle="round,pad=0.5", fc="white", alpha=0.9))
            
            # Turn off axes
            ax1.axis('off')
            ax2.axis('off')
        
        return [ax1, ax2]
    
    # Create animation with n_splits + 1 frames (including summary)
    anim = FuncAnimation(fig, update, frames=n_splits+1, interval=2000, blit=True)
    
    # Save animation
    logger.info(f"Creating ACCUSATIVE case cross-validation animation with {n_splits+1} frames")
    anim.save(cv_animation_path, writer='pillow', fps=1, dpi=100)
    plt.close(fig)
    
    # 3. Residuals analysis animation - another form of model evaluation
    residuals_animation_path = os.path.join(case_dir, "residuals_analysis_animation.gif")
    
    # Calculate residuals for the full dataset model
    y_pred = model.predict(X)
    residuals = y - y_pred
    
    # Create animation showing different residual analyses
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 8))
    
    # Animation update function for residual analysis
    def update_residuals(frame):
        ax1.clear()
        ax2.clear()
        
        # Different ways to evaluate the model
        if frame == 0:
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
                f"Mean of residuals: {np.mean(residuals):.4f}\n"
                f"Standard deviation: {np.std(residuals):.4f}\n"
                f"Skewness: {np.mean(residuals**3)/(np.std(residuals)**3):.4f}"
            )
            
            ax2.text(0.05, 0.95, dist_text, transform=ax2.transAxes, fontsize=10,
                    va='top', ha='left', bbox=dict(facecolor='white', alpha=0.8))
            
        elif frame == 1:
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
                f"Test statistic: {stat:.4f}\n"
                f"p-value: {p_value:.4f}\n\n"
                f"{'Residuals appear to be normally distributed' if p_value > 0.05 else 'Residuals do not appear to be normally distributed'}\n"
                f"(at significance level α=0.05)"
            )
            
            ax2.text(0.5, 0.5, test_result, ha='center', va='center', 
                    transform=ax2.transAxes, fontsize=12, 
                    bbox=dict(boxstyle="round,pad=0.5", fc="white", alpha=0.9))
            
        else:
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
                f"MSE: {mse:.4f}\n"
                f"RMSE: {rmse:.4f}\n"
                f"MAE: {mae:.4f}\n"
                f"R²: {r2:.4f}\n\n"
                f"Cross-validation Mean MSE: {avg_mse:.4f}\n"
                f"Cross-validation Mean R²: {avg_r2:.4f}\n\n"
                "In the ACCUSATIVE case, all these evaluations are actions\n"
                "being performed ON the model rather than BY the model."
            )
            
            ax2.text(0.5, 0.5, eval_summary, ha='center', va='center', 
                    transform=ax2.transAxes, fontsize=12, 
                    bbox=dict(boxstyle="round,pad=0.5", fc="white", alpha=0.9))
        
        return [ax1, ax2]
    
    # Create animation with 3 frames (different residual analyses)
    anim = FuncAnimation(fig, update_residuals, frames=3, interval=3000, blit=True)
    
    # Save animation
    logger.info("Creating ACCUSATIVE case residuals analysis animation")
    anim.save(residuals_animation_path, writer='pillow', fps=0.5, dpi=100)
    plt.close(fig)
    
    # 4. Document all results for this case
    with open(os.path.join(case_dir, "accusative_results.txt"), 'w') as f:
        f.write(f"ACCUSATIVE CASE RESULTS\n")
        f.write(f"======================\n\n")
        f.write(f"Linguistic meaning: {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n")
        f.write(f"Regression context: {case_info['regression_context']}\n\n")
        
        f.write(f"Model Parameters:\n")
        f.write(f"- Intercept: {intercept:.6f}\n")
        f.write(f"- Slope: {slope:.6f}\n")
        f.write(f"- Model Formula: y = {intercept:.6f} + {slope:.6f}x\n\n")
        
        f.write(f"Evaluation Metrics:\n")
        f.write(f"- Mean Squared Error (MSE): {mse:.6f}\n")
        f.write(f"- Root Mean Squared Error (RMSE): {rmse:.6f}\n")
        f.write(f"- Mean Absolute Error (MAE): {mae:.6f}\n")
        f.write(f"- R-squared: {r2:.6f}\n\n")
        
        f.write(f"Cross-Validation Results ({n_splits} folds):\n")
        for i, fold_data in enumerate(fold_metrics):
            f.write(f"- Fold {i+1}: MSE={fold_data['mse']:.6f}, R²={fold_data['r2']:.6f}\n")
        f.write(f"- Average MSE: {avg_mse:.6f} ± {std_mse:.6f}\n")
        f.write(f"- Average R²: {avg_r2:.6f} ± {std_r2:.6f}\n\n")
        
        f.write(f"Residual Analysis:\n")
        f.write(f"- Mean of residuals: {np.mean(residuals):.6f}\n")
        f.write(f"- Standard deviation: {np.std(residuals):.6f}\n")
        f.write(f"- Range: [{np.min(residuals):.6f}, {np.max(residuals):.6f}]\n\n")
        
        f.write(f"Linguistic Analysis (ACCUSATIVE Case):\n")
        f.write(f"- The model is the OBJECT receiving the action of evaluation\n")
        f.write(f"- Verbs acting on the model: Test, Evaluate, Validate, Assess\n")
        f.write(f"- Subjects performing the action: Researchers, Analysts, Tests\n\n")
        
        f.write(f"Linguistic Formula Example:\n")
        f.write(f"{case_info['example']}\n\n")
    
    logger.info(f"Completed ACCUSATIVE case test with visualizations in {case_dir}")
    
    return model 