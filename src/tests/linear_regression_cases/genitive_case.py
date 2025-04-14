#!/usr/bin/env python3
"""
GENITIVE Case Test Module for CEREBRUM
Tests the GENITIVE case in the linear regression context
"""

import os
import logging
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
from sklearn.metrics import mean_squared_error

from src.models.base import Case
from src.models.case_definitions import CaseDefinitions
from src.models.linear_regression import LinearRegressionModel
from src.utils.visualization import plot_case_linguistic_context

# Setup logging
logger = logging.getLogger("cerebrum-genitive-test")

def test_genitive_case(linear_test_data, output_dir):
    """
    Test GENITIVE case: Derivative/possession analysis of the model.
    
    In linguistics: Indicating possession or belonging.
    In regression: Analyzing what belongs to the model - parameters, errors, and derivatives.
    """
    # Get case definitions
    case_definitions = CaseDefinitions.get_all_cases()
    
    # Get case info for logging
    case_info = case_definitions[Case.GENITIVE]
    logger.info(f"Testing {Case.GENITIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(output_dir, Case.GENITIVE.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.GENITIVE, linguistics_path)
    
    # Get data
    X, y = linear_test_data
    
    # Fit a GENITIVE case model - "belonging" to the model
    model = LinearRegressionModel(model_id=f"{Case.GENITIVE.value}_model", case=Case.GENITIVE)
    model.fit(X, y)
    
    logger.info(f"Fitted GENITIVE case model with parameters: {model._params}")
    
    # 1. Model's possessions: Parameters analysis
    parameter_path = os.path.join(case_dir, "parameters_possession.png")
    
    # Extract parameters
    intercept = model._params['intercept']
    slope = model._params['coefficients'][0]
    
    # Create a visualization of what belongs to the model
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Plot model parameters as bars
    param_names = ['Intercept', 'Slope']
    param_values = [intercept, slope]
    
    # Create parameter bar chart
    bars = ax.bar(param_names, param_values, color=['skyblue', 'lightgreen'], alpha=0.7)
    
    # Add value labels on bars
    for bar, val in zip(bars, param_values):
        ax.text(bar.get_x() + bar.get_width()/2, val + 0.05 * np.sign(val),
               f'{val:.4f}', ha='center', va='bottom' if val > 0 else 'top',
               fontweight='bold')
    
    # Style the chart
    ax.set_title("GENITIVE Case: What Belongs to the Model (Parameters)", fontsize=14)
    ax.set_ylabel("Value", fontsize=12)
    ax.grid(axis='y', linestyle='--', alpha=0.7)
    
    # Add explanatory text
    param_text = (
        "GENITIVE CASE ANALYSIS:\n"
        f"Model equation: y = {intercept:.4f} + {slope:.4f}x\n\n"
        "The GENITIVE case examines what belongs to the model:\n"
        "- Parameters that constitute the model\n"
        "- Errors that belong to its predictions\n"
        "- Derivatives that show its sensitivity"
    )
    
    ax.text(0.5, -0.3, param_text, ha='center', va='center', 
            transform=ax.transAxes, bbox=dict(facecolor='white', alpha=0.8))
    
    plt.tight_layout()
    plt.savefig(parameter_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # 2. Model's possessions: Derivatives and sensitivities
    derivatives_path = os.path.join(case_dir, "derivatives_possession.png")
    
    # Calculate model sensitivities for different inputs
    x_test = np.linspace(X.min(), X.max(), 100).reshape(-1, 1)
    
    # Generate predictions
    y_pred = model.predict(x_test)
    
    # Compute derivatives (for linear model, it's just the coefficient)
    derivatives = np.ones_like(x_test) * slope
    
    # Visualize derivatives
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Plot the predictions
    ax.plot(x_test, y_pred, 'b-', linewidth=2, label='Model Predictions')
    
    # Plot the derivatives (constant for linear model)
    ax_deriv = ax.twinx()
    ax_deriv.plot(x_test, derivatives, 'r-', linewidth=2, label='Derivatives (dy/dx)')
    
    # Style the chart
    ax.set_xlabel('x', fontsize=12)
    ax.set_ylabel('Prediction (y)', color='blue', fontsize=12)
    ax_deriv.set_ylabel('Derivative (dy/dx)', color='red', fontsize=12)
    
    ax.tick_params(axis='y', labelcolor='blue')
    ax_deriv.tick_params(axis='y', labelcolor='red')
    
    ax.set_title("GENITIVE Case: Derivatives Belonging to the Model", fontsize=14)
    ax.grid(True, alpha=0.3)
    
    # Combine legends
    lines_1, labels_1 = ax.get_legend_handles_labels()
    lines_2, labels_2 = ax_deriv.get_legend_handles_labels()
    ax.legend(lines_1 + lines_2, labels_1 + labels_2, loc='best')
    
    # Add explanatory text for derivatives
    deriv_text = (
        "DERIVATIVES ANALYSIS:\n"
        f"The model's derivative (dy/dx) = {slope:.4f}\n\n"
        "For a linear model, the derivative is constant and equal to the slope.\n"
        "This represents how much y changes when x increases by 1 unit."
    )
    
    ax.text(0.5, -0.2, deriv_text, ha='center', va='center', 
            transform=ax.transAxes, bbox=dict(facecolor='white', alpha=0.8))
    
    plt.tight_layout()
    plt.savefig(derivatives_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # 3. Model's possessions: Errors that belong to the model
    errors_path = os.path.join(case_dir, "errors_possession.png")
    
    # Calculate errors for original data
    y_pred_orig = model.predict(X)
    errors = y - y_pred_orig
    mse = mean_squared_error(y, y_pred_orig)
    
    # Create error visualization
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Scatter plot of actual vs predicted
    ax.scatter(y, y_pred_orig, alpha=0.7, label='Data Points')
    
    # Perfect prediction line
    min_val = min(y.min(), y_pred_orig.min())
    max_val = max(y.max(), y_pred_orig.max())
    ax.plot([min_val, max_val], [min_val, max_val], 'k--', label='Perfect Predictions')
    
    # Style the chart
    ax.set_xlabel('Actual Values', fontsize=12)
    ax.set_ylabel('Predicted Values', fontsize=12)
    ax.set_title("GENITIVE Case: Errors Belonging to the Model", fontsize=14)
    ax.grid(True, alpha=0.3)
    ax.legend()
    
    # Add error distribution as an inset
    axins = ax.inset_axes([0.6, 0.1, 0.35, 0.35])
    sns.histplot(errors, kde=True, ax=axins, color='red', alpha=0.6)
    axins.set_title('Error Distribution')
    axins.set_xlabel('Error')
    axins.grid(False)
    
    # Add text explaining errors
    error_text = (
        "ERROR ANALYSIS:\n"
        f"Mean Squared Error (MSE): {mse:.4f}\n"
        f"Root Mean Squared Error (RMSE): {np.sqrt(mse):.4f}\n"
        f"Mean Absolute Error (MAE): {np.mean(np.abs(errors)):.4f}\n\n"
        "Errors belong to the model's predictions.\n"
        "The histogram shows the distribution of these errors."
    )
    
    ax.text(0.05, 0.95, error_text, transform=ax.transAxes, fontsize=10,
            va='top', ha='left', bbox=dict(facecolor='white', alpha=0.8))
    
    plt.tight_layout()
    plt.savefig(errors_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # 4. Combined possessions visualization (Parameters, Errors, Derivatives)
    combined_path = os.path.join(case_dir, "combined_possession.png")
    
    # Create a comprehensive visualization of everything belonging to the model
    fig, axs = plt.subplots(2, 2, figsize=(14, 12))
    fig.suptitle(f"GENITIVE Case: All Possessions of the Linear Regression Model", fontsize=16)
    
    # 1. Parameters (top-left)
    bars = axs[0, 0].bar(param_names, param_values, color=['skyblue', 'lightgreen'], alpha=0.7)
    for bar, val in zip(bars, param_values):
        axs[0, 0].text(bar.get_x() + bar.get_width()/2, val + 0.05 * np.sign(val),
                     f'{val:.4f}', ha='center', va='bottom' if val > 0 else 'top',
                     fontweight='bold')
    axs[0, 0].set_title("Model Parameters")
    axs[0, 0].set_ylabel("Value")
    axs[0, 0].grid(axis='y', linestyle='--', alpha=0.7)
    
    # 2. Model fit (top-right)
    axs[0, 1].scatter(X, y, alpha=0.7, label='Data')
    axs[0, 1].plot(x_test, y_pred, 'r-', linewidth=2, label='Model Fit')
    # Add equation to plot
    eq_text = f'y = {intercept:.4f} + {slope:.4f}x'
    axs[0, 1].text(0.05, 0.95, eq_text, transform=axs[0, 1].transAxes, fontsize=12,
                  va='top', ha='left', bbox=dict(facecolor='white', alpha=0.8))
    axs[0, 1].set_title("Model Fit")
    axs[0, 1].set_xlabel("x")
    axs[0, 1].set_ylabel("y")
    axs[0, 1].legend()
    axs[0, 1].grid(True, alpha=0.3)
    
    # 3. Errors (bottom-left)
    # Sort by x for connected error lines
    sorted_indices = np.argsort(X.flatten())
    X_sorted = X[sorted_indices].flatten()
    y_sorted = y[sorted_indices].flatten()
    y_pred_sorted = y_pred_orig[sorted_indices].flatten()
    
    axs[1, 0].scatter(X_sorted, y_sorted, alpha=0.7, color='blue', label='Actual')
    axs[1, 0].scatter(X_sorted, y_pred_sorted, alpha=0.7, color='red', label='Predicted')
    
    # Draw error lines
    for i in range(len(X_sorted)):
        axs[1, 0].plot([X_sorted[i], X_sorted[i]], [y_sorted[i], y_pred_sorted[i]], 
                     'k-', alpha=0.3)
    
    axs[1, 0].set_title("Prediction Errors")
    axs[1, 0].set_xlabel("x")
    axs[1, 0].set_ylabel("y")
    axs[1, 0].text(0.05, 0.95, f'MSE: {mse:.4f}', transform=axs[1, 0].transAxes, fontsize=10,
                  va='top', ha='left', bbox=dict(facecolor='white', alpha=0.8))
    axs[1, 0].legend()
    axs[1, 0].grid(True, alpha=0.3)
    
    # 4. Error histogram (bottom-right)
    sns.histplot(errors, kde=True, ax=axs[1, 1], color='purple', alpha=0.6)
    axs[1, 1].set_title("Error Distribution")
    axs[1, 1].set_xlabel("Error")
    axs[1, 1].axvline(x=0, color='red', linestyle='--', alpha=0.7)
    
    # Add error statistics
    error_stats = (
        f"Error Statistics:\n"
        f"Mean: {np.mean(errors):.4f}\n"
        f"Std Dev: {np.std(errors):.4f}\n"
        f"Min: {np.min(errors):.4f}\n"
        f"Max: {np.max(errors):.4f}"
    )
    
    axs[1, 1].text(0.95, 0.95, error_stats, transform=axs[1, 1].transAxes, fontsize=10,
                  va='top', ha='right', bbox=dict(facecolor='white', alpha=0.8))
    
    plt.tight_layout()
    plt.subplots_adjust(top=0.92)  # Make room for the suptitle
    plt.savefig(combined_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # 5. Create a table of model possessions
    possessions_path = os.path.join(case_dir, "model_possessions.png")
    
    # Create a dataframe of model possessions
    possessions = {
        'Possession Type': [
            'Intercept', 
            'Slope',
            'Mean Squared Error',
            'Root Mean Squared Error',
            'Mean Absolute Error',
            'Error Variance',
            'Derivative (dy/dx)',
            'R-squared',
            'Number of Data Points',
            'Prediction Range'
        ],
        'Value': [
            f'{intercept:.6f}',
            f'{slope:.6f}',
            f'{mse:.6f}',
            f'{np.sqrt(mse):.6f}',
            f'{np.mean(np.abs(errors)):.6f}',
            f'{np.var(errors):.6f}',
            f'{slope:.6f}',
            f'{1 - np.sum(errors**2) / np.sum((y - np.mean(y))**2):.6f}',
            f'{len(X)}',
            f'[{y_pred.min():.4f}, {y_pred.max():.4f}]'
        ],
        'Description': [
            'Constant term in the linear equation (β₀)',
            'Coefficient for x in the linear equation (β₁)',
            'Average of squared differences between predicted and actual values',
            'Square root of MSE, in same units as target variable',
            'Average of absolute differences between predicted and actual values',
            'Variance of the prediction errors',
            'Rate of change of y with respect to x',
            'Proportion of variance in y explained by the model',
            'Total observations used to fit the model',
            'Range of predicted values across the x domain'
        ]
    }
    
    df_possessions = pd.DataFrame(possessions)
    
    # Create table visualization
    fig, ax = plt.subplots(figsize=(12, 10))
    ax.axis('off')
    
    # Create table
    table = ax.table(
        cellText=df_possessions.values,
        colLabels=df_possessions.columns,
        loc='center',
        cellLoc='left',
        colWidths=[0.2, 0.15, 0.65]
    )
    
    # Style the table
    table.auto_set_font_size(False)
    table.set_fontsize(12)
    table.scale(1, 1.8)
    
    # Style header
    for i, key in enumerate(df_possessions.columns):
        cell = table[(0, i)]
        cell.set_text_props(fontweight='bold')
        cell.set_facecolor('#4472C4')
        cell.set_text_props(color='white')
    
    # Alternate row colors
    for row in range(1, len(df_possessions) + 1):
        for col in range(len(df_possessions.columns)):
            cell = table[(row, col)]
            cell.set_facecolor('#E6F0FF' if row % 2 == 0 else '#C5D9F1')
    
    plt.title("GENITIVE Case: Complete Inventory of Model Possessions", fontsize=16, pad=20)
    plt.tight_layout()
    plt.savefig(possessions_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # 6. Document all results for this case
    with open(os.path.join(case_dir, "genitive_results.txt"), 'w') as f:
        f.write(f"GENITIVE CASE RESULTS\n")
        f.write(f"====================\n\n")
        
        f.write(f"Linguistic meaning: {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n")
        f.write(f"Regression context: {case_info['regression_context']}\n\n")
        
        f.write(f"Model Parameters (what the model possesses):\n")
        f.write(f"- Intercept: {intercept:.6f}\n")
        f.write(f"- Slope: {slope:.6f}\n")
        f.write(f"- Model Formula: y = {intercept:.6f} + {slope:.6f}x\n\n")
        
        f.write(f"Model Errors (what the model's predictions possess):\n")
        f.write(f"- Mean Squared Error (MSE): {mse:.6f}\n")
        f.write(f"- Root Mean Squared Error (RMSE): {np.sqrt(mse):.6f}\n")
        f.write(f"- Mean Absolute Error (MAE): {np.mean(np.abs(errors)):.6f}\n")
        f.write(f"- Error Variance: {np.var(errors):.6f}\n")
        f.write(f"- Error Range: [{np.min(errors):.6f}, {np.max(errors):.6f}]\n\n")
        
        f.write(f"Model Derivatives (model's sensitivity):\n")
        f.write(f"- Derivative (dy/dx): {slope:.6f}\n\n")
        
        f.write(f"Additional Properties:\n")
        f.write(f"- R-squared: {1 - np.sum(errors**2) / np.sum((y - np.mean(y))**2):.6f}\n")
        f.write(f"- Number of Data Points: {len(X)}\n")
        f.write(f"- Prediction Range: [{y_pred.min():.6f}, {y_pred.max():.6f}]\n\n")
        
        f.write(f"Linguistic Formula Example:\n")
        f.write(f"{case_info['example']}\n\n")
    
    logger.info(f"Completed GENITIVE case test with visualizations in {case_dir}")
    
    return model 