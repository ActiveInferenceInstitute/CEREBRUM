#!/usr/bin/env python3
"""
INSTRUMENTAL Case Test Module for CEREBRUM
Tests the INSTRUMENTAL case in the linear regression context
"""

import os
import logging
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

from models.base import Case
from models.case_definitions import CaseDefinitions
from models.linear_regression import LinearRegressionModel
from utils.visualization import plot_case_linguistic_context

# Setup logging
logger = logging.getLogger("cerebrum-instrumental-test")

def test_instrumental_case(linear_test_data, output_dir):
    """
    Test INSTRUMENTAL case: The model as an instrument or tool.
    
    In linguistics: The means by which an action is accomplished.
    In regression: Model used as a tool for analysis.
    """
    # Get case definitions
    case_definitions = CaseDefinitions.get_all_cases()
    
    # Get case info for logging
    case_info = case_definitions[Case.INSTRUMENTAL]
    logger.info(f"Testing {Case.INSTRUMENTAL.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(output_dir, Case.INSTRUMENTAL.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.INSTRUMENTAL, linguistics_path)
    
    # 1. Create an INSTRUMENTAL case model as a tool for analysis
    model = LinearRegressionModel(model_id=f"{Case.INSTRUMENTAL.value}_model", case=Case.INSTRUMENTAL)
    
    # 2. First fit the model with training data (using NOMINATIVE temporarily)
    X, y = linear_test_data
    
    # Temporarily switch to NOMINATIVE for fitting
    original_case = model.case
    model.case = Case.NOMINATIVE
    model.fit(X, y)
    
    # Switch back to INSTRUMENTAL
    model.case = original_case
    
    logger.info(f"INSTRUMENTAL case demonstration: Using model as a tool for analysis")
    
    # 3. Use the model as a tool for feature analysis (INSTRUMENTAL role)
    # Create synthetic data with multiple features
    n_samples = 150
    np.random.seed(42)
    
    # Generate features with different importance levels
    X_multi = np.random.randn(n_samples, 4)  # 4 features
    
    # Create target with known coefficients
    true_coefficients = np.array([0.5, 2.0, -1.0, 0.1])
    y_multi = 3.0 + np.dot(X_multi, true_coefficients) + np.random.randn(n_samples) * 0.5
    
    # Fit a new model with multiple features
    multi_model = LinearRegressionModel(model_id="multi_feature_model", case=Case.INSTRUMENTAL)
    multi_model.fit(X_multi, y_multi)
    
    # Get the fitted coefficients
    coefficients = multi_model._params['coefficients']
    intercept = multi_model._params['intercept']
    
    # 4. Visualize feature importance (INSTRUMENTAL analysis)
    importance_vis_path = os.path.join(case_dir, "feature_importance.png")
    
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 6))
    
    # Bar chart of coefficients (absolute values)
    feature_names = [f"Feature {i+1}" for i in range(len(coefficients))]
    colors = ['green' if c > 0 else 'red' for c in coefficients]
    
    ax1.bar(feature_names, np.abs(coefficients), color=colors, alpha=0.7)
    ax1.set_xlabel('Features')
    ax1.set_ylabel('Absolute Coefficient Value')
    ax1.set_title('Feature Importance (Absolute Value)')
    
    # Add values on top of bars
    for i, v in enumerate(np.abs(coefficients)):
        ax1.text(i, v + 0.05, f"{v:.3f}", ha='center')
    
    # Add sign indicators
    for i, c in enumerate(coefficients):
        sign = '+' if c > 0 else '-'
        ax1.text(i, -0.1, sign, ha='center', fontweight='bold', color=colors[i], fontsize=14)
    
    # Contribution to variance
    X_std = (X_multi - X_multi.mean(axis=0)) / X_multi.std(axis=0)
    contributions = np.abs(coefficients * X_multi.std(axis=0))
    contributions = contributions / np.sum(contributions)
    
    ax2.pie(contributions, labels=feature_names, autopct='%1.1f%%', 
           colors=plt.cm.viridis(np.linspace(0, 0.8, len(coefficients))),
           startangle=90)
    ax2.set_title('Contribution to Predicted Variance')
    
    plt.tight_layout()
    plt.savefig(importance_vis_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # 5. Create animation showing sensitivity analysis (INSTRUMENTAL usage)
    sensitivity_anim_path = os.path.join(case_dir, "sensitivity_analysis.gif")
    
    # Generate a grid of points for visualization
    n_grid = 100
    feature_idx = 0  # Feature to analyze
    x_grid = np.linspace(X_multi[:, feature_idx].min(), X_multi[:, feature_idx].max(), n_grid)
    
    fig, ax = plt.subplots(figsize=(10, 6))
    
    # Animation function
    def update(frame):
        ax.clear()
        
        # Plot the actual data points
        ax.scatter(X_multi[:, feature_idx], y_multi, color='blue', alpha=0.3, label='Data points')
        
        # Vary the coefficient for the chosen feature
        coef_multiplier = 0.5 + frame / 20  # Range from 0.5 to 1.5 times original
        adjusted_coef = coefficients[feature_idx] * coef_multiplier
        
        # Create predictions using the adjusted coefficient
        modified_coefficients = coefficients.copy()
        modified_coefficients[feature_idx] = adjusted_coef
        
        # Make predictions for grid points
        # We need to create synthetic data where we only vary the feature of interest
        X_synthetic = np.zeros((n_grid, X_multi.shape[1]))
        X_synthetic[:, feature_idx] = x_grid
        
        # For other features, use mean values
        for i in range(X_multi.shape[1]):
            if i != feature_idx:
                X_synthetic[:, i] = X_multi[:, i].mean()
        
        # Generate predictions
        predictions = intercept + np.dot(X_synthetic, modified_coefficients)
        
        # Plot the regression line
        ax.plot(x_grid, predictions, color='red', lw=2, 
                label=f'Coefficient: {adjusted_coef:.3f} ({"+" if coef_multiplier > 1 else "-"}{abs(coef_multiplier-1)*100:.0f}%)')
        
        # Add reference line (original coefficient)
        original_predictions = intercept + np.dot(X_synthetic, coefficients)
        ax.plot(x_grid, original_predictions, color='green', lw=1, linestyle='--', 
                label=f'Original: {coefficients[feature_idx]:.3f}')
        
        # Set labels and title
        ax.set_xlabel(f'Feature {feature_idx+1}')
        ax.set_ylabel('Target')
        ax.set_title(f'Sensitivity Analysis (INSTRUMENTAL Case)')
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        # Add information text
        info_text = (f"Coefficient Sensitivity Analysis\n"
                     f"Original coefficient: {coefficients[feature_idx]:.3f}\n"
                     f"Modified coefficient: {adjusted_coef:.3f}\n"
                     f"Change: {(coef_multiplier-1)*100:+.1f}%\n\n"
                     f"Impact on predictions:\n"
                     f"Mean change: {np.mean(predictions - original_predictions):.3f}")
        
        ax.text(0.02, 0.98, info_text, transform=ax.transAxes, 
                bbox=dict(facecolor='white', alpha=0.8), va='top')
        
        return [ax]
    
    # Create and save animation
    anim = FuncAnimation(fig, update, frames=21, blit=True)
    anim.save(sensitivity_anim_path, writer='pillow', fps=5, dpi=100)
    plt.close(fig)
    
    # 6. Create interaction plots (another INSTRUMENTAL analysis)
    interaction_vis_path = os.path.join(case_dir, "feature_interactions.png")
    
    fig, axs = plt.subplots(2, 3, figsize=(15, 10))
    axs = axs.flatten()
    
    # Plot pairwise interactions
    plot_idx = 0
    for i in range(X_multi.shape[1]):
        for j in range(i+1, X_multi.shape[1]):
            if plot_idx >= len(axs):
                break
                
            # Create 2D scatter with prediction surface
            scatter = axs[plot_idx].scatter(X_multi[:, i], X_multi[:, j], c=y_multi, 
                                          cmap='viridis', alpha=0.7, s=50)
            
            axs[plot_idx].set_xlabel(f'Feature {i+1}')
            axs[plot_idx].set_ylabel(f'Feature {j+1}')
            axs[plot_idx].set_title(f'Interaction: Features {i+1} & {j+1}')
            
            # Add correlation coefficient
            corr = np.corrcoef(X_multi[:, i], X_multi[:, j])[0, 1]
            axs[plot_idx].text(0.05, 0.95, f'Correlation: {corr:.3f}', transform=axs[plot_idx].transAxes,
                              bbox=dict(facecolor='white', alpha=0.8))
            
            plot_idx += 1
    
    # Remove unused subplots
    for i in range(plot_idx, len(axs)):
        fig.delaxes(axs[i])
    
    # Add colorbar
    cbar = fig.colorbar(scatter, ax=axs, shrink=0.8)
    cbar.set_label('Target Value')
    
    plt.tight_layout()
    plt.savefig(interaction_vis_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # 7. Document all outputs for this case
    with open(os.path.join(case_dir, "instrumental_results.txt"), 'w') as f:
        f.write(f"INSTRUMENTAL CASE RESULTS\n")
        f.write(f"========================\n\n")
        f.write(f"Linguistic meaning: {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n")
        f.write(f"Regression context: {case_info['regression_context']}\n\n")
        
        f.write(f"Model as Analysis Tool:\n")
        f.write(f"- Coefficients (importance): {', '.join([f'{c:.4f}' for c in coefficients])}\n")
        f.write(f"- Intercept: {intercept:.4f}\n")
        f.write(f"- Feature with highest impact: Feature {np.argmax(np.abs(coefficients))+1}\n")
        f.write(f"- Feature with lowest impact: Feature {np.argmin(np.abs(coefficients))+1}\n\n")
        
        f.write(f"Variance Contribution Analysis:\n")
        for i, contrib in enumerate(contributions):
            f.write(f"- Feature {i+1}: {contrib*100:.2f}% of variance\n")
        
        f.write(f"\nLinguistic Formula Example:\n")
        f.write(f"{case_info['example']}\n\n")
            
    logger.info(f"Completed INSTRUMENTAL case test with visualizations in {case_dir}")
    
    return model 