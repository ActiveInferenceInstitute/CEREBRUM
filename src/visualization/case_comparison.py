"""
Visualization utilities for comparing case implementations across model types.

This module provides functionality to generate consistent visualizations
for comparing different case implementations across model types (POMDP, 
Neural Network, and Linear Regression).
"""

import os
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec
import matplotlib.animation as animation
from typing import Dict, List, Any, Optional, Tuple, Union, Callable
from PIL import Image
import io
import logging

from src.core.model import Model, Case
from src.visualization.case_visualization import CASE_COLORS, CASE_SYMBOLS

logger = logging.getLogger(__name__)

class CaseComparisonVisualizer:
    """Utility class for generating cross-model visualizations for each case."""
    
    @staticmethod
    def create_comparison_grid(
        case: Case,
        output_dir: str,
        pomdp_model: Optional[Model] = None,
        nn_model: Optional[Model] = None,
        linear_model: Optional[Model] = None,
        figsize: Tuple[int, int] = (15, 10)
    ) -> str:
        """
        Create a comparison grid visualization for a specific case across model types.
        
        Args:
            case: The case to visualize
            output_dir: Directory to save visualizations
            pomdp_model: POMDP model instance (optional)
            nn_model: Neural Network model instance (optional)
            linear_model: Linear Regression model instance (optional)
            figsize: Figure size
            
        Returns:
            Path to the saved visualization
        """
        os.makedirs(output_dir, exist_ok=True)
        output_path = os.path.join(output_dir, f"{case.value.lower()}_model_comparison.png")
        
        fig = plt.figure(figsize=figsize)
        gs = GridSpec(3, 3, figure=fig, height_ratios=[1, 3, 1])
        
        # Title area with case information
        ax_title = fig.add_subplot(gs[0, :])
        ax_title.axis('off')
        ax_title.set_title(f"{case.value} Case Comparison Across Model Types", fontsize=16)
        
        # Display case information
        ax_title.text(0.5, 0.5, 
            f"Linguistic meaning: Model as {case_linguistic_meanings[case]}\n"
            f"Statistical role: {case_statistical_roles[case]}",
            ha='center', va='center', fontsize=12,
            bbox=dict(boxstyle='round,pad=0.5', facecolor=CASE_COLORS[case], alpha=0.1))
        
        # Model columns
        model_titles = ["POMDP Model", "Neural Network Model", "Linear Regression Model"]
        model_instances = [pomdp_model, nn_model, linear_model]
        
        for i, (title, model) in enumerate(zip(model_titles, model_instances)):
            # Model title
            ax_header = fig.add_subplot(gs[1, i])
            ax_header.set_title(title, fontsize=14)
            
            if model is not None:
                # Get model-specific data for visualization
                if i == 0:  # POMDP
                    CaseComparisonVisualizer._plot_pomdp_representation(ax_header, model, case)
                elif i == 1:  # Neural Network
                    CaseComparisonVisualizer._plot_nn_representation(ax_header, model, case)
                elif i == 2:  # Linear Regression
                    CaseComparisonVisualizer._plot_linear_representation(ax_header, model, case)
            else:
                ax_header.text(0.5, 0.5, "Model not available", ha='center', va='center')
                ax_header.axis('off')
        
        # Footer with common elements across models
        ax_footer = fig.add_subplot(gs[2, :])
        ax_footer.axis('off')
        ax_footer.text(0.5, 0.5, 
            f"Common properties for {case.value} case:\n"
            f"Symbol: {CASE_SYMBOLS[case]}   Color: {CASE_COLORS[case]}",
            ha='center', va='center', fontsize=12,
            bbox=dict(boxstyle='round,pad=0.5', facecolor='lightgray', alpha=0.5))
        
        plt.tight_layout()
        fig.savefig(output_path, dpi=300, bbox_inches='tight')
        plt.close(fig)
        
        logger.info(f"Created {case.value} case comparison visualization at {output_path}")
        return output_path
    
    @staticmethod
    def _plot_pomdp_representation(ax, model, case):
        """Plot POMDP-specific visualization for a case."""
        if case == Case.NOMINATIVE:
            # Active agent role visualization
            if hasattr(model, 'belief_state'):
                ax.bar(range(len(model.belief_state)), model.belief_state, color=CASE_COLORS[case])
                ax.set_xlabel("State")
                ax.set_ylabel("Belief Probability")
                ax.set_ylim(0, 1)
            else:
                ax.text(0.5, 0.5, "Agent-centered view\n(Belief state not available)", ha='center', va='center')
                ax.axis('off')
        
        elif case == Case.ACCUSATIVE:
            # Object of action visualization (transition/observation matrices)
            if hasattr(model, 'parameters') and 'transition_matrix' in model.parameters:
                ax.imshow(model.parameters['transition_matrix'], cmap='Blues', aspect='auto')
                ax.set_title("Transition Matrix (Model Actions)")
                ax.set_xlabel("Next State")
                ax.set_ylabel("Current State")
            else:
                ax.text(0.5, 0.5, "Transition dynamics\n(Matrix not available)", ha='center', va='center')
                ax.axis('off')
        
        elif case == Case.INSTRUMENTAL:
            # Computational tool visualization
            ax.text(0.5, 0.5, "POMDP as computational method\nShowing algorithm efficiency", 
                   ha='center', va='center')
            ax.axis('off')
        
        else:
            # Default visualization for other cases
            ax.text(0.5, 0.5, f"POMDP in {case.value} case\n(Custom visualization)", ha='center', va='center')
            ax.axis('off')
    
    @staticmethod
    def _plot_nn_representation(ax, model, case):
        """Plot Neural Network-specific visualization for a case."""
        if case == Case.NOMINATIVE:
            # Network as active agent
            ax.text(0.5, 0.5, "Neural Network as active agent\nMaking predictions/decisions", 
                   ha='center', va='center')
            ax.axis('off')
        
        elif case == Case.ACCUSATIVE:
            # Network receiving training updates
            ax.text(0.5, 0.5, "Neural Network as training target\nReceiving gradient updates", 
                   ha='center', va='center')
            ax.axis('off')
        
        elif case == Case.DATIVE:
            # Network receiving data
            ax.text(0.5, 0.5, "Neural Network receiving data\nData preparation & feeding", 
                   ha='center', va='center')
            ax.axis('off')
            
        else:
            # Default visualization for other cases
            ax.text(0.5, 0.5, f"Neural Network in {case.value} case\n(Custom visualization)", 
                   ha='center', va='center')
            ax.axis('off')
    
    @staticmethod
    def _plot_linear_representation(ax, model, case):
        """Plot Linear Regression-specific visualization for a case."""
        if case == Case.NOMINATIVE:
            # Linear model as agent making predictions
            if hasattr(model, 'coefficients_') and hasattr(model, 'intercept_'):
                # Show the equation form
                if len(model.coefficients_) == 1:
                    equation = f"y = {model.intercept_:.2f} + {model.coefficients_[0]:.2f}x"
                else:
                    equation = f"y = {model.intercept_:.2f} + " + " + ".join([f"{c:.2f}x_{i+1}" for i, c in enumerate(model.coefficients_)])
                ax.text(0.5, 0.5, f"Linear Model Equation:\n{equation}", ha='center', va='center')
            else:
                ax.text(0.5, 0.5, "Linear model as predictor\n(Coefficients not available)", ha='center', va='center')
            ax.axis('off')
            
        elif case == Case.INSTRUMENTAL:
            # Linear model as computational tool
            ax.text(0.5, 0.5, "Linear Regression as method\nNormal Equation computation", 
                   ha='center', va='center')
            ax.axis('off')
            
        else:
            # Default visualization for other cases
            ax.text(0.5, 0.5, f"Linear Regression in {case.value} case\n(Custom visualization)", 
                   ha='center', va='center')
            ax.axis('off')
    
    @staticmethod
    def create_all_case_comparisons(
        output_dir: str,
        pomdp_models: Dict[Case, Model] = None,
        nn_models: Dict[Case, Model] = None,
        linear_models: Dict[Case, Model] = None
    ) -> List[str]:
        """
        Create comparison visualizations for all cases.
        
        Args:
            output_dir: Directory to save visualizations
            pomdp_models: Dictionary of POMDP models keyed by case
            nn_models: Dictionary of Neural Network models keyed by case
            linear_models: Dictionary of Linear Regression models keyed by case
            
        Returns:
            List of paths to saved visualizations
        """
        os.makedirs(output_dir, exist_ok=True)
        output_paths = []
        
        for case in Case:
            pomdp_model = pomdp_models.get(case) if pomdp_models else None
            nn_model = nn_models.get(case) if nn_models else None
            linear_model = linear_models.get(case) if linear_models else None
            
            path = CaseComparisonVisualizer.create_comparison_grid(
                case=case,
                output_dir=output_dir,
                pomdp_model=pomdp_model,
                nn_model=nn_model,
                linear_model=linear_model
            )
            output_paths.append(path)
        
        # Create a summary visualization
        summary_path = os.path.join(output_dir, "all_cases_summary.png")
        CaseComparisonVisualizer._create_summary_visualization(output_paths, summary_path)
        output_paths.append(summary_path)
        
        return output_paths
    
    @staticmethod
    def _create_summary_visualization(case_paths: List[str], output_path: str) -> None:
        """Create a summary visualization of all cases."""
        # Load all case visualizations
        images = [Image.open(path) for path in case_paths]
        
        # Determine grid layout
        n_cases = len(images)
        cols = 2
        rows = (n_cases + cols - 1) // cols  # Ceiling division
        
        # Create a new figure
        fig = plt.figure(figsize=(18, rows * 8))
        
        # Add each case visualization
        for i, img in enumerate(images):
            if i < n_cases:
                ax = fig.add_subplot(rows, cols, i + 1)
                ax.imshow(np.array(img))
                ax.axis('off')
                
                # Get case name from path
                case_name = os.path.basename(case_paths[i]).split('_')[0].upper()
                ax.set_title(f"{case_name} Case", fontsize=14)
        
        plt.tight_layout()
        fig.savefig(output_path, dpi=200, bbox_inches='tight')
        plt.close(fig)
        
        logger.info(f"Created summary visualization of all cases at {output_path}")

# Define case-specific meanings and roles for reference
case_linguistic_meanings = {
    Case.NOMINATIVE: "agent/subject",
    Case.ACCUSATIVE: "direct object/target",
    Case.DATIVE: "indirect object/recipient",
    Case.GENITIVE: "source/possessor",
    Case.INSTRUMENTAL: "tool/method",
    Case.LOCATIVE: "location/context",
    Case.ABLATIVE: "origin/source of change",
    Case.VOCATIVE: "addressee/interface"
}

case_statistical_roles = {
    Case.NOMINATIVE: "Model making predictions/decisions",
    Case.ACCUSATIVE: "Model receiving updates/being trained",
    Case.DATIVE: "Model receiving data/inputs",
    Case.GENITIVE: "Model generating outputs/predictions",
    Case.INSTRUMENTAL: "Model as computational method/algorithm",
    Case.LOCATIVE: "Model as representational space",
    Case.ABLATIVE: "Model as source of errors/gradients",
    Case.VOCATIVE: "Model as interface for querying/addressing"
}

def get_case_info(case: Case) -> Dict[str, str]:
    """Get standardized information about a grammatical case for visualizations."""
    return {
        "name": case.value,
        "linguistic_meaning": case_linguistic_meanings[case],
        "statistical_role": case_statistical_roles[case],
        "color": CASE_COLORS[case],
        "symbol": CASE_SYMBOLS[case]
    } 