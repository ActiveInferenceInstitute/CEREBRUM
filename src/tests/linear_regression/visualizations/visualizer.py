"""
Visualization utilities for linear regression tests.
"""
import os
import sys
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from typing import Dict, List, Tuple, Any, Optional

# Add the src directory to the path for imports if needed
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..')))

# Import CEREBRUM components
from core.model import Case


class Visualizer:
    """Visualization utilities for linear regression tests."""
    
    @staticmethod
    def plot_data(
        X: np.ndarray,
        y: np.ndarray,
        title: str = "Data Visualization",
        xlabel: str = "X",
        ylabel: str = "y",
        figsize: Tuple[int, int] = (10, 6),
        save_path: Optional[str] = None
    ) -> plt.Figure:
        """
        Plot data points.
        
        Args:
            X: Feature matrix
            y: Target vector
            title: Plot title
            xlabel: Label for x-axis
            ylabel: Label for y-axis
            figsize: Figure size (width, height) in inches
            save_path: Path to save the plot (if None, plot is not saved)
            
        Returns:
            Figure object
        """
        # Flatten input arrays for plotting (if they're not already 1D)
        X_flat = X.flatten() if X.ndim > 1 else X
        y_flat = y.flatten() if y.ndim > 1 else y
        
        # Create figure and axis
        fig, ax = plt.subplots(figsize=figsize)
        
        # Plot data
        ax.scatter(X_flat, y_flat, alpha=0.7, s=50, edgecolor='k')
        
        # Set labels and title
        ax.set_xlabel(xlabel)
        ax.set_ylabel(ylabel)
        ax.set_title(title)
        
        # Add grid
        ax.grid(True, alpha=0.3)
        
        # Save if a path is provided
        if save_path is not None:
            fig.tight_layout()
            fig.savefig(save_path, dpi=300)
        
        return fig
    
    @staticmethod
    def plot_regression_fit(
        X: np.ndarray,
        y: np.ndarray,
        model: Any,  # LinearRegressionModel
        title: str = "Regression Fit",
        xlabel: str = "X",
        ylabel: str = "y",
        figsize: Tuple[int, int] = (10, 6),
        save_path: Optional[str] = None,
        ax: Optional[plt.Axes] = None  # Add optional axes argument
    ) -> Optional[plt.Figure]:
        """
        Plot data with regression line.
        
        Args:
            X: Feature matrix
            y: Target vector
            model: LinearRegressionModel instance
            title: Plot title
            xlabel: Label for x-axis
            ylabel: Label for y-axis
            figsize: Figure size (width, height) in inches
            save_path: Path to save the plot (if None, plot is not saved)
            ax: Optional matplotlib Axes (if provided, plot on this axes)
            
        Returns:
            Figure object (None if ax is provided)
        """
        # Flatten input arrays for plotting (if they're not already 1D)
        X_flat = X.flatten() if X.ndim > 1 else X
        y_flat = y.flatten() if y.ndim > 1 else y
        
        # Get predictions from the model
        y_pred = model.predict(X)
        y_pred_flat = y_pred.flatten() if y_pred.ndim > 1 else y_pred
        
        # Create figure and axis if not provided
        if ax is None:
            fig, ax = plt.subplots(figsize=figsize)
            return_fig = True
        else:
            fig = ax.figure
            return_fig = False
            
        # Plot data
        ax.scatter(X_flat, y_flat, alpha=0.7, s=50, edgecolor='k', label='Data')
        
        # Sort X to get a smooth line
        sort_idx = np.argsort(X_flat)
        X_sorted = X_flat[sort_idx]
        y_pred_sorted = y_pred_flat[sort_idx]
        
        # Plot regression line
        ax.plot(X_sorted, y_pred_sorted, 'r-', linewidth=2, label='Regression Line')
        
        # Set labels and title
        ax.set_xlabel(xlabel)
        ax.set_ylabel(ylabel)
        ax.set_title(title)
        
        # Add grid and legend
        ax.grid(True, alpha=0.3)
        ax.legend()
        
        # Add equation as text
        if hasattr(model, 'intercept_') and hasattr(model, 'coefficients_'):
            intercept = model.intercept_
            coefficient = model.coefficients_[0] if len(model.coefficients_) > 0 else 0
            equation = f"y = {intercept:.4f} + {coefficient:.4f} · x"
            ax.text(0.05, 0.95, equation, transform=ax.transAxes, fontsize=10,
                   verticalalignment='top', bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
        
        # Save if a path is provided
        if save_path is not None:
            fig.tight_layout()
            fig.savefig(save_path, dpi=300)
        
        # Return the figure if we created it
        if return_fig:
            return fig
        return None
    
    @staticmethod
    def create_overview_figure(save_path: str, include_linguistics: bool = True) -> None:
        """
        Create an overview figure showing all CEREBRUM cases in the context of linear regression.
        
        Args:
            save_path: Path to save the figure
            include_linguistics: Whether to include linguistic explanations
        """
        n_cases = 8
        cols = 2
        rows = n_cases // cols + (1 if include_linguistics else 0)
        
        # Create figure with a 2x4 grid (+ extra row for linguistics if requested)
        fig = plt.figure(figsize=(15, 4 * rows))
        
        # Define cases and their descriptions
        cases = [
            (Case.NOMINATIVE, "NOM", "Model as Subject", "The MODEL fits the data"),
            (Case.ACCUSATIVE, "ACC", "Model as Direct Object", "We evaluate the MODEL"),
            (Case.DATIVE, "DAT", "Model as Indirect Object", "We give data TO the MODEL"),
            (Case.GENITIVE, "GEN", "Model as Possessor", "Predictions BELONG TO the MODEL"),
            (Case.INSTRUMENTAL, "INS", "Model as Instrument", "Solve WITH the MODEL"),
            (Case.LOCATIVE, "LOC", "Model as Location", "Patterns exist WITHIN the MODEL"),
            (Case.ABLATIVE, "ABL", "Model as Origin", "Errors come FROM the MODEL"),
            (Case.VOCATIVE, "VOC", "Model as Addressee", "HEY MODEL, predict this!"),
        ]
        
        # Colors for each case
        colors = plt.cm.tab10(np.linspace(0, 1, n_cases))
        
        # Create the grid layout
        if include_linguistics:
            # Add a row for linguistics above the cases
            gs = plt.GridSpec(rows, cols, height_ratios=[1] + [2] * (rows-1))
            
            # Create the linguistics subplot spanning the entire top row
            ling_ax = fig.add_subplot(gs[0, :])
            ling_ax.axis('off')
            
            # Add linguistics explanation
            ling_ax.text(0.5, 0.8, "CEREBRUM Case Framework for Linear Regression",
                       ha='center', fontsize=16, fontweight='bold')
            
            ling_ax.text(0.5, 0.5, 
                       "The CEREBRUM framework applies linguistic case roles to AI models,\n"
                       "enabling systematic organization of model interactions and behaviors.",
                       ha='center', fontsize=12)
            
            ling_ax.text(0.5, 0.2,
                       "Each case below represents a different functional role a linear regression model can take.",
                       ha='center', fontsize=12)
            
            # Offset for the case subplots (skipping the linguistics row)
            row_offset = 1
        else:
            gs = plt.GridSpec(rows, cols)
            row_offset = 0
        
        # Create a subplot for each case
        for i, (case, code, role, example) in enumerate(cases):
            row = i // cols + row_offset
            col = i % cols
            
            # Create subplot
            ax = fig.add_subplot(gs[row, col])
            
            # Set title with case code and role
            ax.set_title(f"{code}: {role}", fontsize=14, color=colors[i])
            
            # Draw a box representing the model
            model_rect = plt.Rectangle((0.3, 0.5), 0.4, 0.3, facecolor=colors[i], alpha=0.3,
                                     edgecolor='black', linewidth=2)
            ax.add_patch(model_rect)
            ax.text(0.5, 0.65, "MODEL", ha='center', va='center', fontsize=12, fontweight='bold')
            
            # Add example text
            ax.text(0.5, 0.3, f'"{example}"', ha='center', va='center', fontsize=10,
                   bbox=dict(boxstyle='round', facecolor='white', alpha=0.7))
            
            # Add statistical context
            if case == Case.NOMINATIVE:
                ax.text(0.5, 0.15, "Focus: Parameter Estimation", ha='center', fontsize=9)
            elif case == Case.ACCUSATIVE:
                ax.text(0.5, 0.15, "Focus: Model Evaluation", ha='center', fontsize=9)
            elif case == Case.DATIVE:
                ax.text(0.5, 0.15, "Focus: Data Reception", ha='center', fontsize=9)
            elif case == Case.GENITIVE:
                ax.text(0.5, 0.15, "Focus: Prediction Generation", ha='center', fontsize=9)
            elif case == Case.INSTRUMENTAL:
                ax.text(0.5, 0.15, "Focus: Computational Method", ha='center', fontsize=9)
            elif case == Case.LOCATIVE:
                ax.text(0.5, 0.15, "Focus: Statistical Context", ha='center', fontsize=9)
            elif case == Case.ABLATIVE:
                ax.text(0.5, 0.15, "Focus: Error Source", ha='center', fontsize=9)
            elif case == Case.VOCATIVE:
                ax.text(0.5, 0.15, "Focus: User Interface", ha='center', fontsize=9)
            
            # Turn off axes
            ax.axis('off')
        
        # Layout and save
        fig.tight_layout()
        fig.savefig(save_path, dpi=300, bbox_inches='tight')
        plt.close(fig)


def plot_case_linguistic_context(case: Case, save_path: str) -> None:
    """
    Create a visualization explaining the linguistic context of a specific case.
    
    Args:
        case: The case to visualize
        save_path: Path to save the figure
    """
    # Define case-specific information
    case_info = {
        Case.NOMINATIVE: {
            "title": "NOMINATIVE Case",
            "code": "NOM",
            "meaning": "Subject performing the action",
            "example": "The LINEAR MODEL fits the data points.",
            "description": "The model actively operates on the data, functioning as the subject of the sentence.",
            "color": "royalblue",
            "direction": "Model → Data"
        },
        Case.ACCUSATIVE: {
            "title": "ACCUSATIVE Case",
            "code": "ACC",
            "meaning": "Direct object receiving action",
            "example": "The analyst EVALUATES the LINEAR MODEL.",
            "description": "The model receives the action of being evaluated or assessed, functioning as the direct object of the sentence.",
            "color": "firebrick",
            "direction": "Action → Model"
        },
        Case.DATIVE: {
            "title": "DATIVE Case",
            "code": "DAT",
            "meaning": "Indirect object or recipient",
            "example": "The researcher GIVES data TO the LINEAR MODEL.",
            "description": "The model serves as a recipient of data or parameters, functioning as the indirect object of the sentence.",
            "color": "forestgreen",
            "direction": "Data → Model"
        },
        Case.GENITIVE: {
            "title": "GENITIVE Case",
            "code": "GEN",
            "meaning": "Possessive or source relation",
            "example": "The predictions BELONG TO the LINEAR MODEL.",
            "description": "The model functions as the source or possessor of predictions or outputs, expressing ownership of results.",
            "color": "darkorange",
            "direction": "Model owns → Outputs"
        },
        Case.INSTRUMENTAL: {
            "title": "INSTRUMENTAL Case",
            "code": "INS",
            "meaning": "Means or instrument of action",
            "example": "The researcher solves the problem WITH the LINEAR MODEL.",
            "description": "The model serves as a tool or methodology to accomplish a task, functioning as the means by which an action is performed.",
            "color": "blueviolet",
            "direction": "Problem solved → WITH Model"
        },
        Case.LOCATIVE: {
            "title": "LOCATIVE Case",
            "code": "LOC",
            "meaning": "Location or context marker",
            "example": "The pattern exists WITHIN the LINEAR MODEL's assumptions.",
            "description": "The model provides a context or environment within which statistical properties exist, serving as a location for analysis.",
            "color": "teal",
            "direction": "Assumptions → WITHIN Model"
        },
        Case.ABLATIVE: {
            "title": "ABLATIVE Case",
            "code": "ABL",
            "meaning": "Origin, separation, or source",
            "example": "The prediction errors COME FROM the LINEAR MODEL.",
            "description": "The model functions as the origin of errors or effects, indicating where residuals or other artifacts originate.",
            "color": "crimson",
            "direction": "Errors ← FROM Model"
        },
        Case.VOCATIVE: {
            "title": "VOCATIVE Case",
            "code": "VOC",
            "meaning": "Direct address or invocation",
            "example": "HEY LINEAR MODEL, what is the prediction for this input?",
            "description": "The model serves as an addressable interface or API that can be directly queried, functioning as the direct addressee.",
            "color": "goldenrod",
            "direction": "User → HEY Model!"
        }
    }
    
    # Get information for the specific case
    info = case_info[case]
    
    # Create figure
    fig, ax = plt.subplots(figsize=(8, 6))
    ax.axis('off')
    
    # Title
    ax.text(0.5, 0.95, f"{info['title']} [{info['code']}]", 
           ha='center', fontsize=18, fontweight='bold', color=info['color'],
           transform=ax.transAxes)
    
    # Linguistic meaning
    ax.text(0.5, 0.87, f"Linguistic meaning: {info['meaning']}", 
           ha='center', fontsize=14, transform=ax.transAxes)
    
    # Example sentence
    example_box = dict(boxstyle='round,pad=0.5', facecolor='wheat', alpha=0.5)
    ax.text(0.5, 0.78, f'Example: "{info["example"]}"', 
           ha='center', fontsize=14, transform=ax.transAxes,
           bbox=example_box)
    
    # Description
    ax.text(0.5, 0.65, info['description'], 
           ha='center', fontsize=12, transform=ax.transAxes,
           wrap=True)
    
    # Draw the model interaction diagram
    model_rect = plt.Rectangle((0.35, 0.3), 0.3, 0.2, 
                             facecolor=info['color'], alpha=0.3,
                             edgecolor='black', linewidth=2)
    ax.add_patch(model_rect)
    ax.text(0.5, 0.4, "LINEAR\nMODEL", 
           ha='center', va='center', fontsize=12, 
           fontweight='bold', transform=ax.transAxes)
    
    # Add direction information
    ax.text(0.5, 0.2, f"Direction: {info['direction']}",
           ha='center', fontsize=12, transform=ax.transAxes,
           bbox=dict(boxstyle='round', facecolor='lightgray', alpha=0.5))
    
    # Save the figure
    fig.tight_layout()
    fig.savefig(save_path, dpi=300)
    plt.close(fig)
    
    # Log that the visualization was created
    print(f"Saved linguistic context visualization for {case.value} case to {save_path}") 