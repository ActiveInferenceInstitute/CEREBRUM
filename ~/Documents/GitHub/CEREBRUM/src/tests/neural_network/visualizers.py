import os
import numpy as np
import matplotlib
# Set matplotlib backend to a non-interactive one for test environment
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from typing import Dict, List, Any, Optional, Tuple, Union

from src.core.model import Case
from src.core.neural_network import NeuralNetworkModel
from src.visualization.case_visualization import CASE_COLORS

# Visualization helper class for Neural Network models
class Visualizer:
    """Helper class for creating visualizations of Neural Network models and data."""
    
    @staticmethod
    def plot_data(
        X: np.ndarray,
        y: np.ndarray,
        title: str = "Dataset",
        figsize: Tuple[int, int] = (10, 6),
        save_path: str = None,
        logger=None
    ) -> plt.Figure:
        """
        Plot the dataset.
        
        Args:
            X: Input data
            y: Target data
            title: Plot title
            figsize: Figure size
            save_path: Path to save the figure
            logger: Logger instance for logging
            
        Returns:
            Matplotlib figure
        """
        fig, ax = plt.subplots(figsize=figsize)
        
        # Determine plot type based on dimensions
        if X.shape[1] == 1 and y.shape[1] == 1:
            # 1D regression
            ax.scatter(X[:, 0], y[:, 0], alpha=0.7, s=30)
            ax.set_xlabel('Input')
            ax.set_ylabel('Target')
            ax.grid(True, linestyle='--', alpha=0.6)
        
        elif X.shape[1] == 2 and y.shape[1] == 1:
            # 2D regression or binary classification
            if len(np.unique(y)) <= 5:  # Likely classification
                # Color by class
                for cls in np.unique(y):
                    idx = np.where(y[:, 0] == cls)[0]
                    ax.scatter(X[idx, 0], X[idx, 1], alpha=0.7, s=30, label=f'Class {int(cls)}')
                ax.legend()
            else:  # Regression
                sc = ax.scatter(X[:, 0], X[:, 1], c=y[:, 0], cmap='viridis', alpha=0.7, s=30)
                plt.colorbar(sc, ax=ax, label='Target')
            
            ax.set_xlabel('Feature 1')
            ax.set_ylabel('Feature 2')
            ax.grid(True, linestyle='--', alpha=0.6)
        
        else:
            # Higher dimensions, show pairplot for first few dimensions
            ax.text(0.5, 0.5, f"Data has {X.shape[1]} input dimensions and {y.shape[1]} output dimensions", 
                   ha='center', va='center', transform=ax.transAxes)
            ax.text(0.5, 0.4, "Higher dimensional data visualization not implemented", 
                   ha='center', va='center', transform=ax.transAxes)
        
        ax.set_title(title)
        
        plt.tight_layout()
        
        if save_path:
            fig.savefig(save_path)
            if logger:
                logger.info(f"Saved data plot to {save_path}")
            plt.close(fig)
        
        return fig
    
    @staticmethod
    def plot_network_structure(
        model: NeuralNetworkModel,
        title: str = "Neural Network Structure",
        figsize: Tuple[int, int] = (12, 8),
        save_path: str = None,
        logger=None
    ) -> plt.Figure:
        """
        Plot the structure of a neural network.
        
        Args:
            model: Neural network model
            title: Plot title
            figsize: Figure size
            save_path: Path to save the figure
            logger: Logger instance for logging
            
        Returns:
            Matplotlib figure
        """
        fig, ax = plt.subplots(figsize=figsize)
        ax.axis('off')
        
        # Get layer dimensions
        layer_dims = [model.input_dim] + model.hidden_dims + [model.output_dim]
        
        # Calculate max neurons for spacing
        max_neurons = max(layer_dims)
        
        # Colors for different layer types
        colors = {
            'input': '#3498db',     # Blue
            'hidden': '#2ecc71',    # Green
            'output': '#e74c3c'     # Red
        }
        
        # Draw network layers
        for i, n_neurons in enumerate(layer_dims):
            # Determine layer type
            if i == 0:
                layer_type = 'input'
            elif i == len(layer_dims) - 1:
                layer_type = 'output'
            else:
                layer_type = 'hidden'
            
            # Calculate x position for layer
            x = i / (len(layer_dims) - 1)
            
            # Draw neurons in the layer
            for j in range(n_neurons):
                # Calculate y position for neuron
                if n_neurons == 1:
                    y = 0.5
                else:
                    y = j / (n_neurons - 1) if n_neurons > 1 else 0.5
                
                # Draw neuron
                circle = plt.Circle((x, y), 0.02, color=colors[layer_type], zorder=2)
                ax.add_patch(circle)
                
                # Draw connections to previous layer
                if i > 0:
                    prev_n_neurons = layer_dims[i-1]
                    for k in range(prev_n_neurons):
                        # Calculate y position for previous neuron
                        if prev_n_neurons == 1:
                            prev_y = 0.5
                        else:
                            prev_y = k / (prev_n_neurons - 1) if prev_n_neurons > 1 else 0.5
                        
                        # Draw connection line
                        ax.plot([x-1/(len(layer_dims)-1), x], [prev_y, y], 
                              color='gray', alpha=0.3, linewidth=0.5, zorder=1)
            
            # Add layer label
            if layer_type == 'input':
                layer_name = 'Input Layer'
            elif layer_type == 'output':
                layer_name = 'Output Layer'
            else:
                layer_name = f'Hidden Layer {i}'
            
            ax.text(x, -0.05, layer_name, ha='center', va='top', fontsize=10)
            ax.text(x, -0.1, f'({n_neurons} neurons)', ha='center', va='top', fontsize=8)
        
        # Add activation function
        ax.text(0.5, 1.05, f'Activation: {model.activation}', ha='center', va='bottom', 
               fontsize=12, bbox=dict(boxstyle='round,pad=0.5', facecolor='lightyellow', alpha=0.5))
        
        # Add title
        ax.text(0.5, 1.15, title, ha='center', va='bottom', fontsize=14, fontweight='bold')
        
        # Set axis limits with some padding
        ax.set_xlim(-0.1, 1.1)
        ax.set_ylim(-0.15, 1.2)
        
        plt.tight_layout()
        
        if save_path:
            fig.savefig(save_path)
            if logger:
                logger.info(f"Saved network structure plot to {save_path}")
            plt.close(fig)
        
        return fig
    
    @staticmethod
    def plot_training_history(
        loss_history: List[float],
        title: str = "Training History",
        figsize: Tuple[int, int] = (10, 6),
        save_path: str = None,
        logger=None
    ) -> plt.Figure:
        """
        Plot the training history of a neural network.
        
        Args:
            loss_history: List of loss values during training
            title: Plot title
            figsize: Figure size
            save_path: Path to save the figure
            logger: Logger instance for logging
            
        Returns:
            Matplotlib figure
        """
        fig, ax = plt.subplots(figsize=figsize)
        
        # Plot loss curve
        ax.plot(loss_history, 'b-', linewidth=2, label='Loss')
        
        # Add smoothed version if enough points
        if len(loss_history) > 10:
            # Simple moving average
            window_size = min(10, len(loss_history) // 5)
            if window_size > 1:
                smoothed = np.convolve(loss_history, np.ones(window_size)/window_size, mode='valid')
                ax.plot(np.arange(window_size-1, len(loss_history)), smoothed, 
                      'r-', linewidth=2, label='Smoothed Loss')
        
        ax.set_title(title)
        ax.set_xlabel('Iteration')
        ax.set_ylabel('Loss')
        ax.grid(True, linestyle='--', alpha=0.6)
        ax.legend()
        
        plt.tight_layout()
        
        if save_path:
            fig.savefig(save_path)
            if logger:
                logger.info(f"Saved training history plot to {save_path}")
            plt.close(fig)
        
        return fig

# Visualization helper for case linguistic context
def plot_case_linguistic_context(case: Case, save_path: str, logger=None) -> None:
    """
    Generate a visualization showing linguistic context for a case.
    
    Args:
        case: The grammatical case to visualize
        save_path: Path to save the visualization
        logger: Logger instance for logging
    """
    from src.tests.neural_network.case_definitions import CaseDefinitions
    
    case_info = CaseDefinitions.get_all_cases()[case]
    
    fig, ax = plt.subplots(figsize=(10, 6))
    
    # Create visual representation
    ax.axis('off')  # Hide the axes
    
    # Title with case
    title = f"CEREBRUM Case: {case.value} ({case_info['linguistic_meaning']})"
    ax.text(0.5, 0.95, title, ha='center', va='top', fontsize=16, 
            fontweight='bold', transform=ax.transAxes)
    
    # Statistical role
    ax.text(0.5, 0.88, f"Statistical Role: {case_info['statistical_role']}", 
            ha='center', va='top', fontsize=14, transform=ax.transAxes)
    
    # Example sentence
    example_box = dict(boxstyle='round,pad=0.5', facecolor='lightblue', alpha=0.5)
    ax.text(0.5, 0.8, f"Linguistic example: {case_info['example']}", 
            ha='center', va='top', fontsize=12, bbox=example_box, 
            transform=ax.transAxes)
    
    # Formula
    formula_box = dict(boxstyle='round,pad=0.5', facecolor='lightyellow', alpha=0.5)
    ax.text(0.5, 0.68, f"Mathematical representation:\n{case_info['formula']}", 
            ha='center', va='top', fontsize=12, bbox=formula_box, 
            transform=ax.transAxes)
    
    # Neural Network context
    context_box = dict(boxstyle='round,pad=0.5', facecolor='lightgreen', alpha=0.5)
    ax.text(0.5, 0.54, "Neural Network Context:", ha='center', va='top', 
            fontsize=12, fontweight='bold', transform=ax.transAxes)
    ax.text(0.5, 0.48, case_info['neural_network_context'], ha='center', va='top', 
            fontsize=12, wrap=True, transform=ax.transAxes)
    
    # Methods
    methods_box = dict(boxstyle='round,pad=0.5', facecolor='lightgray', alpha=0.5)
    ax.text(0.5, 0.36, f"Primary Methods: {case_info['primary_methods']}", 
            ha='center', va='top', fontsize=12, bbox=methods_box, 
            transform=ax.transAxes)
    
    # Save the figure
    fig.savefig(save_path)
    plt.close(fig)
    
    if logger:
        logger.info(f"Created linguistic context visualization for {case.value} case at {save_path}")

def create_overview_visualization(save_path: str, logger=None) -> None:
    """
    Generate an overview visualization of all grammatical cases in neural network context.
    
    Args:
        save_path: Path to save the visualization
        logger: Logger instance for logging
    """
    from src.tests.neural_network.case_definitions import CaseDefinitions
    
    # Get all cases
    cases = [Case.NOMINATIVE, Case.ACCUSATIVE, Case.DATIVE, Case.GENITIVE, 
             Case.INSTRUMENTAL, Case.LOCATIVE, Case.ABLATIVE, Case.VOCATIVE]
    case_infos = [CaseDefinitions.get_all_cases()[case] for case in cases]
    
    # Create figure with grid layout
    fig = plt.figure(figsize=(20, 16))
    grid = plt.GridSpec(4, 2, figure=fig, hspace=0.4, wspace=0.3)
    
    # Add figure title
    fig.suptitle("Neural Network Grammar: CEREBRUM Case System", fontsize=24, fontweight='bold', y=0.98)
    fig.text(0.5, 0.94, "Modeling Neural Networks with Grammatical Cases", 
            ha='center', fontsize=18, style='italic')
    
    # Create a subplot for each case in a 4x2 grid
    for i, (case, case_info) in enumerate(zip(cases, case_infos)):
        # Calculate grid position
        row = i // 2
        col = i % 2
        
        # Create subplot
        ax = fig.add_subplot(grid[row, col])
        ax.axis('off')
        
        # Set case color
        case_color = CASE_COLORS.get(case, 'gray')
        
        # Add case title
        title = f"{case.value}: {case_info['linguistic_meaning']}"
        ax.text(0.5, 0.95, title, ha='center', va='top', fontsize=16, 
                fontweight='bold', transform=ax.transAxes, color=case_color)
        
        # Add statistical role
        ax.text(0.5, 0.85, f"{case_info['statistical_role']}", 
                ha='center', va='top', fontsize=14, fontweight='bold', 
                transform=ax.transAxes)
        
        # Example
        example_box = dict(boxstyle='round,pad=0.5', facecolor='lightblue', alpha=0.3)
        ax.text(0.5, 0.75, f"{case_info['example']}", 
                ha='center', va='top', fontsize=12, bbox=example_box, 
                transform=ax.transAxes)
        
        # Neural Network context
        context_box = dict(boxstyle='round,pad=0.5', facecolor='lightgreen', alpha=0.3)
        ax.text(0.5, 0.60, case_info['neural_network_context'], 
                ha='center', va='top', fontsize=12, bbox=context_box, 
                transform=ax.transAxes, wrap=True)
        
        # Formula
        formula_box = dict(boxstyle='round,pad=0.5', facecolor='lightyellow', alpha=0.3)
        ax.text(0.5, 0.40, f"{case_info['formula']}", 
                ha='center', va='top', fontsize=12, bbox=formula_box, 
                transform=ax.transAxes)
        
        # Methods
        methods_box = dict(boxstyle='round,pad=0.5', facecolor='lightgray', alpha=0.3)
        ax.text(0.5, 0.25, f"{case_info['primary_methods']}", 
                ha='center', va='top', fontsize=12, bbox=methods_box, 
                transform=ax.transAxes)
        
        # Draw border with case color
        border = plt.Rectangle((0, 0), 1, 1, fill=False, 
                             transform=ax.transAxes, 
                             edgecolor=case_color,
                             linewidth=3)
        ax.add_patch(border)
    
    # Footer notes
    fig.text(0.5, 0.05, "CEREBRUM Framework: Cognitive Enhancement by Recasting Ephemeral Brain Representations as Universal Mappings", 
            ha='center', fontsize=14, style='italic')
    fig.text(0.5, 0.03, "Each case represents a different role that a neural network can have in computational processes.", 
            ha='center', fontsize=12)
    
    # Save the figure
    fig.savefig(save_path, dpi=150, bbox_inches='tight')
    plt.close(fig)
    
    if logger:
        logger.info(f"Created neural network cases overview visualization at {save_path}") 