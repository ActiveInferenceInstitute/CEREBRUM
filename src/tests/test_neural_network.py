import os
import numpy as np
import matplotlib
# Set matplotlib backend to a non-interactive one for test environment
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib.gridspec import GridSpec
import pytest
import logging
import math
import time
from typing import Dict, List, Any, Optional, Tuple, Union

from src.core.model import Case
from src.core.neural_network import NeuralNetworkModel
from src.visualization.case_visualization import CASE_COLORS

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

# Set the output directory for visualizations
OUTPUT_DIR = os.path.join(os.path.dirname(__file__), "output", "neural_network")
os.makedirs(OUTPUT_DIR, exist_ok=True)

# Case definitions for Neural Network models
class CaseDefinitions:
    """Class containing definitions for all CEREBRUM cases in Neural Network context."""
    
    @staticmethod
    def nominative() -> Dict[str, str]:
        """
        NOMINATIVE case: The model as active agent.
        
        In linguistics: Subject of verb.
        In Neural Network: Model actively generating predictions.
        """
        return {
            "case": Case.NOMINATIVE.value,
            "linguistic_meaning": "Subject/Doer of action",
            "statistical_role": "Active predictor",
            "neural_network_context": "The neural network as an active agent generating predictions from inputs",
            "formula": "f(x) = W*x + b",
            "primary_methods": "predict(), forward()",
            "visualization": "Forward propagation, activation patterns",
            "example": "The NEURAL NETWORK PREDICTS the output values"
        }
    
    @staticmethod
    def accusative() -> Dict[str, str]:
        """
        ACCUSATIVE case: The model as object of process.
        
        In linguistics: Direct object.
        In Neural Network: Model being trained or evaluated.
        """
        return {
            "case": Case.ACCUSATIVE.value,
            "linguistic_meaning": "Direct object/Receiver of action",
            "statistical_role": "Target of optimization",
            "neural_network_context": "The neural network as an object being trained or evaluated",
            "formula": "L(θ) = (1/n)∑(y - ŷ)²",
            "primary_methods": "evaluate(), calculate_loss()",
            "visualization": "Loss curves, error metrics",
            "example": "The researcher TRAINS the NEURAL NETWORK"
        }
    
    @staticmethod
    def dative() -> Dict[str, str]:
        """
        DATIVE case: The model as recipient.
        
        In linguistics: Indirect object.
        In Neural Network: Model receiving inputs.
        """
        return {
            "case": Case.DATIVE.value,
            "linguistic_meaning": "Indirect object/Recipient",
            "statistical_role": "Data receiver",
            "neural_network_context": "The neural network as a recipient of input data",
            "formula": "input → network",
            "primary_methods": "receive_data(), process_inputs()",
            "visualization": "Input transformations, data flow diagrams",
            "example": "The dataset GIVES inputs TO the NEURAL NETWORK"
        }
    
    @staticmethod
    def genitive() -> Dict[str, str]:
        """
        GENITIVE case: The model as source/possessor.
        
        In linguistics: Possessive.
        In Neural Network: Model generating outputs.
        """
        return {
            "case": Case.GENITIVE.value,
            "linguistic_meaning": "Possessive/Source",
            "statistical_role": "Output source",
            "neural_network_context": "The neural network as a source of outputs and predictions",
            "formula": "ŷ = f(x; θ)",
            "primary_methods": "generate_output(), get_predictions()",
            "visualization": "Output distributions, activation maps",
            "example": "The NEURAL NETWORK's OUTPUTS are used for decision-making"
        }
    
    @staticmethod
    def instrumental() -> Dict[str, str]:
        """
        INSTRUMENTAL case: The model as method/tool.
        
        In linguistics: Means or instrument.
        In Neural Network: Model as computational method.
        """
        return {
            "case": Case.INSTRUMENTAL.value,
            "linguistic_meaning": "By means of/Using",
            "statistical_role": "Computational method",
            "neural_network_context": "The neural network as a computational method for solving problems",
            "formula": "f(x) = σ(Wx + b)",
            "primary_methods": "compute(), apply_method()",
            "visualization": "Architecture diagrams, computational graphs",
            "example": "The team solves the problem BY USING a NEURAL NETWORK"
        }
    
    @staticmethod
    def locative() -> Dict[str, str]:
        """
        LOCATIVE case: The model as location/context.
        
        In linguistics: Location or time.
        In Neural Network: Model as representational space.
        """
        return {
            "case": Case.LOCATIVE.value,
            "linguistic_meaning": "In/At/Within",
            "statistical_role": "Representational context",
            "neural_network_context": "The neural network as a representational space or feature context",
            "formula": "h = f(x) ∈ ℝⁿ",
            "primary_methods": "get_representation(), extract_features()",
            "visualization": "Feature space visualizations, embedding plots",
            "example": "The patterns exist WITHIN the NEURAL NETWORK's hidden layers"
        }
    
    @staticmethod
    def ablative() -> Dict[str, str]:
        """
        ABLATIVE case: The model as origin/cause.
        
        In linguistics: Movement from.
        In Neural Network: Model as source of errors.
        """
        return {
            "case": Case.ABLATIVE.value,
            "linguistic_meaning": "From/Out of/Because of",
            "statistical_role": "Error source",
            "neural_network_context": "The neural network as the source of prediction errors and gradients",
            "formula": "∇L = ∂L/∂θ",
            "primary_methods": "compute_gradients(), backpropagate()",
            "visualization": "Gradient flow, error attribution",
            "example": "The errors ORIGINATE FROM the NEURAL NETWORK's weights"
        }
    
    @staticmethod
    def vocative() -> Dict[str, str]:
        """
        VOCATIVE case: The model as addressable entity.
        
        In linguistics: Direct address.
        In Neural Network: Model as interactive interface.
        """
        return {
            "case": Case.VOCATIVE.value,
            "linguistic_meaning": "Direct address/Invocation",
            "statistical_role": "Interactive interface",
            "neural_network_context": "The neural network as an addressable entity with a query/response interface",
            "formula": "API: query(NN, input) → response",
            "primary_methods": "query(), get_response()",
            "visualization": "API diagrams, interaction flows",
            "example": "HEY NEURAL NETWORK, what is the prediction for this input?"
        }
    
    @staticmethod
    def get_all_cases() -> Dict[Case, Dict[str, str]]:
        """Returns a dictionary with information for all cases."""
        return {
            Case.NOMINATIVE: CaseDefinitions.nominative(),
            Case.ACCUSATIVE: CaseDefinitions.accusative(),
            Case.DATIVE: CaseDefinitions.dative(),
            Case.GENITIVE: CaseDefinitions.genitive(),
            Case.INSTRUMENTAL: CaseDefinitions.instrumental(),
            Case.LOCATIVE: CaseDefinitions.locative(),
            Case.ABLATIVE: CaseDefinitions.ablative(),
            Case.VOCATIVE: CaseDefinitions.vocative(),
        }

# Data generator for Neural Network test data
class DataGenerator:
    """Generates synthetic data for Neural Network tests."""
    
    @staticmethod
    def regression_data(
        n_samples: int = 200,
        input_dim: int = 1,
        output_dim: int = 1,
        noise_level: float = 0.1,
        random_seed: int = 42
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate synthetic regression data.
        
        Args:
            n_samples: Number of data points
            input_dim: Input dimension
            output_dim: Output dimension
            noise_level: Noise level in the data
            random_seed: Random seed for reproducibility
            
        Returns:
            Tuple of (X, y) arrays
        """
        np.random.seed(random_seed)
        
        # Generate random inputs
        X = np.random.randn(n_samples, input_dim)
        
        # Generate true weights and biases
        W = np.random.randn(input_dim, output_dim)
        b = np.random.randn(output_dim)
        
        # Generate outputs with noise
        y_true = np.dot(X, W) + b
        noise = np.random.normal(0, noise_level, (n_samples, output_dim))
        y = y_true + noise
        
        return X, y
    
    @staticmethod
    def classification_data(
        n_samples: int = 200,
        n_classes: int = 2,
        n_features: int = 2,
        class_sep: float = 1.0,
        random_seed: int = 42
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate synthetic classification data.
        
        Args:
            n_samples: Number of data points
            n_classes: Number of classes
            n_features: Number of features
            class_sep: Class separation
            random_seed: Random seed for reproducibility
            
        Returns:
            Tuple of (X, y) arrays
        """
        np.random.seed(random_seed)
        
        # Generate class centers
        centers = np.random.randn(n_classes, n_features) * class_sep
        
        # Initialize data arrays
        X = np.zeros((n_samples, n_features))
        y = np.zeros(n_samples, dtype=int)
        
        # Generate data for each class
        samples_per_class = n_samples // n_classes
        for i in range(n_classes):
            start_idx = i * samples_per_class
            end_idx = (i + 1) * samples_per_class if i < n_classes - 1 else n_samples
            n_class_samples = end_idx - start_idx
            
            # Generate samples around the class center
            X[start_idx:end_idx] = centers[i] + np.random.randn(n_class_samples, n_features)
            y[start_idx:end_idx] = i
        
        # Shuffle the data
        idx = np.random.permutation(n_samples)
        X = X[idx]
        y = y[idx]
        
        # Convert to one-hot encoding if more than 2 classes
        if n_classes > 2:
            y_one_hot = np.zeros((n_samples, n_classes))
            for i in range(n_samples):
                y_one_hot[i, y[i]] = 1
            return X, y_one_hot
        
        return X, y.reshape(-1, 1)

# Visualization helper class for Neural Network models
class Visualizer:
    """Helper class for creating visualizations of Neural Network models and data."""
    
    @staticmethod
    def plot_data(
        X: np.ndarray,
        y: np.ndarray,
        title: str = "Dataset",
        figsize: Tuple[int, int] = (10, 6),
        save_path: str = None
    ) -> plt.Figure:
        """
        Plot the dataset.
        
        Args:
            X: Input data
            y: Target data
            title: Plot title
            figsize: Figure size
            save_path: Path to save the figure
            
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
            logger.info(f"Saved data plot to {save_path}")
            plt.close(fig)
        
        return fig
    
    @staticmethod
    def plot_network_structure(
        model: NeuralNetworkModel,
        title: str = "Neural Network Structure",
        figsize: Tuple[int, int] = (12, 8),
        save_path: str = None
    ) -> plt.Figure:
        """
        Plot the structure of a neural network.
        
        Args:
            model: Neural network model
            title: Plot title
            figsize: Figure size
            save_path: Path to save the figure
            
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
            logger.info(f"Saved network structure plot to {save_path}")
            plt.close(fig)
        
        return fig
    
    @staticmethod
    def plot_training_history(
        loss_history: List[float],
        title: str = "Training History",
        figsize: Tuple[int, int] = (10, 6),
        save_path: str = None
    ) -> plt.Figure:
        """
        Plot the training history of a neural network.
        
        Args:
            loss_history: List of loss values during training
            title: Plot title
            figsize: Figure size
            save_path: Path to save the figure
            
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
            logger.info(f"Saved training history plot to {save_path}")
            plt.close(fig)
        
        return fig

# Visualization helper for case linguistic context
def plot_case_linguistic_context(case: Case, save_path: str) -> None:
    """Generate a visualization showing linguistic context for a case."""
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

# Fixtures
@pytest.fixture
def case_definitions():
    """Fixture for case definitions."""
    return CaseDefinitions.get_all_cases()

@pytest.fixture
def nn_regression_data():
    """Fixture for neural network regression test data."""
    return DataGenerator.regression_data(n_samples=200, input_dim=1, output_dim=1)

@pytest.fixture
def nn_classification_data():
    """Fixture for neural network classification test data."""
    return DataGenerator.classification_data(n_samples=200, n_classes=2, n_features=2)

# Test functions for each case
def test_nominative_case(nn_regression_data, case_definitions):
    """Test for NOMINATIVE case: Model as active agent generating predictions."""
    # Get case info for logging
    case_info = case_definitions[Case.NOMINATIVE]
    logger.info(f"Testing {Case.NOMINATIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(OUTPUT_DIR, Case.NOMINATIVE.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.NOMINATIVE, linguistics_path)
    
    # Unpack the data
    X, y = nn_regression_data
    
    # Create a simple NN model in NOMINATIVE case
    model = NeuralNetworkModel(
        name="NomModel",
        input_dim=X.shape[1],
        output_dim=y.shape[1],
        hidden_dims=[10, 5],
        activation='relu'
    )
    model.case = Case.NOMINATIVE  # Explicitly set to NOMINATIVE case
    
    # Log model details
    logger.info(f"Created neural network model with architecture: input_dim={X.shape[1]}, hidden_dims=[10, 5], output_dim={y.shape[1]}")
    
    # Visualize the model structure 
    network_structure_path = os.path.join(case_dir, "network_structure.png")
    Visualizer.plot_network_structure(
        model=model,
        title=f"Neural Network Structure in {Case.NOMINATIVE.value} Case",
        save_path=network_structure_path
    )
    
    # Train the model
    logger.info("Training model in NOMINATIVE case (model as active prediction generator)")
    train_results = model.train(X, y, epochs=200, learning_rate=0.01, batch_size=32)
    
    # Visualize training progress
    training_path = os.path.join(case_dir, "training_history.png")
    Visualizer.plot_training_history(
        loss_history=model.loss_history,
        title=f"Training Loss in {Case.NOMINATIVE.value} Case",
        save_path=training_path
    )
    
    # Save final metrics
    metrics_file = os.path.join(case_dir, "metrics.txt")
    with open(metrics_file, "w") as f:
        f.write(f"Case: {Case.NOMINATIVE.value} - {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical Role: {case_info['statistical_role']}\n\n")
        f.write(f"Neural Network Context: {case_info['neural_network_context']}\n\n")
        f.write("Training Metrics:\n")
        f.write(f"  Final Loss: {model.loss_history[-1]:.6f}\n")
        f.write(f"  Initial Loss: {model.loss_history[0]:.6f}\n")
        f.write(f"  Improvement: {(1 - model.loss_history[-1]/model.loss_history[0])*100:.2f}%\n")
        f.write(f"  Epochs: {len(model.loss_history)}\n")
    
    # Generate predictions
    logger.info("Generating predictions with trained model")
    predictions = model.predict(X)
    
    # Visualize predictions vs targets
    prediction_path = os.path.join(case_dir, "predictions.png")
    fig, ax = plt.subplots(figsize=(10, 6))
    
    if X.shape[1] == 1 and y.shape[1] == 1:
        # For 1D data, sort by X for better visualization
        sort_idx = np.argsort(X[:, 0])
        X_sorted = X[sort_idx]
        y_sorted = y[sort_idx]
        pred_sorted = predictions[sort_idx]
        
        ax.scatter(X_sorted, y_sorted, alpha=0.7, label='Actual')
        ax.plot(X_sorted, pred_sorted, 'r-', linewidth=2, label='Predicted')
        ax.set_xlabel('Input')
        ax.set_ylabel('Output')
    else:
        # For higher dimensional data, just plot error distribution
        errors = np.sqrt(np.mean((predictions - y)**2, axis=1))
        ax.hist(errors, bins=20, alpha=0.7)
        ax.set_xlabel('Prediction Error (RMSE)')
        ax.set_ylabel('Frequency')
    
    ax.set_title(f"Model Predictions in {Case.NOMINATIVE.value} Case")
    ax.grid(True, linestyle='--', alpha=0.6)
    ax.legend()
    fig.tight_layout()
    fig.savefig(prediction_path)
    plt.close(fig)
    
    # Create an animation of the learning process
    # We'll create this for 1D data only for simplicity
    if X.shape[1] == 1 and y.shape[1] == 1:
        animation_path = os.path.join(case_dir, "learning_animation.gif")
        
        # Train a new model, tracking predictions over time
        anim_model = NeuralNetworkModel(
            name="AnimModel",
            input_dim=X.shape[1],
            output_dim=y.shape[1], 
            hidden_dims=[10, 5],
            activation='relu'
        )
        
        # Sort data for better visualization
        sort_idx = np.argsort(X[:, 0])
        X_sorted = X[sort_idx]
        y_sorted = y[sort_idx]
        
        # Storage for predictions at each step
        epoch_predictions = []
        n_epochs = 50  # Fewer epochs for animation
        
        # Train the model step by step
        for epoch in range(n_epochs):
            if epoch % 5 == 0:  # Save every 5th prediction to keep animation size reasonable
                current_preds = anim_model.predict(X_sorted)
                epoch_predictions.append((epoch, current_preds))
            
            # Update model for one epoch
            anim_model.train(X, y, epochs=1, learning_rate=0.01, batch_size=32)
        
        # Create animation
        fig, ax = plt.subplots(figsize=(10, 6))
        ax.scatter(X_sorted, y_sorted, alpha=0.5, label='Actual')
        line, = ax.plot([], [], 'r-', linewidth=2, label='Predicted')
        ax.set_xlabel('Input')
        ax.set_ylabel('Output')
        ax.set_title(f"Learning Animation in {Case.NOMINATIVE.value} Case")
        ax.grid(True, linestyle='--', alpha=0.6)
        ax.legend()
        
        # Set axis limits
        ax.set_xlim(X_sorted.min() - 0.1, X_sorted.max() + 0.1)
        ax.set_ylim(min(y_sorted.min(), predictions.min()) - 0.1, 
                   max(y_sorted.max(), predictions.max()) + 0.1)
        
        # Text for showing epoch
        epoch_text = ax.text(0.02, 0.95, '', transform=ax.transAxes)
        
        def init():
            line.set_data([], [])
            epoch_text.set_text('')
            return line, epoch_text
        
        def update(frame):
            epoch, preds = epoch_predictions[frame]
            line.set_data(X_sorted, preds)
            epoch_text.set_text(f'Epoch: {epoch}')
            return line, epoch_text
        
        anim = animation.FuncAnimation(fig, update, frames=len(epoch_predictions),
                              init_func=init, blit=True)
        
        # Save animation
        anim.save(animation_path, writer='pillow', fps=2)
        plt.close(fig)
        logger.info(f"Created learning animation: {animation_path}")
    
    # Generate a report
    report_path = os.path.join(case_dir, "report.md")
    with open(report_path, "w") as f:
        f.write(f"# Neural Network in {Case.NOMINATIVE.value} Case\n\n")
        f.write(f"## {case_info['linguistic_meaning']}\n\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n\n")
        f.write(f"### Context\n\n")
        f.write(f"{case_info['neural_network_context']}\n\n")
        f.write("### Mathematical Representation\n\n")
        f.write(f"```\n{case_info['formula']}\n```\n\n")
        f.write("### Primary Methods\n\n")
        f.write(f"{case_info['primary_methods']}\n\n")
        f.write("### Example\n\n")
        f.write(f"{case_info['example']}\n\n")
        f.write("### Analysis Results\n\n")
        f.write(f"* Model architecture: Input({X.shape[1]}) → Hidden(10) → Hidden(5) → Output({y.shape[1]})\n")
        f.write(f"* Activation function: {model.activation}\n")
        f.write(f"* Initial loss: {model.loss_history[0]:.6f}\n")
        f.write(f"* Final loss: {model.loss_history[-1]:.6f}\n")
        f.write(f"* Improvement: {(1 - model.loss_history[-1]/model.loss_history[0])*100:.2f}%\n\n")
        f.write("### Visualizations\n\n")
        f.write(f"1. [Network Structure](network_structure.png)\n")
        f.write(f"2. [Training History](training_history.png)\n")
        f.write(f"3. [Predictions](predictions.png)\n")
        if X.shape[1] == 1 and y.shape[1] == 1:
            f.write(f"4. [Learning Animation](learning_animation.gif)\n")
    
    logger.info(f"Completed NOMINATIVE case test with visualizations in {case_dir}")
    
    # Return the model for potential further testing
    return model

def test_accusative_case(nn_regression_data, case_definitions):
    """Test for ACCUSATIVE case: Model as object of training or evaluation."""
    # Get case info for logging
    case_info = case_definitions[Case.ACCUSATIVE]
    logger.info(f"Testing {Case.ACCUSATIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(OUTPUT_DIR, Case.ACCUSATIVE.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.ACCUSATIVE, linguistics_path)
    
    # Unpack the data
    X, y = nn_regression_data
    
    # Split data into train and test sets
    np.random.seed(42)
    indices = np.random.permutation(len(X))
    train_size = int(0.8 * len(X))
    train_indices = indices[:train_size]
    test_indices = indices[train_size:]
    
    X_train, y_train = X[train_indices], y[train_indices]
    X_test, y_test = X[test_indices], y[test_indices]
    
    logger.info(f"Split data into {len(X_train)} training samples and {len(X_test)} test samples")
    
    # Create a neural network model initially in NOMINATIVE case
    model = NeuralNetworkModel(
        name="AccModel",
        input_dim=X.shape[1],
        output_dim=y.shape[1],
        hidden_dims=[12, 6],
        activation='tanh'
    )
    
    # Train the model in NOMINATIVE case first (as preparation)
    logger.info("Pre-training model in NOMINATIVE case")
    model.train(X_train, y_train, epochs=100, learning_rate=0.01, batch_size=32)
    
    # Now transform to ACCUSATIVE case (model as object of evaluation)
    logger.info("Transforming model to ACCUSATIVE case (model as object of evaluation)")
    model.case = Case.ACCUSATIVE
    
    # Visualize model architecture (with ACCUSATIVE focus)
    network_structure_path = os.path.join(case_dir, "network_structure.png")
    Visualizer.plot_network_structure(
        model=model,
        title=f"Neural Network Structure in {Case.ACCUSATIVE.value} Case",
        save_path=network_structure_path
    )
    
    # In ACCUSATIVE case, we focus on evaluating the model
    logger.info("Evaluating model performance in ACCUSATIVE case")
    evaluation_results = model.evaluate(X_test, y_test)
    
    # Save evaluation metrics
    metrics_file = os.path.join(case_dir, "evaluation_metrics.txt")
    with open(metrics_file, "w") as f:
        f.write(f"Case: {Case.ACCUSATIVE.value} - {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical Role: {case_info['statistical_role']}\n\n")
        f.write(f"Neural Network Context: {case_info['neural_network_context']}\n\n")
        f.write("Evaluation Metrics:\n")
        for metric, value in evaluation_results.items():
            f.write(f"  {metric}: {value:.6f}\n")
    
    # Visualize evaluation results
    evaluation_path = os.path.join(case_dir, "evaluation_results.png")
    fig, axs = plt.subplots(2, 1, figsize=(10, 12))
    
    # Plot 1: Actual vs Predicted
    predictions = model.predict(X_test)
    
    if X_test.shape[1] == 1 and y_test.shape[1] == 1:
        # For 1D data, plot sorted values
        sort_idx = np.argsort(X_test[:, 0])
        X_test_sorted = X_test[sort_idx]
        y_test_sorted = y_test[sort_idx]
        pred_sorted = predictions[sort_idx]
        
        axs[0].scatter(X_test_sorted, y_test_sorted, alpha=0.7, label='Actual')
        axs[0].plot(X_test_sorted, pred_sorted, 'r-', linewidth=2, label='Predicted')
        axs[0].set_xlabel('Input')
        axs[0].set_ylabel('Output')
    else:
        # For higher dimensional data, plot direct comparisons
        axs[0].scatter(y_test, predictions, alpha=0.7)
        min_val = min(y_test.min(), predictions.min())
        max_val = max(y_test.max(), predictions.max())
        axs[0].plot([min_val, max_val], [min_val, max_val], 'r--', linewidth=2)
        axs[0].set_xlabel('Actual Values')
        axs[0].set_ylabel('Predicted Values')
    
    axs[0].set_title(f"Predictions vs Actual Values in {Case.ACCUSATIVE.value} Case")
    axs[0].grid(True, linestyle='--', alpha=0.6)
    if X_test.shape[1] == 1:  # Only add legend for 1D case
        axs[0].legend()
    
    # Plot 2: Error analysis
    errors = predictions - y_test
    if errors.shape[1] == 1:
        errors = errors.ravel()
    
    axs[1].hist(errors, bins=20, alpha=0.7, color='blue')
    axs[1].axvline(0, color='r', linestyle='--', linewidth=2)
    axs[1].set_xlabel('Prediction Error')
    axs[1].set_ylabel('Frequency')
    axs[1].set_title('Error Distribution')
    axs[1].grid(True, linestyle='--', alpha=0.6)
    
    plt.tight_layout()
    fig.savefig(evaluation_path)
    plt.close(fig)
    
    # Create animation showing evaluation process with different metrics
    animation_path = os.path.join(case_dir, "evaluation_animation.gif")
    
    # We'll simulate an evaluation process across different test subsets
    n_segments = 10
    test_segment_size = len(X_test) // n_segments
    
    # Collect metrics for each segment
    segment_metrics = []
    cumulative_metrics = []
    segment_sizes = []
    
    for i in range(1, n_segments + 1):
        # Use first i segments for this evaluation round
        segment_end = i * test_segment_size
        X_segment = X_test[:segment_end]
        y_segment = y_test[:segment_end]
        
        # Evaluate model on this segment
        segment_eval = model.evaluate(X_segment, y_segment)
        segment_metrics.append(segment_eval)
        segment_sizes.append(segment_end)
        
        # Calculate cumulative metrics
        if i == 1:
            cumulative_metrics.append(segment_eval)
        else:
            # Weight previous cumulative metrics with current segment
            new_cumulative = {}
            prev_size = segment_sizes[-2]
            curr_size = segment_sizes[-1]
            
            for metric, value in segment_eval.items():
                prev_value = cumulative_metrics[-1][metric]
                weighted_avg = (prev_value * prev_size + value * (curr_size - prev_size)) / curr_size
                new_cumulative[metric] = weighted_avg
            
            cumulative_metrics.append(new_cumulative)
    
    # Create animation of the evaluation process
    fig, axs = plt.subplots(2, 1, figsize=(10, 12))
    
    # Setup for metric curves
    metric_names = list(segment_metrics[0].keys())
    metric_colors = ['b', 'g', 'r', 'c', 'm', 'y']  # Colors for different metrics
    metric_lines = {}
    
    for i, metric in enumerate(metric_names):
        color = metric_colors[i % len(metric_colors)]
        # Two lines for each metric: segment-specific and cumulative
        segment_line, = axs[0].plot([], [], f"{color}-", label=f"{metric} (segment)", alpha=0.7)
        cumulative_line, = axs[0].plot([], [], f"{color}--", label=f"{metric} (cumulative)", linewidth=2)
        metric_lines[metric] = {'segment': segment_line, 'cumulative': cumulative_line}
    
    axs[0].set_xlim(1, n_segments)
    y_min = min(min(metrics[metric] for metrics in segment_metrics) for metric in metric_names)
    y_max = max(max(metrics[metric] for metrics in segment_metrics) for metric in metric_names)
    margin = (y_max - y_min) * 0.1
    axs[0].set_ylim(y_min - margin, y_max + margin)
    axs[0].set_xlabel("Evaluation Segment")
    axs[0].set_ylabel("Metric Value")
    axs[0].set_title(f"Evaluation Metrics in {Case.ACCUSATIVE.value} Case")
    axs[0].legend()
    axs[0].grid(True, linestyle='--', alpha=0.6)
    
    # Setup for prediction scatter plot
    scatter = axs[1].scatter([], [], alpha=0.7)
    ideal_line, = axs[1].plot([], [], 'r--', linewidth=2)
    axs[1].set_xlabel("Actual Values")
    axs[1].set_ylabel("Predicted Values")
    axs[1].set_title("Predictions vs Actual (Cumulative)")
    axs[1].grid(True, linestyle='--', alpha=0.6)
    
    # Text for segment information
    segment_text = axs[0].text(0.02, 0.95, '', transform=axs[0].transAxes)
    
    def init():
        for metric_data in metric_lines.values():
            metric_data['segment'].set_data([], [])
            metric_data['cumulative'].set_data([], [])
        scatter.set_offsets(np.empty((0, 2)))
        ideal_line.set_data([], [])
        segment_text.set_text('')
        return [line for metric_data in metric_lines.values() for line in metric_data.values()] + [scatter, ideal_line, segment_text]
    
    def update(frame):
        # Update metrics plot
        x_data = list(range(1, frame + 2))  # +2 because frame is 0-indexed and we want to start at 1
        
        for metric, metric_data in metric_lines.items():
            # Update segment metrics line
            segment_values = [segment_metrics[i][metric] for i in range(frame + 1)]
            metric_data['segment'].set_data(x_data, segment_values)
            
            # Update cumulative metrics line
            cumulative_values = [cumulative_metrics[i][metric] for i in range(frame + 1)]
            metric_data['cumulative'].set_data(x_data, cumulative_values)
        
        segment_text.set_text(f'Segment: {frame + 1}/{n_segments} (n={segment_sizes[frame]})')
        
        # Update prediction scatter plot
        current_segment_end = segment_sizes[frame]
        X_current = X_test[:current_segment_end]
        y_current = y_test[:current_segment_end]
        predictions_current = model.predict(X_current)
        
        if y_current.shape[1] == 1:
            # For 1D output, flatten the arrays
            scatter_data = np.column_stack((y_current.ravel(), predictions_current.ravel()))
            scatter.set_offsets(scatter_data)
            
            # Update ideal line
            min_val = min(y_current.min(), predictions_current.min())
            max_val = max(y_current.max(), predictions_current.max())
            ideal_line.set_data([min_val, max_val], [min_val, max_val])
            
            # Adjust axis limits for prediction plot
            axs[1].set_xlim(min_val - 0.1, max_val + 0.1)
            axs[1].set_ylim(min_val - 0.1, max_val + 0.1)
        
        return [line for metric_data in metric_lines.values() for line in metric_data.values()] + [scatter, ideal_line, segment_text]
    
    anim = animation.FuncAnimation(fig, update, frames=n_segments, 
                          init_func=init, blit=True)
    
    # Save animation
    anim.save(animation_path, writer='pillow', fps=2, dpi=100)
    plt.close(fig)
    logger.info(f"Created evaluation animation: {animation_path}")
    
    # Generate a detailed report
    report_path = os.path.join(case_dir, "report.md")
    with open(report_path, "w") as f:
        f.write(f"# Neural Network in {Case.ACCUSATIVE.value} Case\n\n")
        f.write(f"## {case_info['linguistic_meaning']}\n\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n\n")
        f.write(f"### Context\n\n")
        f.write(f"{case_info['neural_network_context']}\n\n")
        f.write("### Mathematical Representation\n\n")
        f.write(f"```\n{case_info['formula']}\n```\n\n")
        f.write("### Primary Methods\n\n")
        f.write(f"{case_info['primary_methods']}\n\n")
        f.write("### Example\n\n")
        f.write(f"{case_info['example']}\n\n")
        f.write("### Analysis Results\n\n")
        f.write(f"* Model architecture: Input({X.shape[1]}) → Hidden(12) → Hidden(6) → Output({y.shape[1]})\n")
        f.write(f"* Activation function: {model.activation}\n")
        f.write("* Evaluation metrics:\n")
        for metric, value in evaluation_results.items():
            f.write(f"  - {metric}: {value:.6f}\n")
        f.write("\n### Evaluation Process\n\n")
        f.write("The model was evaluated across multiple test segments to analyze the stability of its performance metrics. ")
        f.write("The animation shows how different evaluation metrics evolve as more test data is included.\n\n")
        f.write("### Visualizations\n\n")
        f.write(f"1. [Network Structure](network_structure.png)\n")
        f.write(f"2. [Evaluation Results](evaluation_results.png)\n")
        f.write(f"3. [Evaluation Animation](evaluation_animation.gif)\n")
        f.write("\n### ACCUSATIVE Case Significance\n\n")
        f.write("In the ACCUSATIVE case, the neural network is treated as the direct object of evaluation or training. ")
        f.write("This perspective emphasizes how the model receives updates and undergoes evaluation, ")
        f.write("rather than its active role in generating predictions.")
    
    logger.info(f"Completed ACCUSATIVE case test with visualizations in {case_dir}")
    
    # Return the model for potential further testing
    return model

def test_genitive_case(nn_regression_data, case_definitions):
    """Test for GENITIVE case: Model as source of outputs."""
    # Get case info for logging
    case_info = case_definitions[Case.GENITIVE]
    logger.info(f"Testing {Case.GENITIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(OUTPUT_DIR, Case.GENITIVE.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.GENITIVE, linguistics_path)
    
    # Unpack the data
    X, y = nn_regression_data
    
    # Create a simple NN model in GENITIVE case
    model = NeuralNetworkModel(
        name="GenModel",
        input_dim=X.shape[1],
        output_dim=y.shape[1],
        hidden_dims=[10, 5],
        activation='relu'
    )
    model.case = Case.GENITIVE  # Explicitly set to GENITIVE case
    
    # Log model details
    logger.info(f"Created neural network model with architecture: input_dim={X.shape[1]}, hidden_dims=[10, 5], output_dim={y.shape[1]}")
    
    # Visualize the model structure 
    network_structure_path = os.path.join(case_dir, "network_structure.png")
    Visualizer.plot_network_structure(
        model=model,
        title=f"Neural Network Structure in {Case.GENITIVE.value} Case",
        save_path=network_structure_path
    )
    
    # Train the model
    logger.info("Training model in GENITIVE case (model as source of outputs)")
    train_results = model.train(X, y, epochs=200, learning_rate=0.01, batch_size=32)
    
    # Visualize training progress
    training_path = os.path.join(case_dir, "training_history.png")
    Visualizer.plot_training_history(
        loss_history=model.loss_history,
        title=f"Training Loss in {Case.GENITIVE.value} Case",
        save_path=training_path
    )
    
    # Save final metrics
    metrics_file = os.path.join(case_dir, "metrics.txt")
    with open(metrics_file, "w") as f:
        f.write(f"Case: {Case.GENITIVE.value} - {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical Role: {case_info['statistical_role']}\n\n")
        f.write(f"Neural Network Context: {case_info['neural_network_context']}\n\n")
        f.write("Training Metrics:\n")
        f.write(f"  Final Loss: {model.loss_history[-1]:.6f}\n")
        f.write(f"  Initial Loss: {model.loss_history[0]:.6f}\n")
        f.write(f"  Improvement: {(1 - model.loss_history[-1]/model.loss_history[0])*100:.2f}%\n")
        f.write(f"  Epochs: {len(model.loss_history)}\n")
    
    # Generate predictions
    logger.info("Generating predictions with trained model")
    predictions = model.predict(X)
    
    # Visualize predictions vs targets
    prediction_path = os.path.join(case_dir, "predictions.png")
    fig, ax = plt.subplots(figsize=(10, 6))
    
    if X.shape[1] == 1 and y.shape[1] == 1:
        # For 1D data, sort by X for better visualization
        sort_idx = np.argsort(X[:, 0])
        X_sorted = X[sort_idx]
        y_sorted = y[sort_idx]
        pred_sorted = predictions[sort_idx]
        
        ax.scatter(X_sorted, y_sorted, alpha=0.7, label='Actual')
        ax.plot(X_sorted, pred_sorted, 'r-', linewidth=2, label='Predicted')
        ax.set_xlabel('Input')
        ax.set_ylabel('Output')
    else:
        # For higher dimensional data, just plot error distribution
        errors = np.sqrt(np.mean((predictions - y)**2, axis=1))
        ax.hist(errors, bins=20, alpha=0.7)
        ax.set_xlabel('Prediction Error (RMSE)')
        ax.set_ylabel('Frequency')
    
    ax.set_title(f"Model Predictions in {Case.GENITIVE.value} Case")
    ax.grid(True, linestyle='--', alpha=0.6)
    ax.legend()
    fig.tight_layout()
    fig.savefig(prediction_path)
    plt.close(fig)
    
    # Visualize output distributions
    output_distribution_path = os.path.join(case_dir, "output_distribution.png")
    fig, ax = plt.subplots(figsize=(10, 6))
    
    if y.shape[1] == 1:
        ax.hist(predictions, bins=20, alpha=0.7, label='Predicted')
        ax.hist(y, bins=20, alpha=0.7, label='Actual')
        ax.set_xlabel('Output')
        ax.set_ylabel('Frequency')
    else:
        # For higher dimensional data, just plot error distribution
        errors = np.sqrt(np.mean((predictions - y)**2, axis=1))
        ax.hist(errors, bins=20, alpha=0.7)
        ax.set_xlabel('Prediction Error (RMSE)')
        ax.set_ylabel('Frequency')
    
    ax.set_title(f"Output Distributions in {Case.GENITIVE.value} Case")
    ax.grid(True, linestyle='--', alpha=0.6)
    ax.legend()
    fig.tight_layout()
    fig.savefig(output_distribution_path)
    plt.close(fig)
    
    # Visualize final layer activations
    final_activations_path = os.path.join(case_dir, "final_activations.png")
    fig, ax = plt.subplots(figsize=(10, 6))
    
    final_activations = model._forward_with_activations(X)[-1]
    if final_activations.shape[1] == 1:
        ax.hist(final_activations, bins=20, alpha=0.7)
        ax.set_xlabel('Activation')
        ax.set_ylabel('Frequency')
    else:
        # For higher dimensional data, just plot error distribution
        errors = np.sqrt(np.mean((predictions - y)**2, axis=1))
        ax.hist(errors, bins=20, alpha=0.7)
        ax.set_xlabel('Prediction Error (RMSE)')
        ax.set_ylabel('Frequency')
    
    ax.set_title(f"Final Layer Activations in {Case.GENITIVE.value} Case")
    ax.grid(True, linestyle='--', alpha=0.6)
    fig.tight_layout()
    fig.savefig(final_activations_path)
    plt.close(fig)
    
    # Generate a report
    report_path = os.path.join(case_dir, "report.md")
    with open(report_path, "w") as f:
        f.write(f"# Neural Network in {Case.GENITIVE.value} Case\n\n")
        f.write(f"## {case_info['linguistic_meaning']}\n\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n\n")
        f.write(f"### Context\n\n")
        f.write(f"{case_info['neural_network_context']}\n\n")
        f.write("### Mathematical Representation\n\n")
        f.write(f"```\n{case_info['formula']}\n```\n\n")
        f.write("### Primary Methods\n\n")
        f.write(f"{case_info['primary_methods']}\n\n")
        f.write("### Example\n\n")
        f.write(f"{case_info['example']}\n\n")
        f.write("### Analysis Results\n\n")
        f.write(f"* Model architecture: Input({X.shape[1]}) → Hidden(10) → Hidden(5) → Output({y.shape[1]})\n")
        f.write(f"* Activation function: {model.activation}\n")
        f.write(f"* Initial loss: {model.loss_history[0]:.6f}\n")
        f.write(f"* Final loss: {model.loss_history[-1]:.6f}\n")
        f.write(f"* Improvement: {(1 - model.loss_history[-1]/model.loss_history[0])*100:.2f}%\n\n")
        f.write("### Visualizations\n\n")
        f.write(f"1. [Network Structure](network_structure.png)\n")
        f.write(f"2. [Training History](training_history.png)\n")
        f.write(f"3. [Predictions](predictions.png)\n")
        f.write(f"4. [Output Distributions](output_distribution.png)\n")
        f.write(f"5. [Final Layer Activations](final_activations.png)\n")
    
    logger.info(f"Completed GENITIVE case test with visualizations in {case_dir}")
    
    # Return the model for potential further testing
    return model

def test_locative_case(nn_classification_data, case_definitions):
    """Test for LOCATIVE case: Model as internal representational space."""
    # Get case info for logging
    case_info = case_definitions[Case.LOCATIVE]
    logger.info(f"Testing {Case.LOCATIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(OUTPUT_DIR, Case.LOCATIVE.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.LOCATIVE, linguistics_path)
    
    # Unpack the data
    X, y = nn_classification_data
    
    # Convert y to one-hot encoding
    y_one_hot = np.zeros((y.shape[0], np.max(y) + 1))
    y_one_hot[np.arange(y.shape[0]), y] = 1
    
    # Create a simple NN model in LOCATIVE case
    model = NeuralNetworkModel(
        name="LocModel",
        input_dim=X.shape[1],
        output_dim=y_one_hot.shape[1],
        hidden_dims=[10, 5],
        activation='relu'
    )
    model.case = Case.LOCATIVE  # Explicitly set to LOCATIVE case
    
    # Log model details
    logger.info(f"Created neural network model with architecture: input_dim={X.shape[1]}, hidden_dims=[10, 5], output_dim={y_one_hot.shape[1]}")
    
    # Visualize the model structure 
    network_structure_path = os.path.join(case_dir, "network_structure.png")
    Visualizer.plot_network_structure(
        model=model,
        title=f"Neural Network Structure in {Case.LOCATIVE.value} Case",
        save_path=network_structure_path
    )
    
    # Train the model
    logger.info("Training model in LOCATIVE case (model as internal representational space)")
    train_results = model.train(X, y_one_hot, epochs=200, learning_rate=0.01, batch_size=32, loss_function='categorical_crossentropy')
    
    # Visualize training progress
    training_path = os.path.join(case_dir, "training_history.png")
    Visualizer.plot_training_history(
        loss_history=model.loss_history,
        title=f"Training Loss in {Case.LOCATIVE.value} Case",
        save_path=training_path
    )
    
    # Save final metrics
    metrics_file = os.path.join(case_dir, "metrics.txt")
    with open(metrics_file, "w") as f:
        f.write(f"Case: {Case.LOCATIVE.value} - {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical Role: {case_info['statistical_role']}\n\n")
        f.write(f"Neural Network Context: {case_info['neural_network_context']}\n\n")
        f.write("Training Metrics:\n")
        f.write(f"  Final Loss: {model.loss_history[-1]:.6f}\n")
        f.write(f"  Initial Loss: {model.loss_history[0]:.6f}\n")
        f.write(f"  Improvement: {(1 - model.loss_history[-1]/model.loss_history[0])*100:.2f}%\n")
        f.write(f"  Epochs: {len(model.loss_history)}\n")
    
    # Generate predictions
    logger.info("Generating predictions with trained model")
    predictions = model.predict(X)
    
    # Visualize predictions vs targets
    prediction_path = os.path.join(case_dir, "predictions.png")
    fig, ax = plt.subplots(figsize=(10, 6))
    
    if X.shape[1] == 1 and y.shape[1] == 1:
        # For 1D data, sort by X for better visualization
        sort_idx = np.argsort(X[:, 0])
        X_sorted = X[sort_idx]
        y_sorted = y[sort_idx]
        pred_sorted = predictions[sort_idx]
        
        ax.scatter(X_sorted, y_sorted, alpha=0.7, label='Actual')
        ax.plot(X_sorted, pred_sorted, 'r-', linewidth=2, label='Predicted')
        ax.set_xlabel('Input')
        ax.set_ylabel('Output')
    else:
        # For higher dimensional data, just plot error distribution
        errors = np.sqrt(np.mean((predictions - y_one_hot)**2, axis=1))
        ax.hist(errors, bins=20, alpha=0.7)
        ax.set_xlabel('Prediction Error (RMSE)')
        ax.set_ylabel('Frequency')
    
    ax.set_title(f"Model Predictions in {Case.LOCATIVE.value} Case")
    ax.grid(True, linestyle='--', alpha=0.6)
    ax.legend()
    fig.tight_layout()
    fig.savefig(prediction_path)
    plt.close(fig)
    
    # Visualize hidden layer activations
    hidden_activations_path = os.path.join(case_dir, "hidden_activations.png")
    fig, ax = plt.subplots(figsize=(10, 6))
    
    hidden_activations = model._forward_with_activations(X)[1]
    if hidden_activations.shape[1] == 1:
        ax.hist(hidden_activations, bins=20, alpha=0.7)
        ax.set_xlabel('Activation')
        ax.set_ylabel('Frequency')
    else:
        # For higher dimensional data, use PCA or t-SNE for dimensionality reduction
        pca = PCA(n_components=2)
        hidden_activations_2d = pca.fit_transform(hidden_activations)
        
        # Plot PCA components
        ax.scatter(hidden_activations_2d[:, 0], hidden_activations_2d[:, 1], c=y, cmap='viridis', alpha=0.7)
        ax.set_xlabel('PCA Component 1')
        ax.set_ylabel('PCA Component 2')
    
    ax.set_title(f"Hidden Layer Activations in {Case.LOCATIVE.value} Case")
    ax.grid(True, linestyle='--', alpha=0.6)
    fig.tight_layout()
    fig.savefig(hidden_activations_path)
    plt.close(fig)
    
    # Visualize activation patterns for specific samples
    sample_activations_path = os.path.join(case_dir, "sample_activations.png")
    fig, axs = plt.subplots(2, 2, figsize=(12, 10))
    
    sample_indices = [0, 1, 2, 3]
    for i, idx in enumerate(sample_indices):
        sample_activations = model._forward_with_activations(X[idx:idx+1])
        for j, activations in enumerate(sample_activations):
            axs[i//2, i%2].plot(activations[0], label=f'Layer {j+1}')
        axs[i//2, i%2].set_title(f"Sample {idx+1} (Class {y[idx]})")
        axs[i//2, i%2].legend()
    
    fig.suptitle(f"Activation Patterns for Specific Samples in {Case.LOCATIVE.value} Case")
    fig.tight_layout()
    fig.savefig(sample_activations_path)
    plt.close(fig)
    
    # Generate a report
    report_path = os.path.join(case_dir, "report.md")
    with open(report_path, "w") as f:
        f.write(f"# Neural Network in {Case.LOCATIVE.value} Case\n\n")
        f.write(f"## {case_info['linguistic_meaning']}\n\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n\n")
        f.write(f"### Context\n\n")
        f.write(f"{case_info['neural_network_context']}\n\n")
        f.write("### Mathematical Representation\n\n")
        f.write(f"```\n{case_info['formula']}\n```\n\n")
        f.write("### Primary Methods\n\n")
        f.write(f"{case_info['primary_methods']}\n\n")
        f.write("### Example\n\n")
        f.write(f"{case_info['example']}\n\n")
        f.write("### Analysis Results\n\n")
        f.write(f"* Model architecture: Input({X.shape[1]}) → Hidden(10) → Hidden(5) → Output({y_one_hot.shape[1]})\n")
        f.write(f"* Activation function: {model.activation}\n")
        f.write(f"* Initial loss: {model.loss_history[0]:.6f}\n")
        f.write(f"* Final loss: {model.loss_history[-1]:.6f}\n")
        f.write(f"* Improvement: {(1 - model.loss_history[-1]/model.loss_history[0])*100:.2f}%\n\n")
        f.write("### Visualizations\n\n")
        f.write(f"1. [Network Structure](network_structure.png)\n")
        f.write(f"2. [Training History](training_history.png)\n")
        f.write(f"3. [Predictions](predictions.png)\n")
        f.write(f"4. [Hidden Layer Activations](hidden_activations.png)\n")
        f.write(f"5. [Activation Patterns for Specific Samples](sample_activations.png)\n")
    
    logger.info(f"Completed LOCATIVE case test with visualizations in {case_dir}")
    
    # Return the model for potential further testing
    return model

def test_ablative_case(nn_regression_data, case_definitions):
    """Test for ABLATIVE case: Model as source of errors and gradients."""
    # Get case info for logging
    case_info = case_definitions[Case.ABLATIVE]
    logger.info(f"Testing {Case.ABLATIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(OUTPUT_DIR, Case.ABLATIVE.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.ABLATIVE, linguistics_path)
    
    # Unpack the data
    X, y = nn_regression_data
    
    # Create a simple NN model in ABLATIVE case
    model = NeuralNetworkModel(
        name="AblModel",
        input_dim=X.shape[1],
        output_dim=y.shape[1],
        hidden_dims=[10, 5],
        activation='relu'
    )
    model.case = Case.ABLATIVE  # Explicitly set to ABLATIVE case
    
    # Log model details
    logger.info(f"Created neural network model with architecture: input_dim={X.shape[1]}, hidden_dims=[10, 5], output_dim={y.shape[1]}")
    
    # Visualize the model structure 
    network_structure_path = os.path.join(case_dir, "network_structure.png")
    Visualizer.plot_network_structure(
        model=model,
        title=f"Neural Network Structure in {Case.ABLATIVE.value} Case",
        save_path=network_structure_path
    )
    
    # Train the model
    logger.info("Training model in ABLATIVE case (model as source of errors and gradients)")
    train_results = model.train(X, y, epochs=200, learning_rate=0.01, batch_size=32)
    
    # Visualize training progress
    training_path = os.path.join(case_dir, "training_history.png")
    Visualizer.plot_training_history(
        loss_history=model.loss_history,
        title=f"Training Loss in {Case.ABLATIVE.value} Case",
        save_path=training_path
    )
    
    # Save final metrics
    metrics_file = os.path.join(case_dir, "metrics.txt")
    with open(metrics_file, "w") as f:
        f.write(f"Case: {Case.ABLATIVE.value} - {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical Role: {case_info['statistical_role']}\n\n")
        f.write(f"Neural Network Context: {case_info['neural_network_context']}\n\n")
        f.write("Training Metrics:\n")
        f.write(f"  Final Loss: {model.loss_history[-1]:.6f}\n")
        f.write(f"  Initial Loss: {model.loss_history[0]:.6f}\n")
        f.write(f"  Improvement: {(1 - model.loss_history[-1]/model.loss_history[0])*100:.2f}%\n")
        f.write(f"  Epochs: {len(model.loss_history)}\n")
    
    # Generate predictions
    logger.info("Generating predictions with trained model")
    predictions = model.predict(X)
    
    # Visualize predictions vs targets
    prediction_path = os.path.join(case_dir, "predictions.png")
    fig, ax = plt.subplots(figsize=(10, 6))
    
    if X.shape[1] == 1 and y.shape[1] == 1:
        # For 1D data, sort by X for better visualization
        sort_idx = np.argsort(X[:, 0])
        X_sorted = X[sort_idx]
        y_sorted = y[sort_idx]
        pred_sorted = predictions[sort_idx]
        
        ax.scatter(X_sorted, y_sorted, alpha=0.7, label='Actual')
        ax.plot(X_sorted, pred_sorted, 'r-', linewidth=2, label='Predicted')
        ax.set_xlabel('Input')
        ax.set_ylabel('Output')
    else:
        # For higher dimensional data, just plot error distribution
        errors = np.sqrt(np.mean((predictions - y)**2, axis=1))
        ax.hist(errors, bins=20, alpha=0.7)
        ax.set_xlabel('Prediction Error (RMSE)')
        ax.set_ylabel('Frequency')
    
    ax.set_title(f"Model Predictions in {Case.ABLATIVE.value} Case")
    ax.grid(True, linestyle='--', alpha=0.6)
    ax.legend()
    fig.tight_layout()
    fig.savefig(prediction_path)
    plt.close(fig)
    
    # Visualize error distributions
    error_distribution_path = os.path.join(case_dir, "error_distribution.png")
    fig, ax = plt.subplots(figsize=(10, 6))
    
    errors = predictions - y
    if errors.shape[1] == 1:
        ax.hist(errors, bins=20, alpha=0.7)
        ax.set_xlabel('Error')
        ax.set_ylabel('Frequency')
    else:
        # For higher dimensional data, just plot error distribution
        errors = np.sqrt(np.mean((predictions - y)**2, axis=1))
        ax.hist(errors, bins=20, alpha=0.7)
        ax.set_xlabel('Prediction Error (RMSE)')
        ax.set_ylabel('Frequency')
    
    ax.set_title(f"Error Distribution in {Case.ABLATIVE.value} Case")
    ax.grid(True, linestyle='--', alpha=0.6)
    fig.tight_layout()
    fig.savefig(error_distribution_path)
    plt.close(fig)
    
    # Visualize gradient magnitudes per layer
    gradient_magnitudes_path = os.path.join(case_dir, "gradient_magnitudes.png")
    fig, ax = plt.subplots(figsize=(10, 6))
    
    gradients = model._backward_with_metrics(X, y)[1]
    for i, layer_gradients in enumerate(gradients):
        layer_magnitudes = np.linalg.norm(layer_gradients, axis=1)
        ax.hist(layer_magnitudes, bins=20, alpha=0.7, label=f'Layer {i+1}')
    
    ax.set_xlabel('Gradient Magnitude')
    ax.set_ylabel('Frequency')
    ax.set_title(f"Gradient Magnitudes per Layer in {Case.ABLATIVE.value} Case")
    ax.grid(True, linestyle='--', alpha=0.6)
    ax.legend()
    fig.tight_layout()
    fig.savefig(gradient_magnitudes_path)
    plt.close(fig)
    
    # Generate a report
    report_path = os.path.join(case_dir, "report.md")
    with open(report_path, "w") as f:
        f.write(f"# Neural Network in {Case.ABLATIVE.value} Case\n\n")
        f.write(f"## {case_info['linguistic_meaning']}\n\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n\n")
        f.write(f"### Context\n\n")
        f.write(f"{case_info['neural_network_context']}\n\n")
        f.write("### Mathematical Representation\n\n")
        f.write(f"```\n{case_info['formula']}\n```\n\n")
        f.write("### Primary Methods\n\n")
        f.write(f"{case_info['primary_methods']}\n\n")
        f.write("### Example\n\n")
        f.write(f"{case_info['example']}\n\n")
        f.write("### Analysis Results\n\n")
        f.write(f"* Model architecture: Input({X.shape[1]}) → Hidden(10) → Hidden(5) → Output({y.shape[1]})\n")
        f.write(f"* Activation function: {model.activation}\n")
        f.write(f"* Initial loss: {model.loss_history[0]:.6f}\n")
        f.write(f"* Final loss: {model.loss_history[-1]:.6f}\n")
        f.write(f"* Improvement: {(1 - model.loss_history[-1]/model.loss_history[0])*100:.2f}%\n\n")
        f.write("### Visualizations\n\n")
        f.write(f"1. [Network Structure](network_structure.png)\n")
        f.write(f"2. [Training History](training_history.png)\n")
        f.write(f"3. [Predictions](predictions.png)\n")
        f.write(f"4. [Error Distribution](error_distribution.png)\n")
        f.write(f"5. [Gradient Magnitudes per Layer](gradient_magnitudes.png)\n")
    
    logger.info(f"Completed ABLATIVE case test with visualizations in {case_dir}")
    
    # Return the model for potential further testing
    return model

def test_vocative_case(nn_classification_data, case_definitions):
    """Test for VOCATIVE case: Model as interactive interface."""
    # Get case info for logging
    case_info = case_definitions[Case.VOCATIVE]
    logger.info(f"Testing {Case.VOCATIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(OUTPUT_DIR, Case.VOCATIVE.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.VOCATIVE, linguistics_path)
    
    # Unpack the data
    X, y = nn_classification_data
    
    # Convert y to one-hot encoding
    y_one_hot = np.zeros((y.shape[0], np.max(y) + 1))
    y_one_hot[np.arange(y.shape[0]), y] = 1
    
    # Create a simple NN model in VOCATIVE case
    model = NeuralNetworkModel(
        name="VocModel",
        input_dim=X.shape[1],
        output_dim=y_one_hot.shape[1],
        hidden_dims=[10, 5],
        activation='relu'
    )
    model.case = Case.VOCATIVE  # Explicitly set to VOCATIVE case
    
    # Log model details
    logger.info(f"Created neural network model with architecture: input_dim={X.shape[1]}, hidden_dims=[10, 5], output_dim={y_one_hot.shape[1]}")
    
    # Visualize the model structure 
    network_structure_path = os.path.join(case_dir, "network_structure.png")
    Visualizer.plot_network_structure(
        model=model,
        title=f"Neural Network Structure in {Case.VOCATIVE.value} Case",
        save_path=network_structure_path
    )
    
    # Train the model
    logger.info("Training model in VOCATIVE case (model as interactive interface)")
    train_results = model.train(X, y_one_hot, epochs=200, learning_rate=0.01, batch_size=32, loss_function='categorical_crossentropy')
    
    # Visualize training progress
    training_path = os.path.join(case_dir, "training_history.png")
    Visualizer.plot_training_history(
        loss_history=model.loss_history,
        title=f"Training Loss in {Case.VOCATIVE.value} Case",
        save_path=training_path
    )
    
    # Save final metrics
    metrics_file = os.path.join(case_dir, "metrics.txt")
    with open(metrics_file, "w") as f:
        f.write(f"Case: {Case.VOCATIVE.value} - {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical Role: {case_info['statistical_role']}\n\n")
        f.write(f"Neural Network Context: {case_info['neural_network_context']}\n\n")
        f.write("Training Metrics:\n")
        f.write(f"  Final Loss: {model.loss_history[-1]:.6f}\n")
        f.write(f"  Initial Loss: {model.loss_history[0]:.6f}\n")
        f.write(f"  Improvement: {(1 - model.loss_history[-1]/model.loss_history[0])*100:.2f}%\n")
        f.write(f"  Epochs: {len(model.loss_history)}\n")
    
    # Generate predictions
    logger.info("Generating predictions with trained model")
    predictions = model.predict(X)
    
    # Visualize predictions vs targets
    prediction_path = os.path.join(case_dir, "predictions.png")
    fig, ax = plt.subplots(figsize=(10, 6))
    
    if X.shape[1] == 1 and y.shape[1] == 1:
        # For 1D data, sort by X for better visualization
        sort_idx = np.argsort(X[:, 0])
        X_sorted = X[sort_idx]
        y_sorted = y[sort_idx]
        pred_sorted = predictions[sort_idx]
        
        ax.scatter(X_sorted, y_sorted, alpha=0.7, label='Actual')
        ax.plot(X_sorted, pred_sorted, 'r-', linewidth=2, label='Predicted')
        ax.set_xlabel('Input')
        ax.set_ylabel('Output')
    else:
        # For higher dimensional data, just plot error distribution
        errors = np.sqrt(np.mean((predictions - y_one_hot)**2, axis=1))
        ax.hist(errors, bins=20, alpha=0.7)
        ax.set_xlabel('Prediction Error (RMSE)')
        ax.set_ylabel('Frequency')
    
    ax.set_title(f"Model Predictions in {Case.VOCATIVE.value} Case")
    ax.grid(True, linestyle='--', alpha=0.6)
    ax.legend()
    fig.tight_layout()
    fig.savefig(prediction_path)
    plt.close(fig)
    
    # Visualize decision boundary
    decision_boundary_path = os.path.join(case_dir, "decision_boundary.png")
    fig, ax = plt.subplots(figsize=(10, 6))
    
    if X.shape[1] == 2:
        # For 2D data, plot decision boundary
        x_min, x_max = X[:, 0].min() - 1, X[:, 0].max() + 1
        y_min, y_max = X[:, 1].min() - 1, X[:, 1].max() + 1
        xx, yy = np.meshgrid(np.linspace(x_min, x_max, 100),
                             np.linspace(y_min, y_max, 100))
        Z = model.predict(np.c_[xx.ravel(), yy.ravel()])
        Z = np.argmax(Z, axis=1)
        Z = Z.reshape(xx.shape)
        
        ax.contourf(xx, yy, Z, alpha=0.8, cmap='viridis')
        ax.scatter(X[:, 0], X[:, 1], c=y, cmap='viridis', edgecolors='k', alpha=0.6)
        ax.set_xlabel('Feature 1')
        ax.set_ylabel('Feature 2')
    else:
        # For higher dimensional data, just plot error distribution
        errors = np.sqrt(np.mean((predictions - y_one_hot)**2, axis=1))
        ax.hist(errors, bins=20, alpha=0.7)
        ax.set_xlabel('Prediction Error (RMSE)')
        ax.set_ylabel('Frequency')
    
    ax.set_title(f"Decision Boundary in {Case.VOCATIVE.value} Case")
    ax.grid(True, linestyle='--', alpha=0.6)
    fig.tight_layout()
    fig.savefig(decision_boundary_path)
    plt.close(fig)
    
    # Create animation showing decision boundary evolution
    animation_path = os.path.join(case_dir, "decision_boundary_animation.gif")
    
    # We'll simulate an evaluation process across different test subsets
    n_segments = 10
    test_segment_size = len(X) // n_segments
    
    # Collect metrics for each segment
    segment_metrics = []
    cumulative_metrics = []
    segment_sizes = []
    
    for i in range(1, n_segments + 1):
        # Use first i segments for this evaluation round
        segment_end = i * test_segment_size
        X_segment = X[:segment_end]
        y_segment = y[:segment_end]
        
        # Evaluate model on this segment
        segment_eval = model.evaluate(X_segment, y_segment)
        segment_metrics.append(segment_eval)
        segment_sizes.append(segment_end)
        
        # Calculate cumulative metrics
        if i == 1:
            cumulative_metrics.append(segment_eval)
        else:
            # Weight previous cumulative metrics with current segment
            new_cumulative = {}
            prev_size = segment_sizes[-2]
            curr_size = segment_sizes[-1]
            
            for metric, value in segment_eval.items():
                prev_value = cumulative_metrics[-1][metric]
                weighted_avg = (prev_value * prev_size + value * (curr_size - prev_size)) / curr_size
                new_cumulative[metric] = weighted_avg
            
            cumulative_metrics.append(new_cumulative)
    
    # Create animation of the evaluation process
    fig, axs = plt.subplots(2, 1, figsize=(10, 12))
    
    # Setup for metric curves
    metric_names = list(segment_metrics[0].keys())
    metric_colors = ['b', 'g', 'r', 'c', 'm', 'y']  # Colors for different metrics
    metric_lines = {}
    
    for i, metric in enumerate(metric_names):
        color = metric_colors[i % len(metric_colors)]
        # Two lines for each metric: segment-specific and cumulative
        segment_line, = axs[0].plot([], [], f"{color}-", label=f"{metric} (segment)", alpha=0.7)
        cumulative_line, = axs[0].plot([], [], f"{color}--", label=f"{metric} (cumulative)", linewidth=2)
        metric_lines[metric] = {'segment': segment_line, 'cumulative': cumulative_line}
    
    axs[0].set_xlim(1, n_segments)
    y_min = min(min(metrics[metric] for metrics in segment_metrics) for metric in metric_names)
    y_max = max(max(metrics[metric] for metrics in segment_metrics) for metric in metric_names)
    margin = (y_max - y_min) * 0.1
    axs[0].set_ylim(y_min - margin, y_max + margin)
    axs[0].set_xlabel("Evaluation Segment")
    axs[0].set_ylabel("Metric Value")
    axs[0].set_title(f"Evaluation Metrics in {Case.VOCATIVE.value} Case")
    axs[0].legend()
    axs[0].grid(True, linestyle='--', alpha=0.6)
    
    # Setup for prediction scatter plot
    scatter = axs[1].scatter([], [], alpha=0.7)
    ideal_line, = axs[1].plot([], [], 'r--', linewidth=2)
    axs[1].set_xlabel("Actual Values")
    axs[1].set_ylabel("Predicted Values")
    axs[1].set_title("Predictions vs Actual (Cumulative)")
    axs[1].grid(True, linestyle='--', alpha=0.6)
    
    # Text for segment information
    segment_text = axs[0].text(0.02, 0.95, '', transform=axs[0].transAxes)
    
    def init():
        for metric_data in metric_lines.values():
            metric_data['segment'].set_data([], [])
            metric_data['cumulative'].set_data([], [])
        scatter.set_offsets(np.empty((0, 2)))
        ideal_line.set_data([], [])
        segment_text.set_text('')
        return [line for metric_data in metric_lines.values() for line in metric_data.values()] + [scatter, ideal_line, segment_text]
    
    def update(frame):
        # Update metrics plot
        x_data = list(range(1, frame + 2))  # +2 because frame is 0-indexed and we want to start at 1
        
        for metric, metric_data in metric_lines.items():
            # Update segment metrics line
            segment_values = [segment_metrics[i][metric] for i in range(frame + 1)]
            metric_data['segment'].set_data(x_data, segment_values)
            
            # Update cumulative metrics line
            cumulative_values = [cumulative_metrics[i][metric] for i in range(frame + 1)]
            metric_data['cumulative'].set_data(x_data, cumulative_values)
        
        segment_text.set_text(f'Segment: {frame + 1}/{n_segments} (n={segment_sizes[frame]})')
        
        # Update prediction scatter plot
        current_segment_end = segment_sizes[frame]
        X_current = X_test[:current_segment_end]
        y_current = y_test[:current_segment_end]
        predictions_current = model.predict(X_current)
        
        if y_current.shape[1] == 1:
            # For 1D output, flatten the arrays
            scatter_data = np.column_stack((y_current.ravel(), predictions_current.ravel()))
            scatter.set_offsets(scatter_data)
            
            # Update ideal line
            min_val = min(y_current.min(), predictions_current.min())
            max_val = max(y_current.max(), predictions_current.max())
            ideal_line.set_data([min_val, max_val], [min_val, max_val])
            
            # Adjust axis limits for prediction plot
            axs[1].set_xlim(min_val - 0.1, max_val + 0.1)
            axs[1].set_ylim(min_val - 0.1, max_val + 0.1)
        
        return [line for metric_data in metric_lines.values() for line in metric_data.values()] + [scatter, ideal_line, segment_text]
    
    anim = animation.FuncAnimation(fig, update, frames=n_segments, 
                          init_func=init, blit=True)
    
    # Save animation
    anim.save(animation_path, writer='pillow', fps=2, dpi=100)
    plt.close(fig)
    logger.info(f"Created evaluation animation: {animation_path}")
    
    # Generate a detailed report
    report_path = os.path.join(case_dir, "report.md")
    with open(report_path, "w") as f:
        f.write(f"# Neural Network in {Case.VOCATIVE.value} Case\n\n")
        f.write(f"## {case_info['linguistic_meaning']}\n\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n\n")
        f.write(f"### Context\n\n")
        f.write(f"{case_info['neural_network_context']}\n\n")
        f.write("### Mathematical Representation\n\n")
        f.write(f"```\n{case_info['formula']}\n```\n\n")
        f.write("### Primary Methods\n\n")
        f.write(f"{case_info['primary_methods']}\n\n")
        f.write("### Example\n\n")
        f.write(f"{case_info['example']}\n\n")
        f.write("### Analysis Results\n\n")
        f.write(f"* Model architecture: Input({X.shape[1]}) → Hidden(10) → Hidden(5) → Output({y_one_hot.shape[1]})\n")
        f.write(f"* Activation function: {model.activation}\n")
        f.write("* Evaluation metrics:\n")
        for metric, value in evaluation_results.items():
            f.write(f"  - {metric}: {value:.6f}\n")
        f.write("\n### Evaluation Process\n\n")
        f.write("The model was evaluated across multiple test segments to analyze the stability of its performance metrics. ")
        f.write("The animation shows how different evaluation metrics evolve as more test data is included.\n\n")
        f.write("### Visualizations\n\n")
        f.write(f"1. [Network Structure](network_structure.png)\n")
        f.write(f"2. [Evaluation Results](evaluation_results.png)\n")
        f.write(f"3. [Evaluation Animation](evaluation_animation.gif)\n")
        f.write("\n### VOCATIVE Case Significance\n\n")
        f.write("In the VOCATIVE case, the neural network is treated as an addressable entity with a query/response interface. ")
        f.write("This perspective emphasizes how the model interacts with users and receives input data. ")
        f.write("The model's response is based on the input data, rather than its active role in generating predictions.")
    
    logger.info(f"Completed VOCATIVE case test with visualizations in {case_dir}")
    
    # Return the model for potential further testing
    return model

def run_all_case_tests():
    """Run all case tests and generate overview visualization."""
    case_defs = CaseDefinitions.get_all_cases()
    regression_data = DataGenerator.regression_data()
    classification_data = DataGenerator.classification_data()
    
    model_dict = {}
    cases_to_run = [
        Case.NOMINATIVE, Case.ACCUSATIVE, Case.DATIVE, Case.GENITIVE,
        Case.INSTRUMENTAL, Case.LOCATIVE, Case.ABLATIVE, Case.VOCATIVE
    ]
    
    # Run each case test
    for case in cases_to_run:
        try:
            logger.info(f"Running test for {case.value} case")
            
            if case == Case.NOMINATIVE:
                model = test_nominative_case(regression_data, case_defs)
            elif case == Case.ACCUSATIVE:
                model = test_accusative_case(regression_data, case_defs)
            elif case == Case.DATIVE:
                model = test_dative_case(regression_data, case_defs)
            elif case == Case.GENITIVE:
                model = test_genitive_case(regression_data, case_defs)
            elif case == Case.INSTRUMENTAL:
                model = test_instrumental_case(regression_data, case_defs)
            elif case == Case.LOCATIVE:
                model = test_locative_case(classification_data, case_defs)
            elif case == Case.ABLATIVE:
                model = test_ablative_case(regression_data, case_defs)
            elif case == Case.VOCATIVE:
                model = test_vocative_case(classification_data, case_defs)
            
            # Store model in dictionary
            model_dict[case] = model
            
        except Exception as e:
            logger.error(f"Error in {case.value} case test: {str(e)}")
    
    # Generate overview visualization
    overview_path = os.path.join(OUTPUT_DIR, "neural_network_cases_overview.png")
    create_overview_visualization(overview_path)
    
    logger.info(f"All Neural Network case tests completed. Overview available at {overview_path}")
    return model_dict

def create_overview_visualization(save_path: str) -> None:
    """Create an overview visualization of all Neural Network cases."""
    fig, axs = plt.subplots(4, 2, figsize=(14, 20))
    axs = axs.flatten()
    
    cases = [
        Case.NOMINATIVE, Case.ACCUSATIVE, Case.DATIVE, Case.GENITIVE,
        Case.INSTRUMENTAL, Case.LOCATIVE, Case.ABLATIVE, Case.VOCATIVE
    ]
    
    case_defs = CaseDefinitions.get_all_cases()
    
    for i, case in enumerate(cases):
        ax = axs[i]
        case_info = case_defs[case]
        
        # Set up the axis
        ax.axis('off')
        
        # Add case title
        ax.text(0.5, 0.9, f"{case.value}: {case_info['linguistic_meaning']}", 
                ha='center', fontsize=14, fontweight='bold', transform=ax.transAxes)
        
        # Add Neural Network context
        ax.text(0.5, 0.8, case_info['neural_network_context'], 
                ha='center', fontsize=10, transform=ax.transAxes, 
                bbox=dict(boxstyle='round,pad=0.3', facecolor='lightgreen', alpha=0.3))
        
        # Add formula
        ax.text(0.5, 0.6, f"Formula: {case_info['formula']}", 
                ha='center', fontsize=10, transform=ax.transAxes, 
                bbox=dict(boxstyle='round,pad=0.3', facecolor='lightyellow', alpha=0.3))
        
        # Add primary methods
        ax.text(0.5, 0.4, f"Methods: {case_info['primary_methods']}", 
                ha='center', fontsize=10, transform=ax.transAxes,
                bbox=dict(boxstyle='round,pad=0.3', facecolor='lightblue', alpha=0.3))
        
        # Add example
        ax.text(0.5, 0.2, f"Example: {case_info['example']}", 
                ha='center', fontsize=10, fontweight='italic', transform=ax.transAxes)
    
    # Add title
    fig.suptitle("CEREBRUM Cases for Neural Network Models", fontsize=20, y=0.98)
    
    # Add explanatory text
    fig.text(0.5, 0.01, 
             "CEREBRUM framework applies linguistic cases to model different functional roles in Neural Networks", 
             ha='center', fontsize=12, fontweight='bold')
    
    plt.tight_layout(rect=[0, 0.03, 1, 0.95])
    fig.savefig(save_path, dpi=300)
    plt.close(fig)

if __name__ == "__main__":
    run_all_case_tests() 