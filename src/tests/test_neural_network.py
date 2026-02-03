import os
import logging
import numpy as np
import pytest
import matplotlib.pyplot as plt
from matplotlib import animation
from unittest.mock import MagicMock
from typing import Dict, List, Tuple

from src.core.model import Case
from src.core.neural_network import NeuralNetworkModel
from src.utils.visualization import plot_case_linguistic_context
from src.tests.pomdp.visualizers import Visualizer
from sklearn.decomposition import PCA
from sklearn.model_selection import train_test_split

# Configure logging for tests
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Define a constant for the output directory
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
        # Avoid division by zero
        if model.loss_history[0] != 0:
            improvement = (1 - model.loss_history[-1]/model.loss_history[0])*100
            f.write(f"  Improvement: {improvement:.2f}%\n")
        else:
            f.write("  Improvement: N/A (initial loss was zero)\n")
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
        # Avoid division by zero
        if model.loss_history[0] != 0:
            improvement = (1 - model.loss_history[-1]/model.loss_history[0])*100
            f.write(f"* Improvement: {improvement:.2f}%\n\n")
        else:
            f.write("* Improvement: N/A (initial loss was zero)\n\n")
        f.write("### Visualizations\n\n")
        f.write(f"1. [Network Structure](network_structure.png)\n")
        f.write(f"2. [Training History](training_history.png)\n")
        f.write(f"3. [Predictions](predictions.png)\n")
        if X.shape[1] == 1 and y.shape[1] == 1:
            f.write(f"4. [Learning Animation](learning_animation.gif)\n")
    
    logger.info(f"Completed NOMINATIVE case test with visualizations in {case_dir}")

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
    
    # Handle cases where training is not allowed
    if hasattr(model, 'loss_history') and len(model.loss_history) > 0:
        Visualizer.plot_training_history(
            loss_history=model.loss_history,
            title=f"Training Loss in {Case.GENITIVE.value} Case",
            save_path=training_path
        )
    else:
        # Create a placeholder plot if no training occurred
        fig, ax = plt.subplots(figsize=(10, 6))
        ax.text(0.5, 0.5, f"Training not allowed in {Case.GENITIVE.value} case", 
               ha='center', va='center', fontsize=14)
        ax.set_title(f"Training Status in {Case.GENITIVE.value} Case")
        ax.axis('off')
        fig.savefig(training_path)
        plt.close(fig)
        
        # Create an empty loss history for reporting
        model.loss_history = [0.0]
    
    # Save final metrics
    metrics_file = os.path.join(case_dir, "metrics.txt")
    with open(metrics_file, "w") as f:
        f.write(f"Case: {Case.GENITIVE.value} - {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical Role: {case_info['statistical_role']}\n\n")
        f.write(f"Neural Network Context: {case_info['neural_network_context']}\n\n")
        f.write("Training Metrics:\n")
        f.write(f"  Final Loss: {model.loss_history[-1]:.6f}\n")
        f.write(f"  Initial Loss: {model.loss_history[0]:.6f}\n")
        # Avoid division by zero
        if model.loss_history[0] != 0:
            improvement = (1 - model.loss_history[-1]/model.loss_history[0])*100
            f.write(f"  Improvement: {improvement:.2f}%\n")
        else:
            f.write("  Improvement: N/A (initial loss was zero)\n")
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
    
    # Get activations safely
    outputs_and_activations = model._forward_with_activations(X)
    if isinstance(outputs_and_activations, tuple) and len(outputs_and_activations) > 1:
        # The function returns (output, activations)
        _, activations = outputs_and_activations
        if len(activations) > 0:
            final_activations = activations[-1]  # Last layer activations
        else:
            # No activations returned, create dummy data
            final_activations = np.zeros((1, 1))
    else:
        # Function didn't return expected tuple, create dummy data
        final_activations = np.zeros((1, 1))
    
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
        
        # Handle cases where training might not have been allowed
        if hasattr(model, 'loss_history') and len(model.loss_history) > 0 and model.loss_history[0] != 0:
            f.write(f"* Initial loss: {model.loss_history[0]:.6f}\n")
            f.write(f"* Final loss: {model.loss_history[-1]:.6f}\n")
            improvement = (1 - model.loss_history[-1]/model.loss_history[0])*100
            f.write(f"* Improvement: {improvement:.2f}%\n\n")
        else:
            f.write("* Training not allowed in GENITIVE case\n\n")
        f.write("### Visualizations\n\n")
        f.write(f"1. [Network Structure](network_structure.png)\n")
        f.write(f"2. [Training History](training_history.png)\n")
        f.write(f"3. [Predictions](predictions.png)\n")
        f.write(f"4. [Output Distributions](output_distribution.png)\n")
        f.write(f"5. [Final Layer Activations](final_activations.png)\n")
    
    logger.info(f"Completed GENITIVE case test with visualizations in {case_dir}")

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
        hidden_dims=[16, 8],  # Increased hidden layer size for richer representation
        activation='relu'
    )
    model.case = Case.LOCATIVE  # Explicitly set to LOCATIVE case
    
    # Log model details
    logger.info(f"Created neural network model with architecture: input_dim={X.shape[1]}, hidden_dims=[16, 8], output_dim={y_one_hot.shape[1]}")
    
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
    
    # Handle cases where training is not allowed
    if hasattr(model, 'loss_history') and len(model.loss_history) > 0:
        Visualizer.plot_training_history(
            loss_history=model.loss_history,
            title=f"Training Loss in {Case.LOCATIVE.value} Case",
            save_path=training_path
        )
    else:
        # Create a placeholder plot if no training occurred
        fig, ax = plt.subplots(figsize=(10, 6))
        ax.text(0.5, 0.5, f"Training not allowed in {Case.LOCATIVE.value} case", 
               ha='center', va='center', fontsize=14)
        ax.set_title(f"Training Status in {Case.LOCATIVE.value} Case")
        ax.axis('off')
        fig.savefig(training_path)
        plt.close(fig)
        
        # Create an empty loss history for reporting
        model.loss_history = [0.0]
    
    # Save final metrics
    metrics_file = os.path.join(case_dir, "metrics.txt")
    with open(metrics_file, "w") as f:
        f.write(f"Case: {Case.LOCATIVE.value} - {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical Role: {case_info['statistical_role']}\n\n")
        f.write(f"Neural Network Context: {case_info['neural_network_context']}\n\n")
        
        # Check if loss history exists and has values
        if hasattr(model, 'loss_history') and len(model.loss_history) > 0:
            f.write("Training Metrics:\n")
            f.write(f"  Final Loss: {model.loss_history[-1]:.6f}\n")
            f.write(f"  Initial Loss: {model.loss_history[0]:.6f}\n")
            
            # Avoid division by zero
            if model.loss_history[0] != 0:
                improvement = (1 - model.loss_history[-1]/model.loss_history[0])*100
                f.write(f"  Improvement: {improvement:.2f}%\n")
            else:
                f.write("  Improvement: N/A (initial loss was zero)\n")
                
            f.write(f"  Epochs: {len(model.loss_history)}\n")
        else:
            f.write("Training Metrics: Not available (training not allowed in LOCATIVE case)\n")
    
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
    # Only add legend if there are labeled artists
    handles, labels = ax.get_legend_handles_labels()
    if handles:
        ax.legend()
    fig.tight_layout()
    fig.savefig(prediction_path)
    plt.close(fig)
    
    # Visualize hidden layer activations
    hidden_activations_path = os.path.join(case_dir, "hidden_activations.png")
    fig, ax = plt.subplots(figsize=(10, 6))
    
    # Get activations safely
    outputs_and_activations = model._forward_with_activations(X)
    
    if isinstance(outputs_and_activations, tuple) and len(outputs_and_activations) > 1:
        # The function returns (output, activations)
        _, all_activations = outputs_and_activations
        if len(all_activations) > 1:  # Check if we have at least one hidden layer
            hidden_activations = all_activations[1]  # First hidden layer
            
            if isinstance(hidden_activations, np.ndarray):
                if hidden_activations.ndim == 2 and hidden_activations.shape[1] == 1:
                    # For 1D activations
                    ax.hist(hidden_activations.flatten(), bins=20, alpha=0.7)
                    ax.set_xlabel('Activation')
                    ax.set_ylabel('Frequency')
                elif hidden_activations.ndim == 2 and hidden_activations.shape[1] > 1:
                    # For higher dimensional data, use PCA for dimensionality reduction
                    pca = PCA(n_components=2)
                    hidden_activations_2d = pca.fit_transform(hidden_activations)
                    
                    # Plot PCA components
                    scatter = ax.scatter(hidden_activations_2d[:, 0], hidden_activations_2d[:, 1], 
                                      c=y if y.ndim == 1 else np.argmax(y, axis=1), 
                                      cmap='viridis', alpha=0.7)
                    ax.set_xlabel('Principal Component 1')
                    ax.set_ylabel('Principal Component 2')
                    plt.colorbar(scatter, ax=ax, label='Class')
                else:
                    # Handle unexpected dimensions
                    ax.text(0.5, 0.5, "Cannot visualize activations\n(unexpected dimensions)", 
                          ha='center', va='center', fontsize=12, transform=ax.transAxes)
            else:
                # Not a numpy array
                ax.text(0.5, 0.5, "Cannot visualize activations\n(not a numpy array)", 
                      ha='center', va='center', fontsize=12, transform=ax.transAxes)
        else:
            # No hidden layers
            ax.text(0.5, 0.5, "No hidden layer activations available", 
                  ha='center', va='center', fontsize=12, transform=ax.transAxes)
    else:
        # Unexpected return format
        ax.text(0.5, 0.5, "Cannot visualize activations\n(unexpected return format)", 
              ha='center', va='center', fontsize=12, transform=ax.transAxes)
    
    ax.set_title(f"Hidden Layer Activations in {Case.LOCATIVE.value} Case")
    ax.grid(True, linestyle='--', alpha=0.6)
    fig.tight_layout()
    fig.savefig(hidden_activations_path)
    plt.close(fig)
    
    # Visualize activation patterns for specific samples
    sample_activations_path = os.path.join(case_dir, "sample_activations.png")
    fig, axs = plt.subplots(2, 2, figsize=(12, 10))
    
    # Ensure there are enough samples
    num_samples = min(4, len(X))
    sample_indices = list(range(num_samples))
    
    # Convert to 2D array if needed
    axs_flat = axs.flatten() if hasattr(axs, 'flatten') else [axs]
    
    for i, idx in enumerate(sample_indices):
        if i < len(axs_flat):  # Make sure we don't go out of bounds
            # Get sample activations safely
            sample_X = X[idx:idx+1]
            outputs_and_activations = model._forward_with_activations(sample_X)
            
            if isinstance(outputs_and_activations, tuple) and len(outputs_and_activations) > 1:
                # The function returns (output, activations)
                _, all_activations = outputs_and_activations
                
                if isinstance(all_activations, list) and all_activations:
                    # Plot each layer's activations as a line
                    for j, layer_act in enumerate(all_activations):
                        if isinstance(layer_act, np.ndarray) and layer_act.ndim > 0:
                            # For 2D activations, get the first sample
                            act_to_plot = layer_act[0] if layer_act.ndim > 1 else layer_act
                            if len(act_to_plot) > 0:  # Make sure there's data to plot
                                axs_flat[i].plot(act_to_plot, 'o-', label=f'Layer {j+1}')
                    
                    # Get class label for title
                    class_label = y[idx] if y.ndim == 1 else np.argmax(y[idx])
                    axs_flat[i].set_title(f"Sample {idx+1} (Class {class_label})")
                    axs_flat[i].legend()
                    axs_flat[i].grid(True, linestyle='--', alpha=0.3)
                else:
                    axs_flat[i].text(0.5, 0.5, "No activations\navailable", 
                                  ha='center', va='center', fontsize=12, transform=axs_flat[i].transAxes)
                    axs_flat[i].set_title(f"Sample {idx+1}")
            else:
                axs_flat[i].text(0.5, 0.5, "Invalid return format", 
                              ha='center', va='center', fontsize=12, transform=axs_flat[i].transAxes)
                axs_flat[i].set_title(f"Sample {idx+1}")
    
    # Handle any remaining subplots
    for i in range(len(sample_indices), len(axs_flat)):
        axs_flat[i].axis('off')
    
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
        f.write(f"* Model architecture: Input({X.shape[1]}) → Hidden(16) → Hidden(8) → Output({y_one_hot.shape[1]})\n")
        f.write(f"* Activation function: {model.activation}\n")
        
        # Handle cases where training might not have been allowed
        if hasattr(model, 'loss_history') and len(model.loss_history) > 0 and model.loss_history[0] != 0:
            f.write(f"* Initial loss: {model.loss_history[0]:.6f}\n")
            f.write(f"* Final loss: {model.loss_history[-1]:.6f}\n")
            improvement = (1 - model.loss_history[-1]/model.loss_history[0])*100
            f.write(f"* Improvement: {improvement:.2f}%\n\n")
        else:
            f.write("* Training not allowed in LOCATIVE case\n\n")
        f.write("### Visualizations\n\n")
        f.write(f"1. [Network Structure](network_structure.png)\n")
        f.write(f"2. [Training History](training_history.png)\n")
        f.write(f"3. [Predictions](predictions.png)\n")
        f.write(f"4. [Hidden Layer Activations](hidden_activations.png)\n")
        f.write(f"5. [Activation Patterns for Specific Samples](sample_activations.png)\n")
    
    logger.info(f"Completed LOCATIVE case test with visualizations in {case_dir}")

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
    
    # Handle cases where training is not allowed
    if hasattr(model, 'loss_history') and len(model.loss_history) > 0:
        Visualizer.plot_training_history(
            loss_history=model.loss_history,
            title=f"Training Loss in {Case.ABLATIVE.value} Case",
            save_path=training_path
        )
    else:
        # Create a placeholder plot if no training occurred
        fig, ax = plt.subplots(figsize=(10, 6))
        ax.text(0.5, 0.5, f"Training not allowed in {Case.ABLATIVE.value} case", 
               ha='center', va='center', fontsize=14)
        ax.set_title(f"Training Status in {Case.ABLATIVE.value} Case")
        ax.axis('off')
        fig.savefig(training_path)
        plt.close(fig)
        
        # Create an empty loss history for reporting
        model.loss_history = [0.0]
    
    # Save final metrics
    metrics_file = os.path.join(case_dir, "metrics.txt")
    with open(metrics_file, "w") as f:
        f.write(f"Case: {Case.ABLATIVE.value} - {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical Role: {case_info['statistical_role']}\n\n")
        f.write(f"Neural Network Context: {case_info['neural_network_context']}\n\n")
        
        # Check if loss history exists and has values
        if hasattr(model, 'loss_history') and len(model.loss_history) > 0:
            f.write("Training Metrics:\n")
            f.write(f"  Final Loss: {model.loss_history[-1]:.6f}\n")
            f.write(f"  Initial Loss: {model.loss_history[0]:.6f}\n")
            
            # Avoid division by zero
            if model.loss_history[0] != 0:
                improvement = (1 - model.loss_history[-1]/model.loss_history[0])*100
                f.write(f"  Improvement: {improvement:.2f}%\n")
            else:
                f.write("  Improvement: N/A (initial loss was zero)\n")
                
            f.write(f"  Epochs: {len(model.loss_history)}\n")
        else:
            f.write("Training Metrics: Not available (training not allowed in ABLATIVE case)\n")
    
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
    
    # Get predictions first (needed for _backward_with_metrics)
    y_pred = model.predict(X)
    
    try:
        # Use _backward_with_metrics to compute gradients
        gradients = model._backward_with_metrics(X, y, y_pred)[1]
        
        # Visualize gradients for each layer
        for i, layer_gradients in enumerate(gradients):
            layer_magnitudes = np.linalg.norm(layer_gradients, axis=1)
            ax.hist(layer_magnitudes, bins=20, alpha=0.7, label=f'Layer {i+1}')
        
        ax.set_xlabel('Gradient Magnitude')
        ax.set_ylabel('Frequency')
        ax.set_title(f"Gradient Magnitudes per Layer in {Case.ABLATIVE.value} Case")
        ax.grid(True, linestyle='--', alpha=0.6)
        ax.legend()
    except Exception as e:
        # Handle the case where backward pass isn't available
        ax.text(0.5, 0.5, "Gradient computation\nnot available", 
              ha='center', va='center', fontsize=12, transform=ax.transAxes)
        ax.set_title(f"Gradient Magnitudes in {Case.ABLATIVE.value} Case")
        ax.axis('off')
        logger.warning(f"Could not compute gradients: {e}")
    
    # Save and close figure regardless of success or failure
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
        
        # Handle cases where training might not have been allowed
        if hasattr(model, 'loss_history') and len(model.loss_history) > 0 and model.loss_history[0] != 0:
            f.write(f"* Initial loss: {model.loss_history[0]:.6f}\n")
            f.write(f"* Final loss: {model.loss_history[-1]:.6f}\n")
            improvement = (1 - model.loss_history[-1]/model.loss_history[0])*100
            f.write(f"* Improvement: {improvement:.2f}%\n\n")
        else:
            f.write("* Training not allowed in ABLATIVE case\n\n")
        f.write("### Visualizations\n\n")
        f.write(f"1. [Network Structure](network_structure.png)\n")
        f.write(f"2. [Training History](training_history.png)\n")
        f.write(f"3. [Predictions](predictions.png)\n")
        f.write(f"4. [Error Distribution](error_distribution.png)\n")
        f.write(f"5. [Gradient Magnitudes per Layer](gradient_magnitudes.png)\n")
    
    logger.info(f"Completed ABLATIVE case test with visualizations in {case_dir}")

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
        hidden_dims=[10],
        activation='relu'
    )
    model.case = Case.VOCATIVE  # Explicitly set to VOCATIVE case
    
    # Log model details
    logger.info(f"Created neural network model with architecture: input_dim={X.shape[1]}, hidden_dims=[10], output_dim={y_one_hot.shape[1]}")
    
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
    
    # Handle cases where training is not allowed
    if hasattr(model, 'loss_history') and len(model.loss_history) > 0:
        Visualizer.plot_training_history(
            loss_history=model.loss_history,
            title=f"Training Loss in {Case.VOCATIVE.value} Case",
            save_path=training_path
        )
    else:
        # Create a placeholder plot if no training occurred
        fig, ax = plt.subplots(figsize=(10, 6))
        ax.text(0.5, 0.5, f"Training not allowed in {Case.VOCATIVE.value} case", 
               ha='center', va='center', fontsize=14)
        ax.set_title(f"Training Status in {Case.VOCATIVE.value} Case")
        ax.axis('off')
        fig.savefig(training_path)
        plt.close(fig)
        
        # Create an empty loss history for reporting
        model.loss_history = [0.0]
    
    # Save final metrics
    metrics_file = os.path.join(case_dir, "metrics.txt")
    with open(metrics_file, "w") as f:
        f.write(f"Case: {Case.VOCATIVE.value} - {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical Role: {case_info['statistical_role']}\n\n")
        f.write(f"Neural Network Context: {case_info['neural_network_context']}\n\n")
        
        # Check if loss history exists and has values
        if hasattr(model, 'loss_history') and len(model.loss_history) > 0:
            f.write("Training Metrics:\n")
            f.write(f"  Final Loss: {model.loss_history[-1]:.6f}\n")
            f.write(f"  Initial Loss: {model.loss_history[0]:.6f}\n")
            
            # Avoid division by zero
            if model.loss_history[0] != 0:
                improvement = (1 - model.loss_history[-1]/model.loss_history[0])*100
                f.write(f"  Improvement: {improvement:.2f}%\n")
            else:
                f.write("  Improvement: N/A (initial loss was zero)\n")
                
            f.write(f"  Epochs: {len(model.loss_history)}\n")
        else:
            f.write("Training Metrics: Not available (training not allowed in VOCATIVE case)\n")
    
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
    # Only add legend if there are labeled artists
    handles, labels = ax.get_legend_handles_labels()
    if handles:
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
        y_segment = y_one_hot[:segment_end]
        
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
    scatter = axs[1].scatter([], [])
    ideal_line = axs[1].plot([], [], 'r--', label='Ideal')[0]
    
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
        X_current = X[:current_segment_end]
        y_current = y_one_hot[:current_segment_end]
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
        
        # Handle case where evaluation_results might not be defined or empty
        if 'evaluation_results' in locals() and evaluation_results:
            for metric, value in evaluation_results.items():
                f.write(f"  - {metric}: {value:.6f}\n")
        else:
            f.write("  - Evaluation not performed (not allowed in VOCATIVE case)\n")
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

def test_dative_case(nn_regression_data, case_definitions):
    """Test for DATIVE case: Model as recipient of inputs."""
    # Get case info for logging
    case_info = case_definitions[Case.DATIVE]
    logger.info(f"Testing {Case.DATIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(OUTPUT_DIR, Case.DATIVE.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.DATIVE, linguistics_path)
    
    # Unpack the data
    X, y = nn_regression_data
    
    # Ensure X is 2D for consistency
    if len(X.shape) == 1:
        X = X.reshape(-1, 1)
    if len(y.shape) == 1:
        y = y.reshape(-1, 1)
    
    # Create a simple NN model in DATIVE case
    model = NeuralNetworkModel(
        name="DatModel",
        input_dim=X.shape[1],
        output_dim=y.shape[1],
        hidden_dims=[10, 5],
        activation='relu'
    )
    model.case = Case.DATIVE  # Explicitly set to DATIVE case
    
    # Log model details
    logger.info(f"Created neural network model with architecture: input_dim={X.shape[1]}, hidden_dims=[10, 5], output_dim={y.shape[1]}")
    
    # Visualize the model structure 
    network_structure_path = os.path.join(case_dir, "network_structure.png")
    Visualizer.plot_network_structure(
        model=model,
        title=f"Neural Network Structure in {Case.DATIVE.value} Case",
        save_path=network_structure_path
    )
    
    # Generate different input distributions to visualize input processing
    logger.info("Generating various input distributions to test DATIVE case processing")
    
    # Create all input distributions with the same shape
    input_dim = X.shape[1]  # Get actual input dimension
    sample_size = X.shape[0]  # Get number of samples
    
    # 1. Original data (already properly shaped)
    
    # 2. Normal distribution
    np.random.seed(42)
    X_normal = np.random.normal(0, 1, size=(sample_size, input_dim))
    
    # 3. Uniform distribution
    X_uniform = np.random.uniform(-2, 2, size=(sample_size, input_dim))
    
    # 4. Exponential distribution
    X_exp = np.random.exponential(1, size=(sample_size, input_dim))
    
    # 5. Binomial distribution
    X_binomial = np.random.binomial(10, 0.5, size=(sample_size, input_dim)) / 10
    
    # Store input sets and names
    input_sets = [X, X_normal, X_uniform, X_exp, X_binomial]
    input_names = ["Original", "Normal", "Uniform", "Exponential", "Binomial"]
    
    # Visualize input distributions
    input_distribution_path = os.path.join(case_dir, "input_distribution.png")
    fig, axs = plt.subplots(len(input_sets), 1, figsize=(10, 12))
    
    # Make sure axs is always a list/array for iteration
    if len(input_sets) == 1:
        axs = [axs]
    
    for i, (X_input, name) in enumerate(zip(input_sets, input_names)):
        # Ensure X_input is properly shaped for plotting
        if len(X_input.shape) == 1:
            X_input = X_input.reshape(-1, 1)
            
        # Check dimensions for plotting
        if X_input.shape[1] == 1:
            # For 1D data, plot histogram
            axs[i].hist(X_input.flatten(), bins=20, alpha=0.7)
            axs[i].set_title(f"{name} Input Distribution")
            axs[i].set_xlabel("Input Value")
            axs[i].set_ylabel("Frequency")
        else:
            # For multi-dimensional input, plot first two dimensions
            if X_input.shape[1] >= 2:
                axs[i].scatter(X_input[:, 0], X_input[:, 1], alpha=0.7)
            else:
                # For 1D data that needs to be plotted in 2D
                x_indices = np.arange(len(X_input))
                axs[i].scatter(x_indices, X_input[:, 0], alpha=0.7)
            axs[i].set_title(f"{name} Input Distribution")
            axs[i].set_xlabel("Dimension 1")
            axs[i].set_ylabel("Dimension 2")
    
    plt.tight_layout()
    fig.savefig(input_distribution_path)
    plt.close(fig)
    
    # Process inputs through model and visualize activations
    input_activations_path = os.path.join(case_dir, "input_activations.png")
    fig, axs = plt.subplots(len(input_sets), 1, figsize=(12, 15))
    
    # Handle single axes case
    if len(input_sets) == 1:
        axs = [axs]
    
    for i, (X_input, name) in enumerate(zip(input_sets, input_names)):
        # Ensure X_input is properly shaped for processing
        if len(X_input.shape) == 1:
            X_input = X_input.reshape(-1, 1)
            
        # Get activations from first layer (input processing)
        _, activations = model._forward_with_activations(X_input)
        first_layer_activations = activations[1]  # First hidden layer after input
        
        # Plot histogram of activations
        if first_layer_activations.shape[1] <= 5:  # If few neurons, plot individually
            for j in range(first_layer_activations.shape[1]):
                axs[i].hist(first_layer_activations[:, j], bins=20, alpha=0.7, 
                          label=f"Neuron {j+1}")
            axs[i].legend()
        else:  # Otherwise plot distribution of all activations
            axs[i].hist(first_layer_activations.flatten(), bins=20, alpha=0.7)
        
        axs[i].set_title(f"{name} Input: First Layer Activations")
        axs[i].set_xlabel("Activation Value")
        axs[i].set_ylabel("Frequency")
    
    plt.tight_layout()
    fig.savefig(input_activations_path)
    plt.close(fig)
    
    # Calculate input sensitivity
    input_sensitivity_path = os.path.join(case_dir, "input_sensitivity.png")
    fig, ax = plt.subplots(figsize=(10, 6))
    
    # Perturb each input dimension and measure output change
    sensitivities = []
    
    # Ensure X is properly shaped
    if len(X.shape) == 1:
        X = X.reshape(-1, 1)
        
    # Check input dimensions
    input_dim = X.shape[1]
    
    if input_dim == 1:
        # For 1D input, sample points across range
        test_points = np.linspace(X.min(), X.max(), 20).reshape(-1, 1)
        delta = (X.max() - X.min()) * 0.01
        
        for x in test_points:
            # Make sure x is 2D
            if len(x.shape) == 1:
                x = x.reshape(-1, 1)
                
            x_plus = x.copy()
            
            # Handle 1D or 2D array properly
            if len(x_plus.shape) == 1:
                x_plus[0] += delta  # 1D array
            else:
                x_plus[0, 0] += delta  # 2D array
                
            y_base = model.predict(x)
            y_perturbed = model.predict(x_plus)
            
            # Safely extract scalar values for 1D and 2D arrays
            if len(y_base.shape) == 1:
                base_val = y_base[0]
            else:
                base_val = y_base[0, 0]
                
            if len(y_perturbed.shape) == 1:
                perturbed_val = y_perturbed[0]
            else:
                perturbed_val = y_perturbed[0, 0]
                
            sensitivity = np.abs((perturbed_val - base_val) / delta)
            
            # Extract the input value safely
            if len(x.shape) == 1:
                x_val = x[0]
            else:
                x_val = x[0, 0]
                
            sensitivities.append((x_val, sensitivity))
        
        # Plot sensitivity
        x_vals, s_vals = zip(*sensitivities)
        ax.plot(x_vals, s_vals, 'b-', linewidth=2)
        ax.set_xlabel("Input Value")
        ax.set_ylabel("Output Sensitivity")
        
    else:
        # For multi-dimensional input, measure average sensitivity per dimension
        dim_sensitivities = []
        delta = 0.01
        
        for dim in range(input_dim):
            # Sample points for this dimension
            x_base = X.mean(axis=0).reshape(1, -1)  # Use mean as base point
            x_perturbed = x_base.copy()
            x_perturbed[0, dim] += delta
            
            y_base = model.predict(x_base)
            y_perturbed = model.predict(x_perturbed)
            
            sensitivity = np.abs((y_perturbed - y_base) / delta).mean()
            dim_sensitivities.append(sensitivity)
        
        # Plot sensitivity per dimension
        ax.bar(range(len(dim_sensitivities)), dim_sensitivities)
        ax.set_xlabel("Input Dimension")
        ax.set_ylabel("Average Output Sensitivity")
        ax.set_xticks(range(len(dim_sensitivities)))
    
    ax.set_title(f"Input Sensitivity in {Case.DATIVE.value} Case")
    ax.grid(True, linestyle='--', alpha=0.6)
    fig.tight_layout()
    fig.savefig(input_sensitivity_path)
    plt.close(fig)
    
    # Create an animation showing input processing
    animation_path = os.path.join(case_dir, "input_processing_animation.gif")
    
    # We'll create this for different input distributions
    fig, axs = plt.subplots(2, 1, figsize=(10, 12))
    
    # Setup for input distribution
    hist_obj = None
    scatter = None
    
    # Check input dimensions for visualization type
    input_dim = X.shape[1]
    
    if input_dim == 1:
        # For 1D input
        hist_obj = axs[0].hist([], bins=20, alpha=0.7)
        
        # Safely get min/max values
        input_mins = []
        input_maxs = []
        for x_set in input_sets:
            if len(x_set.shape) == 1:
                x_flat = x_set
            else:
                x_flat = x_set.flatten()
            input_mins.append(x_flat.min())
            input_maxs.append(x_flat.max())
            
        axs[0].set_xlim(min(input_mins) - 0.5, max(input_maxs) + 0.5)
        
        # Safely get histogram counts
        hist_maxes = []
        for x_set in input_sets:
            if len(x_set.shape) == 1:
                x_flat = x_set
            else:
                x_flat = x_set.flatten()
            counts, _ = np.histogram(x_flat, bins=20)
            hist_maxes.append(counts.max())
        
        axs[0].set_ylim(0, max(hist_maxes) * 1.1)
        axs[0].set_title("Input Distribution")
        axs[0].set_xlabel("Input Value")
        axs[0].set_ylabel("Frequency")
    else:
        # For 2D input
        scatter = axs[0].scatter([], [])
        
        # Safely get x and y limits
        x_mins = []
        x_maxs = []
        y_mins = []
        y_maxs = []
        
        for x_set in input_sets:
            # Ensure proper shape
            if len(x_set.shape) == 1:
                x_set = x_set.reshape(-1, 1)
                
            if x_set.shape[1] >= 2:
                # True 2D case
                x_mins.append(x_set[:, 0].min())
                x_maxs.append(x_set[:, 0].max())
                y_mins.append(x_set[:, 1].min())
                y_maxs.append(x_set[:, 1].max())
            else:
                # 1D case handling
                indices = np.arange(len(x_set))
                x_mins.append(indices.min())
                x_maxs.append(indices.max())
                y_mins.append(x_set.flatten().min())
                y_maxs.append(x_set.flatten().max())
        
        axs[0].set_xlim(min(x_mins) - 0.5, max(x_maxs) + 0.5)
        axs[0].set_ylim(min(y_mins) - 0.5, max(y_maxs) + 0.5)
        axs[0].set_title("Input Distribution")
        axs[0].set_xlabel("Dimension 1")
        axs[0].set_ylabel("Dimension 2")
    
    # Setup for activation histogram
    activation_hist = axs[1].hist([], bins=20, alpha=0.7)
    
    # Get activation limits across all inputs
    activation_mins = []
    activation_maxs = []
    act_hist_maxes = []
    
    for x_set in input_sets:
        # Ensure proper shape
        if len(x_set.shape) == 1:
            x_set = x_set.reshape(-1, 1)
            
        _, activations = model._forward_with_activations(x_set)
        first_layer_act = activations[1].flatten()
        activation_mins.append(first_layer_act.min())
        activation_maxs.append(first_layer_act.max())
        
        counts, _ = np.histogram(first_layer_act, bins=20)
        act_hist_maxes.append(counts.max())
    
    # Set limits for activation histogram
    axs[1].set_xlim(min(activation_mins) - 0.1, max(activation_maxs) + 0.1)
    axs[1].set_ylim(0, max(act_hist_maxes) * 1.1)
    axs[1].set_title("First Layer Activations")
    axs[1].set_xlabel("Activation Value")
    axs[1].set_ylabel("Frequency")
    
    # Text for distribution information
    dist_text = axs[0].text(0.02, 0.95, '', transform=axs[0].transAxes)
    
    def init():
        if input_dim == 1 and hist_obj is not None:
            # For 1D data
            for rect in hist_obj[2]:
                rect.set_height(0)
        elif scatter is not None:
            # For 2D data
            scatter.set_offsets(np.empty((0, 2)))
        
        # Clear activation histogram
        for patch in activation_hist[2]:
            patch.set_height(0)
        
        dist_text.set_text('')
        
        if input_dim == 1 and hist_obj is not None:
            return list(hist_obj[2]) + list(activation_hist[2]) + [dist_text]
        elif scatter is not None:
            return [scatter] + list(activation_hist[2]) + [dist_text]
        else:
            return list(activation_hist[2]) + [dist_text]
    
    def update(frame):
        # Get current input set
        idx = frame % len(input_sets)
        X_current = input_sets[idx]
        name_current = input_names[idx]
        
        # Ensure proper shape
        if len(X_current.shape) == 1:
            X_current = X_current.reshape(-1, 1)
        
        # Update input distribution
        return_elements = []
        
        if input_dim == 1 and hist_obj is not None:
            # For 1D data, update histogram
            counts, _ = np.histogram(X_current.flatten(), bins=20)
            for count, rect in zip(counts, hist_obj[2]):
                rect.set_height(count)
            return_elements.extend(list(hist_obj[2]))
        elif scatter is not None:
            # For 2D data, update scatter
            if X_current.shape[1] >= 2:
                # True 2D data
                scatter.set_offsets(X_current[:, :2])
            else:
                # 1D data that should be plotted as 2D
                indices = np.arange(len(X_current))
                values = X_current.flatten()
                scatter.set_offsets(np.column_stack((indices, values)))
            return_elements.append(scatter)
        
        # Update activation distribution
        _, activations = model._forward_with_activations(X_current)
        first_layer_activations = activations[1].flatten()
        counts, _ = np.histogram(first_layer_activations, bins=20)
        for count, rect in zip(counts, activation_hist[2]):
            rect.set_height(count)
        
        dist_text.set_text(f'Distribution: {name_current}')
        
        return_elements.extend(list(activation_hist[2]))
        return_elements.append(dist_text)
        
        return return_elements
    
    anim = animation.FuncAnimation(fig, update, frames=len(input_sets)*3,
                          init_func=init, blit=True)
    
    # Save animation
    anim.save(animation_path, writer='pillow', fps=1)
    plt.close(fig)
    logger.info(f"Created input processing animation: {animation_path}")
    
    # Generate a report
    report_path = os.path.join(case_dir, "report.md")
    with open(report_path, "w") as f:
        f.write(f"# Neural Network in {Case.DATIVE.value} Case\n\n")
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
        f.write("* Input processing characteristics:\n")
        f.write("  - Explored various input distributions to test model's input processing ability\n")
        f.write("  - First layer neurons show different activation patterns for different input distributions\n")
        f.write("  - Input sensitivity varies across the input domain, showing higher sensitivity in certain regions\n\n")
        f.write("### Visualizations\n\n")
        f.write(f"1. [Network Structure](network_structure.png)\n")
        f.write(f"2. [Input Distributions](input_distribution.png)\n")
        f.write(f"3. [Input Activations](input_activations.png)\n")
        f.write(f"4. [Input Sensitivity](input_sensitivity.png)\n")
        f.write(f"5. [Input Processing Animation](input_processing_animation.gif)\n")
    
    logger.info(f"Completed DATIVE case test with visualizations in {case_dir}")

def test_instrumental_case(nn_regression_data, case_definitions):
    """Test for INSTRUMENTAL case: Model as method/tool for computation."""
    # Get case info for logging
    case_info = case_definitions[Case.INSTRUMENTAL]
    logger.info(f"Testing {Case.INSTRUMENTAL.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(OUTPUT_DIR, Case.INSTRUMENTAL.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.INSTRUMENTAL, linguistics_path)
    
    # Unpack the data
    X, y = nn_regression_data
    
    # Create a NN model in INSTRUMENTAL case - focusing on it as a computational tool/method
    model = NeuralNetworkModel(
        name="InsModel",
        input_dim=X.shape[1],
        output_dim=y.shape[1],
        hidden_dims=[12, 8, 4],  # More complex architecture to showcase computational ability
        activation='tanh'  # Different activation to highlight versatility
    )
    model.case = Case.INSTRUMENTAL  # Explicitly set to INSTRUMENTAL case
    
    # Log model details
    logger.info(f"Created neural network model with architecture: input_dim={X.shape[1]}, hidden_dims=[12, 8, 4], output_dim={y.shape[1]}")
    
    # Visualize the model structure 
    network_structure_path = os.path.join(case_dir, "network_structure.png")
    Visualizer.plot_network_structure(
        model=model,
        title=f"Neural Network Structure in {Case.INSTRUMENTAL.value} Case",
        save_path=network_structure_path
    )
    
    # Train the model
    logger.info("Training model in INSTRUMENTAL case (model as computational method)")
    train_results = model.train(X, y, epochs=150, learning_rate=0.01, batch_size=32)
    
    # Visualize training progress
    training_path = os.path.join(case_dir, "training_history.png")
    Visualizer.plot_training_history(
        loss_history=model.loss_history,
        title=f"Training Loss in {Case.INSTRUMENTAL.value} Case",
        save_path=training_path
    )
    
    # In INSTRUMENTAL case, we're interested in the computational transformations
    # Let's visualize how data flows through the network
    
    # Create a visualization of data flow through layers
    data_flow_path = os.path.join(case_dir, "data_flow.png")
    
    # Select a small subset of data for visualization clarity
    np.random.seed(42)
    sample_indices = np.random.choice(len(X), min(10, len(X)), replace=False)
    X_sample = X[sample_indices]
    y_sample = y[sample_indices]
    
    # Get activations throughout the network
    _, all_activations = model._forward_with_activations(X_sample)
    
    # Create visualization of data flow (layer-by-layer transformations)
    fig, axs = plt.subplots(len(all_activations), 1, figsize=(12, 4 * len(all_activations)))
    
    for i, layer_activations in enumerate(all_activations):
        if i == 0:
            layer_name = "Input Layer"
        elif i == len(all_activations) - 1:
            layer_name = "Output Layer"
        else:
            layer_name = f"Hidden Layer {i}"
        
        # For each sample, show activation pattern
        for j, sample_activation in enumerate(layer_activations):
            if sample_activation.size > 20:  # If too many neurons, plot summary
                axs[i].plot([j], [np.mean(sample_activation)], 'o', markersize=8, 
                          label=f'Sample {j+1} Mean' if j == 0 else "_nolegend_")
                axs[i].errorbar([j], [np.mean(sample_activation)], 
                              yerr=[np.std(sample_activation)], capsize=5,
                              label=f'Sample {j+1} Std' if j == 0 else "_nolegend_")
            else:
                axs[i].plot(sample_activation, 'o-', alpha=0.7, 
                          label=f'Sample {j+1}' if j == 0 else "_nolegend_")
        
        axs[i].set_title(f"{layer_name} Activations")
        if sample_activation.size <= 20:
            axs[i].set_xticks(range(sample_activation.size))
            axs[i].set_xlabel("Neuron Index")
        else:
            axs[i].set_xlabel("Sample Index")
        axs[i].set_ylabel("Activation")
        axs[i].grid(True, linestyle='--', alpha=0.6)
        
        if i == 0:
            axs[i].legend()
    
    plt.tight_layout()
    fig.savefig(data_flow_path)
    plt.close(fig)
    
    # Now let's visualize the computational operations (weights) of the network
    weights_path = os.path.join(case_dir, "weight_visualization.png")
    fig, axs = plt.subplots(len(model.weights), 1, figsize=(12, 5 * len(model.weights)))
    
    if len(model.weights) == 1:  # Handle single layer case
        axs = [axs]
    
    for i, w in enumerate(model.weights):
        if i == 0:
            layer_name = "Input→Hidden1"
        elif i == len(model.weights) - 1:
            layer_name = f"Hidden{i}→Output"
        else:
            layer_name = f"Hidden{i}→Hidden{i+1}"
        
        # Plot weight matrix
        if w.size > 100:  # For large matrices, use heatmap
            im = axs[i].imshow(w, cmap='viridis', aspect='auto')
            axs[i].set_title(f"{layer_name} Weight Matrix ({w.shape[0]}×{w.shape[1]})")
            axs[i].set_xlabel("Target Neuron")
            axs[i].set_ylabel("Source Neuron")
            plt.colorbar(im, ax=axs[i], label="Weight Value")
        else:  # For small matrices, plot each value
            # Create a coordinate grid (use mesh_x/mesh_y to avoid shadowing X, y)
            mesh_x, mesh_y = np.meshgrid(range(w.shape[1]), range(w.shape[0]))
            # Flatten for scatter plot
            mesh_x = mesh_x.flatten()
            mesh_y = mesh_y.flatten()
            w_flat = w.flatten()
            
            # Use scatter with size proportional to absolute weight
            scatter = axs[i].scatter(mesh_x, mesh_y, s=np.abs(w_flat)*100 + 20, c=w_flat, cmap='coolwarm', 
                                  alpha=0.7, edgecolors='k')
            axs[i].set_title(f"{layer_name} Weight Matrix ({w.shape[0]}×{w.shape[1]})")
            axs[i].set_xticks(range(w.shape[1]))
            axs[i].set_yticks(range(w.shape[0]))
            axs[i].set_xlabel("Target Neuron")
            axs[i].set_ylabel("Source Neuron")
            axs[i].grid(True, linestyle='--', alpha=0.3)
            plt.colorbar(scatter, ax=axs[i], label="Weight Value")
    
    plt.tight_layout()
    fig.savefig(weights_path)
    plt.close(fig)
    
    # Visualize the methods of computation (activation functions and gradients)
    methods_path = os.path.join(case_dir, "computation_methods.png")
    fig, axs = plt.subplots(2, 2, figsize=(14, 10))
    
    # 1. Activation Function
    # Use act_x, act_y to avoid shadowing the test data variables X, y
    act_x = np.linspace(-5, 5, 100)
    if model.activation == 'tanh':
        act_y = np.tanh(act_x)
        act_y_deriv = 1 - np.tanh(act_x)**2
    elif model.activation == 'relu':
        act_y = np.maximum(0, act_x)
        act_y_deriv = np.where(act_x > 0, 1, 0)
    elif model.activation == 'sigmoid':
        act_y = 1 / (1 + np.exp(-act_x))
        act_y_deriv = act_y * (1 - act_y)
    else:
        act_y = act_x  # Linear
        act_y_deriv = np.ones_like(act_x)
    
    axs[0, 0].plot(act_x, act_y, 'b-', linewidth=2)
    axs[0, 0].set_title(f"{model.activation.capitalize()} Activation Function")
    axs[0, 0].set_xlabel("Input")
    axs[0, 0].set_ylabel("Output")
    axs[0, 0].grid(True, linestyle='--', alpha=0.6)
    
    axs[0, 1].plot(act_x, act_y_deriv, 'r-', linewidth=2)
    axs[0, 1].set_title(f"{model.activation.capitalize()} Activation Derivative")
    axs[0, 1].set_xlabel("Input")
    axs[0, 1].set_ylabel("Derivative")
    axs[0, 1].grid(True, linestyle='--', alpha=0.6)
    
    # 2. Learning Curve with Different Learning Rates
    learning_rates = [0.001, 0.01, 0.1]
    for lr in learning_rates:
        temp_model = NeuralNetworkModel(
            name=f"Temp_LR{lr}",
            input_dim=X.shape[1],
            output_dim=y.shape[1] if y.ndim > 1 else 1,
            hidden_dims=[4],  # Smaller for quicker training
            activation=model.activation
        )
        temp_model.train(X, y, epochs=50, learning_rate=lr)
        axs[1, 0].plot(temp_model.loss_history, label=f'LR = {lr}')
    
    axs[1, 0].set_title("Learning Curves for Different Learning Rates")
    axs[1, 0].set_xlabel("Epoch")
    axs[1, 0].set_ylabel("Loss")
    axs[1, 0].grid(True, linestyle='--', alpha=0.6)
    axs[1, 0].legend()
    
    # 3. Model Complexity Comparison (parameter count vs error)
    hidden_configs = [[2], [4], [8], [4, 2], [8, 4], [16, 8]]
    param_counts = []
    test_errors = []
    
    # Split data for testing
    np.random.seed(42)
    indices = np.random.permutation(len(X))
    test_size = min(50, len(X) // 5)
    train_indices = indices[test_size:]
    test_indices = indices[:test_size]

    X_train, y_train = X[train_indices], y[train_indices]
    X_test, y_test = X[test_indices], y[test_indices]
    
    for hidden_dims in hidden_configs:
        # Create model
        temp_model = NeuralNetworkModel(
            name=f"Temp_{'_'.join(map(str, hidden_dims))}",
            input_dim=X.shape[1],
            output_dim=y.shape[1],
            hidden_dims=hidden_dims,
            activation=model.activation
        )
        
        # Calculate parameter count
        param_count = sum(w.size for w in temp_model.weights) + sum(b.size for b in temp_model.biases)
        param_counts.append(param_count)
        
        # Train model
        temp_model.train(X_train, y_train, epochs=100, learning_rate=0.01, batch_size=32, verbose=False)
        
        # Test error
        pred_test = temp_model.predict(X_test)
        test_error = np.mean((pred_test - y_test)**2)
        test_errors.append(test_error)
    
    axs[1, 1].plot(param_counts, test_errors, 'o-', linewidth=2)
    axs[1, 1].set_title("Model Complexity vs. Test Error")
    axs[1, 1].set_xlabel("Parameter Count")
    axs[1, 1].set_ylabel("Test Error (MSE)")
    axs[1, 1].grid(True, linestyle='--', alpha=0.6)
    
    # Add annotations for network architectures
    for i, hidden_dims in enumerate(hidden_configs):
        architecture = f"[{', '.join(map(str, hidden_dims))}]"
        axs[1, 1].annotate(architecture, (param_counts[i], test_errors[i]),
                         textcoords="offset points", xytext=(0, 10), ha='center')
    
    plt.tight_layout()
    fig.savefig(methods_path)
    plt.close(fig)
    
    # Create animation of inference process
    animation_path = os.path.join(case_dir, "computation_animation.gif")
    
    # Select one sample for animation clarity
    # Ensure we have at least one sample
    if len(X_sample) > 0:
        single_sample = X_sample[0:1]  # Ensure it's 2D: shape (1, input_dim)
    else:
        # Fallback if X_sample is somehow empty
        single_sample = np.random.rand(1, X.shape[1])
    
    # Setup figure for animation
    fig, axs = plt.subplots(len(model.hidden_dims) + 2, 1, figsize=(10, 3*(len(model.hidden_dims) + 2)))
    
    # Visualize inference process (data flowing through network)
    layer_bars = []
    
    # For each layer, create a bar plot (initialized empty)
    for i in range(len(model.hidden_dims) + 2):  # Input, hidden layers, output
        if i == 0:
            layer_name = "Input Layer"
            size = model.input_dim
        elif i == len(model.hidden_dims) + 1:
            layer_name = "Output Layer"
            size = model.output_dim
        else:
            layer_name = f"Hidden Layer {i}"
            size = model.hidden_dims[i-1]
        
        bars = axs[i].bar(range(size), np.zeros(size))
        axs[i].set_title(f"{layer_name}")
        axs[i].set_ylim(-1.1, 1.1)  # Adjust based on activation function
        axs[i].set_ylabel("Activation")
        if size <= 20:
            axs[i].set_xticks(range(size))
        axs[i].grid(True, linestyle='--', alpha=0.6)
        
        layer_bars.append(bars)
    
    # Text for step information
    step_text = fig.text(0.5, 0.01, '', ha='center', va='center', fontsize=12, 
                        bbox=dict(boxstyle='round,pad=0.5', facecolor='lightyellow', alpha=0.5))
    
    def init():
        for bars in layer_bars:
            for bar in bars:
                bar.set_height(0)
        step_text.set_text('')
        return [bar for bars in layer_bars for bar in bars] + [step_text]
    
    def update(frame):
        # For inference animation, we'll progressively fill in activations
        # frame represents the layer index (with special frames in between)
        layer_idx = frame // 2
        is_transition = frame % 2 == 1
        
        # Get all activations
        _, activations = model._forward_with_activations(single_sample)
        
        # For full network inference
        if layer_idx < len(activations):
            # Update bars up to current layer
            for i in range(layer_idx + 1):
                if i < len(activations):  # Check if we have this layer's activations
                    current_activations = activations[i][0]  # First (only) sample
                    for j, bar in enumerate(layer_bars[i]):
                        if j < len(current_activations):
                            if is_transition and i == layer_idx:
                                # Animate transition at current layer
                                bar.set_height(current_activations[j] * 0.5)
                            else:
                                bar.set_height(current_activations[j])
        
        # Update step text
        if layer_idx == 0:
            step_text.set_text("Step 1: Input values enter network")
        elif layer_idx < len(activations) - 1:
            if is_transition:
                step_text.set_text(f"Applying {model.activation} activation to Hidden Layer {layer_idx}")
            else:
                step_text.set_text(f"Step {layer_idx+1}: Computing Hidden Layer {layer_idx} activations")
        elif layer_idx == len(activations) - 1:
            step_text.set_text("Final Step: Output values generated")
        else:
            step_text.set_text("Computation Complete")
        
        return [bar for bars in layer_bars for bar in bars] + [step_text]
    
    # Create animation with appropriate number of frames
    num_frames = 2 * (len(model.hidden_dims) + 2)
    anim = animation.FuncAnimation(fig, update, frames=num_frames,
                          init_func=init, blit=True)
    
    # Save animation
    anim.save(animation_path, writer='pillow', fps=1)
    plt.close(fig)
    logger.info(f"Created computation animation: {animation_path}")
    
    # Generate a report
    report_path = os.path.join(case_dir, "report.md")
    with open(report_path, "w") as f:
        f.write(f"# Neural Network in {Case.INSTRUMENTAL.value} Case\n\n")
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
        f.write(f"* Model architecture: Input({X.shape[1]}) → Hidden(12) → Hidden(8) → Hidden(4) → Output({y.shape[1]})\n")
        f.write(f"* Activation function: {model.activation}\n")
        f.write("* Computational method characteristics:\n")
        f.write("  - Analyzed weight distributions and transformations across layers\n")
        f.write("  - Compared different network configurations and learning parameters\n")
        f.write("  - Visualized step-by-step computation process\n")
        f.write("  - Examined gradient-based learning dynamics\n\n")
        f.write("### Visualizations\n\n")
        f.write(f"1. [Network Structure](network_structure.png)\n")
        f.write(f"2. [Data Flow](data_flow.png)\n")
        f.write(f"3. [Weight Visualization](weight_visualization.png)\n")
        f.write(f"4. [Computation Methods](computation_methods.png)\n")
        f.write(f"5. [Computation Animation](computation_animation.gif)\n")
    
    logger.info(f"Completed INSTRUMENTAL case test with visualizations in {case_dir}")

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
                ha='center', fontsize=10, fontstyle='italic', transform=ax.transAxes)
    
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