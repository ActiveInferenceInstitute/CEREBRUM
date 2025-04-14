import os
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from typing import Tuple, List, Dict, Any

from src.core.model import Case
from src.core.neural_network import NeuralNetworkModel

from src.tests.neural_network.utils import get_case_dir, create_animation, generate_report
from src.tests.neural_network.visualizers import Visualizer, plot_case_linguistic_context

def test_nominative_case(nn_regression_data, case_definitions, logger=None):
    """
    Test for NOMINATIVE case: Model as active agent generating predictions.
    
    Args:
        nn_regression_data: Tuple of (X, y) arrays for regression
        case_definitions: Dictionary of case definitions
        logger: Logger instance for logging
        
    Returns:
        Trained NeuralNetworkModel instance
    """
    # Get case info for logging
    case_info = case_definitions[Case.NOMINATIVE]
    if logger:
        logger.info(f"Testing {Case.NOMINATIVE.value} case: {case_info['linguistic_meaning']}")
        logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = get_case_dir(Case.NOMINATIVE.value)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.NOMINATIVE, linguistics_path, logger)
    
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
    if logger:
        logger.info(f"Created neural network model with architecture: input_dim={X.shape[1]}, hidden_dims=[10, 5], output_dim={y.shape[1]}")
    
    # Visualize the model structure 
    network_structure_path = os.path.join(case_dir, "network_structure.png")
    Visualizer.plot_network_structure(
        model=model,
        title=f"Neural Network Structure in {Case.NOMINATIVE.value} Case",
        save_path=network_structure_path,
        logger=logger
    )
    
    # Train the model
    if logger:
        logger.info("Training model in NOMINATIVE case (model as active prediction generator)")
    train_results = model.train(X, y, epochs=200, learning_rate=0.01, batch_size=32)
    
    # Visualize training progress
    training_path = os.path.join(case_dir, "training_history.png")
    Visualizer.plot_training_history(
        loss_history=model.loss_history,
        title=f"Training Loss in {Case.NOMINATIVE.value} Case",
        save_path=training_path,
        logger=logger
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
    if logger:
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
    animation_path = None
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
        
        # Create and save animation
        create_animation(
            fig=fig, 
            update_func=update, 
            init_func=init, 
            frames=len(epoch_predictions),
            save_path=animation_path,
            fps=2,
            logger=logger
        )
        plt.close(fig)
        
        if logger:
            logger.info(f"Created learning animation: {animation_path}")
    
    # Prepare visualizations list for the report
    visualizations = ["network_structure.png", "training_history.png", "predictions.png"]
    if animation_path:
        visualizations.append(os.path.basename(animation_path))
    
    # Prepare model info for the report
    model_info = {
        f"Model architecture": f"Input({X.shape[1]}) → Hidden(10) → Hidden(5) → Output({y.shape[1]})",
        f"Activation function": model.activation,
        f"Initial loss": f"{model.loss_history[0]:.6f}",
        f"Final loss": f"{model.loss_history[-1]:.6f}",
        f"Improvement": f"{(1 - model.loss_history[-1]/model.loss_history[0])*100:.2f}%"
    }
    
    # Generate a report
    report_path = os.path.join(case_dir, "report.md")
    generate_report(
        case_name=Case.NOMINATIVE.value,
        case_info=case_info,
        model_info=model_info,
        visualizations=visualizations,
        save_path=report_path,
        logger=logger
    )
    
    if logger:
        logger.info(f"Completed NOMINATIVE case test with visualizations in {case_dir}")
    
    # Return the model for potential further testing
    return model 