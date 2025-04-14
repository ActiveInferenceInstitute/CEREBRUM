#!/usr/bin/env python3
"""
DATIVE Case Test Module for CEREBRUM
Tests the DATIVE case in the linear regression context
"""

import os
import logging
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

from src.models.base import Case
from src.models.case_definitions import CaseDefinitions
from src.models.linear_regression import LinearRegressionModel
from src.utils.visualization import plot_case_linguistic_context

# Setup logging
logger = logging.getLogger("cerebrum-dative-test")

def test_dative_case(linear_test_data, output_dir):
    """
    Test DATIVE case: The model as recipient of data.
    
    In linguistics: The indirect object, typically a recipient.
    In regression: The model receiving data inputs or serving as destination.
    """
    # Get case definitions
    case_definitions = CaseDefinitions.get_all_cases()
    
    # Get case info for logging
    case_info = case_definitions[Case.DATIVE]
    logger.info(f"Testing {Case.DATIVE.value} case: {case_info['linguistic_meaning']}")
    logger.info(f"Statistical role: {case_info['statistical_role']}")
    
    # Create visuals directory
    case_dir = os.path.join(output_dir, Case.DATIVE.value.lower())
    os.makedirs(case_dir, exist_ok=True)
    
    # Generate linguistic context visualization
    linguistics_path = os.path.join(case_dir, "linguistic_context.png")
    plot_case_linguistic_context(Case.DATIVE, linguistics_path)
    
    # 1. Create a DATIVE case model which receives data
    model = LinearRegressionModel(model_id=f"{Case.DATIVE.value}_model", case=Case.DATIVE)
    
    # 2. Create data for this test
    X, y = linear_test_data
    X_train, X_test = X[:80], X[80:]
    y_train, y_test = y[:80], y[80:]
    
    # 3. Demonstrate DATIVE case: model receives data in batches
    logger.info(f"DATIVE case demonstration: Model receiving data in batches")
    
    # Create visualizations directory
    os.makedirs(os.path.join(case_dir, "batches"), exist_ok=True)
    
    # Set up the animation
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 6))
    
    # Batch parameters
    n_batches = 5
    batch_size = len(X_train) // n_batches
    
    # Animation function for data loading
    def animate_data_receiving(batch_idx):
        # Calculate start/end indices for this batch
        start_idx = batch_idx * batch_size
        end_idx = min(start_idx + batch_size, len(X_train))
        
        # Get batch data
        X_batch = X_train[start_idx:end_idx]
        y_batch = y_train[start_idx:end_idx]
        
        # Send batch to model (DATIVE case action)
        model.receive_data(X_batch, y_batch)
        
        # Plot the accumulated data
        ax1.clear()
        ax2.clear()
        
        # Plot all data points seen so far
        all_X = X_train[:end_idx]
        all_y = y_train[:end_idx]
        ax1.scatter(all_X, all_y, alpha=0.7, color='blue', label='Received data')
        ax1.set_xlabel('X')
        ax1.set_ylabel('y')
        ax1.set_title(f'Data Received by Model (Batch {batch_idx+1}/{n_batches})')
        ax1.grid(True, alpha=0.3)
        ax1.legend()
        
        # Plot histogram of received data
        ax2.hist(all_y, bins=15, alpha=0.7, color='green')
        ax2.set_xlabel('y values')
        ax2.set_ylabel('Frequency')
        ax2.set_title(f'Distribution of Target Values (Received {len(all_y)}/{len(y_train)} points)')
        ax2.grid(True, alpha=0.3)
        
        # Add data summary
        mean_value = np.mean(all_y)
        std_value = np.std(all_y)
        ax2.text(0.05, 0.95, f'Mean: {mean_value:.2f}\nStd: {std_value:.2f}', 
                transform=ax2.transAxes, bbox=dict(facecolor='white', alpha=0.8))
        
        plt.tight_layout()
        
        # Save batch visualization
        batch_vis_path = os.path.join(case_dir, "batches", f"batch_{batch_idx+1}.png")
        plt.savefig(batch_vis_path, dpi=100, bbox_inches='tight')
    
    # Generate animation frames
    for batch in range(n_batches):
        animate_data_receiving(batch)
        logger.info(f"DATIVE case: Model received batch {batch+1}/{n_batches}")
    
    # Save final animation
    animation_path = os.path.join(case_dir, "data_receiving_animation.gif")
    anim = FuncAnimation(fig, animate_data_receiving, frames=n_batches, interval=1000)
    anim.save(animation_path, writer='pillow', fps=1, dpi=100)
    plt.close(fig)
    
    # 4. Process the received data (DATIVE case action)
    logger.info(f"DATIVE case: Model processing received data")
    X_processed, y_processed = model.process_data_buffer()
    
    # 5. Visualize the processed data
    process_vis_path = os.path.join(case_dir, "processed_data_visualization.png")
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 6))
    
    # Plot original data
    ax1.scatter(X_train, y_train, alpha=0.7, color='blue', label='Original data')
    ax1.set_xlabel('X')
    ax1.set_ylabel('y')
    ax1.set_title('Original Data')
    ax1.grid(True, alpha=0.3)
    ax1.legend()
    
    # Plot processed data
    ax2.scatter(X_processed, y_processed, alpha=0.7, color='green', label='Processed data')
    ax2.set_xlabel('X (processed)')
    ax2.set_ylabel('y (processed)')
    ax2.set_title('Processed Data (DATIVE transformation)')
    ax2.grid(True, alpha=0.3)
    ax2.legend()
    
    plt.tight_layout()
    plt.savefig(process_vis_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # 6. Fit a reference model with the processed data
    reference_model = LinearRegressionModel(model_id="reference_for_dative", case=Case.NOMINATIVE)
    reference_model.fit(X_processed, y_processed)
    
    # 7. Evaluate on test data
    metrics = reference_model.evaluate(X_test, y_test)
    
    # 8. Document all outputs for this case
    with open(os.path.join(case_dir, "dative_results.txt"), 'w') as f:
        f.write(f"DATIVE CASE RESULTS\n")
        f.write(f"===================\n\n")
        f.write(f"Linguistic meaning: {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n")
        f.write(f"Regression context: {case_info['regression_context']}\n\n")
        
        f.write(f"Data Processing Summary:\n")
        f.write(f"- Original data: {len(X_train)} training points\n")
        f.write(f"- Processed in {n_batches} batches of ~{batch_size} points each\n")
        f.write(f"- Mean of original y: {np.mean(y_train):.4f}\n")
        f.write(f"- Mean of processed y: {np.mean(y_processed):.4f}\n")
        f.write(f"- Standard deviation change: {np.std(y_train):.4f} → {np.std(y_processed):.4f}\n\n")
        
        f.write(f"Model Results (after receiving data):\n")
        f.write(f"- R²: {metrics['r2']:.4f}\n")
        f.write(f"- MSE: {metrics['mse']:.4f}\n")
        f.write(f"- MAE: {metrics['mae']:.4f}\n\n")
        
        f.write(f"Linguistic Formula Example:\n")
        f.write(f"{case_info['example']}\n\n")
            
    logger.info(f"Completed DATIVE case test with visualizations in {case_dir}")
    
    return model 