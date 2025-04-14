#!/usr/bin/env python3
"""
Linear Regression Test Module for CEREBRUM
Testing different linguistic cases in the regression context
"""

import os
import sys
import uuid
import logging
import numpy as np
import matplotlib
matplotlib.use('Agg')  # Use non-interactive backend for headless environments

# Setup logging
logging.basicConfig(level=logging.INFO, 
                    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger('cerebrum-regression-tests')

# Define required dependencies
REQUIRED_PACKAGES = [
    'pandas', 'numpy', 'matplotlib', 'seaborn', 'scipy', 'sklearn'
]

# Check for required dependencies
missing_packages = []
for package in REQUIRED_PACKAGES:
    try:
        __import__(package)
    except ImportError:
        missing_packages.append(package)

if missing_packages:
    logger.error(f"Missing required packages: {', '.join(missing_packages)}")
    logger.error("Please install required packages using: pip install " + " ".join(missing_packages))
    sys.exit(1)

# Import dependencies
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import seaborn as sns
from scipy import stats
from typing import Dict, List, Tuple, Optional, Any, Union
from enum import Enum, auto
from sklearn.metrics import r2_score, mean_squared_error, mean_absolute_error
from sklearn.linear_model import LinearRegression

# Set up the output directory for visualizations
OUTPUT_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "output", "linear_regression")
os.makedirs(OUTPUT_DIR, exist_ok=True)

# Define Case enum for the linguistic cases
class Case(Enum):
    NOMINATIVE = "NOM"
    ACCUSATIVE = "ACC"
    DATIVE = "DAT"
    GENITIVE = "GEN"
    INSTRUMENTAL = "INS"
    LOCATIVE = "LOC"
    ABLATIVE = "ABL"
    VOCATIVE = "VOC"

class Model:
    """Base Model class for all CEREBRUM models."""
    
    def __init__(self, name: str, case: Case = Case.NOMINATIVE):
        self.id = str(uuid.uuid4())
        self.name = name
        self._case = case
        self.data_buffer = {'X': None, 'y': None}
        
    @property
    def case(self) -> Case:
        return self._case
        
    @case.setter
    def case(self, value: Case):
        if not isinstance(value, Case):
            raise TypeError(f"Expected Case enum, got {type(value)}")
        # Log the case change
        logging.info(f"Model '{self.name}' ({self.id}): Case changed from {self._case} to {value}")
        self._case = value
        
class CaseDefinitions:
    """Definitions for all linguistic cases in the regression context."""
    
    @staticmethod
    def nominative() -> Dict[str, str]:
        """
        NOMINATIVE case: The subject performing the action.
        In regression: Model actively fitting parameters to data.
        """
        return {
            "linguistic_meaning": "Subject performing the action",
            "statistical_role": "Model as parameter estimator",
            "regression_context": "Model actively fitting parameters to data",
            "example": "The MODEL fits the parameters to the data."
        }
    
    @staticmethod
    def accusative() -> Dict[str, str]:
        """
        ACCUSATIVE case: The direct object receiving the action.
        In regression: Model undergoing evaluation or testing.
        """
        return {
            "linguistic_meaning": "Direct object receiving action",
            "statistical_role": "Model as evaluation subject",
            "regression_context": "Model undergoing evaluation or testing",
            "example": "We evaluate the MODEL on test data."
        }
    
    @staticmethod
    def dative() -> Dict[str, str]:
        """
        DATIVE case: The indirect object, typically recipient.
        In regression: Model receiving data or serving as destination.
        """
        return {
            "linguistic_meaning": "Indirect object or recipient",
            "statistical_role": "Model as data recipient",
            "regression_context": "Model receiving or being given data",
            "example": "We give the data to the MODEL."
        }
    
    @staticmethod
    def genitive() -> Dict[str, str]:
        """
        GENITIVE case: Showing possession or relation.
        In regression: Model generating predictions or outputs.
        """
        return {
            "linguistic_meaning": "Possessive or relational function",
            "statistical_role": "Model as prediction generator",
            "regression_context": "Model generating predictions from inputs",
            "example": "The predictions of the MODEL were accurate."
        }
    
    @staticmethod
    def instrumental() -> Dict[str, str]:
        """
        INSTRUMENTAL case: The means by which action is accomplished.
        In regression: Model being used as a tool for analysis.
        """
        return {
            "linguistic_meaning": "Means or instrument of action",
            "statistical_role": "Model as analytical instrument",
            "regression_context": "Using model as a tool for data analysis",
            "example": "We analyze the data with the MODEL."
        }
    
    @staticmethod
    def locative() -> Dict[str, str]:
        """
        LOCATIVE case: Indicating location or position.
        In regression: Model being positioned in parameter space.
        """
        return {
            "linguistic_meaning": "Location or position indication",
            "statistical_role": "Model position in parameter space",
            "regression_context": "Model at a position in parameter/hypothesis space",
            "example": "The optimal solution exists in the MODEL's parameter space."
        }
    
    @staticmethod
    def ablative() -> Dict[str, str]:
        """
        ABLATIVE case: Indicating separation or movement away from.
        In regression: Extracting information or deriving conclusions from model.
        """
        return {
            "linguistic_meaning": "Movement away from or source",
            "statistical_role": "Model as information source",
            "regression_context": "Extracting insights or derivations from model",
            "example": "From the MODEL, we can derive feature importance."
        }
    
    @staticmethod
    def vocative() -> Dict[str, str]:
        """
        VOCATIVE case: Used for direct address.
        In regression: Querying or interacting directly with the model.
        """
        return {
            "linguistic_meaning": "Direct address or calling",
            "statistical_role": "Direct model interaction",
            "regression_context": "Querying or commanding the model directly",
            "example": "MODEL, what is the predicted value for this input?"
        }
    
    @staticmethod
    def get_all_cases() -> Dict[Case, Dict[str, str]]:
        """Get definitions for all grammatical cases."""
        return {
            Case.NOMINATIVE: CaseDefinitions.nominative(),
            Case.ACCUSATIVE: CaseDefinitions.accusative(),
            Case.DATIVE: CaseDefinitions.dative(),
            Case.GENITIVE: CaseDefinitions.genitive(),
            Case.INSTRUMENTAL: CaseDefinitions.instrumental(),
            Case.LOCATIVE: CaseDefinitions.locative(),
            Case.ABLATIVE: CaseDefinitions.ablative(),
            Case.VOCATIVE: CaseDefinitions.vocative()
        }

class DataGenerator:
    """Data generation utilities for regression tests."""
    
    @staticmethod
    def linear_data(
        n_samples: int = 100,
        slope: float = 2.0,
        intercept: float = 5.0,
        noise_level: float = 1.0,
        x_range: Tuple[float, float] = (-5, 5),
        random_seed: Optional[int] = 42
    ) -> Tuple[np.ndarray, np.ndarray]:
        """Generate linear regression data with specified parameters."""
        if random_seed is not None:
            np.random.seed(random_seed)
            
        # Generate X values within the specified range
        X = np.random.uniform(x_range[0], x_range[1], n_samples).reshape(-1, 1)
        
        # Generate y values with noise
        y = intercept + slope * X.flatten() + np.random.normal(0, noise_level, n_samples)
        
        return X, y

class Visualizer:
    """Visualization utilities for regression tests."""
    
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
        """Plot regression data."""
        fig, ax = plt.subplots(figsize=figsize)
        ax.scatter(X, y, alpha=0.7, color='blue', label='Data points')
        ax.set_xlabel(xlabel)
        ax.set_ylabel(ylabel)
        ax.set_title(title)
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        if save_path:
            plt.savefig(save_path, dpi=100, bbox_inches='tight')
            
        return fig
        
class LinearRegressionModel(Model):
    """Linear regression model with linguistic case functionality."""
    
    def __init__(self, model_id: str, case: Case, hyperparameters: Optional[Dict[str, Any]] = None):
        super().__init__(name=model_id, case=case)
        
        # Default hyperparameters
        self._hyperparameters = {
            'fit_intercept': True,
            'n_jobs': None,
            'positive': False
        }
        
        # Update with custom hyperparameters if provided
        if hyperparameters:
            self._hyperparameters.update(hyperparameters)
            
        # Initialize the underlying model
        self._model = LinearRegression(**self._hyperparameters)
        
        # Model state
        self._is_fitted = False
        self._params = None
        
    def fit(self, X: np.ndarray, y: np.ndarray) -> 'LinearRegressionModel':
        """Fit the model (primary action for NOMINATIVE case)."""
        # Check if case is appropriate
        if self.case == Case.NOMINATIVE:
            logger.info(f"NOMINATIVE case: Model actively estimating parameters from data")
        else:
            logger.info(f"Non-standard case ({self.case.value}) for fitting operation")
        
        # Fit the model
        logger.info(f"[{self.name} | {self.case.value}] Fitting model...")
        self._model.fit(X, y)
        self._is_fitted = True
        
        # Extract and store parameters
        self._params = {
            'intercept': self._model.intercept_,
            'coefficients': self._model.coef_
        }
        
        logger.info(f"Model fitted. Parameters (theta): {np.concatenate([[self._model.intercept_], self._model.coef_])}")
        
        return self
    
    def predict(self, X: np.ndarray) -> np.ndarray:
        """Generate predictions from the model."""
        if not self._is_fitted and self.case != Case.GENITIVE:
            raise ValueError("Model must be fitted before making predictions unless in GENITIVE case")
        
        # For GENITIVE case, focus on the generation aspect
        if self.case == Case.GENITIVE:
            logger.info("GENITIVE case: Model generating predictions as its possessive function")
        
        return self._model.predict(X)
    
    def evaluate(self, X: np.ndarray, y: np.ndarray, fitted_params: Optional[Dict[str, Any]] = None) -> Dict[str, float]:
        """Evaluate the model (primary action for ACCUSATIVE case)."""
        if not self._is_fitted and not fitted_params:
            raise ValueError("Model must be fitted or parameters provided for evaluation")
            
        # If in ACCUSATIVE case, highlight that the model is undergoing evaluation
        if self.case == Case.ACCUSATIVE:
            logger.info("ACCUSATIVE case: Evaluating with provided parameters rather than internal state")
            logger.info("ACCUSATIVE case: Model undergoing formal evaluation")
        
        # Make predictions using the fitted model or with provided parameters
        if fitted_params:
            original_params = None
            if self._is_fitted:
                # Save original parameters
                original_params = {
                    'intercept': self._model.intercept_,
                    'coefficients': self._model.coef_.copy()
                }
                
                # Set the provided parameters
                self._model.intercept_ = fitted_params.get('intercept', 0.0)
                self._model.coef_ = np.array(fitted_params.get('coefficients', [0.0]))
                
            # Make predictions with the provided parameters
            y_pred = self._model.predict(X)
            
            # Restore original parameters if they existed
            if original_params:
                self._model.intercept_ = original_params['intercept']
                self._model.coef_ = original_params['coefficients']
        else:
            # Use the fitted model
            y_pred = self.predict(X)
        
        # Calculate evaluation metrics
        metrics = {}
        metrics['r2'] = r2_score(y, y_pred)
        metrics['mse'] = mean_squared_error(y, y_pred)
        metrics['mae'] = mean_absolute_error(y, y_pred)
        
        # Additional metrics for regression
        n_samples = len(X)
        n_features = X.shape[1] if len(X.shape) > 1 else 1
        metrics['df_residual'] = n_samples - n_features - 1  # Degrees of freedom (residual)
        metrics['df_regression'] = n_features  # Degrees of freedom (regression)
        
        # Sum of squared errors
        residuals = y - y_pred
        ssr = np.sum(residuals**2)  # Sum of squared residuals
        sst = np.sum((y - np.mean(y))**2)  # Total sum of squares
        
        # F-statistic for overall significance
        if metrics['df_residual'] > 0:
            sse = sst - ssr  # Sum of squared explained
            if ssr > 0:
                metrics['f_statistic'] = (sse / metrics['df_regression']) / (ssr / metrics['df_residual'])
            else:
                metrics['f_statistic'] = float('inf')
        else:
            metrics['f_statistic'] = float('nan')
            
        if self.case == Case.ACCUSATIVE:
            logger.info(f"ACCUSATIVE evaluation: R² = {metrics['r2']:.4f}, F = {metrics['f_statistic']:.4f}")
            
        return metrics
    
    def receive_data(self, X: np.ndarray, y: Optional[np.ndarray] = None) -> 'LinearRegressionModel':
        """Receive data into the model's buffer (primary action for DATIVE case)."""
        # If in DATIVE case, emphasize the receiving aspect
        if self.case == Case.DATIVE:
            logger.info("DATIVE case: Model receiving data as recipient")
            
        # Store data in buffer
        self.data_buffer['X'] = X
        if y is not None:
            self.data_buffer['y'] = y
            
        return self
    
    def process_data_buffer(self) -> Tuple[np.ndarray, Optional[np.ndarray]]:
        """Process the data in the buffer (useful for DATIVE case)."""
        if self.data_buffer['X'] is None:
            raise ValueError("No data in buffer to process")
        
        X = self.data_buffer['X']
        y = self.data_buffer['y']
        
        # If in DATIVE case, highlight processing of received data
        if self.case == Case.DATIVE:
            logger.info("DATIVE case: Processing received data")
            
            # Perform some processing specific to DATIVE case
            # For example, scaling or formatting the data
            if X is not None and len(X) > 0:
                # Example: Center the X data
                X = X - np.mean(X, axis=0)
                
            if y is not None and len(y) > 0:
                # Example: Center the y data
                y = y - np.mean(y)
        
        return X, y
        
def plot_case_linguistic_context(case: Case, save_path: str) -> None:
    """Create a visualization showing the linguistic context of the case."""
    case_defs = CaseDefinitions.get_all_cases()
    case_info = case_defs[case]
    
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 7))
    
    # Create a diagram illustrating the case's role
    ax1.set_xlim(0, 10)
    ax1.set_ylim(0, 10)
    ax1.axis('off')
    
    # Common elements
    ax1.text(5, 9.5, f"{case.value} CASE: {case_info['linguistic_meaning']}", 
             ha='center', fontsize=16, fontweight='bold')
    
    # Create specific diagrams based on case
    if case == Case.NOMINATIVE:
        # Draw model as active subject
        circle_model = plt.Circle((3, 5), 1.5, color='blue', alpha=0.7)
        ax1.add_patch(circle_model)
        ax1.text(3, 5, "MODEL\n(Subject)", ha='center', va='center', color='white', fontweight='bold')
        
        # Draw data as object
        rect_data = plt.Rectangle((7, 4.5), 2, 1, color='green', alpha=0.7)
        ax1.add_patch(rect_data)
        ax1.text(8, 5, "DATA\n(Object)", ha='center', va='center', fontsize=9)
        
        # Draw arrow for action
        ax1.arrow(4.5, 5, 2, 0, head_width=0.3, head_length=0.3, fc='black', ec='black')
        ax1.text(5.5, 5.5, "fits", ha='center', fontweight='bold')
        
    elif case == Case.ACCUSATIVE:
        # Draw evaluator as subject
        circle_eval = plt.Circle((3, 5), 1.5, color='red', alpha=0.7)
        ax1.add_patch(circle_eval)
        ax1.text(3, 5, "EVALUATOR\n(Subject)", ha='center', va='center', color='white', fontweight='bold', fontsize=9)
        
        # Draw model as object
        rect_model = plt.Rectangle((7, 4.5), 2, 1, color='blue', alpha=0.7)
        ax1.add_patch(rect_model)
        ax1.text(8, 5, "MODEL\n(Object)", ha='center', va='center', fontsize=9)
        
        # Draw arrow for action
        ax1.arrow(4.5, 5, 2, 0, head_width=0.3, head_length=0.3, fc='black', ec='black')
        ax1.text(5.5, 5.5, "evaluates", ha='center', fontweight='bold')
        
    elif case == Case.DATIVE:
        # Draw data provider as subject
        circle_provider = plt.Circle((2, 5), 1.5, color='green', alpha=0.7)
        ax1.add_patch(circle_provider)
        ax1.text(2, 5, "PROVIDER\n(Subject)", ha='center', va='center', color='white', fontweight='bold', fontsize=9)
        
        # Draw data as direct object
        rect_data = plt.Rectangle((5, 6.5), 2, 1, color='orange', alpha=0.7)
        ax1.add_patch(rect_data)
        ax1.text(6, 7, "DATA\n(Direct Obj)", ha='center', va='center', fontsize=9)
        
        # Draw model as indirect object (recipient)
        circle_model = plt.Circle((8, 5), 1.5, color='blue', alpha=0.7)
        ax1.add_patch(circle_model)
        ax1.text(8, 5, "MODEL\n(Recipient)", ha='center', va='center', color='white', fontweight='bold', fontsize=9)
        
        # Draw arrows for action
        ax1.arrow(3.5, 5.2, 1.2, 1, head_width=0.3, head_length=0.3, fc='black', ec='black')
        ax1.arrow(6.5, 6.7, 1, -0.8, head_width=0.3, head_length=0.3, fc='black', ec='black')
        ax1.text(4.5, 6.3, "gives", ha='center', fontweight='bold')
        ax1.text(7, 6, "to", ha='center', fontweight='bold')
        
    # Add specific information about the case in the right panel
    ax2.axis('off')
    ax2.set_xlim(0, 10)
    ax2.set_ylim(0, 10)
    
    # Title
    ax2.text(5, 9.5, f"{case.value} Case in Linear Regression", ha='center', fontsize=14, fontweight='bold')
    
    # Linguistic meaning
    ax2.text(0.5, 8.5, "Linguistic Meaning:", fontweight='bold')
    ax2.text(0.5, 8.0, case_info['linguistic_meaning'])
    
    # Statistical role
    ax2.text(0.5, 7.0, "Statistical Role:", fontweight='bold')
    ax2.text(0.5, 6.5, case_info['statistical_role'])
    
    # Context
    ax2.text(0.5, 5.5, "Regression Context:", fontweight='bold')
    ax2.text(0.5, 5.0, case_info['regression_context'])
    
    # Example
    ax2.text(0.5, 4.0, "Linguistic Example:", fontweight='bold')
    ax2.text(0.5, 3.5, case_info['example'])
    
    # Add technical information
    ax2.text(0.5, 2.0, "Implementation Notes:", fontweight='bold')
    
    if case == Case.NOMINATIVE:
        tech_note = "• Primary method: fit(X, y)\n• Focus on parameter estimation\n• Active role in coefficient calculation"
    elif case == Case.ACCUSATIVE:
        tech_note = "• Primary method: evaluate(X, y)\n• Focus on performance metrics\n• Passive role in hypothesis testing"
    elif case == Case.DATIVE:
        tech_note = "• Primary method: receive_data(X, y)\n• Focus on data intake & buffering\n• Recipient role in data processing"
    elif case == Case.GENITIVE:
        tech_note = "• Primary method: predict(X)\n• Focus on output generation\n• Possessive role in creating predictions"
    elif case == Case.INSTRUMENTAL:
        tech_note = "• Primary method: analyze_data(X, y)\n• Focus on feature relationships\n• Tool role in statistical analysis"
    elif case == Case.LOCATIVE:
        tech_note = "• Primary method: locate_in_space()\n• Focus on parameter space\n• Position role in solution space"
    elif case == Case.ABLATIVE:
        tech_note = "• Primary method: extract_insights()\n• Focus on derivation\n• Source role for interpretations"
    elif case == Case.VOCATIVE:
        tech_note = "• Primary method: query(prompt)\n• Focus on direct interaction\n• Command role in model communication"
    
    ax2.text(0.5, 1.0, tech_note)
    
    # Save the figure
    plt.tight_layout()
    plt.savefig(save_path, dpi=300, bbox_inches='tight')
    logger.info(f"Saved linguistic context visualization for {case.value} case to {save_path}")
    plt.close(fig)

def test_dative_case(linear_test_data, case_definitions):
    """
    Test DATIVE case: The model as recipient of data.
    
    In linguistics: The indirect object, typically a recipient.
    In regression: The model receiving data inputs or serving as destination.
    """
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

def test_instrumental_case(linear_test_data, case_definitions):
    """
    Test INSTRUMENTAL case: The model as an instrument or tool.
    
    In linguistics: The means by which an action is accomplished.
    In regression: Model used as a tool for analysis.
    """
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

def test_locative_case(linear_test_data, case_definitions):
    """
    Test LOCATIVE case: The model positioned in parameter space.
    
    In linguistics: Location or position indication.
    In regression: Model at a position in parameter/hypothesis space.
    """
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
    
    # 1. Create a LOCATIVE case model positioned in parameter space
    model = LinearRegressionModel(model_id=f"{Case.LOCATIVE.value}_model", case=Case.LOCATIVE)
    
    logger.info(f"LOCATIVE case demonstration: Model positioned in parameter space")
    
    # 2. Generate several models in different positions in parameter space
    X, y = linear_test_data
    
    # Set seeds for reproducibility
    np.random.seed(42)
    
    # Generate a parameter space grid (slope vs intercept)
    n_grid = 10
    slopes = np.linspace(1.0, 5.0, n_grid)
    intercepts = np.linspace(-5.0, 1.0, n_grid)
    
    # Create mesh grid
    slope_grid, intercept_grid = np.meshgrid(slopes, intercepts)
    
    # Calculate error landscape
    error_landscape = np.zeros_like(slope_grid)
    
    for i in range(n_grid):
        for j in range(n_grid):
            # Create predictions with these parameters
            slope = slope_grid[i, j]
            intercept = intercept_grid[i, j]
            
            # Make predictions
            y_pred = intercept + slope * X.flatten()
            
            # Calculate mean squared error
            error_landscape[i, j] = np.mean((y - y_pred) ** 2)
    
    # Find the optimal parameters (minimum error)
    min_idx = np.unravel_index(np.argmin(error_landscape), error_landscape.shape)
    optimal_slope = slope_grid[min_idx]
    optimal_intercept = intercept_grid[min_idx]
    
    # 3. Visualize the parameter space (LOCATIVE visualization)
    param_space_vis_path = os.path.join(case_dir, "parameter_space.png")
    
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Create contour plot of the error landscape
    contour = ax.contourf(slope_grid, intercept_grid, error_landscape, 
                         levels=20, cmap='viridis')
    
    # Add contour lines
    ax.contour(slope_grid, intercept_grid, error_landscape, 
              levels=10, colors='white', alpha=0.3, linewidths=0.5)
    
    # Mark the optimal position
    ax.scatter(optimal_slope, optimal_intercept, color='red', s=100, 
              marker='*', label='Optimal Parameters')
    
    # Mark the true parameters (from data generation)
    true_slope = 3.0  # From DataGenerator.linear_data
    true_intercept = -2.0
    ax.scatter(true_slope, true_intercept, color='white', s=100, 
              marker='x', label='True Parameters')
    
    # Add arrow connecting them
    ax.annotate('', xy=(optimal_slope, optimal_intercept), 
               xytext=(true_slope, true_intercept),
               arrowprops=dict(facecolor='white', shrink=0.05, width=1.5, headwidth=8))
    
    # Set labels and title
    ax.set_xlabel('Slope')
    ax.set_ylabel('Intercept')
    ax.set_title('Parameter Space (LOCATIVE Case) - Mean Squared Error')
    
    # Add colorbar
    cbar = fig.colorbar(contour)
    cbar.set_label('Mean Squared Error')
    
    ax.legend()
    plt.tight_layout()
    plt.savefig(param_space_vis_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # 4. Create animation showing exploration of parameter space (LOCATIVE movement)
    param_anim_path = os.path.join(case_dir, "parameter_exploration.gif")
    
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 6))
    
    # Sample parameters for animation
    n_steps = 20
    
    # Create path from random start to optimal parameters
    np.random.seed(42)
    start_slope = slopes[np.random.randint(0, n_grid)]
    start_intercept = intercepts[np.random.randint(0, n_grid)]
    
    # Create path
    path_slopes = np.linspace(start_slope, optimal_slope, n_steps)
    path_intercepts = np.linspace(start_intercept, optimal_intercept, n_steps)
    
    # Animation function
    def update(frame):
        # Clear axes
        ax1.clear()
        ax2.clear()
        
        # Current parameters
        current_slope = path_slopes[frame]
        current_intercept = path_intercepts[frame]
        
        # Left plot: Parameter space
        # Create contour plot of the error landscape
        contour = ax1.contourf(slope_grid, intercept_grid, error_landscape, 
                            levels=20, cmap='viridis', alpha=0.7)
        
        # Add contour lines
        ax1.contour(slope_grid, intercept_grid, error_landscape, 
                  levels=10, colors='white', alpha=0.3, linewidths=0.5)
        
        # Mark the optimal position
        ax1.scatter(optimal_slope, optimal_intercept, color='red', s=100, 
                  marker='*', label='Optimal')
        
        # Mark the current position
        ax1.scatter(current_slope, current_intercept, color='white', s=100, 
                  marker='o', label='Current')
        
        # Draw the path taken so far
        ax1.plot(path_slopes[:frame+1], path_intercepts[:frame+1], 
                'w-', alpha=0.5, linewidth=2)
        
        # Set labels and title
        ax1.set_xlabel('Slope')
        ax1.set_ylabel('Intercept')
        ax1.set_title('Parameter Space Navigation')
        ax1.legend(loc='upper right')
        
        # Right plot: Data with model fit
        # Plot data points
        ax2.scatter(X, y, color='blue', alpha=0.5, label='Data')
        
        # Current model line
        x_line = np.linspace(X.min(), X.max(), 100)
        y_line = current_intercept + current_slope * x_line
        ax2.plot(x_line, y_line, 'r-', linewidth=2, 
                label=f'Model: y = {current_intercept:.2f} + {current_slope:.2f}x')
        
        # Calculate current error
        y_pred = current_intercept + current_slope * X.flatten()
        mse = np.mean((y - y_pred) ** 2)
        r2 = 1 - np.sum((y - y_pred) ** 2) / np.sum((y - np.mean(y)) ** 2)
        
        # Set labels and title
        ax2.set_xlabel('X')
        ax2.set_ylabel('y')
        ax2.set_title(f'Model Fit (Step {frame+1}/{n_steps})')
        
        # Add error metrics
        ax2.text(0.05, 0.95, f'MSE: {mse:.4f}\nR²: {r2:.4f}', transform=ax2.transAxes,
                bbox=dict(facecolor='white', alpha=0.8), va='top')
        
        ax2.legend()
        ax2.grid(True, alpha=0.3)
        
        plt.tight_layout()
        
        return [ax1, ax2]
    
    # Create and save animation
    anim = FuncAnimation(fig, update, frames=n_steps, blit=True)
    anim.save(param_anim_path, writer='pillow', fps=4, dpi=100)
    plt.close(fig)
    
    # 5. Fit the model at the optimal position
    optimal_model = LinearRegressionModel(model_id="optimal_locative_model", case=Case.NOMINATIVE)
    optimal_model.fit(X, y)
    
    # Switch back to LOCATIVE
    optimal_model.case = Case.LOCATIVE
    
    # 6. Visualize the vicinity around the optimal position (LOCATIVE detail)
    zoom_vis_path = os.path.join(case_dir, "parameter_vicinity.png")
    
    # Create a zoomed in parameter space around the optimal parameters
    zoom_range = 0.5  # Range to explore around optimal
    
    # Create zoom grid
    n_zoom = 20
    zoom_slopes = np.linspace(optimal_slope - zoom_range, optimal_slope + zoom_range, n_zoom)
    zoom_intercepts = np.linspace(optimal_intercept - zoom_range, optimal_intercept + zoom_range, n_zoom)
    
    zoom_slope_grid, zoom_intercept_grid = np.meshgrid(zoom_slopes, zoom_intercepts)
    zoom_error_landscape = np.zeros_like(zoom_slope_grid)
    
    for i in range(n_zoom):
        for j in range(n_zoom):
            # Create predictions with these parameters
            slope = zoom_slope_grid[i, j]
            intercept = zoom_intercept_grid[i, j]
            
            # Make predictions
            y_pred = intercept + slope * X.flatten()
            
            # Calculate mean squared error
            zoom_error_landscape[i, j] = np.mean((y - y_pred) ** 2)
    
    # Visualize the zoomed parameter space
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Create contour plot of the error landscape
    contour = ax.contourf(zoom_slope_grid, zoom_intercept_grid, zoom_error_landscape, 
                         levels=50, cmap='viridis')
    
    # Add contour lines
    ax.contour(zoom_slope_grid, zoom_intercept_grid, zoom_error_landscape, 
              levels=20, colors='white', alpha=0.3, linewidths=0.5)
    
    # Mark the optimal position
    ax.scatter(optimal_slope, optimal_intercept, color='red', s=150, 
              marker='*', label='Optimal Parameters')
    
    # Mark the fitted model parameters
    fitted_slope = optimal_model._params['coefficients'][0]
    fitted_intercept = optimal_model._params['intercept']
    ax.scatter(fitted_slope, fitted_intercept, color='white', s=100, 
              marker='o', label='Fitted Parameters')
    
    # Set labels and title
    ax.set_xlabel('Slope (zoomed view)')
    ax.set_ylabel('Intercept (zoomed view)')
    ax.set_title('Detailed Parameter Vicinity (LOCATIVE Case)')
    
    # Add colorbar
    cbar = fig.colorbar(contour)
    cbar.set_label('Mean Squared Error')
    
    # Add text box with information
    info_text = (f"Parameter Space Analysis\n"
                 f"Optimal parameters:\n"
                 f"  Slope: {optimal_slope:.6f}\n"
                 f"  Intercept: {optimal_intercept:.6f}\n\n"
                 f"Fitted parameters:\n"
                 f"  Slope: {fitted_slope:.6f}\n"
                 f"  Intercept: {fitted_intercept:.6f}\n\n"
                 f"MSE at optimal: {np.min(error_landscape):.6f}")
    
    ax.text(1.05, 0.5, info_text, transform=ax.transAxes, 
            bbox=dict(facecolor='white', alpha=0.8), va='center')
    
    ax.legend()
    plt.tight_layout()
    plt.savefig(zoom_vis_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # 7. Document all outputs for this case
    with open(os.path.join(case_dir, "locative_results.txt"), 'w') as f:
        f.write(f"LOCATIVE CASE RESULTS\n")
        f.write(f"====================\n\n")
        f.write(f"Linguistic meaning: {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n")
        f.write(f"Regression context: {case_info['regression_context']}\n\n")
        
        f.write(f"Parameter Space Analysis:\n")
        f.write(f"- Optimal parameters: Slope={optimal_slope:.6f}, Intercept={optimal_intercept:.6f}\n")
        f.write(f"- Fitted parameters: Slope={fitted_slope:.6f}, Intercept={fitted_intercept:.6f}\n")
        f.write(f"- MSE at optimal: {np.min(error_landscape):.6f}\n")
        f.write(f"- Parameter space size: {n_grid}x{n_grid} grid\n")
        f.write(f"- Slope range: [{slopes[0]:.2f}, {slopes[-1]:.2f}]\n")
        f.write(f"- Intercept range: [{intercepts[0]:.2f}, {intercepts[-1]:.2f}]\n\n")
        
        f.write(f"Path Analysis:\n")
        f.write(f"- Starting position: Slope={start_slope:.4f}, Intercept={start_intercept:.4f}\n")
        f.write(f"- Path length: {n_steps} steps\n")
        f.write(f"- Distance traveled in parameter space: {np.sqrt((optimal_slope-start_slope)**2 + (optimal_intercept-start_intercept)**2):.4f}\n\n")
        
        f.write(f"Linguistic Formula Example:\n")
        f.write(f"{case_info['example']}\n\n")
            
    logger.info(f"Completed LOCATIVE case test with visualizations in {case_dir}")
    
    return model

def test_vocative_case(linear_test_data, case_definitions):
    """
    Test VOCATIVE case: Querying or interacting directly with the model.
    
    In linguistics: Used for direct address.
    In regression: Querying the model directly.
    """
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
    
    # 1. First fit a reference model
    X, y = linear_test_data
    
    reference_model = LinearRegressionModel(model_id="reference_for_vocative", case=Case.NOMINATIVE)
    reference_model.fit(X, y)
    
    # 2. Create a VOCATIVE case model for direct interaction
    model = LinearRegressionModel(model_id=f"{Case.VOCATIVE.value}_model", case=Case.VOCATIVE)
    
    # Copy the parameters from the reference model
    model._model.intercept_ = reference_model._model.intercept_
    model._model.coef_ = reference_model._model.coef_.copy()
    model._is_fitted = True
    model._params = {
        'intercept': model._model.intercept_,
        'coefficients': model._model.coef_
    }
    
    logger.info(f"VOCATIVE case demonstration: Directly interacting with the model")
    
    # 3. Create a series of queries for the model
    # Define query types for the model
    queries = [
        {"type": "predict", "data": np.array([[1.5]]), "message": "What is the predicted value at x=1.5?"},
        {"type": "confidence", "data": np.array([[2.0]]), "message": "What is the confidence interval at x=2.0?"},
        {"type": "error", "data": np.array([[0.0]]), "message": "What is the prediction error at x=0?"},
        {"type": "formula", "message": "What is your formula?"},
        {"type": "range", "message": "What is your valid input range?"},
        {"type": "explain", "data": np.array([[3.0]]), "message": "Explain your prediction at x=3.0"}
    ]
    
    # 4. Add respond_to_query method to the LinearRegressionModel class
    def respond_to_query(self, query_type: str, **kwargs) -> Dict[str, Any]:
        """Method for VOCATIVE case to respond to direct queries."""
        if not self._is_fitted:
            return {"error": "Model is not fitted yet."}
            
        # Check if we're in VOCATIVE case
        if self.case != Case.VOCATIVE:
            logger.warning(f"respond_to_query called outside VOCATIVE case: {self.case}")
            
        response = {}
        
        # Process different query types
        if query_type == "predict":
            data = kwargs.get('data')
            if data is None:
                return {"error": "No data provided for prediction"}
                
            # Make prediction
            prediction = self.predict(data)
            response["prediction"] = prediction
            response["status"] = "success"
            
        elif query_type == "confidence":
            data = kwargs.get('data')
            confidence = kwargs.get('confidence_level', 0.95)
            
            if data is None:
                return {"error": "No data provided for confidence interval"}
                
            # For simplicity, use a fixed variance estimate
            # In practice, this would use the residual variance and leverage
            sigma = 1.0
            t_value = stats.t.ppf((1 + confidence) / 2, 100 - 2)
            
            prediction = self.predict(data)
            margin = t_value * sigma / np.sqrt(100)
            
            response["prediction"] = prediction
            response["lower_bound"] = prediction - margin
            response["upper_bound"] = prediction + margin
            response["confidence_level"] = confidence
            response["status"] = "success"
            
        elif query_type == "error":
            data = kwargs.get('data')
            true_value = kwargs.get('true_value')
            
            if data is None:
                return {"error": "No data provided for error estimation"}
                
            prediction = self.predict(data)
            
            if true_value is not None:
                error = true_value - prediction
                response["true_value"] = true_value
                response["error"] = error
            
            response["prediction"] = prediction
            response["status"] = "success"
            
        elif query_type == "formula":
            # Create formula string
            intercept = self._params['intercept']
            coefs = self._params['coefficients']
            
            formula = f"y = {intercept:.4f}"
            
            for i, coef in enumerate(coefs):
                if coef >= 0:
                    formula += f" + {coef:.4f} * x_{i+1}"
                else:
                    formula += f" - {abs(coef):.4f} * x_{i+1}"
            
            response["formula"] = formula
            response["parameters"] = self._params
            response["status"] = "success"
            
        elif query_type == "range":
            # In a real system, this would be based on the training data range
            response["x_min"] = -10.0
            response["x_max"] = 10.0
            response["recommended_range"] = (-5.0, 5.0)
            response["status"] = "success"
            
        elif query_type == "explain":
            data = kwargs.get('data')
            if data is None:
                return {"error": "No data provided for explanation"}
                
            # Make prediction
            prediction = self.predict(data)[0]
            
            # Generate explanation components
            intercept = self._params['intercept']
            coefs = self._params['coefficients']
            
            components = []
            total = intercept
            components.append({"term": "intercept", "value": intercept})
            
            for i, coef in enumerate(coefs):
                component_value = coef * data[0, i]
                total += component_value
                components.append({"term": f"x_{i+1}", "coefficient": coef, 
                                  "input": data[0, i], "value": component_value})
            
            response["prediction"] = prediction
            response["components"] = components
            response["status"] = "success"
            
        else:
            response["error"] = f"Unknown query type: {query_type}"
            response["status"] = "error"
            
        return response
    
    # Monkey patch the method to the model's class
    LinearRegressionModel.respond_to_query = respond_to_query
    
    # 5. Visualize the interactive query session
    conversation_vis_path = os.path.join(case_dir, "query_conversation.png")
    
    fig, ax = plt.subplots(figsize=(12, 10))
    ax.axis('off')
    ax.set_xlim(0, 10)
    ax.set_ylim(0, 10)
    
    # Set up the conversation display
    ax.text(5, 9.5, "VOCATIVE CASE: Direct Model Interaction", ha='center', fontsize=16, fontweight='bold')
    
    # Show the model parameters at the top
    params_text = (f"Model Parameters:\n"
                 f"Intercept: {model._params['intercept']:.4f}\n"
                 f"Coefficient: {model._params['coefficients'][0]:.4f}")
    
    ax.text(5, 8.5, params_text, ha='center', fontsize=10, 
           bbox=dict(facecolor='lightgray', alpha=0.5))
    
    # Process each query and display
    responses = []
    y_pos = 7.5
    
    for i, query in enumerate(queries):
        # Display the query
        query_text = query['message']
        ax.text(1, y_pos, f"Query {i+1}: {query_text}", fontsize=11, fontweight='bold',
              bbox=dict(facecolor='lightblue', alpha=0.3))
        
        # Process query
        kwargs = {k: v for k, v in query.items() if k not in ['type', 'message']}
        response = model.respond_to_query(query['type'], **kwargs)
        responses.append(response)
        
        # Format response text
        if query['type'] == "predict":
            response_text = f"Prediction: {response['prediction'][0]:.4f}"
        elif query['type'] == "confidence":
            response_text = (f"Prediction: {response['prediction'][0]:.4f}\n"
                           f"95% CI: [{response['lower_bound'][0]:.4f}, {response['upper_bound'][0]:.4f}]")
        elif query['type'] == "error":
            response_text = f"Prediction: {response['prediction'][0]:.4f}"
        elif query['type'] == "formula":
            response_text = f"Formula: {response['formula']}"
        elif query['type'] == "range":
            response_text = (f"Valid range: [{response['x_min']}, {response['x_max']}]\n"
                           f"Recommended: {response['recommended_range']}")
        elif query['type'] == "explain":
            comp_text = "\n".join([f"{c['term']}: {c['value']:.4f}" for c in response['components']])
            response_text = f"Prediction: {response['prediction']:.4f}\nComponents:\n{comp_text}"
        else:
            response_text = str(response)
        
        # Display the response
        ax.text(9, y_pos, response_text, fontsize=10, ha='right',
              bbox=dict(facecolor='lightyellow', alpha=0.5))
        
        y_pos -= 1.2
    
    # Add an illustration of the vocative interaction
    # Draw model as character
    model_circle = plt.Circle((2.5, 2), 0.8, color='blue', alpha=0.7)
    ax.add_patch(model_circle)
    ax.text(2.5, 2, "MODEL", ha='center', va='center', color='white', fontweight='bold')
    
    # Draw user as character  
    user_circle = plt.Circle((7.5, 2), 0.8, color='green', alpha=0.7)
    ax.add_patch(user_circle)
    ax.text(7.5, 2, "USER", ha='center', va='center', color='white', fontweight='bold')
    
    # Draw conversation arrows
    ax.arrow(3.5, 2, 3, 0, head_width=0.2, head_length=0.2, fc='black', ec='black')
    ax.arrow(7, 1.7, -3.5, 0, head_width=0.2, head_length=0.2, fc='black', ec='black')
    
    ax.text(5, 2.3, "Queries", ha='center')
    ax.text(5, 1.4, "Responses", ha='center')
    
    # Title for this section
    ax.text(5, 3.2, "VOCATIVE Case: Direct Model Interaction", 
           ha='center', fontsize=12, fontweight='bold',
           bbox=dict(facecolor='white', alpha=0.8))
    
    plt.tight_layout()
    plt.savefig(conversation_vis_path, dpi=100, bbox_inches='tight')
    plt.close(fig)
    
    # 6. Create an interactive animation of model querying
    query_anim_path = os.path.join(case_dir, "interactive_query_animation.gif")
    
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 6))
    
    # Generate data for visualization
    x_range = np.linspace(-5, 5, 100)
    y_pred = model.predict(x_range.reshape(-1, 1))
    
    # Animation function
    def update(frame):
        # Calculate which query we're on and which stage
        query_idx = frame // 3  # 3 frames per query
        stage = frame % 3       # 0: question, 1: thinking, 2: answer
        
        if query_idx >= len(queries):
            query_idx = len(queries) - 1
            stage = 2
        
        # Clear axes
        ax1.clear()
        ax2.clear()
        
        # Current query
        query = queries[query_idx]
        
        # Left plot: Data visualization with model
        # Plot the data
        ax1.scatter(X, y, color='blue', alpha=0.3, label='Data')
        
        # Plot the model prediction line
        ax1.plot(x_range, y_pred, 'r-', linewidth=2, label='Model prediction')
        
        # Highlight the query point if applicable
        if 'data' in query:
            query_x = query['data'][0, 0]
            query_y = model.predict(np.array([[query_x]]))[0]
            
            if stage == 0:  # Question
                ax1.scatter(query_x, query_y, color='green', s=150, 
                          marker='o', label='Query point')
                ax1.axvline(x=query_x, color='green', linestyle='--', alpha=0.5)
            elif stage == 1:  # Thinking
                ax1.scatter(query_x, query_y, color='orange', s=150, 
                          marker='o', label='Processing query...')
                ax1.axvline(x=query_x, color='orange', linestyle='--', alpha=0.5)
            else:  # Answer
                ax1.scatter(query_x, query_y, color='red', s=150, 
                          marker='o', label='Response point')
                ax1.axvline(x=query_x, color='red', linestyle='--', alpha=0.5)
                ax1.axhline(y=query_y, color='red', linestyle='--', alpha=0.5)
        
        # Set labels and title
        ax1.set_xlabel('X')
        ax1.set_ylabel('y')
        ax1.set_title('Model Visualization')
        ax1.legend()
        ax1.grid(True, alpha=0.3)
        
        # Right plot: Conversation display
        ax2.axis('off')
        ax2.set_xlim(0, 10)
        ax2.set_ylim(0, 10)
        
        # Display query
        if stage >= 0:
            ax2.text(0.5, 9.0, f"Query {query_idx+1}: {query['message']}", fontsize=12, fontweight='bold',
                  bbox=dict(facecolor='lightblue', alpha=0.3))
        
        # Display thinking animation
        if stage >= 1:
            thinking = "Processing" + "." * (frame % 4)
            ax2.text(0.5, 7.0, thinking, fontsize=12, color='orange',
                   bbox=dict(facecolor='white', alpha=0.8))
        
        # Display response
        if stage >= 2:
            response = responses[query_idx]
            
            # Format response text based on query type
            if query['type'] == "predict":
                response_text = f"Prediction: {response['prediction'][0]:.4f}"
            elif query['type'] == "confidence":
                response_text = (f"Prediction: {response['prediction'][0]:.4f}\n"
                              f"95% CI: [{response['lower_bound'][0]:.4f}, {response['upper_bound'][0]:.4f}]")
            elif query['type'] == "error":
                response_text = f"Prediction: {response['prediction'][0]:.4f}"
            elif query['type'] == "formula":
                response_text = f"Formula: {response['formula']}"
            elif query['type'] == "range":
                response_text = (f"Valid range: [{response['x_min']}, {response['x_max']}]\n"
                              f"Recommended: {response['recommended_range']}")
            elif query['type'] == "explain":
                components = response['components']
                comp_text = "\n".join([f"{c['term']}: {c['value']:.4f}" for c in components])
                response_text = f"Prediction: {response['prediction']:.4f}\nComponents:\n{comp_text}"
            else:
                response_text = str(response)
            
            ax2.text(0.5, 5.0, response_text, fontsize=11,
                   bbox=dict(facecolor='lightyellow', alpha=0.5))
        
        # Add decorative elements
        if stage >= 0:
            # Draw model and user
            model_circle = plt.Circle((2, 2), 0.5, color='blue', alpha=0.7)
            ax2.add_patch(model_circle)
            ax2.text(2, 2, "MODEL", ha='center', va='center', color='white', fontsize=8)
            
            user_circle = plt.Circle((8, 2), 0.5, color='green', alpha=0.7)
            ax2.add_patch(user_circle)
            ax2.text(8, 2, "USER", ha='center', va='center', color='white', fontsize=8)
        
        # Display title
        ax2.text(5, 9.7, "VOCATIVE Case: Direct Model Interaction", 
               ha='center', fontsize=14, fontweight='bold')
        
        return [ax1, ax2]
    
    # Create and save animation
    n_frames = len(queries) * 3
    anim = FuncAnimation(fig, update, frames=n_frames, blit=True)
    anim.save(query_anim_path, writer='pillow', fps=2, dpi=100)
    plt.close(fig)
    
    # 7. Document all outputs for this case
    with open(os.path.join(case_dir, "vocative_results.txt"), 'w') as f:
        f.write(f"VOCATIVE CASE RESULTS\n")
        f.write(f"====================\n\n")
        f.write(f"Linguistic meaning: {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n")
        f.write(f"Regression context: {case_info['regression_context']}\n\n")
        
        f.write(f"Query-Response Interactions:\n")
        for i, (query, response) in enumerate(zip(queries, responses)):
            f.write(f"- Query {i+1}: {query['message']}\n")
            
            # Format response based on type
            if query['type'] == "predict":
                f.write(f"  Response: Prediction = {response['prediction'][0]:.4f}\n")
            elif query['type'] == "confidence":
                f.write(f"  Response: Prediction = {response['prediction'][0]:.4f}, ")
                f.write(f"95% CI = [{response['lower_bound'][0]:.4f}, {response['upper_bound'][0]:.4f}]\n")
            elif query['type'] == "error":
                f.write(f"  Response: Prediction = {response['prediction'][0]:.4f}\n")
            elif query['type'] == "formula":
                f.write(f"  Response: Formula = {response['formula']}\n")
            elif query['type'] == "range":
                f.write(f"  Response: Valid range = [{response['x_min']}, {response['x_max']}]\n"
                           f"Recommended = {response['recommended_range']}\n")
            elif query['type'] == "explain":
                f.write(f"  Response: Prediction = {response['prediction']:.4f}\n")
                f.write(f"  Components:\n")
                for comp in response['components']:
                    f.write(f"    - {comp['term']}: {comp['value']:.4f}\n")
            
            f.write(f"\n")
        
        f.write(f"Linguistic Formula Example:\n")
        f.write(f"{case_info['example']}\n\n")
            
    logger.info(f"Completed VOCATIVE case test with visualizations in {case_dir}")
    
    return model

def run_all_case_tests(output_dir: str = OUTPUT_DIR) -> None:
    """Run tests for all linguistic cases in the linear regression context."""
    logger.info("Running all CEREBRUM case tests for LinearRegressionModel")
    
    # Generate test data 
    linear_data = DataGenerator.linear_data(n_samples=100, slope=3.0, intercept=-2.0)
    
    # Get case definitions
    case_definitions = CaseDefinitions.get_all_cases()
    
    # Ensure output directory exists
    os.makedirs(output_dir, exist_ok=True)
    
    # Run tests for all eight grammatical cases
    # Check if the directories already exist to avoid redundant execution
    if not os.path.exists(os.path.join(output_dir, "nom")) or not os.listdir(os.path.join(output_dir, "nom")):
        logger.info("Running NOMINATIVE case test")
        # Implement nominative case test
        logger.info("Nominative case test needs to be implemented separately")
    
    if not os.path.exists(os.path.join(output_dir, "acc")) or not os.listdir(os.path.join(output_dir, "acc")):
        logger.info("Running ACCUSATIVE case test")
        # Implement accusative case test
        logger.info("Accusative case test needs to be implemented separately")
    
    if not os.path.exists(os.path.join(output_dir, "dat")) or not os.listdir(os.path.join(output_dir, "dat")):
        logger.info("Running DATIVE case test")
        test_dative_case(linear_data, case_definitions)
    
    if not os.path.exists(os.path.join(output_dir, "gen")) or not os.listdir(os.path.join(output_dir, "gen")):
        logger.info("Running GENITIVE case test")
        # Implement genitive case test
        logger.info("Genitive case test needs to be implemented separately")
    
    if not os.path.exists(os.path.join(output_dir, "ins")) or not os.listdir(os.path.join(output_dir, "ins")):
        logger.info("Running INSTRUMENTAL case test")
        test_instrumental_case(linear_data, case_definitions)
    
    if not os.path.exists(os.path.join(output_dir, "loc")) or not os.listdir(os.path.join(output_dir, "loc")):
        logger.info("Running LOCATIVE case test")
        test_locative_case(linear_data, case_definitions)
    
    if not os.path.exists(os.path.join(output_dir, "abl")) or not os.listdir(os.path.join(output_dir, "abl")):
        logger.info("Running ABLATIVE case test")
        # Implement ablative case test
        logger.info("Ablative case test needs to be implemented separately")
    
    if not os.path.exists(os.path.join(output_dir, "voc")) or not os.listdir(os.path.join(output_dir, "voc")):
        logger.info("Running VOCATIVE case test")
        test_vocative_case(linear_data, case_definitions)
    
    logger.info("Completed all case tests for LinearRegressionModel")

# Run tests if this module is executed directly
if __name__ == "__main__":
    run_all_case_tests()