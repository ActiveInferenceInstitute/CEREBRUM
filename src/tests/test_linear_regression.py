#!/usr/bin/env python
"""
Test module for demonstrating CEREBRUM cases with linear regression models.

This module provides comprehensive visualization and statistical analysis to
illustrate how different linguistic cases can be deployed in a regression context.
"""
import os
import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.animation import FuncAnimation
from matplotlib import cm
import pytest
from typing import Dict, List, Tuple, Any, Optional, Callable
from enum import Enum
import json
import logging
from scipy import stats
from sklearn.metrics import r2_score, mean_squared_error, mean_absolute_error
from matplotlib.gridspec import GridSpec
import matplotlib.patches as patches

# Add the src directory to the path for imports
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

# Import CEREBRUM components
from core.model import Model, Case

# Configure logging
logger = logging.getLogger("cerebrum-regression-tests")

# Set plotting style
# plt.style.use('seaborn-v0_8-whitegrid') # Temporarily comment out
# sns.set_context("notebook", font_scale=1.2) # Temporarily comment out

# Constants
OUTPUT_DIR = os.path.join(os.path.dirname(__file__), "output", "linear_regression")
os.makedirs(OUTPUT_DIR, exist_ok=True)

# Custom marker for tests in this module
pytestmark = [pytest.mark.model, pytest.mark.regression]

class CaseDefinitions:
    """
    Provides linguistic explanations and statistical relevance of each case for linear regression.
    
    This class bridges the gap between linguistic cases from the CEREBRUM framework
    and their statistical/mathematical counterparts in regression analysis.
    """
    
    @staticmethod
    def nominative() -> Dict[str, str]:
        """
        NOMINATIVE case: The model as the active agent or subject.
        
        In linguistics: The subject of the sentence, performing the action.
        In regression: The model actively generating predictions, estimating parameters.
        """
        return {
            "case": Case.NOMINATIVE.value,
            "linguistic_meaning": "Subject performing the action",
            "statistical_role": "Model as parameter estimator",
            "regression_context": "The model that fits coefficients to data, making the line 'act' on the data space",
            "formula": "ŷ = θ₀ + θ₁x",
            "primary_methods": "fit(), predict()",
            "visualization": "Regression line overlaid on data points",
            "example": "The LINEAR MODEL fits the data points.",
        }
    
    @staticmethod
    def accusative() -> Dict[str, str]:
        """
        ACCUSATIVE case: The model as the direct object or recipient of validation.
        
        In linguistics: The direct object receiving the action of the verb.
        In regression: The model being evaluated, tested, or validated.
        """
        return {
            "case": Case.ACCUSATIVE.value, 
            "linguistic_meaning": "Direct object receiving action",
            "statistical_role": "Model as evaluation subject",
            "regression_context": "The model that undergoes validation, hypothesis testing, and performance assessment",
            "formula": "R² = 1 - SSres/SStot",
            "primary_methods": "evaluate(), validate()",
            "visualization": "Diagnostic plots, metrics visualization",
            "example": "The analyst EVALUATES the LINEAR MODEL."
        }
    
    @staticmethod
    def dative() -> Dict[str, str]:
        """
        DATIVE case: The model as indirect object or data recipient.
        
        In linguistics: The indirect object, typically a recipient.
        In regression: The model receiving data inputs or serving as destination.
        """
        return {
            "case": Case.DATIVE.value,
            "linguistic_meaning": "Indirect object or recipient",
            "statistical_role": "Model as data receiver",
            "regression_context": "The model that receives input data, feature vectors, or streaming observations",
            "formula": "Model ← {(x₁,y₁), (x₂,y₂), ..., (xₙ,yₙ)}",
            "primary_methods": "receive_data(), process_features()",
            "visualization": "Data flow diagrams, feature histograms",
            "example": "The researcher GIVES data TO the LINEAR MODEL."
        }
    
    @staticmethod
    def genitive() -> Dict[str, str]:
        """
        GENITIVE case: The model as possessor or source of outputs.
        
        In linguistics: Indicates possession or source.
        In regression: The model as a source of predictions or reports.
        """
        return {
            "case": Case.GENITIVE.value,
            "linguistic_meaning": "Possessive or source relation",
            "statistical_role": "Model as prediction source",
            "regression_context": "The model that generates predictions, intervals, and output reports",
            "formula": "ŷ = Model(x), CI = [ŷ-tα/2·s, ŷ+tα/2·s]",
            "primary_methods": "generate_predictions(), get_summary()",
            "visualization": "Prediction intervals, output reports",
            "example": "The predictions BELONG TO the LINEAR MODEL."
        }
    
    @staticmethod
    def instrumental() -> Dict[str, str]:
        """
        INSTRUMENTAL case: The model as tool or method.
        
        In linguistics: Indicates the means by which an action is done.
        In regression: The model as methodology or algorithm.
        """
        return {
            "case": Case.INSTRUMENTAL.value,
            "linguistic_meaning": "Means or instrument of action",
            "statistical_role": "Model as statistical methodology",
            "regression_context": "The specific algorithm (OLS, gradient descent) used to solve the regression problem",
            "formula": "θ = (XᵀX)⁻¹Xᵀy",
            "primary_methods": "calculate_coefficients(), apply_method()",
            "visualization": "Algorithm convergence, computational process",
            "example": "The researcher solves the problem WITH the LINEAR MODEL."
        }
    
    @staticmethod
    def locative() -> Dict[str, str]:
        """
        LOCATIVE case: The model as location or context.
        
        In linguistics: Indicates location or context.
        In regression: The model as statistical context or assumption framework.
        """
        return {
            "case": Case.LOCATIVE.value,
            "linguistic_meaning": "Location or context marker",
            "statistical_role": "Model as assumption framework",
            "regression_context": "The statistical context including assumptions (homoscedasticity, normality) and conditions",
            "formula": "Var(ε|X) = σ², ε ~ N(0,σ²)",
            "primary_methods": "check_assumptions(), provide_context()",
            "visualization": "Residual plots, QQ plots, assumption diagnostics",
            "example": "The pattern exists WITHIN the LINEAR MODEL's assumptions."
        }
    
    @staticmethod
    def ablative() -> Dict[str, str]:
        """
        ABLATIVE case: The model as origin or causal factor.
        
        In linguistics: Indicates movement away from, origin, or cause.
        In regression: The model as source of error or origin of effects.
        """
        return {
            "case": Case.ABLATIVE.value,
            "linguistic_meaning": "Origin, separation, or source",
            "statistical_role": "Model as error origin",
            "regression_context": "Analysis of where errors come from, residual analysis, and source attribution",
            "formula": "ε = y - ŷ, residuals = y - Xθ",
            "primary_methods": "analyze_residuals(), decompose_error()",
            "visualization": "Error decomposition, residual distribution",
            "example": "The prediction errors COME FROM the LINEAR MODEL."
        }
    
    @staticmethod
    def vocative() -> Dict[str, str]:
        """
        VOCATIVE case: The model as addressable interface.
        
        In linguistics: Direct address, calling someone by name.
        In regression: The model as interactive interface or API.
        """
        return {
            "case": Case.VOCATIVE.value,
            "linguistic_meaning": "Direct address or invocation",
            "statistical_role": "Model as interactive interface",
            "regression_context": "The user interface to the regression model, APIs, or interactive diagnostics",
            "formula": "API: predict(X) → ŷ",
            "primary_methods": "expose_interface(), respond_to_query()",
            "visualization": "Interactive dashboards, query-response diagrams",
            "example": "HEY LINEAR MODEL, what is the prediction for this input?"
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

class DataGenerator:
    """Generates synthetic data for linear regression tests with various properties."""
    
    @staticmethod
    def linear_data(
        n_samples: int = 100,
        slope: float = 2.0,
        intercept: float = 5.0,
        noise_level: float = 1.0,
        x_range: Tuple[float, float] = (-5, 5),
        random_seed: Optional[int] = 42
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate synthetic linear data with specified parameters.
        
        Args:
            n_samples: Number of data points to generate
            slope: True slope of the linear relationship
            intercept: True intercept of the linear relationship
            noise_level: Standard deviation of Gaussian noise
            x_range: Range of x values (min, max)
            random_seed: Random seed for reproducibility
            
        Returns:
            Tuple of (X, y) arrays
        """
        if random_seed is not None:
            np.random.seed(random_seed)
            
        # Generate X values
        X = np.random.uniform(x_range[0], x_range[1], size=n_samples)
        
        # Generate y values with noise
        y = slope * X + intercept + np.random.normal(0, noise_level, size=n_samples)
        
        return X.reshape(-1, 1), y
    
    @staticmethod
    def nonlinear_data(
        n_samples: int = 100,
        function_type: str = 'quadratic',
        noise_level: float = 1.0,
        x_range: Tuple[float, float] = (-5, 5),
        random_seed: Optional[int] = 42
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate synthetic nonlinear data.
        
        Args:
            n_samples: Number of data points to generate
            function_type: Type of nonlinear function ('quadratic', 'exponential', 'sinusoidal')
            noise_level: Standard deviation of Gaussian noise
            x_range: Range of x values (min, max)
            random_seed: Random seed for reproducibility
            
        Returns:
            Tuple of (X, y) arrays
        """
        if random_seed is not None:
            np.random.seed(random_seed)
            
        # Generate X values
        X = np.random.uniform(x_range[0], x_range[1], size=n_samples)
        
        # Generate y values based on function type
        if function_type == 'quadratic':
            y = 2 * X**2 + 3 * X + 5
        elif function_type == 'exponential':
            y = np.exp(0.5 * X)
        elif function_type == 'sinusoidal':
            y = 3 * np.sin(X) + 2
        else:
            raise ValueError(f"Unknown function type: {function_type}")
        
        # Add noise
        y += np.random.normal(0, noise_level, size=n_samples)
        
        return X.reshape(-1, 1), y
    
    @staticmethod
    def heteroskedastic_data(
        n_samples: int = 100,
        slope: float = 2.0,
        intercept: float = 5.0,
        base_noise: float = 0.5,
        x_range: Tuple[float, float] = (-5, 5),
        random_seed: Optional[int] = 42
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate data with heteroskedastic noise (variance increases with x).
        
        Args:
            n_samples: Number of data points to generate
            slope: True slope of the linear relationship
            intercept: True intercept of the linear relationship
            base_noise: Base level of noise
            x_range: Range of x values (min, max)
            random_seed: Random seed for reproducibility
            
        Returns:
            Tuple of (X, y) arrays
        """
        if random_seed is not None:
            np.random.seed(random_seed)
            
        # Generate X values
        X = np.random.uniform(x_range[0], x_range[1], size=n_samples)
        
        # Generate noise with increasing variance
        X_normalized = (X - x_range[0]) / (x_range[1] - x_range[0])  # Scale to [0, 1]
        noise = np.random.normal(0, base_noise + 3 * X_normalized, size=n_samples)
        
        # Generate y values with heteroskedastic noise
        y = slope * X + intercept + noise
        
        return X.reshape(-1, 1), y
    
    @staticmethod
    def multivariate_data(
        n_samples: int = 100,
        n_features: int = 3,
        coefficients: Optional[np.ndarray] = None,
        intercept: float = 5.0,
        noise_level: float = 1.0,
        x_range: Tuple[float, float] = (-5, 5),
        correlated: bool = False,
        correlation: float = 0.7,
        random_seed: Optional[int] = 42
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate multivariate linear data.
        
        Args:
            n_samples: Number of data points to generate
            n_features: Number of features
            coefficients: Array of true coefficients for each feature
            intercept: True intercept
            noise_level: Standard deviation of Gaussian noise
            x_range: Range of x values (min, max)
            correlated: Whether to generate correlated features
            correlation: Correlation coefficient between features
            random_seed: Random seed for reproducibility
            
        Returns:
            Tuple of (X, y) arrays
        """
        if random_seed is not None:
            np.random.seed(random_seed)
            
        if coefficients is None:
            coefficients = np.random.uniform(0.5, 3.0, size=n_features)
            
        if correlated:
            # Generate correlated features using covariance matrix
            cov_matrix = np.ones((n_features, n_features)) * correlation
            np.fill_diagonal(cov_matrix, 1.0)
            mean_vector = np.zeros(n_features)
            X = np.random.multivariate_normal(mean_vector, cov_matrix, size=n_samples)
            
            # Scale to desired range
            X = X * (x_range[1] - x_range[0]) / 2 + (x_range[0] + x_range[1]) / 2
        else:
            # Generate independent features
            X = np.random.uniform(x_range[0], x_range[1], size=(n_samples, n_features))
        
        # Generate target values
        y = np.dot(X, coefficients) + intercept + np.random.normal(0, noise_level, size=n_samples)
        
        return X, y

# Helper functions for visualization
class Visualizer:
    """Helper class for creating visualizations of regression models and data."""
    
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
        Plot input data with scatter plot.
        
        Args:
            X: Feature matrix
            y: Target variable
            title: Plot title
            xlabel: X-axis label
            ylabel: Y-axis label
            figsize: Figure size
            save_path: Path to save the figure
            
        Returns:
            Matplotlib figure
        """
        fig, ax = plt.subplots(figsize=figsize)
        
        if X.shape[1] == 1:  # 1D case
            ax.scatter(X.flatten(), y, alpha=0.7, color='blue', edgecolor='k', s=50, label="Data")
            ax.set_xlabel(xlabel)
            ax.set_ylabel(ylabel)
        else:  # Show pairwise relationships for multivariate data
            # Note: Pairplot creates its own figure, so we handle it differently
            df = pd.DataFrame(np.hstack([X, y.reshape(-1, 1)]))
            df.columns = [f"X{i+1}" for i in range(X.shape[1])] + ["y"]
            pair_fig = sns.pairplot(df, diag_kind='kde')
            if save_path:
                pair_fig.savefig(save_path)
                plt.close(pair_fig.fig) # Close the pairplot figure
            return pair_fig.fig # Return the figure generated by pairplot

        ax.set_title(title)
        ax.legend()
        ax.grid(True, linestyle='--', alpha=0.6)
        
        if save_path:
            fig.savefig(save_path)
            logger.info(f"Saved data plot to {save_path}")
            plt.close(fig) # Close the figure after saving
            
        return fig

    @staticmethod
    def plot_regression_fit(
        X: np.ndarray,
        y: np.ndarray,
        model: 'LinearRegressionModel',
        title: str = "Regression Fit",
        xlabel: str = "X",
        ylabel: str = "y",
        figsize: Tuple[int, int] = (10, 6),
        save_path: Optional[str] = None,
        ax: Optional[plt.Axes] = None  # Add optional axes argument
    ) -> Optional[plt.Figure]:
        """
        Plot the data and the regression line fit by the model.
        Can plot onto an existing Axes object if provided.
        
        Args:
            X: Feature matrix (assumes 1D for line plotting)
            y: Target variable
            model: Fitted LinearRegressionModel instance
            title: Plot title
            xlabel: X-axis label
            ylabel: Y-axis label
            figsize: Figure size (used only if ax is None)
            save_path: Path to save the figure (used only if ax is None)
            ax: Existing Matplotlib Axes object to plot onto. If None, create new figure.
            
        Returns:
            Matplotlib figure if a new one was created, otherwise None.
        """
        if X.shape[1] != 1:
            logger.warning("Regression fit plot currently only supports 1D data. Skipping.")
            # Return None if using existing axes, or an empty figure if creating new
            return None if ax else plt.figure()

        create_new_figure = ax is None
        if create_new_figure:
            fig, ax = plt.subplots(figsize=figsize)
        else:
            fig = ax.figure # Get the figure from the existing axes

        ax.scatter(X.flatten(), y, alpha=0.7, color='blue', edgecolor='k', s=50, label="Data")
        
        # Generate points for the regression line
        x_line = np.linspace(X.min(), X.max(), 100).reshape(-1, 1)
        y_line = model.predict(x_line)
        
        ax.plot(x_line.flatten(), y_line, color='red', linewidth=2, label="Regression Fit")
        
        ax.set_title(title)
        ax.set_xlabel(xlabel)
        ax.set_ylabel(ylabel)
        # Only add legend if it doesn't already exist to avoid duplicates on subplots
        if not ax.get_legend():
             ax.legend()
        ax.grid(True, linestyle='--', alpha=0.6)
        
        # Add summary stats to the plot
        try:
            r2 = model.evaluate(X, y)['r2']
            mse = model.evaluate(X, y)['mse']
            # Handle potential absence of parameters if model wasn't fitted properly
            params = model.get_parameters()
            if params and 'coefficients' in params and params['coefficients'] is not None and len(params['coefficients']) > 0:
                 coef = params['coefficients'][0]
            else:
                 coef = np.nan # Or some other placeholder
            if params and 'intercept' in params and params['intercept'] is not None:
                intercept = params['intercept']
            else:
                intercept = np.nan
            stats_text = f"""R²: {r2:.3f}
MSE: {mse:.3f}
Coef: {coef:.3f}
Intercept: {intercept:.3f}"""
            ax.text(0.05, 0.95, stats_text, transform=ax.transAxes, fontsize=9, # Reduced font size
                    verticalalignment='top', bbox=dict(boxstyle='round,pad=0.3', fc='wheat', alpha=0.7))
        except Exception as e:
            logger.warning(f"Could not add stats to plot: {e}")

        if create_new_figure and save_path:
            fig.savefig(save_path)
            logger.info(f"Saved regression fit plot to {save_path}")
            plt.close(fig)
            return None # Return None as figure is closed
            
        return fig if create_new_figure else None

    @staticmethod
    def create_overview_figure(save_path: str, include_linguistics: bool = True) -> None:
        """
        Create an overview figure for all case tests.
        
        Args:
            save_path: Path to save the overview figure
            include_linguistics: Whether to include linguistic context
        """
        logger.info(f"Creating overview figure at {save_path}")
        
        if include_linguistics:
            # 2x4 grid to show all 8 cases with visuals and linguistics
            fig, axes = plt.subplots(4, 2, figsize=(15, 20))
            axes = axes.flatten()  # Flatten the grid for easy indexing
        else:
            # 2x2 grid for simplified view
            fig, axes = plt.subplots(2, 2, figsize=(12, 10))
            axes = axes.flatten()  # Flatten the grid for easy indexing
        
        # Get case definitions
        case_info = CaseDefinitions.get_all_cases()
        
        # Generate plots based on the selected mode
        try:
            # Create sample data for visualization
            X_lin, y_lin = DataGenerator.linear_data(n_samples=100, noise_level=2.0, random_seed=42)
            X_het, y_het = DataGenerator.heteroskedastic_data(n_samples=100, base_noise=0.5, random_seed=55)
            model = LinearRegressionModel(model_id="overview", case=Case.NOMINATIVE).fit(X_lin, y_lin)
            
            if include_linguistics:
                # Create detailed plots for each case
                for i, case in enumerate(Case):
                    ax = axes[i]
                    # Simple visualization for each case
                    ax.text(0.5, 0.9, f"{case.value} Case", ha='center', fontsize=14, fontweight='bold', transform=ax.transAxes)
                    ax.text(0.5, 0.8, case_info[case]['linguistic_meaning'], ha='center', fontsize=12, transform=ax.transAxes)
                    ax.text(0.5, 0.7, f"Statistical role: {case_info[case]['statistical_role']}", ha='center', fontsize=10, transform=ax.transAxes)
                    ax.text(0.5, 0.5, f"Formula: {case_info[case]['formula']}", ha='center', fontsize=10, transform=ax.transAxes)
                    ax.text(0.5, 0.3, f"Example: {case_info[case]['example']}", ha='center', fontsize=10, transform=ax.transAxes)
                    ax.axis('off')
            else:
                # Simplified view with just four cases
                ax_titles = ["NOMINATIVE/INSTRUMENTAL", "ACCUSATIVE/GENITIVE", "LOCATIVE/ABLATIVE", "DATIVE/VOCATIVE"]
                ax_descriptions = [
                    "Model as active agent/method",
                    "Model as evaluated object/source",
                    "Model as context/origin",
                    "Model as recipient/interface"
                ]
                
                for i, (title, desc) in enumerate(zip(ax_titles, ax_descriptions)):
                    ax = axes[i]
                    ax.text(0.5, 0.7, title, ha='center', fontsize=14, fontweight='bold', transform=ax.transAxes)
                    ax.text(0.5, 0.5, desc, ha='center', fontsize=12, transform=ax.transAxes)
                    ax.axis('off')
            
            # Add title
            title = "CEREBRUM Linear Regression: Linguistic Case Roles" if include_linguistics else "CEREBRUM Linear Regression: Simplified Case Overview"
            plt.suptitle(title, fontsize=16, y=0.98)
            
            # Save the figure
            plt.tight_layout(rect=[0, 0, 1, 0.96])  # Adjust for suptitle
            plt.savefig(save_path, dpi=300)
            plt.close(fig)
            logger.info(f"Overview figure saved to {save_path}")
            
        except Exception as e:
            logger.error(f"Error creating overview figure: {e}")
            plt.close(fig)

# --- Linear Regression Model Implementation ---

class LinearRegressionModel(Model):
    """
    Basic Linear Regression model implemented using analytical solution (Normal Equation).
    Adheres to the CEREBRUM Model interface with case-specific behaviors.
    
    Each case represents a different functional role for the linear regression model:
    - NOMINATIVE: Model as active fitting agent (parameter estimator)
    - ACCUSATIVE: Model as subject of evaluation (quality assessment)
    - DATIVE: Model as data recipient (feature processor)
    - GENITIVE: Model as source of predictions (output generator)
    - INSTRUMENTAL: Model as statistical method (algorithm implementation)
    - LOCATIVE: Model as assumption framework (context provider)
    - ABLATIVE: Model as error origin (residual source)
    - VOCATIVE: Model as interactive interface (API endpoint)
    """
    def __init__(self, model_id: str, case: Case, hyperparameters: Optional[Dict[str, Any]] = None):
        # Correctly call the base class __init__ with expected arguments
        # Use model_id as the name for the base Model
        super().__init__(name=model_id) 
        
        # Set the case using the property setter to trigger any case transformations
        self.case = case 
        
        # Store hyperparameters
        self.hyperparameters = hyperparameters or {}
        
        # Model-specific attributes
        self.coefficients_: Optional[np.ndarray] = None # Slope coefficients
        self.intercept_: Optional[float] = None # Intercept term
        self.is_trained: bool = False
        
        # Statistics storage
        self.statistics = {}
        
        # Store reference data for case-specific analyses
        self.X_train = None
        self.y_train = None
        
        # Case-specific attributes
        self._confidence_level = 0.95  # For GENITIVE confidence intervals
        self._data_buffer = []  # For DATIVE data collection
        self._assumption_checks = {}  # For LOCATIVE assumption verification
        self._residuals = None  # For ABLATIVE residual analysis
        self._query_history = []  # For VOCATIVE interaction tracking

    def fit(self, X: np.ndarray, y: np.ndarray) -> 'LinearRegressionModel':
        """
        Fit the linear regression model using the Normal Equation.
        Primary method for NOMINATIVE case (model as active estimator).

        Args:
            X: Feature matrix (n_samples, n_features)
            y: Target variable (n_samples,)

        Returns:
            The fitted model instance.
        """
        logger.info(f"[{self.name} | {self.case.value}] Fitting model...") # Use self.name and self.case.value
        
        # Store reference data
        self.X_train = X.copy()
        self.y_train = y.copy()
        
        # Add intercept term (bias) to X
        X_b = np.c_[np.ones((X.shape[0], 1)), X] # Add x0 = 1 to each instance
        
        # Case-specific behavior
        if self.case == Case.NOMINATIVE:
            logger.info(f"NOMINATIVE case: Model actively estimating parameters from data")
        elif self.case == Case.INSTRUMENTAL:
            logger.info(f"INSTRUMENTAL case: Using Normal Equation as statistical method")
            
        # Calculate parameters using Normal Equation: theta = (X_b^T * X_b)^(-1) * X_b^T * y
        try:
            theta = np.linalg.inv(X_b.T.dot(X_b)).dot(X_b.T).dot(y)
            self.intercept_ = theta[0]
            self.coefficients_ = theta[1:]
            self.is_trained = True
            logger.info(f"Model fitted. Parameters (theta): {theta}")
        except np.linalg.LinAlgError:
            logger.error("Fitting failed: Matrix X_b.T * X_b is singular. Cannot compute inverse.")
            self.is_trained = False
            # Potentially fall back to gradient descent or raise error
            raise RuntimeError("Linear Regression fitting failed due to singular matrix.")
            
        # Analyze residuals for ABLATIVE case
        if self.case == Case.ABLATIVE:
            self._analyze_residuals()
            
        # Check assumptions for LOCATIVE case
        if self.case == Case.LOCATIVE:
            self._check_assumptions()
            
        return self

    def predict(self, X: np.ndarray) -> np.ndarray:
        """
        Make predictions using the trained linear regression model.
        Primary method for GENITIVE case (model as prediction source).

        Args:
            X: Feature matrix (n_samples, n_features)

        Returns:
            Predicted values (n_samples,)
        """
        if not self.is_trained or self.coefficients_ is None or self.intercept_ is None:
            raise RuntimeError("Model must be trained before making predictions.")
            
        # For VOCATIVE case: log the query
        if self.case == Case.VOCATIVE:
            self._query_history.append({"type": "predict", "timestamp": pd.Timestamp.now(), "data_shape": X.shape})
            logger.info(f"VOCATIVE case: Interface received prediction query for {X.shape[0]} samples")
            
        # For GENITIVE case: note the generation of predictions
        if self.case == Case.GENITIVE:
            logger.info(f"GENITIVE case: Model generating predictions as outputs for {X.shape[0]} samples")

        # Make predictions: y_pred = intercept + X · coefficients
        y_pred = self.intercept_ + X.dot(self.coefficients_)
        
        return y_pred

    def get_parameters(self) -> Optional[Dict[str, Any]]:
        """
        Return the learned parameters.
        Relevant for GENITIVE case (model as source of information).
        """
        if self.coefficients_ is not None and self.intercept_ is not None:
            params = {
                'intercept': self.intercept_,
                'coefficients': self.coefficients_
            }
            
            # For VOCATIVE case: log parameter access
            if self.case == Case.VOCATIVE:
                self._query_history.append({"type": "get_parameters", "timestamp": pd.Timestamp.now()})
                logger.info("VOCATIVE case: Interface provided model parameters upon request")
                
            return params
        return None

    def evaluate(self, X: np.ndarray, y: np.ndarray, fitted_params: Optional[Dict[str, Any]] = None) -> Dict[str, float]:
        """
        Evaluate the model performance.
        Primary method for ACCUSATIVE case (model as evaluation subject).

        Args:
            X: Feature matrix
            y: True target values
            fitted_params: Optional parameters (useful when passing parameters between models)

        Returns:
            Dictionary containing evaluation metrics (R2, MSE, MAE).
        """
        # Apply provided parameters if given (for pure ACCUSATIVE case evaluation)
        if fitted_params is not None and self.case == Case.ACCUSATIVE:
            logger.info("ACCUSATIVE case: Evaluating with provided parameters rather than internal state")
            self.intercept_ = fitted_params['intercept']
            self.coefficients_ = fitted_params['coefficients']
            self.is_trained = True
        
        if not self.is_trained:
            raise RuntimeError("Model must be trained before evaluation.")
            
        y_pred = self.predict(X)
        
        metrics = {
            'r2': r2_score(y, y_pred),
            'mse': mean_squared_error(y, y_pred),
            'mae': mean_absolute_error(y, y_pred)
        }
        
        # Add more detailed statistics for ACCUSATIVE case
        if self.case == Case.ACCUSATIVE:
            logger.info(f"ACCUSATIVE case: Model undergoing formal evaluation")
            
            # Calculate additional statistical metrics
            n = len(y)
            p = X.shape[1]  # Number of predictors
            
            # Detailed metrics
            residuals = y - y_pred
            sse = np.sum(residuals**2)
            sst = np.sum((y - np.mean(y))**2)
            
            # Degrees of freedom
            df_total = n - 1
            df_residual = n - p - 1
            df_regression = p
            
            # Mean squares
            mse_model = (sst - sse) / df_regression if df_regression > 0 else np.nan
            mse_residual = sse / df_residual if df_residual > 0 else np.nan
            
            # F-statistic
            f_stat = mse_model / mse_residual if mse_residual > 0 else np.nan
            
            # Store these in the metrics dictionary
            metrics.update({
                'sse': sse,
                'sst': sst,
                'df_total': df_total,
                'df_residual': df_residual,
                'df_regression': df_regression,
                'mse_model': mse_model,
                'mse_residual': mse_residual,
                'f_statistic': f_stat
            })
            
            # Store in model's statistics attribute
            self.statistics = metrics
            
            logger.info(f"ACCUSATIVE evaluation: R² = {metrics['r2']:.4f}, F = {f_stat:.4f}")
        
        return metrics

    def get_summary(self) -> str:
        """
        Generate a text summary of the model.
        Primary method for GENITIVE case (model as source of information).
        """
        if not self.is_trained or self.coefficients_ is None or self.intercept_ is None:
            return "Model is not trained."
            
        # If in GENITIVE case, provide more detailed output
        if self.case == Case.GENITIVE:
            logger.info("GENITIVE case: Model generating comprehensive summary report")
            
            summary = f"--- Linear Regression Model Summary [{self.name} | {self.case.value}] ---\n"
            summary += f"Status: Trained\n\n"
            
            # Parameters section
            summary += f"MODEL PARAMETERS:\n"
            summary += f"  Intercept (β₀): {self.intercept_:.6f}\n"
            for i, coef in enumerate(self.coefficients_):
                summary += f"  Coefficient β{i+1}: {coef:.6f}\n"
            
            # If we have training data stored, add more statistics
            if self.X_train is not None and self.y_train is not None:
                # Calculate key statistics if not already available
                if not self.statistics:
                    self.statistics = self.evaluate(self.X_train, self.y_train)
                    
                summary += f"\nMODEL FIT STATISTICS:\n"
                summary += f"  R-squared: {self.statistics.get('r2', 'N/A'):.6f}\n"
                summary += f"  Mean Squared Error: {self.statistics.get('mse', 'N/A'):.6f}\n"
                summary += f"  Mean Absolute Error: {self.statistics.get('mae', 'N/A'):.6f}\n"
                
                if 'f_statistic' in self.statistics:
                    summary += f"  F-statistic: {self.statistics.get('f_statistic', 'N/A'):.6f}\n"
                
                # Sample size info
                summary += f"\nDATA INFORMATION:\n"
                summary += f"  Sample size: {self.X_train.shape[0]}\n"
                summary += f"  Number of features: {self.X_train.shape[1]}\n"
                
                # Formula representation
                summary += f"\nMODEL FORMULA:\n"
                formula = f"ŷ = {self.intercept_:.4f}"
                for i, coef in enumerate(self.coefficients_):
                    formula += f" + {coef:.4f}·X{i+1}" if coef >= 0 else f" - {abs(coef):.4f}·X{i+1}"
                summary += f"  {formula}\n"
        else:
            # Basic summary for other cases
            summary = f"--- Linear Regression Model Summary [{self.name} | {self.case.value}] ---\n"
            summary += f"Status: {'Trained' if self.is_trained else 'Untrained'}\n"
            
            if self.is_trained:
                params = self.get_parameters()
                if params:
                    summary += f"Intercept: {params['intercept']:.4f}\n"
                    for i, coef in enumerate(params['coefficients']):
                        summary += f"Coefficient {i+1}: {coef:.4f}\n"
            
            # Add hyperparameters if available
            if self.hyperparameters:
                summary += f"Hyperparameters: {json.dumps(self.hyperparameters, indent=2)}\n"
                
        summary += "-----------------------------------------------------------\n"
        
        # For VOCATIVE case: log the summary request
        if self.case == Case.VOCATIVE:
            self._query_history.append({"type": "get_summary", "timestamp": pd.Timestamp.now()})
            logger.info("VOCATIVE case: Interface provided model summary upon request")
        
        return summary
        
    # --- Case-specific methods ---
    
    def receive_data(self, X: np.ndarray, y: Optional[np.ndarray] = None) -> 'LinearRegressionModel':
        """
        Receive and process incoming data.
        Primary method for DATIVE case (model as data recipient).
        
        Args:
            X: Feature matrix
            y: Optional target variable
            
        Returns:
            The model instance for chaining
        """
        if self.case != Case.DATIVE:
            logger.warning(f"receive_data() is primarily for DATIVE case, but model is in {self.case.value} case")
        
        # For DATIVE case: store the received data in buffer
        data_entry = {"X": X, "timestamp": pd.Timestamp.now()}
        if y is not None:
            data_entry["y"] = y
            
        self._data_buffer.append(data_entry)
        
        logger.info(f"DATIVE case: Model received data chunk of shape X:{X.shape}" + 
                   (f", y:{y.shape}" if y is not None else ""))
        
        return self
        
    def process_data_buffer(self) -> Tuple[np.ndarray, Optional[np.ndarray]]:
        """
        Process collected data from the buffer.
        Related to DATIVE case (model as data recipient).
        
        Returns:
            Tuple of (X, y) with processed data
        """
        if not self._data_buffer:
            logger.warning("No data in buffer to process")
            return None, None
            
        # Extract and concatenate X values
        X_parts = [entry["X"] for entry in self._data_buffer]
        X_combined = np.vstack(X_parts)
        
        # Check if we have y values and combine them if available
        if all("y" in entry for entry in self._data_buffer):
            y_parts = [entry["y"] for entry in self._data_buffer]
            y_combined = np.concatenate(y_parts)
        else:
            y_combined = None
            
        logger.info(f"DATIVE case: Processed data buffer with {len(self._data_buffer)} entries " +
                   f"into combined data of shape X:{X_combined.shape}" +
                   (f", y:{y_combined.shape}" if y_combined is not None else ""))
        
        # Clear the buffer after processing
        self._data_buffer = []
        
        return X_combined, y_combined
    
    def generate_confidence_intervals(self, X: np.ndarray, confidence_level: Optional[float] = None) -> Dict[str, np.ndarray]:
        """
        Generate prediction intervals for given X values.
        Primary method for GENITIVE case (model as source of predictions).
        
        Args:
            X: Feature matrix for predictions
            confidence_level: Confidence level (0-1), defaults to instance value
            
        Returns:
            Dictionary with predictions and confidence intervals
        """
        if not self.is_trained:
            raise RuntimeError("Model must be trained before generating intervals")
            
        if self.X_train is None or self.y_train is None:
            raise RuntimeError("Training data not available for confidence interval calculation")
            
        # Use provided confidence level or default
        conf_level = confidence_level or self._confidence_level
        
        # Make predictions
        y_pred = self.predict(X)
        
        # Calculate residual standard error from training data
        y_train_pred = self.predict(self.X_train)
        residuals = self.y_train - y_train_pred
        n = len(self.y_train)
        p = self.X_train.shape[1]  # Number of predictors
        
        # Residual standard error
        rse = np.sqrt(np.sum(residuals**2) / (n - p - 1))
        
        # Critical value for the confidence level
        alpha = 1 - conf_level
        t_critical = stats.t.ppf(1 - alpha/2, n - p - 1)
        
        # Standard error for predictions (simplified approach)
        # For a more accurate approach, we would need to compute the full prediction variance
        # which includes the variance of the parameter estimates
        prediction_se = rse * np.ones_like(y_pred)
        
        # Confidence interval
        lower_bound = y_pred - t_critical * prediction_se
        upper_bound = y_pred + t_critical * prediction_se
        
        if self.case == Case.GENITIVE:
            logger.info(f"GENITIVE case: Generated {conf_level*100:.1f}% confidence intervals for {len(X)} predictions")
        
        return {
            "predictions": y_pred,
            "lower_bound": lower_bound,
            "upper_bound": upper_bound,
            "confidence_level": conf_level
        }
    
    def _check_assumptions(self) -> Dict[str, bool]:
        """
        Check regression assumptions on the training data.
        Primary method for LOCATIVE case (model as context/assumption framework).
        
        Returns:
            Dictionary of assumption check results
        """
        if not self.is_trained or self.X_train is None or self.y_train is None:
            logger.warning("Cannot check assumptions without fitted model and training data")
            return {}
            
        logger.info("LOCATIVE case: Analyzing regression assumptions and statistical context")
        
        # Get predictions and residuals
        y_pred = self.predict(self.X_train)
        residuals = self.y_train - y_pred
        
        # 1. Linearity check - correlation between predicted values and residuals
        # Should be close to zero for linearity
        linearity_corr = np.corrcoef(y_pred, residuals)[0, 1]
        linearity_ok = abs(linearity_corr) < 0.3  # Simple threshold check
        
        # 2. Independence - Durbin-Watson test (simplified)
        # Values close to 2 indicate no autocorrelation
        diff_residuals = np.diff(residuals)
        durbin_watson = np.sum(diff_residuals**2) / np.sum(residuals**2)
        independence_ok = 1.5 < durbin_watson < 2.5  # Simple range check
        
        # 3. Normality check - Shapiro-Wilk test
        try:
            # Sample for large datasets
            sample_size = min(len(residuals), 5000)
            sample_indices = np.random.choice(len(residuals), sample_size, replace=False)
            sample_residuals = residuals[sample_indices]
            
            shapiro_stat, shapiro_p = stats.shapiro(sample_residuals)
            normality_ok = shapiro_p > 0.05  # Null hypothesis: data is normally distributed
        except:
            # Fallback if the test fails
            normality_ok = None
            
        # 4. Homoscedasticity - Breusch-Pagan test (simplified)
        # Regress squared residuals on predicted values
        X_bp = np.c_[np.ones(len(y_pred)), y_pred]
        squared_residuals = residuals**2
        try:
            bp_params = np.linalg.inv(X_bp.T.dot(X_bp)).dot(X_bp.T).dot(squared_residuals)
            bp_slope = bp_params[1]
            homoscedasticity_ok = abs(bp_slope) < 0.3  # Simple threshold check
        except:
            homoscedasticity_ok = None
            
        # Store the results
        self._assumption_checks = {
            "linearity": linearity_ok,
            "independence": independence_ok,
            "normality": normality_ok,
            "homoscedasticity": homoscedasticity_ok
        }
        
        logger.info(f"LOCATIVE case assumption checks: {self._assumption_checks}")
        
        return self._assumption_checks
        
    def _analyze_residuals(self) -> Dict[str, Any]:
        """
        Analyze residuals to determine error sources.
        Primary method for ABLATIVE case (model as error origin).
        
        Returns:
            Dictionary with residual analysis results
        """
        if not self.is_trained or self.X_train is None or self.y_train is None:
            logger.warning("Cannot analyze residuals without fitted model and training data")
            return {}
            
        logger.info("ABLATIVE case: Analyzing residuals as sources of model error")
        
        # Get predictions and residuals
        y_pred = self.predict(self.X_train)
        residuals = self.y_train - y_pred
        self._residuals = residuals
        
        # Basic statistics
        mean_residual = np.mean(residuals)
        std_residual = np.std(residuals)
        abs_residuals = np.abs(residuals)
        
        # Identify potential outliers
        outlier_threshold = 2.5 * std_residual
        outlier_indices = np.where(abs_residuals > outlier_threshold)[0]
        
        # Analyze residual pattern
        # Check correlation between each feature and residuals
        feature_residual_corr = {}
        for i in range(self.X_train.shape[1]):
            feature = self.X_train[:, i]
            corr = np.corrcoef(feature, residuals)[0, 1]
            feature_residual_corr[f'X{i+1}'] = corr
            
        # Find features that may be missing or misspecified
        problem_features = {k: v for k, v in feature_residual_corr.items() if abs(v) > 0.3}
        
        # Compile results 
        residual_analysis = {
            "mean_residual": mean_residual,
            "std_residual": std_residual,
            "n_outliers": len(outlier_indices),
            "outlier_indices": outlier_indices,
            "feature_residual_correlations": feature_residual_corr,
            "potential_misspecification": len(problem_features) > 0,
            "problem_features": problem_features
        }
        
        logger.info(f"ABLATIVE case residual analysis: Mean={mean_residual:.4f}, StdDev={std_residual:.4f}, "
                  f"Outliers={len(outlier_indices)}")
                  
        if problem_features:
            logger.info(f"ABLATIVE case identified potential misspecification in features: {list(problem_features.keys())}")
        
        return residual_analysis
        
    def respond_to_query(self, query_type: str, **kwargs) -> Dict[str, Any]:
        """
        Respond to user queries about the model.
        Primary method for VOCATIVE case (model as interactive interface).
        
        Args:
            query_type: Type of query ("predict", "parameters", "summary", "evaluate")
            **kwargs: Query-specific parameters
            
        Returns:
            Response dictionary
        """
        if self.case != Case.VOCATIVE:
            logger.warning(f"respond_to_query() is for VOCATIVE case, but model is in {self.case.value} case")
            
        # Log the query
        query_record = {
            "type": query_type,
            "timestamp": pd.Timestamp.now(),
            "params": kwargs
        }
        self._query_history.append(query_record)
        
        logger.info(f"VOCATIVE case: Interface received '{query_type}' query")
        
        # Process based on query type
        if query_type == "predict" and "X" in kwargs:
            X = kwargs["X"]
            response = {"predictions": self.predict(X)}
            
        elif query_type == "parameters":
            response = {"parameters": self.get_parameters()}
            
        elif query_type == "summary":
            response = {"summary": self.get_summary()}
            
        elif query_type == "evaluate" and "X" in kwargs and "y" in kwargs:
            X, y = kwargs["X"], kwargs["y"]
            response = {"evaluation": self.evaluate(X, y)}
            
        else:
            response = {
                "error": "Invalid query",
                "message": f"Query type '{query_type}' not recognized or missing required parameters"
            }
            
        # Add interface metadata
        response["model_id"] = self.name
        response["timestamp"] = pd.Timestamp.now().isoformat()
        response["query_count"] = len(self._query_history)
        
        return response

# --- Case-specific tests --- 

def test_nominative_case(linear_test_data, case_definitions):
    """
    Test NOMINATIVE case: The model as active agent fitting the data.
    
    In linguistics: The subject of the sentence performing the action.
    In regression: The model actively estimating parameters from data.
    """
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
    
    # Get test data
    X, y = linear_test_data
    
    # 1. Create a NOMINATIVE case model
    model = LinearRegressionModel(model_id=f"{Case.NOMINATIVE.value}_model", case=Case.NOMINATIVE)
    
    # 2. Fit the model to demonstrate NOMINATIVE action (model as parameter estimator)
    logger.info(f"NOMINATIVE case demonstration: Model actively fitting parameters to data")
    model.fit(X, y)
    
    # 3. Verify the model has learned parameters
    params = model.get_parameters()
    assert params is not None, f"NOMINATIVE model failed to learn parameters"
    assert 'intercept' in params, "Intercept missing from model parameters"
    assert 'coefficients' in params, "Coefficients missing from model parameters"
    
    logger.info(f"NOMINATIVE model fitted with parameters: intercept={params['intercept']:.4f}, " +
               f"coefficients={[f'{c:.4f}' for c in params['coefficients']]}")
    
    # 4. Visualize the NOMINATIVE acting on data (fitting)
    fit_vis_path = os.path.join(case_dir, "fitting_visualization.png")
    Visualizer.plot_regression_fit(
        X, y, model, 
        title=f"NOMINATIVE Case: Model Actively Fitting Data",
        save_path=fit_vis_path
    )
    assert os.path.exists(fit_vis_path), f"NOMINATIVE fit visualization not created"
    
    # 5. Create an animation showing the fitting process (model as active agent)
    anim_path = os.path.join(case_dir, "fitting_animation.gif")
    
    # Create data for the animation - simulate gradient descent
    # Starting with random parameters and gradually approaching the optimal ones
    intercept_start = 0.0  # Start from zero
    coefficient_start = 0.0  # Start from zero
    
    # Number of steps for animation
    n_steps = 30
    
    # Calculate parameter changes per step
    intercept_step = params['intercept'] / n_steps
    coefficient_step = params['coefficients'][0] / n_steps

    # Create figure for animation
    fig, ax = plt.subplots(figsize=(10, 6))

    # Plot data points (won't change during animation)
    ax.scatter(X.flatten(), y, alpha=0.7, color='blue', edgecolor='k', s=50, label="Data")
    
    # Initial line
    line, = ax.plot([], [], 'r-', linewidth=2, label="Regression Line")
    
    # Text for parameter display
    param_text = ax.text(0.05, 0.95, '', transform=ax.transAxes, fontsize=10,
                        verticalalignment='top', bbox=dict(boxstyle='round,pad=0.3', fc='wheat', alpha=0.7))
    
    # Text for iteration count
    iter_text = ax.text(0.95, 0.05, '', transform=ax.transAxes, fontsize=10,
                       horizontalalignment='right', verticalalignment='bottom')
    
    # Text for MSE value
    mse_text = ax.text(0.95, 0.95, '', transform=ax.transAxes, fontsize=10,
                      horizontalalignment='right', verticalalignment='top', 
                      bbox=dict(boxstyle='round,pad=0.3', fc='lightcoral', alpha=0.7))
    
    # Setup plot
    ax.set_title("NOMINATIVE Case: Model Actively Fitting Data")
    ax.set_xlabel("X")
    ax.set_ylabel("y")
    ax.grid(True, linestyle='--', alpha=0.6)
    ax.legend()
    
    # Set axis limits slightly beyond data
    x_margin = (X.max() - X.min()) * 0.1
    y_margin = (y.max() - y.min()) * 0.1
    ax.set_xlim(X.min() - x_margin, X.max() + x_margin)
    ax.set_ylim(y.min() - y_margin, y.max() + y_margin)
    
    # Animation initialization function
    def init():
        line.set_data([], [])
        param_text.set_text('')
        iter_text.set_text('')
        mse_text.set_text('')
        return line, param_text, iter_text, mse_text
    
    # Animation update function
    def update(frame):
        # Current parameters
        current_intercept = intercept_start + intercept_step * frame
        current_coefficient = coefficient_start + coefficient_step * frame
        
        # Generate line points
        x_line = np.linspace(X.min(), X.max(), 100)
        y_line = current_intercept + current_coefficient * x_line
        
        # Update line
        line.set_data(x_line, y_line)
        
        # Calculate current MSE
        y_pred = current_intercept + current_coefficient * X.flatten()
        mse = np.mean((y - y_pred) ** 2)
        
        # Update texts
        param_text.set_text(f"y = {current_intercept:.4f} + {current_coefficient:.4f}·x")
        iter_text.set_text(f"Iteration: {frame+1}/{n_steps}")
        mse_text.set_text(f"MSE: {mse:.4f}")
        
        return line, param_text, iter_text, mse_text
    
    # Create animation
    anim = FuncAnimation(fig, update, frames=n_steps, init_func=init, blit=True, interval=200)
    
    # Save animation as GIF
    anim.save(anim_path, writer='pillow', fps=5, dpi=100)
    plt.close(fig)
    
    assert os.path.exists(anim_path), "NOMINATIVE fitting animation not created"
    
    # 6. Create a more detailed visualization of the parameter space (NOM as explorer)
    param_space_path = os.path.join(case_dir, "parameter_space_visualization.png")
    
    # Create a grid of possible intercept and coefficient values
    intercept_range = np.linspace(params['intercept'] - 5, params['intercept'] + 5, 50)
    coef_range = np.linspace(params['coefficients'][0] - 3, params['coefficients'][0] + 3, 50)
    intercept_grid, coef_grid = np.meshgrid(intercept_range, coef_range)
    
    # Calculate MSE for each parameter combination
    mse_grid = np.zeros_like(intercept_grid)
    for i in range(len(intercept_range)):
        for j in range(len(coef_range)):
            y_pred = intercept_grid[j, i] + coef_grid[j, i] * X.flatten()
            mse_grid[j, i] = np.mean((y - y_pred) ** 2)
    
    # Plot the parameter space
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Create contour plot
    contour = ax.contourf(intercept_grid, coef_grid, mse_grid, 50, cmap='viridis')
    
    # Mark the optimal parameters
    ax.plot(params['intercept'], params['coefficients'][0], 'ro', markersize=10, label='Optimal Parameters')
    
    # Annotations
    ax.set_title("NOMINATIVE Case: Model Exploring Parameter Space")
    ax.set_xlabel("Intercept")
    ax.set_ylabel("Coefficient")
    ax.grid(True, linestyle='--', alpha=0.3)
    
    # Add colorbar
    cbar = fig.colorbar(contour, ax=ax)
    cbar.set_label('Mean Squared Error')
    
    # Add legend and text annotation
    ax.legend()
    ax.text(0.05, 0.95, f"Optimal Parameters:\nIntercept = {params['intercept']:.4f}\nCoefficient = {params['coefficients'][0]:.4f}", 
            transform=ax.transAxes, fontsize=10, verticalalignment='top', 
            bbox=dict(boxstyle='round,pad=0.5', fc='white', alpha=0.8))
    
    fig.tight_layout()
    fig.savefig(param_space_path, dpi=300)
    plt.close(fig)
    
    assert os.path.exists(param_space_path), "NOMINATIVE parameter space visualization not created"
    
    # 7. Add the linguistic formula representation
    formula_vis_path = os.path.join(case_dir, "formula_visualization.png")
    
    # Create formula visualization
    fig, ax = plt.subplots(figsize=(8, 4))
    ax.axis('off')
    
    # Formula with estimated parameters
    formula = r"$\hat{y} = "
    formula += f"{params['intercept']:.4f}"
    for i, coef in enumerate(params['coefficients']):
        if coef >= 0:
            formula += f" + {coef:.4f} \cdot x_{{{i+1}}}"
        else:
            formula += f" - {abs(coef):.4f} \cdot x_{{{i+1}}}"
    formula += "$"
    
    ax.text(0.5, 0.6, formula, ha='center', va='center', fontsize=18, 
            bbox=dict(boxstyle='round,pad=0.8', facecolor='white', alpha=0.8))
    
    ax.text(0.5, 0.2, "NOMINATIVE Case: Model as Active Parameter Estimator", 
            ha='center', va='center', fontsize=14, fontstyle='italic')
    
    fig.tight_layout()
    fig.savefig(formula_vis_path, dpi=300, bbox_inches='tight')
    plt.close(fig)
    
    # 8. Test prediction as an active function (still NOMINATIVE aspect)
    y_pred = model.predict(X)
    assert len(y_pred) == len(y), f"NOMINATIVE model failed to make correct number of predictions"
    
    # 9. Log evaluation metrics to demonstrate the model's action
    metrics = model.evaluate(X, y)
    logger.info(f"NOMINATIVE model evaluation on training data: R² = {metrics['r2']:.4f}")
    
    # 10. Document all outputs for this case
    with open(os.path.join(case_dir, "nominative_results.txt"), 'w') as f:
        f.write(f"NOMINATIVE CASE RESULTS\n")
        f.write(f"======================\n\n")
        f.write(f"Linguistic meaning: {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n")
        f.write(f"Regression context: {case_info['regression_context']}\n\n")
        
        f.write(f"Fitted Parameters:\n")
        f.write(f"- Intercept: {params['intercept']:.6f}\n")
        for i, coef in enumerate(params['coefficients']):
            f.write(f"- Coefficient {i+1}: {coef:.6f}\n")
        
        f.write(f"\nPerformance Metrics:\n")
        for name, value in metrics.items():
            f.write(f"- {name}: {value:.6f}\n")
            
        f.write(f"\nLinguistic Formula Example:\n")
        f.write(f"{case_info['example']}\n\n")
        
        f.write(f"\nVisualizations Generated:\n")
        f.write(f"- Linguistic context: linguistic_context.png\n")
        f.write(f"- Model fitting: fitting_visualization.png\n")
        f.write(f"- Parameter space exploration: parameter_space_visualization.png\n")
        f.write(f"- Fitting animation: fitting_animation.gif\n")
        f.write(f"- Formula representation: formula_visualization.png\n")
            
    logger.info(f"Completed NOMINATIVE case test with visualizations in {case_dir}")
    
    return model  # Return the model for potential use by other tests

def test_accusative_case(linear_test_data, case_definitions):
    """
    Test ACCUSATIVE case: The model as object of evaluation.
    
    In linguistics: The direct object receiving the action of the verb.
    In regression: The model being evaluated, tested, or validated.
    """
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
    
    # Get test data
    X, y = linear_test_data
    
    # 1. First, we need a trained model to evaluate
    # Create and fit a NOMINATIVE model to use as reference
    reference_model = LinearRegressionModel(model_id="reference_for_acc", case=Case.NOMINATIVE)
    reference_model.fit(X, y)
    reference_params = reference_model.get_parameters()
    
    # 2. Create ACCUSATIVE case model for evaluation 
    evaluator_model = LinearRegressionModel(model_id=f"{Case.ACCUSATIVE.value}_model", case=Case.ACCUSATIVE)
    
    # 3. Use the ACCUSATIVE model to evaluate the fitted parameters
    # This demonstrates the model receiving the action of evaluation
    logger.info(f"ACCUSATIVE case demonstration: Model undergoing evaluation")
    
    # We pass the parameters from the reference model to show the ACCUSATIVE model
    # is the object receiving evaluation
    metrics = evaluator_model.evaluate(X, y, fitted_params=reference_params)
    
    assert 'r2' in metrics, "R-squared missing from evaluation metrics"
    assert 'mse' in metrics, "MSE missing from evaluation metrics"
    assert 'f_statistic' in metrics, "F-statistic missing from evaluation metrics"
    
    logger.info(f"ACCUSATIVE model evaluation: R² = {metrics['r2']:.4f}, " +
               f"F = {metrics['f_statistic']:.4f}, MSE = {metrics['mse']:.4f}")
    
    # 4. Create visualization of the evaluation results (model as object of analysis)
    eval_vis_path = os.path.join(case_dir, "evaluation_visualization.png")
    
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))
    
    # Plot the data and fitted line
    ax1.scatter(X.flatten(), y, alpha=0.6, color='blue', s=40, label="Data")
    x_line = np.linspace(X.min(), X.max(), 100).reshape(-1, 1)
    y_line = evaluator_model.predict(x_line)
    ax1.plot(x_line.flatten(), y_line, color='red', linewidth=2, label="Fitted Line")
    ax1.set_title("Model Under Evaluation")
    ax1.set_xlabel("X")
    ax1.set_ylabel("y")
    ax1.legend()
    ax1.grid(True, alpha=0.3)
    
    # Plot the fit statistics
    eval_stats = [
        f"R² = {metrics['r2']:.4f}",
        f"MSE = {metrics['mse']:.4f}",
        f"MAE = {metrics['mae']:.4f}",
        f"F = {metrics['f_statistic']:.4f}"
    ]
    
    colors = ['#2ecc71', '#3498db', '#f1c40f', '#e74c3c']
    
    # Create bar chart of metrics
    bar_width = 0.5
    bar_positions = np.arange(len(eval_stats))
    metric_values = [metrics['r2'], metrics['mse'], metrics['mae'], metrics['f_statistic']]
    
    # Normalize for display (dividing by max to get relative scale)
    normalized_values = [v / max(metric_values) for v in metric_values]
    
    ax2.barh(bar_positions, normalized_values, bar_width, color=colors)
    ax2.set_yticks(bar_positions)
    ax2.set_yticklabels(eval_stats)
    ax2.set_title("ACCUSATIVE Case: Model Evaluation")
    ax2.set_xlabel("Normalized Metric Value")
    ax2.grid(True, axis='x', alpha=0.3)
    
    fig.tight_layout()
    fig.savefig(eval_vis_path, dpi=300)
    plt.close(fig)
    
    assert os.path.exists(eval_vis_path), "ACCUSATIVE evaluation visualization not created"
    
    # 5. Create visualization of hypothesis testing (another ACCUSATIVE aspect)
    hypothesis_vis_path = os.path.join(case_dir, "hypothesis_testing.png")
    
    fig, ax = plt.subplots(figsize=(10, 6))
    
    # Simple hypothesis test visualization (F-test)
    f_stat = metrics['f_statistic']
    dfn = metrics['df_regression'] 
    dfd = metrics['df_residual']
    
    # Create F-distribution
    x = np.linspace(0, 15, 1000)
    y = stats.f.pdf(x, dfn, dfd)
    
    # Plot F distribution
    ax.plot(x, y, 'b-', lw=2, label='F distribution')
    
    # Plot the critical F value
    f_crit = stats.f.ppf(0.95, dfn, dfd)
    ax.axvline(f_crit, color='r', linestyle='--', label=f'Critical F (α=0.05): {f_crit:.4f}')
    
    # Plot the observed F value
    ax.axvline(f_stat, color='g', linestyle='-', label=f'Observed F: {f_stat:.4f}')
    
    # Shade rejection region
    idx = x >= f_crit
    ax.fill_between(x[idx], y[idx], color='r', alpha=0.3)
    
    # Shade region to the left of observed value
    idx = (x >= 0) & (x <= f_stat)
    ax.fill_between(x[idx], y[idx], color='g', alpha=0.3)
    
    ax.set_title("ACCUSATIVE Case: Hypothesis Test for Model Significance")
    ax.set_xlabel("F Statistic")
    ax.set_ylabel("Probability Density")
    ax.legend()
    ax.grid(True, alpha=0.3)
    
    text = f"Null hypothesis: All coefficients are zero\n"
    text += f"F({dfn}, {dfd}) = {f_stat:.4f}, p < 0.05"
    if f_stat > f_crit:
        text += "\nReject null hypothesis"
    else:
        text += "\nFail to reject null hypothesis"
    
    ax.text(0.05, 0.95, text, transform=ax.transAxes, fontsize=10,
            verticalalignment='top', bbox=dict(boxstyle='round,pad=0.5', facecolor='white', alpha=0.7))
    
    fig.tight_layout()
    fig.savefig(hypothesis_vis_path, dpi=300)
    plt.close(fig)
    
    assert os.path.exists(hypothesis_vis_path), "ACCUSATIVE hypothesis test visualization not created"
    
    # 6. Create animation showing the evaluation process
    evaluation_anim_path = os.path.join(case_dir, "evaluation_animation.gif")
    
    # Create evaluation animation figure
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 7))
    
    # Setup plot for animation
    ax1.set_title("Model Evaluation: Residual Analysis")
    ax1.set_xlabel("X")
    ax1.set_ylabel("Residual")
    ax1.grid(True, alpha=0.3)
    
    # Add points for data
    points = ax1.scatter([], [], color='blue', alpha=0.6, s=40)
    
    # Add line for zero residual
    ax1.axhline(y=0, color='gray', linestyle='--', alpha=0.7)
    
    # Add error bars for residuals
    error_lines = []
    for i in range(len(X)):
        line = ax1.plot([], [], 'r-', alpha=0.3)[0]
        error_lines.append(line)
    
    # Setup diagnostic plot for the right side
    ax2.set_title("Evaluation Metrics")
    ax2.axis('off')
    
    # Add text elements for statistics
    stats_text = ax2.text(0.1, 0.85, "", transform=ax2.transAxes, fontsize=12,
                        verticalalignment='top', 
                        bbox=dict(boxstyle='round,pad=0.5', facecolor='white', alpha=0.9))
    
    # Add progressively revealing F-distribution for hypothesis testing
    f_stat = metrics['f_statistic']
    dfn = metrics['df_regression']
    dfd = metrics['df_residual']
    f_crit = stats.f.ppf(0.95, dfn, dfd)
    
    # Create bins for the F-distribution
    x_f = np.linspace(0, max(15, f_stat * 1.2), 1000)
    y_f = stats.f.pdf(x_f, dfn, dfd)
    
    # Create horizontal bar chart for metrics
    metric_names = ['R²', 'MSE', 'MAE', 'F']
    metric_values = [0, 0, 0, 0]  # Initialize with zeros
    bars = ax2.barh(metric_names, metric_values, color=['#2ecc71', '#3498db', '#f1c40f', '#e74c3c'], alpha=0.7)
    ax2.set_xlim(0, 1.1)  # Set limit for normalized metrics
    
    # Add F-distribution subplot
    f_dist_ax = fig.add_axes([0.65, 0.2, 0.3, 0.2])
    f_dist_ax.set_title("F-Statistic Distribution")
    f_dist_ax.set_xlabel("F Value")
    f_dist_ax.plot(x_f, y_f, 'b-', alpha=0.5)
    f_dist_ax.axvline(f_crit, color='r', linestyle='--', alpha=0.7, label=f'Critical F: {f_crit:.2f}')
    
    # Add elements for F-test visualization
    f_value_line = f_dist_ax.axvline(-1, color='g', linestyle='-', alpha=0.0)
    f_rejection_region = f_dist_ax.fill_between(x_f[x_f >= f_crit], y_f[x_f >= f_crit], 
                                           color='r', alpha=0.2)
    f_dist_ax.legend(fontsize=8)
    
    # Add p-value text
    p_value_text = f_dist_ax.text(0.5, 0.9, "", ha='center', transform=f_dist_ax.transAxes, fontsize=9)
    
    # Add title text showing evaluation phase
    title_text = fig.suptitle("ACCUSATIVE Case: Model Evaluation Process", fontsize=16)
    
    # Footer describing the current step
    step_text = fig.text(0.5, 0.01, "", ha='center', fontsize=12, 
                       bbox=dict(boxstyle='round,pad=0.4', facecolor='lightblue', alpha=0.6))
    
    # Number of frames for the animation
    n_frames = 100
    
    # Initialize animation
    def init():
        points.set_offsets(np.zeros((0, 2)))
        for line in error_lines:
            line.set_data([], [])
        stats_text.set_text("")
        for bar in bars:
            bar.set_width(0)
        f_value_line.set_alpha(0)
        p_value_text.set_text("")
        step_text.set_text("")
        return [points, stats_text, step_text, p_value_text, f_value_line] + error_lines + list(bars)
    
    # Update function for animation
    def update(frame):
        progress = frame / n_frames
        
        if progress < 0.2:  # Phase 1: Show data points
            phase_progress = min(1, progress / 0.2)
            n_points = int(phase_progress * len(X))
            
            points_data = np.column_stack([X[:n_points].flatten(), np.zeros(n_points)])
            points.set_offsets(points_data)
            
            step_text.set_text("Step 1: Data loaded for evaluation")
            stats_text.set_text(f"Loading data...\n{n_points}/{len(X)} points")
            
        elif progress < 0.4:  # Phase 2: Show fitted line and predictions
            phase_progress = (progress - 0.2) / 0.2
            n_points = len(X)
            
            # Predictions from the model
            y_pred = evaluator_model.predict(X)
            
            # Set up data points
            residuals = (y - y_pred) * phase_progress
            points_data = np.column_stack([X.flatten(), residuals])
            points.set_offsets(points_data)
            
            # Update error lines
            for i in range(len(X)):
                if i < len(residuals):
                    error_lines[i].set_data([X[i, 0], X[i, 0]], [0, residuals[i]])
                else:
                    error_lines[i].set_data([], [])
            
            step_text.set_text("Step 2: Computing residuals (differences between actual and predicted values)")
            stats_text.set_text(f"Computing residuals...\n"
                              f"Predictions calculated: {int(n_points * phase_progress)}/{n_points}")
            
        elif progress < 0.6:  # Phase 3: Calculate statistics
            phase_progress = (progress - 0.4) / 0.2
            
            # Predictions and residuals
            y_pred = evaluator_model.predict(X)
            residuals = y - y_pred
            
            # Set up data points
            points_data = np.column_stack([X.flatten(), residuals])
            points.set_offsets(points_data)
            
            # Update error lines
            for i in range(len(X)):
                error_lines[i].set_data([X[i, 0], X[i, 0]], [0, residuals[i]])
            
            # Update metrics progressively
            current_r2 = metrics['r2'] * phase_progress
            current_mse = metrics['mse'] * phase_progress
            current_mae = metrics['mae'] * phase_progress
            current_f = metrics['f_statistic'] * phase_progress
            
            # Normalize metrics for display
            max_metric = max(metrics['r2'], metrics['mse'], metrics['mae'], metrics['f_statistic'])
            bars[0].set_width(current_r2 / max_metric)
            bars[1].set_width(current_mse / max_metric)
            bars[2].set_width(current_mae / max_metric)
            bars[3].set_width(current_f / max_metric)
            
            # Update statistics text
            stats_text.set_text(f"Calculating evaluation metrics...\n"
                              f"R²: {current_r2:.4f}\n"
                              f"MSE: {current_mse:.4f}\n"
                              f"MAE: {current_mae:.4f}\n"
                              f"F: {current_f:.4f}")
            
            step_text.set_text("Step 3: Computing evaluation statistics")
            
        elif progress < 0.8:  # Phase 4: Perform hypothesis testing
            phase_progress = (progress - 0.6) / 0.2
            
            # Keep residuals visible
            y_pred = evaluator_model.predict(X)
            residuals = y - y_pred
            points_data = np.column_stack([X.flatten(), residuals])
            points.set_offsets(points_data)
            
            # Update error lines
            for i in range(len(X)):
                error_lines[i].set_data([X[i, 0], X[i, 0]], [0, residuals[i]])
            
            # Show final metrics
            bars[0].set_width(metrics['r2'] / max_metric)
            bars[1].set_width(metrics['mse'] / max_metric)
            bars[2].set_width(metrics['mae'] / max_metric)
            bars[3].set_width(metrics['f_statistic'] / max_metric)
            
            # Show F-statistic progressing
            current_f = min(f_stat, f_stat * phase_progress)
            f_value_line.set_alpha(0.8)
            f_value_line.set_xdata([current_f, current_f])
            
            # Calculate and display p-value
            p_value = 1 - stats.f.cdf(current_f, dfn, dfd)
            p_value_text.set_text(f"p-value: {p_value:.6f}")
            
            # Update statistics text with hypothesis test info
            stats_text.set_text(f"Final Evaluation Metrics:\n"
                              f"R²: {metrics['r2']:.4f}\n"
                              f"MSE: {metrics['mse']:.4f}\n"
                              f"MAE: {metrics['mae']:.4f}\n"
                              f"F: {current_f:.4f}\n\n"
                              f"Hypothesis Testing:\n"
                              f"H₀: All coefficients are zero\n"
                              f"H₁: At least one coefficient ≠ 0")
            
            step_text.set_text("Step 4: Performing hypothesis testing (F-test)")
            
        else:  # Phase 5: Show final evaluation results
            # Keep residuals visible
            y_pred = evaluator_model.predict(X)
            residuals = y - y_pred
            points_data = np.column_stack([X.flatten(), residuals])
            points.set_offsets(points_data)
            
            # Update error lines
            for i in range(len(X)):
                error_lines[i].set_data([X[i, 0], X[i, 0]], [0, residuals[i]])
            
            # Show metrics
            bars[0].set_width(metrics['r2'] / max_metric)
            bars[1].set_width(metrics['mse'] / max_metric)
            bars[2].set_width(metrics['mae'] / max_metric)
            bars[3].set_width(metrics['f_statistic'] / max_metric)
            
            # Show F-statistic
            f_value_line.set_alpha(0.8)
            f_value_line.set_xdata([f_stat, f_stat])
            
            # Display final p-value
            p_value = 1 - stats.f.cdf(f_stat, dfn, dfd)
            p_value_text.set_text(f"p-value: {p_value:.6f}")
            
            # Final result text
            result_text = "Reject null hypothesis" if f_stat > f_crit else "Fail to reject null hypothesis"
            interpretation = "Model is significant" if f_stat > f_crit else "Model is not significant"
            
            stats_text.set_text(f"Evaluation Complete\n"
                              f"R²: {metrics['r2']:.4f}\n"
                              f"MSE: {metrics['mse']:.4f}\n"
                              f"MAE: {metrics['mae']:.4f}\n"
                              f"F({dfn}, {dfd}): {f_stat:.4f}\n\n"
                              f"Hypothesis Test Result:\n"
                              f"{result_text}\n"
                              f"{interpretation}")
            
            step_text.set_text("ACCUSATIVE Case Complete: Model has been evaluated")
        
        return [points, stats_text, step_text, p_value_text, f_value_line] + error_lines + list(bars)
    
    # Create animation
    anim = FuncAnimation(fig, update, frames=n_frames, init_func=init, blit=True, interval=50)
    
    # Save animation
    anim.save(evaluation_anim_path, writer='pillow', fps=20, dpi=100)
    plt.close(fig)
    
    assert os.path.exists(evaluation_anim_path), "ACCUSATIVE evaluation animation not created"
    
    # 7. Document all outputs for this case
    with open(os.path.join(case_dir, "accusative_results.txt"), 'w') as f:
        f.write(f"ACCUSATIVE CASE RESULTS\n")
        f.write(f"======================\n\n")
        f.write(f"Linguistic meaning: {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n")
        f.write(f"Regression context: {case_info['regression_context']}\n\n")
        
        f.write(f"Detailed Evaluation Metrics:\n")
        for metric, value in metrics.items():
            f.write(f"- {metric}: {value:.6f}\n")
        
        f.write(f"\nHypothesis Testing:\n")
        f.write(f"- F({metrics['df_regression']}, {metrics['df_residual']}) = {metrics['f_statistic']:.6f}\n")
        f.write(f"- Critical F (α=0.05): {f_crit:.6f}\n")
        if f_stat > f_crit:
            f.write(f"- Result: Reject null hypothesis, model is significant\n")
        else:
            f.write(f"- Result: Fail to reject null hypothesis\n")
            
        f.write(f"\nLinguistic Formula Example:\n")
        f.write(f"{case_info['example']}\n\n")
            
    logger.info(f"Completed ACCUSATIVE case test with visualizations in {case_dir}")
    
    return evaluator_model

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
    
    # 2. Get test data and split into chunks to demonstrate the receiving role
    X, y = linear_test_data
    
    # Split data into chunks to simulate data receiving over time
    chunk_size = 30
    n_samples = len(X)
    n_chunks = n_samples // chunk_size
    
    # Create chunks of data
    data_chunks = []
    for i in range(n_chunks):
        start_idx = i * chunk_size
        end_idx = start_idx + chunk_size
        data_chunks.append((
            X[start_idx:end_idx], 
            y[start_idx:end_idx],
            f"Chunk {i+1}/{n_chunks}"
        ))
    
    # 3. Demonstrate DATIVE case by having the model receive data chunks
    logger.info(f"DATIVE case demonstration: Model receiving data in chunks")
    
    received_samples = 0
    for chunk_X, chunk_y, chunk_name in data_chunks:
        # Use the receive_data method to demonstrate DATIVE role
        model.receive_data(chunk_X, chunk_y)
        received_samples += len(chunk_X)
        logger.info(f"DATIVE model received {chunk_name}: {len(chunk_X)} samples " +
                    f"(total: {received_samples}/{n_samples})")
    
    # 4. Process the received data
    X_combined, y_combined = model.process_data_buffer()
    assert X_combined.shape == X.shape, "DATIVE model failed to correctly process X data"
    assert y_combined.shape == y.shape, "DATIVE model failed to correctly process y data"
    
    # 5. Create visualization of data receiving process (model as recipient)
    data_flow_vis_path = os.path.join(case_dir, "data_flow_visualization.png")
    
    fig, ax = plt.subplots(figsize=(10, 6))
    
    # Create data flow visualization
    colors = plt.cm.viridis(np.linspace(0, 1, n_chunks))
    
    # Plot each data chunk with different color
    for i, (chunk_X, chunk_y, chunk_name) in enumerate(data_chunks):
        ax.scatter(chunk_X.flatten(), chunk_y, alpha=0.7, color=colors[i], s=60, 
                  label=chunk_name)
    
    # Add arrows pointing to the model (represented as a rectangle)
    # First plot the "model" as a rectangle
    rect = plt.Rectangle((0.4, 0.4), 0.2, 0.2, transform=ax.transAxes, 
                         facecolor='gray', alpha=0.3, edgecolor='black')
    ax.add_patch(rect)
    ax.text(0.5, 0.5, "DATIVE\nModel", transform=ax.transAxes, ha='center', va='center',
           fontsize=12, fontweight='bold')
    
    # Add arrows to the model
    arrow_positions = [(0.2, 0.3), (0.3, 0.7), (0.15, 0.5), (0.25, 0.4)]
    for i, pos in enumerate(arrow_positions[:n_chunks]):
        ax.annotate("", xy=(0.4, 0.5), xytext=pos, xycoords='axes fraction',
                   arrowprops=dict(arrowstyle="->", color=colors[i], lw=2))
    
    ax.set_title("DATIVE Case: Model Receiving Data Chunks")
    ax.set_xlabel("X")
    ax.set_ylabel("y")
    ax.legend(bbox_to_anchor=(1.05, 1), loc='upper left')
    ax.grid(True, alpha=0.3)
    
    # Add annotations
    ax.text(0.02, 0.02, "DATIVE case: Model as recipient of data flow", 
            transform=ax.transAxes, fontsize=10, fontweight='bold',
            bbox=dict(boxstyle="round,pad=0.3", facecolor='white', alpha=0.7))
    
    fig.tight_layout()
    fig.savefig(data_flow_vis_path, dpi=300)
    plt.close(fig)
    
    assert os.path.exists(data_flow_vis_path), "DATIVE data flow visualization not created"
    
    # 6. Create diagram showing data processing within DATIVE model
    processing_vis_path = os.path.join(case_dir, "data_processing_visualization.png")
    
    fig, ax = plt.subplots(figsize=(10, 6))
    ax.axis('off')
    
    # Draw flow diagram
    stages = [
        "Data\nChunk 1",
        "Data\nChunk 2",
        "Data\nChunk 3",
        "Buffer\nProcessing",
        "Combined\nDataset"
    ]
    
    # Draw boxes and arrows for the flow
    box_pos = np.array([0.1, 0.3, 0.5, 0.7, 0.9])
    box_y = [0.7, 0.5, 0.3, 0.5, 0.5]
    
    for i, (stage, x, y) in enumerate(zip(stages, box_pos, box_y)):
        color = 'skyblue' if i < 3 else 'lightgreen' if i == 3 else 'gold'
        rect = plt.Rectangle((x-0.08, y-0.1), 0.16, 0.2, transform=ax.transAxes, 
                          facecolor=color, alpha=0.6, edgecolor='black')
        ax.add_patch(rect)
        ax.text(x, y, stage, transform=ax.transAxes, ha='center', va='center',
               fontsize=12, fontweight='bold')
    
    # Add arrows
    for i in range(len(stages) - 1):
        if i < 3:  # First 3 boxes connect to the buffer
            start_x = box_pos[i] + 0.08
            end_x = box_pos[3] - 0.08
            start_y = box_y[i]
            end_y = box_y[3]
            
            # Create curved arrows
            ax.annotate("", xy=(end_x, end_y), xytext=(start_x, start_y), 
                       xycoords='axes fraction',
                       arrowprops=dict(arrowstyle="->", connectionstyle="arc3,rad=0.2", 
                                      color='black', lw=1.5))
        else:
            # Straight arrow from buffer to combined
            ax.annotate("", xy=(box_pos[i+1]-0.08, box_y[i+1]), 
                       xytext=(box_pos[i]+0.08, box_y[i]), 
                       xycoords='axes fraction',
                       arrowprops=dict(arrowstyle="->", color='black', lw=1.5))
    
    # Add model boundary
    dative_rect = plt.Rectangle((0.05, 0.1), 0.9, 0.7, transform=ax.transAxes, 
                             fill=False, edgecolor='red', linestyle='--', lw=2)
    ax.add_patch(dative_rect)
    ax.text(0.5, 0.85, "DATIVE Case: Model as Data Recipient", transform=ax.transAxes, 
           ha='center', va='center', fontsize=16, fontweight='bold',
           bbox=dict(boxstyle="round,pad=0.3", facecolor='white', alpha=0.7))
    
    fig.tight_layout()
    fig.savefig(processing_vis_path, dpi=300)
    plt.close(fig)
    
    assert os.path.exists(processing_vis_path), "DATIVE processing visualization not created"
    
    # 7. Now let's use the combined data to fit the model
    # Here we're temporarily switching to a NOMINATIVE role for fitting
    logger.info("After receiving data, DATIVE model switches temporarily to NOMINATIVE role for fitting")
    model.case = Case.NOMINATIVE
    model.fit(X_combined, y_combined)
    model.case = Case.DATIVE
    
    # 8. Document all outputs for this case
    with open(os.path.join(case_dir, "dative_results.txt"), 'w') as f:
        f.write(f"DATIVE CASE RESULTS\n")
        f.write(f"=================\n\n")
        f.write(f"Linguistic meaning: {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n")
        f.write(f"Regression context: {case_info['regression_context']}\n\n")
        
        f.write(f"Data Receiving Process:\n")
        f.write(f"- Total samples received: {received_samples}\n")
        f.write(f"- Number of data chunks: {n_chunks}\n")
        f.write(f"- Samples per chunk: {chunk_size}\n\n")
        
        f.write(f"Combined Data Properties:\n")
        f.write(f"- X shape: {X_combined.shape}\n")
        f.write(f"- y shape: {y_combined.shape}\n")
        
        # Add fitted parameters 
        params = model.get_parameters()
        f.write(f"\nFitted Parameters (after NOMINATIVE role switching):\n")
        f.write(f"- Intercept: {params['intercept']:.6f}\n")
        for i, coef in enumerate(params['coefficients']):
            f.write(f"- Coefficient {i+1}: {coef:.6f}\n")
            
        f.write(f"\nLinguistic Formula Example:\n")
        f.write(f"{case_info['example']}\n\n")
            
    logger.info(f"Completed DATIVE case test with visualizations in {case_dir}")
    
    return model

def test_genitive_case(linear_test_data, case_definitions):
    """
    Test GENITIVE case: The model as possessor or source of outputs.
    
    In linguistics: Indicates possession or source.
    In regression: The model as a source of predictions or reports.
    """
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
    
    # Get test data
    X, y = linear_test_data
    
    # 1. First, we need a fitted model to generate predictions
    # Fit a model in NOMINATIVE case
    fitted_model = LinearRegressionModel(model_id="fit_for_gen", case=Case.NOMINATIVE).fit(X, y)
    
    # 2. Create a GENITIVE case model as source/generator
    model = LinearRegressionModel(model_id=f"{Case.GENITIVE.value}_model", case=Case.GENITIVE)
    
    # Transfer parameters to the GENITIVE model (it needs to have parameters to generate outputs)
    model.intercept_ = fitted_model.intercept_
    model.coefficients_ = fitted_model.coefficients_
    model.is_trained = True
    
    # 3. Test the GENITIVE case by generating a summary (model as source)
    logger.info(f"GENITIVE case demonstration: Model generating detailed summary report")
    summary = model.get_summary()
    
    assert summary is not None and len(summary) > 0, "GENITIVE model failed to generate summary"
    
    # Write the summary to a file
    summary_path = os.path.join(case_dir, "model_summary.txt")
    with open(summary_path, 'w') as f:
        f.write(summary)
    
    # 4. Test the GENITIVE case by generating predictions with confidence intervals
    logger.info(f"GENITIVE case demonstration: Model generating predictions with confidence intervals")
    
    # Store training data for confidence interval calculation
    model.X_train = X
    model.y_train = y
    
    # Generate test points
    X_test = np.linspace(X.min(), X.max(), 50).reshape(-1, 1)
    
    # Generate predictions and confidence intervals
    predictions = model.generate_confidence_intervals(X_test, confidence_level=0.95)
    
    # 5. Visualize the predictions with confidence intervals
    pred_vis_path = os.path.join(case_dir, "predictions_visualization.png")
    
    fig, ax = plt.subplots(figsize=(10, 6))
    
    # Plot original data
    ax.scatter(X.flatten(), y, alpha=0.5, color='blue', s=40, label="Original Data")
    
    # Plot predictions
    ax.plot(X_test.flatten(), predictions['predictions'], color='red', linewidth=2, label="Model Predictions")
    
    # Plot confidence intervals
    ax.fill_between(X_test.flatten(), 
                    predictions['lower_bound'], 
                    predictions['upper_bound'],
                    color='red', alpha=0.2, label=f"{predictions['confidence_level']*100:.0f}% Confidence Interval")
    
    ax.set_title("GENITIVE Case: Model as Source of Predictions")
    ax.set_xlabel("X")
    ax.set_ylabel("y")
    ax.legend()
    ax.grid(True, alpha=0.3)
    
    # Add annotation
    ax.text(0.02, 0.02, "The predictions BELONG TO the linear model (GENITIVE case)", 
            transform=ax.transAxes, fontsize=10, fontweight='bold',
            bbox=dict(boxstyle="round,pad=0.3", facecolor='white', alpha=0.7))
    
    fig.tight_layout()
    fig.savefig(pred_vis_path, dpi=300)
    plt.close(fig)
    
    assert os.path.exists(pred_vis_path), "GENITIVE predictions visualization not created"
    
    # 6. Create visualization of model summary aspects
    summary_vis_path = os.path.join(case_dir, "summary_visualization.png")
    
    fig, ax = plt.subplots(figsize=(10, 7))
    ax.axis('off')
    
    # Create a visual representation of the model summary report
    title = "GENITIVE Case: Model as Source of Information"
    ax.text(0.5, 0.95, title, ha='center', va='top', fontsize=16, 
            fontweight='bold', transform=ax.transAxes)
    
    # Create a mock report document
    report_rect = plt.Rectangle((0.1, 0.1), 0.8, 0.8, transform=ax.transAxes, 
                              facecolor='beige', alpha=0.3, edgecolor='black')
    ax.add_patch(report_rect)
    
    # Add sections to the mock report
    sections = [
        ("MODEL PARAMETERS", 0.85, 'lightblue'),
        ("MODEL FIT STATISTICS", 0.65, 'lightgreen'),
        ("DATA INFORMATION", 0.45, 'lightyellow'),
        ("MODEL FORMULA", 0.25, 'lightcoral')
    ]
    
    for title, y_pos, color in sections:
        # Section header
        section_rect = plt.Rectangle((0.15, y_pos-0.05), 0.7, 0.15, transform=ax.transAxes, 
                                   facecolor=color, alpha=0.5, edgecolor='black')
        ax.add_patch(section_rect)
        ax.text(0.5, y_pos, title, transform=ax.transAxes, ha='center', va='center',
               fontsize=12, fontweight='bold')
        
        # Section content lines
        for i in range(3):
            y = y_pos - 0.02 - (i+1)*0.03
            ax.plot([0.2, 0.8], [y, y], transform=ax.transAxes, color='gray', alpha=0.5, linestyle='-')
    
    # Add "property of" text to emphasize GENITIVE case (possessive)
    property_text = "PROPERTY OF: Linear Regression Model"
    ax.text(0.5, 0.05, property_text, transform=ax.transAxes, ha='center', va='bottom',
           fontsize=14, fontweight='bold', color='darkred',
           bbox=dict(boxstyle="round,pad=0.3", facecolor='white', alpha=0.7))
    
    fig.tight_layout()
    fig.savefig(summary_vis_path, dpi=300)
    plt.close(fig)
    
    assert os.path.exists(summary_vis_path), "GENITIVE summary visualization not created"
    
    # 7. Create animation showing the model generating predictions over time
    prediction_anim_path = os.path.join(case_dir, "prediction_generation_animation.gif")
    
    # Create figure for animation with multiple panels
    fig = plt.figure(figsize=(14, 8))
    gs = GridSpec(2, 2, height_ratios=[3, 1], width_ratios=[3, 1])
    
    # Main plot for predictions and confidence intervals
    ax_main = fig.add_subplot(gs[0, 0])
    ax_main.set_title("GENITIVE Case: Model Generating Predictions")
    ax_main.set_xlabel("X")
    ax_main.set_ylabel("y")
    ax_main.grid(True, alpha=0.3)
    
    # Plot original data points that won't change
    ax_main.scatter(X.flatten(), y, alpha=0.5, color='blue', s=40, label="Training Data")
    
    # Create prediction line and confidence interval objects that will be updated
    pred_line, = ax_main.plot([], [], color='red', linewidth=2, label="Model Predictions")
    conf_fill = ax_main.fill_between([], [], [], color='red', alpha=0.2, label="95% Confidence Interval")
    
    # Create a legend
    ax_main.legend(loc='upper left')
    
    # Set axis limits
    x_margin = (X.max() - X.min()) * 0.1
    y_margin = (y.max() - y.min()) * 0.2
    ax_main.set_xlim(X.min() - x_margin, X.max() + x_margin)
    ax_main.set_ylim(y.min() - y_margin, y.max() + y_margin)
    
    # Panel for model parameters
    ax_params = fig.add_subplot(gs[0, 1])
    ax_params.set_title("Model Parameters")
    ax_params.axis('off')
    
    param_text = ax_params.text(0.1, 0.9, "", transform=ax_params.transAxes, 
                               fontsize=10, verticalalignment='top',
                               bbox=dict(boxstyle='round,pad=0.5', facecolor='lightblue', alpha=0.4))
    
    # Panel for prediction metrics
    ax_metrics = fig.add_subplot(gs[1, 0])
    ax_metrics.set_title("Prediction Information")
    ax_metrics.axis('off')
    
    metrics_text = ax_metrics.text(0.05, 0.8, "", transform=ax_metrics.transAxes,
                                  fontsize=10, verticalalignment='top',
                                  bbox=dict(boxstyle='round,pad=0.5', facecolor='lightgreen', alpha=0.4))
    
    # Panel for generation progress
    ax_progress = fig.add_subplot(gs[1, 1])
    ax_progress.set_title("Generation Progress")
    ax_progress.set_xlim(0, 100)
    ax_progress.set_ylim(0, 1)
    ax_progress.set_xlabel("% Complete")
    ax_progress.set_yticks([])
    
    progress_bar = ax_progress.bar(0, 0.5, width=0, color='green', alpha=0.7)
    
    # Create title for the whole figure with case emphasis
    fig.suptitle("GENITIVE Case: Model as Source of Predictions", fontsize=16)
    
    # Add a footer with linguistic context
    footer_text = fig.text(0.5, 0.01, 
                         "The predictions BELONG TO the linear model (GENITIVE case)", 
                         ha='center', fontsize=12, fontweight='bold',
                         bbox=dict(boxstyle="round,pad=0.3", facecolor='white', alpha=0.7))
    
    # Number of frames for the animation
    n_frames = 100
    
    # Initialize function for animation
    def init():
        pred_line.set_data([], [])
        param_text.set_text("")
        metrics_text.set_text("")
        progress_bar[0].set_width(0)
        return pred_line, param_text, metrics_text, progress_bar[0]
    
    # Update function for animation
    def update(frame):
        progress = frame / n_frames
        
        # Calculate how many prediction points to show based on progress
        n_points = max(2, int(progress * len(X_test)))
        X_visible = X_test[:n_points]
        
        # Get predictions for visible points
        pred_slice = predictions['predictions'][:n_points]
        lower_slice = predictions['lower_bound'][:n_points]
        upper_slice = predictions['upper_bound'][:n_points]
        
        # Update prediction line
        pred_line.set_data(X_visible.flatten(), pred_slice)
        
        # Update confidence interval fill
        # Need to remove old fill and create new one
        ax_main.collections.remove(conf_fill)
        conf_fill = ax_main.fill_between(X_visible.flatten(), lower_slice, upper_slice, 
                                        color='red', alpha=0.2)
        
        # Update parameter text
        params_str = f"Intercept: {model.intercept_:.4f}\n"
        for i, coef in enumerate(model.coefficients_):
            params_str += f"Coefficient {i+1}: {coef:.4f}\n"
        params_str += f"\nR²: {fitted_model.r_squared_:.4f}"
        param_text.set_text(params_str)
        
        # Update metrics text
        if n_points > 0:
            mean_pred = np.mean(pred_slice)
            std_pred = np.std(pred_slice) if len(pred_slice) > 1 else 0
            mean_width = np.mean(upper_slice - lower_slice) if len(upper_slice) > 0 else 0
            
            metrics_str = (
                f"Points Generated: {n_points}/{len(X_test)}\n"
                f"Mean Prediction: {mean_pred:.4f}\n"
                f"Std Deviation: {std_pred:.4f}\n"
                f"Mean CI Width: {mean_width:.4f}\n"
                f"Confidence Level: {predictions['confidence_level']*100:.0f}%"
            )
            metrics_text.set_text(metrics_str)
        
        # Update progress bar
        progress_bar[0].set_width(progress * 100)
        
        return pred_line, param_text, metrics_text, progress_bar[0], conf_fill
    
    # Create animation
    anim = FuncAnimation(fig, update, frames=n_frames, init_func=init, 
                        blit=False, interval=100)
    
    # Save animation
    anim.save(prediction_anim_path, writer='pillow', fps=10, dpi=100)
    plt.close(fig)
    
    assert os.path.exists(prediction_anim_path), "GENITIVE prediction animation not created"
    
    # Create results summary file
    results_path = os.path.join(case_dir, "genitive_results.txt")
    with open(results_path, 'w') as f:
        f.write(f"GENITIVE CASE RESULTS\n")
        f.write(f"===================\n\n")
        f.write(f"Linguistic meaning: {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n")
        f.write(f"Regression context: {case_info['regression_context']}\n\n")
        
        f.write(f"Model Parameters (source of predictions):\n")
        f.write(f"- Intercept: {model.intercept_:.6f}\n")
        for i, coef in enumerate(model.coefficients_):
            f.write(f"- Coefficient {i+1}: {coef:.6f}\n")
        
        f.write(f"\nConfidence Interval Information:\n")
        f.write(f"- Confidence level: {predictions['confidence_level']*100:.1f}%\n")
        f.write(f"- Number of prediction points: {len(X_test)}\n\n")
        
        f.write(f"Linguistic Formula Example:\n")
        f.write(f"{case_info['example']}\n\n")
        
        f.write(f"\nModel Summary (excerpt):\n")
        f.write(f"{summary[:500]}...\n")
        
        f.write(f"\nVisualizations:\n")
        f.write(f"- Static predictions: predictions_visualization.png\n")
        f.write(f"- Prediction generation animation: prediction_generation_animation.gif\n")
        f.write(f"- Summary visualization: summary_visualization.png\n")
    
    return model

def test_instrumental_case(linear_test_data, case_definitions):
    """
    Test INSTRUMENTAL case: The model as tool or method.
    
    In linguistics: Indicates the means by which an action is done.
    In regression: The model as methodology or algorithm.
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
    
    # Get test data
    X_complex, y_complex = linear_test_data
    
    # Create a simpler dataset for clearer INSTRUMENTAL case visualization
    # (Just a subset of the original data)
    np.random.seed(42)  # for reproducibility
    indices = np.random.choice(len(X_complex), size=20, replace=False)
    X_simple = X_complex[indices]
    y_simple = y_complex[indices]
    
    # 1. Create an INSTRUMENTAL case model (model as method)
    model = LinearRegressionModel(model_id=f"{Case.INSTRUMENTAL.value}_model", case=Case.INSTRUMENTAL)
    
    # 2. Use the model to fit data with visible methodology
    logger.info(f"INSTRUMENTAL case demonstration: Model as computational methodology")
    
    # Track the actual Normal Equation calculation steps
    # Step 1: Calculate X^T * X
    X_with_intercept = np.column_stack([np.ones(X_simple.shape[0]), X_simple])
    XTX = X_with_intercept.T @ X_with_intercept
    
    # Step 2: Calculate inverse of X^T * X
    XTX_inv = np.linalg.inv(XTX)
    
    # Step 3: Calculate X^T * y
    XTy = X_with_intercept.T @ y_simple
    
    # Step 4: Calculate theta = (X^T * X)^-1 * X^T * y
    theta = XTX_inv @ XTy
    
    # Now fit using the model to get the same results
    model.fit(X_simple, y_simple)
    
    # Verify model calculation matches direct calculation
    direct_params = {"intercept": theta[0], "coefficients": theta[1:]}
    assert np.allclose(model.intercept_, direct_params["intercept"]), "INSTRUMENTAL model calculation mismatch"
    assert np.allclose(model.coefficients_, direct_params["coefficients"]), "INSTRUMENTAL model calculation mismatch"
    
    # 3. Visualize the computational process
    computation_vis_path = os.path.join(case_dir, "computation_visualization.png")
    
    # Create a figure showing the computational steps of linear regression
    fig, axs = plt.subplots(2, 2, figsize=(12, 10))
    axs = axs.flatten()
    
    # Plot 1: X^T visualization
    axs[0].imshow(X_with_intercept.T, cmap='viridis', aspect='auto')
    axs[0].set_title("Step 1: Form X^T Matrix")
    axs[0].set_xlabel("n samples")
    axs[0].set_ylabel("Features")
    axs[0].set_yticks([0, 1])
    axs[0].set_yticklabels(["Intercept", "X"])
    
    # Plot 2: X^T * X visualization
    axs[1].imshow(XTX, cmap='plasma')
    axs[1].set_title("Step 2: Calculate X^T * X")
    axs[1].set_xlabel("Features")
    axs[1].set_ylabel("Features")
    axs[1].set_xticks([0, 1])
    axs[1].set_yticks([0, 1])
    axs[1].set_xticklabels(["Intercept", "X"])
    axs[1].set_yticklabels(["Intercept", "X"])
    
    # Add matrix values
    for i in range(XTX.shape[0]):
        for j in range(XTX.shape[1]):
            axs[1].text(j, i, f'{XTX[i, j]:.1f}', ha='center', va='center', 
                       color='white' if XTX[i, j] > XTX.max()/2 else 'black')
    
    # Plot 3: X^T * y visualization
    axs[2].bar(['Intercept', 'Slope'], XTy.flatten(), color='green', alpha=0.6)
    axs[2].set_title("Step 3: Calculate X^T * y")
    axs[2].set_ylabel("Value")
    
    # Plot 4: Final result visualization
    axs[3].scatter(X_simple, y_simple, color='blue', s=60, alpha=0.7, label='Data')
    
    # Fitted line
    X_line = np.linspace(X_simple.min(), X_simple.max(), 100).reshape(-1, 1)
    y_line = theta[0] + theta[1] * X_line
    axs[3].plot(X_line, y_line, color='red', linewidth=2, 
               label=f'y = {float(theta[0]):.2f} + {float(theta[1]):.2f}x')
    
    axs[3].set_title(f"Step 4: Apply Formula to Get θ = [{float(theta[0]):.2f}, {float(theta[1]):.2f}]")
    axs[3].set_xlabel("X")
    axs[3].set_ylabel("y")
    axs[3].legend()
    axs[3].grid(True, alpha=0.3)
    
    # Main title
    fig.suptitle("INSTRUMENTAL Case: Linear Regression as Computational Method", fontsize=16)
    
    # Add annotation about INSTRUMENTAL case
    fig.text(0.5, 0.01, "INSTRUMENTAL case: Using Linear Regression WITH Normal Equation method", 
             ha='center', fontsize=12, fontweight='bold',
             bbox=dict(boxstyle="round,pad=0.3", facecolor='white', alpha=0.7))
    
    fig.tight_layout(rect=[0, 0.03, 1, 0.95])
    fig.savefig(computation_vis_path, dpi=300)
    plt.close(fig)
    
    assert os.path.exists(computation_vis_path), "INSTRUMENTAL computation visualization not created"
    
    # 4. Create an animation showing the computational process
    computation_anim_path = os.path.join(case_dir, "computational_method_animation.gif")
    
    # Create a figure for the animation
    fig = plt.figure(figsize=(12, 8))
    gs = GridSpec(2, 2, height_ratios=[1, 1])
    
    # Main plot areas
    ax_matrix = fig.add_subplot(gs[0, 0])  # For matrix visualizations
    ax_formula = fig.add_subplot(gs[0, 1])  # For formula display
    ax_graph = fig.add_subplot(gs[1, 0])    # For regression line
    ax_steps = fig.add_subplot(gs[1, 1])    # For step descriptions
    
    # Setup axis for the matrix visualization
    ax_matrix.set_title("Matrix Operations")
    ax_matrix.axis('off')
    
    # Setup axis for the formula
    ax_formula.set_title("Normal Equation")
    ax_formula.axis('off')
    formula_text = ax_formula.text(0.5, 0.5, "", ha='center', va='center', 
                                 fontsize=14, transform=ax_formula.transAxes)
    
    # Setup axis for the regression graph
    ax_graph.set_title("Linear Regression Model")
    ax_graph.set_xlabel("X")
    ax_graph.set_ylabel("y")
    ax_graph.grid(True, alpha=0.3)
    
    # Plot data points
    ax_graph.scatter(X_simple, y_simple, color='blue', s=40, alpha=0.6, label='Data')
    
    # Create empty line for the regression fit
    line, = ax_graph.plot([], [], 'r-', linewidth=2, label='Regression Line')
    ax_graph.legend(loc='upper left')
    
    # Set reasonable axis limits
    x_margin = (X_simple.max() - X_simple.min()) * 0.1
    y_margin = (y_simple.max() - y_simple.min()) * 0.2
    ax_graph.set_xlim(X_simple.min() - x_margin, X_simple.max() + x_margin)
    ax_graph.set_ylim(y_simple.min() - y_margin, y_simple.max() + y_margin)
    
    # Setup axis for the steps description
    ax_steps.set_title("Computational Steps")
    ax_steps.axis('off')
    steps_text = ax_steps.text(0.1, 0.9, "", fontsize=10, va='top',
                             transform=ax_steps.transAxes,
                             bbox=dict(boxstyle='round,pad=0.5', facecolor='wheat', alpha=0.5))
    
    # Main title
    fig.suptitle("INSTRUMENTAL Case: Linear Regression Method", fontsize=16)
    
    # Footer with case example
    footer_text = fig.text(0.5, 0.01, 
                         "The researcher solves the problem WITH the LINEAR MODEL (INSTRUMENTAL case)", 
                         ha='center', fontsize=12, fontweight='bold',
                         bbox=dict(boxstyle="round,pad=0.3", facecolor='white', alpha=0.7))
    
    # Image objects for matrices
    matrix_img = ax_matrix.imshow(np.zeros((2, 2)), cmap='viridis', aspect='auto')
    
    # Number of frames for animation
    n_frames = 100
    
    # Animation initialization function
    def init():
        line.set_data([], [])
        formula_text.set_text("")
        steps_text.set_text("")
        matrix_img.set_array(np.zeros((2, 2)))
        return line, formula_text, steps_text, matrix_img
    
    # Animation update function
    def update(frame):
        progress = frame / n_frames
        
        if progress < 0.2:  # Phase 1: Setup design matrix
            phase_progress = progress / 0.2
            
            # Show the design matrix gradually forming
            visible_rows = max(1, int(phase_progress * X_with_intercept.shape[0]))
            current_X = X_with_intercept[:visible_rows]
            
            # Update matrix visualization
            if len(current_X) > 0:
                matrix_img.set_array(current_X)
                matrix_img.set_extent([-0.5, 1.5, visible_rows-0.5, -0.5])
                ax_matrix.set_title("Design Matrix X")
            
            # Update formula
            formula_text.set_text("Step 1: Form design matrix X\nwith intercept column")
            
            # Update steps description
            steps_text.set_text(
                f"Forming design matrix X:\n"
                f"- Adding column of 1's for intercept\n"
                f"- X has shape {current_X.shape}\n"
                f"- Building matrix: {visible_rows}/{X_with_intercept.shape[0]} rows"
            )
            
            # No regression line yet
            line.set_data([], [])
            
        elif progress < 0.4:  # Phase 2: X^T * X calculation
            phase_progress = (progress - 0.2) / 0.2
            
            # Gradually transition from X to X^T * X
            # Interpolate between empty matrix and XTX
            current_XTX = XTX * phase_progress
            
            # Update matrix visualization
            matrix_img.set_array(current_XTX)
            matrix_img.set_extent([-0.5, 1.5, 1.5, -0.5])
            ax_matrix.set_title("X^T * X Matrix")
            
            # Update formula
            highlighted_part = r"(X^T X)"
            formula_text.set_text(
                f"Normal Equation:\nθ = {highlighted_part}^(-1) X^T y"
            )
            
            # Update steps description
            steps_text.set_text(
                f"Calculating X^T * X:\n"
                f"- Matrix multiplication of X^T and X\n"
                f"- Resulting matrix has shape {XTX.shape}\n"
                f"- Result stores inner products of feature columns\n"
                f"- Computation progress: {int(phase_progress*100)}%"
            )
            
            # Still no regression line
            line.set_data([], [])
            
        elif progress < 0.6:  # Phase 3: Calculate inverse and X^T * y
            phase_progress = (progress - 0.4) / 0.2
            
            if phase_progress < 0.5:  # First half: show inverse calculation
                sub_progress = phase_progress * 2
                
                # Show the inverse calculation
                current_inv = XTX_inv * sub_progress
                
                # Update matrix visualization
                matrix_img.set_array(current_inv)
                matrix_img.set_extent([-0.5, 1.5, 1.5, -0.5])
                ax_matrix.set_title("(X^T * X)^(-1) Matrix")
                
                # Update formula
                highlighted_part = r"(X^T X)^(-1)"
                formula_text.set_text(
                    f"Normal Equation:\nθ = {highlighted_part} X^T y"
                )
                
                # Update steps description
                steps_text.set_text(
                    f"Calculating (X^T * X)^(-1):\n"
                    f"- Matrix inversion operation\n"
                    f"- Ensures solution minimizes squared errors\n"
                    f"- Computation progress: {int(sub_progress*100)}%"
                )
                
            else:  # Second half: show X^T * y calculation
                sub_progress = (phase_progress - 0.5) * 2
                
                # Show X^T * y calculation
                current_XTy = XTy * sub_progress
                
                # Update matrix visualization
                # Reshape to make it visible as a column vector
                display_XTy = current_XTy.reshape(-1, 1)
                matrix_img.set_array(display_XTy)
                matrix_img.set_extent([-0.5, 0.5, 1.5, -0.5])
                ax_matrix.set_title("X^T * y Vector")
                
                # Update formula
                highlighted_part = r"X^T y"
                formula_text.set_text(
                    f"Normal Equation:\nθ = (X^T X)^(-1) {highlighted_part}"
                )
                
                # Update steps description
                steps_text.set_text(
                    f"Calculating X^T * y:\n"
                    f"- Matrix-vector multiplication\n"
                    f"- Projects target y onto feature space\n"
                    f"- Computation progress: {int(sub_progress*100)}%\n"
                    f"- Vector values: [{current_XTy[0]:.2f}, {current_XTy[1]:.2f}]"
                )
            
            # Still no regression line
            line.set_data([], [])
            
        elif progress < 0.8:  # Phase 4: Final calculation and line drawing
            phase_progress = (progress - 0.6) / 0.2
            
            # Gradually calculate the final parameters
            current_theta = theta * phase_progress
            
            # Update matrix visualization - reshape for column vector
            display_theta = current_theta.reshape(-1, 1)
            matrix_img.set_array(display_theta)
            matrix_img.set_extent([-0.5, 0.5, 1.5, -0.5])
            ax_matrix.set_title("θ Parameters Vector")
            
            # Update formula
            formula_text.set_text(
                f"Final Solution:\nθ = (X^T X)^(-1) X^T y\n\n"
                f"θ = [{current_theta[0]:.4f}, {current_theta[1]:.4f}]"
            )
            
            # Update steps description
            steps_text.set_text(
                f"Calculating final parameters θ:\n"
                f"- Matrix multiplication: (X^T * X)^(-1) * (X^T * y)\n"
                f"- Parameter computation progress: {int(phase_progress*100)}%\n"
                f"- Current intercept: {current_theta[0]:.4f}\n"
                f"- Current slope: {current_theta[1]:.4f}"
            )
            
            # Start drawing the regression line
            X_line_points = np.linspace(X_simple.min(), X_simple.max(), 100)
            # Calculate predictions with current parameters
            y_line_points = current_theta[0] + current_theta[1] * X_line_points
            line.set_data(X_line_points, y_line_points)
            
        else:  # Phase 5: Show final results and equation
            # Display final parameters
            matrix_img.set_array(theta.reshape(-1, 1))
            matrix_img.set_extent([-0.5, 0.5, 1.5, -0.5])
            ax_matrix.set_title("Final Parameters θ")
            
            # Final equation
            formula_text.set_text(
                f"Linear Regression Model:\n"
                f"y = {theta[0]:.4f} + {theta[1]:.4f}x\n\n"
                f"Solved using Normal Equation:\n"
                f"θ = (X^T X)^(-1) X^T y"
            )
            
            # Final description
            steps_text.set_text(
                f"INSTRUMENTAL Case Complete:\n"
                f"- Model used as computational method\n"
                f"- Normal equation solution implemented\n"
                f"- Final parameters: [{theta[0]:.4f}, {theta[1]:.4f}]\n"
                f"- Model ready for predictions\n"
                f"- Method minimizes sum of squared errors"
            )
            
            # Final regression line
            X_line_points = np.linspace(X_simple.min(), X_simple.max(), 100)
            y_line_points = theta[0] + theta[1] * X_line_points
            line.set_data(X_line_points, y_line_points)
            
        return line, formula_text, steps_text, matrix_img
    
    # Create animation
    anim = FuncAnimation(fig, update, frames=n_frames, init_func=init, 
                        blit=False, interval=100)
    
    # Save animation
    anim.save(computation_anim_path, writer='pillow', fps=10, dpi=100)
    plt.close(fig)
    
    assert os.path.exists(computation_anim_path), "INSTRUMENTAL computation animation not created"
    
    # 5. Create results summary file
    results_path = os.path.join(case_dir, "instrumental_results.txt")
    with open(results_path, 'w') as f:
        f.write(f"INSTRUMENTAL CASE RESULTS\n")
        f.write(f"========================\n\n")
        f.write(f"Linguistic meaning: {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n")
        f.write(f"Regression context: {case_info['regression_context']}\n\n")
        
        f.write(f"Computational Method: Normal Equation\n")
        f.write(f"Formula: θ = (X^T X)^(-1) X^T y\n\n")
        
        f.write(f"Parameters Obtained:\n")
        f.write(f"- Intercept: {float(theta[0]):.6f}\n")
        f.write(f"- Slope: {float(theta[1]):.6f}\n\n")
        
        f.write(f"Linguistic Formula Example:\n")
        f.write(f"{case_info['example']}\n\n")
        
        f.write(f"Visualizations:\n")
        f.write(f"- Computation steps: computation_visualization.png\n")
        f.write(f"- Computation animation: computational_method_animation.gif\n")
    
    logger.info(f"Completed INSTRUMENTAL case test with visualizations in {case_dir}")
    
    return model

def test_vocative_case(linear_test_data, case_definitions):
    """
    Test VOCATIVE case: The model as addressable interface.
    
    In linguistics: Direct address, calling someone by name.
    In regression: The model as interactive interface or API.
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
    
    # 1. Create a VOCATIVE case model (model as interface)
    model = LinearRegressionModel(model_id=f"{Case.VOCATIVE.value}_model", case=Case.VOCATIVE)
    
    # 2. First fit the model so it has some parameters
    X, y = linear_test_data
    logger.info(f"VOCATIVE case demonstration: Model as interactive interface")
    
    # Temporarily switch to NOMINATIVE to fit, then switch back to VOCATIVE
    model.case = Case.NOMINATIVE
    model.fit(X, y)
    model.case = Case.VOCATIVE  # Switch back to VOCATIVE case
    
    # 3. Simulate queries to the interface
    logger.info(f"Making queries to VOCATIVE model interface")
    
    # Store query responses for visualization
    query_responses = {}
    
    # Query 1: Ask for a prediction
    X_query = np.array([[2.5]])
    prediction_response = model.respond_to_query("predict", X=X_query)
    query_responses["predict"] = prediction_response
    
    assert "predictions" in prediction_response, "Prediction missing from query response"
    logger.info(f"VOCATIVE model prediction query response: {prediction_response['predictions'][0]:.4f}")
    
    # Query 2: Ask for parameters
    params_response = model.respond_to_query("parameters")
    query_responses["parameters"] = params_response
    
    assert "parameters" in params_response, "Parameters missing from query response"
    logger.info(f"VOCATIVE model parameter query response: intercept={params_response['parameters']['intercept']:.4f}")
    
    # Query 3: Ask for a model summary
    summary_response = model.respond_to_query("summary")
    query_responses["summary"] = summary_response
    
    assert "summary" in summary_response, "Summary missing from query response"
    logger.info(f"VOCATIVE model summary query: received {len(summary_response['summary'])} character response")
    
    # Query 4: Ask for model evaluation
    eval_response = model.respond_to_query("evaluate", X=X, y=y)
    query_responses["evaluate"] = eval_response
    
    assert "evaluation" in eval_response, "Evaluation missing from query response"
    logger.info(f"VOCATIVE model evaluation query: R²={eval_response['evaluation']['r2']:.4f}")
    
    # 4. Create visualization of interface interaction
    interface_vis_path = os.path.join(case_dir, "interface_visualization.png")
    
    # Create a more detailed and visually appealing visualization showing interaction
    fig, axs = plt.subplots(2, 2, figsize=(12, 10))
    axs = axs.flatten()
    
    # Style for boxes
    interface_style = dict(boxstyle='round,pad=0.6', facecolor='#e8f4f8', edgecolor='#4682b4', alpha=0.9)
    query_style = dict(boxstyle='round,pad=0.5', facecolor='#f0f8ff', edgecolor='#4682b4', alpha=0.8)
    response_style = dict(boxstyle='round,pad=0.5', facecolor='#f5f5f5', edgecolor='#2f4f4f', alpha=0.8)
    
    # Subplot 1: Prediction Query
    axs[0].axis('off')
    axs[0].set_title("Prediction Interface", fontsize=12, fontweight='bold')
    
    # Draw interface
    axs[0].text(0.5, 0.85, "VOCATIVE Case: Model as Prediction Interface", 
              ha='center', fontsize=11, fontweight='bold', 
              bbox=interface_style)
    
    # Draw query
    query_text = f"Query: predict(X=[[2.5]])"
    axs[0].text(0.3, 0.65, query_text, ha='center', fontsize=10, 
              bbox=query_style)
    
    # Draw response
    pred_value = prediction_response["predictions"][0]
    response_text = f"Response: {pred_value:.4f}"
    axs[0].text(0.7, 0.5, response_text, ha='center', fontsize=10, 
              bbox=response_style)
    
    # Draw arrows
    axs[0].annotate("", xy=(0.5, 0.62), xytext=(0.3, 0.62),
                  arrowprops=dict(arrowstyle="->", color='#4682b4'))
    axs[0].annotate("", xy=(0.5, 0.53), xytext=(0.7, 0.53),
                  arrowprops=dict(arrowstyle="->", color='#2f4f4f'))
    
    # Add visualization
    x_line = np.linspace(-5, 5, 100)
    y_line = params_response["parameters"]["intercept"] + params_response["parameters"]["coefficients"][0] * x_line
    
    # Create small plot inside
    small_ax = fig.add_axes([0.15, 0.05, 0.3, 0.25])
    small_ax.plot(x_line, y_line, 'r-')
    small_ax.scatter([2.5], [pred_value], c='blue', s=50)
    small_ax.set_xlabel('X')
    small_ax.set_ylabel('y')
    small_ax.grid(True, alpha=0.3)
    small_ax.set_title('Prediction Visualization', fontsize=8)
    
    # Subplot 2: Parameters Query
    axs[1].axis('off')
    axs[1].set_title("Parameter Interface", fontsize=12, fontweight='bold')
    
    # Draw interface
    axs[1].text(0.5, 0.85, "VOCATIVE Case: Model as Parameter Interface", 
              ha='center', fontsize=11, fontweight='bold', 
              bbox=interface_style)
    
    # Draw query
    query_text = "Query: get_parameters()"
    axs[1].text(0.3, 0.65, query_text, ha='center', fontsize=10, 
              bbox=query_style)
    
    # Draw response
    intercept = params_response["parameters"]["intercept"]
    coef = params_response["parameters"]["coefficients"][0]
    response_text = f"Response:\n - intercept: {intercept:.4f}\n - coefficient: {coef:.4f}"
    axs[1].text(0.7, 0.5, response_text, ha='center', fontsize=10, 
              bbox=response_style)
    
    # Draw arrows
    axs[1].annotate("", xy=(0.5, 0.62), xytext=(0.3, 0.62),
                  arrowprops=dict(arrowstyle="->", color='#4682b4'))
    axs[1].annotate("", xy=(0.5, 0.53), xytext=(0.7, 0.53),
                  arrowprops=dict(arrowstyle="->", color='#2f4f4f'))
    
    # Add formula visualization
    formula_text = f"y = {intercept:.4f} + {coef:.4f} · x"
    axs[1].text(0.5, 0.2, formula_text, ha='center', fontsize=12, 
              bbox=dict(boxstyle='round,pad=0.5', facecolor='lightyellow', alpha=0.5))
    
    # Subplot 3: Evaluation Query
    axs[2].axis('off')
    axs[2].set_title("Evaluation Interface", fontsize=12, fontweight='bold')
    
    # Draw interface
    axs[2].text(0.5, 0.85, "VOCATIVE Case: Model as Evaluation Interface", 
              ha='center', fontsize=11, fontweight='bold', 
              bbox=interface_style)
    
    # Draw query
    query_text = "Query: evaluate(X, y)"
    axs[2].text(0.3, 0.65, query_text, ha='center', fontsize=10, 
              bbox=query_style)
    
    # Draw response
    metrics = eval_response["evaluation"]
    response_text = f"Response:\n - R²: {metrics['r2']:.4f}\n - MSE: {metrics['mse']:.4f}\n - MAE: {metrics['mae']:.4f}"
    axs[2].text(0.7, 0.5, response_text, ha='center', fontsize=10, 
              bbox=response_style)
    
    # Draw arrows
    axs[2].annotate("", xy=(0.5, 0.62), xytext=(0.3, 0.62),
                  arrowprops=dict(arrowstyle="->", color='#4682b4'))
    axs[2].annotate("", xy=(0.5, 0.53), xytext=(0.7, 0.53),
                  arrowprops=dict(arrowstyle="->", color='#2f4f4f'))
    
    # Add small metrics visualization
    metrics_fig = fig.add_axes([0.56, 0.05, 0.3, 0.25])
    metric_names = ['R²', 'MSE', 'MAE']
    metric_values = [metrics['r2'], metrics['mse'], metrics['mae']]
    metrics_fig.bar(metric_names, metric_values, color=['#4682b4', '#ff7f50', '#2e8b57'])
    metrics_fig.set_title('Evaluation Metrics', fontsize=8)
    
    # Subplot 4: Summary of VOCATIVE Case
    axs[3].axis('off')
    axs[3].set_title("VOCATIVE Case Overview", fontsize=12, fontweight='bold')
    
    # Create a diagram showing the vocative role
    role_text = "Linear Regression Model as Interactive Interface"
    axs[3].text(0.5, 0.85, role_text, ha='center', fontsize=12, fontweight='bold', 
             bbox=dict(boxstyle='round,pad=0.5', facecolor='lightblue', alpha=0.5))
    
    # Interface description
    interface_desc = """The VOCATIVE case represents the model as an
addressable entity with a defined API that
users can interact with through queries."""
    axs[3].text(0.5, 0.65, interface_desc, ha='center', fontsize=10, 
             bbox=dict(boxstyle='round,pad=0.5', facecolor='#f0f8ff', alpha=0.8))
    
    # Example
    example_text = f'Example: "{case_info["example"]}"'
    axs[3].text(0.5, 0.4, example_text, ha='center', fontsize=10, fontstyle='italic',
             bbox=dict(boxstyle='round,pad=0.5', facecolor='#e6f2ff', alpha=0.8))
    
    # API diagram
    api_functions = [
        "predict(X) → predictions",
        "get_parameters() → model parameters",
        "evaluate(X, y) → metrics",
        "get_summary() → model description"
    ]
    
    for i, func in enumerate(api_functions):
        y_pos = 0.25 - i * 0.05
        axs[3].text(0.5, y_pos, func, ha='center', fontsize=9,
                 bbox=dict(boxstyle='round,pad=0.2', facecolor='#f5f5f5', alpha=0.8))
    
    # Add user icon on left and model icon on right
    user_icon = "👤"  # User icon
    model_icon = "📊"  # Chart icon for model
    
    axs[3].text(0.2, 0.5, user_icon, ha='center', fontsize=20)
    axs[3].text(0.8, 0.5, model_icon, ha='center', fontsize=20)
    
    # Add arrows between user and model
    axs[3].annotate("", xy=(0.75, 0.52), xytext=(0.25, 0.52),
                  arrowprops=dict(arrowstyle="->", color='#4682b4'))
    axs[3].text(0.5, 0.54, "Query", ha='center', fontsize=8)
    
    axs[3].annotate("", xy=(0.25, 0.48), xytext=(0.75, 0.48),
                  arrowprops=dict(arrowstyle="->", color='#2f4f4f'))
    axs[3].text(0.5, 0.46, "Response", ha='center', fontsize=8)
    
    # Add title for the full figure
    fig.suptitle("VOCATIVE Case: Model as Interactive Interface", fontsize=14, fontweight='bold', y=0.98)
    
    # Adjust layout and save
    fig.tight_layout()
    fig.savefig(interface_vis_path, dpi=300)
    plt.close(fig)
    
    assert os.path.exists(interface_vis_path), "VOCATIVE interface visualization not created"
    
    # 5. Create an animated interaction visualization
    interaction_anim_path = os.path.join(case_dir, "interaction_animation.gif")
    
    # Create animation figure
    fig, ax = plt.subplots(figsize=(10, 6))
    ax.axis('off')
    
    # Settings
    query_types = ["predict", "parameters", "evaluate", "summary"]
    query_texts = [
        "predict(X=[[2.5]])",
        "get_parameters()",
        "evaluate(X, y)",
        "get_summary()"
    ]
    response_texts = [
        f"predictions: [{prediction_response['predictions'][0]:.4f}]",
        f"intercept: {params_response['parameters']['intercept']:.4f}, coef: {params_response['parameters']['coefficients'][0]:.4f}",
        f"R²: {eval_response['evaluation']['r2']:.4f}, MSE: {eval_response['evaluation']['mse']:.4f}",
        "Summary of trained linear regression model..."
    ]
    
    # Animation elements
    query_box = dict(boxstyle='round,pad=0.5', facecolor='#f0f8ff', edgecolor='#4682b4', alpha=0.0)
    response_box = dict(boxstyle='round,pad=0.5', facecolor='#f5f5f5', edgecolor='#2f4f4f', alpha=0.0)
    
    # Add title
    title = ax.text(0.5, 0.9, "VOCATIVE Case: Model as Interactive Interface", 
                 ha='center', fontsize=14, fontweight='bold', transform=ax.transAxes,
                 bbox=dict(boxstyle='round,pad=0.6', facecolor='#e8f4f8', edgecolor='#4682b4', alpha=0.9))
    
    # Query and response text objects
    query_text = ax.text(0.3, 0.65, "", ha='center', fontsize=12, transform=ax.transAxes, bbox=query_box)
    response_text = ax.text(0.7, 0.5, "", ha='center', fontsize=12, transform=ax.transAxes, bbox=response_box)
    
    # User icon and model icon
    user_text = ax.text(0.2, 0.5, "👤", ha='center', fontsize=24, transform=ax.transAxes)
    model_text = ax.text(0.8, 0.5, "📊", ha='center', fontsize=24, transform=ax.transAxes)
    
    # Arrows
    query_arrow = ax.annotate("", xy=(0.75, 0.52), xytext=(0.25, 0.52), 
                           arrowprops=dict(arrowstyle="->", color='#4682b4', alpha=0), 
                           xycoords=ax.transAxes, textcoords=ax.transAxes)
    
    response_arrow = ax.annotate("", xy=(0.25, 0.48), xytext=(0.75, 0.48), 
                              arrowprops=dict(arrowstyle="->", color='#2f4f4f', alpha=0), 
                              xycoords=ax.transAxes, textcoords=ax.transAxes)
    
    # Footer text
    footer = ax.text(0.5, 0.1, "", ha='center', fontsize=10, fontstyle='italic', 
                  transform=ax.transAxes, alpha=0)
    
    # Stages for each query type: 
    # 0-starting, 1-query appears, 2-query flies, 3-processing, 4-response appears, 5-response flies, 6-complete
    frames_per_stage = 5
    total_stages = 7
    
    def init():
        query_text.set_text("")
        query_text.get_bbox_patch().set_alpha(0)
        response_text.set_text("")
        response_text.get_bbox_patch().set_alpha(0)
        query_arrow.arrow_patch.set_alpha(0)
        response_arrow.arrow_patch.set_alpha(0)
        footer.set_alpha(0)
        return query_text, response_text, query_arrow, response_arrow, footer
    
    def update(frame):
        # Calculate which query we're on and which stage
        query_idx = frame // (frames_per_stage * total_stages)
        stage = (frame % (frames_per_stage * total_stages)) // frames_per_stage
        
        if query_idx >= len(query_types):
            # Animation complete, show all interactions
            summary = "Model provides a complete interactive API"
            footer.set_text(summary)
            footer.set_alpha(1.0)
            return query_text, response_text, query_arrow, response_arrow, footer
        
        current_query = query_texts[query_idx]
        current_response = response_texts[query_idx]
        
        # Reset all states
        query_text.get_bbox_patch().set_alpha(0)
        response_text.get_bbox_patch().set_alpha(0)
        query_arrow.arrow_patch.set_alpha(0)
        response_arrow.arrow_patch.set_alpha(0)
        
        # Process current stage
        if stage == 0:  # Starting state
            query_text.set_text("")
            response_text.set_text("")
            footer.set_text(f"Query type: {query_types[query_idx]}")
            footer.set_alpha(1.0)
        
        elif stage == 1:  # Query appears
            query_text.set_text(f"Query: {current_query}")
            query_text.get_bbox_patch().set_alpha(0.8)
        
        elif stage == 2:  # Query flies to model
            query_text.set_text(f"Query: {current_query}")
            query_text.get_bbox_patch().set_alpha(0.8)
            query_arrow.arrow_patch.set_alpha(1.0)
        
        elif stage == 3:  # Processing
            query_text.set_text(f"Query: {current_query}")
            query_text.get_bbox_patch().set_alpha(0.8)
            model_text.set_text("🔄")  # Processing icon
        
        elif stage == 4:  # Response appears
            query_text.set_text(f"Query: {current_query}")
            query_text.get_bbox_patch().set_alpha(0.8)
            response_text.set_text(f"Response: {current_response}")
            response_text.get_bbox_patch().set_alpha(0.8)
            model_text.set_text("📊")  # Back to model icon
        
        elif stage == 5:  # Response flies to user
            query_text.set_text(f"Query: {current_query}")
            query_text.get_bbox_patch().set_alpha(0.8)
            response_text.set_text(f"Response: {current_response}")
            response_text.get_bbox_patch().set_alpha(0.8)
            response_arrow.arrow_patch.set_alpha(1.0)
        
        elif stage == 6:  # Complete
            query_text.set_text(f"Query: {current_query}")
            query_text.get_bbox_patch().set_alpha(0.8)
            response_text.set_text(f"Response: {current_response}")
            response_text.get_bbox_patch().set_alpha(0.8)
            footer.set_text(f"Completed {query_types[query_idx]} interaction")
            
        return query_text, response_text, query_arrow, response_arrow, footer
    
    # Create animation
    frames = frames_per_stage * total_stages * len(query_types) + 5  # Add a few extra frames at the end
    anim = FuncAnimation(fig, update, frames=frames, init_func=init, blit=True, interval=200)
    
    # Save animation
    anim.save(interaction_anim_path, writer='pillow', fps=5, dpi=100)
    plt.close(fig)
    
    assert os.path.exists(interaction_anim_path), "VOCATIVE interaction animation not created"
    
    # 6. Document all outputs for this case
    with open(os.path.join(case_dir, "vocative_results.txt"), 'w') as f:
        f.write(f"VOCATIVE CASE RESULTS\n")
        f.write(f"=====================\n\n")
        f.write(f"Linguistic meaning: {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n")
        f.write(f"Regression context: {case_info['regression_context']}\n\n")
        
        f.write(f"Interface Queries:\n")
        f.write(f"- predict: Model predicts value at X=2.5: {prediction_response['predictions'][0]:.6f}\n")
        
        f.write(f"\nModel Parameters (via interface):\n")
        f.write(f"- Intercept: {params_response['parameters']['intercept']:.6f}\n")
        for i, coef in enumerate(params_response['parameters']['coefficients']):
            f.write(f"- Coefficient {i+1}: {coef:.6f}\n")
        
        f.write(f"\nEvaluation Metrics (via interface):\n")
        for name, value in eval_response['evaluation'].items():
            f.write(f"- {name}: {value:.6f}\n")
            
        f.write(f"\nLinguistic Formula Example:\n")
        f.write(f"{case_info['example']}\n\n")
        
        f.write(f"\nVisualizations Generated:\n")
        f.write(f"- Linguistic context: linguistic_context.png\n")
        f.write(f"- Interface visualization: interface_visualization.png\n")
        f.write(f"- Interaction animation: interaction_animation.gif\n")
        
        f.write(f"\nVOCATIVE Case Description:\n")
        f.write(f"The VOCATIVE case represents the model as an addressable entity that responds to\n")
        f.write(f"specific queries through well-defined interfaces. In natural language, this parallels\n")
        f.write(f"direct address or calling something by name ('Hey Siri...'). For regression models,\n")
        f.write(f"this case emphasizes the interactive API aspects of the model, with clear query-response\n")
        f.write(f"patterns and interface conventions. The model is treated as an entity that can be\n")
        f.write(f"directly addressed, queried, and will respond with appropriate information.\n")
            
    logger.info(f"Completed VOCATIVE case test with visualizations in {case_dir}")
    
    return model

def test_locative_case(linear_test_data, case_definitions):
    """
    Test LOCATIVE case: The model as location or context.
    
    In linguistics: Indicates location or context.
    In regression: The model as statistical context or assumption framework.
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
    
    # Get test data
    X, y = linear_test_data
    
    # 1. First, fit a model to generate residuals and context information
    fitted_model = LinearRegressionModel(model_id="fit_for_loc", case=Case.NOMINATIVE).fit(X, y)
    
    # 2. Create a LOCATIVE case model as context provider
    model = LinearRegressionModel(model_id=f"{Case.LOCATIVE.value}_model", case=Case.LOCATIVE)
    
    # Transfer parameters to LOCATIVE model
    model.intercept_ = fitted_model.intercept_
    model.coefficients_ = fitted_model.coefficients_
    model.is_trained = True
    model.X_train = X
    model.y_train = y
    
    # 3. Get prediction and compute residuals for context/assumptions
    y_pred = model.predict(X)
    residuals = y - y_pred
    
    # 4. Check assumptions
    assumption_results = model._check_assumptions()
    logger.info(f"LOCATIVE case demonstration: Statistical context and assumptions")
    
    # 5. Create visualization of statistical context (assumptions)
    context_vis_path = os.path.join(case_dir, "statistical_context_visualization.png")
    
    # Create multi-panel visualization of assumptions
    fig, axs = plt.subplots(2, 2, figsize=(12, 10))
    axs = axs.flatten()
    
    # Plot 1: Residuals vs Fitted (for homoscedasticity)
    axs[0].scatter(y_pred, residuals, alpha=0.6, color='blue')
    axs[0].axhline(y=0, color='red', linestyle='--')
    axs[0].set_xlabel("Fitted values")
    axs[0].set_ylabel("Residuals")
    axs[0].set_title("Homoscedasticity Assumption\nResiduals vs Fitted")
    
    # Add status label
    is_homoscedastic = assumption_results.get('homoscedasticity', False)
    status_color = 'green' if is_homoscedastic else 'red'
    status_text = 'Satisfied' if is_homoscedastic else 'Violated'
    axs[0].text(0.05, 0.05, f"Status: {status_text}", transform=axs[0].transAxes,
               fontsize=10, fontweight='bold', color=status_color,
               bbox=dict(boxstyle="round,pad=0.3", facecolor='white', alpha=0.8))
    
    # Plot 2: Q-Q plot for normality
    stats.probplot(residuals.flatten(), plot=axs[1])
    axs[1].set_title("Normality Assumption\nQ-Q Plot")
    
    # Add status label
    is_normal = assumption_results.get('normality', False)
    status_color = 'green' if is_normal else 'red'
    status_text = 'Satisfied' if is_normal else 'Violated'
    axs[1].text(0.05, 0.05, f"Status: {status_text}", transform=axs[1].transAxes,
               fontsize=10, fontweight='bold', color=status_color,
               bbox=dict(boxstyle="round,pad=0.3", facecolor='white', alpha=0.8))
    
    # Plot 3: Linearity assumption
    axs[2].scatter(X.flatten(), y, alpha=0.6, color='blue', label='Data')
    axs[2].plot(X.flatten(), y_pred, color='red', linestyle='-', label='Linear fit')
    axs[2].set_xlabel("X")
    axs[2].set_ylabel("y")
    axs[2].set_title("Linearity Assumption")
    axs[2].legend()
    
    # Add status label
    is_linear = assumption_results.get('linearity', False)
    status_color = 'green' if is_linear else 'red'
    status_text = 'Satisfied' if is_linear else 'Violated'
    axs[2].text(0.05, 0.05, f"Status: {status_text}", transform=axs[2].transAxes,
               fontsize=10, fontweight='bold', color=status_color,
               bbox=dict(boxstyle="round,pad=0.3", facecolor='white', alpha=0.8))
    
    # Plot 4: Independence of errors (Durbin-Watson like visualization)
    # Plot lagged residuals
    if len(residuals) > 1:
        axs[3].scatter(residuals[:-1], residuals[1:], alpha=0.6, color='blue')
        axs[3].set_xlabel("Residual_t")
        axs[3].set_ylabel("Residual_t+1")
        axs[3].set_title("Independence Assumption\nResiduals vs Lagged Residuals")
        
        # Add status label
        is_independent = assumption_results.get('independence', False)
        status_color = 'green' if is_independent else 'red'
        status_text = 'Satisfied' if is_independent else 'Violated'
        axs[3].text(0.05, 0.05, f"Status: {status_text}", transform=axs[3].transAxes,
                   fontsize=10, fontweight='bold', color=status_color,
                   bbox=dict(boxstyle="round,pad=0.3", facecolor='white', alpha=0.8))
    else:
        axs[3].text(0.5, 0.5, "Insufficient data for independence test", 
                   ha='center', va='center', transform=axs[3].transAxes)
    
    # Overall title
    fig.suptitle("LOCATIVE Case: Statistical Context and Assumptions", fontsize=16)
    
    # Add annotation about LOCATIVE case
    fig.text(0.5, 0.01, "The pattern exists WITHIN the LINEAR MODEL's assumptions (LOCATIVE case)", 
             ha='center', fontsize=12, fontweight='bold',
             bbox=dict(boxstyle="round,pad=0.3", facecolor='white', alpha=0.7))
    
    fig.tight_layout(rect=[0, 0.03, 1, 0.95])
    fig.savefig(context_vis_path, dpi=300)
    plt.close(fig)
    
    assert os.path.exists(context_vis_path), "LOCATIVE context visualization not created"
    
    # 6. Create animation showing the assumptions and statistical context
    assumptions_anim_path = os.path.join(case_dir, "statistical_context_animation.gif")
    
    # Create figure for animation
    fig = plt.figure(figsize=(14, 8))
    gs = GridSpec(2, 3, height_ratios=[2, 1], width_ratios=[1, 1, 1])
    
    # Three main assumption plots
    ax_resid = fig.add_subplot(gs[0, 0])  # Residuals vs Fitted
    ax_qq = fig.add_subplot(gs[0, 1])     # Q-Q Plot
    ax_fit = fig.add_subplot(gs[0, 2])    # Linearity plot
    
    # Bottom row for stats and explanation
    ax_stats = fig.add_subplot(gs[1, :2])  # Stats summary
    ax_phase = fig.add_subplot(gs[1, 2])   # Current phase indicator
    
    # Configure main plot areas
    ax_resid.set_title("Homoscedasticity")
    ax_resid.set_xlabel("Fitted values")
    ax_resid.set_ylabel("Residuals")
    
    ax_qq.set_title("Normality of Residuals")
    
    ax_fit.set_title("Linearity")
    ax_fit.set_xlabel("X")
    ax_fit.set_ylabel("y")
    
    # Configure stats area
    ax_stats.set_title("Statistical Context")
    ax_stats.axis('off')
    
    # Configure phase indicator
    ax_phase.set_title("Current Focus")
    ax_phase.axis('off')
    
    # Main title
    fig.suptitle("LOCATIVE Case: Model as Statistical Context", fontsize=16)
    
    # Footer text
    footer_text = fig.text(0.5, 0.01, 
                         "Statistical patterns exist WITHIN the LINEAR MODEL's context (LOCATIVE case)", 
                         ha='center', fontsize=12, fontweight='bold',
                         bbox=dict(boxstyle="round,pad=0.3", facecolor='white', alpha=0.7))
    
    # Create text objects for updating
    stats_text = ax_stats.text(0.05, 0.95, "", va='top', transform=ax_stats.transAxes,
                             fontsize=10, 
                             bbox=dict(boxstyle='round,pad=0.5', facecolor='wheat', alpha=0.5))
    
    phase_text = ax_phase.text(0.5, 0.5, "", ha='center', va='center', 
                             transform=ax_phase.transAxes, fontsize=14, fontweight='bold',
                             bbox=dict(boxstyle='round,pad=0.7', facecolor='lightblue', alpha=0.7))
    
    # Static data for the animation
    X_flat = X.flatten()
    y_pred_flat = y_pred.flatten()
    residuals_flat = residuals.flatten()
    
    # Prepare data for Q-Q plot
    theoretical_quantiles = np.linspace(-3, 3, len(residuals_flat))
    sorted_residuals = np.sort(residuals_flat)
    
    # Create scatter objects for updating
    resid_scatter = ax_resid.scatter([], [], alpha=0.6, color='blue')
    fit_scatter = ax_fit.scatter([], [], alpha=0.6, color='blue', label='Data')
    qq_scatter = ax_qq.scatter([], [], alpha=0.6, color='blue')
    
    # Create line objects
    qq_line, = ax_qq.plot([], [], 'r--', linewidth=1)
    fit_line, = ax_fit.plot([], [], 'r-', linewidth=2, label='Linear fit')
    resid_line = ax_resid.axhline(y=0, color='red', linestyle='--')
    
    # Set limits for the plots
    ax_resid.set_xlim(y_pred_flat.min() - 0.1, y_pred_flat.max() + 0.1)
    ax_resid.set_ylim(residuals_flat.min() - 0.1, residuals_flat.max() + 0.1)
    
    ax_qq.set_xlim(-3.2, 3.2)
    ax_qq.set_ylim(sorted_residuals.min() - 0.1, sorted_residuals.max() + 0.1)
    
    ax_fit.set_xlim(X_flat.min() - 0.1, X_flat.max() + 0.1)
    ax_fit.set_ylim(y.min() - 0.1, y.max() + 0.1)
    ax_fit.legend()
    
    # Number of frames for the animation
    n_frames = 120
    
    # Initialize function for animation
    def init():
        resid_scatter.set_offsets(np.empty((0, 2)))
        fit_scatter.set_offsets(np.empty((0, 2)))
        qq_scatter.set_offsets(np.empty((0, 2)))
        qq_line.set_data([], [])
        fit_line.set_data([], [])
        stats_text.set_text("")
        phase_text.set_text("")
        return resid_scatter, fit_scatter, qq_scatter, qq_line, fit_line, stats_text, phase_text
    
    # Update function for animation
    def update(frame):
        progress = frame / n_frames
        
        if progress < 0.25:  # Phase 1: Introduce the model and data
            phase_progress = progress / 0.25
            n_points = int(phase_progress * len(X_flat))
            
            # Update data points gradually
            if n_points > 0:
                # For fit plot
                fit_scatter.set_offsets(np.column_stack([X_flat[:n_points], y[:n_points]]))
                
                # Start revealing the fit line
                x_line = np.linspace(X_flat.min(), X_flat.max(), 100)
                y_line = model.intercept_ + model.coefficients_[0] * x_line
                fit_line.set_data(x_line, y_line)
                
                # No residuals yet
                resid_scatter.set_offsets(np.empty((0, 2)))
                qq_scatter.set_offsets(np.empty((0, 2)))
                qq_line.set_data([], [])
            
            # Update text
            stats_text.set_text(
                f"Model Parameters:\n"
                f"Intercept: {model.intercept_:.4f}\n"
                f"Coefficient: {model.coefficients_[0]:.4f}\n\n"
                f"Data Points Loaded: {n_points}/{len(X_flat)}\n"
                f"Linear Regression Context"
            )
            
            phase_text.set_text("Data & Model Setup")
            
        elif progress < 0.5:  # Phase 2: Introduce residuals and homoscedasticity
            phase_progress = (progress - 0.25) / 0.25
            n_points = int(phase_progress * len(X_flat))
            
            # Show all data points in fit plot
            fit_scatter.set_offsets(np.column_stack([X_flat, y]))
            fit_line.set_data(np.linspace(X_flat.min(), X_flat.max(), 100), 
                            model.intercept_ + model.coefficients_[0] * np.linspace(X_flat.min(), X_flat.max(), 100))
            
            # Gradually show residuals
            if n_points > 0:
                resid_scatter.set_offsets(np.column_stack([y_pred_flat[:n_points], residuals_flat[:n_points]]))
            
            # Still no QQ plot
            qq_scatter.set_offsets(np.empty((0, 2)))
            qq_line.set_data([], [])
            
            # Update text
            stats_text.set_text(
                f"Homoscedasticity Assumption:\n"
                f"Constant variance across fitted values\n\n"
                f"Residual Statistics:\n"
                f"Mean: {np.mean(residuals_flat[:max(1, n_points)]):.4f}\n"
                f"Std Dev: {np.std(residuals_flat[:max(1, n_points)]):.4f}\n"
                f"Points Analyzed: {n_points}/{len(residuals_flat)}"
            )
            
            phase_text.set_text("Homoscedasticity\nAssumption")
            
        elif progress < 0.75:  # Phase 3: Introduce normality assumption with QQ plot
            phase_progress = (progress - 0.5) / 0.25
            n_points = int(phase_progress * len(residuals_flat))
            
            # Keep all points in other plots
            resid_scatter.set_offsets(np.column_stack([y_pred_flat, residuals_flat]))
            fit_scatter.set_offsets(np.column_stack([X_flat, y]))
            fit_line.set_data(np.linspace(X_flat.min(), X_flat.max(), 100), 
                            model.intercept_ + model.coefficients_[0] * np.linspace(X_flat.min(), X_flat.max(), 100))
            
            # Gradually reveal QQ plot
            if n_points > 0:
                # For QQ plot - plot theoretical quantiles vs sorted residuals
                qq_scatter.set_offsets(np.column_stack([theoretical_quantiles[:n_points], sorted_residuals[:n_points]]))
                
                # Add the reference line
                # Use min/max of the visible portion to define the line
                if n_points >= 2:
                    visible_quant = theoretical_quantiles[:n_points]
                    visible_resid = sorted_residuals[:n_points]
                    
                    # Simple linear fit for the reference line
                    slope = np.std(visible_resid) / np.std(visible_quant)
                    intercept = np.mean(visible_resid) - slope * np.mean(visible_quant)
                    
                    # Draw the line across the full range
                    line_x = np.array([-3, 3])
                    line_y = intercept + slope * line_x
                    qq_line.set_data(line_x, line_y)
            
            # Update text
            if n_points > 0:
                # Perform Shapiro-Wilk test on the visible portion
                if n_points >= 3:  # Minimum required for the test
                    _, p_value = stats.shapiro(residuals_flat[:n_points])
                    normality_result = f"Shapiro-Wilk p-value: {p_value:.4f}"
                    normality_status = "Normal" if p_value > 0.05 else "Non-normal"
                else:
                    normality_result = "Insufficient data for test"
                    normality_status = "Unknown"
                
                stats_text.set_text(
                    f"Normality Assumption:\n"
                    f"Residuals should follow normal distribution\n\n"
                    f"QQ Plot Analysis:\n"
                    f"{normality_result}\n"
                    f"Status: {normality_status}\n"
                    f"Points Analyzed: {n_points}/{len(residuals_flat)}"
                )
            
            phase_text.set_text("Normality\nAssumption")
            
        else:  # Phase 4: Overall assumption summary and context
            # Keep all plots fully populated
            resid_scatter.set_offsets(np.column_stack([y_pred_flat, residuals_flat]))
            fit_scatter.set_offsets(np.column_stack([X_flat, y]))
            fit_line.set_data(np.linspace(X_flat.min(), X_flat.max(), 100), 
                            model.intercept_ + model.coefficients_[0] * np.linspace(X_flat.min(), X_flat.max(), 100))
            qq_scatter.set_offsets(np.column_stack([theoretical_quantiles, sorted_residuals]))
            
            # Complete QQ reference line
            slope = np.std(sorted_residuals) / np.std(theoretical_quantiles)
            intercept = np.mean(sorted_residuals) - slope * np.mean(theoretical_quantiles)
            qq_line.set_data(np.array([-3, 3]), intercept + slope * np.array([-3, 3]))
            
            # Final summary text
            stats_text.set_text(
                f"Statistical Context (LOCATIVE Case):\n"
                f"Linearity: {'✓' if assumption_results.get('linearity', False) else '✗'}\n"
                f"Homoscedasticity: {'✓' if assumption_results.get('homoscedasticity', False) else '✗'}\n"
                f"Normality: {'✓' if assumption_results.get('normality', False) else '✗'}\n"
                f"Independence: {'✓' if assumption_results.get('independence', False) else '✗'}\n\n"
                f"Overall model validity depends on these assumptions."
            )
            
            phase_text.set_text("Complete Statistical\nContext")
        
        return resid_scatter, fit_scatter, qq_scatter, qq_line, fit_line, stats_text, phase_text
    
    # Create animation
    anim = FuncAnimation(fig, update, frames=n_frames, init_func=init, 
                        blit=False, interval=100)
    
    # Save animation
    anim.save(assumptions_anim_path, writer='pillow', fps=15, dpi=100)
    plt.close(fig)
    
    assert os.path.exists(assumptions_anim_path), "LOCATIVE assumptions animation not created"
    
    # 7. Create results summary file
    results_path = os.path.join(case_dir, "locative_results.txt")
    with open(results_path, 'w') as f:
        f.write(f"LOCATIVE CASE RESULTS\n")
        f.write(f"====================\n\n")
        f.write(f"Linguistic meaning: {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n")
        f.write(f"Regression context: {case_info['regression_context']}\n\n")
        
        f.write(f"Statistical Assumptions Analysis:\n")
        for assumption, result in assumption_results.items():
            f.write(f"- {assumption.capitalize()}: {'Satisfied' if result else 'Violated'}\n")
        
        f.write(f"\nResidual Statistics:\n")
        f.write(f"- Mean: {float(np.mean(residuals)):.6f}\n")
        f.write(f"- Std Dev: {float(np.std(residuals)):.6f}\n")
        f.write(f"- Min: {float(np.min(residuals)):.6f}\n")
        f.write(f"- Max: {float(np.max(residuals)):.6f}\n\n")
        
        f.write(f"Linguistic Formula Example:\n")
        f.write(f"{case_info['example']}\n\n")
        
        f.write(f"Visualizations:\n")
        f.write(f"- Statistical context: statistical_context_visualization.png\n")
        f.write(f"- Assumptions animation: statistical_context_animation.gif\n")
    
    logger.info(f"Completed LOCATIVE case test with visualizations in {case_dir}")
    
    return model

def test_ablative_case(linear_test_data, case_definitions):
    """
    Test ABLATIVE case: The model as origin or causal factor.
    
    In linguistics: Indicates movement away from, origin, or cause.
    In regression: The model as source of error or origin of effects.
    """
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
    
    # Get test data
    X, y = linear_test_data
    
    # 1. First, fit a model to generate the basis for error analysis
    fitted_model = LinearRegressionModel(model_id="fit_for_abl", case=Case.NOMINATIVE).fit(X, y)
    
    # 2. Create ABLATIVE case model as error source/origin
    model = LinearRegressionModel(model_id=f"{Case.ABLATIVE.value}_model", case=Case.ABLATIVE)
    
    # Transfer parameters to the ABLATIVE model for analysis
    model.intercept_ = fitted_model.intercept_
    model.coefficients_ = fitted_model.coefficients_
    model.is_trained = True
    model.X_train = X
    model.y_train = y
    
    # 3. Generate predictions to analyze residuals
    y_pred = model.predict(X)
    residuals = y - y_pred
    
    # 4. Use ABLATIVE case to analyze residuals (model as source of errors)
    residual_analysis = model._analyze_residuals()
    logger.info(f"ABLATIVE case demonstration: Model as source of errors and effects")
    
    # 5. Create visualization of error sources
    error_vis_path = os.path.join(case_dir, "error_source_visualization.png")
    
    # Complex visualization with multiple panels
    fig, axs = plt.subplots(2, 2, figsize=(12, 10))
    axs = axs.flatten()
    
    # Plot 1: Residuals as they "come from" the model
    axs[0].scatter(X.flatten(), residuals, alpha=0.6, color='red')
    axs[0].axhline(y=0, color='blue', linestyle='--')
    axs[0].set_xlabel("X")
    axs[0].set_ylabel("Residual")
    axs[0].set_title("Errors Originating FROM the Model")
    
    # Plot 2: Histogram of residuals with distribution fit
    sns.histplot(residuals, kde=True, ax=axs[1], color='red', alpha=0.6)
    axs[1].set_xlabel("Residual Value")
    axs[1].set_ylabel("Frequency")
    axs[1].set_title("Distribution of Errors")
    
    # Add vertical line for mean residual
    mean_residual = np.mean(residuals)
    axs[1].axvline(x=mean_residual, color='blue', linestyle='--', 
                  label=f'Mean: {mean_residual:.4f}')
    axs[1].legend()
    
    # Plot 3: Actual vs Predicted with error bars
    axs[2].scatter(y_pred, y, alpha=0.6, color='blue')
    
    # Add perfect prediction line
    min_val = min(y_pred.min(), y.min())
    max_val = max(y_pred.max(), y.max())
    axs[2].plot([min_val, max_val], [min_val, max_val], 'k--', alpha=0.5)
    
    # Add error bars for a few points to emphasize errors coming from model
    sample_indices = np.random.choice(len(y_pred), size=10, replace=False)
    for idx in sample_indices:
        axs[2].plot([y_pred[idx], y_pred[idx]], [y_pred[idx], y[idx]], 
                   'r-', alpha=0.7)
    
    axs[2].set_xlabel("Predicted Values")
    axs[2].set_ylabel("Actual Values")
    axs[2].set_title("Errors as Deviations from Predictions")
    
    # Plot 4: Error decomposition (if available in residual_analysis)
    outlier_indices = residual_analysis.get('outlier_indices', [])
    problem_features = residual_analysis.get('problem_features', {})
    
    # Create a pie chart for error sources
    error_sources = {
        'Random Variation': 1.0,  # Start with all error as random
    }
    
    # Adjust based on analysis
    if outlier_indices:
        # Assign some error to outliers based on their proportion
        outlier_weight = min(0.4, len(outlier_indices) / len(residuals))
        error_sources['Random Variation'] -= outlier_weight
        error_sources['Outliers'] = outlier_weight
    
    if problem_features:
        # Assign some error to feature issues
        feature_weight = min(0.4, len(problem_features) / max(1, X.shape[1]))
        error_sources['Random Variation'] -= feature_weight
        error_sources['Feature Issues'] = feature_weight
    
    # If heteroscedasticity detected, add as error source
    if not residual_analysis.get('is_homoscedastic', True):
        error_sources['Random Variation'] -= 0.2
        error_sources['Heteroscedasticity'] = 0.2
    
    # Create the pie chart
    axs[3].pie(error_sources.values(), labels=error_sources.keys(), autopct='%1.1f%%',
              shadow=True, startangle=90)
    axs[3].set_title("Error Source Attribution")
    axs[3].axis('equal')  # Equal aspect ratio ensures the pie chart is circular
    
    # Overall title
    fig.suptitle("ABLATIVE Case: Model as Source of Errors", fontsize=16)
    
    # Add annotation about ABLATIVE case
    fig.text(0.5, 0.01, "The prediction errors COME FROM the LINEAR MODEL (ABLATIVE case)", 
             ha='center', fontsize=12, fontweight='bold',
             bbox=dict(boxstyle="round,pad=0.3", facecolor='white', alpha=0.7))
    
    fig.tight_layout(rect=[0, 0.03, 1, 0.95])
    fig.savefig(error_vis_path, dpi=300)
    plt.close(fig)
    
    assert os.path.exists(error_vis_path), "ABLATIVE error source visualization not created"
    
    # 6. Create an animation showing errors originating from the model
    error_anim_path = os.path.join(case_dir, "error_origin_animation.gif")
    
    # Create figure for animation
    fig = plt.figure(figsize=(14, 8))
    gs = GridSpec(2, 2, height_ratios=[2, 1])
    
    # Main plot for predictions and errors
    ax_main = fig.add_subplot(gs[0, :])
    ax_main.set_title("Errors Originating FROM the Model")
    ax_main.set_xlabel("X")
    ax_main.set_ylabel("y")
    ax_main.grid(True, alpha=0.3)
    
    # Plot for error histogram
    ax_hist = fig.add_subplot(gs[1, 0])
    ax_hist.set_title("Error Distribution")
    ax_hist.set_xlabel("Error Value")
    ax_hist.set_ylabel("Frequency")
    
    # Plot for error decomposition
    ax_decomp = fig.add_subplot(gs[1, 1])
    ax_decomp.set_title("Error Attribution")
    ax_decomp.axis('off')
    
    # Main title
    fig.suptitle("ABLATIVE Case: Model as Source of Errors", fontsize=16)
    
    # Footer with case explanation
    footer_text = fig.text(0.5, 0.01, 
                         "The prediction errors COME FROM the LINEAR MODEL (ABLATIVE case)", 
                         ha='center', fontsize=12, fontweight='bold',
                         bbox=dict(boxstyle="round,pad=0.3", facecolor='white', alpha=0.7))
    
    # Create static elements
    ax_main.scatter(X.flatten(), y, color='blue', alpha=0.5, s=40, label='Actual Data')
    model_line, = ax_main.plot([], [], 'r-', linewidth=2, label='Model Prediction')
    error_lines = []
    
    # Set number of error lines to show (not all to avoid clutter)
    n_error_lines = min(30, len(X))
    for _ in range(n_error_lines):
        line, = ax_main.plot([], [], 'r-', alpha=0.3)
        error_lines.append(line)
    
    # Create histogram bars (will be updated)
    hist_bars = ax_hist.bar([], [], alpha=0.6, color='red')
    hist_line = None  # Will be created later for KDE
    
    # Create text for decomposition
    decomp_text = ax_decomp.text(0.5, 0.5, "", ha='center', va='center', 
                               transform=ax_decomp.transAxes, fontsize=10,
                               bbox=dict(boxstyle='round,pad=0.5', facecolor='wheat', alpha=0.5))
    
    # Set up axes limits
    X_flat = X.flatten()
    ax_main.set_xlim(X_flat.min() - 0.1 * (X_flat.max() - X_flat.min()), 
                    X_flat.max() + 0.1 * (X_flat.max() - X_flat.min()))
    ax_main.set_ylim(y.min() - 0.1 * (y.max() - y.min()), 
                    y.max() + 0.1 * (y.max() - y.min()))
    
    # Legend for main plot
    ax_main.legend(loc='upper left')
    
    # Number of frames for animation
    n_frames = 150
    
    # Initialize function
    def init():
        model_line.set_data([], [])
        for line in error_lines:
            line.set_data([], [])
        decomp_text.set_text("")
        # Returns all artists that will be updated
        return [model_line, decomp_text] + error_lines
    
    # Update function
    def update(frame):
        progress = frame / n_frames
        
        if progress < 0.2:  # Phase 1: Introduce the model
            phase_progress = progress / 0.2
            
            # Gradually draw the model prediction line
            x_line = np.linspace(X_flat.min(), X_flat.max(), 100)
            y_line = model.intercept_ + model.coefficients_[0] * x_line
            
            # Only show part of the line based on progress
            visible_points = max(2, int(phase_progress * len(x_line)))
            model_line.set_data(x_line[:visible_points], y_line[:visible_points])
            
            # No error lines yet
            for line in error_lines:
                line.set_data([], [])
            
            # Update decomposition text
            decomp_text.set_text(f"Phase 1: Model Introduction\n\n"
                               f"The model generates predictions\n"
                               f"y = {model.intercept_:.4f} + {model.coefficients_[0]:.4f}·x\n\n"
                               f"Progress: {int(phase_progress*100)}%")
            
            # No histogram yet
            ax_hist.clear()
            ax_hist.set_title("Error Distribution")
            ax_hist.set_xlabel("Error Value")
            ax_hist.set_ylabel("Frequency")
            
        elif progress < 0.4:  # Phase 2: Show errors emanating from model predictions
            phase_progress = (progress - 0.2) / 0.2
            
            # Show full model line
            x_line = np.linspace(X_flat.min(), X_flat.max(), 100)
            y_line = model.intercept_ + model.coefficients_[0] * x_line
            model_line.set_data(x_line, y_line)
            
            # Calculate all predictions 
            y_pred_flat = y_pred.flatten()
            
            # Gradually show error lines
            visible_errors = max(1, int(phase_progress * n_error_lines))
            
            # Sample indices to show (to prevent overcrowding)
            indices = np.round(np.linspace(0, len(X) - 1, n_error_lines)).astype(int)
            
            for i, line in enumerate(error_lines):
                if i < visible_errors:
                    idx = indices[i]
                    line.set_data([X_flat[idx], X_flat[idx]], [y_pred_flat[idx], y[idx]])
                else:
                    line.set_data([], [])
            
            # Update decomposition text
            error_magnitude = np.sum(np.abs(residuals[:indices[:visible_errors]]))
            decomp_text.set_text(f"Phase 2: Error Visualization\n\n"
                               f"Errors are the differences between\n"
                               f"actual and predicted values\n\n"
                               f"Error Lines: {visible_errors}/{n_error_lines}\n"
                               f"Current Error Magnitude: {error_magnitude:.4f}")
            
            # Start showing histogram
            if visible_errors > 5:  # Need some errors to show a histogram
                visible_residuals = residuals.flatten()[indices[:visible_errors]]
                ax_hist.clear()
                ax_hist.set_title("Error Distribution")
                ax_hist.set_xlabel("Error Value")
                ax_hist.set_ylabel("Frequency")
                ax_hist.set_xlim(residuals.min(), residuals.max())
                
                # Create histogram
                hist_vals, bins, _ = ax_hist.hist(visible_residuals, bins=10, 
                                               alpha=0.6, color='red')
                
                # Add vertical line for mean
                if len(visible_residuals) > 0:
                    mean_visible = np.mean(visible_residuals)
                    ax_hist.axvline(x=mean_visible, color='blue', linestyle='--',
                                   label=f'Mean: {mean_visible:.4f}')
                    ax_hist.legend()
            
        elif progress < 0.7:  # Phase 3: Analyze error patterns and sources
            phase_progress = (progress - 0.4) / 0.3
            
            # Show full model and all error lines
            x_line = np.linspace(X_flat.min(), X_flat.max(), 100)
            y_line = model.intercept_ + model.coefficients_[0] * x_line
            model_line.set_data(x_line, y_line)
            
            # Calculate all predictions 
            y_pred_flat = y_pred.flatten()
            
            # Sample indices to show
            indices = np.round(np.linspace(0, len(X) - 1, n_error_lines)).astype(int)
            
            # Show all error lines
            for i, line in enumerate(error_lines):
                idx = indices[i]
                line.set_data([X_flat[idx], X_flat[idx]], [y_pred_flat[idx], y[idx]])
            
            # Full histogram
            ax_hist.clear()
            ax_hist.set_title("Error Distribution")
            ax_hist.set_xlabel("Error Value")
            ax_hist.set_ylabel("Frequency")
            
            # Create histogram with KDE
            hist_vals, bins, _ = ax_hist.hist(residuals.flatten(), bins=15, 
                                           alpha=0.6, color='red', density=True,
                                           label="Errors")
            
            # Add KDE line
            kde_x = np.linspace(residuals.min(), residuals.max(), 100)
            kde = stats.gaussian_kde(residuals.flatten())
            kde_y = kde(kde_x)
            ax_hist.plot(kde_x, kde_y, 'b-', linewidth=2, label="Distribution")
            
            # Add vertical line for mean
            mean_residual = np.mean(residuals)
            ax_hist.axvline(x=mean_residual, color='blue', linestyle='--',
                           label=f'Mean: {mean_residual:.4f}')
            ax_hist.legend()
            
            # Gradually reveal error analysis
            analysis_progress = int(phase_progress * 100)
            
            # Identifying different error sources in phases
            if phase_progress < 0.33:  # First third: basic metrics
                decomp_text.set_text(f"Phase 3: Error Analysis\n\n"
                                   f"Basic Error Metrics:\n"
                                   f"Mean Error: {mean_residual:.4f}\n"
                                   f"Std Dev: {np.std(residuals):.4f}\n"
                                   f"Min: {np.min(residuals):.4f}\n"
                                   f"Max: {np.max(residuals):.4f}\n"
                                   f"Analysis Progress: {analysis_progress}%")
                
            elif phase_progress < 0.66:  # Second third: outlier detection
                decomp_text.set_text(f"Phase 3: Error Analysis\n\n"
                                   f"Outlier Detection:\n"
                                   f"Total Points: {len(residuals)}\n"
                                   f"Outliers Found: {len(outlier_indices)}\n"
                                   f"Outlier %: {len(outlier_indices)/len(residuals)*100:.1f}%\n"
                                   f"Analysis Progress: {analysis_progress}%")
                
            else:  # Final third: heteroscedasticity check
                hetero_status = "Present" if not residual_analysis.get('is_homoscedastic', True) else "Not Detected"
                decomp_text.set_text(f"Phase 3: Error Analysis\n\n"
                                   f"Error Pattern Analysis:\n"
                                   f"Heteroscedasticity: {hetero_status}\n"
                                   f"Problem Features: {len(problem_features)}\n"
                                   f"Analysis Progress: {analysis_progress}%")
        
        else:  # Phase 4: Error attribution and source identification
            phase_progress = (progress - 0.7) / 0.3
            
            # Show full model and all error lines
            x_line = np.linspace(X_flat.min(), X_flat.max(), 100)
            y_line = model.intercept_ + model.coefficients_[0] * x_line
            model_line.set_data(x_line, y_line)
            
            # Keep all error lines visible
            y_pred_flat = y_pred.flatten()
            indices = np.round(np.linspace(0, len(X) - 1, n_error_lines)).astype(int)
            for i, line in enumerate(error_lines):
                idx = indices[i]
                line.set_data([X_flat[idx], X_flat[idx]], [y_pred_flat[idx], y[idx]])
            
            # Keep histogram visible
            ax_hist.clear()
            ax_hist.set_title("Error Distribution")
            ax_hist.set_xlabel("Error Value")
            ax_hist.set_ylabel("Frequency")
            
            # Create histogram with KDE
            hist_vals, bins, _ = ax_hist.hist(residuals.flatten(), bins=15, 
                                           alpha=0.6, color='red', density=True)
            
            # Add KDE line
            kde_x = np.linspace(residuals.min(), residuals.max(), 100)
            kde = stats.gaussian_kde(residuals.flatten())
            kde_y = kde(kde_x)
            ax_hist.plot(kde_x, kde_y, 'b-', linewidth=2)
            
            # Add vertical line for mean
            mean_residual = np.mean(residuals)
            ax_hist.axvline(x=mean_residual, color='blue', linestyle='--',
                           label=f'Mean: {mean_residual:.4f}')
            ax_hist.legend()
            
            # Final phase: error source attribution
            # Create a dynamic view of error attribution
            # Gradually build up the full text
            attribution_text = "Phase 4: Error Source Attribution\n\n"
            
            if phase_progress >= 0.25:
                attribution_text += f"Random Variation: {error_sources.get('Random Variation', 0)*100:.1f}%\n"
            
            if phase_progress >= 0.5 and 'Outliers' in error_sources:
                attribution_text += f"Outliers: {error_sources.get('Outliers', 0)*100:.1f}%\n"
            
            if phase_progress >= 0.75 and 'Feature Issues' in error_sources:
                attribution_text += f"Feature Issues: {error_sources.get('Feature Issues', 0)*100:.1f}%\n"
            
            if phase_progress >= 1.0 and 'Heteroscedasticity' in error_sources:
                attribution_text += f"Heteroscedasticity: {error_sources.get('Heteroscedasticity', 0)*100:.1f}%\n"
            
            # If we're at the end, add a conclusion
            if phase_progress >= 1.0:
                attribution_text += f"\nABLATIVE Case Analysis Complete\n"
                attribution_text += f"All errors originate FROM the model"
            
            decomp_text.set_text(attribution_text)
        
        # Returns all artists that are updated
        return [model_line, decomp_text] + error_lines
    
    # Create animation
    anim = FuncAnimation(fig, update, frames=n_frames, init_func=init, 
                        blit=False, interval=50)
    
    # Save animation
    anim.save(error_anim_path, writer='pillow', fps=15, dpi=100)
    plt.close(fig)
    
    assert os.path.exists(error_anim_path), "ABLATIVE error origin animation not created"
    
    # 7. Create results summary file
    results_path = os.path.join(case_dir, "ablative_results.txt")
    with open(results_path, 'w') as f:
        f.write(f"ABLATIVE CASE RESULTS\n")
        f.write(f"====================\n\n")
        f.write(f"Linguistic meaning: {case_info['linguistic_meaning']}\n")
        f.write(f"Statistical role: {case_info['statistical_role']}\n")
        f.write(f"Regression context: {case_info['regression_context']}\n\n")
        
        f.write(f"Error Source Analysis:\n")
        f.write(f"- Mean Residual: {float(np.mean(residuals)):.6f}\n")
        f.write(f"- Std Deviation: {float(np.std(residuals)):.6f}\n")
        f.write(f"- Outlier Count: {len(residual_analysis.get('outlier_indices', []))}\n")
        f.write(f"- Is Homoscedastic: {residual_analysis.get('is_homoscedastic', False)}\n")
        
        f.write(f"\nError Attribution:\n")
        for source, value in error_sources.items():
            f.write(f"- {source}: {value*100:.1f}%\n")
        
        f.write(f"\nLinguistic Formula Example:\n")
        f.write(f"{case_info['example']}\n\n")
        
        f.write(f"Visualizations:\n")
        f.write(f"- Error source analysis: error_source_visualization.png\n")
        f.write(f"- Error origin animation: error_origin_animation.gif\n")
    
    logger.info(f"Completed ABLATIVE case test with visualizations in {case_dir}")
    
    return model

# --- Main function to run all tests for demo purposes ---

def run_all_case_tests(output_dir: str = OUTPUT_DIR) -> None:
    """
    Run all case-specific tests and generate the overview visualization.
    
    Args:
        output_dir: Directory to store all test outputs
    """
    logger.info("Running all CEREBRUM case tests for LinearRegressionModel")
    
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    # Create test fixtures
    case_definitions = CaseDefinitions.get_all_cases()
    linear_data = DataGenerator.linear_data(n_samples=150, slope=3.0, intercept=-2.0, noise_level=2.0)
    
    # Run all individual case tests
    test_nominative_case(linear_data, case_definitions)
    test_accusative_case(linear_data, case_definitions)
    test_dative_case(linear_data, case_definitions)
    test_genitive_case(linear_data, case_definitions)
    test_instrumental_case(linear_data, case_definitions)
    test_vocative_case(linear_data, case_definitions)
    test_locative_case(linear_data, case_definitions)
    test_ablative_case(linear_data, case_definitions)
    
    # Generate the overview visualization directly in the proper output directory
    Visualizer.create_overview_figure(
        save_path=os.path.join(output_dir, "cerebrum_cases_overview.png"),
        include_linguistics=True
    )
    
    # Generate a simplified overview without linguistic details
    Visualizer.create_overview_figure(
        save_path=os.path.join(output_dir, "cerebrum_cases_simplified.png"),
        include_linguistics=False
    )
    
    logger.info(f"All case tests completed. Results saved to {output_dir}")

# --- Helper Functions ---

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
    
    # Regression context
    context_box = dict(boxstyle='round,pad=0.5', facecolor='lightgreen', alpha=0.5)
    ax.text(0.5, 0.54, "Regression Context:", ha='center', va='top', 
            fontsize=12, fontweight='bold', transform=ax.transAxes)
    ax.text(0.5, 0.48, case_info['regression_context'], ha='center', va='top', 
            fontsize=12, wrap=True, transform=ax.transAxes)
    
    # Methods
    methods_box = dict(boxstyle='round,pad=0.5', facecolor='lightgray', alpha=0.5)
    ax.text(0.5, 0.36, f"Primary Methods: {case_info['primary_methods']}", 
            ha='center', va='top', fontsize=12, bbox=methods_box, 
            transform=ax.transAxes)
    
    # Visualization type
    vis_box = dict(boxstyle='round,pad=0.5', facecolor='lightcoral', alpha=0.5)
    ax.text(0.5, 0.24, f"Typical Visualization: {case_info['visualization']}", 
            ha='center', va='top', fontsize=12, bbox=vis_box, 
            transform=ax.transAxes)
    
    # Footer
    ax.text(0.5, 0.05, "CEREBRUM Framework: Linguistically-Informed Model Roles", 
            ha='center', va='bottom', fontsize=10, fontstyle='italic', 
            transform=ax.transAxes)
    
    # Save the figure
    fig.tight_layout()
    fig.savefig(save_path, dpi=300, bbox_inches='tight')
    plt.close(fig)
    logger.info(f"Saved linguistic context visualization for {case.value} case to {save_path}")


if __name__ == "__main__":
    # Run all tests when script is executed directly
    run_all_case_tests()