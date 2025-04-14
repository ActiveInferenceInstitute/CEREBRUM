#!/usr/bin/env python3
"""
Linear Regression Model for CEREBRUM
Implements a linear regression model with different linguistic cases
"""

import logging
import numpy as np
from typing import Dict, Any, Optional
from sklearn.metrics import r2_score, mean_squared_error, mean_absolute_error
from sklearn.linear_model import LinearRegression
from scipy import stats

from .base import Model, Case

# Setup logging
logger = logging.getLogger("cerebrum-linear-regression")

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
    
    def process_data_buffer(self) -> tuple:
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