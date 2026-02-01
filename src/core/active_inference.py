from typing import Dict, Any, Optional, List, Callable, Tuple, Union
import numpy as np
from scipy.stats import multivariate_normal
import logging

from src.core.model import Model, Case

logger = logging.getLogger(__name__)

class ActiveInferenceModel(Model):
    """
    Base class for active inference generative models in the CEREBRUM framework.
    
    This class extends the base Model with active inference capabilities,
    including variational free energy minimization, precision-weighted message passing,
    and prediction/error propagation.
    """
    
    def __init__(
        self, 
        name: str = None, 
        parameters: Dict[str, Any] = None,
        prior_means: np.ndarray = None,
        prior_precision: np.ndarray = None,
        likelihood_precision: np.ndarray = None
    ):
        """
        Initialize an ActiveInferenceModel with the necessary components.
        
        Args:
            name: A unique identifier for the model
            parameters: Initial parameters for the model
            prior_means: Prior expectations over states
            prior_precision: Prior precision (inverse covariance) over states
            likelihood_precision: Precision of the likelihood mapping
        """
        super().__init__(name, parameters)
        
        # Initialize model dimensions based on inputs or defaults
        self.n_states = 1
        self.n_observations = 1
        
        if prior_means is not None:
            self.n_states = len(prior_means)
        
        # Set up prior distributions
        self.prior_means = prior_means if prior_means is not None else np.zeros(self.n_states)
        
        if prior_precision is not None:
            if prior_precision.ndim == 1:
                self.prior_precision = np.diag(prior_precision)
            else:
                self.prior_precision = prior_precision
        else:
            self.prior_precision = np.eye(self.n_states)
        
        # Set up likelihood mapping
        if likelihood_precision is not None:
            if likelihood_precision.ndim == 1:
                self.likelihood_precision = np.diag(likelihood_precision)
            else:
                self.likelihood_precision = likelihood_precision
        else:
            self.likelihood_precision = np.eye(self.n_observations)
        
        # Posterior beliefs (these will be updated during inference)
        self.posterior_means = self.prior_means.copy()
        self.posterior_precision = self.prior_precision.copy()
        
        # Case-specific parameter access patterns
        self._setup_case_configurations()
        
        # History for tracking belief updates
        self.belief_history = []
        
        # Prediction errors and free energy history
        self.prediction_errors = []
        self.free_energy_history = []
        
        # Ensure required parameters are available (if parameters dict was provided)
        # This is optional - models can be initialized with prior_means/prior_precision directly
        if parameters is not None:
            required_params = ['transition_matrix', 'observation_matrix', 'n_states', 'n_actions', 'n_observations']
            missing_params = [p for p in required_params if p not in parameters]
            if missing_params:
                logger.debug(f"Optional params not provided: {missing_params}. Using default initialization.")
            else:
                # Initialize posterior (belief state) from parameters
                self.posterior_means = np.ones(parameters['n_states']) / parameters['n_states']
        
        # By default, set to NOMINATIVE case (model as active agent)
        self.case = Case.NOMINATIVE
        
        logger.info(f"Initialized ActiveInferenceModel '{name}'")
    
    def _setup_case_configurations(self):
        """Set up case-specific configurations for parameter access."""
        # NOMINATIVE: All parameters fully accessible for prediction generation
        self._case_configurations[Case.NOMINATIVE] = {
            "params_accessible": True,
            "precision_focus": "likelihood",
            "interface_mode": "prediction_generation"
        }
        
        # ACCUSATIVE: Restricted parameter access, focus on updates
        self._case_configurations[Case.ACCUSATIVE] = {
            "params_accessible": False,
            "precision_focus": "parameters",
            "interface_mode": "update_reception"
        }
        
        # GENITIVE: Output-focused, generative pathway emphasis
        self._case_configurations[Case.GENITIVE] = {
            "params_accessible": True,
            "precision_focus": "outputs",
            "interface_mode": "product_generation"
        }
        
        # DATIVE: Input-focused parameterization
        self._case_configurations[Case.DATIVE] = {
            "params_accessible": True,
            "precision_focus": "inputs",
            "interface_mode": "data_reception"
        }
        
        # INSTRUMENTAL: Method-oriented parameters
        self._case_configurations[Case.INSTRUMENTAL] = {
            "params_accessible": True,
            "precision_focus": "operations",
            "interface_mode": "process_execution"
        }
        
        # LOCATIVE: Context parameters emphasized
        self._case_configurations[Case.LOCATIVE] = {
            "params_accessible": True,
            "precision_focus": "contexts",
            "interface_mode": "context_provision"
        }
        
        # ABLATIVE: Origin states emphasized
        self._case_configurations[Case.ABLATIVE] = {
            "params_accessible": False,
            "precision_focus": "historical",
            "interface_mode": "source_reference"
        }
        
        # VOCATIVE: Identity parameters prioritized
        self._case_configurations[Case.VOCATIVE] = {
            "params_accessible": False,
            "precision_focus": "identity",
            "interface_mode": "direct_address"
        }
    
    def _apply_case_transformation(self):
        """Apply transformations when changing between cases."""
        # Each case transformation adjusts parameter access, precision weighting, etc.
        # based on the case-specific configurations
        
        # Save current state before transformation
        pre_transform_state = {
            "posterior_means": self.posterior_means.copy(),
            "posterior_precision": self.posterior_precision.copy()
        }
        
        # Adjust precision weights based on case focus
        precision_focus = self._case_configurations[self._case]["precision_focus"]
        
        if precision_focus == "likelihood":
            # Nominative case: Emphasize precision of generative mapping
            self.likelihood_precision *= self.get_precision(self._case)
        elif precision_focus == "parameters":
            # Accusative case: Emphasize precision of parameter updates
            pass  # This will be handled during update
        elif precision_focus == "inputs":
            # Dative case: Emphasize precision of incoming data
            pass  # This will be handled during data reception
        elif precision_focus == "outputs":
            # Genitive case: Emphasize precision of generated outputs
            pass  # This will be handled during output generation
        
        # Record the transformation in history
        self.belief_history.append({
            "from_case": self._prior_case,
            "to_case": self._case,
            "pre_transform": pre_transform_state,
            "post_transform": {
                "posterior_means": self.posterior_means.copy(),
                "posterior_precision": self.posterior_precision.copy()
            }
        })
    
    def likelihood(self, states: np.ndarray) -> np.ndarray:
        """
        Compute the likelihood mapping from states to observations.
        
        This is the generative model p(o|s) that maps from hidden states to observations.
        
        Args:
            states: The hidden states of the model
            
        Returns:
            The expected observations
        """
        # Default implementation assumes linear mapping - override in subclasses
        # for more complex likelihood mappings
        return states
    
    def prediction_error(self, observations: np.ndarray) -> np.ndarray:
        """
        Compute the prediction error between expected and actual observations.
        
        Args:
            observations: The actual observations
            
        Returns:
            The prediction error (observations - expected observations)
        """
        expected_obs = self.likelihood(self.posterior_means)
        return observations - expected_obs
    
    def free_energy(self, observations: Optional[np.ndarray] = None) -> float:
        """
        Calculate the variational free energy of the model.
        
        Args:
            observations: Optional observations to compute free energy for
            
        Returns:
            The free energy value (to be minimized)
        """
        # If no observations provided, return the complexity term only
        if observations is None:
            # KL divergence between posterior and prior
            kl_divergence = 0.5 * (
                np.trace(np.linalg.inv(self.prior_precision) @ self.posterior_precision) +
                (self.prior_means - self.posterior_means).T @ self.prior_precision @ (self.prior_means - self.posterior_means) -
                self.n_states +
                np.log(np.linalg.det(self.prior_precision) / np.linalg.det(self.posterior_precision))
            )
            return kl_divergence
        
        # With observations, compute complete free energy (accuracy + complexity)
        expected_obs = self.likelihood(self.posterior_means)
        
        # Accuracy term: precision-weighted prediction error
        prediction_error = observations - expected_obs
        accuracy = 0.5 * prediction_error.T @ self.likelihood_precision @ prediction_error
        
        # Complexity term: KL divergence between posterior and prior
        complexity = 0.5 * (
            np.trace(np.linalg.inv(self.prior_precision) @ self.posterior_precision) +
            (self.prior_means - self.posterior_means).T @ self.prior_precision @ (self.prior_means - self.posterior_means) -
            self.n_states +
            np.log(np.linalg.det(self.prior_precision) / np.linalg.det(self.posterior_precision))
        )
        
        # Free energy = accuracy + complexity
        free_energy = accuracy + complexity
        
        # Store in history
        self.free_energy_history.append(free_energy)
        
        return free_energy
    
    def update_posterior(self, observation: np.ndarray) -> Dict[str, Any]:
        """
        Update posterior belief based on new observation.
        
        Args:
            observation: Observation vector (one-hot encoding or distribution)
            
        Returns:
            Dictionary with update results
        """
        # Check if parameters are available
        if self.parameters is None or 'observation_matrix' not in self.parameters:
            # Simplified update: directly use observation to update posterior
            observation = np.atleast_1d(observation)
            
            # Simple weighted average update
            learning_rate = 0.1
            if len(observation) == len(self.posterior_means):
                self.posterior_means = (1 - learning_rate) * self.posterior_means + learning_rate * observation
            else:
                # Just store the observation as prediction error
                pass
            
            logger.debug(f"Simplified posterior update: {np.round(self.posterior_means, 3)}")
            return {"status": "success", "method": "simplified"}
        
        # Full Bayesian update with observation matrix
        observation_matrix = self.parameters['observation_matrix']
        n_states = self.parameters['n_states']
        
        # Prior is current posterior
        prior = self.posterior_means.copy()
        
        # Calculate likelihood for each state
        likelihood = np.zeros(n_states)
        for s in range(n_states):
            # Observation likelihood for state s
            # Using dot product for distribution observations
            likelihood[s] = np.dot(observation_matrix[s, :], observation)
        
        # Bayes rule: posterior ∝ likelihood * prior
        posterior = likelihood * prior
        
        # Normalize
        if np.sum(posterior) > 0:
            posterior = posterior / np.sum(posterior)
        else:
            # If all probabilities are zero, maintain prior
            logger.warning("All posterior probabilities are zero, maintaining prior")
            posterior = prior
        
        # Update posterior means
        self.posterior_means = posterior
        
        logger.debug(f"Updated posterior: {np.round(self.posterior_means, 3)}")
        return {"status": "success", "method": "bayesian"}
    
    def get_optimal_action(self) -> int:
        """
        Get the optimal action based on current beliefs.
        
        Returns:
            Index of the optimal action
        """
        # Simple implementation: use action that maximizes expected reward
        # In more complex implementations, this would use a policy or value function
        
        # For now, just return the action corresponding to the most likely state
        max_state = np.argmax(self.posterior_means)
        
        # In a more sophisticated implementation, we would:
        # 1. Compute expected rewards for each action
        # 2. Select action with highest expected reward
        
        # For simplicity, we'll just use the state index (modulo n_actions)
        n_actions = self.parameters['n_actions']
        optimal_action = max_state % n_actions
        
        logger.debug(f"Selected optimal action {optimal_action} for max state {max_state}")
        
        return optimal_action
    
    def predict_next_state(self, action: int) -> np.ndarray:
        """
        Predict distribution over next states given an action.
        
        Args:
            action: Action index
            
        Returns:
            Distribution over next states
        """
        # Get transition matrix
        transition_matrix = self.parameters['transition_matrix']
        
        # Compute expected next state distribution
        next_state_dist = np.zeros(self.parameters['n_states'])
        
        for s in range(self.parameters['n_states']):
            # Weight transitions by current belief in each state
            next_state_dist += self.posterior_means[s] * transition_matrix[s, action, :]
        
        return next_state_dist
    
    def predict_observation(self, state_dist: Optional[np.ndarray] = None) -> np.ndarray:
        """
        Predict expected observation given a state distribution.
        
        Args:
            state_dist: Distribution over states (if None, uses current belief)
            
        Returns:
            Expected observation distribution
        """
        if state_dist is None:
            state_dist = self.posterior_means
        
        # Get observation matrix
        observation_matrix = self.parameters['observation_matrix']
        
        # Compute expected observation
        expected_observation = np.zeros(self.parameters['n_observations'])
        
        for s in range(self.parameters['n_states']):
            # Weight observation probabilities by state probabilities
            expected_observation += state_dist[s] * observation_matrix[s, :]
        
        return expected_observation
    
    # Implement case-specific updates
    def _update_nominative(self, data: Any) -> Dict[str, Any]:
        """Update for NOMINATIVE case: model as active agent generating predictions"""
        if data is None:
            # Generate predictions without observations
            predictions = self.likelihood(self.posterior_means)
            return {
                "status": "success",
                "predictions": predictions,
                "case": "NOMINATIVE"
            }
        else:
            # Generate predictions and compare with observations
            observations = np.array(data)
            predictions = self.likelihood(self.posterior_means)
            prediction_error = observations - predictions
            
            return {
                "status": "success",
                "predictions": predictions,
                "prediction_error": prediction_error,
                "case": "NOMINATIVE"
            }
    
    def _update_accusative(self, data: Any) -> Dict[str, Any]:
        """Update for ACCUSATIVE case: model as object receiving updates"""
        if data is None:
            return {"status": "error", "message": "Data required for ACCUSATIVE update", "case": "ACCUSATIVE"}
        
        # In ACCUSATIVE case, the model receives parameter updates
        observations = np.array(data)
        update_result = self.update_posterior(observations)
        update_result["case"] = "ACCUSATIVE"
        
        return update_result
    
    def _update_genitive(self, data: Any) -> Dict[str, Any]:
        """Update for GENITIVE case: model as source/generator of outputs"""
        # In GENITIVE case, model focuses on generating products
        # This could be reports, analyses, derived models, etc.
        
        # Generate multiple samples from the posterior for output diversity
        n_samples = 5 if data is None else data.get("n_samples", 5)
        
        # Convert posterior precision to covariance for sampling
        posterior_cov = np.linalg.inv(self.posterior_precision)
        
        # Sample from posterior
        samples = np.random.multivariate_normal(
            mean=self.posterior_means, 
            cov=posterior_cov, 
            size=n_samples
        )
        
        # Generate observations for each sample
        products = [self.likelihood(sample) for sample in samples]
        
        return {
            "status": "success",
            "products": products,
            "samples": samples,
            "case": "GENITIVE"
        }
    
    def _update_dative(self, data: Any) -> Dict[str, Any]:
        """Update for DATIVE case: model as recipient of data flows"""
        if data is None:
            return {"status": "error", "message": "Data required for DATIVE update", "case": "DATIVE"}
        
        # In DATIVE case, model receives and processes data
        # But unlike ACCUSATIVE, it doesn't necessarily update itself
        
        observations = np.array(data)
        expected_obs = self.likelihood(self.posterior_means)
        prediction_error = observations - expected_obs
        
        return {
            "status": "success",
            "received_data": observations,
            "prediction_error": prediction_error,
            "case": "DATIVE"
        }
    
    def _update_instrumental(self, data: Any) -> Dict[str, Any]:
        """Update for INSTRUMENTAL case: model as method/tool"""
        # In INSTRUMENTAL case, model serves as a computational tool
        
        if data is None or not isinstance(data, dict):
            return {"status": "error", "message": "Method parameters required for INSTRUMENTAL update", "case": "INSTRUMENTAL"}
        
        method = data.get("method", "predict")
        method_params = data.get("params", {})
        
        if method == "predict":
            # Use model to make predictions
            state = method_params.get("state", self.posterior_means)
            predictions = self.likelihood(state)
            return {
                "status": "success",
                "method": "predict",
                "predictions": predictions,
                "case": "INSTRUMENTAL"
            }
        elif method == "invert":
            # Use model to infer states from observations
            if "observations" not in method_params:
                return {"status": "error", "message": "Observations required for inversion", "case": "INSTRUMENTAL"}
            
            observations = np.array(method_params["observations"])
            learning_rate = method_params.get("learning_rate", 0.1)
            iterations = method_params.get("iterations", 10)
            
            # Copy current posterior to avoid changing it
            saved_posterior = self.posterior_means.copy()
            
            # Start from prior or specified initial state
            self.posterior_means = method_params.get("initial_state", self.prior_means.copy())
            
            # Run gradient descent for specified iterations
            results = []
            for i in range(iterations):
                update_result = self.update_posterior(observations, learning_rate)
                results.append(update_result)
            
            # Restore original posterior
            inferred_state = self.posterior_means.copy()
            self.posterior_means = saved_posterior
            
            return {
                "status": "success",
                "method": "invert",
                "inferred_state": inferred_state,
                "iterations": iterations,
                "results": results,
                "case": "INSTRUMENTAL"
            }
        else:
            return {"status": "error", "message": f"Unknown method {method}", "case": "INSTRUMENTAL"}
    
    def _update_locative(self, data: Any) -> Dict[str, Any]:
        """Update for LOCATIVE case: model as context"""
        # In LOCATIVE case, model provides contextual parameters for other processes
        
        # Extract relevant contextual parameters based on data request
        if data is None:
            # Return all context parameters
            context = {
                "prior_means": self.prior_means.copy(),
                "prior_precision": self.prior_precision.copy(),
                "likelihood_precision": self.likelihood_precision.copy(),
                "n_states": self.n_states,
                "n_observations": self.n_observations
            }
        else:
            # Return requested context parameters
            context = {}
            if isinstance(data, list):
                for param in data:
                    if hasattr(self, param):
                        context[param] = getattr(self, param)
            elif isinstance(data, dict):
                for param, transform in data.items():
                    if hasattr(self, param):
                        value = getattr(self, param)
                        if callable(transform):
                            context[param] = transform(value)
                        else:
                            context[param] = value
        
        return {
            "status": "success",
            "context": context,
            "case": "LOCATIVE"
        }
    
    def _update_ablative(self, data: Any) -> Dict[str, Any]:
        """Update for ABLATIVE case: model as origin/cause"""
        # In ABLATIVE case, model serves as source of historical data and causal explanations
        
        if data is None:
            # Return summary of model history
            history_summary = {
                "belief_history_length": len(self.belief_history),
                "free_energy_history": self.free_energy_history.copy(),
                "case_transitions": self._case_history.copy()
            }
            return {
                "status": "success",
                "history": history_summary,
                "case": "ABLATIVE"
            }
        
        if isinstance(data, str):
            # Return specific historical aspect
            if data == "beliefs":
                return {
                    "status": "success",
                    "beliefs": self.belief_history.copy(),
                    "case": "ABLATIVE"
                }
            elif data == "free_energy":
                return {
                    "status": "success",
                    "free_energy": self.free_energy_history.copy(),
                    "case": "ABLATIVE"
                }
            elif data == "transitions":
                return {
                    "status": "success",
                    "transitions": self._case_history.copy(),
                    "case": "ABLATIVE"
                }
        
        return {
            "status": "error",
            "message": "Invalid query for ABLATIVE case",
            "case": "ABLATIVE"
        }
    
    def _update_vocative(self, data: Any) -> Dict[str, Any]:
        """Update for VOCATIVE case: model as addressable entity"""
        # In VOCATIVE case, model responds to direct address/commands
        
        if data is None:
            # Respond to basic address
            return {
                "status": "success",
                "response": f"Model {self.name} ready",
                "case": "VOCATIVE"
            }
        
        if isinstance(data, str):
            # Respond to simple commands
            if data.lower() == "identify":
                return {
                    "status": "success",
                    "identity": {
                        "name": self.name,
                        "id": self.id,
                        "type": self.__class__.__name__
                    },
                    "case": "VOCATIVE"
                }
            elif data.lower() == "status":
                return {
                    "status": "success",
                    "model_status": {
                        "case": self._case.name,
                        "prior_case": self._prior_case.name if self._prior_case else None,
                        "free_energy": self.free_energy() if len(self.free_energy_history) > 0 else None
                    },
                    "case": "VOCATIVE"
                }
            elif data.lower() == "help":
                return {
                    "status": "success",
                    "commands": ["identify", "status", "help", "capabilities"],
                    "case": "VOCATIVE"
                }
            elif data.lower() == "capabilities":
                return {
                    "status": "success",
                    "capabilities": {
                        "states": self.n_states,
                        "observations": self.n_observations,
                        "cases": [case.name for case in Case]
                    },
                    "case": "VOCATIVE"
                }
        
        return {
            "status": "error",
            "message": f"Command not recognized: {data}",
            "case": "VOCATIVE"
        } 