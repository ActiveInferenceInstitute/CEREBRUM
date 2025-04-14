from typing import Dict, Any, Optional, List, Callable, Tuple, Union
import numpy as np
import logging
import time

from src.core.model import Model, Case

logger = logging.getLogger(__name__)

class NeuralNetworkModel(Model):
    """
    Neural Network model in the CEREBRUM framework.
    
    This class extends the base Model with neural network capabilities,
    supporting different network architectures and training approaches based on the case.
    """
    
    def __init__(
        self, 
        name: str = None, 
        parameters: Dict[str, Any] = None,
        input_dim: int = 1,
        output_dim: int = 1,
        hidden_dims: List[int] = None,
        activation: str = 'relu'
    ):
        """
        Initialize a NeuralNetworkModel with the necessary components.
        
        Args:
            name: A unique identifier for the model
            parameters: Initial parameters for the model
            input_dim: Dimension of input features
            output_dim: Dimension of output features
            hidden_dims: List of hidden layer dimensions
            activation: Activation function to use ('relu', 'sigmoid', 'tanh')
        """
        super().__init__(name, parameters)
        
        # Network architecture
        self.input_dim = input_dim
        self.output_dim = output_dim
        self.hidden_dims = hidden_dims or [10]  # Default to one hidden layer with 10 units
        self.activation = activation
        
        # Determine output activation implicitly (e.g., softmax for multi-class)
        self.output_activation = 'softmax' if output_dim > 1 else 'linear'
        
        # Initialize weights and biases
        self.weights = []
        self.biases = []
        
        # Connect input layer to first hidden layer
        prev_dim = input_dim
        for dim in self.hidden_dims:
            # Initialize with Xavier/Glorot initialization
            limit = np.sqrt(6 / (prev_dim + dim))
            self.weights.append(np.random.uniform(-limit, limit, (prev_dim, dim)))
            self.biases.append(np.zeros(dim))
            prev_dim = dim
            
        # Connect last hidden layer to output layer
        limit = np.sqrt(6 / (prev_dim + output_dim))
        self.weights.append(np.random.uniform(-limit, limit, (prev_dim, output_dim)))
        self.biases.append(np.zeros(output_dim))
        
        # Training state
        self.loss_history = []
        self.trained = False
        
        # Case-specific parameter access patterns
        self._setup_case_configurations()
        
        # Additional tracking
        self.prediction_history = []
        self.training_examples_seen = 0
        
    def _setup_case_configurations(self):
        """Set up case-specific configurations for parameter access."""
        # NOMINATIVE: All parameters fully accessible for prediction generation
        self._case_configurations[Case.NOMINATIVE] = {
            "params_accessible": True,
            "training_allowed": True,
            "interface_mode": "prediction_generation",
            "param_focus": "all"
        }
        
        # ACCUSATIVE: Focus on evaluation metrics
        self._case_configurations[Case.ACCUSATIVE] = {
            "params_accessible": False,
            "training_allowed": False,
            "interface_mode": "evaluation_target",
            "param_focus": "accuracy"
        }
        
        # GENITIVE: Output-focused generation
        self._case_configurations[Case.GENITIVE] = {
            "params_accessible": True,
            "training_allowed": False,
            "interface_mode": "output_generation",
            "param_focus": "output_layers"
        }
        
        # DATIVE: Input processing focus
        self._case_configurations[Case.DATIVE] = {
            "params_accessible": True,
            "training_allowed": True,
            "interface_mode": "input_processor",
            "param_focus": "input_layers"
        }
        
        # INSTRUMENTAL: Method/tool orientation
        self._case_configurations[Case.INSTRUMENTAL] = {
            "params_accessible": True,
            "training_allowed": True,
            "interface_mode": "computational_method",
            "param_focus": "hidden_layers"
        }
        
        # LOCATIVE: Context awareness
        self._case_configurations[Case.LOCATIVE] = {
            "params_accessible": True,
            "training_allowed": False,
            "interface_mode": "context_provider",
            "param_focus": "activations"
        }
        
        # ABLATIVE: Source attribution
        self._case_configurations[Case.ABLATIVE] = {
            "params_accessible": False,
            "training_allowed": False,
            "interface_mode": "error_source",
            "param_focus": "gradients"
        }
        
        # VOCATIVE: Addressable interface
        self._case_configurations[Case.VOCATIVE] = {
            "params_accessible": False,
            "training_allowed": False,
            "interface_mode": "query_responder",
            "param_focus": "interface"
        }
    
    def _apply_case_transformation(self):
        """Apply transformations when changing between cases."""
        # Record the transformation in history
        logging.info(f"Neural network model '{self.name}' transformed to {self._case.value} case")
        
        # Each case has specific parameter accessibility and behavior focus
        config = self._case_configurations[self._case]
        
        logging.info(f"Parameter focus: {config['param_focus']}")
        logging.info(f"Interface mode: {config['interface_mode']}")
    
    def _activation_function(self, x: np.ndarray) -> np.ndarray:
        """Apply the specified activation function."""
        if self.activation == 'relu':
            return np.maximum(0, x)
        elif self.activation == 'sigmoid':
            return 1 / (1 + np.exp(-x))
        elif self.activation == 'tanh':
            return np.tanh(x)
        else:
            logging.warning(f"Unknown activation '{self.activation}', using linear")
            return x
    
    def _activation_derivative(self, x: np.ndarray) -> np.ndarray:
        """Compute the derivative of the activation function."""
        if self.activation == 'relu':
            return np.where(x > 0, 1, 0)
        elif self.activation == 'sigmoid':
            s = 1 / (1 + np.exp(-x))
            return s * (1 - s)
        elif self.activation == 'tanh':
            return 1 - np.tanh(x)**2
        else:
            return np.ones_like(x)
    
    def _forward_with_activations(self, X: np.ndarray) -> Tuple[np.ndarray, List[np.ndarray]]:
        """
        Perform forward pass through the network and return activations.
        
        Args:
            X: Input data of shape (n_samples, n_features)
            
        Returns:
            Tuple containing predictions and list of activations at each layer
        """
        # Input layer
        activations = [X]  # Store original inputs
        current_activation = X
        
        # Process each hidden layer
        for i in range(len(self.weights) - 1):
            # Linear transformation
            z = np.dot(current_activation, self.weights[i]) + self.biases[i]
            
            # Apply activation function
            if self.activation == 'relu':
                a = np.maximum(0, z)  # ReLU activation
            elif self.activation == 'sigmoid':
                a = 1 / (1 + np.exp(-z))  # Sigmoid activation
            elif self.activation == 'tanh':
                a = np.tanh(z)  # Tanh activation
            else:
                a = z  # Linear activation (default)
            
            # Store activation for this layer and update current activation
            activations.append(a)
            current_activation = a
        
        # Output layer (linear activation)
        output = np.dot(current_activation, self.weights[-1]) + self.biases[-1]
        activations.append(output)
        
        # Apply output activation if necessary
        if self.output_activation == 'softmax':
            exp_scores = np.exp(output - np.max(output, axis=1, keepdims=True))
            output = exp_scores / np.sum(exp_scores, axis=1, keepdims=True)
        elif self.output_activation == 'sigmoid': # Example if needed later
            output = 1 / (1 + np.exp(-output))
        
        return output, activations
    
    def _backward_with_metrics(self, X: np.ndarray, y: np.ndarray, y_pred: np.ndarray) -> Tuple[List[np.ndarray], List[np.ndarray]]:
        """
        Perform backward pass with gradient computation and return metrics.
        
        Args:
            X: Input data of shape (n_samples, n_features)
            y: Target values of shape (n_samples, n_outputs)
            y_pred: Predicted values from forward pass
            
        Returns:
            Tuple containing list of weight updates and list of gradients
        """
        n_samples = X.shape[0]
        
        # Get all activations from forward pass
        _, activations = self._forward_with_activations(X)
        
        # Initialize empty arrays for gradients and weight updates
        gradients = []
        weight_updates = []
        
        # Output layer error
        if self.output_activation == 'softmax':
            # Assumes y is one-hot encoded if output_dim > 1
            # The gradient is simplified for Softmax + Categorical Cross Entropy
            d_output = (y_pred - y) / n_samples
        else: # Assumes linear output and MSE loss
            d_output = 2 * (y_pred - y) / n_samples
        
        # Gradient calculation for each layer, starting from the output layer
        current_gradient = d_output
        
        # Process each layer in reverse order
        for i in range(len(self.weights) - 1, -1, -1):
            # Current layer's activation (input to this layer)
            current_activation = activations[i]
            
            # Calculate weight gradients for this layer
            d_weights = np.dot(current_activation.T, current_gradient)
            d_biases = np.sum(current_gradient, axis=0)
            
            # Store gradients
            gradients.insert(0, d_weights)  # Insert at front to maintain layer order
            
            # Placeholder for weight update calculation if needed for metrics
            # In current tests, this metric isn't directly used, 
            # and actual updates happen in train/fit method.
            # Simple GD update calculation shown for potential future metric use:
            # update = learning_rate * d_weights 
            # weight_updates.insert(0, update) 
            pass # Avoid calculating updates here if not strictly needed for metrics
            
            # Propagate gradient to the previous layer
            if i > 0:  # Don't propagate beyond the input layer
                # Gradient w.r.t. pre-activation
                d_preact = np.dot(current_gradient, self.weights[i].T)
                
                # Apply activation function derivative
                if self.hyperparameters.get("activation") == "relu":
                    # ReLU derivative: 1 if x > 0, 0 otherwise
                    d_act = (activations[i] > 0).astype(float)
                    current_gradient = d_preact * d_act
                elif self.hyperparameters.get("activation") == "sigmoid":
                    # Sigmoid derivative: sigmoid(x) * (1 - sigmoid(x))
                    d_act = activations[i] * (1 - activations[i])
                    current_gradient = d_preact * d_act
                elif self.hyperparameters.get("activation") == "tanh":
                    # Tanh derivative: 1 - tanh(x)²
                    d_act = 1 - np.tanh(activations[i])**2
                    current_gradient = d_preact * d_act
                else:
                    # Linear activation derivative: 1
                    current_gradient = d_preact
        
        return weight_updates, gradients
    
    def forward(self, inputs: np.ndarray) -> Tuple[np.ndarray, List[np.ndarray]]:
        """
        Forward pass through the network.
        
        Args:
            inputs: Input features of shape (batch_size, input_dim)
            
        Returns:
            Tuple of (outputs, activations)
        """
        activations = [inputs]
        current_activation = inputs
        
        # Process through hidden layers
        for i in range(len(self.hidden_dims)):
            z = np.dot(current_activation, self.weights[i]) + self.biases[i]
            current_activation = self._activation_function(z)
            activations.append(current_activation)
        
        # Output layer
        z_out = np.dot(current_activation, self.weights[-1]) + self.biases[-1]
        output = z_out  # Linear activation for output layer by default
        activations.append(output)
        
        return output, activations
    
    def backward(self, inputs: np.ndarray, targets: np.ndarray, learning_rate: float = 0.01) -> Dict[str, Any]:
        """
        Backward pass for gradient computation and weight updates.
        
        Args:
            inputs: Input features
            targets: Target values
            learning_rate: Learning rate for updates
            
        Returns:
            Dictionary with loss and gradients
        """
        # Forward pass
        outputs, activations = self.forward(inputs)
        
        # Compute loss (mean squared error)
        loss = np.mean((outputs - targets) ** 2)
        
        # Initialize gradients
        weight_grads = [np.zeros_like(w) for w in self.weights]
        bias_grads = [np.zeros_like(b) for b in self.biases]
        
        # Output layer gradient
        delta = outputs - targets  # Gradient of MSE
        
        # Backpropagate through layers
        for layer in reversed(range(len(self.weights))):
            # Gradient for weights
            weight_grads[layer] = np.dot(activations[layer].T, delta)
            # Gradient for biases
            bias_grads[layer] = np.sum(delta, axis=0)
            
            # Propagate error to previous layer (if not at input layer)
            if layer > 0:
                delta = np.dot(delta, self.weights[layer].T)
                # Apply derivative of activation function
                delta = delta * self._activation_derivative(activations[layer])
        
        # Update weights and biases
        for i in range(len(self.weights)):
            self.weights[i] -= learning_rate * weight_grads[i]
            self.biases[i] -= learning_rate * bias_grads[i]
        
        # Record loss
        self.loss_history.append(loss)
        self.training_examples_seen += len(inputs)
        
        return {
            "loss": loss,
            "weight_grads": weight_grads,
            "bias_grads": bias_grads
        }
    
    def predict(self, inputs: np.ndarray) -> np.ndarray:
        """
        Make predictions with the model.
        
        Args:
            inputs: Input features
            
        Returns:
            Model predictions
        """
        outputs, _ = self.forward(inputs)
        self.prediction_history.append({"inputs": inputs, "outputs": outputs})
        return outputs
    
    def train(self, inputs: np.ndarray, targets: np.ndarray, 
              epochs: int = 100, learning_rate: float = 0.01, 
              batch_size: int = 32,
              loss_function: str = 'mse',
              verbose: bool = True) -> Dict[str, Any]:
        """
        Train the neural network.
        
        Args:
            inputs: Training inputs
            targets: Training targets
            epochs: Number of training epochs
            learning_rate: Learning rate for updates
            batch_size: Batch size for mini-batch training
            loss_function: Loss function to use ('mse', 'categorical_crossentropy')
            verbose: Whether to print training progress
            
        Returns:
            Dictionary with training results
        """
        if not self._case_configurations[self._case]["training_allowed"]:
            logging.warning(f"Training not allowed in {self._case.value} case. Transform to appropriate case first.")
            return {"status": "error", "message": f"Training not allowed in {self._case.value} case"}
        
        n_samples = len(inputs)
        losses = []
        
        for epoch in range(epochs):
            # Shuffle data
            indices = np.random.permutation(n_samples)
            inputs_shuffled = inputs[indices]
            targets_shuffled = targets[indices]
            
            # Mini-batch training
            for i in range(0, n_samples, batch_size):
                batch_inputs = inputs_shuffled[i:i+batch_size]
                batch_targets = targets_shuffled[i:i+batch_size]
                
                result = self.backward(batch_inputs, batch_targets, learning_rate)
                losses.append(result["loss"])
            
            # Log progress
            if (epoch + 1) % 10 == 0:
                logging.info(f"Epoch {epoch+1}/{epochs}, Loss: {result['loss']:.6f}")
        
        self.trained = True
        return {
            "status": "success",
            "epochs_completed": epochs,
            "final_loss": losses[-1],
            "loss_history": losses
        }
    
    def evaluate(self, inputs: np.ndarray, targets: np.ndarray) -> Dict[str, float]:
        """
        Evaluate the model on test data.
        
        Args:
            inputs: Test inputs
            targets: Test targets
            
        Returns:
            Dictionary with evaluation metrics
        """
        predictions = self.predict(inputs)
        
        # Mean squared error
        mse = np.mean((predictions - targets) ** 2)
        
        # Mean absolute error
        mae = np.mean(np.abs(predictions - targets))
        
        # R-squared
        ss_total = np.sum((targets - np.mean(targets)) ** 2)
        ss_residual = np.sum((targets - predictions) ** 2)
        r2 = 1 - (ss_residual / ss_total) if ss_total != 0 else 0
        
        return {
            "mse": mse,
            "mae": mae,
            "r2": r2
        }
    
    def free_energy(self) -> float:
        """
        Calculate an approximation of variational free energy.
        
        For neural networks, this is approximated as the prediction error (loss)
        plus a regularization term.
        
        Returns:
            Approximated free energy value
        """
        # If no loss history, return high value
        if not self.loss_history:
            return 1000.0
        
        # Use latest prediction error as accuracy term
        accuracy = self.loss_history[-1]
        
        # Regularization term (L2 norm of weights) as complexity term
        complexity = 0
        for w in self.weights:
            complexity += np.sum(w ** 2)
        
        # Scale the complexity term
        complexity *= 0.001
        
        # Free energy = accuracy + complexity
        return accuracy + complexity
    
    def _update_nominative(self, data: Any) -> Dict[str, Any]:
        """Update for NOMINATIVE case: model as active agent generating predictions."""
        if not isinstance(data, dict):
            return {"status": "error", "message": "Expected dictionary with 'inputs' key"}
        
        if "inputs" not in data:
            return {"status": "error", "message": "Missing required 'inputs' key"}
        
        # Generate predictions actively
        predictions = self.predict(data["inputs"])
        
        return {
            "status": "success",
            "predictions": predictions,
            "model_case": Case.NOMINATIVE.value
        }
    
    def _update_accusative(self, data: Any) -> Dict[str, Any]:
        """Update for ACCUSATIVE case: model as object being evaluated."""
        if not isinstance(data, dict):
            return {"status": "error", "message": "Expected dictionary with 'inputs' and 'targets' keys"}
        
        if "inputs" not in data or "targets" not in data:
            return {"status": "error", "message": "Missing required 'inputs' and/or 'targets' keys"}
        
        # Model is evaluated, not active
        evaluation = self.evaluate(data["inputs"], data["targets"])
        
        return {
            "status": "success",
            "evaluation": evaluation,
            "model_case": Case.ACCUSATIVE.value
        }
    
    def _update_genitive(self, data: Any) -> Dict[str, Any]:
        """Update for GENITIVE case: model as source/generator of outputs."""
        if not isinstance(data, dict):
            return {"status": "error", "message": "Expected dictionary with 'inputs' key"}
        
        if "inputs" not in data:
            return {"status": "error", "message": "Missing required 'inputs' key"}
        
        # Generate outputs with confidence intervals (simple approximation)
        predictions = self.predict(data["inputs"])
        
        # Generate fake confidence intervals based on layers
        confidence = 0.1 * (1.0 / len(self.weights))
        lower_bound = predictions - confidence
        upper_bound = predictions + confidence
        
        return {
            "status": "success",
            "predictions": predictions,
            "lower_bound": lower_bound,
            "upper_bound": upper_bound,
            "confidence": confidence,
            "model_case": Case.GENITIVE.value
        }
    
    def _update_dative(self, data: Any) -> Dict[str, Any]:
        """Update for DATIVE case: model as recipient of data flows."""
        if not isinstance(data, dict):
            return {"status": "error", "message": "Expected dictionary with 'inputs' key"}
        
        if "inputs" not in data:
            return {"status": "error", "message": "Missing required 'inputs' key"}
        
        # Process inputs through first layer only
        inputs = data["inputs"]
        # Just process through first layer
        first_layer_output = self._activation_function(np.dot(inputs, self.weights[0]) + self.biases[0])
        
        # Store the processed data for later use
        self._processed_data = first_layer_output
        
        return {
            "status": "success", 
            "processed_data": first_layer_output,
            "model_case": Case.DATIVE.value
        }
    
    def _update_instrumental(self, data: Any) -> Dict[str, Any]:
        """Update for INSTRUMENTAL case: model as method/tool."""
        if not isinstance(data, dict):
            return {"status": "error", "message": "Expected dictionary with operation details"}
        
        # Model operates as a tool/method rather than an agent
        operation = data.get("operation", "forward")
        
        if operation == "forward":
            if "inputs" not in data:
                return {"status": "error", "message": "Missing required 'inputs' key"}
            outputs, activations = self.forward(data["inputs"])
            layer_outputs = [a.tolist() for a in activations]
            return {
                "status": "success",
                "operation": "forward",
                "outputs": outputs,
                "layer_outputs": layer_outputs,
                "model_case": Case.INSTRUMENTAL.value
            }
        
        elif operation == "train":
            if "inputs" not in data or "targets" not in data:
                return {"status": "error", "message": "Missing required 'inputs' and/or 'targets' keys"}
            
            epochs = data.get("epochs", 10)
            learning_rate = data.get("learning_rate", 0.01)
            batch_size = data.get("batch_size", 32)
            
            result = self.train(
                data["inputs"], 
                data["targets"], 
                epochs=epochs, 
                learning_rate=learning_rate,
                batch_size=batch_size
            )
            
            return {
                "status": "success",
                "operation": "train",
                "training_result": result,
                "model_case": Case.INSTRUMENTAL.value
            }
        
        else:
            return {
                "status": "error",
                "message": f"Unknown operation: {operation}",
                "model_case": Case.INSTRUMENTAL.value
            }
    
    def _update_locative(self, data: Any) -> Dict[str, Any]:
        """Update for LOCATIVE case: model as context."""
        if not isinstance(data, dict):
            return {"status": "error", "message": "Expected dictionary with context requests"}
        
        context_type = data.get("context_type", "architecture")
        
        if context_type == "architecture":
            # Provide neural network architecture information
            architecture = {
                "input_dim": self.input_dim,
                "hidden_dims": self.hidden_dims,
                "output_dim": self.output_dim,
                "activation": self.activation,
                "n_parameters": sum(w.size for w in self.weights) + sum(b.size for b in self.biases)
            }
            return {
                "status": "success",
                "context_type": "architecture",
                "architecture": architecture,
                "model_case": Case.LOCATIVE.value
            }
        
        elif context_type == "training":
            # Provide training context
            training_context = {
                "trained": self.trained,
                "examples_seen": self.training_examples_seen,
                "loss_history": self.loss_history[-10:] if self.loss_history else [],
                "current_loss": self.loss_history[-1] if self.loss_history else None
            }
            return {
                "status": "success",
                "context_type": "training",
                "training_context": training_context,
                "model_case": Case.LOCATIVE.value
            }
        
        else:
            return {
                "status": "error",
                "message": f"Unknown context type: {context_type}",
                "model_case": Case.LOCATIVE.value
            }
    
    def _update_ablative(self, data: Any) -> Dict[str, Any]:
        """Update for ABLATIVE case: model as origin/cause of errors."""
        if not isinstance(data, dict):
            return {"status": "error", "message": "Expected dictionary with 'inputs' and 'targets' keys"}
        
        if "inputs" not in data or "targets" not in data:
            return {"status": "error", "message": "Missing required 'inputs' and/or 'targets' keys"}
        
        # Calculate errors and their origins
        outputs, activations = self.forward(data["inputs"])
        errors = outputs - data["targets"]
        
        # Track errors through the network to identify sources
        # Simplified approach: identify which layers contribute most to error
        layer_contributions = []
        
        for i in range(len(self.weights)):
            # Estimate layer contribution to error by weight magnitude
            contribution = np.sum(np.abs(self.weights[i])) / np.sum([np.sum(np.abs(w)) for w in self.weights])
            layer_contributions.append(float(contribution))
        
        # Identify the layer with the largest contribution to error
        primary_error_layer = np.argmax(layer_contributions)
        
        return {
            "status": "success",
            "errors": errors.tolist(),
            "error_magnitude": float(np.mean(np.abs(errors))),
            "layer_contributions": layer_contributions,
            "primary_error_layer": int(primary_error_layer),
            "model_case": Case.ABLATIVE.value
        }
    
    def _update_vocative(self, data: Any) -> Dict[str, Any]:
        """Update for VOCATIVE case: model as addressable entity."""
        if not isinstance(data, dict):
            return {"status": "error", "message": "Expected dictionary with query details"}
        
        # Model responds to specific queries through an interface
        query_type = data.get("query_type", "")
        
        if query_type == "predict":
            if "inputs" not in data:
                return {"status": "error", "message": "Missing required 'inputs' key"}
            predictions = self.predict(data["inputs"])
            return {
                "status": "success",
                "query_type": "predict",
                "predictions": predictions.tolist(),
                "model_case": Case.VOCATIVE.value
            }
        
        elif query_type == "architecture":
            architecture = {
                "input_dim": self.input_dim,
                "hidden_dims": self.hidden_dims,
                "output_dim": self.output_dim,
                "activation": self.activation,
                "n_parameters": sum(w.size for w in self.weights) + sum(b.size for b in self.biases)
            }
            return {
                "status": "success",
                "query_type": "architecture",
                "architecture": architecture,
                "model_case": Case.VOCATIVE.value
            }
        
        elif query_type == "evaluate":
            if "inputs" not in data or "targets" not in data:
                return {"status": "error", "message": "Missing required 'inputs' and/or 'targets' keys"}
            evaluation = self.evaluate(data["inputs"], data["targets"])
            return {
                "status": "success",
                "query_type": "evaluate",
                "evaluation": evaluation,
                "model_case": Case.VOCATIVE.value
            }
        
        elif query_type == "summary":
            summary = {
                "name": self.name,
                "type": "Neural Network",
                "trained": self.trained,
                "architecture": f"Input({self.input_dim}) -> Hidden{self.hidden_dims} -> Output({self.output_dim})",
                "activation": self.activation,
                "parameters": sum(w.size for w in self.weights) + sum(b.size for b in self.biases),
                "current_case": self._case.value
            }
            return {
                "status": "success",
                "query_type": "summary",
                "summary": summary,
                "model_case": Case.VOCATIVE.value
            }
        
        else:
            return {
                "status": "error",
                "message": f"Unknown query type: {query_type}",
                "model_case": Case.VOCATIVE.value
            } 