# Active Inference Integration in CEREBRUM

This document details how the CEREBRUM framework integrates with Active Inference principles, providing a mathematically rigorous foundation for case transformations and model interactions.

## 1. Free Energy Minimization in Case Transformations

Case transformations in CEREBRUM are formally grounded in the Free Energy Principle, treating each transformation as an inference process.

### 1.1 Mathematical Formulation

The variational free energy for a model $m$ in case $c$ is defined as:

$$F(m, c) = D_{KL}[q(\theta|c) || p(\theta|c)] - \mathbb{E}_q[\ln p(o|\theta, c)]$$

Where:
- $F$ is the free energy
- $D_{KL}$ is the Kullback-Leibler divergence
- $q(\theta|c)$ is the approximate posterior over parameters given the case
- $p(\theta|c)$ is the prior over parameters given the case
- $\mathbb{E}_q$ is the expectation under the approximate posterior
- $p(o|\theta, c)$ is the likelihood of observations given parameters and case

Case transformations are driven by free energy minimization, seeking the case that minimizes surprise:

$$c^* = \arg\min_c F(m, c)$$

### 1.2 Implementation Example

```python
def calculate_free_energy(model, case):
    """Calculate variational free energy for a model in a specific case."""
    # Get case-specific parameter distributions
    q_theta = model.get_posterior_distribution(case)
    p_theta = model.get_prior_distribution(case)
    
    # Calculate KL divergence between posterior and prior
    kl_divergence = calculate_kl_divergence(q_theta, p_theta)
    
    # Calculate expected log likelihood
    expected_log_likelihood = calculate_expected_log_likelihood(model, case)
    
    # Calculate free energy
    free_energy = kl_divergence - expected_log_likelihood
    
    return free_energy

def find_optimal_case(model, available_cases):
    """Find the optimal case for a model using free energy minimization."""
    energies = {case: calculate_free_energy(model, case) for case in available_cases}
    return min(energies.items(), key=lambda x: x[1])[0]
```

## 2. Precision-Weighted Message Passing

CEREBRUM implements hierarchical message passing with precision weighting, modulating the influence of different models based on their current case.

### 2.1 Precision Weighting

The precision weight for a model $m$ in case $c$ is defined as:

$$\pi(m, c) = \exp(-\alpha F(m, c))$$

Where:
- $\pi$ is the precision weight
- $\alpha$ is a temperature parameter
- $F$ is the free energy

### 2.2 Message Passing Implementation

```python
class MessageBus:
    def __init__(self):
        self.message_queue = []
        self.handlers = {}
        
    def send_message(self, source_model, target_model, content):
        """Send a precision-weighted message between models."""
        # Calculate precision of the source model in its current case
        source_precision = calculate_precision(source_model)
        
        # Create message with precision weighting
        message = {
            'source': source_model.id,
            'target': target_model.id,
            'content': content,
            'precision': source_precision,
            'timestamp': current_time()
        }
        
        # Add to queue
        self.message_queue.append(message)
        
        # Process immediately if handler exists
        if target_model.id in self.handlers:
            self.handlers[target_model.id](message)
            
        return message
    
    def register_handler(self, model_id, handler_function):
        """Register a message handler for a model."""
        self.handlers[model_id] = handler_function
        
    def process_queue(self):
        """Process all messages in the queue."""
        for message in self.message_queue:
            if message['target'] in self.handlers:
                self.handlers[message['target']](message)
        
        # Clear processed messages
        self.message_queue = []

def calculate_precision(model):
    """Calculate precision for a model based on its current case."""
    # Get current case
    current_case = model.current_case
    
    # Calculate free energy
    free_energy = calculate_free_energy(model, current_case)
    
    # Calculate precision using exponential transformation
    alpha = 1.0  # Temperature parameter
    precision = math.exp(-alpha * free_energy)
    
    return precision
```

## 3. Case-Specific Prediction and Update Equations

Different cases implement different prediction and update dynamics according to Active Inference principles.

### 3.1 Prediction Dynamics

For a model in nominative case [NOM], predictions follow the standard generative model:

$$\hat{o} = g(s, \theta_{NOM})$$

Where:
- $\hat{o}$ are predicted observations
- $g$ is the generative function
- $s$ are internal states
- $\theta_{NOM}$ are parameters prioritized in nominative case

For a model in genitive case [GEN], predictions focus on output generation:

$$\hat{y} = h(s, \theta_{GEN})$$

Where:
- $\hat{y}$ are generated outputs
- $h$ is the output generation function
- $\theta_{GEN}$ are parameters prioritized in genitive case

### 3.2 Update Dynamics

For a model in accusative case [ACC], parameter updates follow gradient descent on free energy:

$$\Delta \theta_{ACC} = -\eta \frac{\partial F}{\partial \theta_{ACC}}$$

Where:
- $\Delta \theta_{ACC}$ is the parameter update
- $\eta$ is the learning rate
- $\frac{\partial F}{\partial \theta_{ACC}}$ is the free energy gradient

For a model in dative case [DAT], state updates prioritize incoming data:

$$\Delta s = -\kappa \frac{\partial F}{\partial s} + \lambda I$$

Where:
- $\Delta s$ is the state update
- $\kappa$ is the state update rate
- $\frac{\partial F}{\partial s}$ is the free energy gradient with respect to state
- $\lambda$ is the input weighting
- $I$ is the incoming data

### 3.3 Implementation Example

```python
class ActiveInferenceModel:
    def __init__(self):
        self.parameters = {}
        self.state = {}
        self.current_case = None
        
    def predict(self, inputs=None):
        """Generate predictions based on current case."""
        if self.current_case.abbreviation == 'NOM':
            return self._predict_nominative()
        elif self.current_case.abbreviation == 'GEN':
            return self._predict_genitive()
        # Other cases...
    
    def update(self, prediction, observation):
        """Update model based on current case."""
        if self.current_case.abbreviation == 'ACC':
            return self._update_accusative(prediction, observation)
        elif self.current_case.abbreviation == 'DAT':
            return self._update_dative(observation)
        # Other cases...
    
    def _predict_nominative(self):
        """Generate predictions as an active agent."""
        # Get parameters prioritized for nominative case
        theta_nom = self._get_case_parameters('NOM')
        
        # Generate predictions using the generative model
        predictions = self._generative_function(self.state, theta_nom)
        
        return predictions
    
    def _predict_genitive(self):
        """Generate outputs as a source/producer."""
        # Get parameters prioritized for genitive case
        theta_gen = self._get_case_parameters('GEN')
        
        # Generate outputs using the output function
        outputs = self._output_function(self.state, theta_gen)
        
        return outputs
    
    def _update_accusative(self, prediction, observation):
        """Update parameters as the object of optimization."""
        # Calculate prediction error
        prediction_error = self._calculate_error(prediction, observation)
        
        # Calculate free energy gradient
        gradient = self._calculate_gradient(prediction_error)
        
        # Update parameters with gradient descent
        learning_rate = 0.01
        for param_name, grad_value in gradient.items():
            if param_name in self.parameters:
                self.parameters[param_name] -= learning_rate * grad_value
        
        return {
            'error': prediction_error,
            'gradient_magnitude': sum(abs(g) for g in gradient.values()),
            'updated_parameters': self.parameters
        }
    
    def _update_dative(self, observation):
        """Update state based on incoming data."""
        # Calculate state update
        state_update = self._calculate_state_update(observation)
        
        # Apply update
        input_weight = 0.8
        for state_var, update_value in state_update.items():
            if state_var in self.state:
                self.state[state_var] += update_value
        
        # Directly incorporate input
        for input_var, input_value in observation.items():
            if input_var in self.state:
                self.state[input_var] = (1-input_weight) * self.state[input_var] + input_weight * input_value
        
        return {
            'updated_state': self.state,
            'input_influence': input_weight
        }
```

## 4. Markov Blanket Formulation

Case relationships in CEREBRUM can be formally described using Markov blankets, creating a principled organization of model interactions.

### 4.1 Formal Definition

For a model $m$ in case $c$, its Markov blanket consists of:

- **Parents**: Models that send messages to $m$
- **Children**: Models that receive messages from $m$
- **Co-parents**: Models that send messages to the same children as $m$

The conditional independence structure imposed by the Markov blanket ensures that a model's interactions are fully mediated by its blanket states.

### 4.2 Case-Specific Blanket Properties

Different cases establish different Markov blanket configurations:

- **Nominative [NOM]**: Larger child set, smaller parent set
- **Accusative [ACC]**: Larger parent set, smaller child set
- **Genitive [GEN]**: Large child set, emphasis on message generation
- **Dative [DAT]**: Large parent set, emphasis on message reception
- **Instrumental [INS]**: Mediating position between other models
- **Locative [LOC]**: Context-providing blanket configuration
- **Ablative [ABL]**: Source-oriented blanket configuration
- **Vocative [VOC]**: Addressable configuration with direct connections

### 4.3 Implementation Example

```python
class MarkovBlanket:
    def __init__(self, central_model):
        self.central_model = central_model
        self.parents = set()
        self.children = set()
        self.co_parents = set()
        
    def update_blanket_for_case(self, case):
        """Update the Markov blanket configuration based on the model's case."""
        if case.abbreviation == 'NOM':
            # Nominative case: more children, fewer parents
            self._expand_children()
            self._contract_parents()
        elif case.abbreviation == 'ACC':
            # Accusative case: more parents, fewer children
            self._expand_parents()
            self._contract_children()
        elif case.abbreviation == 'DAT':
            # Dative case: maximize parents
            self._maximize_parents()
        elif case.abbreviation == 'GEN':
            # Genitive case: maximize children
            self._maximize_children()
        # Other cases...
        
        # Update co-parents based on new children
        self._update_co_parents()
        
    def _expand_children(self):
        """Expand the set of child models."""
        # Implementation would add appropriate models to children set
        pass
        
    def _contract_parents(self):
        """Reduce the set of parent models."""
        # Implementation would remove less relevant models from parents set
        pass
        
    def _update_co_parents(self):
        """Update the set of co-parent models."""
        # Find all models that send messages to any of this model's children
        registry = ModelRegistry.get_instance()
        
        self.co_parents = set()
        for child in self.children:
            parents = registry.get_parents(child)
            self.co_parents.update(parents)
            
        # Remove the central model itself from co-parents
        self.co_parents.discard(self.central_model)
```

## 5. Active Inference Workflow Integration

CEREBRUM integrates Active Inference principles into complete workflows, using free energy minimization to guide the entire intelligence production process.

### 5.1 Workflow as Active Inference

An intelligence workflow can be formulated as an active inference process:

1. The workflow has a generative model of how intelligence products are created
2. It makes predictions about intermediate products at each stage
3. It updates its internal model based on prediction errors
4. It selects actions (case transformations) to minimize expected free energy

### 5.2 Implementation Example

```python
class ActiveInferenceWorkflow:
    def __init__(self, name):
        self.name = name
        self.stages = []
        self.current_stage = None
        self.generative_model = WorkflowGenerativeModel()
        
    def add_stage(self, stage_config):
        """Add a stage to the workflow."""
        self.stages.append(stage_config)
        
    def execute(self, initial_input):
        """Execute the workflow using active inference principles."""
        current_input = initial_input
        results = {}
        
        for stage in self.stages:
            self.current_stage = stage
            
            # Models for this stage
            models = stage['models']
            
            # Transform models to appropriate cases for this stage
            self._optimize_model_cases(models, stage)
            
            # Generate predictions for this stage
            predictions = self._predict_stage_output(models, current_input)
            
            # Process stage and get actual output
            stage_output = self._process_stage(models, current_input)
            
            # Update models based on prediction errors
            self._update_models(models, predictions, stage_output)
            
            # Store results
            results[stage['name']] = stage_output
            
            # Use this stage's output as input to next stage
            current_input = stage_output
            
        return results
    
    def _optimize_model_cases(self, models, stage):
        """Optimize the case assignments for models in this stage."""
        for model in models:
            # Calculate free energy for each possible case
            case_energies = {}
            for case in model.supported_cases:
                # Temporarily transform to this case
                original_case = model.current_case
                model.transform_case(case.abbreviation)
                
                # Calculate free energy for this stage context
                energy = self._calculate_contextual_free_energy(model, stage)
                case_energies[case] = energy
                
                # Return to original case
                model.transform_case(original_case.abbreviation)
            
            # Find optimal case (minimum free energy)
            optimal_case = min(case_energies.items(), key=lambda x: x[1])[0]
            
            # Transform to optimal case
            model.transform_case(optimal_case.abbreviation)
    
    def _calculate_contextual_free_energy(self, model, stage):
        """Calculate free energy considering workflow context."""
        # Base free energy of the model
        base_energy = calculate_free_energy(model, model.current_case)
        
        # Contextual factor based on stage requirements
        context_factor = self._calculate_stage_compatibility(model, stage)
        
        # Combined energy
        return base_energy * context_factor
    
    def _calculate_stage_compatibility(self, model, stage):
        """Calculate compatibility factor between model case and stage."""
        # Different stages have different preferred cases
        stage_name = stage['name']
        case_abbr = model.current_case.abbreviation
        
        compatibility_map = {
            'data_collection': {'INS': 0.8, 'DAT': 0.9, 'NOM': 1.2},
            'analysis': {'NOM': 0.8, 'LOC': 0.9, 'INS': 1.1},
            'reporting': {'GEN': 0.7, 'NOM': 1.3, 'ABL': 1.2},
            'evaluation': {'ACC': 0.7, 'INS': 1.2, 'LOC': 1.1},
        }
        
        # Default compatibility factor
        default_factor = 1.0
        
        # Get compatibility factor for this stage and case
        if stage_name in compatibility_map and case_abbr in compatibility_map[stage_name]:
            return compatibility_map[stage_name][case_abbr]
        
        return default_factor
```

## 6. Free Energy Landscape of Case Transformations

The free energy landscape provides a visualization of how different cases relate to each other in terms of transformation costs and stability.

### 6.1 Landscape Properties

The free energy landscape has several important properties:

- **Local minima**: Stable case configurations for specific contexts
- **Transition barriers**: Free energy costs of case transformations
- **Basins of attraction**: Contextual regions where particular cases are optimal
- **Global minimum**: Most stable case for a given model and context

### 6.2 Visualizing the Landscape

```python
def generate_free_energy_landscape(model, context_parameters, case_grid):
    """Generate a free energy landscape for visualization."""
    # Initialize landscape grid
    landscape = np.zeros(case_grid.shape)
    
    # Calculate free energy at each point in the grid
    for i in range(case_grid.shape[0]):
        for j in range(case_grid.shape[1]):
            # Get case parameters at this grid point
            case_params = case_grid[i, j]
            
            # Create temporary case with these parameters
            temp_case = create_parameterized_case(case_params)
            
            # Calculate free energy
            free_energy = calculate_contextual_free_energy(model, temp_case, context_parameters)
            
            # Store in landscape
            landscape[i, j] = free_energy
    
    return landscape

def plot_free_energy_landscape(landscape, case_grid, standard_cases):
    """Plot the free energy landscape with standard cases marked."""
    plt.figure(figsize=(10, 8))
    
    # Create contour plot of landscape
    contour = plt.contourf(case_grid[0], case_grid[1], landscape, 20, cmap='viridis')
    plt.colorbar(contour, label='Free Energy')
    
    # Mark standard cases
    for case_name, case_coords in standard_cases.items():
        plt.plot(case_coords[0], case_coords[1], 'o', markersize=10, label=case_name)
    
    plt.legend()
    plt.xlabel('Parameter Access Dimension')
    plt.ylabel('Interface Dimension')
    plt.title('Free Energy Landscape of Case Transformations')
    
    plt.show()
```

This implementation provides a concrete foundation for how CEREBRUM integrates with Active Inference principles, enabling mathematically principled case transformations and model interactions. 