# Getting Started with CEREBRUM

This guide helps new developers get started with the CEREBRUM framework. It covers setting up the development environment, installing dependencies, and building a simple CEREBRUM model.

## Prerequisites

Before you begin, ensure you have the following installed:

- Python 3.9+ for the Python reference implementation
- Node.js 16+ (if working with JavaScript implementation)
- Rust 1.60+ (if working with Rust implementation)
- Git for version control

## Installation

### Python Implementation

1. Clone the repository:
   ```bash
   git clone https://github.com/ActiveInferenceInstitute/CEREBRUM.git
   cd CEREBRUM
   ```

2. Create a virtual environment:
   ```bash
   python -m venv venv
   source venv/bin/activate  # On Windows: venv\Scripts\activate
   ```

3. Install dependencies:
   ```bash
   pip install -r requirements.txt
   ```

4. Install the development version:
   ```bash
   pip install -e .
   ```

### JavaScript Implementation

1. Navigate to the JavaScript implementation:
   ```bash
   cd implementations/js
   ```

2. Install dependencies:
   ```bash
   npm install
   ```

3. Build the library:
   ```bash
   npm run build
   ```

### Rust Implementation

1. Navigate to the Rust implementation:
   ```bash
   cd implementations/rust
   ```

2. Build the library:
   ```bash
   cargo build
   ```

## Your First CEREBRUM Model

Let's create a simple temperature model that can transform between different cases.

### Python Example

```python
from cerebrum import GenerativeModel, Case, ModelRegistry

# 1. Define a simple model
class TemperatureModel(GenerativeModel):
    def __init__(self, model_id):
        super().__init__(model_id)
        # Core parameters
        self.parameters = {
            'current_temp': 72.0,
            'target_temp': 70.0,
            'heating_rate': 0.5,
            'cooling_rate': 0.3,
        }
        # Define supported cases
        self.supported_cases = [
            Case('nominative', 'NOM'),  # Active predictor
            Case('accusative', 'ACC'),  # Recipient of updates
            Case('genitive', 'GEN'),    # Output generator
        ]
        # Default to nominative case
        self.current_case = self.supported_cases[0]
    
    def predict(self, inputs=None):
        """Generate predictions based on current case."""
        if self.current_case.abbreviation == 'NOM':
            # As active predictor
            return self._predict_temperature()
        elif self.current_case.abbreviation == 'GEN':
            # As report generator
            return self._generate_report()
        else:
            return {"error": "Prediction not implemented for this case"}
    
    def _predict_temperature(self):
        """Nominative case: predict future temperature."""
        current = self.parameters['current_temp']
        target = self.parameters['target_temp']
        
        if current < target:
            # Heating
            new_temp = current + self.parameters['heating_rate']
        else:
            # Cooling
            new_temp = current - self.parameters['cooling_rate']
        
        return {
            'predicted_temp': new_temp,
            'system_state': 'heating' if current < target else 'cooling',
        }
    
    def _generate_report(self):
        """Genitive case: generate performance report."""
        return {
            'current_temperature': self.parameters['current_temp'],
            'target_temperature': self.parameters['target_temp'],
            'system_status': 'heating' if self.parameters['current_temp'] < self.parameters['target_temp'] else 'cooling',
            'efficiency': self._calculate_efficiency(),
        }
    
    def _calculate_efficiency(self):
        """Helper method for report generation."""
        # Simple efficiency calculation
        temp_diff = abs(self.parameters['current_temp'] - self.parameters['target_temp'])
        return max(0, 100 - (temp_diff * 5))
    
    def update(self, prediction, observation):
        """Update model based on observations."""
        if self.current_case.abbreviation == 'ACC':
            # As recipient of updates
            return self._update_parameters(observation)
        else:
            return {"error": "Update not implemented for this case"}
    
    def _update_parameters(self, observation):
        """Accusative case: update model parameters."""
        if 'measured_temp' in observation:
            self.parameters['current_temp'] = observation['measured_temp']
        
        if 'new_target' in observation:
            self.parameters['target_temp'] = observation['new_target']
        
        return {
            'updated_parameters': {
                'current_temp': self.parameters['current_temp'],
                'target_temp': self.parameters['target_temp'],
            },
            'update_status': 'success',
        }
    
    def transform_case(self, target_case_abbr):
        """Transform the model to a different case."""
        # Find the target case
        target_case = next((case for case in self.supported_cases 
                           if case.abbreviation == target_case_abbr), None)
        
        if not target_case:
            return {'success': False, 'error': 'Unsupported case'}
            
        # Perform the transformation
        old_case = self.current_case
        self.current_case = target_case
        
        return {
            'success': True,
            'previous_case': old_case.abbreviation,
            'new_case': target_case.abbreviation,
        }
```

### Using the Model

```python
# Create and register the model
model = TemperatureModel('home_thermostat')
registry = ModelRegistry()
registry.register_model(model, 'NOM')  # Register with nominative case

# Use as active predictor (nominative case)
prediction = model.predict()
print(f"Predicted temperature: {prediction['predicted_temp']}°F")
print(f"System state: {prediction['system_state']}")

# Transform to accusative case (recipient of updates)
model.transform_case('ACC')
update_result = model.update(None, {'measured_temp': 68.0, 'new_target': 72.0})
print(f"Update status: {update_result['update_status']}")
print(f"Updated parameters: {update_result['updated_parameters']}")

# Transform to genitive case (report generator)
model.transform_case('GEN')
report = model.predict()
print(f"Current temperature: {report['current_temperature']}°F")
print(f"Target temperature: {report['target_temperature']}°F")
print(f"System status: {report['system_status']}")
print(f"Efficiency: {report['efficiency']}%")
```

## Key Components

When working with CEREBRUM, keep these key components in mind:

### 1. GenerativeModel

The base class for all CEREBRUM models, providing:
- Parameter management
- State tracking
- Case transformation capabilities
- Prediction and update methods

### 2. Case

Represents a linguistic case with:
- Name and abbreviation
- Parameter access patterns
- Interface requirements
- Update dynamics

### 3. ModelRegistry

Manages model instances:
- Registers models with initial cases
- Provides model lookup
- Tracks model relationships
- Maintains case assignments

### 4. Transformation Engine

Handles case transformations:
- Validates transformation validity
- Optimizes transformations
- Applies transformations
- Maintains transformation history

## Development Workflow

A typical development workflow with CEREBRUM includes:

1. **Define your model**: Create a subclass of `GenerativeModel`
2. **Implement case-specific behaviors**: Add methods for different cases
3. **Register with ModelRegistry**: Make your model available to the framework
4. **Transform cases as needed**: Change your model's functional role
5. **Connect models together**: Use the message bus for model communication

## Next Steps

After building your first model, explore these advanced topics:

1. [Active Inference Integration](active_inference_integration.md): Learn how to apply free energy principles
2. [Model Examples](model_examples.md): Study more complex example implementations
3. [Core Specification](cerebrum_core_spec.md): Understand the complete framework architecture

## Troubleshooting

### Common Issues

1. **Case transformation errors**: Ensure your model supports the target case
2. **Prediction/update errors**: Check that you've implemented the appropriate methods for each case
3. **Registry errors**: Verify that your model is properly registered with the correct initial case

### Getting Help

- Check the [GitHub Issues](https://github.com/ActiveInferenceInstitute/CEREBRUM/issues) for known problems
- Join the community discussion forum
- Review the detailed documentation in the `docs/` directory 