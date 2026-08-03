# Getting Started with CEREBRUM

This guide helps new developers get started with the CEREBRUM framework. It covers setting up the development environment, installing dependencies, and building a simple CEREBRUM model.

## Table of Contents

- [Prerequisites](#prerequisites)
- [Installation](#installation)
  - [Python Implementation](#python-implementation)
  - [Other Languages](#other-languages)
- [Your First CEREBRUM Model](#your-first-cerebrum-model)
  - [Python Example](#python-example)
  - [Using the Model](#using-the-model)
- [Key Components](#key-components)
- [Development Workflow](#development-workflow)
- [Contributing to CEREBRUM](#contributing-to-cerebrum)
- [Next Steps](#next-steps)
- [Troubleshooting](#troubleshooting)

## Prerequisites

Before you begin, ensure you have the following installed:

- Python 3.9+ (the reference implementation lives in `src/`)
- Git for version control
- [uv](https://docs.astral.sh/uv/) (recommended) for dependency management

The reference implementation is Python-only. Case-system mappings and
implementation guidelines for other languages (JavaScript, Rust, Julia, and
35+ more) are provided as documentation in [`docs/languages/computer/`](languages/computer/README.md).

## Installation

### Python Implementation

We use [uv](https://docs.astral.sh/uv/) for fast, reliable Python dependency management.

1. Clone the repository:

   ```bash
   git clone https://github.com/ActiveInferenceInstitute/CEREBRUM.git
   cd CEREBRUM
   ```

2. Install uv (if not already installed):

   ```bash
   curl -LsSf https://astral.sh/uv/install.sh | sh
   ```

3. Create a virtual environment and install dependencies:

   ```bash
   uv venv
   source .venv/bin/activate  # On Windows: .venv\Scripts\activate
   uv pip install -e .
   ```

4. Install the development version:

   ```bash
   uv pip install -e ".[dev]"
   ```

### Other Languages

There are no runnable non-Python implementations in this repository. If you
want to implement CEREBRUM in another language, see the
[Language Implementation Guidelines](language_implementations.md) and the
per-language mappings in [`docs/languages/computer/`](languages/computer/README.md).

## Your First CEREBRUM Model

Let's create a simple temperature model that can transform between different cases. This example demonstrates the core concepts of CEREBRUM including case transformations and role-specific behaviors.

### Python Example

```python
from src import Model, Case, register_model, get_global_registry
from src.transformations import transform_case

# 1. Define a simple model
class TemperatureModel(Model):
    def __init__(self, model_id):
        super().__init__(name=model_id, parameters={
            'current_temp': 72.0,
            'target_temp': 70.0,
            'heating_rate': 0.5,
            'cooling_rate': 0.3,
        })

    def predict(self, inputs=None):
        """Generate predictions based on current case."""
        current = self.parameters['current_temp']
        target = self.parameters['target_temp']

        if self.case == Case.NOMINATIVE:
            # As active predictor
            if current < target:
                new_temp = current + self.parameters['heating_rate']
            else:
                new_temp = current - self.parameters['cooling_rate']
            return {
                'predicted_temp': new_temp,
                'system_state': 'heating' if current < target else 'cooling',
            }
        elif self.case == Case.GENITIVE:
            # As report generator
            return {
                'current_temperature': current,
                'target_temperature': target,
                'system_status': 'heating' if current < target else 'cooling',
                'efficiency': max(0, 100 - abs(current - target) * 5),
            }
        else:
            return {"error": f"Prediction not implemented for case {self.case}"}

    def update(self, prediction=None, observation=None):
        """Update model parameters based on observations."""
        if self.case == Case.ACCUSATIVE:
            # As recipient of updates
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
        else:
            return {"error": f"Update not implemented for case {self.case}"}
```

### Using the Model

```python
# Create and register the model
model = TemperatureModel('home_thermostat')
register_model(model, tags=["demo"])  # Persists to output/model_registry/

# A model starts in the nominative case (active predictor)
print(f"Initial case: {model.case}")          # Case.NOMINATIVE

# Use as active predictor (nominative case)
prediction = model.predict()
print(f"Predicted temperature: {prediction['predicted_temp']}°F")
print(f"System state: {prediction['system_state']}")

# Transform to accusative case (recipient of updates)
transform_case(model, Case.ACCUSATIVE)
update_result = model.update(None, {'measured_temp': 68.0, 'new_target': 72.0})
print(f"Update status: {update_result['update_status']}")
print(f"Updated parameters: {update_result['updated_parameters']}")

# Transform to genitive case (report generator)
transform_case(model, Case.GENITIVE)
report = model.predict()
print(f"Current temperature: {report['current_temperature']}°F")
print(f"Target temperature: {report['target_temperature']}°F")
print(f"System status: {report['system_status']}")
print(f"Efficiency: {report['efficiency']}%")
```

#### Expected Output

```
Initial case: Case.NOMINATIVE
Predicted temperature: 71.7°F
System state: cooling
Update status: success
Updated parameters: {'current_temp': 68.0, 'target_temp': 72.0}
Current temperature: 68.0°F
Target temperature: 72.0°F
System status: heating
Efficiency: 80.0%
```

## Key Components

When working with CEREBRUM, keep these key components in mind:

### 1. Model (`src.core.model.Model`)

The base class for all CEREBRUM models (`Model(name=None, parameters=None)`),
providing:

- Parameter management (`model.parameters`)
- Case state tracking (`model.case`)
- `predict()` and `update()` hooks that dispatch on the current case

### 2. Case (`src.core.model.Case`)

An enum of the eight standard linguistic cases:

- `Case.NOMINATIVE` [NOM] — model as active agent
- `Case.ACCUSATIVE` [ACC] — model as object of a process
- `Case.GENITIVE` [GEN] — model as source/possessor
- `Case.DATIVE` [DAT] — model as recipient
- `Case.INSTRUMENTAL` [INS] — model as method/tool
- `Case.LOCATIVE` [LOC] — model as context
- `Case.ABLATIVE` [ABL] — model as origin/cause
- `Case.VOCATIVE` [VOC] — model as addressable entity

### 3. ModelRegistry (`src.core.model_registry.ModelRegistry`)

Manages model instances:

- Registers models with initial case assignments (`register_model(model, tags=...)`)
- Persists models to `output/model_registry/`
- Provides model lookup (`get_global_registry().get_model(name)`)

### 4. Transformation Functions (`src.transformations`)

Handle case transformations:

- `transform_case(model, target_case)` — apply a case transformation
- `revert_case(model)` — revert to the previous case

## Development Workflow

A typical development workflow with CEREBRUM includes:

1. **Define your model**: Create a subclass of `Model` with a `parameters` dict
2. **Implement case-specific behaviors**: Branch on `self.case` inside `predict()` / `update()`
3. **Register with ModelRegistry**: `register_model(model, tags=[...])`
4. **Transform cases as needed**: `transform_case(model, Case.ACCUSATIVE)`
5. **Connect models together**: Use `model.connect(other)` to link models

## Contributing to CEREBRUM

If you're interested in contributing to CEREBRUM, we welcome contributions across a wide spectrum of activities:

### Research Contributions

- **Theoretical Development**
  - Extending the mathematical framework
  - Proposing novel case structures
  - Developing new inference algorithms

- **Empirical Testing**
  - Designing experiments
  - Benchmarking performance
  - Creating validation datasets

### Technical Contributions

- **Core Framework Development**
  - Implementing new model components
  - Optimizing existing implementations
  - Improving the transformation engine

- **Language Implementations**
  - Creating or improving implementation guidelines for various languages
  - Building integrations with other frameworks
  - Developing specialized variants

### Documentation & Examples

- **Educational Content**
  - Creating tutorials and guides
  - Developing learning resources
  - Documenting use cases

- **Model Examples**
  - Creating demonstrative models
  - Implementing domain-specific applications
  - Building interactive demonstrations

### Getting Started as a Contributor

1. Choose an area that matches your interests and skills
2. Check the GitHub issues for "good first issue" tags
3. Read our contribution guides:
   - [Technical Contributions](contributing_technical.md)
   - [Research Contributions](contributing_research.md)
   - [Documentation Contributions](contributing_documentation.md)
4. Join our community channels to connect with other contributors
5. Start with small contributions to familiarize yourself with the codebase

## Next Steps

After building your first model, explore these advanced topics:

1. [Active Inference Integration](active_inference_integration.md): Learn how to apply free energy principles
2. [Model Examples](model_examples.md): Study more complex example implementations
3. [Core Specification](cerebrum_core_spec.md): Understand the complete framework architecture
4. [Language Nuance Handling](language_nuance_handling.md): Explore linguistic features and capabilities
5. [Implementation Roadmap](implementation_roadmap.md): Learn about upcoming features and improvements

## Troubleshooting

### Common Issues

1. **Case transformation errors**: Ensure your model supports the target case.

   ```python
   # Check the current case
   print(f"Current case: {model.case}")
   ```

2. **Prediction/update errors**: Check that you've implemented the appropriate methods for each case.

   ```python
   # Debug case-specific method dispatch
   if model.case == Case.NOMINATIVE:
       print("Using nominative prediction method")
   ```

3. **Registry errors**: Verify that your model is properly registered.

   ```python
   # Verify model registration
   registered_model = get_global_registry().get_model(model.name)
   print(f"Is registered: {registered_model is not None}")
   ```

### Getting Help

- Check the [GitHub Issues](https://github.com/ActiveInferenceInstitute/CEREBRUM/issues) for known problems
- Join the [community discussion forum](https://github.com/ActiveInferenceInstitute/CEREBRUM/discussions)
- Review the detailed documentation in the `docs/` directory
