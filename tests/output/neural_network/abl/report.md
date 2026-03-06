# Neural Network in ABL Case

## From/Out of/Because of

Statistical role: Error source

### Context

The neural network as the source of prediction errors and gradients

### Mathematical Representation

```
∇L = ∂L/∂θ
```

### Primary Methods

compute_gradients(), backpropagate()

### Example

The errors ORIGINATE FROM the NEURAL NETWORK's weights

### Analysis Results

* Model architecture: Input(1) → Hidden(10) → Hidden(5) → Output(1)
* Activation function: relu
* Training not allowed in ABLATIVE case

### Visualizations

1. [Network Structure](network_structure.png)
2. [Training History](training_history.png)
3. [Predictions](predictions.png)
4. [Error Distribution](error_distribution.png)
5. [Gradient Magnitudes per Layer](gradient_magnitudes.png)
