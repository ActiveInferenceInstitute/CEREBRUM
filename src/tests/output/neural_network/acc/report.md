# Neural Network in ACC Case

## Direct object/Receiver of action

Statistical role: Target of optimization

### Context

The neural network as an object being trained or evaluated

### Mathematical Representation

```
L(θ) = (1/n)∑(y - ŷ)²
```

### Primary Methods

evaluate(), calculate_loss()

### Example

The researcher TRAINS the NEURAL NETWORK

### Analysis Results

* Model architecture: Input(1) → Hidden(12) → Hidden(6) → Output(1)
* Activation function: tanh
* Evaluation metrics:
  - mse: 0.010405
  - mae: 0.078077
  - r2: 0.937965

### Evaluation Process

The model was evaluated across multiple test segments to analyze the stability of its performance metrics. The animation shows how different evaluation metrics evolve as more test data is included.

### Visualizations

1. [Network Structure](network_structure.png)
2. [Evaluation Results](evaluation_results.png)
3. [Evaluation Animation](evaluation_animation.gif)

### ACCUSATIVE Case Significance

In the ACCUSATIVE case, the neural network is treated as the direct object of evaluation or training. This perspective emphasizes how the model receives updates and undergoes evaluation, rather than its active role in generating predictions.