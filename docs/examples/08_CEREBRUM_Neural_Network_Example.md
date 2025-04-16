# CEREBRUM Example: Deconstructing Neural Networks with Linguistic Cases

This document explores the application of the CEREBRUM framework to Neural Networks (NNs), providing a multi-faceted understanding based on linguistic cases. The concepts and demonstrations are derived from the comprehensive test suite in `src/tests/test_neural_network.py`.

## Introduction to Neural Networks

**Neural Networks** are powerful machine learning models inspired by the structure of the brain. They consist of interconnected nodes (neurons) organized in layers (input, hidden, output). NNs learn complex patterns by adjusting the strengths (weights) of these connections during a training process, typically involving:
- **Forward Propagation:** Input data flows through the network, layer by layer, undergoing linear transformations (weighted sums + biases) and non-linear activation functions (e.g., ReLU, Sigmoid, Tanh) to produce an output.
- **Loss Calculation:** The network's output is compared to the true target values using a loss function (e.g., Mean Squared Error for regression, Cross-Entropy for classification).
- **Backpropagation:** The error is propagated backward through the network, calculating gradients (the rate of change of the loss with respect to each weight and bias).
- **Weight Update:** Weights and biases are adjusted in the direction that minimizes the loss, typically using an optimization algorithm like Gradient Descent.

## CEREBRUM Cases for Neural Networks

Neural networks are complex systems with many interacting components and processes. CEREBRUM's linguistic cases provide distinct viewpoints to analyze these complexities, as defined and tested in `test_neural_network.py`:

### 1. NOMINATIVE Case: The Network as Active Predictor

- **Linguistic Meaning:** Subject/Doer of action.
- **NN Context:** Focuses on the trained network *actively generating predictions* based on new input data. This emphasizes the forward pass (`predict()`, `forward()`) as the primary action.
- **Test Focus:** Training the network and then using it to make predictions. Visualizations often include plots comparing predictions to actual values, the learning curve during training (loss vs. epoch), and potentially animations of the output changing as the network learns.
- **Why it Matters:** Understanding the network's core function: mapping inputs to outputs after learning.

### 2. ACCUSATIVE Case: The Network as Object of Training/Evaluation

- **Linguistic Meaning:** Direct object/Receiver of action.
- **NN Context:** Focuses on the network *being modified* during training (weights updated) or *being assessed* during evaluation. The network is the *object* receiving updates or scrutiny (`train()`, `evaluate()`).
- **Test Focus:** Analyzing the training process itself (loss curves, learning dynamics) and evaluating the final model's performance on unseen data using various metrics (accuracy, MSE, precision, recall). Visualizations include training/validation loss curves, confusion matrices, and metric plots.
- **Why it Matters:** Understanding how the network learns and assessing its final performance and generalization.

### 3. DATIVE Case: The Network as Recipient of Inputs

- **Linguistic Meaning:** Indirect object/Recipient.
- **NN Context:** Focuses on the network *receiving* input data. How does the network process different types of inputs? How sensitive is it to the input distribution?
- **Test Focus:** Feeding various input distributions *to* the network and observing the resulting activations in the initial layers. Analyzing input sensitivity by perturbing inputs. Visualizations include histograms of input data, plots of first-layer activations for different inputs, and input sensitivity analyses.
- **Why it Matters:** Understanding how the network initially processes and responds to incoming information.

### 4. GENITIVE Case: The Network's Outputs and Internal States

- **Linguistic Meaning:** Possessive/Source.
- **NN Context:** Focuses on the properties *of* the network's outputs or internal states (activations). The network is the *source* of these outputs and representations.
- **Test Focus:** Analyzing the distribution of the network's predictions (`predict()`) or the activations *of* specific layers. Visualizations often include histograms of output values or activation distributions across different layers.
- **Why it Matters:** Characterizing the nature of the information produced or represented by the network.

### 5. INSTRUMENTAL Case: The Network as Computational Method

- **Linguistic Meaning:** By means of/Using.
- **NN Context:** Views the network architecture, its weights, biases, and activation functions as the *instrument* or *computational method* used to transform inputs into outputs.
- **Test Focus:** Analyzing the network's structure (layers, connections), visualizing the weight matrices, examining the activation functions, and potentially comparing different architectures or learning algorithms as computational tools. Visualizations include network diagrams, weight heatmaps/plots, and activation function graphs.
- **Why it Matters:** Understanding the specific computational mechanisms employed by the network.

### 6. LOCATIVE Case: The Network as Representational Space

- **Linguistic Meaning:** In/At/Within (Location).
- **NN Context:** Focuses on the hidden layers *as* spaces where data is represented. How are inputs transformed and clustered *within* these internal representational spaces?
- **Test Focus:** Extracting and visualizing hidden layer activations (`get_representation()`, `extract_features()`). Techniques like PCA or t-SNE are often used to project high-dimensional activations into 2D or 3D for visualization, often colored by class labels.
- **Why it Matters:** Gaining insights into how the network internally organizes and transforms information to solve the task.

### 7. ABLATIVE Case: The Network as Source of Errors/Gradients

- **Linguistic Meaning:** From/Out of/Because of.
- **NN Context:** Focuses on the network (specifically its current weights and predictions) as the *source* from which errors and gradients originate during backpropagation (`compute_gradients()`, `backpropagate()`).
- **Test Focus:** Analyzing the distribution of prediction errors (residuals) and the magnitude and flow of gradients backward through the layers. Visualizations include error histograms and plots/histograms of gradient magnitudes for each layer.
- **Why it Matters:** Understanding the learning signals (gradients) that drive weight updates and diagnosing issues like vanishing or exploding gradients.

### 8. VOCATIVE Case: The Network as Interactive Interface

- **Linguistic Meaning:** Direct address/Invocation.
- **NN Context:** Views the trained network as an addressable entity that can be queried or interacted with, often via a defined interface (`query()`, `get_response()`).
- **Test Focus:** Simulating interaction by providing specific inputs and observing the outputs. For classification tasks, visualizing the decision boundary the network has learned, showing how it responds *to* different inputs across the feature space.
- **Why it Matters:** Testing the network's response in specific scenarios and understanding its input-output mapping as an interactive function.

## Demonstration (`test_neural_network.py`)

The extensive test suite for neural networks embodies these cases:
1.  **Case Definitions:** A dedicated `CaseDefinitions` class provides detailed linguistic and NN context for each case.
2.  **Data Generation:** Functions (`DataGenerator`) create synthetic regression and classification datasets.
3.  **Visualization:** A `Visualizer` class provides methods for plotting data, network structure, and training history. A `plot_case_linguistic_context` function visualizes the definition of each case.
4.  **Case-Specific Tests:** Functions like `test_nominative_case`, `test_accusative_case`, etc., instantiate `NeuralNetworkModel`, train/evaluate it, and generate visualizations tailored to the specific case perspective.
5.  **Output Generation:** Results, including detailed plots, reports (`report.md`), and animations, are saved in case-specific subdirectories within `src/tests/output/neural_network/`. Examples include:
    - Network structure diagrams (INSTRUMENTAL)
    - Training loss curves (ACCUSATIVE)
    - Prediction plots (NOMINATIVE)
    - Evaluation metric plots/animations (ACCUSATIVE)
    - Input distribution & sensitivity plots (DATIVE)
    - Output/Activation histograms (GENITIVE)
    - Hidden activation PCA/t-SNE plots (LOCATIVE)
    - Error distribution & Gradient magnitude plots (ABLATIVE)
    - Decision boundary plots (VOCATIVE)

## Conclusion

Neural networks, while powerful, can often feel like black boxes. Applying the CEREBRUM framework, as demonstrated in `test_neural_network.py`, provides a systematic way to dissect their behavior from multiple angles. By adopting the viewpoints offered by linguistic cases—seeing the network as an actor (Nominative), an object of learning (Accusative), a representational space (Locative), a source of errors (Ablative), etc.—we can achieve a more comprehensive and interpretable understanding. The rich set of visualizations generated for each case serves as tangible evidence of the insights gained from that specific perspective, ultimately aiding in the design, debugging, and application of neural network models. 