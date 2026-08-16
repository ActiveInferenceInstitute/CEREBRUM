"""
Tests for NeuralNetworkModel case-specific update methods and edge cases.

Targets the uncovered _update_* methods and free_energy calculation
to raise coverage from 58% toward 80%+.
"""

import numpy as np
import pytest

from src.core.model import Case
from src.core.neural_network import NeuralNetworkModel


@pytest.fixture
def nn():
    """Create a simple NeuralNetworkModel for testing."""
    return NeuralNetworkModel(
        name="TestNN", input_dim=2, output_dim=1, hidden_dims=[4], activation='relu'
    )


@pytest.fixture
def trained_nn():
    """Create a trained NeuralNetworkModel."""
    model = NeuralNetworkModel(
        name="TrainedNN", input_dim=2, output_dim=1, hidden_dims=[4], activation='relu'
    )
    X = np.random.randn(50, 2)
    y = (X[:, 0:1] + X[:, 1:2]) * 0.5
    model.train(X, y, epochs=5, learning_rate=0.01, verbose=False)
    return model


# ── Activation Functions ──────────────────────────────────────────

class TestActivations:
    def test_relu(self, nn):
        x = np.array([-1, 0, 1, 2])
        result = nn._activation_function(x)
        np.testing.assert_array_equal(result, [0, 0, 1, 2])

    def test_sigmoid(self):
        model = NeuralNetworkModel(activation='sigmoid', input_dim=1, output_dim=1)
        x = np.array([0.0])
        result = model._activation_function(x)
        assert result[0] == pytest.approx(0.5)

    def test_tanh(self):
        model = NeuralNetworkModel(activation='tanh', input_dim=1, output_dim=1)
        x = np.array([0.0])
        result = model._activation_function(x)
        assert result[0] == pytest.approx(0.0)

    def test_unknown_activation(self):
        model = NeuralNetworkModel(activation='unknown', input_dim=1, output_dim=1)
        x = np.array([3.0])
        result = model._activation_function(x)
        assert result[0] == 3.0  # linear fallback

    def test_relu_derivative(self, nn):
        x = np.array([-1, 0, 1, 2])
        dydx = nn._activation_derivative(x)
        np.testing.assert_array_equal(dydx, [0, 0, 1, 1])

    def test_sigmoid_derivative(self):
        model = NeuralNetworkModel(activation='sigmoid', input_dim=1, output_dim=1)
        x = np.array([0.0])
        dydx = model._activation_derivative(x)
        assert dydx[0] == pytest.approx(0.25)

    def test_tanh_derivative(self):
        model = NeuralNetworkModel(activation='tanh', input_dim=1, output_dim=1)
        x = np.array([0.0])
        dydx = model._activation_derivative(x)
        assert dydx[0] == pytest.approx(1.0)


# ── Forward & Backward ───────────────────────────────────────────

class TestForwardBackward:
    def test_forward_shape(self, nn):
        X = np.random.randn(10, 2)
        output, activations = nn.forward(X)
        assert output.shape == (10, 1)
        assert len(activations) == 3  # input + hidden + output

    def test_multiclass_forward_returns_probabilities(self):
        model = NeuralNetworkModel(input_dim=2, output_dim=3, hidden_dims=[4])
        output, _ = model.forward(np.ones((5, 2)))
        assert np.all(output >= 0.0)
        np.testing.assert_allclose(output.sum(axis=1), 1.0)

    def test_mse_backward_gradient_is_normalized(self):
        model = NeuralNetworkModel(input_dim=1, output_dim=1, hidden_dims=[])
        model.weights[0][:] = 0.5
        model.biases[0][:] = 0.1
        X = np.array([[2.0], [3.0]])
        y = np.array([[0.0], [1.0]])
        old_weight = model.weights[0][0, 0]
        epsilon = 1e-6
        model.weights[0][0, 0] = old_weight + epsilon
        loss_plus = np.mean((model.forward(X)[0] - y) ** 2)
        model.weights[0][0, 0] = old_weight - epsilon
        loss_minus = np.mean((model.forward(X)[0] - y) ** 2)
        model.weights[0][0, 0] = old_weight
        expected = (loss_plus - loss_minus) / (2 * epsilon)
        result = model.backward(X, y, learning_rate=0.0)
        np.testing.assert_allclose(result["weight_grads"][0][0, 0], expected, rtol=1e-5, atol=1e-7)

    def test_backward_returns_loss(self, nn):
        X = np.random.randn(10, 2)
        y = np.random.randn(10, 1)
        result = nn.backward(X, y)
        assert 'loss' in result
        assert result['loss'] >= 0

    def test_backward_updates_weights(self, nn):
        X = np.random.randn(10, 2)
        y = np.random.randn(10, 1)
        old_weights = [w.copy() for w in nn.weights]
        nn.backward(X, y)
        changed = any(not np.array_equal(old_weights[i], nn.weights[i]) for i in range(len(nn.weights)))
        assert changed

    def test_backward_with_metrics_returns_gradients(self, nn):
        """Regression: _backward_with_metrics must return one gradient per layer."""
        X = np.random.randn(10, 2)
        y = np.random.randn(10, 1)
        y_pred = nn.predict(X)
        weight_updates, gradients = nn._backward_with_metrics(X, y, y_pred)
        # One gradient per weight matrix (input->hidden, hidden->output)
        assert len(gradients) == len(nn.weights)
        for grad, weight in zip(gradients, nn.weights):
            assert grad.shape == weight.shape
        # weight_updates list is produced but empty in the current implementation
        assert isinstance(weight_updates, list)

    @pytest.mark.parametrize("activation", ["relu", "sigmoid", "tanh", "linear"])
    def test_backward_with_metrics_all_activations(self, activation):
        """_backward_with_metrics must not raise for any supported activation."""
        model = NeuralNetworkModel(
            name="ActNN", input_dim=2, output_dim=1, hidden_dims=[3], activation=activation
        )
        X = np.random.randn(8, 2)
        y = np.random.randn(8, 1)
        y_pred = model.predict(X)
        _, gradients = model._backward_with_metrics(X, y, y_pred)
        assert len(gradients) == len(model.weights)

    def test_predict(self, nn):
        X = np.random.randn(5, 2)
        pred = nn.predict(X)
        assert pred.shape == (5, 1)
        assert len(nn.prediction_history) == 1

    def test_evaluate(self, trained_nn):
        X = np.random.randn(20, 2)
        y = (X[:, 0:1] + X[:, 1:2]) * 0.5
        metrics = trained_nn.evaluate(X, y)
        assert 'mse' in metrics
        assert 'mae' in metrics
        assert 'r2' in metrics


# ── Training ──────────────────────────────────────────────────────

class TestTraining:
    def test_train_nominative_case(self, nn):
        X = np.random.randn(30, 2)
        y = np.random.randn(30, 1)
        result = nn.train(X, y, epochs=3, verbose=False)
        assert result['status'] == 'success'
        assert result['epochs_completed'] == 3
        assert nn.trained is True

    def test_train_blocked_in_wrong_case(self, nn):
        nn.case = Case.ACCUSATIVE  # training not allowed
        X = np.random.randn(10, 2)
        y = np.random.randn(10, 1)
        result = nn.train(X, y, epochs=1, verbose=False)
        assert result['status'] == 'error'


# ── Free Energy ───────────────────────────────────────────────────

class TestFreeEnergy:
    def test_no_history_returns_high(self, nn):
        assert nn.free_energy() == 1000.0

    def test_after_training(self, trained_nn):
        fe = trained_nn.free_energy()
        assert fe < 1000.0
        assert fe >= 0


# ── Case-Specific Updates ────────────────────────────────────────

class TestCaseUpdates:
    def test_update_nominative(self, nn):
        result = nn._update_nominative({"inputs": np.random.randn(5, 2)})
        assert result["status"] == "success"
        assert "predictions" in result

    def test_update_nominative_missing_inputs(self, nn):
        result = nn._update_nominative({"other": 123})
        assert result["status"] == "error"

    def test_update_nominative_bad_type(self, nn):
        result = nn._update_nominative("not_a_dict")
        assert result["status"] == "error"

    def test_update_accusative(self, nn):
        X = np.random.randn(10, 2)
        y = np.random.randn(10, 1)
        result = nn._update_accusative({"inputs": X, "targets": y})
        assert result["status"] == "success"
        assert "evaluation" in result

    def test_update_accusative_missing_keys(self, nn):
        result = nn._update_accusative({"inputs": np.random.randn(5, 2)})
        assert result["status"] == "error"

    def test_update_genitive(self, nn):
        result = nn._update_genitive({"inputs": np.random.randn(5, 2)})
        assert result["status"] == "success"
        assert "predictions" in result
        assert "lower_bound" in result
        assert "upper_bound" in result

    def test_update_dative(self, nn):
        result = nn._update_dative({"inputs": np.random.randn(5, 2)})
        assert result["status"] == "success"
        assert "processed_data" in result

    def test_update_instrumental_forward(self, nn):
        result = nn._update_instrumental({
            "operation": "forward",
            "inputs": np.random.randn(5, 2)
        })
        assert result["status"] == "success"
        assert result["operation"] == "forward"

    def test_update_instrumental_train(self, nn):
        result = nn._update_instrumental({
            "operation": "train",
            "inputs": np.random.randn(30, 2),
            "targets": np.random.randn(30, 1),
            "epochs": 2
        })
        assert result["status"] == "success"
        assert result["operation"] == "train"

    def test_update_instrumental_unknown_op(self, nn):
        result = nn._update_instrumental({"operation": "fly"})
        assert result["status"] == "error"

    def test_update_locative_architecture(self, nn):
        result = nn._update_locative({"context_type": "architecture"})
        assert result["status"] == "success"
        assert result["architecture"]["input_dim"] == 2

    def test_update_locative_training(self, trained_nn):
        result = trained_nn._update_locative({"context_type": "training"})
        assert result["status"] == "success"
        assert result["training_context"]["trained"] is True

    def test_update_locative_unknown(self, nn):
        result = nn._update_locative({"context_type": "quantum"})
        assert result["status"] == "error"

    def test_update_ablative(self, nn):
        X = np.random.randn(10, 2)
        y = np.random.randn(10, 1)
        result = nn._update_ablative({"inputs": X, "targets": y})
        assert result["status"] == "success"
        assert "layer_contributions" in result

    def test_update_vocative_predict(self, nn):
        result = nn._update_vocative({
            "query_type": "predict",
            "inputs": np.random.randn(3, 2)
        })
        assert result["status"] == "success"
        assert result["query_type"] == "predict"

    def test_update_vocative_architecture(self, nn):
        result = nn._update_vocative({"query_type": "architecture"})
        assert result["status"] == "success"
        assert "architecture" in result

    def test_update_vocative_evaluate(self, nn):
        X = np.random.randn(10, 2)
        y = np.random.randn(10, 1)
        result = nn._update_vocative({
            "query_type": "evaluate", "inputs": X, "targets": y
        })
        assert result["status"] == "success"

    def test_update_vocative_summary(self, nn):
        result = nn._update_vocative({"query_type": "summary"})
        assert result["status"] == "success"
        assert result["summary"]["name"] == "TestNN"

    def test_update_vocative_unknown(self, nn):
        result = nn._update_vocative({"query_type": "unknown"})
        assert result["status"] == "error"
