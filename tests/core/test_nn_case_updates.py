"""
Tests for NeuralNetworkModel case-specific update methods and edge cases.

Targets the uncovered _update_* methods and free_energy calculation
to raise coverage from 58% toward 80%+.
"""

import pytest
import numpy as np

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
