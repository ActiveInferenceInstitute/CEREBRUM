"""
Tests for src/models/linear_regression.py

Comprehensive tests for LinearRegressionModel with real sklearn,
covering fit, predict, evaluate, receive_data, process_data_buffer,
and respond_to_query (all 6 query types).
"""

import pytest
import numpy as np

from src.models.linear_regression import LinearRegressionModel
from src.models.base import Case


@pytest.fixture
def data():
    """Generate simple linear data: y = 2x + 1 + noise."""
    np.random.seed(42)
    X = np.random.rand(50, 1) * 10
    y = 2 * X.ravel() + 1 + np.random.randn(50) * 0.5
    return X, y


@pytest.fixture
def fitted_model(data):
    """Return a fitted LinearRegressionModel in NOMINATIVE case."""
    X, y = data
    model = LinearRegressionModel("test_lr", Case.NOMINATIVE)
    model.fit(X, y)
    return model


class TestInit:
    def test_creates_model(self):
        model = LinearRegressionModel("test", Case.NOMINATIVE)
        assert model.name == "test"
        assert model.case == Case.NOMINATIVE
        assert not model._is_fitted

    def test_custom_hyperparameters(self):
        model = LinearRegressionModel("test", Case.NOMINATIVE, hyperparameters={"fit_intercept": False})
        assert model._hyperparameters["fit_intercept"] is False


class TestFit:
    def test_fit_returns_self(self, data):
        X, y = data
        model = LinearRegressionModel("test", Case.NOMINATIVE)
        result = model.fit(X, y)
        assert result is model

    def test_sets_fitted_flag(self, data):
        X, y = data
        model = LinearRegressionModel("test", Case.NOMINATIVE)
        model.fit(X, y)
        assert model._is_fitted is True

    def test_stores_params(self, fitted_model):
        assert fitted_model._params is not None
        assert "intercept" in fitted_model._params
        assert "coefficients" in fitted_model._params

    def test_coefficient_close_to_2(self, fitted_model):
        coef = fitted_model._params["coefficients"][0]
        assert abs(coef - 2.0) < 0.5  # Should be close to 2

    def test_intercept_close_to_1(self, fitted_model):
        intercept = fitted_model._params["intercept"]
        assert abs(intercept - 1.0) < 1.0


class TestPredict:
    def test_predict_returns_array(self, fitted_model):
        X_new = np.array([[5.0]])
        preds = fitted_model.predict(X_new)
        assert isinstance(preds, np.ndarray)

    def test_predict_shape(self, fitted_model, data):
        X, _ = data
        preds = fitted_model.predict(X)
        assert len(preds) == len(X)

    def test_unfitted_raises(self):
        model = LinearRegressionModel("test", Case.NOMINATIVE)
        with pytest.raises(ValueError, match="fitted"):
            model.predict(np.array([[1.0]]))


class TestEvaluate:
    def test_returns_metrics(self, fitted_model, data):
        X, y = data
        metrics = fitted_model.evaluate(X, y)
        assert "r2" in metrics
        assert "mse" in metrics
        assert "mae" in metrics
        assert "f_statistic" in metrics

    def test_r2_high(self, fitted_model, data):
        X, y = data
        metrics = fitted_model.evaluate(X, y)
        assert metrics["r2"] > 0.95  # Good fit

    def test_accusative_case_evaluation(self, data):
        X, y = data
        model = LinearRegressionModel("test_acc", Case.ACCUSATIVE)
        model.fit(X, y)
        metrics = model.evaluate(X, y)
        assert metrics["r2"] > 0.9

    def test_with_fitted_params(self, fitted_model, data):
        X, y = data
        params = {"intercept": 1.0, "coefficients": [2.0]}
        metrics = fitted_model.evaluate(X, y, fitted_params=params)
        assert "r2" in metrics


class TestReceiveData:
    def test_receive_data_returns_self(self, data):
        X, y = data
        model = LinearRegressionModel("test", Case.DATIVE)
        result = model.receive_data(X, y)
        assert result is model

    def test_stores_in_buffer(self, data):
        X, y = data
        model = LinearRegressionModel("test", Case.DATIVE)
        model.receive_data(X, y)
        assert model.data_buffer["X"] is not None
        assert model.data_buffer["y"] is not None


class TestProcessDataBuffer:
    def test_processes_buffer(self, data):
        X, y = data
        model = LinearRegressionModel("test", Case.DATIVE)
        model.receive_data(X, y)
        X_proc, y_proc = model.process_data_buffer()
        assert X_proc is not None
        assert y_proc is not None

    def test_centers_data_in_dative(self, data):
        X, y = data
        model = LinearRegressionModel("test", Case.DATIVE)
        model.receive_data(X, y)
        X_proc, y_proc = model.process_data_buffer()
        assert abs(np.mean(X_proc)) < 1e-10  # Centered

    def test_empty_buffer_raises(self):
        model = LinearRegressionModel("test", Case.DATIVE)
        with pytest.raises(ValueError, match="No data"):
            model.process_data_buffer()


class TestRespondToQuery:
    def test_predict_query(self, fitted_model):
        X_new = np.array([[5.0]])
        response = fitted_model.respond_to_query("predict", data=X_new)
        assert response["status"] == "success"
        assert "prediction" in response

    def test_confidence_query(self, fitted_model):
        X_new = np.array([[5.0]])
        response = fitted_model.respond_to_query("confidence", data=X_new)
        assert response["status"] == "success"
        assert "lower_bound" in response
        assert "upper_bound" in response

    def test_error_query(self, fitted_model):
        X_new = np.array([[5.0]])
        response = fitted_model.respond_to_query("error", data=X_new, true_value=11.0)
        assert response["status"] == "success"
        assert "error" in response

    def test_formula_query(self, fitted_model):
        response = fitted_model.respond_to_query("formula")
        assert response["status"] == "success"
        assert "formula" in response
        assert "y = " in response["formula"]

    def test_range_query(self, fitted_model):
        response = fitted_model.respond_to_query("range")
        assert response["status"] == "success"
        assert "x_min" in response

    def test_explain_query(self, fitted_model):
        X_new = np.array([[5.0]])
        response = fitted_model.respond_to_query("explain", data=X_new)
        assert response["status"] == "success"
        assert "components" in response

    def test_unknown_query(self, fitted_model):
        response = fitted_model.respond_to_query("unknown_type")
        assert response["status"] == "error"

    def test_unfitted_returns_error(self):
        model = LinearRegressionModel("test", Case.VOCATIVE)
        response = model.respond_to_query("predict", data=np.array([[1.0]]))
        assert "error" in response

    def test_missing_data_returns_error(self, fitted_model):
        response = fitted_model.respond_to_query("predict")
        assert "error" in response
