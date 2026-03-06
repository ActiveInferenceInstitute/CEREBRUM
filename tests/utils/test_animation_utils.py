"""
Tests for src/utils/animation.py

Tests ensure_scalar utility — the main testable pure function.
save_animation/save_frames_as_gif require imageio + matplotlib animation
and are tested via integration patterns only.
"""

import pytest
import numpy as np

from src.utils.animation import ensure_scalar


class TestEnsureScalar:
    """Tests for the ensure_scalar helper function."""

    def test_none_returns_zero(self):
        assert ensure_scalar(None) == 0

    def test_single_element_array(self):
        assert ensure_scalar(np.array([42.0])) == 42.0

    def test_multi_element_same_value(self):
        arr = np.array([7.0, 7.0, 7.0])
        assert ensure_scalar(arr) == 7.0

    def test_multi_element_different_values(self):
        arr = np.array([1.0, 2.0, 3.0])
        result = ensure_scalar(arr)
        # Should return first element as last resort
        assert result == 1.0

    def test_empty_array(self):
        assert ensure_scalar(np.array([])) == 0

    def test_2d_single_element(self):
        arr = np.array([[5.0]])
        assert ensure_scalar(arr) == 5.0

    def test_scalar_float(self):
        assert ensure_scalar(3.14) == 3.14

    def test_scalar_int(self):
        assert ensure_scalar(42) == 42.0

    def test_empty_list(self):
        assert ensure_scalar([]) == 0

    def test_single_element_list(self):
        assert ensure_scalar([99.0]) == 99.0

    def test_string_returns_as_is(self):
        """Non-convertible types should be returned unchanged."""
        assert ensure_scalar("hello") == "hello"

    def test_numpy_scalar(self):
        assert ensure_scalar(np.float64(2.5)) == 2.5

    def test_numpy_int_scalar(self):
        assert ensure_scalar(np.int32(10)) == 10.0
