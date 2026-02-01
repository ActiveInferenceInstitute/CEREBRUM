"""
CEREBRUM Scripts Module

This module provides executable scripts for running tests, examples, and utilities.
"""

from .run_all_tests import main as run_tests
from .run_all_examples import main as run_examples

__all__ = [
    'run_tests',
    'run_examples',
]