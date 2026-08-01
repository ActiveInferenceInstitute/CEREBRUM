"""
CEREBRUM Scripts Module

This module provides executable scripts for running tests, examples, and utilities.

Uses lazy loading so that importing the package does not import heavy or
incompletely-implemented test backends (e.g. `tests.models.test_linear_regression`,
which pytest.ini explicitly ignores).
"""

__all__ = [
    'run_tests',
    'run_examples',
]


def __getattr__(name):
    """Lazy import to avoid importing broken/ignored test backends at package load."""
    if name == 'run_tests':
        from .run_all_tests import main as run_tests
        return run_tests
    elif name == 'run_examples':
        from .run_all_examples import main as run_examples
        return run_examples
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")
