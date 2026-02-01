"""
CEREBRUM Linear Regression Case Tests Package

Contains test functions for each linguistic case in the linear regression context.
These tests require optional dependencies (imageio) for animation generation.

Usage:
    # Import test functions directly from their modules:
    from src.tests.linear_regression_cases.nominative_case import test_nominative_case
    from src.tests.linear_regression_cases.accusative_case import test_accusative_case
    # etc.
    
    # Or run via test_linear_regression.py which handles all imports
"""

# Note: Eager imports removed to prevent test collection failures
# when optional dependencies (imageio) are not installed.
# Import test functions directly from their respective modules.

__all__ = [
    'test_dative_case',
    'test_instrumental_case',
    'test_vocative_case',
    'test_genitive_case',
    'test_locative_case',
    'test_nominative_case',
    'test_accusative_case',
    'test_ablative_case'
]
 