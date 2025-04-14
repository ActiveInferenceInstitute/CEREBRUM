"""
POMDP test cases package for testing different grammatical cases.
"""

from src.tests.pomdp.test_cases.test_nominative import test_nominative_case
from src.tests.pomdp.test_cases.test_locative import test_locative_case
from src.tests.pomdp.test_cases.test_ablative import test_ablative_case
from src.tests.pomdp.test_cases.test_vocative import test_vocative_case

__all__ = [
    'test_nominative_case',
    'test_locative_case',
    'test_ablative_case',
    'test_vocative_case'
]
