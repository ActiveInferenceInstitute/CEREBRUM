"""
CEREBRUM Linear Regression Case Tests Package
Contains test functions for each linguistic case in the linear regression context
"""

from .dative_case import test_dative_case
from .instrumental_case import test_instrumental_case
from .vocative_case import test_vocative_case
from .genitive_case import test_genitive_case
from .locative_case import test_locative_case
from .nominative_case import test_nominative_case
from .accusative_case import test_accusative_case
from .ablative_case import test_ablative_case

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