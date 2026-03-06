"""
LEXICON Case Declension System

Components for tagging text with CEREBRUM's 8-case system.
"""

from .tagger import CaseTagger  # noqa: F401
from .rules import CaseRules  # noqa: F401
from .structured_case_determiner import StructuredCaseDeterminer, CaseAssignment  # noqa: F401
