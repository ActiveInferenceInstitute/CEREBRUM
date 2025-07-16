"""
LEXICON Case Declension System

Components for tagging text with CEREBRUM's 8-case system.
"""

from .tagger import CaseTagger
from .rules import CaseRules
from .structured_case_determiner import StructuredCaseDeterminer, CaseAssignment 