"""
LEXICON - Linguistic Entity eXtraction and Iterative Case-Oriented Navigation

A high-performance analysis engine that transforms unstructured language 
artifacts into tightly-typed, case-declined knowledge graphs.
"""

from .core.engine import LexiconEngine
from .core.config import LexiconConfig
from .core.exceptions import LexiconError
from .declension.tagger import CaseTagger
from .graph.assembler import GraphAssembler

__version__ = "0.1.0"
__author__ = "CEREBRUM Team"
__license__ = "MIT" 