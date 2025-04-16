"""
FORMICA Formalisms Package.

Contains modules defining the core data structures, type systems,
and theoretical bases (e.g., category theory) for linguistic representation.
"""

from .types import (
    Phoneme,
    Morpheme,
    LexicalItem,
    SyntacticLabel,
    SemanticConcept,
    PragmaticFeature,
    DiscourseRelation,
    LinguisticSequence,
)
from .structures import (
    TreeNode,
    Tree,
    GraphNode,
    GraphEdge,
    Graph,
    FeatureStructure,
)

__all__ = [
    # Types
    'Phoneme',
    'Morpheme',
    'LexicalItem',
    'SyntacticLabel',
    'SemanticConcept',
    'PragmaticFeature',
    'DiscourseRelation',
    'LinguisticSequence',
    # Structures
    'TreeNode',
    'Tree',
    'GraphNode',
    'GraphEdge',
    'Graph',
    'FeatureStructure',
] 

# This file makes the directory a Python package 