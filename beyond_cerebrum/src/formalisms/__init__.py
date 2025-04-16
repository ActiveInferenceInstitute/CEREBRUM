"""
FORMICA Formalisms Package.

Contains modules defining the core data structures, type systems,
and theoretical bases (e.g., category theory) for linguistic representation.
"""

from .types import (
    Phoneme,
    Morpheme,
    LexicalItem,
    SyntacticConstituent,
    SemanticConcept,
    PragmaticContext,
    DiscourseUnit,
    LinguisticStructure,
    SyntacticTree as SyntacticTreeType, # Avoid name clash with structure class
    SemanticGraph as SemanticGraphType, # Avoid name clash with structure class
    Verb
)
from .structures import (
    AbstractStructure,
    TreeNode,
    Tree,
    SyntacticTree,
    GraphNode,
    GraphEdge,
    Graph,
    SemanticGraph,
    FeatureStructure,
)
from .categories import (
    Morphism,
    Category,
    Functor
) 

__all__ = [
    # Types
    'Phoneme',
    'Morpheme',
    'LexicalItem',
    'SyntacticConstituent',
    'SemanticConcept',
    'PragmaticContext',
    'DiscourseUnit',
    'LinguisticStructure',
    'SyntacticTreeType',
    'SemanticGraphType',
    'Verb',
    # Structures
    'AbstractStructure',
    'TreeNode',
    'Tree',
    'SyntacticTree',
    'GraphNode',
    'GraphEdge',
    'Graph',
    'SemanticGraph',
    'FeatureStructure',
    # Categories (Conceptual)
    'Morphism',
    'Category',
    'Functor',
] 