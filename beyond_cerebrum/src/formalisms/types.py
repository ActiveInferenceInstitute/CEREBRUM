"""
Core linguistic type definitions for the FORMICA framework.

Based on Section 2.2 of the specification, this module defines:
- Base types for fundamental linguistic units.
- Mechanisms for creating composite types.
- Potential integration points for dependent types.
"""

from typing import TypeVar, Generic, List, Dict, Any, NewType

# --- Base Linguistic Unit Types (Examples) ---
# Using NewType for nominal typing to distinguish related string/int concepts
Phoneme = NewType('Phoneme', str)
Morpheme = NewType('Morpheme', str)
LexicalItem = NewType('LexicalItem', Dict[str, Any]) # e.g., {'lemma': 'run', 'pos': 'VERB', ...}
SyntacticLabel = NewType('SyntacticLabel', str) # e.g., 'NP', 'VP', 'S'
SemanticConcept = NewType('SemanticConcept', str) # Placeholder, could be complex object/URI
PragmaticFeature = NewType('PragmaticFeature', str) # e.g., 'SpeakerIntention', 'ContextualSalience'
DiscourseRelation = NewType('DiscourseRelation', str) # e.g., 'Elaboration', 'Contrast'

# --- Type Variables for Generics ---
T = TypeVar('T')
ConstituentType = TypeVar('ConstituentType')
NodeType = TypeVar('NodeType')
EdgeType = TypeVar('EdgeType')

# --- Composite Type Placeholders (to be refined in structures.py) ---
# These are illustrative; concrete structure implementations are in structures.py
class LinguisticSequence(Generic[T]):
    """Generic sequence of linguistic units (e.g., Phoneme sequence)."""
    def __init__(self, elements: List[T]):
        self.elements = elements
    def __repr__(self): 
        return f"Sequence({self.elements})"

class LinguisticTree(Generic[ConstituentType]):
    """Generic tree structure (e.g., Syntax Tree)."""
    # Actual implementation in structures.py might use nodes, children etc.
    pass

class LinguisticGraph(Generic[NodeType, EdgeType]):
    """Generic graph structure (e.g., Semantic Network)."""
    # Actual implementation in structures.py might use nodes, edges, adjacency lists etc.
    pass

# --- Potential Dependent Type / Constraint Sketch ---
# This is highly conceptual and requires a more advanced type system 
# or runtime checks for full implementation in standard Python.

# Example: A Verb type whose 'arguments' structure depends on the verb's subcategory.
# class VerbLexicalItem(LexicalItem):
#     # Requires specific argument types based on 'subcat_frame'
#     subcat_frame: str 
#     arguments: Dict[str, Type] # How to enforce this dependency?

# --- Higher-Order Types (Functions) ---
# Example: A function type from Pragmatic Context to Semantic Interpretation
# InterpretationFunction = Callable[[PragmaticContext], SemanticConcept]

print("FORMICA core types module initialized.") 