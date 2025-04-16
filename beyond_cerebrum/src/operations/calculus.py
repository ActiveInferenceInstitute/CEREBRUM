"""
Core calculus operations for manipulating linguistic structures in FORMICA.

This module defines the fundamental building blocks based on the formalisms
(e.g., type theory, category theory) chosen for FORMICA.
"""

from typing import TypeVar, Generic, Optional, Any, Callable

# Import structures (adjust path if needed)
from ..formalisms.structures import AbstractStructure, FeatureStructure, Tree, Graph
from ..formalisms.categories import Morphism # If using category theory

# Generic Type Variables
StructureType = TypeVar('StructureType', bound=AbstractStructure)
ArgType = TypeVar('ArgType')
ResultType = TypeVar('ResultType')

# --- Core Calculus Operations --- 

def unify(struct1: FeatureStructure, struct2: FeatureStructure) -> Optional[FeatureStructure]:
    """
    Unifies two feature structures.
    Leverages the unification method defined on the FeatureStructure class.

    Args:
        struct1: The first feature structure.
        struct2: The second feature structure.

    Returns:
        The unified feature structure, or None if unification fails.
    """
    # Basic implementation relies on the method in structures.py
    # More complex logic might be needed here depending on the FeatureStructure implementation
    return struct1.unify(struct2)

def project(structure: StructureType, feature_path: str) -> Any:
    """
    Projects a specific feature or value from a linguistic structure.
    The exact implementation depends heavily on the structure type.

    Args:
        structure: The linguistic structure (e.g., Tree, Graph, FeatureStructure).
        feature_path: A path or query specifying the feature to extract 
                      (e.g., 'head.pos', 'nodes[\'concept1\'].type').

    Returns:
        The extracted value or feature.

    Raises:
        KeyError: If the feature path is invalid for the structure.
        NotImplementedError: If projection is not defined for the structure type.
    """
    if isinstance(structure, FeatureStructure):
        # Basic dictionary-like access, assumes simple paths
        try:
            # Rudimentary path handling - needs a proper parser for complex paths
            keys = feature_path.split('.')
            value = structure
            for key in keys:
                value = value[key]
            return value
        except (KeyError, TypeError) as e:
            raise KeyError(f"Invalid feature path '{feature_path}' for FeatureStructure: {e}")
    elif isinstance(structure, Tree):
        # Basic projection for Trees (e.g., get root content)
        # Needs a proper path language (e.g., XPath-like)
        if feature_path == 'root.content':
            return structure.root.content
        # Add more basic cases or raise error
        raise NotImplementedError(f"Basic Tree projection only supports 'root.content', not '{feature_path}'")
    elif isinstance(structure, Graph):
        # Basic projection for Graphs (e.g., get node content by id)
        # Needs a proper path/query language
        if feature_path.startswith('nodes[') and feature_path.endswith('].content'):
            node_id = None # Initialize in case of exception before assignment
            try:
                # Extract the ID, handling potential surrounding quotes
                node_id_str = feature_path[len('nodes['):-len('].content')]
                node_id = node_id_str.strip(''"') # Strip both single and double quotes
                return structure.nodes[node_id].content
            except KeyError:
                 # Reraise with a more informative message including the extracted ID if available
                 if node_id:
                     raise KeyError(f"Node ID '{node_id}' not found in graph for path '{feature_path}'")
                 else:
                     raise KeyError(f"Could not extract valid Node ID from path '{feature_path}'")
        # Clarify the supported format in the error message
        raise NotImplementedError(f"Graph projection supports 'nodes["id"].content' format, not '{feature_path}'")
    elif hasattr(structure, 'project'): # Check for a custom method
        # Allow structures to define their own projection method
        return structure.project(feature_path)
    else:
        raise NotImplementedError(f"Projection not implemented for type {type(structure).__name__}")

def compose(morphism1: Morphism, morphism2: Morphism) -> Morphism:
    """
    Composes two morphisms (if using Category Theory).
    NOTE: Category Theory is currently conceptual in FORMICA.

    Args:
        morphism1: The first morphism (applied second).
        morphism2: The second morphism (applied first).

    Returns:
        The composed morphism.

    Raises:
        ValueError: If the morphisms are not compatible for composition.
        NotImplementedError: This operation is not implemented as CT is conceptual.
    """
    # As Category Theory is conceptual, we don't provide a concrete implementation.
    # A real implementation would require concrete Morphism classes and categories.
    raise NotImplementedError("Category theory composition logic not implemented (conceptual).")

def apply(func: Callable[[ArgType], ResultType], argument: ArgType) -> ResultType:
    """
    Applies a function (potentially representing a linguistic rule or transformation)
    to an argument (a linguistic structure or value).

    Args:
        func: The function to apply.
        argument: The argument to the function.

    Returns:
        The result of applying the function.
    """
    # Simple application, but could be extended with type checking, context passing, etc.
    return func(argument)

# TODO: Define more sophisticated calculus operations.
# TODO: Implement robust error handling.
# TODO: Integrate with the type system for static checks where possible.
# TODO: Consider operations specific to probabilistic reasoning. 