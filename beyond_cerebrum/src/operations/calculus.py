"""
Core calculus functions for manipulating linguistic structures in FORMICA.

This module implements primitive operations as defined in Section 2.3 
of the specification, serving as the building blocks for more complex
linguistic transformations and inferences.
"""

from typing import TypeVar, Generic, Callable, Any, Optional, Dict, List
# Import necessary structures and types (adjust paths as needed)
# TODO: Define and import a proper FeatureStructure class
# from ..formalisms.structures import FeatureStructure 
from ..formalisms.structures import FeatureStructure, Graph, Tree 
from ..formalisms.types import LinguisticSequence

# Type variables for generic operations
S = TypeVar('S') # Represents a generic structure type
F = TypeVar('F') # Represents a feature or element type
K = TypeVar('K') # Key type for dict-like structures
V = TypeVar('V') # Value type for dict-like structures

# --- Primitive Operations --- 

def unify(struct1: Any, struct2: Any) -> Optional[Any]:
    """Attempts to unify two structures.

    Prioritizes FeatureStructure.unify if both inputs are FeatureStructures.
    Fails unification if mixing FeatureStructure and plain dict.
    Handles recursive unification for dictionaries and checks for equality
    for other primitive types. Returns the unified structure or None if
    unification fails.
    
    NOTE: This is a basic implementation for dictionaries. A full 
    FeatureStructure implementation would handle types, cycles, and more 
    complex unification rules.
    """
    # print(f"Attempting to unify {struct1!r} and {struct2!r}")

    # Prioritize FeatureStructure unification if both are FeatureStructures
    if isinstance(struct1, FeatureStructure) and isinstance(struct2, FeatureStructure):
        # Use the dedicated unify method of FeatureStructure
        return struct1.unify(struct2)

    # Check for direct type mismatch (FS vs dict) - should fail
    elif (isinstance(struct1, FeatureStructure) and isinstance(struct2, dict) and not isinstance(struct2, FeatureStructure)) or \
         (isinstance(struct1, dict) and not isinstance(struct1, FeatureStructure) and isinstance(struct2, FeatureStructure)):
        # print(f"Unification failed: Type mismatch FeatureStructure vs dict")
        return None

    # Handle dictionary unification (only if both are plain dicts or compatible subtypes handled above)
    elif isinstance(struct1, dict) and isinstance(struct2, dict):
        # Basic recursive dict unification
        unified: Dict[Any, Any] = struct1.copy()
        for key, value2 in struct2.items():
            if key not in unified:
                unified[key] = value2
            else:
                value1 = unified[key]
                # Recursive call using this top-level unify
                unified_value = unify(value1, value2)
                if unified_value is None:
                    # print(f"Unification failed: Conflict at key '{key}': {value1!r} vs {value2!r}")
                    return None
                else:
                    unified[key] = unified_value
        return unified

    # Handle non-dict, non-FeatureStructure cases (e.g., primitives, lists)
    else:
        if struct1 == struct2:
            return struct1 # Unification succeeds if they are identical
        else:
            # print(f"Unification failed: Mismatch {struct1!r} ({type(struct1)}) vs {struct2!r} ({type(struct2)})")
            return None # Conflict if they are different

def project(structure: S, feature_path: str | List[str] | F) -> Any:
    """Projects or extracts a component from a structure using a path or key.

    Handles simple dictionary key access and basic attribute access.
    Accepts a single key/attribute name or a list representing a path.

    Examples:
        project({'a': {'b': 1}}, ['a', 'b']) -> 1
        project(my_object, 'attribute_name') -> value
        project({'key': 10}, 'key') -> 10
    """
    # print(f"Projecting path {feature_path} from structure {structure}")

    current_value = structure
    
    if isinstance(feature_path, list):
        path_elements = feature_path
    elif isinstance(feature_path, str):
        # Basic assumption: dot notation for attributes, otherwise direct key/attr
        # A more robust implementation would need a proper path parser
        if '.' in feature_path:
             path_elements = feature_path.split('.')
        else:
             path_elements = [feature_path]
    else: # Assume single feature key/attribute
        path_elements = [feature_path]
        
    try:
        for element in path_elements:
            if isinstance(current_value, dict):
                current_value = current_value[element]
            elif hasattr(current_value, str(element)):
                current_value = getattr(current_value, str(element))
            else:
                raise KeyError(f"Cannot access element '{element}' in structure {current_value}")
        return current_value
    except (KeyError, AttributeError, TypeError) as e:
        # print(f"Projection failed: {e}")
        raise LookupError(f"Failed to project path {feature_path} from structure. Reason: {e}") from e
    
def compose(morphism1: Callable, morphism2: Callable) -> Callable:
    """Composes two linguistic operations (morphisms).

    Represents the sequential application of transformations or inferences.
    Assumes morphisms are functions.
    """
    print(f"Composing {morphism1.__name__} and {morphism2.__name__}")
    # Standard function composition
    def composed_morphism(*args, **kwargs):
        # Apply morphism2 first, then morphism1 to the result
        intermediate_result = morphism2(*args, **kwargs)
        return morphism1(intermediate_result)
    
    # Try to give the composed function a meaningful name
    composed_morphism.__name__ = f"{morphism1.__name__}_o_{morphism2.__name__}"
    return composed_morphism

def apply(func: Callable[[Any], Any], argument: Any) -> Any:
    """Applies a linguistic function (operation) to an argument (structure).

    This is essentially function application, but made explicit as a 
    core calculus operation.
    """
    print(f"Applying function {func.__name__} to argument {argument}")
    return func(argument)

# --- Potential Additional Primitives ---

# def merge(struct1: S, struct2: S) -> S:
#     """Merges two structures (potentially less strict than unification)."""
#     raise NotImplementedError

# def match(pattern: P, structure: S) -> bool:
#     """Checks if a structure matches a given pattern."""
#     raise NotImplementedError


print("FORMICA core calculus module initialized.") 