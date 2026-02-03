"""
CEREBRUM Transformations Module

This module provides case transformation utilities for models in the CEREBRUM framework.

Core Functions:
    - transform_case: Transform a model to a target case
    - revert_case: Revert a model to its previous case
    - apply_morphosyntactic_alignment: Apply linguistic alignment patterns
    - create_case_relationship: Create relationships between models
    - convert_message_between_cases: Convert messages between case formats

Animal-Specific Functions:
    - transform_to_goal_seeker: Optimize animal for goal seeking
    - transform_to_explorer: Optimize animal for exploration
    - transform_to_follower: Configure animal to follow another
    - create_animal_formation: Create coordinated animal formations
    - swap_animal_roles: Swap case roles between animals
"""

from .case_transformations import (
    transform_case,
    revert_case,
    apply_morphosyntactic_alignment,
    create_case_relationship,
    convert_message_between_cases,
)

from .animal_transformations import (
    transform_to_goal_seeker,
    transform_to_explorer,
    transform_to_follower,
    create_animal_formation,
    swap_animal_roles,
)

__all__ = [
    # Core transformations
    'transform_case',
    'revert_case',
    'apply_morphosyntactic_alignment',
    'create_case_relationship',
    'convert_message_between_cases',
    # Animal transformations
    'transform_to_goal_seeker',
    'transform_to_explorer',
    'transform_to_follower',
    'create_animal_formation',
    'swap_animal_roles',
]
