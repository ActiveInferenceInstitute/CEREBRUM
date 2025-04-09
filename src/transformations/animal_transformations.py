"""
Animal-specific case transformations for the CEREBRUM framework.

This module provides specialized transformations for animal agents,
implementing the case-based model framework for navigating entities.
"""

import numpy as np
from typing import Dict, Any, List, Optional, Tuple, Union
import logging
from copy import deepcopy

from ..core.model import Model, Case
from ..examples.animal_agent import AnimalAgent
from .case_transformations import transform_case, revert_case

def transform_to_goal_seeker(animal: AnimalAgent) -> AnimalAgent:
    """
    Transform an animal agent into a goal-seeking configuration.
    
    This is a specialized transformation that puts the animal in NOMINATIVE case
    and optimizes parameters for goal seeking.
    
    Args:
        animal: The animal agent to transform
        
    Returns:
        The transformed animal (same instance)
    """
    # First apply the basic case transformation
    transform_case(animal, Case.NOMINATIVE)
    
    # Adjust parameters to optimize for goal seeking
    animal.parameters['max_rotation'] = np.pi/4  # Increase rotation ability
    animal.parameters['speed'] = 0.8  # Increase speed
    
    logging.info(f"Animal {animal.name} transformed to goal-seeker mode")
    
    return animal

def transform_to_explorer(animal: AnimalAgent) -> AnimalAgent:
    """
    Transform an animal agent into an exploration configuration.
    
    This transformation puts the animal in a hybrid NOMINATIVE/LOCATIVE case
    and optimizes parameters for environment exploration.
    
    Args:
        animal: The animal agent to transform
        
    Returns:
        The transformed animal (same instance)
    """
    # First apply the nominative case to enable movement
    transform_case(animal, Case.NOMINATIVE)
    
    # Store the original goal
    original_goal = animal.goal_position.copy() if hasattr(animal, 'goal_position') else None
    
    # Modify parameters for exploration
    animal.parameters['vision_range'] = animal.parameters.get('vision_range', 5.0) * 1.5
    animal.parameters['speed'] = animal.parameters.get('speed', 0.5) * 0.7
    animal._exploration_mode = True
    
    # Add random movement bias
    # This would be used in the animal's update logic if it checks for the presence of _exploration_mode
    animal._random_move_probability = 0.3
    
    logging.info(f"Animal {animal.name} transformed to explorer mode")
    
    return animal

def transform_to_follower(animal: AnimalAgent, target: AnimalAgent) -> AnimalAgent:
    """
    Transform an animal agent to follow another animal.
    
    This transformation puts the animal in DATIVE case and configures it
    to receive position updates from a target animal.
    
    Args:
        animal: The animal agent to transform
        target: The target animal to follow
        
    Returns:
        The transformed animal (same instance)
    """
    # Apply dative case to receive data
    transform_case(animal, Case.DATIVE)
    
    # Connect the animals
    animal._follow_target = target
    
    # Create a following connection
    animal.connections.append((target, "follows"))
    target.connections.append((animal, "is_followed_by"))
    
    logging.info(f"Animal {animal.name} now following {target.name}")
    
    return animal

def create_animal_formation(animals: List[AnimalAgent], formation_type: str = "v") -> Dict[str, Any]:
    """
    Create a formation of animals with specialized case assignments.
    
    Args:
        animals: List of animal agents
        formation_type: Type of formation ("v", "line", "circle")
        
    Returns:
        Dictionary with formation metadata
    """
    if not animals:
        return {"status": "failed", "reason": "No animals provided"}
    
    # Ensure we have enough animals
    min_animals = {"v": 3, "line": 2, "circle": 3}
    if len(animals) < min_animals.get(formation_type, 1):
        return {
            "status": "failed", 
            "reason": f"Need at least {min_animals.get(formation_type)} animals for {formation_type} formation"
        }
    
    # Create the formation based on type
    if formation_type == "v":
        # V formation: Leader in NOMINATIVE, wings in DATIVE following leader
        leader = animals[0]
        transform_case(leader, Case.NOMINATIVE)
        
        # Set up wing animals
        left_wing = animals[1:len(animals)//2 + 1]
        right_wing = animals[len(animals)//2 + 1:]
        
        # Transform wings to followers
        for animal in left_wing + right_wing:
            transform_to_follower(animal, leader)
            
        return {
            "status": "success",
            "formation": formation_type,
            "leader": leader,
            "left_wing": left_wing,
            "right_wing": right_wing
        }
        
    elif formation_type == "line":
        # Line formation: Leader in NOMINATIVE, others follow the one in front
        leader = animals[0]
        transform_case(leader, Case.NOMINATIVE)
        
        # Each animal follows the one before it
        for i in range(1, len(animals)):
            transform_to_follower(animals[i], animals[i-1])
            
        return {
            "status": "success",
            "formation": formation_type,
            "leader": leader,
            "followers": animals[1:]
        }
        
    elif formation_type == "circle":
        # Circle formation: All in DATIVE/NOMINATIVE hybrid, following each other
        for i, animal in enumerate(animals):
            # First transform to NOMINATIVE for movement
            transform_case(animal, Case.NOMINATIVE)
            
            # Each animal follows the next one in the circle
            next_animal = animals[(i + 1) % len(animals)]
            animal._circle_target = next_animal
            animal.connections.append((next_animal, "circles_with"))
        
        return {
            "status": "success",
            "formation": formation_type,
            "members": animals
        }
        
    else:
        return {"status": "failed", "reason": f"Unknown formation type: {formation_type}"}

def swap_animal_roles(animal1: AnimalAgent, animal2: AnimalAgent) -> Tuple[AnimalAgent, AnimalAgent]:
    """
    Swap the case roles between two animal agents.
    
    Args:
        animal1: First animal agent
        animal2: Second animal agent
        
    Returns:
        Tuple of (animal1, animal2) after the swap
    """
    # Save original cases
    case1 = animal1.case
    case2 = animal2.case
    
    # Save follow targets
    follow_target1 = getattr(animal1, '_follow_target', None)
    follow_target2 = getattr(animal2, '_follow_target', None)
    
    # Save connections
    connections1 = deepcopy(animal1.connections)
    connections2 = deepcopy(animal2.connections)
    
    # Clear existing connections
    animal1.connections = []
    animal2.connections = []
    
    # Transform each animal to the other's case
    transform_case(animal1, case2)
    transform_case(animal2, case1)
    
    # Swap follow targets - properly handle self-references
    if follow_target1 == animal2:
        animal1._follow_target = animal1  # Self-reference after swap
    else:
        animal1._follow_target = follow_target2
        
    if follow_target2 == animal1:
        animal2._follow_target = animal2  # Self-reference after swap
    else:
        animal2._follow_target = follow_target1
    
    # Swap and update connection relationships
    for conn, rel_type in connections1:
        if conn == animal2:
            # Special handling for connections between the two animals
            animal2.connections.append((animal2, "self_" + rel_type))
        else:
            # Regular connection to third party - now belongs to animal2
            animal2.connections.append((conn, rel_type))
            # Update the third party's connection
            for i, (target, target_rel) in enumerate(conn.connections):
                if target == animal1:
                    conn.connections[i] = (animal2, target_rel)
                    
    for conn, rel_type in connections2:
        if conn == animal1:
            # Special handling for connections between the two animals
            animal1.connections.append((animal1, "self_" + rel_type))
        else:
            # Regular connection to third party - now belongs to animal1
            animal1.connections.append((conn, rel_type))
            # Update the third party's connection
            for i, (target, target_rel) in enumerate(conn.connections):
                if target == animal2:
                    conn.connections[i] = (animal1, target_rel)
    
    # Add relevant relationship connections for the swap
    animal1.connections.append((animal2, "follows"))
    animal2.connections.append((animal1, "is_followed_by"))
    
    logging.info(f"Swapped roles between {animal1.name} and {animal2.name}")
    
    return animal1, animal2 