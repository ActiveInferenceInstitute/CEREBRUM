"""
Animal-specific case implementations for CEREBRUM framework.

This module provides specialized case handlers for animal agents,
extending the general case system with animal-specific behaviors.
"""

from typing import Dict, Any, List, Optional, Tuple, Union
import numpy as np
import logging

from ..core.model import Model, Case
from ..examples.animal_agent import AnimalAgent
from .case_manager import CaseManager

class AnimalCaseManager(CaseManager):
    """
    Case manager specialized for animal agents.
    
    This manager extends the base CaseManager with animal-specific
    transformations and case behaviors.
    """
    
    def __init__(self):
        """Initialize the animal case manager."""
        super().__init__()
        # Add specialized animal formations
        self.formations = {
            "v_formation": self.create_v_formation,
            "line_formation": self.create_line_formation,
            "circle_formation": self.create_circle_formation,
        }
    
    def transform_to_goal_seeker(self, animal: AnimalAgent) -> AnimalAgent:
        """
        Transform an animal agent into a goal-seeking configuration.
        
        Args:
            animal: The animal agent to transform
            
        Returns:
            The transformed animal
        """
        # Apply nominative case for active goal seeking
        self.transform_case(animal, Case.NOMINATIVE)
        
        # Adjust parameters for goal seeking
        animal.parameters['max_rotation'] = np.pi/4  # Increase rotation ability
        animal.parameters['speed'] = 0.8  # Increase speed
        
        logging.info(f"Animal {animal.name} transformed to goal-seeker mode")
        return animal
    
    def transform_to_explorer(self, animal: AnimalAgent) -> AnimalAgent:
        """
        Transform an animal agent into an exploration configuration.
        
        Args:
            animal: The animal agent to transform
            
        Returns:
            The transformed animal
        """
        # Apply nominative case with exploration behavior
        self.transform_case(animal, Case.NOMINATIVE)
        
        # Store the original goal if there is one
        if hasattr(animal, 'goal_position'):
            animal._original_goal = animal.goal_position.copy()
        
        # Modify parameters for exploration
        animal.parameters['vision_range'] = animal.parameters.get('vision_range', 5.0) * 1.5
        animal.parameters['speed'] = animal.parameters.get('speed', 0.5) * 0.7
        animal._exploration_mode = True
        animal._random_move_probability = 0.3
        
        logging.info(f"Animal {animal.name} transformed to explorer mode")
        return animal
    
    def transform_to_follower(self, follower: AnimalAgent, leader: AnimalAgent) -> AnimalAgent:
        """
        Transform an animal to follow another animal.
        
        Args:
            follower: The animal agent to transform into a follower
            leader: The leader animal to follow
            
        Returns:
            The transformed follower animal
        """
        # Create a "receives_from" relationship from follower to leader
        self.create_relationship(follower, leader, "receives_from")
        
        # Store the leader as the follow target
        follower._follow_target = leader
        
        logging.info(f"Animal {follower.name} now following {leader.name}")
        return follower
    
    def create_v_formation(self, animals: List[AnimalAgent]) -> Dict[str, Any]:
        """
        Create a V formation of animals.
        
        Args:
            animals: List of animal agents
            
        Returns:
            Formation metadata
        """
        if len(animals) < 3:
            return {"status": "failed", "reason": "Need at least 3 animals for V formation"}
        
        # Assign the leader
        leader = animals[0]
        self.transform_to_goal_seeker(leader)
        
        # Set up wing animals
        left_wing = animals[1:len(animals)//2 + 1]
        right_wing = animals[len(animals)//2 + 1:]
        
        # Transform wings to followers
        for animal in left_wing + right_wing:
            self.transform_to_follower(animal, leader)
            
        # Set formation-specific parameters
        # Left wing positions slightly to the left and behind leader
        for i, animal in enumerate(left_wing):
            animal._formation_position = (-1.0 - i * 0.5, -1.0 - i * 0.5)  # (left, back)
            animal._formation = "v_left"
            
        # Right wing positions slightly to the right and behind leader
        for i, animal in enumerate(right_wing):
            animal._formation_position = (1.0 + i * 0.5, -1.0 - i * 0.5)  # (right, back)
            animal._formation = "v_right"
            
        return {
            "status": "success",
            "formation": "v_formation",
            "leader": leader,
            "left_wing": left_wing,
            "right_wing": right_wing
        }
    
    def create_line_formation(self, animals: List[AnimalAgent]) -> Dict[str, Any]:
        """
        Create a line formation of animals.
        
        Args:
            animals: List of animal agents
            
        Returns:
            Formation metadata
        """
        if len(animals) < 2:
            return {"status": "failed", "reason": "Need at least 2 animals for line formation"}
        
        # Assign the leader
        leader = animals[0]
        self.transform_to_goal_seeker(leader)
        
        # Each animal follows the one before it
        for i in range(1, len(animals)):
            follower = animals[i]
            self.transform_to_follower(follower, animals[i-1])
            follower._formation = "line"
            follower._formation_position = (0.0, -1.0 * i)  # Directly behind the previous animal
            
        return {
            "status": "success",
            "formation": "line_formation",
            "leader": leader,
            "followers": animals[1:]
        }
    
    def create_circle_formation(self, animals: List[AnimalAgent]) -> Dict[str, Any]:
        """
        Create a circle formation of animals.
        
        Args:
            animals: List of animal agents
            
        Returns:
            Formation metadata
        """
        if len(animals) < 3:
            return {"status": "failed", "reason": "Need at least 3 animals for circle formation"}
        
        # All animals in NOMINATIVE/DATIVE hybrid setup
        for i, animal in enumerate(animals):
            # First transform to NOMINATIVE for movement capability
            self.transform_case(animal, Case.NOMINATIVE)
            
            # Each animal watches the next one in the circle
            next_animal = animals[(i + 1) % len(animals)]
            animal._circle_target = next_animal
            
            # Create relationship
            self.create_relationship(animal, next_animal, "receives_from")
            
            # Set formation attributes
            animal._formation = "circle"
            
            # Calculate position in circle - equally spaced
            angle = 2 * np.pi * i / len(animals)
            radius = 2.0  # Arbitrary circle radius
            animal._formation_position = (radius * np.cos(angle), radius * np.sin(angle))
        
        return {
            "status": "success",
            "formation": "circle_formation",
            "members": animals
        }
    
    def swap_animal_roles(self, animal1: AnimalAgent, animal2: AnimalAgent) -> Tuple[AnimalAgent, AnimalAgent]:
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
        
        # Save follow targets if they exist
        follow_target1 = getattr(animal1, '_follow_target', None)
        follow_target2 = getattr(animal2, '_follow_target', None)
        
        # Save formation data
        formation1 = getattr(animal1, '_formation', None)
        formation2 = getattr(animal2, '_formation', None)
        formation_pos1 = getattr(animal1, '_formation_position', None)
        formation_pos2 = getattr(animal2, '_formation_position', None)
        
        # Transform each animal to the other's case
        self.transform_case(animal1, case2)
        self.transform_case(animal2, case1)
        
        # Swap follow targets, being careful with self-references
        if follow_target1 == animal2:
            animal1._follow_target = animal1  # Self-reference after swap
        else:
            animal1._follow_target = follow_target2
            
        if follow_target2 == animal1:
            animal2._follow_target = animal2  # Self-reference after swap
        else:
            animal2._follow_target = follow_target1
        
        # Swap formation data
        animal1._formation = formation2
        animal2._formation = formation1
        animal1._formation_position = formation_pos2
        animal2._formation_position = formation_pos1
        
        # Update relationships in the manager
        self._swap_relationships(animal1, animal2)
        
        logging.info(f"Swapped roles between {animal1.name} and {animal2.name}")
        return animal1, animal2
    
    def _swap_relationships(self, model1: Model, model2: Model) -> None:
        """
        Swap relationships between two models in the relationship registry.
        
        Args:
            model1: First model
            model2: Second model
        """
        # Create a new relationship list
        new_relationships = []
        
        for src, tgt, rel in self.case_relationships:
            if src.id == model1.id and tgt.id == model2.id:
                # Direct relationship between model1->model2 becomes model2->model1
                new_relationships.append((model2, model1, rel))
            elif src.id == model2.id and tgt.id == model1.id:
                # Direct relationship between model2->model1 becomes model1->model2
                new_relationships.append((model1, model2, rel))
            elif src.id == model1.id:
                # model1->other becomes model2->other
                new_relationships.append((model2, tgt, rel))
            elif tgt.id == model1.id:
                # other->model1 becomes other->model2
                new_relationships.append((src, model2, rel))
            elif src.id == model2.id:
                # model2->other becomes model1->other
                new_relationships.append((model1, tgt, rel))
            elif tgt.id == model2.id:
                # other->model2 becomes other->model1
                new_relationships.append((src, model1, rel))
            else:
                # Relationship doesn't involve either model, keep it unchanged
                new_relationships.append((src, tgt, rel))
        
        self.case_relationships = new_relationships 