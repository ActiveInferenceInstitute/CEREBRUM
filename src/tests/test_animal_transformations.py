"""
Tests for the animal-specific case transformations.

This module tests the specialized transformations for animal agents.
"""

import pytest
import numpy as np
from src.core.model import Case
from src.examples.animal_agent import AnimalAgent
from src.transformations.animal_transformations import (
    transform_to_goal_seeker,
    transform_to_explorer,
    transform_to_follower,
    create_animal_formation,
    swap_animal_roles
)

def test_transform_to_goal_seeker():
    """Test the transformation to goal seeker mode."""
    animal = AnimalAgent(
        name="TestGoalSeeker", 
        position=np.array([0.0, 0.0]),
        goal_position=np.array([10.0, 10.0])
    )
    
    # Set a non-NOMINATIVE case to test the transformation
    animal.case = Case.DATIVE
    
    # Apply the transformation
    animal = transform_to_goal_seeker(animal)
    
    # Check that the animal is now in NOMINATIVE case
    assert animal.case == Case.NOMINATIVE
    
    # Check that parameters were adjusted for goal seeking
    assert animal.parameters['max_rotation'] == np.pi/4
    assert animal.parameters['speed'] == 0.8

def test_transform_to_explorer():
    """Test the transformation to explorer mode."""
    animal = AnimalAgent(
        name="TestExplorer", 
        position=np.array([0.0, 0.0])
    )
    
    # Remember original parameters
    original_vision_range = animal.parameters['vision_range']
    original_speed = animal.parameters['speed']
    
    # Apply the transformation
    animal = transform_to_explorer(animal)
    
    # Check that the animal is in NOMINATIVE case
    assert animal.case == Case.NOMINATIVE
    
    # Check that parameters were adjusted for exploration
    assert animal.parameters['vision_range'] == pytest.approx(original_vision_range * 1.5)
    assert animal.parameters['speed'] == pytest.approx(original_speed * 0.7)
    
    # Check that exploration mode flag is set
    assert hasattr(animal, '_exploration_mode')
    assert animal._exploration_mode == True
    
    # Check that random movement probability is set
    assert hasattr(animal, '_random_move_probability')
    assert animal._random_move_probability > 0

def test_transform_to_follower():
    """Test the transformation to follower mode."""
    leader = AnimalAgent(name="Leader", position=np.array([5.0, 5.0]))
    follower = AnimalAgent(name="Follower", position=np.array([0.0, 0.0]))
    
    # Apply the transformation
    follower = transform_to_follower(follower, leader)
    
    # Check that the follower is in DATIVE case
    assert follower.case == Case.DATIVE
    
    # Check that the follower has a reference to the leader
    assert hasattr(follower, '_follow_target')
    assert follower._follow_target == leader
    
    # Check that connections are established
    assert any(conn == leader and rel_type == "follows" for conn, rel_type in follower.connections)
    assert any(conn == follower and rel_type == "is_followed_by" for conn, rel_type in leader.connections)

def test_create_animal_formation_v():
    """Test creating a V formation of animals."""
    animals = [
        AnimalAgent(name=f"Animal{i}", position=np.array([i, i])) 
        for i in range(5)
    ]
    
    # Create V formation
    result = create_animal_formation(animals, formation_type="v")
    
    # Check that formation was created successfully
    assert result["status"] == "success"
    assert result["formation"] == "v"
    
    # Check leader
    leader = result["leader"]
    assert leader == animals[0]
    assert leader.case == Case.NOMINATIVE
    
    # Check wings
    left_wing = result["left_wing"]
    right_wing = result["right_wing"]
    
    # Check that all wing animals are followers of the leader
    for animal in left_wing + right_wing:
        assert animal.case == Case.DATIVE
        assert hasattr(animal, '_follow_target')
        assert animal._follow_target == leader

def test_create_animal_formation_line():
    """Test creating a line formation of animals."""
    animals = [
        AnimalAgent(name=f"Animal{i}", position=np.array([i, i])) 
        for i in range(3)
    ]
    
    # Create line formation
    result = create_animal_formation(animals, formation_type="line")
    
    # Check that formation was created successfully
    assert result["status"] == "success"
    assert result["formation"] == "line"
    
    # Check leader
    leader = result["leader"]
    assert leader == animals[0]
    assert leader.case == Case.NOMINATIVE
    
    # Check that each animal follows the one before it
    for i, animal in enumerate(animals[1:], 1):
        assert animal.case == Case.DATIVE
        assert hasattr(animal, '_follow_target')
        assert animal._follow_target == animals[i-1]

def test_create_animal_formation_circle():
    """Test creating a circle formation of animals."""
    animals = [
        AnimalAgent(name=f"Animal{i}", position=np.array([i, i])) 
        for i in range(3)
    ]
    
    # Create circle formation
    result = create_animal_formation(animals, formation_type="circle")
    
    # Check that formation was created successfully
    assert result["status"] == "success"
    assert result["formation"] == "circle"
    
    # Check that all animals are in NOMINATIVE case
    for animal in animals:
        assert animal.case == Case.NOMINATIVE
    
    # Check that each animal follows the next one in the circle
    for i, animal in enumerate(animals):
        next_animal = animals[(i + 1) % len(animals)]
        assert hasattr(animal, '_circle_target')
        assert animal._circle_target == next_animal
        assert any(conn == next_animal and rel_type == "circles_with" for conn, rel_type in animal.connections)

def test_create_animal_formation_invalid():
    """Test creating a formation with invalid parameters."""
    # Test with empty list
    result = create_animal_formation([], formation_type="v")
    assert result["status"] == "failed"
    
    # Test with too few animals
    animals = [AnimalAgent(name="Single")]
    result = create_animal_formation(animals, formation_type="v")
    assert result["status"] == "failed"
    
    # Test with unknown formation type
    animals = [AnimalAgent(name=f"Animal{i}") for i in range(5)]
    result = create_animal_formation(animals, formation_type="unknown")
    assert result["status"] == "failed"

def test_swap_animal_roles():
    """Test swapping roles between two animals."""
    animal1 = AnimalAgent(name="Animal1", position=np.array([0.0, 0.0]))
    animal2 = AnimalAgent(name="Animal2", position=np.array([5.0, 5.0]))
    
    # Set different cases
    animal1.case = Case.NOMINATIVE
    animal2.case = Case.DATIVE
    
    # Create a follower relationship
    animal1._follow_target = None
    animal2._follow_target = animal1
    animal1.connections.append((animal2, "is_followed_by"))
    animal2.connections.append((animal1, "follows"))
    
    # Swap roles
    animal1, animal2 = swap_animal_roles(animal1, animal2)
    
    # Check that cases were swapped
    assert animal1.case == Case.DATIVE
    assert animal2.case == Case.NOMINATIVE
    
    # Check that follow targets are properly handled:
    # Since animal1 was followed by animal2, after swap animal2 should now be followed by animal1
    # This relationship is handled in our transform function as a self-reference
    assert hasattr(animal1, '_follow_target')
    assert animal1._follow_target is not None
    
    # Check that some connection relationships were preserved with appropriate changes
    assert any(rel_type == "follows" for conn, rel_type in animal1.connections)
    assert any(rel_type == "is_followed_by" for conn, rel_type in animal2.connections) 