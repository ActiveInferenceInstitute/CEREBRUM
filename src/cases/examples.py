"""
Examples of using the case system in the CEREBRUM framework.

This module provides example usage of the case system, showing how to
apply different cases and use the case management functionality.
"""

import numpy as np
import logging
from typing import Dict, Any, List

from ..core.model import Model, Case
from ..examples.animal_agent import AnimalAgent
from .case_manager import CaseManager
from .animal_cases import AnimalCaseManager
from .nominative import NominativeCase
from .accusative import AccusativeCase
from .dative import DativeCase

def basic_case_example():
    """
    Basic example of using the case system with a Model.
    """
    # Create a model
    model = Model(name="TestModel", parameters={"speed": 0.5, "size": 2.0})
    
    # Create a case manager
    manager = CaseManager()
    manager.register_model(model)
    
    # Transform the model to different cases
    print(f"Initial case: {model.case}")
    
    # Using the case manager
    manager.transform_case(model, Case.ACCUSATIVE)
    print(f"Case after transform: {model.case}")
    
    # Using direct case transformation
    model.case = Case.DATIVE
    print(f"Case after direct change: {model.case}")
    
    # Using a case handler
    NominativeCase.apply(model)
    print(f"Case after handler application: {model.case}")
    
    return model, manager

def animal_formation_example():
    """
    Example of creating animal formations with the case system.
    """
    # Create animal agents
    leader = AnimalAgent(name="Leader", position=np.array([0.0, 0.0]))
    follower1 = AnimalAgent(name="Follower1", position=np.array([1.0, -1.0]))
    follower2 = AnimalAgent(name="Follower2", position=np.array([-1.0, -1.0]))
    follower3 = AnimalAgent(name="Follower3", position=np.array([2.0, -2.0]))
    
    animals = [leader, follower1, follower2, follower3]
    
    # Create animal case manager
    animal_manager = AnimalCaseManager()
    for animal in animals:
        animal_manager.register_model(animal)
    
    # Create a V formation
    formation_result = animal_manager.create_v_formation(animals)
    print(f"Formation created: {formation_result['formation']}")
    print(f"Leader: {formation_result['leader'].name}")
    print(f"Left wing: {[a.name for a in formation_result['left_wing']]}")
    print(f"Right wing: {[a.name for a in formation_result['right_wing']]}")
    
    # Demonstrate role swapping
    print(f"\nBefore swap:")
    print(f"  {leader.name}: {leader.case}")
    print(f"  {follower1.name}: {follower1.case}")
    
    animal_manager.swap_animal_roles(leader, follower1)
    
    print(f"\nAfter swap:")
    print(f"  {leader.name}: {leader.case}")
    print(f"  {follower1.name}: {follower1.case}")
    
    return animals, animal_manager

def case_update_example():
    """
    Example of processing updates with different cases.
    """
    # Create a model in nominative case
    active_model = Model(name="ActiveModel")
    NominativeCase.apply(active_model)
    
    # Create a model in accusative case
    passive_model = Model(name="PassiveModel", parameters={"speed": 1.0, "weight": 10.0})
    AccusativeCase.apply(passive_model)
    
    # Create a model in dative case
    recipient_model = Model(name="RecipientModel")
    DativeCase.apply(recipient_model)
    
    # Process updates with different case handlers
    nominative_result = NominativeCase.process_update(active_model, {"action": "move"})
    print(f"Nominative update result: {nominative_result}")
    
    accusative_result = AccusativeCase.process_update(passive_model, {"speed": 2.0})
    print(f"Accusative update result: {accusative_result}")
    
    dative_result = DativeCase.process_update(recipient_model, {"goal": [5.0, 5.0]})
    print(f"Dative update result: {dative_result}")
    
    # Check parameter changes
    print(f"Passive model parameters after update: {passive_model.parameters}")
    
    return active_model, passive_model, recipient_model

def relationship_example():
    """
    Example of creating case relationships between models.
    """
    # Create models
    source = Model(name="SourceModel")
    target = Model(name="TargetModel")
    
    # Create case manager
    manager = CaseManager()
    manager.register_model(source)
    manager.register_model(target)
    
    # Create different types of relationships
    print("\nCreating 'generates' relationship:")
    manager.create_relationship(source, target, "generates")
    print(f"Source case: {source.case}")
    print(f"Target case: {target.case}")
    
    # Create another relationship
    print("\nCreating 'implements' relationship:")
    helper = Model(name="HelperModel")
    tool = Model(name="ToolModel")
    manager.register_model(helper)
    manager.register_model(tool)
    
    manager.create_relationship(helper, tool, "implements")
    print(f"Helper case: {helper.case}")
    print(f"Tool case: {tool.case}")
    
    # Get related models
    related = manager.get_related_models(source)
    print(f"\nModels related to {source.name}: {[(model.name, rel) for model, rel in related]}")
    
    return manager

if __name__ == "__main__":
    # Configure logging
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
    
    print("\n====== BASIC CASE EXAMPLE ======")
    model, manager = basic_case_example()
    
    print("\n====== ANIMAL FORMATION EXAMPLE ======")
    animals, animal_manager = animal_formation_example()
    
    print("\n====== CASE UPDATE EXAMPLE ======")
    active, passive, recipient = case_update_example()
    
    print("\n====== RELATIONSHIP EXAMPLE ======")
    relationship_manager = relationship_example() 