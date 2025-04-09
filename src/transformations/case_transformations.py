"""
Case transformation utilities for CEREBRUM framework.

This module provides functions for transforming models between different cases,
as well as utilities for managing case relationships between multiple models.
"""

from typing import Dict, Any, List, Optional, Tuple, Union
import numpy as np

from src.core.model import Model, Case

def transform_case(model: Model, target_case: Case) -> Model:
    """
    Transform a model to a target case.
    
    Args:
        model: The model to transform
        target_case: The target case to transform to
        
    Returns:
        The transformed model (same instance, new case)
    """
    # Store original case for potential reversion
    original_case = model.case
    
    # Apply the transformation by setting the case property
    # This will trigger the model's _apply_case_transformation method
    model.case = target_case
    
    return model

def revert_case(model: Model) -> Model:
    """
    Revert a model to its previous case.
    
    Args:
        model: The model to revert
        
    Returns:
        The reverted model (same instance, previous case)
    """
    if model._prior_case is not None:
        model.case = model._prior_case
    return model

def apply_morphosyntactic_alignment(
    models: List[Model], 
    alignment_type: str = "nominative_accusative",
    subject: Optional[Model] = None,
    object: Optional[Model] = None,
    action: Optional[str] = None
) -> Dict[str, List[Model]]:
    """
    Apply a morphosyntactic alignment pattern to a group of models.
    
    Args:
        models: List of models to align
        alignment_type: The type of alignment to apply
        subject: The model acting as subject (if known)
        object: The model acting as object (if known)
        action: The action being performed
        
    Returns:
        Dictionary mapping case roles to lists of models
    """
    result = {case.name: [] for case in Case}
    
    if alignment_type == "nominative_accusative":
        # In nominative-accusative alignment, subjects of both transitive and
        # intransitive verbs are treated the same (nominative case)
        if subject:
            transform_case(subject, Case.NOMINATIVE)
            result["NOMINATIVE"].append(subject)
        
        if object:
            transform_case(object, Case.ACCUSATIVE)
            result["ACCUSATIVE"].append(object)
        
        # Assign remaining models based on heuristics
        for model in models:
            if model == subject or model == object:
                continue
            
            # Use simple heuristic: models with higher free energy are more likely
            # to be objects (receiving updates)
            if hasattr(model, "free_energy") and callable(getattr(model, "free_energy")):
                try:
                    fe = model.free_energy()
                    if fe > 0:  # High free energy suggests it needs updates (Object-like)
                        transform_case(model, Case.ACCUSATIVE)
                        result["ACCUSATIVE"].append(model)
                    else: # Low or zero free energy suggests it's stable or acting (Subject-like)
                        transform_case(model, Case.NOMINATIVE)
                        result["NOMINATIVE"].append(model)
                except Exception as e: # Catch potential errors during free_energy calculation
                    # Default to instrumental if free energy calculation fails
                    # Optionally log the error: logging.warning(f"Failed to get free energy for {model.name}: {e}")
                    transform_case(model, Case.INSTRUMENTAL)
                    result["INSTRUMENTAL"].append(model)
            else:
                # Default to instrumental case for models without a valid free_energy method
                transform_case(model, Case.INSTRUMENTAL)
                result["INSTRUMENTAL"].append(model)
    
    elif alignment_type == "ergative_absolutive":
        # In ergative-absolutive alignment, subjects of transitive verbs (ergative)
        # are treated differently than subjects of intransitive verbs and objects (absolutive)
        
        # Determine if action is transitive (has an object)
        is_transitive = object is not None
        
        if subject:
            if is_transitive:
                # Subject of transitive verb gets INSTRUMENTAL case (closest to ergative)
                transform_case(subject, Case.INSTRUMENTAL)
                result["INSTRUMENTAL"].append(subject)
            else:
                # Subject of intransitive verb gets NOMINATIVE case (closest to absolutive)
                transform_case(subject, Case.NOMINATIVE)
                result["NOMINATIVE"].append(subject)
        
        if object:
            # Object gets ACCUSATIVE case (closest to absolutive)
            transform_case(object, Case.ACCUSATIVE)
            result["ACCUSATIVE"].append(object)
        
        # Assign remaining models
        for model in models:
            if model == subject or model == object:
                continue
            
            # Default to LOCATIVE for context models in ergative-absolutive alignment
            transform_case(model, Case.LOCATIVE)
            result["LOCATIVE"].append(model)
    
    elif alignment_type == "tripartite":
        # In tripartite alignment, subjects of transitive verbs, subjects of intransitive verbs,
        # and objects all get distinct cases
        
        # Determine if action is transitive (has an object)
        is_transitive = object is not None
        
        if subject:
            if is_transitive:
                # Subject of transitive verb gets NOMINATIVE
                transform_case(subject, Case.NOMINATIVE)
                result["NOMINATIVE"].append(subject)
            else:
                # Subject of intransitive verb gets ABLATIVE
                transform_case(subject, Case.ABLATIVE)
                result["ABLATIVE"].append(subject)
        
        if object:
            # Object gets ACCUSATIVE
            transform_case(object, Case.ACCUSATIVE)
            result["ACCUSATIVE"].append(object)
        
        # Assign remaining models
        for model in models:
            if model == subject or model == object:
                continue
            
            # Use dative for recipients
            transform_case(model, Case.DATIVE)
            result["DATIVE"].append(model)
    
    else:
        # Default to setting all models to NOMINATIVE
        for model in models:
            transform_case(model, Case.NOMINATIVE)
            result["NOMINATIVE"].append(model)
    
    return result

def create_case_relationship(
    source_model: Model,
    target_model: Model,
    relationship_type: str
) -> Tuple[Model, Model]:
    """
    Create a case relationship between two models.
    
    Args:
        source_model: The source model of the relationship
        target_model: The target model of the relationship
        relationship_type: The type of relationship to create
        
    Returns:
        The transformed models as a tuple (source_model, target_model)
    """
    # Map relationship types to appropriate case transformations
    if relationship_type == "generates":
        # Source generates predictions/products for target
        transform_case(source_model, Case.NOMINATIVE)
        transform_case(target_model, Case.DATIVE)
    
    elif relationship_type == "updates":
        # Source updates parameters of target
        transform_case(source_model, Case.NOMINATIVE)
        transform_case(target_model, Case.ACCUSATIVE)
    
    elif relationship_type == "receives_from":
        # Source receives data from target
        transform_case(source_model, Case.DATIVE)
        transform_case(target_model, Case.GENITIVE)
    
    elif relationship_type == "implements":
        # Source implements target as a method
        transform_case(source_model, Case.INSTRUMENTAL)
        transform_case(target_model, Case.ACCUSATIVE)
    
    elif relationship_type == "contextualizes":
        # Source provides context for target
        transform_case(source_model, Case.LOCATIVE)
        transform_case(target_model, Case.NOMINATIVE)
    
    elif relationship_type == "derives_from":
        # Source derives from target as origin
        transform_case(source_model, Case.NOMINATIVE)
        transform_case(target_model, Case.ABLATIVE)
    
    elif relationship_type == "addresses":
        # Source addresses target directly
        transform_case(source_model, Case.NOMINATIVE)
        transform_case(target_model, Case.VOCATIVE)
    
    else:
        # Default transformation
        transform_case(source_model, Case.NOMINATIVE)
        transform_case(target_model, Case.ACCUSATIVE)
    
    # Record the relationship in both models
    source_model.connect(target_model, relationship_type)
    target_model.connect(source_model, f"inverse_{relationship_type}")
    
    return source_model, target_model

def convert_message_between_cases(
    message: Dict[str, Any],
    source_case: Case,
    target_case: Case
) -> Dict[str, Any]:
    """
    Convert a message between case-specific formats.
    
    Args:
        message: The message to convert
        source_case: The original case of the message
        target_case: The target case for the message
        
    Returns:
        The converted message
    """
    converted_message = message.copy()
    
    # Add basic conversion metadata
    converted_message["converted_from"] = source_case.name
    converted_message["converted_to"] = target_case.name
    
    # Apply case-specific conversions
    if source_case == Case.NOMINATIVE and target_case == Case.ACCUSATIVE:
        # Convert predictions to update targets
        if "predictions" in message:
            converted_message["target_values"] = message["predictions"]
            converted_message.pop("predictions", None)
    
    elif source_case == Case.GENITIVE and target_case == Case.DATIVE:
        # Convert products to received data
        if "products" in message:
            converted_message["received_data"] = message["products"]
            converted_message.pop("products", None)
    
    elif source_case == Case.INSTRUMENTAL and target_case == Case.NOMINATIVE:
        # Convert method results to predictions
        if "method" in message and message["method"] == "predict":
            if "predictions" in message:
                converted_message["predictions"] = message["predictions"]
                converted_message.pop("method", None)
    
    elif source_case == Case.LOCATIVE and target_case == Case.NOMINATIVE:
        # Convert context to parameters
        if "context" in message:
            converted_message["parameters"] = message["context"]
            converted_message.pop("context", None)
    
    # Update the case field
    converted_message["case"] = target_case.name
    
    return converted_message 