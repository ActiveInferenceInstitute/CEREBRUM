"""
Case Manager for CEREBRUM framework.

This module provides a centralized manager for coordinating case transformations
and case-specific behavior across models.
"""

from typing import Dict, Any, List, Optional, Tuple, Union, Set
import logging

from ..core.model import Model, Case
from .nominative import NominativeCase
from .accusative import AccusativeCase

class CaseManager:
    """
    Manager for case transformations and case-specific operations.
    
    This class provides a centralized way to handle case transformations,
    track case relationships, and apply consistent case-based reasoning
    to multiple models.
    """
    
    def __init__(self):
        """Initialize the case manager."""
        self.tracked_models: Dict[str, Model] = {}
        self.case_relationships: List[Tuple[Model, Model, str]] = []
        self.case_handlers = {
            Case.NOMINATIVE: NominativeCase,
            Case.ACCUSATIVE: AccusativeCase,
            # Add other cases as they're implemented
        }
        self.default_case = Case.NOMINATIVE
    
    def register_model(self, model: Model) -> None:
        """
        Register a model with the case manager.
        
        Args:
            model: The model to register
        """
        self.tracked_models[model.id] = model
        logging.info(f"Model {model.name} registered with case manager")
    
    def unregister_model(self, model: Model) -> bool:
        """
        Unregister a model from the case manager.
        
        Args:
            model: The model to unregister
            
        Returns:
            True if the model was unregistered, False if not found
        """
        if model.id in self.tracked_models:
            del self.tracked_models[model.id]
            # Remove any relationships involving this model
            self.case_relationships = [
                (src, tgt, rel) for src, tgt, rel in self.case_relationships
                if src.id != model.id and tgt.id != model.id
            ]
            logging.info(f"Model {model.name} unregistered from case manager")
            return True
        return False
    
    def transform_case(self, model: Model, target_case: Case) -> Model:
        """
        Transform a model to a target case.
        
        Args:
            model: The model to transform
            target_case: The target case to transform to
            
        Returns:
            The transformed model
        """
        if target_case in self.case_handlers:
            handler = self.case_handlers[target_case]
            return handler.apply(model)
        else:
            # Fall back to basic transformation
            model.case = target_case
            return model
    
    def create_relationship(self, source: Model, target: Model, 
                           relationship_type: str) -> Tuple[Model, Model]:
        """
        Create a case relationship between two models.
        
        Args:
            source: The source model
            target: The target model
            relationship_type: The type of relationship
            
        Returns:
            Tuple of (source, target) models after transformation
        """
        # Define transformations based on relationship types
        transformations = {
            "generates": (Case.NOMINATIVE, Case.DATIVE),
            "updates": (Case.NOMINATIVE, Case.ACCUSATIVE),
            "receives_from": (Case.DATIVE, Case.GENITIVE),
            "implements": (Case.INSTRUMENTAL, Case.ACCUSATIVE),
            "contextualizes": (Case.LOCATIVE, Case.NOMINATIVE),
            "derives_from": (Case.NOMINATIVE, Case.ABLATIVE),
            "addresses": (Case.NOMINATIVE, Case.VOCATIVE),
        }
        
        if relationship_type in transformations:
            source_case, target_case = transformations[relationship_type]
            self.transform_case(source, source_case)
            self.transform_case(target, target_case)
            
            # Record the relationship
            self.case_relationships.append((source, target, relationship_type))
            
            # Connect the models
            source.connect(target, relationship_type)
            target.connect(source, f"inverse_{relationship_type}")
            
            logging.info(f"Created {relationship_type} relationship from {source.name} to {target.name}")
        else:
            logging.warning(f"Unknown relationship type: {relationship_type}")
        
        return source, target
    
    def process_update(self, model: Model, data: Any) -> Dict[str, Any]:
        """
        Process an update using the appropriate case handler.
        
        Args:
            model: The model to update
            data: The update data
            
        Returns:
            Update result dictionary
        """
        case = model.case
        if case in self.case_handlers:
            handler = self.case_handlers[case]
            return handler.process_update(model, data)
        else:
            # Fall back to model's default update method
            if hasattr(model, 'update') and callable(model.update):
                return model.update(data)
            return {"status": "error", "message": f"No handler for case {case}"}
    
    def get_models_by_case(self, case: Case) -> List[Model]:
        """
        Get all tracked models with a specific case.
        
        Args:
            case: The case to filter by
            
        Returns:
            List of models with the specified case
        """
        return [model for model in self.tracked_models.values() if model.case == case]
    
    def get_related_models(self, model: Model, relationship_type: Optional[str] = None) -> List[Tuple[Model, str]]:
        """
        Get models related to the given model.
        
        Args:
            model: The model to find relationships for
            relationship_type: Optional type to filter by
            
        Returns:
            List of (related_model, relationship_type) tuples
        """
        related = []
        
        # Check source relationships
        for src, tgt, rel in self.case_relationships:
            if src.id == model.id and (relationship_type is None or rel == relationship_type):
                related.append((tgt, rel))
                
        # Check target relationships
        for src, tgt, rel in self.case_relationships:
            if tgt.id == model.id and (relationship_type is None or rel == relationship_type):
                related.append((src, f"inverse_{rel}"))
                
        return related
    
    def calculate_free_energy(self, model: Model) -> float:
        """
        Calculate free energy using the appropriate case handler.
        
        Args:
            model: The model to calculate free energy for
            
        Returns:
            The calculated free energy
        """
        case = model.case
        if case in self.case_handlers:
            handler = self.case_handlers[case]
            if hasattr(handler, 'calculate_free_energy'):
                return handler.calculate_free_energy(model)
        
        # Fall back to model's free_energy method
        if hasattr(model, 'free_energy') and callable(model.free_energy):
            return model.free_energy()
        
        # Default fallback
        return 1.0 