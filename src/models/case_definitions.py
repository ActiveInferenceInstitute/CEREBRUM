#!/usr/bin/env python3
"""
Case Definitions Module for CEREBRUM
Defines linguistic meanings for each grammatical case in the statistical context
"""

from typing import Dict
from .base import Case

class CaseDefinitions:
    """Definitions for all linguistic cases in the regression context."""
    
    @staticmethod
    def nominative() -> Dict[str, str]:
        """
        NOMINATIVE case: The subject performing the action.
        In regression: Model actively fitting parameters to data.
        """
        return {
            "linguistic_meaning": "Subject performing the action",
            "statistical_role": "Model as parameter estimator",
            "regression_context": "Model actively fitting parameters to data",
            "example": "The MODEL fits the parameters to the data."
        }
    
    @staticmethod
    def accusative() -> Dict[str, str]:
        """
        ACCUSATIVE case: The direct object receiving the action.
        In regression: Model undergoing evaluation or testing.
        """
        return {
            "linguistic_meaning": "Direct object receiving action",
            "statistical_role": "Model as evaluation subject",
            "regression_context": "Model undergoing evaluation or testing",
            "example": "We evaluate the MODEL on test data."
        }
    
    @staticmethod
    def dative() -> Dict[str, str]:
        """
        DATIVE case: The indirect object, typically recipient.
        In regression: Model receiving data or serving as destination.
        """
        return {
            "linguistic_meaning": "Indirect object or recipient",
            "statistical_role": "Model as data recipient",
            "regression_context": "Model receiving or being given data",
            "example": "We give the data to the MODEL."
        }
    
    @staticmethod
    def genitive() -> Dict[str, str]:
        """
        GENITIVE case: Showing possession or relation.
        In regression: Model generating predictions or outputs.
        """
        return {
            "linguistic_meaning": "Possessive or relational function",
            "statistical_role": "Model as prediction generator",
            "regression_context": "Model generating predictions from inputs",
            "example": "The predictions of the MODEL were accurate."
        }
    
    @staticmethod
    def instrumental() -> Dict[str, str]:
        """
        INSTRUMENTAL case: The means by which action is accomplished.
        In regression: Model being used as a tool for analysis.
        """
        return {
            "linguistic_meaning": "Means or instrument of action",
            "statistical_role": "Model as analytical instrument",
            "regression_context": "Using model as a tool for data analysis",
            "example": "We analyze the data with the MODEL."
        }
    
    @staticmethod
    def locative() -> Dict[str, str]:
        """
        LOCATIVE case: Indicating location or position.
        In regression: Model being positioned in parameter space.
        """
        return {
            "linguistic_meaning": "Location or position indication",
            "statistical_role": "Model position in parameter space",
            "regression_context": "Model at a position in parameter/hypothesis space",
            "example": "The optimal solution exists in the MODEL's parameter space."
        }
    
    @staticmethod
    def ablative() -> Dict[str, str]:
        """
        ABLATIVE case: Indicating separation or movement away from.
        In regression: Extracting information or deriving conclusions from model.
        """
        return {
            "linguistic_meaning": "Movement away from or source",
            "statistical_role": "Model as information source",
            "regression_context": "Extracting insights or derivations from model",
            "example": "From the MODEL, we can derive feature importance."
        }
    
    @staticmethod
    def vocative() -> Dict[str, str]:
        """
        VOCATIVE case: Used for direct address.
        In regression: Querying or interacting directly with the model.
        """
        return {
            "linguistic_meaning": "Direct address or calling",
            "statistical_role": "Direct model interaction",
            "regression_context": "Querying or commanding the model directly",
            "example": "MODEL, what is the predicted value for this input?"
        }
    
    @staticmethod
    def get_all_cases() -> Dict[Case, Dict[str, str]]:
        """Get definitions for all grammatical cases."""
        return {
            Case.NOMINATIVE: CaseDefinitions.nominative(),
            Case.ACCUSATIVE: CaseDefinitions.accusative(),
            Case.DATIVE: CaseDefinitions.dative(),
            Case.GENITIVE: CaseDefinitions.genitive(),
            Case.INSTRUMENTAL: CaseDefinitions.instrumental(),
            Case.LOCATIVE: CaseDefinitions.locative(),
            Case.ABLATIVE: CaseDefinitions.ablative(),
            Case.VOCATIVE: CaseDefinitions.vocative()
        } 