"""
Case definitions for linear regression tests.

This module provides linguistic explanations and statistical relevance 
of each case in the context of linear regression models.
"""
import os
import sys
import logging
from typing import Dict

# Add the src directory to the path for imports if needed
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..')))

# Import CEREBRUM components
from core.model import Case

# Configure logging
logger = logging.getLogger("cerebrum-regression-tests")


class CaseDefinitions:
    """
    Provides linguistic explanations and statistical relevance of each case for linear regression.
    
    This class bridges the gap between linguistic cases from the CEREBRUM framework
    and their statistical/mathematical counterparts in regression analysis.
    """
    
    @staticmethod
    def nominative() -> Dict[str, str]:
        """
        NOMINATIVE case: The model as the active agent or subject.
        
        In linguistics: The subject of the sentence, performing the action.
        In regression: The model actively generating predictions, estimating parameters.
        """
        return {
            "case": Case.NOMINATIVE.value,
            "linguistic_meaning": "Subject performing the action",
            "statistical_role": "Model as parameter estimator",
            "regression_context": "The model that fits coefficients to data, making the line 'act' on the data space",
            "formula": "ŷ = θ₀ + θ₁x",
            "primary_methods": "fit(), predict()",
            "visualization": "Regression line overlaid on data points",
            "example": "The LINEAR MODEL fits the data points.",
        }
    
    @staticmethod
    def accusative() -> Dict[str, str]:
        """
        ACCUSATIVE case: The model as the direct object or recipient of validation.
        
        In linguistics: The direct object receiving the action of the verb.
        In regression: The model being evaluated, tested, or validated.
        """
        return {
            "case": Case.ACCUSATIVE.value, 
            "linguistic_meaning": "Direct object receiving action",
            "statistical_role": "Model as evaluation subject",
            "regression_context": "The model that undergoes validation, hypothesis testing, and performance assessment",
            "formula": "R² = 1 - SSres/SStot",
            "primary_methods": "evaluate(), validate()",
            "visualization": "Diagnostic plots, metrics visualization",
            "example": "The analyst EVALUATES the LINEAR MODEL."
        }
    
    @staticmethod
    def dative() -> Dict[str, str]:
        """
        DATIVE case: The model as indirect object or data recipient.
        
        In linguistics: The indirect object, typically a recipient.
        In regression: The model receiving data inputs or serving as destination.
        """
        return {
            "case": Case.DATIVE.value,
            "linguistic_meaning": "Indirect object or recipient",
            "statistical_role": "Model as data receiver",
            "regression_context": "The model that receives input data, feature vectors, or streaming observations",
            "formula": "Model ← {(x₁,y₁), (x₂,y₂), ..., (xₙ,yₙ)}",
            "primary_methods": "receive_data(), process_features()",
            "visualization": "Data flow diagrams, feature histograms",
            "example": "The researcher GIVES data TO the LINEAR MODEL."
        }
    
    @staticmethod
    def genitive() -> Dict[str, str]:
        """
        GENITIVE case: The model as possessor or source of outputs.
        
        In linguistics: Indicates possession or source.
        In regression: The model as a source of predictions or reports.
        """
        return {
            "case": Case.GENITIVE.value,
            "linguistic_meaning": "Possessive or source relation",
            "statistical_role": "Model as prediction source",
            "regression_context": "The model that generates predictions, intervals, and output reports",
            "formula": "ŷ = Model(x), CI = [ŷ-tα/2·s, ŷ+tα/2·s]",
            "primary_methods": "generate_predictions(), get_summary()",
            "visualization": "Prediction intervals, output reports",
            "example": "The predictions BELONG TO the LINEAR MODEL."
        }
    
    @staticmethod
    def instrumental() -> Dict[str, str]:
        """
        INSTRUMENTAL case: The model as tool or method.
        
        In linguistics: Indicates the means by which an action is done.
        In regression: The model as methodology or algorithm.
        """
        return {
            "case": Case.INSTRUMENTAL.value,
            "linguistic_meaning": "Means or instrument of action",
            "statistical_role": "Model as statistical methodology",
            "regression_context": "The specific algorithm (OLS, gradient descent) used to solve the regression problem",
            "formula": "θ = (XᵀX)⁻¹Xᵀy",
            "primary_methods": "calculate_coefficients(), apply_method()",
            "visualization": "Algorithm convergence, computational process",
            "example": "The researcher solves the problem WITH the LINEAR MODEL."
        }
    
    @staticmethod
    def locative() -> Dict[str, str]:
        """
        LOCATIVE case: The model as location or context.
        
        In linguistics: Indicates location or context.
        In regression: The model as statistical context or assumption framework.
        """
        return {
            "case": Case.LOCATIVE.value,
            "linguistic_meaning": "Location or context marker",
            "statistical_role": "Model as assumption framework",
            "regression_context": "The statistical context including assumptions (homoscedasticity, normality) and conditions",
            "formula": "Var(ε|X) = σ², ε ~ N(0,σ²)",
            "primary_methods": "check_assumptions(), provide_context()",
            "visualization": "Residual plots, QQ plots, assumption diagnostics",
            "example": "The pattern exists WITHIN the LINEAR MODEL's assumptions."
        }
    
    @staticmethod
    def ablative() -> Dict[str, str]:
        """
        ABLATIVE case: The model as origin or causal factor.
        
        In linguistics: Indicates movement away from, origin, or cause.
        In regression: The model as source of error or origin of effects.
        """
        return {
            "case": Case.ABLATIVE.value,
            "linguistic_meaning": "Origin, separation, or source",
            "statistical_role": "Model as error origin",
            "regression_context": "Analysis of where errors come from, residual analysis, and source attribution",
            "formula": "ε = y - ŷ, residuals = y - Xθ",
            "primary_methods": "analyze_residuals(), decompose_error()",
            "visualization": "Error decomposition, residual distribution",
            "example": "The prediction errors COME FROM the LINEAR MODEL."
        }
    
    @staticmethod
    def vocative() -> Dict[str, str]:
        """
        VOCATIVE case: The model as addressable interface.
        
        In linguistics: Direct address, calling someone by name.
        In regression: The model as interactive interface or API.
        """
        return {
            "case": Case.VOCATIVE.value,
            "linguistic_meaning": "Direct address or invocation",
            "statistical_role": "Model as interactive interface",
            "regression_context": "The user interface to the regression model, APIs, or interactive diagnostics",
            "formula": "API: predict(X) → ŷ",
            "primary_methods": "expose_interface(), respond_to_query()",
            "visualization": "Interactive dashboards, query-response diagrams",
            "example": "HEY LINEAR MODEL, what is the prediction for this input?"
        }
    
    @staticmethod
    def get_all_cases() -> Dict[Case, Dict[str, str]]:
        """Returns a dictionary with information for all cases."""
        return {
            Case.NOMINATIVE: CaseDefinitions.nominative(),
            Case.ACCUSATIVE: CaseDefinitions.accusative(),
            Case.DATIVE: CaseDefinitions.dative(),
            Case.GENITIVE: CaseDefinitions.genitive(),
            Case.INSTRUMENTAL: CaseDefinitions.instrumental(),
            Case.LOCATIVE: CaseDefinitions.locative(),
            Case.ABLATIVE: CaseDefinitions.ablative(),
            Case.VOCATIVE: CaseDefinitions.vocative(),
        } 