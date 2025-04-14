import os
import numpy as np
from typing import Dict, List, Any, Optional, Tuple, Union

from src.core.model import Case

# Case definitions for Neural Network models
class CaseDefinitions:
    """Class containing definitions for all CEREBRUM cases in Neural Network context."""
    
    @staticmethod
    def nominative() -> Dict[str, str]:
        """
        NOMINATIVE case: The model as active agent.
        
        In linguistics: Subject of verb.
        In Neural Network: Model actively generating predictions.
        """
        return {
            "case": Case.NOMINATIVE.value,
            "linguistic_meaning": "Subject/Doer of action",
            "statistical_role": "Active predictor",
            "neural_network_context": "The neural network as an active agent generating predictions from inputs",
            "formula": "f(x) = W*x + b",
            "primary_methods": "predict(), forward()",
            "visualization": "Forward propagation, activation patterns",
            "example": "The NEURAL NETWORK PREDICTS the output values"
        }
    
    @staticmethod
    def accusative() -> Dict[str, str]:
        """
        ACCUSATIVE case: The model as object of process.
        
        In linguistics: Direct object.
        In Neural Network: Model being trained or evaluated.
        """
        return {
            "case": Case.ACCUSATIVE.value,
            "linguistic_meaning": "Direct object/Receiver of action",
            "statistical_role": "Target of optimization",
            "neural_network_context": "The neural network as an object being trained or evaluated",
            "formula": "L(θ) = (1/n)∑(y - ŷ)²",
            "primary_methods": "evaluate(), calculate_loss()",
            "visualization": "Loss curves, error metrics",
            "example": "The researcher TRAINS the NEURAL NETWORK"
        }
    
    @staticmethod
    def dative() -> Dict[str, str]:
        """
        DATIVE case: The model as recipient.
        
        In linguistics: Indirect object.
        In Neural Network: Model receiving inputs.
        """
        return {
            "case": Case.DATIVE.value,
            "linguistic_meaning": "Indirect object/Recipient",
            "statistical_role": "Data receiver",
            "neural_network_context": "The neural network as a recipient of input data",
            "formula": "input → network",
            "primary_methods": "receive_data(), process_inputs()",
            "visualization": "Input transformations, data flow diagrams",
            "example": "The dataset GIVES inputs TO the NEURAL NETWORK"
        }
    
    @staticmethod
    def genitive() -> Dict[str, str]:
        """
        GENITIVE case: The model as source/possessor.
        
        In linguistics: Possessive.
        In Neural Network: Model generating outputs.
        """
        return {
            "case": Case.GENITIVE.value,
            "linguistic_meaning": "Possessive/Source",
            "statistical_role": "Output source",
            "neural_network_context": "The neural network as a source of outputs and predictions",
            "formula": "ŷ = f(x; θ)",
            "primary_methods": "generate_output(), get_predictions()",
            "visualization": "Output distributions, activation maps",
            "example": "The NEURAL NETWORK's OUTPUTS are used for decision-making"
        }
    
    @staticmethod
    def instrumental() -> Dict[str, str]:
        """
        INSTRUMENTAL case: The model as method/tool.
        
        In linguistics: Means or instrument.
        In Neural Network: Model as computational method.
        """
        return {
            "case": Case.INSTRUMENTAL.value,
            "linguistic_meaning": "By means of/Using",
            "statistical_role": "Computational method",
            "neural_network_context": "The neural network as a computational method for solving problems",
            "formula": "f(x) = σ(Wx + b)",
            "primary_methods": "compute(), apply_method()",
            "visualization": "Architecture diagrams, computational graphs",
            "example": "The team solves the problem BY USING a NEURAL NETWORK"
        }
    
    @staticmethod
    def locative() -> Dict[str, str]:
        """
        LOCATIVE case: The model as location/context.
        
        In linguistics: Location or time.
        In Neural Network: Model as representational space.
        """
        return {
            "case": Case.LOCATIVE.value,
            "linguistic_meaning": "In/At/Within",
            "statistical_role": "Representational context",
            "neural_network_context": "The neural network as a representational space or feature context",
            "formula": "h = f(x) ∈ ℝⁿ",
            "primary_methods": "get_representation(), extract_features()",
            "visualization": "Feature space visualizations, embedding plots",
            "example": "The patterns exist WITHIN the NEURAL NETWORK's hidden layers"
        }
    
    @staticmethod
    def ablative() -> Dict[str, str]:
        """
        ABLATIVE case: The model as origin/cause.
        
        In linguistics: Movement from.
        In Neural Network: Model as source of errors.
        """
        return {
            "case": Case.ABLATIVE.value,
            "linguistic_meaning": "From/Out of/Because of",
            "statistical_role": "Error source",
            "neural_network_context": "The neural network as the source of prediction errors and gradients",
            "formula": "∇L = ∂L/∂θ",
            "primary_methods": "compute_gradients(), backpropagate()",
            "visualization": "Gradient flow, error attribution",
            "example": "The errors ORIGINATE FROM the NEURAL NETWORK's weights"
        }
    
    @staticmethod
    def vocative() -> Dict[str, str]:
        """
        VOCATIVE case: The model as addressable entity.
        
        In linguistics: Direct address.
        In Neural Network: Model as interactive interface.
        """
        return {
            "case": Case.VOCATIVE.value,
            "linguistic_meaning": "Direct address/Invocation",
            "statistical_role": "Interactive interface",
            "neural_network_context": "The neural network as an addressable entity with a query/response interface",
            "formula": "API: query(NN, input) → response",
            "primary_methods": "query(), get_response()",
            "visualization": "API diagrams, interaction flows",
            "example": "HEY NEURAL NETWORK, what is the prediction for this input?"
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