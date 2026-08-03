"""
CEREBRUM LLM Integration Framework

This module provides integration with Large Language Models for the CEREBRUM
framework. It supports multiple LLM providers (OpenRouter cloud and local
Ollama) with robust error handling, retry mechanisms, and performance
tracking.

Key Components:
    OpenRouter Integration:
        - OpenRouterClient: Provider client with chat completion, streaming,
          conversation management, and circuit-breaker fault tolerance
        - OpenRouterConfig: Client configuration (model, temperature, tokens)
        - Conversation: Multi-turn conversation management
        - quick_chat: One-shot convenience helper

    Analysis:
        - OpenRouterAnalysisEngine: Multi-model analysis workflows
        - AnalysisResult: Typed analysis result container

    Utilities:
        - LLMUtils: Common utilities for LLM operations

    Local Inference:
        - ollama.OllamaClient: Local Ollama LLM integration
        - ollama.check_ollama_running / list_models / pull_model: model helpers

Features:
    - Multi-provider support (OpenRouter, Ollama)
    - Rate limiting and retry mechanisms
    - Circuit breakers for fault tolerance
    - Response validation and parsing
    - Performance metrics tracking
    - Cost monitoring and optimization

Note:
    Research-assistant style components (PerplexityResearcher,
    OpenAIResearcher, WebResearcher, ResearchAssistant, StrategicAnalyzer,
    ProductEvaluator, EvaluationResultsManager) are planned but not yet
    implemented; see docs/README.md for the implementation status.

Usage Examples:
    OpenRouter chat:
        >>> from src.llm.OpenRouter.openrouter import quick_chat
        >>> response = quick_chat("What is CEREBRUM?")

    Multi-model analysis:
        >>> from src.llm import OpenRouterAnalysisEngine
        >>> engine = OpenRouterAnalysisEngine()
        >>> result = engine.analyze("Describe the CEREBRUM framework")

    Local Ollama inference:
        >>> from src.llm.ollama import OllamaClient, check_ollama_running
        >>> if check_ollama_running():
        ...     client = OllamaClient(model="llama3.2")
        ...     response = client.generate("What is CEREBRUM?")
"""

import logging
from typing import List, Dict, Any, Optional  # noqa: F401

# Set up LLM-specific logging
logger = logging.getLogger("corym.llm")

# Track what components are available
_available_components = {}

# Core utilities (should always be available)
try:
    from .llm_utils import LLMUtils  # noqa: F401
    _available_components["LLMUtils"] = True
    logger.debug("LLMUtils loaded successfully")
except ImportError as e:
    logger.warning(f"LLMUtils could not be loaded: {e}")
    _available_components["LLMUtils"] = False

# OpenRouter analysis engine
try:
    from .openrouter_analysis_engine import OpenRouterAnalysisEngine, AnalysisResult  # noqa: F401
    _available_components["OpenRouterAnalysisEngine"] = True
    logger.debug("OpenRouterAnalysisEngine loaded successfully")
except ImportError as e:
    logger.warning(f"OpenRouterAnalysisEngine could not be loaded: {e}")
    _available_components["OpenRouterAnalysisEngine"] = False

# OpenRouter client
try:
    from .OpenRouter.openrouter import OpenRouterClient, OpenRouterConfig, Conversation  # noqa: F401
    _available_components["OpenRouterClient"] = True
    logger.debug("OpenRouterClient loaded successfully")
except ImportError as e:
    logger.warning(f"OpenRouterClient could not be loaded: {e}")
    _available_components["OpenRouterClient"] = False

# Version and metadata
__version__ = "1.0.0"
__author__ = "Corym LLM Team"
__license__ = "MIT"

# Dynamic public API based on available components
__all__ = ["__version__", "__author__", "__license__", "get_available_components", "check_component_availability"]

# Add available components to __all__
if _available_components.get("LLMUtils"):
    __all__.append("LLMUtils")
if _available_components.get("OpenRouterAnalysisEngine"):
    __all__.extend(["OpenRouterAnalysisEngine", "AnalysisResult"])
if _available_components.get("OpenRouterClient"):
    __all__.extend(["OpenRouterClient", "OpenRouterConfig", "Conversation"])


def get_available_components() -> Dict[str, bool]:
    """
    Get a dictionary of available LLM components and their status.
    
    Returns:
        Dict[str, bool]: Component names mapped to availability status
        
    Example:
        >>> from corym.llm import get_available_components
        >>> components = get_available_components()
        >>> if components.get("PerplexityResearcher"):
        ...     print("Perplexity research is available")
    """
    return _available_components.copy()


def check_component_availability(component_name: str) -> bool:
    """
    Check if a specific LLM component is available.
    
    Args:
        component_name (str): Name of the component to check
        
    Returns:
        bool: True if component is available, False otherwise
        
    Example:
        >>> from corym.llm import check_component_availability
        >>> if check_component_availability("OpenAIResearcher"):
        ...     from corym.llm import OpenAIResearcher
    """
    return _available_components.get(component_name, False)


# Module constants
SUPPORTED_PROVIDERS = ["openai", "perplexity", "openrouter", "web"]
DEFAULT_MODELS = {
    "openai": "gpt-3.5-turbo",
    "perplexity": "sonar-small",
    "openrouter": "openai/gpt-3.5-turbo"
}
MAX_RETRIES = 3
DEFAULT_TIMEOUT = 30

# Add constants to exports
__all__.extend([
    "SUPPORTED_PROVIDERS",
    "DEFAULT_MODELS", 
    "MAX_RETRIES",
    "DEFAULT_TIMEOUT"
])

# Log module initialization
available_count = sum(_available_components.values())
total_count = len(_available_components)
logger.info(f"LLM module initialized: {available_count}/{total_count} components available") 