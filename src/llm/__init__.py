"""
Corym Library Large Language Model Integration Framework

This module provides comprehensive tools for integrating with various Large Language Models,
conducting research, performing analysis, and managing LLM-based workflows. It supports
multiple LLM providers and research methodologies with robust error handling and
performance tracking.

Key Components:
    Research Assistants:
        - PerplexityResearcher: Real-time internet research with citations
        - OpenAIResearcher: Knowledge-based research and analysis  
        - WebResearcher: Web scraping and content extraction
        - ResearchAssistant: General-purpose research interface
        
    Analysis Engines:
        - StrategicAnalyzer: Strategic planning and recommendation generation
        - ProductEvaluator: Product-market fit and team evaluation
        - OpenRouterAnalysisEngine: Multi-model analysis workflows
        
    Utilities and Management:
        - LLMUtils: Common utilities for LLM operations
        - EvaluationResultsManager: Results processing and reporting

Features:
    Multi-Provider Support:
        - OpenAI GPT models (3.5-turbo, 4, etc.)
        - Perplexity Sonar models with real-time search
        - OpenRouter model marketplace access
        - Custom provider integration support
        
    Research Capabilities:
        - Real-time internet search and analysis
        - Multi-query batch processing  
        - Citation extraction and validation
        - Domain-specific filtering
        - Recency-based search constraints
        
    Analysis Workflows:
        - Team capability assessment
        - Product-market fit evaluation
        - Strategic recommendation generation
        - Narrative development and adaptation
        - Audience targeting and segmentation
        
    Quality Assurance:
        - Rate limiting and retry mechanisms
        - Response validation and parsing
        - Error handling and recovery
        - Performance metrics tracking
        - Cost monitoring and optimization

Architecture:
    The LLM module uses a layered architecture with provider abstraction:
    
    Application Layer:
        - StrategicAnalyzer: High-level strategic analysis
        - ProductEvaluator: Specialized product evaluation
        - EvaluationResultsManager: Results processing
        
    Research Layer:
        - ResearchAssistant: General research interface
        - PerplexityResearcher: Real-time web research
        - OpenAIResearcher: Knowledge-based analysis
        - WebResearcher: Direct web access
        
    Provider Layer:
        - OpenRouterAnalysisEngine: Multi-model access
        - Provider-specific API clients
        - Rate limiting and authentication
        
    Utility Layer:
        - LLMUtils: Common operations and utilities
        - Token counting and text processing
        - Response parsing and validation

Usage Examples:
    Basic Research:
        >>> from corym.llm import PerplexityResearcher
        >>> researcher = PerplexityResearcher()
        >>> results = await researcher.search("AI safety trends 2024")
        
    Strategic Analysis:
        >>> from corym.llm import StrategicAnalyzer, ResearchAssistant
        >>> analyzer = StrategicAnalyzer(ResearchAssistant(), ProductEvaluator(), FileUtils())
        >>> recommendations = analyzer.generate_strategic_recommendations(context)
        
    Product Evaluation:
        >>> from corym.llm import ProductEvaluator
        >>> evaluator = ProductEvaluator()
        >>> evaluation = await evaluator.evaluate_team_for_product(team, product)
        
    Batch Research:
        >>> queries = ["AI trends", "Market analysis", "Tech adoption"]
        >>> results = await researcher.multi_query_research(queries)

API Reference:
    Research Interfaces:
        - BaseResearcher: Abstract researcher interface
        - PerplexityResearcher: Real-time search with Perplexity API
        - OpenAIResearcher: Knowledge analysis with OpenAI models
        - WebResearcher: Direct web scraping and extraction
        - ResearchAssistant: General-purpose research wrapper
        
    Analysis Components:
        - StrategicAnalyzer: Strategic planning and recommendations
        - ProductEvaluator: Product and team evaluation
        - OpenRouterAnalysisEngine: Multi-model analysis workflows
        
    Result Management:
        - EvaluationResultsManager: Processing and reporting
        - Result formatting and export capabilities
        - Performance metrics and analytics
        
    Utilities:
        - LLMUtils: Token counting, validation, formatting
        - Rate limiting and retry mechanisms
        - Response parsing and extraction
        - Cost tracking and optimization

Provider Integration:
    Perplexity API:
        - Real-time web search capabilities
        - Citation extraction and validation
        - Multiple model sizes (small, large, pro)
        - Domain and recency filtering
        
    OpenAI API:
        - GPT model family access
        - Function calling and structured outputs
        - Fine-tuned model support
        - Cost-optimized model selection
        
    OpenRouter API:
        - Multi-provider model access
        - Cost comparison and optimization
        - Rate limit management
        - Model capability matching

Error Handling:
    - Graceful degradation for missing dependencies
    - Automatic retry with exponential backoff
    - Provider failover and fallback
    - Comprehensive error logging and reporting
    - Circuit breaker patterns for reliability

Performance Features:
    - Request batching and optimization
    - Response caching and memoization
    - Token usage tracking and alerts
    - Rate limit management
    - Concurrent request handling

Version: 1.0.0
License: MIT
"""

import logging
from typing import List, Dict, Any, Optional

# Set up LLM-specific logging
logger = logging.getLogger("corym.llm")

# Track what components are available
_available_components = {}

# Core utilities (should always be available)
try:
    from .llm_utils import LLMUtils
    _available_components["LLMUtils"] = True
    logger.debug("LLMUtils loaded successfully")
except ImportError as e:
    logger.warning(f"LLMUtils could not be loaded: {e}")
    _available_components["LLMUtils"] = False

# OpenRouter analysis engine
try:
    from .openrouter_analysis_engine import OpenRouterAnalysisEngine, AnalysisResult
    _available_components["OpenRouterAnalysisEngine"] = True
    logger.debug("OpenRouterAnalysisEngine loaded successfully")
except ImportError as e:
    logger.warning(f"OpenRouterAnalysisEngine could not be loaded: {e}")
    _available_components["OpenRouterAnalysisEngine"] = False

# OpenRouter client
try:
    from .OpenRouter.openrouter import OpenRouterClient, OpenRouterConfig, Conversation
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