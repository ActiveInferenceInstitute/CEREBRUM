"""OpenRouter Python Client - Production Ready

Production-ready Python wrapper for OpenRouter API providing access to multiple AI models 
with comprehensive error handling, retry logic, circuit breaker patterns, and performance monitoring.

Usage:
    from src.llm.OpenRouter import OpenRouterClient, OpenRouterConfig, Conversation, quick_chat
    
    # Basic usage
    client = OpenRouterClient()
    response = client.simple_chat("Hello, how are you?")
    
    # Advanced configuration
    config = OpenRouterConfig(
        retry_config=RetryConfig(max_retries=5),
        enable_logging=True
    )
    client = OpenRouterClient(config)
"""

from .openrouter import (
    OpenRouterClient, 
    OpenRouterConfig, 
    Conversation, 
    quick_chat,
    RetryConfig,
    CircuitBreakerConfig,
    CircuitBreaker
)

__all__ = [
    # Main client functionality
    "OpenRouterClient", 
    "OpenRouterConfig", 
    "Conversation", 
    "quick_chat",
    
    # Configuration classes
    "RetryConfig", 
    "CircuitBreakerConfig",
    "CircuitBreaker"
]

__version__ = "2.0.0" 