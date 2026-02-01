"""
Ollama Integration Module

This module provides a client for interacting with local Ollama LLM instances.
"""

from .client import OllamaClient
from .utils import (
    list_models,
    pull_model,
    check_ollama_running,
    get_default_model,
)

__all__ = [
    'OllamaClient',
    'list_models',
    'pull_model',
    'check_ollama_running',
    'get_default_model',
]
