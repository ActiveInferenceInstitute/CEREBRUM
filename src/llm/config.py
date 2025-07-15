"""
LLM Configuration

Configuration file for LLM models and settings used across the CEREBRUM project.
"""

import os
from typing import Dict, List, Optional
from dotenv import load_dotenv

# Load environment variables from .env file
load_dotenv()

# Available OpenRouter Models
AVAILABLE_MODELS = {
    "kimi": "moonshotai/kimi-k2:free",
    "deepseek": "tngtech/deepseek-r1t2-chimera:free"
}

# Default model selection
DEFAULT_MODEL = "kimi"  # Uses moonshotai/kimi-k2:free

# Model configurations
MODEL_CONFIGS = {
    "kimi": {
        "name": "moonshotai/kimi-k2:free",
        "display_name": "Kimi K2 (Free)",
        "provider": "Moonshot AI",
        "max_tokens": 4000,
        "temperature": 0.7,
        "description": "Free tier of Moonshot AI's Kimi K2 model"
    },
    "deepseek": {
        "name": "tngtech/deepseek-r1t2-chimera:free", 
        "display_name": "DeepSeek R1T2 Chimera (Free)",
        "provider": "TNG Tech",
        "max_tokens": 4000,
        "temperature": 0.7,
        "description": "Free tier of DeepSeek's R1T2 Chimera model"
    }
}

# API Configuration
API_CONFIG = {
    "openrouter_api_key": os.getenv("OPENROUTER_API_KEY"),
    "timeout": 90.0,
    "max_retries": 3,
    "base_delay": 1.0,
    "max_delay": 60.0
}

# Analysis Configuration
ANALYSIS_CONFIG = {
    "default_temperature": 0.7,
    "default_max_tokens": 1000,
    "keyword_extraction_max": 5,
    "overview_max_length": 200,
    "content_analysis_limit": 2000
}

def get_model_name(model_key: Optional[str] = None) -> str:
    """
    Get the full model name for the specified model key.
    
    Args:
        model_key: Model key (e.g., 'kimi', 'deepseek'). If None, uses DEFAULT_MODEL.
        
    Returns:
        Full model name (e.g., 'moonshotai/kimi-k2:free')
    """
    if model_key is None:
        model_key = DEFAULT_MODEL
    
    if model_key not in AVAILABLE_MODELS:
        raise ValueError(f"Unknown model key: {model_key}. Available: {list(AVAILABLE_MODELS.keys())}")
    
    return AVAILABLE_MODELS[model_key]

def get_model_config(model_key: Optional[str] = None) -> Dict:
    """
    Get the full configuration for the specified model.
    
    Args:
        model_key: Model key (e.g., 'kimi', 'deepseek'). If None, uses DEFAULT_MODEL.
        
    Returns:
        Dictionary with model configuration
    """
    if model_key is None:
        model_key = DEFAULT_MODEL
    
    if model_key not in MODEL_CONFIGS:
        raise ValueError(f"Unknown model key: {model_key}. Available: {list(MODEL_CONFIGS.keys())}")
    
    return MODEL_CONFIGS[model_key]

def list_available_models() -> List[Dict]:
    """
    Get a list of all available models with their configurations.
    
    Returns:
        List of model configuration dictionaries
    """
    return [
        {
            "key": key,
            **config
        }
        for key, config in MODEL_CONFIGS.items()
    ]

def validate_api_key() -> bool:
    """
    Validate that the OpenRouter API key is set.
    
    Returns:
        True if API key is set, False otherwise
    """
    return API_CONFIG["openrouter_api_key"] is not None

def get_api_key() -> Optional[str]:
    """
    Get the OpenRouter API key.
    
    Returns:
        API key string or None if not set
    """
    return API_CONFIG["openrouter_api_key"] 