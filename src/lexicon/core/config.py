"""
LEXICON Configuration

Configuration settings for LEXICON and OpenRouter integration.
"""

import os
from typing import Dict, Any, Optional
from dataclasses import dataclass, field
from pathlib import Path
import json


@dataclass
class LexiconConfig:
    """Configuration for LEXICON engine."""
    
    # API settings
    openrouter_api_key: Optional[str] = None
    base_api_url: str = "https://openrouter.ai/api/v1"
    
    # Model selection
    default_model: str = "anthropic/claude-3.5-sonnet"
    fallback_models: Dict[str, str] = field(default_factory=lambda: {
        "case_declension": "openai/gpt-4o-mini",
        "paraphrase": "openai/gpt-4o-mini",
        "entity_extraction": "anthropic/claude-3-haiku",
        "graph_assembly": "openai/gpt-4o-mini",
    })
    
    # Processing settings
    max_batch_size: int = 25
    chunk_size: int = 2000
    chunk_overlap: int = 200
    
    # Output settings
    output_dir: Path = field(default_factory=lambda: Path("output"))
    cache_dir: Path = field(default_factory=lambda: Path("cache"))
    
    # Performance settings
    parallel_requests: int = 5
    timeout_seconds: float = 60.0
    
    # Logging settings
    log_level: str = "INFO"
    enable_detailed_logging: bool = True
    
    def __post_init__(self):
        """Initialize after creation."""
        # Load API key from environment if not provided
        if self.openrouter_api_key is None:
            self.openrouter_api_key = os.environ.get("OPENROUTER_API_KEY")
            
        # Create directories if they don't exist
        self.output_dir.mkdir(parents=True, exist_ok=True)
        self.cache_dir.mkdir(parents=True, exist_ok=True)
    
    def save(self, config_path: Path) -> None:
        """Save configuration to file."""
        # Convert paths to strings for JSON serialization
        config_dict = {
            k: str(v) if isinstance(v, Path) else v 
            for k, v in self.__dict__.items()
        }
        
        with open(config_path, 'w') as f:
            json.dump(config_dict, f, indent=2)
    
    @classmethod
    def load(cls, config_path: Path) -> 'LexiconConfig':
        """Load configuration from file."""
        with open(config_path, 'r') as f:
            config_dict = json.load(f)
        
        # Convert string paths back to Path objects
        for key in ['output_dir', 'cache_dir']:
            if key in config_dict:
                config_dict[key] = Path(config_dict[key])
        
        return cls(**config_dict)


def get_default_config() -> LexiconConfig:
    """Return default configuration with environment variables applied."""
    config = LexiconConfig()
    
    # Override with environment variables if present
    if "LEXICON_DEFAULT_MODEL" in os.environ:
        config.default_model = os.environ["LEXICON_DEFAULT_MODEL"]
    
    if "LEXICON_OUTPUT_DIR" in os.environ:
        config.output_dir = Path(os.environ["LEXICON_OUTPUT_DIR"])
        config.output_dir.mkdir(parents=True, exist_ok=True)
    
    if "LEXICON_LOG_LEVEL" in os.environ:
        config.log_level = os.environ["LEXICON_LOG_LEVEL"]
    
    return config


# OpenRouter model selection for different LEXICON tasks
LEXICON_OPENROUTER_CONFIG = {
    "api_key": os.getenv("OPENROUTER_API_KEY"),
    "base_url": "https://openrouter.ai/api/v1",
    "default_models": {
        "case_declension": "anthropic/claude-3.5-sonnet",
        "paraphrase": "openai/gpt-4o-mini", 
        "entity_extraction": "anthropic/claude-3.5-sonnet",
        "graph_assembly": "openai/gpt-4o-mini",
        "active_learning": "openai/gpt-4o"
    },
    "fallback_strategy": "auto_router",
    "retry_config": {
        "max_retries": 3,
        "base_delay": 1.0,
        "max_delay": 30.0
    },
    "circuit_breaker": {
        "failure_threshold": 5,
        "recovery_timeout": 60
    }
} 