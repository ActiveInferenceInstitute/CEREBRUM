"""
LEXICON Configuration

Configuration settings for LEXICON and OpenRouter integration.
"""

import json
import os
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional


@dataclass
class LexiconConfig:
    """Configuration for LEXICON engine."""
    
    # API settings
    openrouter_api_key: Optional[str] = None
    base_api_url: str = "https://openrouter.ai/api/v1"
    
    # Model selection
    default_model: str = "qwen/qwen3.8-27b"
    # Per-task model preferences; each is used as the primary model for that
    # task, with client_fallback_models used when the primary is unavailable.
    fallback_models: Dict[str, str] = field(default_factory=lambda: {
        "case_declension": "nvidia/nemotron-3.5-lightning:free",
        "paraphrase": "nvidia/nemotron-3.5-lightning:free",
        "entity_extraction": "dots-studio/dots-3-note-preview:free",
        "graph_assembly": "nvidia/nemotron-3.5-lightning:free",
    })
    # Client-wide ordered fallback chain tried when any model 404s
    # (dead slug, no endpoints, or account guardrail restriction).
    client_fallback_models: List[str] = field(default_factory=lambda: [
        "nvidia/nemotron-3.5-lightning:free",
        "dots-studio/dots-3-note-preview:free",
        "qwen/qwen3.8-27b",
        "moonshotai/kimi-k2",
    ])
    
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
        """Save configuration to file.

        The OpenRouter API key is treated as a secret and is intentionally
        excluded from the serialized config so it is never written to disk in
        plaintext. It is re-populated from the environment / config on load.
        """
        # Convert paths to strings for JSON serialization and drop the secret key.
        config_dict = {
            k: str(v) if isinstance(v, Path) else v
            for k, v in self.__dict__.items()
            if k != "openrouter_api_key"
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
