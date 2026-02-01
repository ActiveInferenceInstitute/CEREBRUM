"""
Ollama Utility Functions

Helper functions for Ollama model management and server interaction.
"""

import logging
import subprocess
import requests
from typing import List, Dict, Any, Optional

logger = logging.getLogger(__name__)

DEFAULT_BASE_URL = "http://localhost:11434"


def check_ollama_running(base_url: str = None) -> bool:
    """
    Check if Ollama server is running and accessible.
    
    Args:
        base_url: Ollama server URL (default: http://localhost:11434)
        
    Returns:
        True if Ollama is running
        
    Example:
        >>> if check_ollama_running():
        ...     print("Ollama is ready!")
    """
    url = base_url or DEFAULT_BASE_URL
    try:
        response = requests.get(f"{url}/api/tags", timeout=5)
        return response.status_code == 200
    except requests.exceptions.RequestException:
        return False


def list_models(base_url: str = None) -> List[Dict[str, Any]]:
    """
    List all available Ollama models.
    
    Args:
        base_url: Ollama server URL
        
    Returns:
        List of model information dictionaries with keys:
        - name: Model name
        - size: Model size in bytes
        - digest: Model digest
        - modified_at: Last modification timestamp
        
    Example:
        >>> models = list_models()
        >>> for m in models:
        ...     print(f"{m['name']}: {m['size'] / 1e9:.1f}GB")
    """
    url = base_url or DEFAULT_BASE_URL
    try:
        response = requests.get(f"{url}/api/tags", timeout=10)
        response.raise_for_status()
        return response.json().get("models", [])
    except requests.exceptions.RequestException as e:
        logger.error(f"Failed to list models: {e}")
        return []


def pull_model(
    model: str,
    base_url: str = None,
    progress_callback: callable = None
) -> bool:
    """
    Pull (download) a model from Ollama registry.
    
    Args:
        model: Model name (e.g., 'llama3.2', 'mistral', 'codellama')
        base_url: Ollama server URL
        progress_callback: Optional callback function for progress updates
        
    Returns:
        True if model was pulled successfully
        
    Example:
        >>> def on_progress(status, completed, total):
        ...     print(f"{status}: {completed}/{total}")
        >>> pull_model("llama3.2", progress_callback=on_progress)
    """
    url = base_url or DEFAULT_BASE_URL
    
    try:
        response = requests.post(
            f"{url}/api/pull",
            json={"name": model},
            stream=True,
            timeout=3600  # 1 hour timeout for large models
        )
        response.raise_for_status()
        
        for line in response.iter_lines():
            if line:
                import json
                data = json.loads(line.decode('utf-8'))
                
                if progress_callback:
                    progress_callback(
                        data.get("status", ""),
                        data.get("completed", 0),
                        data.get("total", 0)
                    )
                
                if data.get("status") == "success":
                    logger.info(f"Successfully pulled model: {model}")
                    return True
        
        return True
        
    except requests.exceptions.RequestException as e:
        logger.error(f"Failed to pull model {model}: {e}")
        return False


def get_default_model(base_url: str = None) -> Optional[str]:
    """
    Get the first available model, or suggest a good default.
    
    Args:
        base_url: Ollama server URL
        
    Returns:
        Name of an available model, or suggested default
        
    Example:
        >>> model = get_default_model()
        >>> client = OllamaClient(model=model)
    """
    models = list_models(base_url)
    
    if models:
        # Prefer these models in order
        preferred = ["llama3.2", "llama3.1", "mistral", "codellama", "phi"]
        for pref in preferred:
            for model in models:
                if pref in model.get("name", "").lower():
                    return model["name"]
        # Return first available
        return models[0].get("name")
    
    # No models available, suggest default
    return "llama3.2"


def get_model_info(model: str, base_url: str = None) -> Optional[Dict[str, Any]]:
    """
    Get detailed information about a specific model.
    
    Args:
        model: Model name
        base_url: Ollama server URL
        
    Returns:
        Model information dictionary or None
        
    Example:
        >>> info = get_model_info("llama3.2")
        >>> print(f"Parameters: {info.get('details', {}).get('parameter_size')}")
    """
    url = base_url or DEFAULT_BASE_URL
    try:
        response = requests.post(
            f"{url}/api/show",
            json={"name": model},
            timeout=10
        )
        response.raise_for_status()
        return response.json()
    except requests.exceptions.RequestException as e:
        logger.error(f"Failed to get model info for {model}: {e}")
        return None


def start_ollama_server() -> bool:
    """
    Attempt to start the Ollama server.
    
    Returns:
        True if server started or was already running
        
    Note:
        This requires 'ollama' to be installed and in PATH.
    """
    if check_ollama_running():
        logger.info("Ollama server already running")
        return True
    
    try:
        # Try to start ollama serve in background
        subprocess.Popen(
            ["ollama", "serve"],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL
        )
        
        # Wait for server to start
        import time
        for _ in range(10):
            time.sleep(1)
            if check_ollama_running():
                logger.info("Ollama server started successfully")
                return True
        
        logger.warning("Ollama server did not start within timeout")
        return False
        
    except FileNotFoundError:
        logger.error("Ollama not found. Install from https://ollama.ai")
        return False
    except Exception as e:
        logger.error(f"Failed to start Ollama server: {e}")
        return False


def format_model_size(size_bytes: int) -> str:
    """
    Format model size in human-readable format.
    
    Args:
        size_bytes: Size in bytes
        
    Returns:
        Formatted size string (e.g., "4.7 GB")
    """
    if size_bytes >= 1e9:
        return f"{size_bytes / 1e9:.1f} GB"
    elif size_bytes >= 1e6:
        return f"{size_bytes / 1e6:.1f} MB"
    elif size_bytes >= 1e3:
        return f"{size_bytes / 1e3:.1f} KB"
    return f"{size_bytes} B"
