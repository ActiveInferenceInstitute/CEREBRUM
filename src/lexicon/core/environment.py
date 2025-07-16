"""
LEXICON Environment Configuration

Handles environment setup including CUDA configuration, protobuf configuration, and system checks.
"""

import os
import logging
from typing import Optional, Any, Dict

logger = logging.getLogger(__name__)


def configure_protobuf_environment() -> None:
    """
    Configure protobuf environment to handle version conflicts.
    
    This fixes issues with transformers library failing to import due to
    protobuf version incompatibilities.
    """
    try:
        # Set protobuf implementation to use pure Python parsing
        # This is slower but more compatible across protobuf versions
        os.environ['PROTOCOL_BUFFERS_PYTHON_IMPLEMENTATION'] = 'python'
        
        # Also try to configure the protobuf library directly
        try:
            import google.protobuf
            # Clear any cached modules to ensure the environment variable takes effect
            if hasattr(google.protobuf, '_internal'):
                # Reset protobuf internal state
                pass
            logger.debug("Protobuf environment configured")
        except ImportError:
            logger.debug("Protobuf not available for direct configuration")
            
    except Exception as e:
        logger.warning(f"Could not configure protobuf environment: {e}")


def configure_transformers_environment() -> None:
    """
    Configure transformers library environment to avoid import failures.
    """
    try:
        # Disable transformers logging to reduce noise
        os.environ['TRANSFORMERS_VERBOSITY'] = 'error'
        
        # Set offline mode if needed to avoid network calls during import
        # This can help if there are network-related import issues
        # os.environ['TRANSFORMERS_OFFLINE'] = '1'
        
        logger.debug("Transformers environment configured")
    except Exception as e:
        logger.warning(f"Could not configure transformers environment: {e}")


def test_transformers_import() -> bool:
    """
    Test if transformers can be imported successfully.
    
    Returns:
        True if transformers can be imported, False otherwise
    """
    try:
        import transformers
        logger.debug("Transformers imported successfully")
        return True
    except Exception as e:
        logger.warning(f"Transformers import failed: {e}")
        return False


def configure_cuda_environment() -> Any:
    """
    Configure CUDA environment to handle potential initialization issues.
    
    This function:
    1. Sets CUDA device visibility
    2. Checks CUDA availability
    3. Handles potential environment configuration errors
    
    Returns:
        torch.device: Configured device
    """
    try:
        import torch
        
        # Ensure consistent CUDA device visibility
        if 'CUDA_VISIBLE_DEVICES' not in os.environ:
            os.environ['CUDA_VISIBLE_DEVICES'] = '0'  # Default to first GPU
        
        # Check CUDA availability
        if torch.cuda.is_available():
            # Use the first available device
            device = torch.device('cuda:0')
            torch.cuda.set_device(device)
            print(f"Using CUDA device: {torch.cuda.get_device_name(0)}")
            logger.info(f"CUDA configured: {torch.cuda.get_device_name(0)}")
        else:
            print("CUDA not available. Falling back to CPU.")
            logger.info("CUDA not available, using CPU")
            device = torch.device('cpu')
        
        return device
    
    except ImportError:
        print("PyTorch not available. Proceeding without CUDA configuration.")
        logger.warning("PyTorch not available")
        return None
    except Exception as e:
        print(f"CUDA configuration error: {e}")
        print("Falling back to CPU.")
        logger.error(f"CUDA configuration error: {e}")
        try:
            import torch
            return torch.device('cpu')
        except ImportError:
            return None


def check_environment_dependencies() -> Dict[str, bool]:
    """
    Check availability of optional dependencies.
    
    Returns:
        Dictionary with dependency availability status
    """
    dependencies = {
        "torch": False,
        "spacy": False,
        "matplotlib": False,
        "networkx": False,
        "imageio": False,
        "openai": False,
        "transformers": False,
        "protobuf": False
    }
    
    # Check PyTorch
    try:
        import torch
        dependencies["torch"] = True
    except ImportError:
        pass
    
    # Check spaCy
    try:
        import spacy
        dependencies["spacy"] = True
    except ImportError:
        pass
    
    # Check matplotlib
    try:
        import matplotlib
        dependencies["matplotlib"] = True
    except ImportError:
        pass
    
    # Check networkx
    try:
        import networkx
        dependencies["networkx"] = True
    except ImportError:
        pass
    
    # Check imageio
    try:
        import imageio
        dependencies["imageio"] = True
    except ImportError:
        pass
    
    # Check OpenAI (for fallback LLM)
    try:
        import openai
        dependencies["openai"] = True
    except ImportError:
        pass
    
    # Check transformers (after protobuf configuration)
    try:
        configure_protobuf_environment()
        import transformers
        dependencies["transformers"] = True
    except ImportError:
        pass
    
    # Check protobuf
    try:
        import google.protobuf
        dependencies["protobuf"] = True
    except ImportError:
        pass
    
    return dependencies


def print_environment_status() -> None:
    """
    Print environment and dependency status.
    """
    print("\nLEXICON Environment Status:")
    print("=" * 40)
    
    # Check CUDA
    device = configure_cuda_environment()
    if device:
        print(f"Compute Device: {device}")
    else:
        print("Compute Device: CPU (PyTorch not available)")
    
    # Check dependencies
    deps = check_environment_dependencies()
    print("\nDependency Status:")
    for dep, available in deps.items():
        status = "✓ Available" if available else "✗ Missing"
        print(f"  {dep:12} : {status}")
    
    # Test transformers specifically
    transformers_working = test_transformers_import()
    if transformers_working:
        print("  transformers : ✓ Working")
    else:
        print("  transformers : ⚠ Import issues")
    
    # Environment variables
    print("\nEnvironment Variables:")
    openrouter_key = os.environ.get("OPENROUTER_API_KEY")
    if openrouter_key:
        print("  OPENROUTER_API_KEY : ✓ Set")
    else:
        print("  OPENROUTER_API_KEY : ✗ Not set")
    
    protobuf_impl = os.environ.get("PROTOCOL_BUFFERS_PYTHON_IMPLEMENTATION")
    if protobuf_impl:
        print(f"  PROTOBUF_IMPL      : {protobuf_impl}")
    
    print()


def validate_environment() -> bool:
    """
    Validate that the environment has minimum required dependencies.
    
    Returns:
        True if environment is valid, False otherwise
    """
    # Configure protobuf first
    configure_protobuf_environment()
    configure_transformers_environment()
    
    # Check for OpenRouter API key
    if not os.environ.get("OPENROUTER_API_KEY"):
        logger.error("OPENROUTER_API_KEY environment variable not set")
        return False
    
    # Check for basic Python dependencies (these should be in requirements.txt)
    try:
        import json
        import pathlib
        import datetime
    except ImportError as e:
        logger.error(f"Missing basic Python dependency: {e}")
        return False
    
    return True


def setup_environment_logging() -> None:
    """
    Set up environment-specific logging configuration.
    """
    # Configure protobuf and transformers environment first
    configure_protobuf_environment()
    configure_transformers_environment()
    
    # Suppress specific warnings
    import warnings
    
    # Suppress protobuf warnings
    warnings.filterwarnings(
        "ignore",
        message=".*Descriptors cannot be created directly.*",
        category=Warning
    )
    
    warnings.filterwarnings(
        "ignore", 
        message=".*protoc >= 3.19.0.*",
        category=Warning
    )
    
    # Suppress transformers warnings
    warnings.filterwarnings(
        "ignore",
        message=".*Failed to import transformers.*",
        category=Warning
    )
    
    # Suppress CUDA warnings if PyTorch is available
    try:
        import torch
        warnings.filterwarnings(
            "ignore", 
            message="CUDA initialization: CUDA unknown error",
            category=UserWarning
        )
        warnings.filterwarnings(
            "ignore", 
            message="`torch.cuda.amp.autocast(args...)` is deprecated",
            category=FutureWarning
        )
    except ImportError:
        pass
    
    # Suppress matplotlib backend warnings
    try:
        import matplotlib
        matplotlib.use('Agg')  # Use non-interactive backend
        warnings.filterwarnings(
            "ignore",
            message="Matplotlib is currently using agg",
            category=UserWarning
        )
    except ImportError:
        pass 