"""
LEXICON Device Management

PyTorch device configuration utilities for LEXICON.
"""

import warnings

try:
    import torch as _torch  # noqa: F401
    # Suppress torch CUDA warnings only when torch is actually installed
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
    del _torch
except ImportError:
    pass


def configure_torch_environment(default_device='cuda'):
    """
    Configure PyTorch environment with robust device selection.

    Args:
        default_device (str): Preferred device type ('cuda' or 'cpu')

    Returns:
        torch.device: Configured device
    """
    import torch

    try:
        if default_device == 'cuda' and torch.cuda.is_available():
            device = torch.device('cuda')
            torch.cuda.empty_cache()
            print(f"Using CUDA device: {torch.cuda.get_device_name(0)}")
        else:
            device = torch.device('cpu')
            print("Using CPU device")

        return device

    except Exception as e:
        print(f"Device configuration error: {e}")
        return torch.device('cpu')


class DeviceContext:
    """
    Context manager for managing PyTorch device configuration.

    Example:
        with DeviceContext() as device:
            model = model.to(device)
    """

    def __init__(self, default_device='cuda'):
        self.default_device = default_device
        self.device = None

    def __enter__(self):
        self.device = configure_torch_environment(self.default_device)
        return self.device

    def __exit__(self, exc_type, exc_val, exc_tb):
        pass
