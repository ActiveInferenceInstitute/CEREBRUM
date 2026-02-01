"""
CEREBRUM Analysis Module

This module provides simulation assessment and analysis tools for CEREBRUM models.

Note: Some analysis features require optional dependencies (matplotlib, seaborn).
Install with: uv pip install -e ".[all]"
"""

__all__ = [
    'SimulationEffectivenessAnalyzer',
    'run_comprehensive_assessment',
]

def __getattr__(name):
    """Lazy import to avoid loading heavy dependencies at package import time."""
    if name == 'SimulationEffectivenessAnalyzer':
        from .simulation_assessment import SimulationEffectivenessAnalyzer
        return SimulationEffectivenessAnalyzer
    elif name == 'run_comprehensive_assessment':
        from .simulation_assessment import run_comprehensive_assessment
        return run_comprehensive_assessment
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")
