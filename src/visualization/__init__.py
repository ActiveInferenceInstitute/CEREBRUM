"""
CEREBRUM Visualization Package

This package provides comprehensive visualization tools for the CEREBRUM cognitive modeling framework,
including case-based reasoning visualization, neural activity patterns, behavioral dynamics,
and specialized insect model visualizations.
"""

from .animal_visualization import (
    animate_animal_navigation,
    create_animal_animation,
    plot_animal_environment,
    plot_animal_path,
    plot_animal_sensory_state,
    visualize_animal_cases,
)
from .case_comparison import CaseComparisonVisualizer, get_case_info
from .case_visualization import (
    plot_case_transformation_cycle,
    plot_free_energy_landscape,
    plot_model_ecosystem,
    plot_model_state,
    plot_model_transition,
)

# Import insect visualization modules
try:
    from .insect import (  # noqa: F401
        BehavioralLogger,
        BehaviorPatternVisualizer,
        BrainActivityVisualizer,
        CaseEffectivenessVisualizer,
        CasePerformanceLogger,
        CaseRelevanceVisualizer,
        CaseTransitionVisualizer,
        InsectAnimationCreator,
        InsectCaseVisualizer,
        InsectSimulationLogger,
        InsectSimulationVisualizer,
        InsectVisualizer,
        NeuralStructureVisualizer,
        SwarmAnimationCreator,
        SwarmBehaviorVisualizer,
    )
    INSECT_VISUALIZATION_AVAILABLE = True
except ModuleNotFoundError as _e:  # optional dependency unavailable -> graceful degrade
    # Log the missing dependency so a real bug isn't silently hidden; this is a
    # deliberate, narrow catch (ModuleNotFoundError, not bare ImportError).
    try:
        import logging
        logging.getLogger(__name__).warning(
            "Insect visualization unavailable (missing optional dependency): %s", _e
        )
    except Exception:
        pass
    INSECT_VISUALIZATION_AVAILABLE = False
except ImportError as _e:  # genuine import bug inside the insect package -> surface it
    import logging
    logging.getLogger(__name__).warning(
        "Failed to import insect visualization (internal error): %r", _e
    )
    INSECT_VISUALIZATION_AVAILABLE = False

__all__ = [
    # Core visualization functions
    'plot_model_state',
    'plot_model_transition',
    'plot_model_ecosystem',
    'plot_free_energy_landscape',
    'plot_case_transformation_cycle',
    'CaseComparisonVisualizer',
    'get_case_info',
    
    # Animal visualization
    'plot_animal_environment',
    'plot_animal_sensory_state',
    'plot_animal_path',
    'create_animal_animation',
    'animate_animal_navigation',
    'visualize_animal_cases',
    
    # Insect visualization (if available)
    'INSECT_VISUALIZATION_AVAILABLE'
]

# Add insect visualization classes if available
if INSECT_VISUALIZATION_AVAILABLE:
    __all__.extend([
        'InsectVisualizer',
        'InsectSimulationVisualizer',
        'CaseRelevanceVisualizer',
        'NeuralStructureVisualizer',
        'BrainActivityVisualizer',
        'BehaviorPatternVisualizer',
        'SwarmBehaviorVisualizer',
        'InsectCaseVisualizer',
        'CaseTransitionVisualizer',
        'CaseEffectivenessVisualizer',
        'InsectSimulationLogger',
        'CasePerformanceLogger',
        'BehavioralLogger',
        'InsectAnimationCreator',
        'SwarmAnimationCreator'
    ])

__version__ = "0.1.0"
__author__ = "CEREBRUM Development Team"
