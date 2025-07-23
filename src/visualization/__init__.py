"""
CEREBRUM Visualization Package

This package provides comprehensive visualization tools for the CEREBRUM cognitive modeling framework,
including case-based reasoning visualization, neural activity patterns, behavioral dynamics,
and specialized insect model visualizations.
"""

from .case_visualization import (
    plot_model_state,
    plot_model_transition,
    plot_model_ecosystem,
    plot_free_energy_landscape,
    plot_case_transformation_cycle
)
from .case_comparison import CaseComparisonVisualizer, get_case_info
from .animal_visualization import (
    plot_animal_environment,
    plot_animal_sensory_state,
    plot_animal_path,
    create_animal_animation,
    animate_animal_navigation,
    visualize_animal_cases
)

# Import insect visualization modules
try:
    from .insect import (
        InsectVisualizer,
        InsectSimulationVisualizer,
        CaseRelevanceVisualizer,
        NeuralStructureVisualizer,
        BrainActivityVisualizer,
        BehaviorPatternVisualizer,
        SwarmBehaviorVisualizer,
        InsectCaseVisualizer,
        CaseTransitionVisualizer,
        CaseEffectivenessVisualizer,
        InsectSimulationLogger,
        CasePerformanceLogger,
        BehavioralLogger,
        InsectAnimationCreator,
        SwarmAnimationCreator
    )
    INSECT_VISUALIZATION_AVAILABLE = True
except ImportError:
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
