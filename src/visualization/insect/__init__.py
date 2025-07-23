"""
Insect Visualization Package for CEREBRUM

This package provides comprehensive visualization tools for insect cognitive models,
including case-based reasoning visualization, neural structure activity, behavioral
patterns, and simulation tracking with special emphasis on case relevance.
"""

from .insect_visualizer import (
    InsectVisualizer,
    InsectSimulationVisualizer,
    CaseRelevanceVisualizer
)
from .neural_visualizer import (
    NeuralStructureVisualizer,
    BrainActivityVisualizer
)
from .behavior_visualizer import (
    BehaviorPatternVisualizer,
    SwarmBehaviorVisualizer
)
from .case_visualizer import (
    InsectCaseVisualizer,
    CaseTransitionVisualizer,
    CaseEffectivenessVisualizer
)
from .simulation_logger import (
    InsectSimulationLogger,
    CasePerformanceLogger,
    BehavioralLogger
)
from .animation_creator import (
    InsectAnimationCreator,
    SwarmAnimationCreator
)
from .comprehensive_visualizer import ComprehensiveVisualizer

__all__ = [
    # Main visualizers
    'InsectVisualizer',
    'InsectSimulationVisualizer',
    'CaseRelevanceVisualizer',
    
    # Neural visualization
    'NeuralStructureVisualizer',
    'BrainActivityVisualizer',
    
    # Behavior visualization
    'BehaviorPatternVisualizer',
    'SwarmBehaviorVisualizer',
    
    # Case visualization
    'InsectCaseVisualizer',
    'CaseTransitionVisualizer',
    'CaseEffectivenessVisualizer',
    
    # Logging and tracking
    'InsectSimulationLogger',
    'CasePerformanceLogger',
    'BehavioralLogger',
    
    # Animation
    'InsectAnimationCreator',
    'SwarmAnimationCreator',
    
    # Comprehensive visualization
    'ComprehensiveVisualizer'
]

__version__ = "0.1.0"
__author__ = "CEREBRUM Development Team" 