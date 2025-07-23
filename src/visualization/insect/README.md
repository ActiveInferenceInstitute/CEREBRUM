# Insect Visualization System for CEREBRUM

## Overview

The Insect Visualization System provides comprehensive visualization tools for insect cognitive models within the CEREBRUM framework. This system integrates case-based reasoning visualization, neural structure activity tracking, behavioral pattern analysis, and simulation logging with special emphasis on linguistic case relevance and effectiveness.

## Architecture

The system is organized into several specialized modules:

### Core Visualizers

#### 1. InsectVisualizer (`insect_visualizer.py`)
- **Purpose**: Main visualization orchestrator for individual insects
- **Key Features**:
  - Case transition tracking and visualization
  - Behavioral pattern analysis
  - Neural activity heatmaps
  - Comprehensive dashboards
  - Performance metrics visualization

#### 2. InsectSimulationVisualizer
- **Purpose**: Multi-insect simulation visualization
- **Key Features**:
  - Swarm position tracking
  - Collective behavior analysis
  - Case distribution across swarm
  - Performance comparison between insects

#### 3. CaseRelevanceVisualizer
- **Purpose**: Case effectiveness and relevance analysis
- **Key Features**:
  - Context-aware case recommendations
  - Performance correlation analysis
  - Case transition optimization

### Neural Visualization

#### 1. NeuralStructureVisualizer (`neural_visualizer.py`)
- **Purpose**: Individual neural structure analysis
- **Key Features**:
  - Structure activity visualization
  - Connectivity mapping
  - Case parameter analysis
  - Performance statistics

#### 2. BrainActivityVisualizer
- **Purpose**: Whole-brain activity visualization
- **Key Features**:
  - Multi-structure activity tracking
  - Brain connectivity visualization
  - Learning dynamics analysis
  - Activity animations

### Behavioral Visualization

#### 1. BehaviorPatternVisualizer (`behavior_visualizer.py`)
- **Purpose**: Individual behavior pattern analysis
- **Key Features**:
  - Behavioral timeline visualization
  - Performance metrics analysis
  - State transition matrices
  - Energy usage tracking

#### 2. SwarmBehaviorVisualizer
- **Purpose**: Collective behavior analysis
- **Key Features**:
  - Swarm dynamics visualization
  - Cohesion and dispersion tracking
  - Collective performance analysis
  - Swarm state visualization

### Case Visualization

#### 1. InsectCaseVisualizer (`case_visualizer.py`)
- **Purpose**: Case-based reasoning visualization
- **Key Features**:
  - Case distribution analysis
  - Effectiveness evaluation
  - Context factor analysis
  - Case recommendations

#### 2. CaseTransitionVisualizer
- **Purpose**: Case transition pattern analysis
- **Key Features**:
  - Transition matrix visualization
  - Success rate analysis
  - Context correlation analysis
  - Transition optimization

#### 3. CaseEffectivenessVisualizer
- **Purpose**: Case performance evaluation
- **Key Features**:
  - Effectiveness trends
  - Performance correlation
  - Context factor analysis
  - Optimization recommendations

### Logging and Tracking

#### 1. InsectSimulationLogger (`simulation_logger.py`)
- **Purpose**: Comprehensive simulation data logging
- **Key Features**:
  - Event logging with timestamps
  - Performance metrics tracking
  - Context factor recording
  - Data export (JSON/CSV)

#### 2. CasePerformanceLogger
- **Purpose**: Specialized case performance analysis
- **Key Features**:
  - Case effectiveness metrics
  - Context correlation analysis
  - Performance recommendations
  - Detailed reporting

#### 3. BehavioralLogger
- **Purpose**: Behavioral pattern logging
- **Key Features**:
  - Behavior sequence tracking
  - Transition analysis
  - Context effectiveness
  - Pattern recognition

### Animation

#### 1. InsectAnimationCreator (`animation_creator.py`)
- **Purpose**: Individual insect animation creation
- **Key Features**:
  - Trajectory animations
  - Case transition animations
  - Neural activity animations
  - Behavioral timeline animations

#### 2. SwarmAnimationCreator
- **Purpose**: Swarm dynamics animation
- **Key Features**:
  - Swarm movement animations
  - Collective behavior animations
  - Metrics timeline animations
  - State distribution animations

## Usage Examples

### Basic Individual Insect Visualization

```python
from src.visualization.insect import InsectVisualizer, CaseRelevanceVisualizer
from src.models.insect.species import HoneybeeModel

# Initialize
honeybee = HoneybeeModel()
visualizer = InsectVisualizer()
case_visualizer = CaseRelevanceVisualizer()

# Create visualizations
fig1 = visualizer.visualize_case_transitions()
fig2 = visualizer.visualize_behavioral_patterns()
fig3 = visualizer.visualize_neural_activity()
fig4 = visualizer.create_comprehensive_dashboard(honeybee)

# Case relevance analysis
context = {'food_available': True, 'threat_level': 0.1}
fig5 = case_visualizer.visualize_case_relevance(honeybee, context)
```

### Swarm Visualization

```python
from src.visualization.insect import SwarmBehaviorVisualizer
from src.models.insect.species import AntModel

# Initialize swarm
ants = [AntModel() for _ in range(10)]
swarm_visualizer = SwarmBehaviorVisualizer()

# Track swarm state
swarm_center = np.mean([ant.position for ant in ants], axis=0)
swarm_dispersion = calculate_dispersion(ants)
swarm_cohesion = calculate_cohesion(ants)

swarm_visualizer.track_swarm_state(ants, swarm_center, swarm_dispersion, swarm_cohesion)

# Create visualizations
fig1 = swarm_visualizer.visualize_swarm_dynamics()
fig2 = swarm_visualizer.visualize_swarm_state(ants, environment)
```

### Comprehensive Logging

```python
from src.visualization.insect import (
    InsectSimulationLogger, 
    CasePerformanceLogger, 
    BehavioralLogger
)

# Initialize loggers
sim_logger = InsectSimulationLogger()
case_logger = CasePerformanceLogger()
behavior_logger = BehavioralLogger()

# Log simulation events
context = {'step': 1, 'food_available': True}
sim_logger.log_event(insect, 'foraging_action', context)

# Log case performance
case_logger.log_case_metrics(
    insect.current_case,
    insect.get_performance_summary(),
    context,
    1.0
)

# Log behavioral patterns
behavior_logger.log_behavior_context(
    'foraging',
    context,
    success=True,
    duration=1.0
)

# Export data
sim_logger.export_data('json')
case_logger.export_case_report('json')
behavior_logger.export_behavioral_report('json')
```

### Animation Creation

```python
from src.visualization.insect import InsectAnimationCreator, SwarmAnimationCreator

# Individual insect animation
insect_creator = InsectAnimationCreator()
anim1 = insect_creator.create_insect_trajectory_animation(
    insect_history,
    environment,
    duration=10.0,
    fps=10
)

# Swarm animation
swarm_creator = SwarmAnimationCreator()
anim2 = swarm_creator.create_swarm_animation(
    swarm_history,
    environment,
    duration=10.0,
    fps=10
)
```

## Data Flow

1. **Simulation Execution**: Insect models execute with case-based reasoning
2. **Event Logging**: All events, state changes, and performance metrics are logged
3. **Data Processing**: Logged data is processed for analysis and visualization
4. **Visualization Generation**: Static plots, interactive dashboards, and animations are created
5. **Analysis**: Case effectiveness, behavioral patterns, and performance trends are analyzed
6. **Export**: Data and reports are exported in various formats (JSON, CSV, PNG)

## Output Formats

### Visualizations
- **Static Plots**: High-resolution PNG images (150 DPI)
- **Dashboards**: Comprehensive multi-panel visualizations
- **Animations**: MP4/GIF animations for temporal data
- **Interactive**: Matplotlib-based interactive plots

### Data Export
- **JSON**: Structured data with metadata
- **CSV**: Tabular data for analysis
- **Reports**: Comprehensive analysis reports

### Log Files
- **Event Logs**: Detailed simulation events
- **Performance Logs**: Case and behavioral performance metrics
- **Analysis Logs**: Statistical analysis results

## Configuration

### Visualization Settings
```python
from src.visualization.insect import VisualizationConfig

config = VisualizationConfig(
    figsize=(12, 8),
    dpi=100,
    save_format='png',
    animation_fps=10,
    max_history_length=1000
)
```

### Color Schemes
- **Case Colors**: Distinct colors for each linguistic case
- **Behavioral Colors**: Color-coded behavioral states
- **Performance Colors**: Gradient colors for performance metrics

## Integration with CEREBRUM

The Insect Visualization System integrates seamlessly with the CEREBRUM framework:

1. **Case-Based Reasoning**: Visualizes linguistic case transformations and effectiveness
2. **Active Inference**: Tracks neural activity and belief updates
3. **Behavioral Modeling**: Analyzes behavioral patterns and transitions
4. **Swarm Intelligence**: Visualizes collective behavior and emergent properties

## Performance Considerations

- **Memory Management**: Configurable history limits prevent memory overflow
- **Efficient Logging**: Optimized data structures for high-frequency logging
- **Batch Processing**: Efficient handling of large datasets
- **Modular Design**: Independent components for selective use

## Future Enhancements

1. **Real-time Visualization**: Live updating dashboards
2. **3D Visualization**: Three-dimensional swarm and neural visualizations
3. **Machine Learning Integration**: Automated pattern recognition
4. **Web Interface**: Browser-based visualization interface
5. **Advanced Analytics**: Statistical analysis and predictive modeling

## Dependencies

- **Matplotlib**: Core visualization library
- **NumPy**: Numerical computations
- **Seaborn**: Statistical visualizations
- **Pandas**: Data manipulation (optional)
- **Pillow**: Image processing for animations

## Contributing

When contributing to the Insect Visualization System:

1. Follow the established modular architecture
2. Maintain consistent color schemes and styling
3. Include comprehensive documentation
4. Add appropriate error handling
5. Ensure backward compatibility
6. Include unit tests for new features

## License

This module is part of the CEREBRUM project and follows the same licensing terms. 