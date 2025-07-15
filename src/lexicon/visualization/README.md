# LEXICON Visualization

This module provides visualization capabilities for LEXICON knowledge graphs, generating both static PNG images and animated GIF visualizations.

## Overview

The LEXICON visualization module creates several types of visualizations:

1. **Static Visualizations (PNG)**:
   - **Knowledge Graph**: A network visualization of the entire graph showing entities, claims, and their relationships
   - **Case Distribution**: A bar chart showing the distribution of grammatical cases across the graph
   - **Entity Network**: A focused view of entity relationships

2. **Animated Visualizations (GIF)**:
   - **Graph Animation**: An animation showing the construction of the knowledge graph
   - **Case Evolution**: An animation showing the evolution of cases as different node types are added

## Usage

The visualization components are automatically used by the LEXICON engine when processing text or files. You can also use them directly:

```python
from src.lexicon.visualization.graph_visualizer import GraphVisualizer
from src.lexicon.visualization.animated_graph import create_graph_animation
from pathlib import Path

# Create static visualizations
visualizer = GraphVisualizer()
visualizer.visualize_graph(graph_data, output_dir)

# Create animated visualizations
create_graph_animation(graph_data, output_dir)
```

## Requirements

The visualization module requires the following Python packages:
- matplotlib
- networkx
- numpy
- imageio (for animations)

Install these dependencies with:
```
pip install matplotlib networkx numpy imageio
```

## Visualization Types

### Knowledge Graph (PNG)

A network visualization of the entire graph showing:
- Nodes colored by type (entity, claim, segment, etc.)
- Directed edges showing relationships
- Node labels showing text content
- A legend identifying node types

### Case Distribution (PNG)

A bar chart showing:
- Distribution of the 8 grammatical cases in the graph
- Color-coded bars for each case
- Count labels

### Entity Network (PNG)

A focused network visualization showing:
- Only entity nodes and their direct relationships
- Larger nodes for better readability
- Clear labels for each entity

### Graph Animation (GIF)

An animated visualization showing:
- Progressive construction of the graph
- Nodes appearing first, followed by edges
- Evolution of the graph structure

### Case Evolution (GIF)

An animated visualization showing:
- Evolution of case distribution as different node types are added
- Progressive buildup of the case distribution
- Color-coded bars for each case

## Output Structure

Visualizations are saved in the `visualizations` subdirectory of the LEXICON output directory:

```
output/lexicon/[session_id]/
├── visualizations/
│   ├── knowledge_graph.png
│   ├── case_distribution.png
│   ├── entity_network.png
│   ├── graph_animation.gif
│   ├── case_evolution.gif
│   ├── graph_statistics.txt
│   ├── entities.txt
│   └── claims.txt
```

## Customization

The visualization appearance can be customized by modifying the following files:
- `graph_visualizer.py`: For static visualizations
- `animated_graph.py`: For animated visualizations 