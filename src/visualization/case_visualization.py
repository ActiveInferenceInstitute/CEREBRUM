"""
Visualization utilities for CEREBRUM case frameworks.

This module provides functions for visualizing case-bearing models,
their transformations, and relationships in model ecosystems.
"""

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import networkx as nx
from typing import Dict, List, Any, Optional, Tuple, Union, Callable
import io
from PIL import Image

from src.core.model import Model, Case

# Define colors for each case
CASE_COLORS = {
    Case.NOMINATIVE: "#4285F4",    # Blue
    Case.ACCUSATIVE: "#EA4335",    # Red
    Case.GENITIVE: "#FBBC05",      # Yellow
    Case.DATIVE: "#34A853",        # Green
    Case.INSTRUMENTAL: "#9C27B0",  # Purple
    Case.LOCATIVE: "#FF9800",      # Orange
    Case.ABLATIVE: "#00BCD4",      # Cyan
    Case.VOCATIVE: "#795548"       # Brown
}

# Icons or symbols for each case (for use in visualization)
CASE_SYMBOLS = {
    Case.NOMINATIVE: "→",      # Arrow pointing right (active agent)
    Case.ACCUSATIVE: "↓",      # Arrow pointing down (receiving updates)
    Case.GENITIVE: "⊙",        # Source/origin
    Case.DATIVE: "⊕",          # Receiving data
    Case.INSTRUMENTAL: "⚒",    # Tool/method
    Case.LOCATIVE: "⌘",        # Context
    Case.ABLATIVE: "↑",        # Origin pointing up
    Case.VOCATIVE: "★"         # Direct address
}

def plot_model_state(
    model: Model, 
    figsize: Tuple[int, int] = (10, 6),
    show_parameters: bool = True,
    precision_plot: bool = True
) -> plt.Figure:
    """
    Visualize the current state of a model, including its case and key parameters.
    
    Args:
        model: The model to visualize
        figsize: Figure size
        show_parameters: Whether to show model parameters
        precision_plot: Whether to show precision values
        
    Returns:
        The matplotlib figure
    """
    fig, axes = plt.subplots(1, 2 if precision_plot else 1, figsize=figsize)
    
    if not precision_plot:
        axes = [axes]
    
    # Main state plot
    ax = axes[0]
    ax.set_title(f"Model: {model.name} [{model.case.value}]")
    
    # Base rectangle representing the model
    rect = patches.Rectangle(
        (0.1, 0.1), 0.8, 0.8, 
        linewidth=2, 
        edgecolor=CASE_COLORS[model.case],
        facecolor=CASE_COLORS[model.case],
        alpha=0.3
    )
    ax.add_patch(rect)
    
    # Add case symbol
    ax.text(
        0.5, 0.5, 
        CASE_SYMBOLS[model.case], 
        fontsize=50, 
        ha='center', 
        va='center',
        color=CASE_COLORS[model.case]
    )
    
    # Add case label
    ax.text(
        0.5, 0.9, 
        f"Case: {model.case.name}", 
        fontsize=14, 
        ha='center', 
        va='center',
        bbox=dict(facecolor='white', alpha=0.8, boxstyle='round,pad=0.3')
    )
    
    # Add parameter info if available and requested
    if hasattr(model, "parameters") and show_parameters:
        param_str = ""
        
        if hasattr(model, "n_states"):
            param_str += f"States: {model.n_states}\n"
        
        if hasattr(model, "n_observations"):
            param_str += f"Observations: {model.n_observations}\n"
        
        # Add active inference specific parameters
        if hasattr(model, "posterior_means") and hasattr(model, "prior_means"):
            param_str += f"Prior-Posterior KL: {np.round(np.sum((model.posterior_means - model.prior_means)**2), 4)}\n"
        
        if hasattr(model, "free_energy_history") and len(model.free_energy_history) > 0:
            param_str += f"Free Energy: {np.round(model.free_energy_history[-1], 4)}"
        
        ax.text(
            0.5, 0.2, 
            param_str, 
            fontsize=10, 
            ha='center', 
            va='center',
            bbox=dict(facecolor='white', alpha=0.8, boxstyle='round,pad=0.3')
        )
    
    # Remove axes
    ax.set_xlim(0, 1)
    ax.set_ylim(0, 1)
    ax.axis('off')
    
    # Plot precision values if requested
    if precision_plot:
        ax = axes[1]
        ax.set_title("Case Precision Weights")
        
        # Get precision values for each case
        cases = list(Case)
        precision_values = [model.get_precision(case) for case in cases]
        case_names = [case.name for case in cases]
        
        # Create bar plot
        bars = ax.bar(
            case_names, 
            precision_values,
            color=[CASE_COLORS[case] for case in cases]
        )
        
        # Highlight current case
        current_idx = cases.index(model.case)
        bars[current_idx].set_edgecolor('black')
        bars[current_idx].set_linewidth(2)
        
        ax.set_ylabel("Precision")
        ax.set_ylim(0, max(precision_values) * 1.2)
        
        # Rotate x-axis labels for readability
        plt.setp(ax.get_xticklabels(), rotation=45, ha='right')
    
    plt.tight_layout()
    return fig

def plot_model_transition(
    model: Model,
    from_case: Case,
    to_case: Case,
    figsize: Tuple[int, int] = (12, 6)
) -> plt.Figure:
    """
    Visualize a case transition between two states of a model.
    
    Args:
        model: The model to visualize
        from_case: The original case
        to_case: The target case
        figsize: Figure size
        
    Returns:
        The matplotlib figure
    """
    fig, (ax1, ax2, ax3) = plt.subplots(1, 3, figsize=figsize)
    
    # Original state
    ax1.set_title(f"Original: [{from_case.value}]")
    rect1 = patches.Rectangle(
        (0.1, 0.1), 0.8, 0.8, 
        linewidth=2, 
        edgecolor=CASE_COLORS[from_case],
        facecolor=CASE_COLORS[from_case],
        alpha=0.3
    )
    ax1.add_patch(rect1)
    ax1.text(
        0.5, 0.5, 
        CASE_SYMBOLS[from_case], 
        fontsize=50, 
        ha='center', 
        va='center',
        color=CASE_COLORS[from_case]
    )
    ax1.set_xlim(0, 1)
    ax1.set_ylim(0, 1)
    ax1.axis('off')
    
    # Transition arrow
    ax2.set_title("Case Transformation")
    ax2.arrow(
        0.1, 0.5, 0.8, 0,
        head_width=0.1, head_length=0.1,
        linewidth=3, color='black'
    )
    ax2.text(
        0.5, 0.7,
        f"{from_case.value} → {to_case.value}",
        fontsize=12, ha='center', va='center',
        bbox=dict(facecolor='white', alpha=0.8, boxstyle='round,pad=0.3')
    )
    
    # Add transformation properties
    if hasattr(model, "_case_configurations"):
        from_config = model._case_configurations[from_case]
        to_config = model._case_configurations[to_case]
        
        # Compare interface modes
        ax2.text(
            0.5, 0.3,
            f"Interface: {from_config.get('interface_mode', 'N/A')} → {to_config.get('interface_mode', 'N/A')}",
            fontsize=8, ha='center', va='center'
        )
        
        # Compare precision focus
        ax2.text(
            0.5, 0.2,
            f"Precision: {from_config.get('precision_focus', 'N/A')} → {to_config.get('precision_focus', 'N/A')}",
            fontsize=8, ha='center', va='center'
        )
    
    ax2.set_xlim(0, 1)
    ax2.set_ylim(0, 1)
    ax2.axis('off')
    
    # Target state
    ax3.set_title(f"Target: [{to_case.value}]")
    rect3 = patches.Rectangle(
        (0.1, 0.1), 0.8, 0.8, 
        linewidth=2, 
        edgecolor=CASE_COLORS[to_case],
        facecolor=CASE_COLORS[to_case],
        alpha=0.3
    )
    ax3.add_patch(rect3)
    ax3.text(
        0.5, 0.5, 
        CASE_SYMBOLS[to_case], 
        fontsize=50, 
        ha='center', 
        va='center',
        color=CASE_COLORS[to_case]
    )
    ax3.set_xlim(0, 1)
    ax3.set_ylim(0, 1)
    ax3.axis('off')
    
    plt.suptitle(f"Case Transformation: {model.name}")
    plt.tight_layout()
    return fig

def plot_model_ecosystem(
    models: List[Model],
    relationships: List[Tuple[Model, Model, str]] = None,
    figsize: Tuple[int, int] = (12, 10),
    layout: str = "spring"
) -> plt.Figure:
    """
    Visualize a model ecosystem with case relationships.
    
    Args:
        models: List of models in the ecosystem
        relationships: Optional list of explicit relationships between models
                      as (source, target, relationship_type) tuples
        figsize: Figure size
        layout: Graph layout algorithm ('spring', 'circular', 'spectral', etc.)
        
    Returns:
        The matplotlib figure
    """
    G = nx.DiGraph()
    
    # Add nodes for each model
    for model in models:
        G.add_node(
            model.name, 
            case=model.case.name,
            color=CASE_COLORS[model.case],
            symbol=CASE_SYMBOLS[model.case]
        )
    
    # Add edges for relationships
    if relationships:
        for source, target, rel_type in relationships:
            G.add_edge(
                source.name, 
                target.name, 
                relationship=rel_type
            )
    
    # Add edges from model connections if no explicit relationships
    if not relationships:
        for model in models:
            for target, rel_type in model.connections:
                if target.name in [m.name for m in models]:
                    G.add_edge(
                        model.name,
                        target.name,
                        relationship=rel_type
                    )
    
    # Create figure
    fig, ax = plt.subplots(figsize=figsize)
    
    # Apply layout
    if layout == "spring":
        pos = nx.spring_layout(G, seed=42)
    elif layout == "circular":
        pos = nx.circular_layout(G)
    elif layout == "spectral":
        pos = nx.spectral_layout(G)
    else:
        pos = nx.spring_layout(G)
    
    # Draw nodes colored by case
    for node, attrs in G.nodes(data=True):
        nx.draw_networkx_nodes(
            G, pos, 
            nodelist=[node], 
            node_color=attrs['color'],
            node_size=1000,
            alpha=0.7
        )
    
    # Draw node symbols
    for node, attrs in G.nodes(data=True):
        x, y = pos[node]
        plt.text(
            x, y, 
            attrs['symbol'], 
            fontsize=20, 
            ha='center', 
            va='center',
            color='black'
        )
    
    # Draw edges with arrows
    edges = G.edges(data=True)
    nx.draw_networkx_edges(
        G, pos, 
        edgelist=[(u, v) for u, v, d in edges],
        width=1.5, 
        alpha=0.7, 
        edge_color='black',
        arrows=True,
        arrowstyle='-|>',
        arrowsize=15
    )
    
    # Draw node labels
    nx.draw_networkx_labels(
        G, pos,
        font_size=10,
        font_family='sans-serif'
    )
    
    # Draw edge labels (relationship types)
    edge_labels = {(u, v): d['relationship'] for u, v, d in edges}
    nx.draw_networkx_edge_labels(
        G, pos,
        edge_labels=edge_labels,
        font_size=8
    )
    
    # Add legend for cases
    handles = []
    for case in Case:
        handle = patches.Patch(
            color=CASE_COLORS[case], 
            alpha=0.7,
            label=f"{case.name} [{case.value}] {CASE_SYMBOLS[case]}"
        )
        handles.append(handle)
    
    plt.legend(
        handles=handles, 
        title="Cases",
        loc='upper left', 
        bbox_to_anchor=(1, 1),
        fontsize=8
    )
    
    plt.title("Model Ecosystem with Case Relationships")
    plt.axis('off')
    plt.tight_layout()
    return fig

def plot_free_energy_landscape(
    model,
    param1_range: Tuple[float, float] = (-2, 2),
    param2_range: Tuple[float, float] = (-2, 2),
    resolution: int = 50,
    observations: np.ndarray = None,
    figsize: Tuple[int, int] = (10, 8),
    show_current: bool = True
) -> plt.Figure:
    """
    Visualize the free energy landscape of a model across two parameter dimensions.
    
    Args:
        model: The model to visualize
        param1_range: Range for the first parameter dimension
        param2_range: Range for the second parameter dimension
        resolution: Grid resolution for sampling
        observations: Optional observations to use for free energy calculation
        figsize: Figure size
        show_current: Whether to highlight the current parameter values
        
    Returns:
        The matplotlib figure
    """
    # Ensure the model has the required active inference methods
    if not hasattr(model, "free_energy") or not callable(getattr(model, "free_energy")):
        raise ValueError("Model must implement free_energy method")
    
    if not hasattr(model, "posterior_means") or not hasattr(model, "prior_means"):
        raise ValueError("Model must have posterior_means and prior_means attributes")
    
    # We'll focus on just the first two dimensions for visualization
    n_dims = min(2, len(model.posterior_means))
    
    # Save original model state
    original_posterior = model.posterior_means.copy()
    
    # Create parameter grid
    p1 = np.linspace(param1_range[0], param1_range[1], resolution)
    p2 = np.linspace(param2_range[0], param2_range[1], resolution) if n_dims > 1 else [0]
    P1, P2 = np.meshgrid(p1, p2)
    FE = np.zeros_like(P1)
    
    # Compute free energy across the grid
    for i in range(resolution):
        for j in range(resolution):
            # Set model parameters
            if n_dims == 1:
                model.posterior_means[0] = P1[i, j]
            else:
                model.posterior_means[0] = P1[i, j]
                model.posterior_means[1] = P2[i, j]
            
            # Calculate free energy
            FE[i, j] = model.free_energy(observations)
    
    # Create figure
    fig = plt.figure(figsize=figsize)
    
    if n_dims == 1:
        # 1D visualization
        ax = fig.add_subplot(111)
        ax.plot(p1, FE[:, 0], 'b-', linewidth=2)
        
        # Mark original position
        if show_current:
            ax.plot(original_posterior[0], model.free_energy(observations), 'ro', markersize=8)
        
        ax.set_xlabel("Parameter 1")
        ax.set_ylabel("Free Energy")
        
    else:
        # 3D visualization
        ax = fig.add_subplot(111, projection='3d')
        surf = ax.plot_surface(P1, P2, FE, cmap='viridis', alpha=0.8)
        
        # Mark original position
        if show_current:
            fe = model.free_energy(observations)
            ax.scatter(
                original_posterior[0], 
                original_posterior[1], 
                fe,
                color='red', 
                s=100, 
                marker='o'
            )
        
        ax.set_xlabel("Parameter 1")
        ax.set_ylabel("Parameter 2")
        ax.set_zlabel("Free Energy")
        
        # Add colorbar
        fig.colorbar(surf, shrink=0.5, aspect=5)
    
    # Restore original model state
    model.posterior_means = original_posterior
    
    plt.title(f"Free Energy Landscape - {model.name} [{model.case.value}]")
    return fig

def plot_case_transformation_cycle(
    model: Model, 
    case_sequence: List[Case],
    figsize: Tuple[int, int] = (12, 10)
) -> plt.Figure:
    """
    Visualize a complete cycle of case transformations.
    
    Args:
        model: The model to visualize
        case_sequence: Sequence of cases to visualize
        figsize: Figure size
        
    Returns:
        The matplotlib figure
    """
    n_cases = len(case_sequence)
    
    # Determine layout
    if n_cases <= 4:
        rows, cols = 1, n_cases
    else:
        rows = (n_cases + 3) // 4  # Ceiling division
        cols = min(4, n_cases)
    
    fig, axes = plt.subplots(rows, cols, figsize=figsize)
    if rows == 1 and cols == 1:
        axes = [axes]
    elif rows == 1 or cols == 1:
        axes = axes.flat
    
    # Save original case to restore later
    original_case = model.case
    
    # Plot each case in the sequence
    for i, case in enumerate(case_sequence):
        if i < len(axes):
            ax = axes[i]
            
            # Transform model to this case
            model.case = case
            
            # Create rectangle for this case
            rect = patches.Rectangle(
                (0.1, 0.1), 0.8, 0.8, 
                linewidth=2, 
                edgecolor=CASE_COLORS[case],
                facecolor=CASE_COLORS[case],
                alpha=0.3
            )
            ax.add_patch(rect)
            
            # Add case symbol
            ax.text(
                0.5, 0.5, 
                CASE_SYMBOLS[case], 
                fontsize=40, 
                ha='center', 
                va='center',
                color=CASE_COLORS[case]
            )
            
            # Add case label
            ax.text(
                0.5, 0.9, 
                f"Case: {case.name}", 
                fontsize=12, 
                ha='center', 
                va='center',
                bbox=dict(facecolor='white', alpha=0.8, boxstyle='round,pad=0.3')
            )
            
            # Add case-specific properties
            if hasattr(model, "_case_configurations"):
                config = model._case_configurations[case]
                ax.text(
                    0.5, 0.15, 
                    f"Focus: {config.get('precision_focus', 'N/A')}\n"
                    f"Interface: {config.get('interface_mode', 'N/A')}",
                    fontsize=8, 
                    ha='center', 
                    va='center',
                    bbox=dict(facecolor='white', alpha=0.8, boxstyle='round,pad=0.3')
                )
            
            # Add arrow to next case (if not the last one)
            if i < len(case_sequence) - 1:
                next_case = case_sequence[(i + 1) % len(case_sequence)]
                ax.arrow(
                    0.9, 0.5, 0.1, 0,
                    head_width=0.05, head_length=0.05,
                    linewidth=2, color='black',
                    transform=ax.transAxes, 
                    clip_on=False,
                    length_includes_head=True,
                    zorder=10
                )
                
                # For the last in a row, connect to the start of next row
                if (i + 1) % cols == 0 and (i + 1) < len(case_sequence):
                    # Draw a curved arrow or some indication of continuation
                    ax.text(0.95, 0.3, "↴", fontsize=20, color='black')
            
            # Remove axes
            ax.set_xlim(0, 1)
            ax.set_ylim(0, 1)
            ax.axis('off')
    
    # Restore original case
    model.case = original_case
    
    # Hide any unused axes
    for i in range(len(case_sequence), len(axes)):
        axes[i].axis('off')
    
    plt.suptitle(f"Case Transformation Cycle: {model.name}")
    plt.tight_layout()
    return fig 