"""
Neural Structure Visualizer for Insect Models

This module provides visualization tools for insect neural structures,
including brain activity patterns, neural connectivity, and learning dynamics.
"""

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib.patches import Circle, Rectangle, Polygon
from matplotlib.collections import LineCollection
import networkx as nx
from typing import Dict, Any, List, Optional, Tuple, Union
import logging
import os
import time
from collections import deque

from src.core.model import Case
from src.models.insect.neural_structures import NeuralStructure
from src.models.insect.base import InsectModel

logger = logging.getLogger(__name__)


class NeuralStructureVisualizer:
    """
    Visualizer for individual neural structures in insect models.
    """
    
    def __init__(self, figsize: Tuple[int, int] = (12, 8), dpi: int = 100):
        """
        Initialize the neural structure visualizer.
        
        Args:
            figsize: Figure size
            dpi: DPI for figures
        """
        self.figsize = figsize
        self.dpi = dpi
        self.activity_history = {}
        self.connection_history = {}
        
        logger.info("Initialized NeuralStructureVisualizer")
        
    def track_activity(self, insect: InsectModel, event_data: Dict[str, Any]):
        """
        Track neural activity data.
        
        Args:
            insect: The insect model
            event_data: Event data from simulation
        """
        # Extract neural activity from the event data or insect model
        neural_activity = {}
        
        # Get activity from neural structures
        for structure_name, structure in insect.neural_structure.items():
            if hasattr(structure, 'activity') and isinstance(structure.activity, np.ndarray):
                neural_activity[structure_name] = structure.activity.copy()
            else:
                neural_activity[structure_name] = np.zeros(10)  # Default
        
        # Store the activity data
        if not hasattr(self, 'activity_history'):
            self.activity_history = deque(maxlen=1000)
        elif not isinstance(self.activity_history, deque):
            self.activity_history = deque(maxlen=1000)
        
        entry = {
            'timestamp': event_data.get('timestamp', time.time()),
            'step': event_data.get('step', 0),
            'insect_id': event_data.get('insect_id', 'unknown'),
            'neural_activity': neural_activity,
            'case': insect.current_case,
            'confidence': event_data.get('processed_data', {}).get('confidence', 0.0)
        }
        
        self.activity_history.append(entry)
    
    def visualize_structure_activity(self, structure, 
                                   save_path: Optional[str] = None) -> plt.Figure:
        """
        Visualize the activity of a neural structure.
        
        Args:
            structure: The neural structure to visualize
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        fig, axes = plt.subplots(2, 2, figsize=self.figsize, dpi=self.dpi)
        
        # Get current activity - handle both NeuralStructure and NeuralStructureProcessor
        if hasattr(structure, 'get_activity'):
            activity = structure.get_activity()
        elif hasattr(structure, 'activity'):
            activity = structure.activity
        else:
            activity = np.zeros(10)  # Default
        
        # Ensure activity is a numpy array with proper data type
        if not isinstance(activity, np.ndarray):
            try:
                activity = np.array(activity, dtype=float)
            except (ValueError, TypeError):
                # If conversion fails, create synthetic activity data
                activity = np.random.rand(10)
        elif activity.dtype.kind in ['U', 'S']:  # String types
            # Convert string activity to numeric or use synthetic data
            try:
                activity = np.array([float(x) if isinstance(x, str) and x.replace('.', '').replace('-', '').isdigit() else 0.0 for x in activity], dtype=float)
            except (ValueError, TypeError):
                activity = np.random.rand(len(activity))
        elif activity.dtype.kind == 'i':  # Integer types
            activity = activity.astype(float)
        
        # Plot activity distribution
        axes[0, 0].hist(activity.flatten(), bins=20, alpha=0.7, color='skyblue')
        axes[0, 0].set_title('Activity Distribution')
        axes[0, 0].set_xlabel('Activity Level')
        axes[0, 0].set_ylabel('Frequency')
        
        # Plot activity heatmap
        if activity.ndim > 1:
            im = axes[0, 1].imshow(activity, cmap='viridis', aspect='auto')
            axes[0, 1].set_title('Activity Heatmap')
            plt.colorbar(im, ax=axes[0, 1])
        else:
            axes[0, 1].plot(activity, 'o-', linewidth=2, markersize=4)
            axes[0, 1].set_title('Activity Pattern')
            axes[0, 1].set_xlabel('Neuron Index')
            axes[0, 1].set_ylabel('Activity')
        
        # Plot case-specific parameters
        self._plot_case_parameters(axes[1, 0], structure)
        
        # Plot structure statistics
        self._plot_structure_stats(axes[1, 1], structure)
        
        # Get structure type - handle both NeuralStructure and NeuralStructureProcessor
        if hasattr(structure, 'structure_type'):
            structure_type = structure.structure_type
        elif hasattr(structure, 'case_assignment'):
            structure_type = structure.case_assignment.value
        else:
            structure_type = "Unknown"
        
        plt.suptitle(f'Neural Structure: {structure_type}', fontsize=14)
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        
        return fig
    
    def _plot_case_parameters(self, ax: plt.Axes, structure: NeuralStructure):
        """Plot case-specific parameters."""
        if hasattr(structure, 'case_parameters'):
            params = structure.case_parameters
            if params:
                param_names = list(params.keys())
                param_values = list(params.values())
                
                bars = ax.bar(range(len(param_names)), param_values, color='lightcoral', alpha=0.7)
                ax.set_title('Case Parameters')
                ax.set_ylabel('Value')
                ax.set_xticks(range(len(param_names)))
                ax.set_xticklabels(param_names, rotation=45, ha='right')
        else:
            ax.text(0.5, 0.5, 'No case parameters', ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Case Parameters')
    
    def _plot_structure_stats(self, ax: plt.Axes, structure):
        """Plot structure statistics."""
        # Handle both NeuralStructure and NeuralStructureProcessor
        if hasattr(structure, 'input_dim'):
            input_dim = structure.input_dim
        else:
            input_dim = 0
            
        if hasattr(structure, 'output_dim'):
            output_dim = structure.output_dim
        else:
            output_dim = 0
            
        if hasattr(structure, 'case_assignment'):
            case = structure.case_assignment.value
        else:
            case = "Unknown"
            
        if hasattr(structure, 'input_connections') and hasattr(structure, 'output_connections'):
            connections = len(structure.input_connections) + len(structure.output_connections)
        else:
            connections = 0
            
        # Separate numeric and text stats
        numeric_stats = {
            'Input Dim': input_dim,
            'Output Dim': output_dim,
            'Connections': connections
        }
        
        # Plot numeric stats
        if numeric_stats:
            stat_names = list(numeric_stats.keys())
            stat_values = list(numeric_stats.values())
            
            bars = ax.bar(range(len(stat_names)), stat_values, color='lightgreen', alpha=0.7)
            ax.set_title('Structure Statistics')
            ax.set_ylabel('Value')
            ax.set_xticks(range(len(stat_names)))
            ax.set_xticklabels(stat_names, rotation=45, ha='right')
            
            # Add case information as text
            ax.text(0.02, 0.98, f'Case: {case}', transform=ax.transAxes, 
                   verticalalignment='top', bbox=dict(boxstyle='round', facecolor='lightblue', alpha=0.5))
        else:
            ax.text(0.5, 0.5, 'No statistics available', ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Structure Statistics')
    
    def visualize_connectivity(self, structure: NeuralStructure, 
                             save_path: Optional[str] = None) -> plt.Figure:
        """
        Visualize the connectivity of a neural structure.
        
        Args:
            structure: The neural structure to visualize
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        fig, ax = plt.subplots(figsize=self.figsize, dpi=self.dpi)
        
        # Create network graph
        G = nx.DiGraph()
        
        # Add nodes for input, structure, and output
        G.add_node('input', pos=(0, 0.5))
        G.add_node('structure', pos=(0.5, 0.5))
        G.add_node('output', pos=(1, 0.5))
        
        # Add connections
        if structure.input_connections:
            for conn in structure.input_connections:
                G.add_edge('input', 'structure', weight=conn.weight, type=conn.connection_type)
        
        if structure.output_connections:
            for conn in structure.output_connections:
                G.add_edge('structure', 'output', weight=conn.weight, type=conn.connection_type)
        
        # Draw the network
        pos = nx.get_node_attributes(G, 'pos')
        nx.draw_networkx_nodes(G, pos, node_color='lightblue', node_size=1000, ax=ax)
        nx.draw_networkx_labels(G, pos, ax=ax)
        
        # Draw edges with different colors for different types
        edge_colors = []
        edge_weights = []
        for u, v, data in G.edges(data=True):
            if data.get('type') == 'excitatory':
                edge_colors.append('green')
            elif data.get('type') == 'inhibitory':
                edge_colors.append('red')
            else:
                edge_colors.append('blue')
            edge_weights.append(data.get('weight', 1.0) * 2)
        
        nx.draw_networkx_edges(G, pos, edge_color=edge_colors, width=edge_weights, ax=ax)
        
        ax.set_title(f'Connectivity: {structure.structure_type}')
        ax.set_xlim(-0.1, 1.1)
        ax.set_ylim(-0.1, 1.1)
        ax.axis('off')
        
        if save_path:
            plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        
        return fig
    
    def generate_comprehensive_visualizations(self, output_dir: str):
        """
        Generate comprehensive neural activity visualizations.
        
        Args:
            output_dir: Output directory for visualizations
        """
        neural_dir = os.path.join(output_dir, "visualizations", "neural_activity")
        os.makedirs(neural_dir, exist_ok=True)
        
        if not hasattr(self, 'activity_history') or not self.activity_history:
            print("    No neural activity data available for visualization")
            return
        
        # Generate neural activity comprehensive analysis
        fig, axes = plt.subplots(2, 2, figsize=(15, 12))
        fig.suptitle('Comprehensive Neural Activity Analysis', fontsize=16)
        
        # Extract data from activity history
        steps = [entry['step'] for entry in self.activity_history]
        confidences = [entry['confidence'] for entry in self.activity_history]
        
        # Plot confidence over time
        axes[0, 0].plot(steps, confidences, 'b-', linewidth=2)
        axes[0, 0].set_title('Confidence Over Time')
        axes[0, 0].set_xlabel('Simulation Step')
        axes[0, 0].set_ylabel('Confidence')
        axes[0, 0].grid(True, alpha=0.3)
        
        # Plot neural activity heatmap
        if len(self.activity_history) > 0:
            # Get the latest neural activity
            latest_activity = self.activity_history[-1]['neural_activity']
            if latest_activity:
                # Create a matrix of neural activities
                activity_matrix = []
                structure_names = []
                for structure_name, activity in latest_activity.items():
                    if isinstance(activity, np.ndarray):
                        activity_matrix.append(activity)
                        structure_names.append(structure_name)
                
                if activity_matrix:
                    # Handle inhomogeneous shapes by padding or truncating
                    max_length = max(len(arr) for arr in activity_matrix)
                    padded_matrix = []
                    for arr in activity_matrix:
                        if len(arr) < max_length:
                            # Pad with zeros
                            padded = np.zeros(max_length)
                            padded[:len(arr)] = arr
                            padded_matrix.append(padded)
                        else:
                            # Truncate to max_length
                            padded_matrix.append(arr[:max_length])
                    
                    activity_matrix = np.array(padded_matrix)
                    im = axes[0, 1].imshow(activity_matrix, cmap='viridis', aspect='auto')
                    axes[0, 1].set_title('Neural Activity Heatmap')
                    axes[0, 1].set_yticks(range(len(structure_names)))
                    axes[0, 1].set_yticklabels(structure_names)
                    plt.colorbar(im, ax=axes[0, 1])
        
        # Plot case distribution
        cases = [entry['case'].value for entry in self.activity_history]
        case_counts = {}
        for case in cases:
            case_counts[case] = case_counts.get(case, 0) + 1
        
        if case_counts:
            case_names = list(case_counts.keys())
            case_values = list(case_counts.values())
            axes[1, 0].bar(case_names, case_values, color='lightcoral', alpha=0.7)
            axes[1, 0].set_title('Case Distribution')
            axes[1, 0].set_ylabel('Frequency')
            axes[1, 0].tick_params(axis='x', rotation=45)
        
        # Plot activity statistics
        if len(confidences) > 0:
            axes[1, 1].hist(confidences, bins=20, alpha=0.7, color='skyblue', edgecolor='black')
            axes[1, 1].set_title('Confidence Distribution')
            axes[1, 1].set_xlabel('Confidence')
            axes[1, 1].set_ylabel('Frequency')
            axes[1, 1].axvline(np.mean(confidences), color='red', linestyle='--', label=f'Mean: {np.mean(confidences):.3f}')
            axes[1, 1].legend()
        
        plt.tight_layout()
        plt.savefig(os.path.join(neural_dir, 'neural_activity_comprehensive.png'), dpi=300, bbox_inches='tight')
        plt.close(fig)
        
        # Generate learning dynamics analysis
        fig, ax = plt.subplots(figsize=(12, 8))
        ax.plot(steps, confidences, 'g-', linewidth=2, label='Learning Progress')
        ax.set_title('Learning Dynamics Analysis')
        ax.set_xlabel('Simulation Step')
        ax.set_ylabel('Confidence')
        ax.grid(True, alpha=0.3)
        ax.legend()
        
        # Add trend line
        if len(steps) > 1:
            z = np.polyfit(steps, confidences, 1)
            p = np.poly1d(z)
            ax.plot(steps, p(steps), "r--", alpha=0.8, label=f'Trend (slope: {z[0]:.4f})')
            ax.legend()
        
        plt.tight_layout()
        plt.savefig(os.path.join(neural_dir, 'learning_dynamics_analysis.png'), dpi=300, bbox_inches='tight')
        plt.close(fig)
        
        # Generate neural activity animation
        if len(self.activity_history) > 10:
            fig, ax = plt.subplots(figsize=(10, 8))
            
            def animate(frame):
                ax.clear()
                if frame < len(self.activity_history):
                    entry = self.activity_history[frame]
                    activity = entry['neural_activity']
                    
                    if activity:
                        # Plot the first neural structure activity
                        first_structure = list(activity.keys())[0]
                        first_activity = activity[first_structure]
                        if isinstance(first_activity, np.ndarray):
                            ax.plot(first_activity, 'b-', linewidth=2)
                            ax.set_title(f'Neural Activity - {first_structure} (Step {entry["step"]})')
                            ax.set_xlabel('Neuron Index')
                            ax.set_ylabel('Activity')
                            ax.grid(True, alpha=0.3)
            
            anim = animation.FuncAnimation(fig, animate, frames=min(len(self.activity_history), 50), 
                                         interval=200, repeat=True)
            anim.save(os.path.join(neural_dir, 'neural_activity_animation.gif'), writer='pillow')
            plt.close(fig)
        
        print(f"    Generated neural activity visualizations in {neural_dir}")


class BrainActivityVisualizer:
    """
    Visualizer for overall brain activity patterns in insect models.
    """
    
    def __init__(self, figsize: Tuple[int, int] = (15, 10), dpi: int = 100):
        """
        Initialize the brain activity visualizer.
        
        Args:
            figsize: Figure size
            dpi: DPI for figures
        """
        self.figsize = figsize
        self.dpi = dpi
        self.activity_history = {}
        
        logger.info("Initialized BrainActivityVisualizer")
    
    def visualize_brain_activity(self, insect, 
                               save_path: Optional[str] = None) -> plt.Figure:
        """
        Visualize the overall brain activity of an insect.
        
        Args:
            insect: The insect model
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        # Check for neural structures - handle both neural_structures and neural_structure
        if hasattr(insect, 'neural_structures'):
            structures = insect.neural_structures
        elif hasattr(insect, 'neural_structure'):
            structures = insect.neural_structure
        else:
            fig, ax = plt.subplots(figsize=self.figsize, dpi=self.dpi)
            ax.text(0.5, 0.5, 'No neural structures available', ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Brain Activity')
            return fig
        
        if not structures:
            fig, ax = plt.subplots(figsize=self.figsize, dpi=self.dpi)
            ax.text(0.5, 0.5, 'No neural structures available', ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Brain Activity')
            return fig
        
        fig, axes = plt.subplots(2, 3, figsize=self.figsize, dpi=self.dpi)
        axes = axes.flatten()
        
        # Plot activity for each neural structure
        for i, (name, structure) in enumerate(structures.items()):
            if i >= len(axes):
                break
            
            ax = axes[i]
            self._plot_structure_activity(ax, structure, name)
        
        # Hide unused subplots
        for i in range(len(structures), len(axes)):
            axes[i].set_visible(False)
        
        # Get insect species name
        if hasattr(insect, 'species'):
            species = insect.species
        elif hasattr(insect, '__class__'):
            species = insect.__class__.__name__
        else:
            species = "Unknown"
            
        plt.suptitle(f'Brain Activity: {species}', fontsize=16)
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        
        return fig
    
    def _plot_structure_activity(self, ax: plt.Axes, structure, name: str):
        """Plot activity for a single neural structure."""
        # Handle both NeuralStructure and NeuralStructureProcessor
        if hasattr(structure, 'get_activity'):
            activity = structure.get_activity()
        elif hasattr(structure, 'activity'):
            activity = structure.activity
        else:
            ax.text(0.5, 0.5, 'No activity data', ha='center', va='center', transform=ax.transAxes)
            ax.set_title(f'{name}')
            return
            
        if activity.ndim > 1:
            # 2D activity - show as heatmap
            im = ax.imshow(activity, cmap='viridis', aspect='auto')
            ax.set_title(f'{name}')
            plt.colorbar(im, ax=ax)
        else:
            # 1D activity - show as line plot
            ax.plot(activity, 'o-', linewidth=2, markersize=3)
            ax.set_title(f'{name}')
            ax.set_xlabel('Neuron')
            ax.set_ylabel('Activity')
            ax.grid(True, alpha=0.3)
    
    def visualize_brain_connectivity(self, insect: InsectModel, 
                                   save_path: Optional[str] = None) -> plt.Figure:
        """
        Visualize the connectivity between neural structures.
        
        Args:
            insect: The insect model
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        if not hasattr(insect, 'neural_structures') or not insect.neural_structures:
            fig, ax = plt.subplots(figsize=self.figsize, dpi=self.dpi)
            ax.text(0.5, 0.5, 'No neural structures available', ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Brain Connectivity')
            return fig
        
        fig, ax = plt.subplots(figsize=self.figsize, dpi=self.dpi)
        
        # Create network graph
        G = nx.DiGraph()
        
        # Add nodes for each neural structure
        structures = list(insect.neural_structures.keys())
        for i, structure_name in enumerate(structures):
            G.add_node(structure_name, pos=(i, 0))
        
        # Add edges based on connections
        for structure_name, structure in insect.neural_structures.items():
            if hasattr(structure, 'output_connections'):
                for conn in structure.output_connections:
                    if conn.target in structures:
                        G.add_edge(structure_name, conn.target, weight=conn.weight)
        
        # Draw the network
        pos = nx.get_node_attributes(G, 'pos')
        nx.draw_networkx_nodes(G, pos, node_color='lightblue', node_size=2000, ax=ax)
        nx.draw_networkx_labels(G, pos, ax=ax, font_size=8)
        
        # Draw edges
        edge_weights = [G[u][v]['weight'] * 2 for u, v in G.edges()]
        nx.draw_networkx_edges(G, pos, width=edge_weights, ax=ax, edge_color='gray')
        
        ax.set_title(f'Brain Connectivity: {insect.species}')
        ax.set_xlim(-0.5, len(structures) - 0.5)
        ax.set_ylim(-0.5, 0.5)
        ax.axis('off')
        
        if save_path:
            plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        
        return fig
    
    def create_activity_animation(self, insect: InsectModel, 
                                duration: float = 10.0,
                                fps: int = 10,
                                save_path: Optional[str] = None) -> animation.Animation:
        """
        Create an animation of brain activity over time.
        
        Args:
            insect: The insect model
            duration: Animation duration in seconds
            fps: Frames per second
            save_path: Optional path to save the animation
            
        Returns:
            Matplotlib animation
        """
        if not hasattr(insect, 'neural_structures') or not insect.neural_structures:
            logger.warning("No neural structures available for animation")
            return None
        
        fig, axes = plt.subplots(2, 3, figsize=self.figsize, dpi=self.dpi)
        axes = axes.flatten()
        
        # Initialize plots
        lines = []
        for i, (name, structure) in enumerate(insect.neural_structures.items()):
            if i >= len(axes):
                break
            
            ax = axes[i]
            if hasattr(structure, 'get_activity'):
                activity = structure.get_activity()
                if activity.ndim == 1:
                    line, = ax.plot(activity, 'o-', linewidth=2, markersize=3)
                    lines.append(line)
                    ax.set_title(f'{name}')
                    ax.set_xlabel('Neuron')
                    ax.set_ylabel('Activity')
                    ax.grid(True, alpha=0.3)
                else:
                    # For 2D activity, we'll just show the mean
                    line, = ax.plot(np.mean(activity, axis=1), 'o-', linewidth=2, markersize=3)
                    lines.append(line)
                    ax.set_title(f'{name} (Mean)')
                    ax.set_xlabel('Neuron')
                    ax.set_ylabel('Mean Activity')
                    ax.grid(True, alpha=0.3)
        
        def animate(frame):
            # Update activity for each structure
            for i, (name, structure) in enumerate(insect.neural_structures.items()):
                if i >= len(lines):
                    break
                
                if hasattr(structure, 'get_activity'):
                    activity = structure.get_activity()
                    if activity.ndim == 1:
                        lines[i].set_ydata(activity)
                    else:
                        lines[i].set_ydata(np.mean(activity, axis=1))
            
            return lines
        
        anim = animation.FuncAnimation(fig, animate, frames=int(duration * fps), 
                                     interval=1000//fps, blit=True)
        
        if save_path:
            anim.save(save_path, writer='pillow', fps=fps)
        
        return anim
    
    def track_learning_dynamics(self, insect: InsectModel, 
                              learning_signal: np.ndarray,
                              save_path: Optional[str] = None) -> plt.Figure:
        """
        Track learning dynamics in neural structures.
        
        Args:
            insect: The insect model
            learning_signal: Learning signal to apply
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        if not hasattr(insect, 'neural_structures') or not insect.neural_structures:
            fig, ax = plt.subplots(figsize=self.figsize, dpi=self.dpi)
            ax.text(0.5, 0.5, 'No neural structures available', ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Learning Dynamics')
            return fig
        
        fig, axes = plt.subplots(2, 3, figsize=self.figsize, dpi=self.dpi)
        axes = axes.flatten()
        
        # Track learning for each structure
        for i, (name, structure) in enumerate(insect.neural_structures.items()):
            if i >= len(axes):
                break
            
            ax = axes[i]
            
            # Get activity before learning
            if hasattr(structure, 'get_activity'):
                activity_before = structure.get_activity().copy()
                
                # Apply learning signal
                if hasattr(structure, 'update_weights'):
                    structure.update_weights(learning_signal)
                
                # Get activity after learning
                activity_after = structure.get_activity()
                
                # Plot comparison
                if activity_before.ndim == 1 and activity_after.ndim == 1:
                    ax.plot(activity_before, 'o-', label='Before', linewidth=2, markersize=3)
                    ax.plot(activity_after, 's-', label='After', linewidth=2, markersize=3)
                    ax.set_title(f'{name} Learning')
                    ax.set_xlabel('Neuron')
                    ax.set_ylabel('Activity')
                    ax.legend()
                    ax.grid(True, alpha=0.3)
                else:
                    # For 2D, show mean activity
                    mean_before = np.mean(activity_before, axis=1)
                    mean_after = np.mean(activity_after, axis=1)
                    ax.plot(mean_before, 'o-', label='Before', linewidth=2, markersize=3)
                    ax.plot(mean_after, 's-', label='After', linewidth=2, markersize=3)
                    ax.set_title(f'{name} Learning (Mean)')
                    ax.set_xlabel('Neuron')
                    ax.set_ylabel('Mean Activity')
                    ax.legend()
                    ax.grid(True, alpha=0.3)
        
        plt.suptitle(f'Learning Dynamics: {insect.species}', fontsize=16)
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        
        return fig 