"""
Main Insect Visualizer for CEREBRUM

This module provides the primary visualization interface for insect cognitive models,
including case-based reasoning visualization, neural structure activity, and behavioral patterns.
"""

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib.patches import Circle, Arrow, Rectangle, Polygon
from matplotlib.collections import LineCollection
import seaborn as sns
from typing import Dict, Any, List, Optional, Tuple, Union
import logging
import os
import time
import json
from dataclasses import dataclass, field
from collections import defaultdict, deque

from src.core.model import Case
from src.models.insect.base import InsectModel, BehavioralState
from src.models.insect.neural_structures import NeuralStructure
from src.utils.path_utils import get_output_dir

logger = logging.getLogger(__name__)


@dataclass
class VisualizationConfig:
    """Configuration for insect visualization."""
    figsize: Tuple[int, int] = (12, 8)
    dpi: int = 100
    save_format: str = 'png'
    animation_fps: int = 10
    max_history_length: int = 1000
    case_colors: Dict[Case, str] = field(default_factory=lambda: {
        Case.NOMINATIVE: '#1f77b4',
        Case.ACCUSATIVE: '#ff7f0e',
        Case.DATIVE: '#2ca02c',
        Case.GENITIVE: '#d62728',
        Case.INSTRUMENTAL: '#9467bd',
        Case.LOCATIVE: '#8c564b',
        Case.ABLATIVE: '#e377c2',
        Case.VOCATIVE: '#7f7f7f'
    })
    behavioral_colors: Dict[BehavioralState, str] = field(default_factory=lambda: {
        BehavioralState.IDLE: '#cccccc',
        BehavioralState.FORAGING: '#ff9999',
        BehavioralState.NAVIGATING: '#99ccff',
        BehavioralState.COMMUNICATING: '#99ff99',
        BehavioralState.SOCIAL_INTERACTING: '#ffcc99',
        BehavioralState.ESCAPING: '#ff6666',
        BehavioralState.MATING: '#cc99ff',
        BehavioralState.NEST_BUILDING: '#99ffcc',
        BehavioralState.GROOMING: '#ff99cc',
        BehavioralState.RESTING: '#ccccff'
    })


class InsectVisualizer:
    """
    Main visualizer for insect cognitive models.
    
    This class provides comprehensive visualization capabilities for insect simulations,
    with special emphasis on case-based reasoning and neural activity tracking.
    """
    
    def __init__(self, config: Optional[VisualizationConfig] = None, output_dir: Optional[str] = None):
        """
        Initialize the insect visualizer.
        
        Args:
            config: Visualization configuration
            output_dir: Optional output directory override
        """
        self.config = config or VisualizationConfig()
        self.output_dir = output_dir or get_output_dir()
        self.insect_dir = os.path.join(self.output_dir, "insect_visualizations")
        os.makedirs(self.insect_dir, exist_ok=True)
        
        # History tracking
        self.case_history = deque(maxlen=self.config.max_history_length)
        self.behavioral_history = deque(maxlen=self.config.max_history_length)
        self.neural_activity_history = deque(maxlen=self.config.max_history_length)
        self.position_history = deque(maxlen=self.config.max_history_length)
        self.performance_history = deque(maxlen=self.config.max_history_length)
        
        # Statistics
        self.case_statistics = defaultdict(int)
        self.behavioral_statistics = defaultdict(int)
        self.performance_metrics = defaultdict(list)
        
        logger.info("Initialized InsectVisualizer")
    
    def update_history(self, insect: InsectModel, timestamp: float):
        """
        Update visualization history with current insect state.
        
        Args:
            insect: The insect model
            timestamp: Current simulation time
        """
        # Record case state
        self.case_history.append({
            'case': insect.current_case,
            'timestamp': timestamp,
            'performance': insect.get_performance_summary()
        })
        
        # Record behavioral state
        self.behavioral_history.append({
            'state': insect.behavioral_state,
            'timestamp': timestamp,
            'position': getattr(insect, 'position', np.zeros(3))
        })
        
        # Record neural activity if available
        if hasattr(insect, 'neural_structures'):
            neural_activity = {}
            for name, structure in insect.neural_structures.items():
                if hasattr(structure, 'get_activity'):
                    neural_activity[name] = structure.get_activity()
            self.neural_activity_history.append({
                'activity': neural_activity,
                'timestamp': timestamp
            })
        
        # Update statistics
        self.case_statistics[insect.current_case] += 1
        self.behavioral_statistics[insect.behavioral_state] += 1
    
    def visualize_case_transitions(self, save_path: Optional[str] = None) -> plt.Figure:
        """
        Visualize case transitions over time.
        
        Args:
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        if not self.case_history:
            logger.warning("No case history available for visualization")
            return plt.figure()
        
        fig, (ax1, ax2) = plt.subplots(2, 1, figsize=self.config.figsize, dpi=self.config.dpi)
        
        # Extract data
        timestamps = [entry['timestamp'] for entry in self.case_history]
        cases = [entry['case'] for entry in self.case_history]
        case_values = [case.value for case in cases]
        
        # Plot case timeline
        ax1.plot(timestamps, case_values, 'o-', linewidth=2, markersize=4)
        ax1.set_ylabel('Case')
        ax1.set_title('Case Transitions Over Time')
        ax1.grid(True, alpha=0.3)
        
        # Set y-axis labels
        unique_cases = list(set(cases))
        ax1.set_yticks(range(len(unique_cases)))
        ax1.set_yticklabels([case.value for case in unique_cases])
        
        # Plot case distribution
        case_counts = list(self.case_statistics.values())
        case_names = [case.value for case in self.case_statistics.keys()]
        
        colors = [self.config.case_colors.get(case, '#cccccc') for case in self.case_statistics.keys()]
        ax2.bar(range(len(case_counts)), case_counts, color=colors)
        ax2.set_xlabel('Case Type')
        ax2.set_ylabel('Frequency')
        ax2.set_title('Case Distribution')
        ax2.set_xticks(range(len(case_names)))
        ax2.set_xticklabels(case_names, rotation=45)
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=self.config.dpi, bbox_inches='tight')
        
        return fig
    
    def visualize_behavioral_patterns(self, save_path: Optional[str] = None) -> plt.Figure:
        """
        Visualize behavioral patterns over time.
        
        Args:
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        if not self.behavioral_history:
            logger.warning("No behavioral history available for visualization")
            return plt.figure()
        
        fig, (ax1, ax2) = plt.subplots(2, 1, figsize=self.config.figsize, dpi=self.config.dpi)
        
        # Extract data
        timestamps = [entry['timestamp'] for entry in self.behavioral_history]
        states = [entry['state'] for entry in self.behavioral_history]
        state_values = [state.value for state in states]
        
        # Plot behavioral timeline
        ax1.plot(timestamps, state_values, 'o-', linewidth=2, markersize=4)
        ax1.set_ylabel('Behavioral State')
        ax1.set_title('Behavioral State Transitions Over Time')
        ax1.grid(True, alpha=0.3)
        
        # Set y-axis labels
        unique_states = list(set(states))
        ax1.set_yticks(range(len(unique_states)))
        ax1.set_yticklabels([state.value for state in unique_states])
        
        # Plot behavioral distribution
        state_counts = list(self.behavioral_statistics.values())
        state_names = [state.value for state in self.behavioral_statistics.keys()]
        
        colors = [self.config.behavioral_colors.get(state, '#cccccc') for state in self.behavioral_statistics.keys()]
        ax2.bar(range(len(state_counts)), state_counts, color=colors)
        ax2.set_xlabel('Behavioral State')
        ax2.set_ylabel('Frequency')
        ax2.set_title('Behavioral State Distribution')
        ax2.set_xticks(range(len(state_names)))
        ax2.set_xticklabels(state_names, rotation=45)
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=self.config.dpi, bbox_inches='tight')
        
        return fig
    
    def visualize_neural_activity(self, save_path: Optional[str] = None) -> plt.Figure:
        """
        Visualize neural structure activity over time.
        
        Args:
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        if not self.neural_activity_history:
            logger.warning("No neural activity history available for visualization")
            return plt.figure()
        
        fig, axes = plt.subplots(2, 2, figsize=self.config.figsize, dpi=self.config.dpi)
        axes = axes.flatten()
        
        # Get all neural structure names
        all_structures = set()
        for entry in self.neural_activity_history:
            all_structures.update(entry['activity'].keys())
        
        if not all_structures:
            logger.warning("No neural structures found in activity history")
            return fig
        
        # Plot activity for each structure
        for i, structure_name in enumerate(list(all_structures)[:4]):  # Limit to 4 structures
            if i >= len(axes):
                break
                
            ax = axes[i]
            timestamps = []
            activities = []
            
            for entry in self.neural_activity_history:
                if structure_name in entry['activity']:
                    timestamps.append(entry['timestamp'])
                    activity = entry['activity'][structure_name]
                    if isinstance(activity, np.ndarray):
                        activities.append(np.mean(activity))
                    else:
                        activities.append(float(activity))
            
            if timestamps:
                ax.plot(timestamps, activities, 'o-', linewidth=2, markersize=3)
                ax.set_title(f'{structure_name} Activity')
                ax.set_ylabel('Mean Activity')
                ax.grid(True, alpha=0.3)
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=self.config.dpi, bbox_inches='tight')
        
        return fig
    
    def create_comprehensive_dashboard(self, insect: InsectModel, 
                                     save_path: Optional[str] = None) -> plt.Figure:
        """
        Create a comprehensive dashboard showing all aspects of the insect simulation.
        
        Args:
            insect: The insect model
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        fig = plt.figure(figsize=(16, 12), dpi=self.config.dpi)
        
        # Create subplot grid
        gs = fig.add_gridspec(3, 3, hspace=0.3, wspace=0.3)
        
        # Current state panel
        ax1 = fig.add_subplot(gs[0, 0])
        self._plot_current_state(ax1, insect)
        
        # Case timeline
        ax2 = fig.add_subplot(gs[0, 1:])
        self._plot_case_timeline(ax2)
        
        # Behavioral timeline
        ax3 = fig.add_subplot(gs[1, :])
        self._plot_behavioral_timeline(ax3)
        
        # Performance metrics
        ax4 = fig.add_subplot(gs[2, 0])
        self._plot_performance_metrics(ax4, insect)
        
        # Neural activity heatmap
        ax5 = fig.add_subplot(gs[2, 1])
        self._plot_neural_heatmap(ax5)
        
        # Case effectiveness
        ax6 = fig.add_subplot(gs[2, 2])
        self._plot_case_effectiveness(ax6)
        
        plt.suptitle(f'Insect Simulation Dashboard - {insect.species}', fontsize=16)
        
        if save_path:
            plt.savefig(save_path, dpi=self.config.dpi, bbox_inches='tight')
        
        return fig
    
    def _plot_current_state(self, ax: plt.Axes, insect: InsectModel):
        """Plot current insect state."""
        ax.text(0.1, 0.8, f'Species: {insect.species}', fontsize=12, transform=ax.transAxes)
        ax.text(0.1, 0.7, f'Current Case: {insect.current_case.value}', fontsize=12, transform=ax.transAxes)
        ax.text(0.1, 0.6, f'Behavioral State: {insect.behavioral_state.value}', fontsize=12, transform=ax.transAxes)
        
        # Color code the current case
        case_color = self.config.case_colors.get(insect.current_case, '#cccccc')
        ax.add_patch(Rectangle((0.05, 0.55), 0.9, 0.1, facecolor=case_color, alpha=0.3))
        
        ax.set_title('Current State')
        ax.set_xlim(0, 1)
        ax.set_ylim(0, 1)
        ax.axis('off')
    
    def _plot_case_timeline(self, ax: plt.Axes):
        """Plot case timeline."""
        if not self.case_history:
            ax.text(0.5, 0.5, 'No case history', ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Case Timeline')
            return
        
        timestamps = [entry['timestamp'] for entry in self.case_history]
        cases = [entry['case'] for entry in self.case_history]
        
        # Create color-coded timeline
        for i, (timestamp, case) in enumerate(zip(timestamps, cases)):
            color = self.config.case_colors.get(case, '#cccccc')
            ax.axvline(x=timestamp, color=color, alpha=0.7, linewidth=2)
        
        ax.set_title('Case Timeline')
        ax.set_xlabel('Time')
        ax.set_ylabel('Case')
        ax.grid(True, alpha=0.3)
    
    def _plot_behavioral_timeline(self, ax: plt.Axes):
        """Plot behavioral timeline."""
        if not self.behavioral_history:
            ax.text(0.5, 0.5, 'No behavioral history', ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Behavioral Timeline')
            return
        
        timestamps = [entry['timestamp'] for entry in self.behavioral_history]
        states = [entry['state'] for entry in self.behavioral_history]
        
        # Create color-coded timeline
        for i, (timestamp, state) in enumerate(zip(timestamps, states)):
            color = self.config.behavioral_colors.get(state, '#cccccc')
            ax.axvline(x=timestamp, color=color, alpha=0.7, linewidth=2)
        
        ax.set_title('Behavioral Timeline')
        ax.set_xlabel('Time')
        ax.set_ylabel('Behavioral State')
        ax.grid(True, alpha=0.3)
    
    def _plot_performance_metrics(self, ax: plt.Axes, insect: InsectModel):
        """Plot performance metrics."""
        performance = insect.get_performance_summary()
        
        metrics = ['Total Actions', 'Case Transformations', 'Avg Sensory Time', 'Avg Action Time']
        values = [
            performance['total_actions'],
            performance['case_transformations'],
            performance['avg_sensory_processing_time'],
            performance['avg_action_selection_time']
        ]
        
        bars = ax.bar(metrics, values, color=['#ff9999', '#99ccff', '#99ff99', '#ffcc99'])
        ax.set_title('Performance Metrics')
        ax.set_ylabel('Value')
        plt.setp(ax.get_xticklabels(), rotation=45, ha='right')
    
    def _plot_neural_heatmap(self, ax: plt.Axes):
        """Plot neural activity heatmap."""
        if not self.neural_activity_history:
            ax.text(0.5, 0.5, 'No neural activity data', ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Neural Activity Heatmap')
            return
        
        # Create activity matrix
        structures = set()
        for entry in self.neural_activity_history:
            structures.update(entry['activity'].keys())
        
        if not structures:
            ax.text(0.5, 0.5, 'No neural structures', ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Neural Activity Heatmap')
            return
        
        structures = list(structures)
        activity_matrix = []
        
        for entry in self.neural_activity_history:
            row = []
            for structure in structures:
                if structure in entry['activity']:
                    activity = entry['activity'][structure]
                    if isinstance(activity, np.ndarray):
                        row.append(np.mean(activity))
                    else:
                        row.append(float(activity))
                else:
                    row.append(0.0)
            activity_matrix.append(row)
        
        if activity_matrix:
            im = ax.imshow(activity_matrix, cmap='viridis', aspect='auto')
            ax.set_yticks(range(len(structures)))
            ax.set_yticklabels(structures)
            ax.set_title('Neural Activity Heatmap')
            plt.colorbar(im, ax=ax)
    
    def _plot_case_effectiveness(self, ax: plt.Axes):
        """Plot case effectiveness analysis."""
        if not self.case_history:
            ax.text(0.5, 0.5, 'No case data', ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Case Effectiveness')
            return
        
        # Calculate effectiveness metrics for each case
        case_performance = defaultdict(list)
        
        for entry in self.case_history:
            case = entry['case']
            performance = entry['performance']
            case_performance[case].append(performance.get('total_actions', 0))
        
        cases = list(case_performance.keys())
        avg_performance = [np.mean(case_performance[case]) for case in cases]
        colors = [self.config.case_colors.get(case, '#cccccc') for case in cases]
        
        bars = ax.bar(range(len(cases)), avg_performance, color=colors)
        ax.set_title('Case Effectiveness')
        ax.set_ylabel('Average Performance')
        ax.set_xticks(range(len(cases)))
        ax.set_xticklabels([case.value for case in cases], rotation=45, ha='right')
    
    def save_visualization_data(self, filename: str = "insect_visualization_data.json"):
        """
        Save visualization data to JSON file.
        
        Args:
            filename: Name of the file to save
        """
        data = {
            'case_history': [{
                'case': entry['case'].value,
                'timestamp': entry['timestamp'],
                'performance': entry['performance']
            } for entry in self.case_history],
            'behavioral_history': [{
                'state': entry['state'].value,
                'timestamp': entry['timestamp'],
                'position': entry['position'].tolist() if isinstance(entry['position'], np.ndarray) else entry['position']
            } for entry in self.behavioral_history],
            'case_statistics': {case.value: count for case, count in self.case_statistics.items()},
            'behavioral_statistics': {state.value: count for state, count in self.behavioral_statistics.items()},
            'performance_metrics': dict(self.performance_metrics)
        }
        
        filepath = os.path.join(self.insect_dir, filename)
        with open(filepath, 'w') as f:
            json.dump(data, f, indent=2)
        
        logger.info(f"Saved visualization data to {filepath}")


class InsectSimulationVisualizer(InsectVisualizer):
    """
    Specialized visualizer for insect simulation scenarios.
    """
    
    def __init__(self, config: Optional[VisualizationConfig] = None):
        super().__init__(config)
        self.simulation_data = {}
    
    def visualize_simulation_state(self, insects: List[InsectModel], 
                                 environment: Dict[str, Any],
                                 timestamp: float,
                                 save_path: Optional[str] = None) -> plt.Figure:
        """
        Visualize the current state of a multi-insect simulation.
        
        Args:
            insects: List of insect models
            environment: Environment data
            timestamp: Current simulation time
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        fig, axes = plt.subplots(2, 2, figsize=(16, 12), dpi=self.config.dpi)
        
        # Plot insect positions and states
        self._plot_insect_positions(axes[0, 0], insects, environment)
        
        # Plot case distribution
        self._plot_case_distribution(axes[0, 1], insects)
        
        # Plot behavioral distribution
        self._plot_behavioral_distribution(axes[1, 0], insects)
        
        # Plot performance comparison
        self._plot_performance_comparison(axes[1, 1], insects)
        
        plt.suptitle(f'Insect Simulation State - Time: {timestamp:.2f}', fontsize=16)
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=self.config.dpi, bbox_inches='tight')
        
        return fig
    
    def _plot_insect_positions(self, ax: plt.Axes, insects: List[InsectModel], 
                             environment: Dict[str, Any]):
        """Plot insect positions in the environment."""
        # Plot environment features
        if 'obstacles' in environment:
            for obs in environment['obstacles']:
                circle = Circle(obs[:2], 0.5, color='gray', alpha=0.7)
                ax.add_patch(circle)
        
        # Plot each insect
        for i, insect in enumerate(insects):
            if hasattr(insect, 'position'):
                pos = insect.position[:2]  # Use first 2 dimensions
                
                # Color by case
                color = self.config.case_colors.get(insect.current_case, '#cccccc')
                
                # Size by behavioral state
                size = 100 if insect.behavioral_state == BehavioralState.FORAGING else 50
                
                ax.scatter(pos[0], pos[1], c=[color], s=size, alpha=0.8, edgecolors='black')
                ax.annotate(f'{i}', (pos[0], pos[1]), xytext=(5, 5), textcoords='offset points')
        
        ax.set_title('Insect Positions')
        ax.set_xlabel('X')
        ax.set_ylabel('Y')
        ax.grid(True, alpha=0.3)
        ax.set_aspect('equal')
    
    def _plot_case_distribution(self, ax: plt.Axes, insects: List[InsectModel]):
        """Plot case distribution across insects."""
        case_counts = defaultdict(int)
        for insect in insects:
            case_counts[insect.current_case] += 1
        
        if case_counts:
            cases = list(case_counts.keys())
            counts = list(case_counts.values())
            colors = [self.config.case_colors.get(case, '#cccccc') for case in cases]
            
            ax.pie(counts, labels=[case.value for case in cases], colors=colors, autopct='%1.1f%%')
            ax.set_title('Case Distribution')
    
    def _plot_behavioral_distribution(self, ax: plt.Axes, insects: List[InsectModel]):
        """Plot behavioral distribution across insects."""
        behavioral_counts = defaultdict(int)
        for insect in insects:
            behavioral_counts[insect.behavioral_state] += 1
        
        if behavioral_counts:
            states = list(behavioral_counts.keys())
            counts = list(behavioral_counts.values())
            colors = [self.config.behavioral_colors.get(state, '#cccccc') for state in states]
            
            ax.bar(range(len(counts)), counts, color=colors)
            ax.set_title('Behavioral Distribution')
            ax.set_ylabel('Count')
            ax.set_xticks(range(len(states)))
            ax.set_xticklabels([state.value for state in states], rotation=45, ha='right')
    
    def _plot_performance_comparison(self, ax: plt.Axes, insects: List[InsectModel]):
        """Plot performance comparison across insects."""
        performances = []
        labels = []
        
        for i, insect in enumerate(insects):
            perf = insect.get_performance_summary()
            performances.append(perf['total_actions'])
            labels.append(f'Insect {i}')
        
        ax.bar(labels, performances, color='skyblue', alpha=0.7)
        ax.set_title('Performance Comparison')
        ax.set_ylabel('Total Actions')
        plt.setp(ax.get_xticklabels(), rotation=45, ha='right')


class CaseRelevanceVisualizer(InsectVisualizer):
    """
    Specialized visualizer for case relevance analysis.
    """
    
    def __init__(self, config: Optional[VisualizationConfig] = None):
        super().__init__(config)
        self.case_effectiveness_data = defaultdict(list)
    
    def analyze_case_effectiveness(self, insect: InsectModel, 
                                 context: Dict[str, Any]) -> Dict[str, float]:
        """
        Analyze the effectiveness of different cases in different contexts.
        
        Args:
            insect: The insect model
            context: Current context information
            
        Returns:
            Dictionary of case effectiveness scores
        """
        effectiveness = {}
        
        for case in Case:
            # Simulate case transformation and measure performance
            original_case = insect.current_case
            insect.transform_case(case)
            
            # Measure performance in this case
            performance = insect.get_performance_summary()
            effectiveness[case] = performance['total_actions']
            
            # Restore original case
            insect.transform_case(original_case)
        
        return effectiveness
    
    def visualize_case_relevance(self, insect: InsectModel, 
                               context: Dict[str, Any],
                               save_path: Optional[str] = None) -> plt.Figure:
        """
        Visualize case relevance in different contexts.
        
        Args:
            insect: The insect model
            context: Current context
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        effectiveness = self.analyze_case_effectiveness(insect, context)
        
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 6), dpi=self.config.dpi)
        
        # Plot case effectiveness
        cases = list(effectiveness.keys())
        scores = list(effectiveness.values())
        colors = [self.config.case_colors.get(case, '#cccccc') for case in cases]
        
        bars = ax1.bar(range(len(cases)), scores, color=colors)
        ax1.set_title('Case Effectiveness in Current Context')
        ax1.set_ylabel('Performance Score')
        ax1.set_xticks(range(len(cases)))
        ax1.set_xticklabels([case.value for case in cases], rotation=45, ha='right')
        
        # Highlight current case
        current_case_idx = cases.index(insect.current_case)
        bars[current_case_idx].set_edgecolor('red')
        bars[current_case_idx].set_linewidth(3)
        
        # Plot case transition recommendations
        self._plot_case_recommendations(ax2, effectiveness, insect.current_case)
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=self.config.dpi, bbox_inches='tight')
        
        return fig
    
    def _plot_case_recommendations(self, ax: plt.Axes, effectiveness: Dict[Case, float], 
                                 current_case: Case):
        """Plot case transition recommendations."""
        # Find best alternative case
        sorted_cases = sorted(effectiveness.items(), key=lambda x: x[1], reverse=True)
        
        if len(sorted_cases) > 1:
            best_case, best_score = sorted_cases[0]
            current_score = effectiveness[current_case]
            
            if best_case != current_case:
                improvement = best_score - current_score
                
                ax.text(0.1, 0.8, f'Current Case: {current_case.value}', fontsize=12, transform=ax.transAxes)
                ax.text(0.1, 0.7, f'Current Score: {current_score:.2f}', fontsize=12, transform=ax.transAxes)
                ax.text(0.1, 0.6, f'Recommended: {best_case.value}', fontsize=12, transform=ax.transAxes)
                ax.text(0.1, 0.5, f'Potential Improvement: {improvement:.2f}', fontsize=12, transform=ax.transAxes)
                
                # Color code the recommendation
                if improvement > 0:
                    ax.add_patch(Rectangle((0.05, 0.45), 0.9, 0.1, facecolor='lightgreen', alpha=0.3))
                else:
                    ax.add_patch(Rectangle((0.05, 0.45), 0.9, 0.1, facecolor='lightcoral', alpha=0.3))
            else:
                ax.text(0.1, 0.6, f'Optimal case already selected: {current_case.value}', fontsize=12, transform=ax.transAxes)
                ax.add_patch(Rectangle((0.05, 0.55), 0.9, 0.1, facecolor='lightblue', alpha=0.3))
        else:
            ax.text(0.1, 0.6, 'Insufficient data for recommendations', fontsize=12, transform=ax.transAxes)
        
        ax.set_title('Case Transition Recommendations')
        ax.set_xlim(0, 1)
        ax.set_ylim(0, 1)
        ax.axis('off') 