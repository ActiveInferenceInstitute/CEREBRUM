"""
Behavior Visualizer for Insect Models

This module provides visualization tools for insect behavioral patterns,
including individual behaviors, swarm dynamics, and behavioral transitions.
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
from collections import defaultdict, deque

from src.core.model import Case
from src.models.insect.base import InsectModel, BehavioralState
from src.models.insect.behaviors import BehaviorType, BehaviorResult

logger = logging.getLogger(__name__)


class BehaviorPatternVisualizer:
    """
    Visualizer for individual insect behavioral patterns.
    """
    
    def __init__(self, figsize: Tuple[int, int] = (12, 8), dpi: int = 100):
        """
        Initialize the behavior pattern visualizer.
        
        Args:
            figsize: Figure size
            dpi: DPI for figures
        """
        self.figsize = figsize
        self.dpi = dpi
        self.behavior_history = deque(maxlen=1000)
        self.performance_history = deque(maxlen=1000)
        
        # Color scheme for behavioral states
        self.behavior_colors = {
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
        }
        
        logger.info("Initialized BehaviorPatternVisualizer")
    
    def track_behavior(self, insect: InsectModel, event_data: Dict[str, Any]):
        """
        Track behavioral changes and results.
        
        Args:
            insect: The insect model
            event_data: Event data from simulation
        """
        entry = {
            'timestamp': event_data.get('timestamp', time.time()),
            'behavioral_state': insect.behavioral_state,
            'case': insect.current_case,
            'position': event_data.get('position', getattr(insect, 'position', np.zeros(3))),
            'performance': insect.get_performance_summary(),
            'confidence': event_data.get('processed_data', {}).get('confidence', 0.0),
            'action_type': event_data.get('processed_data', {}).get('action_selected', 'unknown'),
            'step': event_data.get('step', 0)
        }
        
        self.behavior_history.append(entry)
    
    def visualize_behavior_timeline(self, save_path: Optional[str] = None) -> plt.Figure:
        """
        Visualize behavioral timeline over time.
        
        Args:
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        if not self.behavior_history:
            fig, ax = plt.subplots(figsize=self.figsize, dpi=self.dpi)
            ax.text(0.5, 0.5, 'Behavior data being collected...', ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Behavior Timeline')
            if save_path:
                plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
            return fig
        
        # Create timeline visualization
        fig, ax = plt.subplots(figsize=self.figsize, dpi=self.dpi)
        
        # Extract timeline data
        timestamps = [entry['timestamp'] for entry in self.behavior_history]
        behaviors = [entry['behavioral_state'].value for entry in self.behavior_history]
        confidences = [entry['confidence'] for entry in self.behavior_history]
        
        # Normalize timestamps
        start_time = min(timestamps) if timestamps else 0
        normalized_times = [t - start_time for t in timestamps]
        
        # Plot behavior timeline
        unique_behaviors = list(set(behaviors))
        behavior_indices = [unique_behaviors.index(b) for b in behaviors]
        
        ax.plot(normalized_times, behavior_indices, 'o-', linewidth=2, markersize=4)
        ax.set_title('Behavior Timeline')
        ax.set_xlabel('Time (seconds)')
        ax.set_ylabel('Behavior Type')
        ax.set_yticks(range(len(unique_behaviors)))
        ax.set_yticklabels(unique_behaviors)
        ax.grid(True, alpha=0.3)
        
        # Add confidence as color intensity
        if confidences:
            scatter = ax.scatter(normalized_times, behavior_indices, c=confidences, 
                               cmap='viridis', s=50, alpha=0.7)
            plt.colorbar(scatter, ax=ax, label='Confidence')
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        
        return fig
    
    def generate_comprehensive_visualizations(self, output_dir: str):
        """
        Generate comprehensive swarm analysis visualizations.
        
        Args:
            output_dir: Output directory for visualizations
        """
        swarm_dir = os.path.join(output_dir, "visualizations", "swarm_analysis")
        os.makedirs(swarm_dir, exist_ok=True)
        
        if not self.behavior_history:
            print("    No swarm behavior data available for visualization")
            return
        
        # Generate swarm coordination analysis
        fig, axes = plt.subplots(2, 2, figsize=(15, 12))
        fig.suptitle('Comprehensive Swarm Analysis', fontsize=16)
        
        # Extract data from behavior history
        steps = [entry['step'] for entry in self.behavior_history]
        confidences = [entry['confidence'] for entry in self.behavior_history]
        behaviors = [entry['behavioral_state'].value for entry in self.behavior_history]
        cases = [entry['case'].value for entry in self.behavior_history]
        
        # Plot confidence over time
        if len(steps) > 0 and len(confidences) > 0:
            axes[0, 0].plot(steps, confidences, 'b-', linewidth=2, marker='o', markersize=3)
            axes[0, 0].set_title('Swarm Confidence Over Time')
            axes[0, 0].set_xlabel('Simulation Step')
            axes[0, 0].set_ylabel('Confidence')
            axes[0, 0].grid(True, alpha=0.3)
        
        # Plot behavioral state distribution
        if behaviors:
            behavior_counts = {}
            for behavior in behaviors:
                behavior_counts[behavior] = behavior_counts.get(behavior, 0) + 1
            
            if behavior_counts:
                behavior_names = list(behavior_counts.keys())
                behavior_values = list(behavior_counts.values())
                colors = [self.behavior_colors.get(BehavioralState(name), '#cccccc') for name in behavior_names]
                axes[0, 1].bar(behavior_names, behavior_values, color=colors, alpha=0.7)
                axes[0, 1].set_title('Behavioral State Distribution')
                axes[0, 1].set_ylabel('Frequency')
                axes[0, 1].tick_params(axis='x', rotation=45)
        
        # Plot case distribution
        if cases:
            case_counts = {}
            for case in cases:
                case_counts[case] = case_counts.get(case, 0) + 1
            
            if case_counts:
                case_names = list(case_counts.keys())
                case_values = list(case_counts.values())
                axes[1, 0].bar(case_names, case_values, color='lightcoral', alpha=0.7)
                axes[1, 0].set_title('Case Distribution in Swarm')
                axes[1, 0].set_ylabel('Frequency')
                axes[1, 0].tick_params(axis='x', rotation=45)
        
        # Plot performance statistics
        if len(confidences) > 0:
            axes[1, 1].hist(confidences, bins=20, alpha=0.7, color='skyblue', edgecolor='black')
            axes[1, 1].set_title('Swarm Performance Distribution')
            axes[1, 1].set_xlabel('Confidence')
            axes[1, 1].set_ylabel('Frequency')
            axes[1, 1].axvline(np.mean(confidences), color='red', linestyle='--', 
                              label=f'Mean: {np.mean(confidences):.3f}')
            axes[1, 1].legend()
        
        plt.tight_layout()
        plt.savefig(os.path.join(swarm_dir, 'swarm_coordination_analysis.png'), dpi=300, bbox_inches='tight')
        plt.close(fig)
        
        # Generate collective behavior analysis
        fig, ax = plt.subplots(figsize=(12, 8))
        
        if len(steps) > 0 and len(confidences) > 0:
            # Calculate moving average for smoother trend
            window_size = min(10, len(confidences) // 4)
            if window_size > 1:
                moving_avg = np.convolve(confidences, np.ones(window_size)/window_size, mode='valid')
                moving_steps = steps[window_size-1:]
                ax.plot(moving_steps, moving_avg, 'g-', linewidth=3, label='Moving Average')
            
            ax.plot(steps, confidences, 'b-', linewidth=1, alpha=0.5, label='Raw Data')
            ax.set_title('Collective Behavior Analysis')
            ax.set_xlabel('Simulation Step')
            ax.set_ylabel('Confidence')
            ax.grid(True, alpha=0.3)
            ax.legend()
        
        plt.tight_layout()
        plt.savefig(os.path.join(swarm_dir, 'collective_behavior_analysis.png'), dpi=300, bbox_inches='tight')
        plt.close(fig)
        
        # Generate swarm performance metrics
        if len(confidences) > 0:
            fig, ax = plt.subplots(figsize=(10, 8))
            
            # Calculate performance metrics
            avg_confidence = np.mean(confidences)
            max_confidence = np.max(confidences)
            min_confidence = np.min(confidences)
            std_confidence = np.std(confidences)
            
            metrics = ['Average', 'Maximum', 'Minimum', 'Std Dev']
            values = [avg_confidence, max_confidence, min_confidence, std_confidence]
            colors = ['green', 'blue', 'red', 'orange']
            
            bars = ax.bar(metrics, values, color=colors, alpha=0.7)
            ax.set_title('Swarm Performance Metrics')
            ax.set_ylabel('Confidence Value')
            
            # Add value labels on bars
            for bar, value in zip(bars, values):
                height = bar.get_height()
                ax.text(bar.get_x() + bar.get_width()/2., height + 0.01,
                       f'{value:.3f}', ha='center', va='bottom')
            
            plt.tight_layout()
            plt.savefig(os.path.join(swarm_dir, 'swarm_performance_metrics.png'), dpi=300, bbox_inches='tight')
            plt.close(fig)
        
        print(f"    Generated swarm analysis visualizations in {swarm_dir}")
        
        fig, (ax1, ax2) = plt.subplots(2, 1, figsize=self.figsize, dpi=self.dpi)
        
        # Extract data
        timestamps = [entry['timestamp'] for entry in self.behavior_history]
        states = [entry['behavioral_state'] for entry in self.behavior_history]
        cases = [entry['case'] for entry in self.behavior_history]
        
        # Normalize timestamps
        start_time = min(timestamps)
        normalized_times = [t - start_time for t in timestamps]
        
        # Plot behavioral state timeline
        state_values = [state.value for state in states]
        unique_states = list(set(states))
        state_indices = [unique_states.index(state) for state in states]
        
        ax1.plot(normalized_times, state_indices, 'o-', linewidth=2, markersize=4)
        ax1.set_ylabel('Behavioral State')
        ax1.set_title('Behavioral State Timeline')
        ax1.set_yticks(range(len(unique_states)))
        ax1.set_yticklabels([state.value for state in unique_states])
        ax1.grid(True, alpha=0.3)
        
        # Plot case timeline
        case_values = [case.value for case in cases]
        unique_cases = list(set(cases))
        case_indices = [unique_cases.index(case) for case in cases]
        
        ax2.plot(normalized_times, case_indices, 's-', linewidth=2, markersize=4)
        ax2.set_xlabel('Time (seconds)')
        ax2.set_ylabel('Case')
        ax2.set_title('Case Timeline')
        ax2.set_yticks(range(len(unique_cases)))
        ax2.set_yticklabels([case.value for case in unique_cases])
        ax2.grid(True, alpha=0.3)
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        
        return fig
    
    def visualize_behavior_performance(self, save_path: Optional[str] = None) -> plt.Figure:
        """
        Visualize behavioral performance metrics.
        
        Args:
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        if not self.behavior_history:
            fig, ax = plt.subplots(figsize=self.figsize, dpi=self.dpi)
            ax.text(0.5, 0.5, 'No behavior history available', ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Behavior Performance')
            return fig
        
        fig, axes = plt.subplots(2, 2, figsize=self.figsize, dpi=self.dpi)
        
        # Extract performance data
        behavioral_performance = defaultdict(list)
        case_performance = defaultdict(list)
        energy_usage = []
        success_rates = []
        
        for entry in self.behavior_history:
            state = entry['behavioral_state']
            case = entry['case']
            performance = entry['performance']
            
            behavioral_performance[state].append(performance['total_actions'])
            case_performance[case].append(performance['total_actions'])
            
            if 'behavior_result' in entry:
                result = entry['behavior_result']
                energy_usage.append(result['energy_expended'])
                success_rates.append(1.0 if result['success'] else 0.0)
        
        # Plot behavioral performance
        if behavioral_performance:
            states = list(behavioral_performance.keys())
            avg_performance = [np.mean(behavioral_performance[state]) for state in states]
            colors = [self.behavior_colors.get(state, '#cccccc') for state in states]
            
            axes[0, 0].bar(range(len(states)), avg_performance, color=colors, alpha=0.7)
            axes[0, 0].set_title('Behavioral Performance')
            axes[0, 0].set_ylabel('Average Actions')
            axes[0, 0].set_xticks(range(len(states)))
            axes[0, 0].set_xticklabels([state.value for state in states], rotation=45, ha='right')
        
        # Plot case performance
        if case_performance:
            cases = list(case_performance.keys())
            avg_performance = [np.mean(case_performance[case]) for case in cases]
            
            axes[0, 1].bar(range(len(cases)), avg_performance, color='lightblue', alpha=0.7)
            axes[0, 1].set_title('Case Performance')
            axes[0, 1].set_ylabel('Average Actions')
            axes[0, 1].set_xticks(range(len(cases)))
            axes[0, 1].set_xticklabels([case.value for case in cases], rotation=45, ha='right')
        
        # Plot energy usage
        if energy_usage:
            axes[1, 0].hist(energy_usage, bins=20, alpha=0.7, color='orange')
            axes[1, 0].set_title('Energy Usage Distribution')
            axes[1, 0].set_xlabel('Energy Expended')
            axes[1, 0].set_ylabel('Frequency')
        
        # Plot success rates
        if success_rates:
            axes[1, 1].hist(success_rates, bins=2, alpha=0.7, color='green')
            axes[1, 1].set_title('Success Rate Distribution')
            axes[1, 1].set_xlabel('Success (0=No, 1=Yes)')
            axes[1, 1].set_ylabel('Frequency')
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        
        return fig
    
    def visualize_behavior_transitions(self, save_path: Optional[str] = None) -> plt.Figure:
        """
        Visualize behavioral state transitions.
        
        Args:
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        if len(self.behavior_history) < 2:
            fig, ax = plt.subplots(figsize=self.figsize, dpi=self.dpi)
            ax.text(0.5, 0.5, 'Insufficient data for transition analysis', ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Behavior Transitions')
            return fig
        
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=self.figsize, dpi=self.dpi)
        
        # Create transition matrix
        states = list(set(entry['behavioral_state'] for entry in self.behavior_history))
        transition_matrix = np.zeros((len(states), len(states)))
        
        for i in range(len(self.behavior_history) - 1):
            current_state = self.behavior_history[i]['behavioral_state']
            next_state = self.behavior_history[i + 1]['behavioral_state']
            
            current_idx = states.index(current_state)
            next_idx = states.index(next_state)
            
            transition_matrix[current_idx, next_idx] += 1
        
        # Plot transition matrix
        im = ax1.imshow(transition_matrix, cmap='Blues', aspect='auto')
        ax1.set_title('Behavior Transition Matrix')
        ax1.set_xlabel('To State')
        ax1.set_ylabel('From State')
        ax1.set_xticks(range(len(states)))
        ax1.set_yticks(range(len(states)))
        ax1.set_xticklabels([state.value for state in states], rotation=45, ha='right')
        ax1.set_yticklabels([state.value for state in states])
        plt.colorbar(im, ax=ax1)
        
        # Plot transition probabilities
        row_sums = transition_matrix.sum(axis=1, keepdims=True)
        transition_probs = np.divide(transition_matrix, row_sums, where=row_sums != 0)
        
        im2 = ax2.imshow(transition_probs, cmap='Reds', aspect='auto')
        ax2.set_title('Transition Probabilities')
        ax2.set_xlabel('To State')
        ax2.set_ylabel('From State')
        ax2.set_xticks(range(len(states)))
        ax2.set_yticks(range(len(states)))
        ax2.set_xticklabels([state.value for state in states], rotation=45, ha='right')
        ax2.set_yticklabels([state.value for state in states])
        plt.colorbar(im2, ax=ax2)
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        
        return fig


class SwarmBehaviorVisualizer:
    """
    Visualizer for swarm behavior and collective dynamics.
    """
    
    def __init__(self, figsize: Tuple[int, int] = (15, 10), dpi: int = 100):
        """
        Initialize the swarm behavior visualizer.
        
        Args:
            figsize: Figure size
            dpi: DPI for figures
        """
        self.figsize = figsize
        self.dpi = dpi
        self.swarm_history = deque(maxlen=1000)
        
        logger.info("Initialized SwarmBehaviorVisualizer")
    
    def track_swarm_state(self, insects: List[InsectModel], 
                         swarm_center: np.ndarray,
                         swarm_dispersion: float,
                         swarm_cohesion: float):
        """
        Track swarm state and dynamics.
        
        Args:
            insects: List of insect models in the swarm
            swarm_center: Center of mass of the swarm
            swarm_dispersion: Dispersion measure of the swarm
            swarm_cohesion: Cohesion measure of the swarm
        """
        entry = {
            'timestamp': time.time(),
            'swarm_center': swarm_center,
            'swarm_dispersion': swarm_dispersion,
            'swarm_cohesion': swarm_cohesion,
            'insect_positions': [getattr(insect, 'position', np.zeros(3)) for insect in insects],
            'insect_states': [insect.behavioral_state for insect in insects],
            'insect_cases': [insect.current_case for insect in insects]
        }
        
        self.swarm_history.append(entry)
    
    def visualize_swarm_dynamics(self, save_path: Optional[str] = None) -> plt.Figure:
        """
        Visualize swarm dynamics over time.
        
        Args:
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        if not self.swarm_history:
            fig, ax = plt.subplots(figsize=self.figsize, dpi=self.dpi)
            ax.text(0.5, 0.5, 'No swarm history available', ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Swarm Dynamics')
            return fig
        
        fig, axes = plt.subplots(2, 2, figsize=self.figsize, dpi=self.dpi)
        
        # Extract data
        timestamps = [entry['timestamp'] for entry in self.swarm_history]
        dispersions = [entry['swarm_dispersion'] for entry in self.swarm_history]
        cohesions = [entry['swarm_cohesion'] for entry in self.swarm_history]
        
        # Normalize timestamps
        start_time = min(timestamps)
        normalized_times = [t - start_time for t in timestamps]
        
        # Plot dispersion over time
        axes[0, 0].plot(normalized_times, dispersions, 'o-', linewidth=2, markersize=4)
        axes[0, 0].set_title('Swarm Dispersion')
        axes[0, 0].set_ylabel('Dispersion')
        axes[0, 0].grid(True, alpha=0.3)
        
        # Plot cohesion over time
        axes[0, 1].plot(normalized_times, cohesions, 's-', linewidth=2, markersize=4)
        axes[0, 1].set_title('Swarm Cohesion')
        axes[0, 1].set_ylabel('Cohesion')
        axes[0, 1].grid(True, alpha=0.3)
        
        # Plot dispersion vs cohesion
        axes[1, 0].scatter(dispersions, cohesions, alpha=0.6)
        axes[1, 0].set_xlabel('Dispersion')
        axes[1, 0].set_ylabel('Cohesion')
        axes[1, 0].set_title('Dispersion vs Cohesion')
        axes[1, 0].grid(True, alpha=0.3)
        
        # Plot swarm center trajectory
        centers = [entry['swarm_center'] for entry in self.swarm_history]
        center_x = [center[0] for center in centers]
        center_y = [center[1] for center in centers]
        
        axes[1, 1].plot(center_x, center_y, 'o-', linewidth=2, markersize=4)
        axes[1, 1].set_xlabel('X Position')
        axes[1, 1].set_ylabel('Y Position')
        axes[1, 1].set_title('Swarm Center Trajectory')
        axes[1, 1].grid(True, alpha=0.3)
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        
        return fig
    
    def generate_comprehensive_visualizations(self, output_dir: str):
        """
        Generate comprehensive swarm analysis visualizations.
        
        Args:
            output_dir: Output directory for visualizations
        """
        swarm_dir = os.path.join(output_dir, "visualizations", "swarm_analysis")
        os.makedirs(swarm_dir, exist_ok=True)
        
        if not self.swarm_history:
            print("    No swarm behavior data available for visualization")
            return
        
        # Generate swarm dynamics analysis
        fig, axes = plt.subplots(2, 2, figsize=(15, 12))
        fig.suptitle('Comprehensive Swarm Analysis', fontsize=16)
        
        # Extract data from swarm history
        timestamps = [entry['timestamp'] for entry in self.swarm_history]
        dispersions = [entry['swarm_dispersion'] for entry in self.swarm_history]
        cohesions = [entry['swarm_cohesion'] for entry in self.swarm_history]
        centers = [entry['swarm_center'] for entry in self.swarm_history]
        
        # Normalize timestamps
        start_time = min(timestamps) if timestamps else 0
        normalized_times = [t - start_time for t in timestamps]
        
        # Plot dispersion over time
        if len(normalized_times) > 0 and len(dispersions) > 0:
            axes[0, 0].plot(normalized_times, dispersions, 'o-', linewidth=2, markersize=4)
            axes[0, 0].set_title('Swarm Dispersion Over Time')
            axes[0, 0].set_xlabel('Time (seconds)')
            axes[0, 0].set_ylabel('Dispersion')
            axes[0, 0].grid(True, alpha=0.3)
        
        # Plot cohesion over time
        if len(normalized_times) > 0 and len(cohesions) > 0:
            axes[0, 1].plot(normalized_times, cohesions, 's-', linewidth=2, markersize=4)
            axes[0, 1].set_title('Swarm Cohesion Over Time')
            axes[0, 1].set_xlabel('Time (seconds)')
            axes[0, 1].set_ylabel('Cohesion')
            axes[0, 1].grid(True, alpha=0.3)
        
        # Plot dispersion vs cohesion
        if len(dispersions) > 0 and len(cohesions) > 0:
            axes[1, 0].scatter(dispersions, cohesions, alpha=0.6, c=normalized_times, cmap='viridis')
            axes[1, 0].set_xlabel('Dispersion')
            axes[1, 0].set_ylabel('Cohesion')
            axes[1, 0].set_title('Dispersion vs Cohesion')
            axes[1, 0].grid(True, alpha=0.3)
            plt.colorbar(axes[1, 0].collections[0], ax=axes[1, 0], label='Time')
        
        # Plot swarm center trajectory
        if len(centers) > 0:
            center_x = [center[0] for center in centers]
            center_y = [center[1] for center in centers]
            scatter = axes[1, 1].scatter(center_x, center_y, c=normalized_times, cmap='viridis', s=50, alpha=0.7)
            axes[1, 1].set_xlabel('X Position')
            axes[1, 1].set_ylabel('Y Position')
            axes[1, 1].set_title('Swarm Center Trajectory')
            axes[1, 1].grid(True, alpha=0.3)
            plt.colorbar(scatter, ax=axes[1, 1], label='Time')
        
        plt.tight_layout()
        plt.savefig(os.path.join(swarm_dir, 'swarm_dynamics_analysis.png'), dpi=300, bbox_inches='tight')
        plt.close(fig)
        
        # Generate swarm state distribution
        if self.swarm_history:
            fig, axes = plt.subplots(2, 2, figsize=(12, 10))
            fig.suptitle('Swarm State Distribution', fontsize=16)
            
            # Extract insect states and cases
            all_states = []
            all_cases = []
            for entry in self.swarm_history:
                all_states.extend([state.value for state in entry['insect_states']])
                all_cases.extend([case.value for case in entry['insect_cases']])
            
            # Plot state distribution
            if all_states:
                state_counts = {}
                for state in all_states:
                    state_counts[state] = state_counts.get(state, 0) + 1
                
                if state_counts:
                    state_names = list(state_counts.keys())
                    state_values = list(state_counts.values())
                    axes[0, 0].bar(state_names, state_values, color='skyblue', alpha=0.7)
                    axes[0, 0].set_title('Behavioral State Distribution')
                    axes[0, 0].set_ylabel('Frequency')
                    axes[0, 0].tick_params(axis='x', rotation=45)
            
            # Plot case distribution
            if all_cases:
                case_counts = {}
                for case in all_cases:
                    case_counts[case] = case_counts.get(case, 0) + 1
                
                if case_counts:
                    case_names = list(case_counts.keys())
                    case_values = list(case_counts.values())
                    axes[0, 1].bar(case_names, case_values, color='lightcoral', alpha=0.7)
                    axes[0, 1].set_title('Case Distribution')
                    axes[0, 1].set_ylabel('Frequency')
                    axes[0, 1].tick_params(axis='x', rotation=45)
            
            # Plot dispersion histogram
            if dispersions:
                axes[1, 0].hist(dispersions, bins=20, alpha=0.7, color='orange', edgecolor='black')
                axes[1, 0].set_title('Dispersion Distribution')
                axes[1, 0].set_xlabel('Dispersion')
                axes[1, 0].set_ylabel('Frequency')
                axes[1, 0].axvline(np.mean(dispersions), color='red', linestyle='--', 
                                  label=f'Mean: {np.mean(dispersions):.3f}')
                axes[1, 0].legend()
            
            # Plot cohesion histogram
            if cohesions:
                axes[1, 1].hist(cohesions, bins=20, alpha=0.7, color='green', edgecolor='black')
                axes[1, 1].set_title('Cohesion Distribution')
                axes[1, 1].set_xlabel('Cohesion')
                axes[1, 1].set_ylabel('Frequency')
                axes[1, 1].axvline(np.mean(cohesions), color='red', linestyle='--', 
                                  label=f'Mean: {np.mean(cohesions):.3f}')
                axes[1, 1].legend()
            
            plt.tight_layout()
            plt.savefig(os.path.join(swarm_dir, 'swarm_state_distribution.png'), dpi=300, bbox_inches='tight')
            plt.close(fig)
        
        print(f"    Generated swarm analysis visualizations in {swarm_dir}")
        axes[1, 1].set_title('Swarm Center Trajectory')
        axes[1, 1].grid(True, alpha=0.3)
        axes[1, 1].set_aspect('equal')
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        
        return fig
    
    def visualize_swarm_state(self, insects: List[InsectModel], 
                            environment: Dict[str, Any],
                            save_path: Optional[str] = None) -> plt.Figure:
        """
        Visualize current swarm state in the environment.
        
        Args:
            insects: List of insect models
            environment: Environment data
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=self.figsize, dpi=self.dpi)
        
        # Plot swarm positions
        positions = []
        states = []
        cases = []
        
        for insect in insects:
            if hasattr(insect, 'position'):
                positions.append(insect.position[:2])  # Use first 2 dimensions
                states.append(insect.behavioral_state)
                cases.append(insect.current_case)
        
        if positions:
            positions = np.array(positions)
            
            # Plot environment features
            if 'obstacles' in environment:
                for obs in environment['obstacles']:
                    circle = Circle(obs[:2], 0.5, color='gray', alpha=0.7)
                    ax1.add_patch(circle)
            
            # Plot each insect
            for i, (pos, state, case) in enumerate(zip(positions, states, cases)):
                # Color by behavioral state
                color = self._get_behavior_color(state)
                
                # Size by case
                size = 100 if case == Case.NOMINATIVE else 50
                
                ax1.scatter(pos[0], pos[1], c=[color], s=size, alpha=0.8, edgecolors='black')
                ax1.annotate(f'{i}', (pos[0], pos[1]), xytext=(5, 5), textcoords='offset points')
            
            # Plot swarm center
            swarm_center = np.mean(positions, axis=0)
            ax1.scatter(swarm_center[0], swarm_center[1], c='red', s=200, marker='*', alpha=0.8)
            ax1.annotate('Center', (swarm_center[0], swarm_center[1]), xytext=(10, 10), textcoords='offset points')
        
        ax1.set_title('Swarm Positions')
        ax1.set_xlabel('X')
        ax1.set_ylabel('Y')
        ax1.grid(True, alpha=0.3)
        ax1.set_aspect('equal')
        
        # Plot state distribution
        if states:
            state_counts = defaultdict(int)
            for state in states:
                state_counts[state] += 1
            
            state_names = list(state_counts.keys())
            state_values = list(state_counts.values())
            colors = [self._get_behavior_color(state) for state in state_names]
            
            ax2.pie(state_values, labels=[state.value for state in state_names], 
                   colors=colors, autopct='%1.1f%%')
            ax2.set_title('Behavioral State Distribution')
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        
        return fig
    
    def create_swarm_animation(self, duration: float = 10.0,
                             fps: int = 10,
                             save_path: Optional[str] = None) -> animation.Animation:
        """
        Create an animation of swarm dynamics.
        
        Args:
            duration: Animation duration in seconds
            fps: Frames per second
            save_path: Optional path to save the animation
            
        Returns:
            Matplotlib animation
        """
        if not self.swarm_history:
            logger.warning("No swarm history available for animation")
            return None
        
        fig, ax = plt.subplots(figsize=self.figsize, dpi=self.dpi)
        
        # Initialize plot
        ax.set_xlim(-10, 10)
        ax.set_ylim(-10, 10)
        ax.set_title('Swarm Animation')
        ax.set_xlabel('X')
        ax.set_ylabel('Y')
        ax.grid(True, alpha=0.3)
        ax.set_aspect('equal')
        
        # Initialize scatter plot
        scatter = ax.scatter([], [], alpha=0.8, s=50)
        center_scatter = ax.scatter([], [], c='red', s=200, marker='*', alpha=0.8)
        
        def animate(frame):
            if frame < len(self.swarm_history):
                entry = self.swarm_history[frame]
                
                # Update insect positions
                positions = np.array(entry['insect_positions'])
                if positions.size > 0:
                    positions_2d = positions[:, :2]  # Use first 2 dimensions
                    scatter.set_offsets(positions_2d)
                
                # Update swarm center
                center = entry['swarm_center'][:2]
                center_scatter.set_offsets([center])
            
            return scatter, center_scatter
        
        anim = animation.FuncAnimation(fig, animate, frames=int(duration * fps), 
                                     interval=1000//fps, blit=True)
        
        if save_path:
            anim.save(save_path, writer='pillow', fps=fps)
        
        return anim
    
    def _get_behavior_color(self, state: BehavioralState) -> str:
        """Get color for behavioral state."""
        colors = {
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
        }
        return colors.get(state, '#cccccc') 