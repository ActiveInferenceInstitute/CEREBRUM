"""
Animation Creator for Insect Simulations

This module provides animation creation tools for insect simulations,
including individual insect animations and swarm dynamics animations.
"""

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib.patches import Circle, Arrow, Rectangle, Polygon
from matplotlib.collections import LineCollection
from typing import Dict, Any, List, Optional, Tuple, Union
import logging
import os
import time
from collections import deque

from src.core.model import Case
from src.models.insect.base import InsectModel, BehavioralState

logger = logging.getLogger(__name__)


class InsectAnimationCreator:
    """
    Creator for individual insect animations.
    """
    
    def __init__(self, figsize: Tuple[int, int] = (12, 8), dpi: int = 100):
        """
        Initialize the insect animation creator.
        
        Args:
            figsize: Figure size
            dpi: DPI for figures
        """
        self.figsize = figsize
        self.dpi = dpi
        
        # Color schemes
        self.case_colors = {
            Case.NOMINATIVE: '#1f77b4',
            Case.ACCUSATIVE: '#ff7f0e',
            Case.DATIVE: '#2ca02c',
            Case.GENITIVE: '#d62728',
            Case.INSTRUMENTAL: '#9467bd',
            Case.LOCATIVE: '#8c564b',
            Case.ABLATIVE: '#e377c2',
            Case.VOCATIVE: '#7f7f7f'
        }
        
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
        
        logger.info("Initialized InsectAnimationCreator")
    
    def create_insect_trajectory_animation(self, insect_history: List[Dict[str, Any]],
                                         environment: Optional[Dict[str, Any]] = None,
                                         duration: float = 10.0,
                                         fps: int = 10,
                                         save_path: Optional[str] = None) -> animation.Animation:
        """
        Create animation of insect trajectory over time.
        
        Args:
            insect_history: List of insect states over time
            environment: Optional environment data
            duration: Animation duration in seconds
            fps: Frames per second
            save_path: Optional path to save the animation
            
        Returns:
            Matplotlib animation
        """
        if not insect_history:
            logger.warning("No insect history provided for animation")
            return None
        
        fig, ax = plt.subplots(figsize=self.figsize, dpi=self.dpi)
        
        # Set up plot limits
        all_positions = []
        for entry in insect_history:
            if 'position' in entry:
                pos = entry['position']
                if isinstance(pos, (list, np.ndarray)) and len(pos) >= 2:
                    all_positions.append(pos[:2])
        
        if all_positions:
            all_positions = np.array(all_positions)
            x_min, x_max = all_positions[:, 0].min(), all_positions[:, 0].max()
            y_min, y_max = all_positions[:, 1].min(), all_positions[:, 1].max()
            
            # Add margin
            margin = max(x_max - x_min, y_max - y_min) * 0.1
            ax.set_xlim(x_min - margin, x_max + margin)
            ax.set_ylim(y_min - margin, y_max + margin)
        else:
            ax.set_xlim(-10, 10)
            ax.set_ylim(-10, 10)
        
        # Plot environment features
        if environment and 'obstacles' in environment:
            for obs in environment['obstacles']:
                if len(obs) >= 2:
                    circle = Circle(obs[:2], 0.5, color='gray', alpha=0.7)
                    ax.add_patch(circle)
        
        # Initialize trajectory line
        trajectory_line, = ax.plot([], [], 'b-', alpha=0.5, linewidth=2)
        
        # Initialize insect marker
        insect_marker = ax.scatter([], [], c='red', s=100, alpha=0.8, edgecolors='black')
        
        # Initialize case indicator
        case_text = ax.text(0.02, 0.98, '', transform=ax.transAxes, fontsize=12,
                           verticalalignment='top', bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
        
        # Initialize behavioral state indicator
        behavior_text = ax.text(0.02, 0.92, '', transform=ax.transAxes, fontsize=12,
                               verticalalignment='top', bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
        
        ax.set_title('Insect Trajectory Animation')
        ax.set_xlabel('X Position')
        ax.set_ylabel('Y Position')
        ax.grid(True, alpha=0.3)
        ax.set_aspect('equal')
        
        def animate(frame):
            if frame < len(insect_history):
                entry = insect_history[frame]
                
                # Update trajectory
                if 'position' in entry:
                    pos = entry['position']
                    if isinstance(pos, (list, np.ndarray)) and len(pos) >= 2:
                        pos_2d = pos[:2]
                        
                        # Update trajectory line
                        if frame > 0:
                            prev_pos = insect_history[frame - 1].get('position', [0, 0])
                            if isinstance(prev_pos, (list, np.ndarray)) and len(prev_pos) >= 2:
                                prev_pos_2d = prev_pos[:2]
                                trajectory_line.set_data([prev_pos_2d[0], pos_2d[0]], 
                                                       [prev_pos_2d[1], pos_2d[1]])
                        
                        # Update insect marker
                        insect_marker.set_offsets([pos_2d])
                        
                        # Color by case
                        if 'case' in entry:
                            case = entry['case']
                            if isinstance(case, Case):
                                color = self.case_colors.get(case, '#cccccc')
                                insect_marker.set_color(color)
                
                # Update text indicators
                if 'case' in entry:
                    case = entry['case']
                    if isinstance(case, Case):
                        case_text.set_text(f'Case: {case.value}')
                
                if 'behavioral_state' in entry:
                    state = entry['behavioral_state']
                    if isinstance(state, BehavioralState):
                        behavior_text.set_text(f'State: {state.value}')
            
            return trajectory_line, insect_marker, case_text, behavior_text
        
        anim = animation.FuncAnimation(fig, animate, frames=int(duration * fps), 
                                     interval=1000//fps, blit=True)
        
        if save_path:
            anim.save(save_path, writer='pillow', fps=fps)
        
        return anim
    
    def create_case_transition_animation(self, case_history: List[Dict[str, Any]],
                                       duration: float = 10.0,
                                       fps: int = 10,
                                       save_path: Optional[str] = None) -> animation.Animation:
        """
        Create animation of case transitions over time.
        
        Args:
            case_history: List of case states over time
            duration: Animation duration in seconds
            fps: Frames per second
            save_path: Optional path to save the animation
            
        Returns:
            Matplotlib animation
        """
        if not case_history:
            logger.warning("No case history provided for animation")
            return None
        
        fig, (ax1, ax2) = plt.subplots(2, 1, figsize=self.figsize, dpi=self.dpi)
        
        # Set up timeline plot
        timestamps = [entry.get('timestamp', i) for i, entry in enumerate(case_history)]
        cases = [entry.get('case', Case.NOMINATIVE) for entry in case_history]
        
        # Normalize timestamps
        start_time = min(timestamps)
        normalized_times = [t - start_time for t in timestamps]
        
        # Create case timeline
        case_values = [case.value if isinstance(case, Case) else str(case) for case in cases]
        unique_cases = list(set(case_values))
        case_indices = [unique_cases.index(case) for case in case_values]
        
        ax1.plot(normalized_times, case_indices, 'o-', linewidth=2, markersize=4)
        ax1.set_ylabel('Case')
        ax1.set_title('Case Transitions Over Time')
        ax1.set_yticks(range(len(unique_cases)))
        ax1.set_yticklabels(unique_cases)
        ax1.grid(True, alpha=0.3)
        
        # Set up performance plot
        performances = [entry.get('performance', {}).get('total_actions', 0) for entry in case_history]
        
        performance_line, = ax2.plot([], [], 'r-', linewidth=2)
        performance_scatter = ax2.scatter([], [], c='red', s=50, alpha=0.8)
        
        ax2.set_xlabel('Time (seconds)')
        ax2.set_ylabel('Performance')
        ax2.set_title('Performance Over Time')
        ax2.grid(True, alpha=0.3)
        
        # Set limits
        ax2.set_xlim(0, max(normalized_times) if normalized_times else 10)
        ax2.set_ylim(0, max(performances) * 1.1 if performances else 10)
        
        def animate(frame):
            if frame < len(case_history):
                # Update performance plot
                current_times = normalized_times[:frame + 1]
                current_performances = performances[:frame + 1]
                
                if current_times and current_performances:
                    performance_line.set_data(current_times, current_performances)
                    performance_scatter.set_offsets(np.column_stack([current_times, current_performances]))
            
            return performance_line, performance_scatter
        
        anim = animation.FuncAnimation(fig, animate, frames=int(duration * fps), 
                                     interval=1000//fps, blit=True)
        
        if save_path:
            anim.save(save_path, writer='pillow', fps=fps)
        
        return anim
    
    def create_neural_activity_animation(self, neural_history: List[Dict[str, Any]],
                                       duration: float = 10.0,
                                       fps: int = 10,
                                       save_path: Optional[str] = None) -> animation.Animation:
        """
        Create animation of neural activity over time.
        
        Args:
            neural_history: List of neural activity states over time
            duration: Animation duration in seconds
            fps: Frames per second
            save_path: Optional path to save the animation
            
        Returns:
            Matplotlib animation
        """
        if not neural_history:
            logger.warning("No neural history provided for animation")
            return None
        
        fig, axes = plt.subplots(2, 2, figsize=self.figsize, dpi=self.dpi)
        axes = axes.flatten()
        
        # Get all neural structure names
        all_structures = set()
        for entry in neural_history:
            if 'activity' in entry:
                all_structures.update(entry['activity'].keys())
        
        if not all_structures:
            logger.warning("No neural structures found in history")
            return None
        
        # Initialize plots for each structure
        structure_plots = {}
        for i, structure_name in enumerate(list(all_structures)[:4]):  # Limit to 4 structures
            if i >= len(axes):
                break
            
            ax = axes[i]
            line, = ax.plot([], [], 'o-', linewidth=2, markersize=3)
            ax.set_title(f'{structure_name} Activity')
            ax.set_ylabel('Activity Level')
            ax.grid(True, alpha=0.3)
            
            structure_plots[structure_name] = line
        
        # Set up timeline
        timestamps = [entry.get('timestamp', i) for i, entry in enumerate(neural_history)]
        if timestamps:
            start_time = min(timestamps)
            normalized_times = [t - start_time for t in timestamps]
            max_time = max(normalized_times) if normalized_times else 10
        else:
            max_time = 10
        
        for ax in axes:
            ax.set_xlim(0, max_time)
            ax.set_ylim(0, 1)  # Assuming normalized activity
        
        def animate(frame):
            if frame < len(neural_history):
                entry = neural_history[frame]
                
                if 'activity' in entry:
                    current_time = normalized_times[frame] if frame < len(normalized_times) else frame
                    
                    for structure_name, line in structure_plots.items():
                        if structure_name in entry['activity']:
                            activity = entry['activity'][structure_name]
                            
                            if isinstance(activity, np.ndarray):
                                if activity.ndim > 1:
                                    # Use mean activity for 2D arrays
                                    mean_activity = np.mean(activity, axis=1)
                                    line.set_data(range(len(mean_activity)), mean_activity)
                                else:
                                    # Use 1D activity directly
                                    line.set_data(range(len(activity)), activity)
                            else:
                                # Scalar activity
                                line.set_data([0], [float(activity)])
            
            return list(structure_plots.values())
        
        anim = animation.FuncAnimation(fig, animate, frames=int(duration * fps), 
                                     interval=1000//fps, blit=True)
        
        if save_path:
            anim.save(save_path, writer='pillow', fps=fps)
        
        return anim


class SwarmAnimationCreator:
    """
    Creator for swarm dynamics animations.
    """
    
    def __init__(self, figsize: Tuple[int, int] = (15, 10), dpi: int = 100):
        """
        Initialize the swarm animation creator.
        
        Args:
            figsize: Figure size
            dpi: DPI for figures
        """
        self.figsize = figsize
        self.dpi = dpi
        
        # Color schemes
        self.case_colors = {
            Case.NOMINATIVE: '#1f77b4',
            Case.ACCUSATIVE: '#ff7f0e',
            Case.DATIVE: '#2ca02c',
            Case.GENITIVE: '#d62728',
            Case.INSTRUMENTAL: '#9467bd',
            Case.LOCATIVE: '#8c564b',
            Case.ABLATIVE: '#e377c2',
            Case.VOCATIVE: '#7f7f7f'
        }
        
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
        
        logger.info("Initialized SwarmAnimationCreator")
    
    def create_swarm_animation(self, swarm_history: List[Dict[str, Any]],
                             environment: Optional[Dict[str, Any]] = None,
                             duration: float = 10.0,
                             fps: int = 10,
                             save_path: Optional[str] = None) -> animation.Animation:
        """
        Create animation of swarm dynamics over time.
        
        Args:
            swarm_history: List of swarm states over time
            environment: Optional environment data
            duration: Animation duration in seconds
            fps: Frames per second
            save_path: Optional path to save the animation
            
        Returns:
            Matplotlib animation
        """
        if not swarm_history:
            logger.warning("No swarm history provided for animation")
            return None
        
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=self.figsize, dpi=self.dpi)
        
        # Set up swarm position plot
        all_positions = []
        for entry in swarm_history:
            if 'insect_positions' in entry:
                positions = entry['insect_positions']
                for pos in positions:
                    if isinstance(pos, (list, np.ndarray)) and len(pos) >= 2:
                        all_positions.append(pos[:2])
        
        if all_positions:
            all_positions = np.array(all_positions)
            x_min, x_max = all_positions[:, 0].min(), all_positions[:, 0].max()
            y_min, y_max = all_positions[:, 1].min(), all_positions[:, 1].max()
            
            # Add margin
            margin = max(x_max - x_min, y_max - y_min) * 0.1
            ax1.set_xlim(x_min - margin, x_max + margin)
            ax1.set_ylim(y_min - margin, y_max + margin)
        else:
            ax1.set_xlim(-10, 10)
            ax1.set_ylim(-10, 10)
        
        # Plot environment features
        if environment and 'obstacles' in environment:
            for obs in environment['obstacles']:
                if len(obs) >= 2:
                    circle = Circle(obs[:2], 0.5, color='gray', alpha=0.7)
                    ax1.add_patch(circle)
        
        # Initialize swarm scatter plot
        swarm_scatter = ax1.scatter([], [], alpha=0.8, s=50)
        center_scatter = ax1.scatter([], [], c='red', s=200, marker='*', alpha=0.8)
        
        ax1.set_title('Swarm Positions')
        ax1.set_xlabel('X Position')
        ax1.set_ylabel('Y Position')
        ax1.grid(True, alpha=0.3)
        ax1.set_aspect('equal')
        
        # Set up metrics plot
        timestamps = [entry.get('timestamp', i) for i, entry in enumerate(swarm_history)]
        dispersions = [entry.get('swarm_dispersion', 0) for entry in swarm_history]
        cohesions = [entry.get('swarm_cohesion', 0) for entry in swarm_history]
        
        # Normalize timestamps
        if timestamps:
            start_time = min(timestamps)
            normalized_times = [t - start_time for t in timestamps]
        else:
            normalized_times = list(range(len(swarm_history)))
        
        dispersion_line, = ax2.plot([], [], 'b-', linewidth=2, label='Dispersion')
        cohesion_line, = ax2.plot([], [], 'r-', linewidth=2, label='Cohesion')
        
        ax2.set_xlabel('Time (seconds)')
        ax2.set_ylabel('Metric Value')
        ax2.set_title('Swarm Metrics')
        ax2.legend()
        ax2.grid(True, alpha=0.3)
        
        # Set limits for metrics plot
        if dispersions and cohesions:
            ax2.set_xlim(0, max(normalized_times) if normalized_times else 10)
            ax2.set_ylim(0, max(max(dispersions), max(cohesions)) * 1.1)
        else:
            ax2.set_xlim(0, 10)
            ax2.set_ylim(0, 1)
        
        def animate(frame):
            if frame < len(swarm_history):
                entry = swarm_history[frame]
                
                # Update swarm positions
                if 'insect_positions' in entry:
                    positions = entry['insect_positions']
                    positions_2d = []
                    colors = []
                    
                    for i, pos in enumerate(positions):
                        if isinstance(pos, (list, np.ndarray)) and len(pos) >= 2:
                            positions_2d.append(pos[:2])
                            
                            # Color by case if available
                            if 'insect_cases' in entry and i < len(entry['insect_cases']):
                                case = entry['insect_cases'][i]
                                if isinstance(case, Case):
                                    colors.append(self.case_colors.get(case, '#cccccc'))
                                else:
                                    colors.append('#cccccc')
                            else:
                                colors.append('#cccccc')
                    
                    if positions_2d:
                        positions_2d = np.array(positions_2d)
                        swarm_scatter.set_offsets(positions_2d)
                        swarm_scatter.set_color(colors)
                
                # Update swarm center
                if 'swarm_center' in entry:
                    center = entry['swarm_center']
                    if isinstance(center, (list, np.ndarray)) and len(center) >= 2:
                        center_scatter.set_offsets([center[:2]])
                
                # Update metrics
                current_times = normalized_times[:frame + 1]
                current_dispersions = dispersions[:frame + 1]
                current_cohesions = cohesions[:frame + 1]
                
                if current_times and current_dispersions:
                    dispersion_line.set_data(current_times, current_dispersions)
                
                if current_times and current_cohesions:
                    cohesion_line.set_data(current_times, current_cohesions)
            
            return swarm_scatter, center_scatter, dispersion_line, cohesion_line
        
        anim = animation.FuncAnimation(fig, animate, frames=int(duration * fps), 
                                     interval=1000//fps, blit=True)
        
        if save_path:
            anim.save(save_path, writer='pillow', fps=fps)
        
        return anim
    
    def create_collective_behavior_animation(self, behavior_history: List[Dict[str, Any]],
                                           duration: float = 10.0,
                                           fps: int = 10,
                                           save_path: Optional[str] = None) -> animation.Animation:
        """
        Create animation of collective behavior patterns.
        
        Args:
            behavior_history: List of collective behavior states over time
            duration: Animation duration in seconds
            fps: Frames per second
            save_path: Optional path to save the animation
            
        Returns:
            Matplotlib animation
        """
        if not behavior_history:
            logger.warning("No behavior history provided for animation")
            return None
        
        fig, axes = plt.subplots(2, 2, figsize=self.figsize, dpi=self.dpi)
        
        # Set up behavior distribution plot
        behavior_distribution = axes[0, 0].bar([], [], alpha=0.7)
        axes[0, 0].set_title('Behavior Distribution')
        axes[0, 0].set_ylabel('Count')
        axes[0, 0].set_xticklabels([], rotation=45, ha='right')
        
        # Set up case distribution plot
        case_distribution = axes[0, 1].bar([], [], alpha=0.7)
        axes[0, 1].set_title('Case Distribution')
        axes[0, 1].set_ylabel('Count')
        axes[0, 1].set_xticklabels([], rotation=45, ha='right')
        
        # Set up performance timeline
        performance_line, = axes[1, 0].plot([], [], 'g-', linewidth=2)
        axes[1, 0].set_title('Collective Performance')
        axes[1, 0].set_xlabel('Time')
        axes[1, 0].set_ylabel('Performance')
        axes[1, 0].grid(True, alpha=0.3)
        
        # Set up behavior transitions
        transition_matrix = axes[1, 1].imshow(np.zeros((5, 5)), cmap='Blues', aspect='auto')
        axes[1, 1].set_title('Behavior Transitions')
        axes[1, 1].set_xlabel('To Behavior')
        axes[1, 1].set_ylabel('From Behavior')
        
        def animate(frame):
            if frame < len(behavior_history):
                entry = behavior_history[frame]
                
                # Update behavior distribution
                if 'behavioral_states' in entry:
                    states = entry['behavioral_states']
                    state_counts = defaultdict(int)
                    for state in states:
                        if isinstance(state, BehavioralState):
                            state_counts[state.value] += 1
                    
                    if state_counts:
                        state_names = list(state_counts.keys())
                        counts = list(state_counts.values())
                        colors = [self.behavior_colors.get(BehavioralState(state), '#cccccc') for state in state_names]
                        
                        axes[0, 0].clear()
                        bars = axes[0, 0].bar(range(len(counts)), counts, color=colors, alpha=0.7)
                        axes[0, 0].set_title('Behavior Distribution')
                        axes[0, 0].set_ylabel('Count')
                        axes[0, 0].set_xticks(range(len(state_names)))
                        axes[0, 0].set_xticklabels(state_names, rotation=45, ha='right')
                
                # Update case distribution
                if 'insect_cases' in entry:
                    cases = entry['insect_cases']
                    case_counts = defaultdict(int)
                    for case in cases:
                        if isinstance(case, Case):
                            case_counts[case.value] += 1
                    
                    if case_counts:
                        case_names = list(case_counts.keys())
                        counts = list(case_counts.values())
                        colors = [self.case_colors.get(Case(case), '#cccccc') for case in case_names]
                        
                        axes[0, 1].clear()
                        bars = axes[0, 1].bar(range(len(counts)), counts, color=colors, alpha=0.7)
                        axes[0, 1].set_title('Case Distribution')
                        axes[0, 1].set_ylabel('Count')
                        axes[0, 1].set_xticks(range(len(case_names)))
                        axes[0, 1].set_xticklabels(case_names, rotation=45, ha='right')
                
                # Update performance timeline
                if 'collective_performance' in entry:
                    performance = entry['collective_performance']
                    current_time = frame
                    
                    if frame == 0:
                        performance_line.set_data([current_time], [performance])
                    else:
                        x_data = list(performance_line.get_xdata()) + [current_time]
                        y_data = list(performance_line.get_ydata()) + [performance]
                        performance_line.set_data(x_data, y_data)
            
            return performance_line,
        
        anim = animation.FuncAnimation(fig, animate, frames=int(duration * fps), 
                                     interval=1000//fps, blit=True)
        
        if save_path:
            anim.save(save_path, writer='pillow', fps=fps)
        
        return anim 