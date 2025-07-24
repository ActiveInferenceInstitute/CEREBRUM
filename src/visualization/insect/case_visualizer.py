"""
Case Visualizer for Insect Models

This module provides visualization tools for insect case-based reasoning,
including case transitions, effectiveness analysis, and case relevance tracking.
"""

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib.patches import Circle, Rectangle, Polygon
from matplotlib.collections import LineCollection
import seaborn as sns
from typing import Dict, Any, List, Optional, Tuple, Union
import logging
import os
import time
from collections import defaultdict, deque

from src.core.model import Case
from src.models.insect.base import InsectModel, BehavioralState

logger = logging.getLogger(__name__)


class InsectCaseVisualizer:
    """
    Visualizer for insect case-based reasoning analysis.
    """
    
    def __init__(self, figsize: Tuple[int, int] = (12, 8), dpi: int = 100):
        """
        Initialize the insect case visualizer.
        
        Args:
            figsize: Figure size
            dpi: DPI for figures
        """
        self.figsize = figsize
        self.dpi = dpi
        self.case_history = deque(maxlen=1000)
        self.case_performance = defaultdict(list)
        
        # Color scheme for cases
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
        
        logger.info("Initialized InsectCaseVisualizer")
    
    def track_case_usage(self, insect: InsectModel, context: Dict[str, Any]):
        """
        Track case usage and performance.
        
        Args:
            insect: The insect model
            context: Current context information
        """
        entry = {
            'timestamp': time.time(),
            'case': insect.current_case,
            'behavioral_state': insect.behavioral_state,
            'context': context,
            'performance': insect.get_performance_summary()
        }
        
        self.case_history.append(entry)
        self.case_performance[insect.current_case].append(entry['performance']['total_actions'])
    
    def visualize_case_distribution(self, save_path: Optional[str] = None) -> plt.Figure:
        """
        Visualize case distribution and usage patterns.
        
        Args:
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        fig, axes = plt.subplots(2, 2, figsize=self.figsize, dpi=self.dpi)
        
        if not self.case_history:
            # Create a simple visualization with available data
            for ax in axes.flatten():
                ax.text(0.5, 0.5, 'Case data being collected...', ha='center', va='center', transform=ax.transAxes)
            axes[0, 0].set_title('Case Distribution')
            axes[0, 1].set_title('Case Timeline')
            axes[1, 0].set_title('Case Performance')
            axes[1, 1].set_title('Case Transitions')
            plt.tight_layout()
            if save_path:
                plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
            return fig
        
        # Extract data from case history
        cases = [entry['case'] for entry in self.case_history]
        case_names = [case.value for case in cases]
        timestamps = [entry['timestamp'] for entry in self.case_history]
        
        # Plot case frequency
        case_counts = {}
        for case in cases:
            case_counts[case.value] = case_counts.get(case.value, 0) + 1
        
        if case_counts:
            names = list(case_counts.keys())
            counts = list(case_counts.values())
            colors = [self.case_colors.get(Case(name), '#cccccc') for name in names]
            axes[0, 0].bar(names, counts, color=colors, alpha=0.7)
            axes[0, 0].set_title('Case Usage Frequency')
            axes[0, 0].set_ylabel('Frequency')
            axes[0, 0].tick_params(axis='x', rotation=45)
        
        # Plot case timeline
        if len(case_names) > 0:
            unique_cases = list(set(case_names))
            case_indices = [unique_cases.index(case) for case in case_names]
            axes[0, 1].plot(range(len(case_indices)), case_indices, 'b-', linewidth=2, marker='o', markersize=4)
            axes[0, 1].set_title('Case Timeline')
            axes[0, 1].set_xlabel('Event Number')
            axes[0, 1].set_ylabel('Case Index')
            axes[0, 1].set_yticks(range(len(unique_cases)))
            axes[0, 1].set_yticklabels(unique_cases)
            axes[0, 1].grid(True, alpha=0.3)
        
        # Plot case performance
        if self.case_performance:
            case_perf = {}
            for case, performances in self.case_performance.items():
                if performances:
                    case_perf[case.value] = np.mean(performances)
            
            if case_perf:
                perf_names = list(case_perf.keys())
                perf_values = list(case_perf.values())
                colors = [self.case_colors.get(Case(name), '#cccccc') for name in perf_names]
                axes[1, 0].bar(perf_names, perf_values, color=colors, alpha=0.7)
                axes[1, 0].set_title('Case Performance')
                axes[1, 0].set_ylabel('Average Performance')
                axes[1, 0].tick_params(axis='x', rotation=45)
        
        # Plot case transitions
        if len(cases) > 1:
            transition_counts = {}
            for i in range(1, len(cases)):
                if cases[i] != cases[i-1]:
                    transition = f"{cases[i-1].value}→{cases[i].value}"
                    transition_counts[transition] = transition_counts.get(transition, 0) + 1
            
            if transition_counts:
                transition_names = list(transition_counts.keys())
                transition_values = list(transition_counts.values())
                axes[1, 1].bar(range(len(transition_names)), transition_values, color='lightcoral', alpha=0.7)
                axes[1, 1].set_title('Case Transitions')
                axes[1, 1].set_ylabel('Count')
                axes[1, 1].set_xticks(range(len(transition_names)))
                axes[1, 1].set_xticklabels(transition_names, rotation=45, ha='right')
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        
        return fig
    
    def generate_comprehensive_visualizations(self, output_dir: str):
        """
        Generate comprehensive case analysis visualizations.
        
        Args:
            output_dir: Output directory for visualizations
        """
        case_dir = os.path.join(output_dir, "visualizations", "case_analysis")
        os.makedirs(case_dir, exist_ok=True)
        
        if not self.case_history:
            print("    No case data available for visualization")
            return
        
        # Generate case transition analysis
        fig, axes = plt.subplots(2, 2, figsize=(15, 12))
        fig.suptitle('Comprehensive Case Analysis', fontsize=16)
        
        # Extract data from case history
        cases = [entry['case'] for entry in self.case_history]
        case_names = [case.value for case in cases]
        timestamps = [entry['timestamp'] for entry in self.case_history]
        
        # Plot case frequency
        case_counts = {}
        for case in cases:
            case_counts[case.value] = case_counts.get(case.value, 0) + 1
        
        if case_counts:
            names = list(case_counts.keys())
            counts = list(case_counts.values())
            colors = [self.case_colors.get(Case(name), '#cccccc') for name in names]
            axes[0, 0].bar(names, counts, color=colors, alpha=0.7)
            axes[0, 0].set_title('Case Usage Frequency')
            axes[0, 0].set_ylabel('Frequency')
            axes[0, 0].tick_params(axis='x', rotation=45)
        
        # Plot case transitions over time
        if len(cases) > 1:
            transition_points = []
            transition_cases = []
            for i in range(1, len(cases)):
                if cases[i] != cases[i-1]:
                    transition_points.append(timestamps[i])
                    transition_cases.append(f"{cases[i-1].value}→{cases[i].value}")
            
            if transition_points:
                axes[0, 1].plot(transition_points, range(len(transition_points)), 'ro-', markersize=8)
                axes[0, 1].set_title('Case Transitions Over Time')
                axes[0, 1].set_xlabel('Timestamp')
                axes[0, 1].set_ylabel('Transition Number')
                axes[0, 1].grid(True, alpha=0.3)
        
        # Plot case performance
        if self.case_performance:
            case_perf = {}
            for case, performances in self.case_performance.items():
                if performances:
                    case_perf[case.value] = np.mean(performances)
            
            if case_perf:
                perf_names = list(case_perf.keys())
                perf_values = list(case_perf.values())
                colors = [self.case_colors.get(Case(name), '#cccccc') for name in perf_names]
                axes[1, 0].bar(perf_names, perf_values, color=colors, alpha=0.7)
                axes[1, 0].set_title('Case Performance')
                axes[1, 0].set_ylabel('Average Performance')
                axes[1, 0].tick_params(axis='x', rotation=45)
        
        # Plot case timeline
        if len(case_names) > 0:
            unique_cases = list(set(case_names))
            case_indices = [unique_cases.index(case) for case in case_names]
            axes[1, 1].plot(range(len(case_indices)), case_indices, 'b-', linewidth=2, marker='o', markersize=4)
            axes[1, 1].set_title('Case Timeline')
            axes[1, 1].set_xlabel('Event Number')
            axes[1, 1].set_ylabel('Case Index')
            axes[1, 1].set_yticks(range(len(unique_cases)))
            axes[1, 1].set_yticklabels(unique_cases)
            axes[1, 1].grid(True, alpha=0.3)
        
        plt.tight_layout()
        plt.savefig(os.path.join(case_dir, 'case_transition_analysis.png'), dpi=300, bbox_inches='tight')
        plt.close(fig)
        
        # Generate case effectiveness analysis
        fig, ax = plt.subplots(figsize=(12, 8))
        
        if case_counts:
            effectiveness = {}
            for case_name in case_counts.keys():
                # Calculate effectiveness based on usage frequency and performance
                usage_freq = case_counts[case_name] / len(cases)
                effectiveness[case_name] = usage_freq * 100  # Convert to percentage
            
            eff_names = list(effectiveness.keys())
            eff_values = list(effectiveness.values())
            colors = [self.case_colors.get(Case(name), '#cccccc') for name in eff_names]
            
            bars = ax.bar(eff_names, eff_values, color=colors, alpha=0.7)
            ax.set_title('Case Effectiveness Analysis')
            ax.set_ylabel('Effectiveness Score')
            ax.tick_params(axis='x', rotation=45)
            
            # Add value labels on bars
            for bar, value in zip(bars, eff_values):
                height = bar.get_height()
                ax.text(bar.get_x() + bar.get_width()/2., height + 0.01,
                       f'{value:.1f}%', ha='center', va='bottom')
        
        plt.tight_layout()
        plt.savefig(os.path.join(case_dir, 'case_effectiveness_analysis.png'), dpi=300, bbox_inches='tight')
        plt.close(fig)
        
        # Generate case distribution pie chart
        if case_counts:
            fig, ax = plt.subplots(figsize=(10, 8))
            
            labels = list(case_counts.keys())
            sizes = list(case_counts.values())
            colors = [self.case_colors.get(Case(name), '#cccccc') for name in labels]
            
            wedges, texts, autotexts = ax.pie(sizes, labels=labels, colors=colors, autopct='%1.1f%%',
                                             startangle=90)
            ax.set_title('Case Distribution')
            
            plt.tight_layout()
            plt.savefig(os.path.join(case_dir, 'case_distribution_pie.png'), dpi=300, bbox_inches='tight')
            plt.close(fig)
        
        print(f"    Generated case analysis visualizations in {case_dir}")
    
    def visualize_case_effectiveness(self, insect: InsectModel, 
                                   context: Dict[str, Any],
                                   save_path: Optional[str] = None) -> plt.Figure:
        """
        Visualize case effectiveness in different contexts.
        
        Args:
            insect: The insect model
            context: Current context
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        fig, axes = plt.subplots(2, 2, figsize=self.figsize, dpi=self.dpi)
        
        # Test each case in the current context
        case_effectiveness = {}
        original_case = insect.current_case
        
        for case in Case:
            # Transform to case
            insect.transform_case(case)
            
            # Measure performance
            performance = insect.get_performance_summary()
            case_effectiveness[case] = performance['total_actions']
            
            # Restore original case
            insect.transform_case(original_case)
        
        # Plot case effectiveness
        cases = list(case_effectiveness.keys())
        effectiveness = list(case_effectiveness.values())
        colors = [self.case_colors.get(case, '#cccccc') for case in cases]
        
        bars = axes[0, 0].bar(range(len(cases)), effectiveness, color=colors, alpha=0.7)
        axes[0, 0].set_title('Case Effectiveness')
        axes[0, 0].set_ylabel('Performance Score')
        axes[0, 0].set_xticks(range(len(cases)))
        axes[0, 0].set_xticklabels([case.value for case in cases], rotation=45, ha='right')
        
        # Highlight current case
        current_idx = cases.index(original_case)
        bars[current_idx].set_edgecolor('red')
        bars[current_idx].set_linewidth(3)
        
        # Plot context analysis
        self._plot_context_analysis(axes[0, 1], context)
        
        # Plot case recommendations
        self._plot_case_recommendations(axes[1, 0], case_effectiveness, original_case)
        
        # Plot case transitions
        self._plot_case_transitions(axes[1, 1])
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        
        return fig
    
    def _plot_context_analysis(self, ax: plt.Axes, context: Dict[str, Any]):
        """Plot context analysis."""
        if not context:
            ax.text(0.5, 0.5, 'No context data', ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Context Analysis')
            return
        
        # Extract context features
        context_items = list(context.items())
        if len(context_items) > 8:  # Limit to 8 items
            context_items = context_items[:8]
        
        feature_names = [item[0] for item in context_items]
        feature_values = [str(item[1]) for item in context_items]
        
        # Create text display
        text_content = "Context Features:\n"
        for name, value in zip(feature_names, feature_values):
            text_content += f"{name}: {value}\n"
        
        ax.text(0.1, 0.9, text_content, transform=ax.transAxes, fontsize=10, 
                verticalalignment='top', bbox=dict(boxstyle='round', facecolor='lightblue', alpha=0.5))
        ax.set_title('Context Analysis')
        ax.set_xlim(0, 1)
        ax.set_ylim(0, 1)
        ax.axis('off')
    
    def _plot_case_recommendations(self, ax: plt.Axes, case_effectiveness: Dict[Case, float], 
                                 current_case: Case):
        """Plot case transition recommendations."""
        # Find best case
        sorted_cases = sorted(case_effectiveness.items(), key=lambda x: x[1], reverse=True)
        
        if len(sorted_cases) > 1:
            best_case, best_score = sorted_cases[0]
            current_score = case_effectiveness[current_case]
            
            if best_case != current_case:
                improvement = best_score - current_score
                
                text_content = f"Current Case: {current_case.value}\n"
                text_content += f"Current Score: {current_score:.2f}\n"
                text_content += f"Recommended: {best_case.value}\n"
                text_content += f"Potential Improvement: {improvement:.2f}"
                
                # Color code the recommendation
                if improvement > 0:
                    bg_color = 'lightgreen'
                else:
                    bg_color = 'lightcoral'
            else:
                text_content = f"Optimal case already selected:\n{current_case.value}"
                bg_color = 'lightblue'
        else:
            text_content = "Insufficient data for recommendations"
            bg_color = 'lightgray'
        
        ax.text(0.1, 0.5, text_content, transform=ax.transAxes, fontsize=12, 
                verticalalignment='center', bbox=dict(boxstyle='round', facecolor=bg_color, alpha=0.7))
        ax.set_title('Case Recommendations')
        ax.set_xlim(0, 1)
        ax.set_ylim(0, 1)
        ax.axis('off')
    
    def _plot_case_transitions(self, ax: plt.Axes):
        """Plot case transition patterns."""
        if len(self.case_history) < 2:
            ax.text(0.5, 0.5, 'Insufficient data for transition analysis', ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Case Transitions')
            return
        
        # Create transition matrix
        cases = list(set(entry['case'] for entry in self.case_history))
        transition_matrix = np.zeros((len(cases), len(cases)))
        
        for i in range(len(self.case_history) - 1):
            current_case = self.case_history[i]['case']
            next_case = self.case_history[i + 1]['case']
            
            current_idx = cases.index(current_case)
            next_idx = cases.index(next_case)
            
            transition_matrix[current_idx, next_idx] += 1
        
        # Plot transition matrix
        im = ax.imshow(transition_matrix, cmap='Blues', aspect='auto')
        ax.set_title('Case Transition Matrix')
        ax.set_xlabel('To Case')
        ax.set_ylabel('From Case')
        ax.set_xticks(range(len(cases)))
        ax.set_yticks(range(len(cases)))
        ax.set_xticklabels([case.value for case in cases], rotation=45, ha='right')
        ax.set_yticklabels([case.value for case in cases])
        plt.colorbar(im, ax=ax)


class CaseTransitionVisualizer:
    """
    Specialized visualizer for case transition analysis.
    """
    
    def __init__(self, figsize: Tuple[int, int] = (12, 8), dpi: int = 100):
        """
        Initialize the case transition visualizer.
        
        Args:
            figsize: Figure size
            dpi: DPI for figures
        """
        self.figsize = figsize
        self.dpi = dpi
        self.transition_history = deque(maxlen=1000)
        
        logger.info("Initialized CaseTransitionVisualizer")
    
    def track_transition(self, from_case: Case, to_case: Case, 
                        context: Dict[str, Any], success: bool):
        """
        Track a case transition.
        
        Args:
            from_case: Original case
            to_case: Target case
            context: Context during transition
            success: Whether transition was successful
        """
        entry = {
            'timestamp': time.time(),
            'from_case': from_case,
            'to_case': to_case,
            'context': context,
            'success': success
        }
        
        self.transition_history.append(entry)
    
    def visualize_transition_patterns(self, save_path: Optional[str] = None) -> plt.Figure:
        """
        Visualize case transition patterns.
        
        Args:
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        if not self.transition_history:
            fig, ax = plt.subplots(figsize=self.figsize, dpi=self.dpi)
            ax.text(0.5, 0.5, 'No transition data available', ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Case Transition Patterns')
            return fig
        
        fig, axes = plt.subplots(2, 2, figsize=self.figsize, dpi=self.dpi)
        
        # Extract data
        from_cases = [entry['from_case'] for entry in self.transition_history]
        to_cases = [entry['to_case'] for entry in self.transition_history]
        successes = [entry['success'] for entry in self.transition_history]
        
        # Plot transition frequency
        transition_counts = defaultdict(int)
        for from_case, to_case in zip(from_cases, to_cases):
            transition_counts[(from_case, to_case)] += 1
        
        if transition_counts:
            transitions = list(transition_counts.keys())
            counts = list(transition_counts.values())
            
            # Create labels
            labels = [f"{f.value}→{t.value}" for f, t in transitions]
            
            axes[0, 0].bar(range(len(counts)), counts, alpha=0.7)
            axes[0, 0].set_title('Transition Frequency')
            axes[0, 0].set_ylabel('Count')
            axes[0, 0].set_xticks(range(len(labels)))
            axes[0, 0].set_xticklabels(labels, rotation=45, ha='right')
        
        # Plot success rate by transition
        success_by_transition = defaultdict(list)
        for (from_case, to_case), success in zip(zip(from_cases, to_cases), successes):
            success_by_transition[(from_case, to_case)].append(success)
        
        if success_by_transition:
            transitions = list(success_by_transition.keys())
            success_rates = [np.mean(success_by_transition[t]) for t in transitions]
            labels = [f"{f.value}→{t.value}" for f, t in transitions]
            
            axes[0, 1].bar(range(len(success_rates)), success_rates, alpha=0.7, color='green')
            axes[0, 1].set_title('Transition Success Rate')
            axes[0, 1].set_ylabel('Success Rate')
            axes[0, 1].set_xticks(range(len(labels)))
            axes[0, 1].set_xticklabels(labels, rotation=45, ha='right')
        
        # Plot transition timeline
        timestamps = [entry['timestamp'] for entry in self.transition_history]
        transition_values = [f"{entry['from_case'].value}→{entry['to_case'].value}" for entry in self.transition_history]
        
        # Normalize timestamps
        start_time = min(timestamps)
        normalized_times = [t - start_time for t in timestamps]
        
        unique_transitions = list(set(transition_values))
        transition_indices = [unique_transitions.index(t) for t in transition_values]
        
        axes[1, 0].plot(normalized_times, transition_indices, 'o-', linewidth=2, markersize=4)
        axes[1, 0].set_title('Transition Timeline')
        axes[1, 0].set_xlabel('Time (seconds)')
        axes[1, 0].set_ylabel('Transition Type')
        axes[1, 0].set_yticks(range(len(unique_transitions)))
        axes[1, 0].set_yticklabels(unique_transitions)
        axes[1, 0].grid(True, alpha=0.3)
        
        # Plot overall success rate
        overall_success_rate = np.mean(successes)
        axes[1, 1].pie([overall_success_rate, 1 - overall_success_rate], 
                      labels=['Success', 'Failure'], autopct='%1.1f%%')
        axes[1, 1].set_title('Overall Transition Success Rate')
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        
        return fig


class CaseEffectivenessVisualizer:
    """
    Specialized visualizer for case effectiveness analysis.
    """
    
    def __init__(self, figsize: Tuple[int, int] = (12, 8), dpi: int = 100):
        """
        Initialize the case effectiveness visualizer.
        
        Args:
            figsize: Figure size
            dpi: DPI for figures
        """
        self.figsize = figsize
        self.dpi = dpi
        self.effectiveness_data = defaultdict(list)
        
        logger.info("Initialized CaseEffectivenessVisualizer")
    
    def track_effectiveness(self, case: Case, performance: Dict[str, float], 
                          context: Dict[str, Any]):
        """
        Track case effectiveness.
        
        Args:
            case: The case being evaluated
            performance: Performance metrics
            context: Context information
        """
        entry = {
            'timestamp': time.time(),
            'performance': performance,
            'context': context
        }
        
        self.effectiveness_data[case].append(entry)
    
    def visualize_effectiveness_trends(self, save_path: Optional[str] = None) -> plt.Figure:
        """
        Visualize case effectiveness trends over time.
        
        Args:
            save_path: Optional path to save the figure
            
        Returns:
            Matplotlib figure
        """
        if not self.effectiveness_data:
            fig, ax = plt.subplots(figsize=self.figsize, dpi=self.dpi)
            ax.text(0.5, 0.5, 'No effectiveness data available', ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Case Effectiveness Trends')
            return fig
        
        fig, axes = plt.subplots(2, 2, figsize=self.figsize, dpi=self.dpi)
        
        # Plot effectiveness over time for each case
        for case, data in self.effectiveness_data.items():
            if data:
                timestamps = [entry['timestamp'] for entry in data]
                performances = [entry['performance'].get('total_actions', 0) for entry in data]
                
                # Normalize timestamps
                start_time = min(timestamps)
                normalized_times = [t - start_time for t in timestamps]
                
                axes[0, 0].plot(normalized_times, performances, 'o-', linewidth=2, markersize=3, 
                               label=case.value)
        
        axes[0, 0].set_title('Case Effectiveness Over Time')
        axes[0, 0].set_xlabel('Time (seconds)')
        axes[0, 0].set_ylabel('Performance')
        axes[0, 0].legend()
        axes[0, 0].grid(True, alpha=0.3)
        
        # Plot average effectiveness by case
        avg_effectiveness = {}
        for case, data in self.effectiveness_data.items():
            if data:
                performances = [entry['performance'].get('total_actions', 0) for entry in data]
                avg_effectiveness[case] = np.mean(performances)
        
        if avg_effectiveness:
            cases = list(avg_effectiveness.keys())
            effectiveness = list(avg_effectiveness.values())
            
            axes[0, 1].bar(range(len(cases)), effectiveness, alpha=0.7)
            axes[0, 1].set_title('Average Case Effectiveness')
            axes[0, 1].set_ylabel('Average Performance')
            axes[0, 1].set_xticks(range(len(cases)))
            axes[0, 1].set_xticklabels([case.value for case in cases], rotation=45, ha='right')
        
        # Plot effectiveness distribution
        all_performances = []
        for data in self.effectiveness_data.values():
            performances = [entry['performance'].get('total_actions', 0) for entry in data]
            all_performances.extend(performances)
        
        if all_performances:
            axes[1, 0].hist(all_performances, bins=20, alpha=0.7, color='skyblue')
            axes[1, 0].set_title('Performance Distribution')
            axes[1, 0].set_xlabel('Performance')
            axes[1, 0].set_ylabel('Frequency')
        
        # Plot case comparison
        if len(self.effectiveness_data) > 1:
            cases = list(self.effectiveness_data.keys())
            case_performances = []
            case_labels = []
            
            for case in cases:
                data = self.effectiveness_data[case]
                if data:
                    performances = [entry['performance'].get('total_actions', 0) for entry in data]
                    case_performances.append(performances)
                    case_labels.append(case.value)
            
            if case_performances:
                axes[1, 1].boxplot(case_performances, labels=case_labels)
                axes[1, 1].set_title('Case Performance Comparison')
                axes[1, 1].set_ylabel('Performance')
                plt.setp(axes[1, 1].get_xticklabels(), rotation=45, ha='right')
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        
        return fig 