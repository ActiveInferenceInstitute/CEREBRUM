"""
POMDP Visualization Module

This module provides functions for visualizing POMDP models and their dynamics.
"""

import os
import numpy as np
import matplotlib.pyplot as plt
import networkx as nx
from typing import Dict, Any, List, Optional, Tuple
import seaborn as sns

from src.core.model import Case

def plot_case_linguistic_context(case: Case, save_path: str, logger=None):
    """
    Create a visualization of the linguistic context for a grammatical case.
    
    Args:
        case: The grammatical case to visualize
        save_path: Path to save the visualization to
        logger: Optional logger for logging progress
    """
    if logger:
        logger.info(f"Creating linguistic context visualization for {case.value} case")
    
    # Define case information
    case_info = {
        Case.NOMINATIVE: {
            "description": "Subject/Agent performing an action",
            "examples": ["The model generates predictions", 
                        "The POMDP chooses actions", 
                        "The system decides the policy"],
            "color": "royalblue"
        },
        Case.ACCUSATIVE: {
            "description": "Direct Object/Patient receiving an action",
            "examples": ["The data updates the model", 
                        "The observation changes the belief state", 
                        "The reward modifies the policy"],
            "color": "firebrick"
        },
        Case.GENITIVE: {
            "description": "Possession relationship",
            "examples": ["The model's parameters", 
                        "The POMDP's belief state", 
                        "The policy's expected return"],
            "color": "darkgreen"
        },
        Case.DATIVE: {
            "description": "Indirect Object/Recipient",
            "examples": ["Data given to the model", 
                        "Actions applied to the environment", 
                        "Parameters passed to the function"],
            "color": "darkorange"
        },
        Case.INSTRUMENTAL: {
            "description": "Means or instrument",
            "examples": ["Learning with gradient descent", 
                        "Planning with value iteration", 
                        "Deciding with policy evaluation"],
            "color": "purple"
        },
        Case.LOCATIVE: {
            "description": "Location or context",
            "examples": ["In the state space", 
                        "Within the action domain", 
                        "Under uncertainty constraints"],
            "color": "teal"
        },
        Case.ABLATIVE: {
            "description": "Origin or source",
            "examples": ["Derived from observed data", 
                        "Originating from previous state", 
                        "Starting from initial belief"],
            "color": "chocolate"
        },
        Case.VOCATIVE: {
            "description": "Direct address",
            "examples": ["Call to specific model instance", 
                        "Reference to particular POMDP agent", 
                        "Addressing system component directly"],
            "color": "goldenrod"
        }
    }
    
    selected_case = case_info[case]
    
    # Create figure
    fig, ax = plt.subplots(figsize=(10, 6))
    
    # Title and background
    ax.set_title(f"Linguistic Context: {case.value} Case", fontsize=16, fontweight='bold')
    ax.set_facecolor('#f8f8f8')
    
    # Main description box
    desc_box = dict(boxstyle='round,pad=1', facecolor=selected_case["color"], alpha=0.3)
    ax.text(0.5, 0.8, selected_case["description"], fontsize=14, 
            ha='center', va='center', bbox=desc_box, transform=ax.transAxes)
    
    # Examples
    example_y_positions = [0.6, 0.5, 0.4]
    for i, example in enumerate(selected_case["examples"]):
        if i < len(example_y_positions):
            ex_box = dict(boxstyle='round,pad=0.5', facecolor='white', alpha=0.8)
            ax.text(0.5, example_y_positions[i], f"Example: {example}", fontsize=12,
                   ha='center', va='center', bbox=ex_box, transform=ax.transAxes)
    
    # Remove axes
    ax.set_xticks([])
    ax.set_yticks([])
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    ax.spines['left'].set_visible(False)
    
    # Add border
    border = plt.Rectangle((0, 0), 1, 1, fill=False, 
                          transform=ax.transAxes, linestyle='--',
                          linewidth=2, edgecolor=selected_case["color"])
    ax.add_patch(border)
    
    # Save figure
    fig.tight_layout()
    fig.savefig(save_path, dpi=150)
    plt.close(fig)
    
    if logger:
        logger.info(f"Linguistic context visualization saved to {save_path}")

class Visualizer:
    """Visualization utilities for POMDP models."""
    
    @staticmethod
    def plot_pomdp_structure(
        transition_matrix: np.ndarray,
        observation_matrix: np.ndarray,
        title: str,
        save_path: str,
        logger: Optional[Any] = None
    ) -> None:
        """
        Visualize the structure of a POMDP model.
        
        Args:
            transition_matrix: The transition matrix of the POMDP
            observation_matrix: The observation matrix of the POMDP
            title: Title for the visualization
            save_path: Path to save the visualization to
            logger: Optional logger for logging progress
        """
        if logger:
            logger.info(f"Creating POMDP structure visualization")
        
        n_states = transition_matrix.shape[0]
        n_actions = transition_matrix.shape[1]
        n_observations = observation_matrix.shape[1]
        
        # Create figure with 2 subplots
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 8))
        
        # Plot transition probabilities (average over actions for visualization)
        avg_transitions = np.mean(transition_matrix, axis=1)
        sns.heatmap(avg_transitions, ax=ax1, cmap='Blues', annot=True, fmt=".2f", 
                   cbar_kws={'label': 'Transition Probability'})
        ax1.set_title('State Transition Probabilities\n(Averaged over actions)')
        ax1.set_xlabel('Next State')
        ax1.set_ylabel('Current State')
        
        # Plot observation probabilities
        sns.heatmap(observation_matrix, ax=ax2, cmap='Greens', annot=True, fmt=".2f",
                   cbar_kws={'label': 'Observation Probability'})
        ax2.set_title('Observation Probabilities')
        ax2.set_xlabel('Observation')
        ax2.set_ylabel('State')
        
        # Set overall title
        fig.suptitle(title, fontsize=16, fontweight='bold', y=0.98)
        
        # Add POMDP details text
        details_text = f"POMDP with {n_states} states, {n_actions} actions, and {n_observations} observations"
        fig.text(0.5, 0.01, details_text, ha='center', fontsize=12)
        
        # Save figure
        fig.tight_layout(rect=[0, 0.03, 1, 0.95])
        fig.savefig(save_path, dpi=150)
        plt.close(fig)
        
        if logger:
            logger.info(f"POMDP structure visualization saved to {save_path}")
    
    @staticmethod
    def plot_belief_state(
        belief: np.ndarray,
        title: str,
        save_path: str,
        logger: Optional[Any] = None
    ) -> None:
        """
        Visualize a belief state of a POMDP model.
        
        Args:
            belief: The belief state to visualize
            title: Title for the visualization
            save_path: Path to save the visualization to
            logger: Optional logger for logging progress
        """
        if logger:
            logger.info(f"Creating belief state visualization")
        
        fig, ax = plt.subplots(figsize=(10, 6))
        
        # Plot belief state as bars
        x = np.arange(len(belief))
        ax.bar(x, belief, width=0.6, color='royalblue', alpha=0.7)
        
        # Add value labels on top of bars
        for i, v in enumerate(belief):
            ax.text(i, v + 0.01, f"{v:.2f}", ha='center', va='bottom', fontsize=10)
        
        # Set labels and title
        ax.set_xlabel('State')
        ax.set_ylabel('Probability')
        ax.set_title(title, fontsize=14, fontweight='bold')
        
        # Set axis limits
        ax.set_ylim(0, max(belief) * 1.2)
        ax.set_xticks(x)
        
        # Add grid
        ax.grid(axis='y', linestyle='--', alpha=0.7)
        
        # Save figure
        fig.tight_layout()
        fig.savefig(save_path, dpi=150)
        plt.close(fig)
        
        if logger:
            logger.info(f"Belief state visualization saved to {save_path}")
    
    @staticmethod
    def plot_policy(
        policy: np.ndarray,
        title: str,
        save_path: str,
        action_labels: Optional[List[str]] = None,
        logger: Optional[Any] = None
    ) -> None:
        """
        Visualize a policy for a POMDP model.
        
        Args:
            policy: The policy to visualize (mapping from states to actions)
            title: Title for the visualization
            save_path: Path to save the visualization to
            action_labels: Labels for the actions
            logger: Optional logger for logging progress
        """
        if logger:
            logger.info(f"Creating policy visualization")
        
        n_states = len(policy)
        
        fig, ax = plt.subplots(figsize=(12, 6))
        
        # Plot policy as bars
        x = np.arange(n_states)
        bars = ax.bar(x, policy, width=0.6)
        
        # Add value labels on top of bars
        for i, v in enumerate(policy):
            ax.text(i, v + 0.1, f"{int(v)}", ha='center', va='bottom', fontsize=10)
        
        # Set labels and title
        ax.set_xlabel('State')
        ax.set_ylabel('Action')
        ax.set_title(title, fontsize=14, fontweight='bold')
        
        # Set axis limits
        max_action = np.max(policy)
        ax.set_ylim(0, max_action + 1)
        ax.set_xticks(x)
        
        # Set action labels on y-axis if provided
        if action_labels:
            ax.set_yticks(np.arange(len(action_labels)))
            ax.set_yticklabels(action_labels)
        
        # Add grid
        ax.grid(axis='y', linestyle='--', alpha=0.7)
        
        # Color the bars based on action
        for i, bar in enumerate(bars):
            bar.set_color(plt.cm.tab10(policy[i] % 10))
        
        # Save figure
        fig.tight_layout()
        fig.savefig(save_path, dpi=150)
        plt.close(fig)
        
        if logger:
            logger.info(f"Policy visualization saved to {save_path}")
    
    @staticmethod
    def plot_value_function(
        value_function: np.ndarray,
        title: str,
        save_path: str,
        logger: Optional[Any] = None
    ) -> None:
        """
        Visualize a value function for a POMDP model.
        
        Args:
            value_function: The value function to visualize
            title: Title for the visualization
            save_path: Path to save the visualization to
            logger: Optional logger for logging progress
        """
        if logger:
            logger.info(f"Creating value function visualization")
        
        fig, ax = plt.subplots(figsize=(10, 6))
        
        # Plot value function as bars
        x = np.arange(len(value_function))
        ax.bar(x, value_function, width=0.6, color='green', alpha=0.7)
        
        # Add value labels on top of bars
        for i, v in enumerate(value_function):
            ax.text(i, v + 0.1, f"{v:.2f}", ha='center', va='bottom', fontsize=10)
        
        # Set labels and title
        ax.set_xlabel('State')
        ax.set_ylabel('Value')
        ax.set_title(title, fontsize=14, fontweight='bold')
        
        # Set axis limits
        ax.set_ylim(min(value_function) - 0.5, max(value_function) + 0.5)
        ax.set_xticks(x)
        
        # Add grid
        ax.grid(axis='y', linestyle='--', alpha=0.7)
        
        # Save figure
        fig.tight_layout()
        fig.savefig(save_path, dpi=150)
        plt.close(fig)
        
        if logger:
            logger.info(f"Value function visualization saved to {save_path}") 