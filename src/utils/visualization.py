#!/usr/bin/env python3
"""
Visualization Module for CEREBRUM
Provides utilities for creating visualizations of regression models and cases
"""

import os
import logging
import numpy as np
import matplotlib.pyplot as plt
from typing import Tuple, Optional, Dict
from matplotlib.figure import Figure

from src.models.base import Case
from src.models.case_definitions import CaseDefinitions

# Setup logging
logger = logging.getLogger("cerebrum-visualization")

class Visualizer:
    """Visualization utilities for regression tests."""
    
    @staticmethod
    def plot_data(
        X: np.ndarray,
        y: np.ndarray,
        title: str = "Data Visualization",
        xlabel: str = "X",
        ylabel: str = "y",
        figsize: Tuple[int, int] = (10, 6),
        save_path: Optional[str] = None
    ) -> Figure:
        """Plot regression data."""
        fig, ax = plt.subplots(figsize=figsize)
        ax.scatter(X, y, alpha=0.7, color='blue', label='Data points')
        ax.set_xlabel(xlabel)
        ax.set_ylabel(ylabel)
        ax.set_title(title)
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        if save_path:
            plt.savefig(save_path, dpi=100, bbox_inches='tight')
            
        return fig

def plot_case_linguistic_context(case: Case, save_path: str) -> None:
    """Create a visualization showing the linguistic context of the case."""
    case_defs = CaseDefinitions.get_all_cases()
    case_info = case_defs[case]
    
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 7))
    
    # Create a diagram illustrating the case's role
    ax1.set_xlim(0, 10)
    ax1.set_ylim(0, 10)
    ax1.axis('off')
    
    # Common elements
    ax1.text(5, 9.5, f"{case.value} CASE: {case_info['linguistic_meaning']}", 
             ha='center', fontsize=16, fontweight='bold')
    
    # Create specific diagrams based on case
    if case == Case.NOMINATIVE:
        # Draw model as active subject
        circle_model = plt.Circle((3, 5), 1.5, color='blue', alpha=0.7)
        ax1.add_patch(circle_model)
        ax1.text(3, 5, "MODEL\n(Subject)", ha='center', va='center', color='white', fontweight='bold')
        
        # Draw data as object
        rect_data = plt.Rectangle((7, 4.5), 2, 1, color='green', alpha=0.7)
        ax1.add_patch(rect_data)
        ax1.text(8, 5, "DATA\n(Object)", ha='center', va='center', fontsize=9)
        
        # Draw arrow for action
        ax1.arrow(4.5, 5, 2, 0, head_width=0.3, head_length=0.3, fc='black', ec='black')
        ax1.text(5.5, 5.5, "fits", ha='center', fontweight='bold')
        
    elif case == Case.ACCUSATIVE:
        # Draw evaluator as subject
        circle_eval = plt.Circle((3, 5), 1.5, color='red', alpha=0.7)
        ax1.add_patch(circle_eval)
        ax1.text(3, 5, "EVALUATOR\n(Subject)", ha='center', va='center', color='white', fontweight='bold', fontsize=9)
        
        # Draw model as object
        rect_model = plt.Rectangle((7, 4.5), 2, 1, color='blue', alpha=0.7)
        ax1.add_patch(rect_model)
        ax1.text(8, 5, "MODEL\n(Object)", ha='center', va='center', fontsize=9)
        
        # Draw arrow for action
        ax1.arrow(4.5, 5, 2, 0, head_width=0.3, head_length=0.3, fc='black', ec='black')
        ax1.text(5.5, 5.5, "evaluates", ha='center', fontweight='bold')
        
    elif case == Case.DATIVE:
        # Draw data provider as subject
        circle_provider = plt.Circle((2, 5), 1.5, color='green', alpha=0.7)
        ax1.add_patch(circle_provider)
        ax1.text(2, 5, "PROVIDER\n(Subject)", ha='center', va='center', color='white', fontweight='bold', fontsize=9)
        
        # Draw data as direct object
        rect_data = plt.Rectangle((5, 6.5), 2, 1, color='orange', alpha=0.7)
        ax1.add_patch(rect_data)
        ax1.text(6, 7, "DATA\n(Direct Obj)", ha='center', va='center', fontsize=9)
        
        # Draw model as indirect object (recipient)
        circle_model = plt.Circle((8, 5), 1.5, color='blue', alpha=0.7)
        ax1.add_patch(circle_model)
        ax1.text(8, 5, "MODEL\n(Recipient)", ha='center', va='center', color='white', fontweight='bold', fontsize=9)
        
        # Draw arrows for action
        ax1.arrow(3.5, 5.2, 1.2, 1, head_width=0.3, head_length=0.3, fc='black', ec='black')
        ax1.arrow(6.5, 6.7, 1, -0.8, head_width=0.3, head_length=0.3, fc='black', ec='black')
        ax1.text(4.5, 6.3, "gives", ha='center', fontweight='bold')
        ax1.text(7, 6, "to", ha='center', fontweight='bold')
        
    # Add specific information about the case in the right panel
    ax2.axis('off')
    ax2.set_xlim(0, 10)
    ax2.set_ylim(0, 10)
    
    # Title
    ax2.text(5, 9.5, f"{case.value} Case in Linear Regression", ha='center', fontsize=14, fontweight='bold')
    
    # Linguistic meaning
    ax2.text(0.5, 8.5, "Linguistic Meaning:", fontweight='bold')
    ax2.text(0.5, 8.0, case_info['linguistic_meaning'])
    
    # Statistical role
    ax2.text(0.5, 7.0, "Statistical Role:", fontweight='bold')
    ax2.text(0.5, 6.5, case_info['statistical_role'])
    
    # Context
    ax2.text(0.5, 5.5, "Regression Context:", fontweight='bold')
    ax2.text(0.5, 5.0, case_info['regression_context'])
    
    # Example
    ax2.text(0.5, 4.0, "Linguistic Example:", fontweight='bold')
    ax2.text(0.5, 3.5, case_info['example'])
    
    # Add technical information
    ax2.text(0.5, 2.0, "Implementation Notes:", fontweight='bold')
    
    if case == Case.NOMINATIVE:
        tech_note = "• Primary method: fit(X, y)\n• Focus on parameter estimation\n• Active role in coefficient calculation"
    elif case == Case.ACCUSATIVE:
        tech_note = "• Primary method: evaluate(X, y)\n• Focus on performance metrics\n• Passive role in hypothesis testing"
    elif case == Case.DATIVE:
        tech_note = "• Primary method: receive_data(X, y)\n• Focus on data intake & buffering\n• Recipient role in data processing"
    elif case == Case.GENITIVE:
        tech_note = "• Primary method: predict(X)\n• Focus on output generation\n• Possessive role in creating predictions"
    elif case == Case.INSTRUMENTAL:
        tech_note = "• Primary method: analyze_data(X, y)\n• Focus on feature relationships\n• Tool role in statistical analysis"
    elif case == Case.LOCATIVE:
        tech_note = "• Primary method: locate_in_space()\n• Focus on parameter space\n• Position role in solution space"
    elif case == Case.ABLATIVE:
        tech_note = "• Primary method: extract_insights()\n• Focus on derivation\n• Source role for interpretations"
    elif case == Case.VOCATIVE:
        tech_note = "• Primary method: query(prompt)\n• Focus on direct interaction\n• Command role in model communication"
    
    ax2.text(0.5, 1.0, tech_note)
    
    # Save the figure
    plt.tight_layout()
    plt.savefig(save_path, dpi=300, bbox_inches='tight')
    logger.info(f"Saved linguistic context visualization for {case.value} case to {save_path}")
    plt.close(fig) 