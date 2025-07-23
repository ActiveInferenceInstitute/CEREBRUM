"""
Output Organizer for CEREBRUM Insect Simulations

This module provides utilities for organizing simulation outputs into properly
structured timestamped folders with comprehensive data organization.
"""

import os
import shutil
import time
import json
from datetime import datetime
from typing import Dict, List, Any, Optional
from pathlib import Path

from .path_utils import get_output_dir


class SimulationOutputOrganizer:
    """
    Organizes simulation outputs into timestamped folders with proper structure.
    """
    
    def __init__(self, simulation_name: str = "insect_simulation"):
        """
        Initialize the output organizer.
        
        Args:
            simulation_name: Name of the simulation for folder organization
        """
        self.simulation_name = simulation_name
        self.timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        self.base_output_dir = get_output_dir()
        self.simulation_dir = os.path.join(
            self.base_output_dir, 
            f"{simulation_name}_{self.timestamp}"
        )
        
        # Create directory structure
        self._create_directory_structure()
        
    def _create_directory_structure(self):
        """Create the complete directory structure for simulation outputs."""
        directories = [
            "logs",
            "logs/simulation_events",
            "logs/case_performance", 
            "logs/behavioral_patterns",
            "visualizations",
            "visualizations/individual_insects",
            "visualizations/swarm_analysis",
            "visualizations/case_analysis",
            "visualizations/neural_activity",
            "data",
            "data/raw_simulation",
            "data/processed_metrics",
            "data/environment_states",
            "reports",
            "reports/performance_analysis",
            "reports/behavioral_analysis",
            "reports/case_effectiveness"
        ]
        
        for directory in directories:
            full_path = os.path.join(self.simulation_dir, directory)
            os.makedirs(full_path, exist_ok=True)
            
    def get_simulation_path(self) -> str:
        """Get the path to the current simulation directory."""
        return self.simulation_dir
        
    def get_logs_path(self) -> str:
        """Get the path to the logs directory."""
        return os.path.join(self.simulation_dir, "logs")
        
    def get_visualizations_path(self) -> str:
        """Get the path to the visualizations directory."""
        return os.path.join(self.simulation_dir, "visualizations")
        
    def get_data_path(self) -> str:
        """Get the path to the data directory."""
        return os.path.join(self.simulation_dir, "data")
        
    def get_reports_path(self) -> str:
        """Get the path to the reports directory."""
        return os.path.join(self.simulation_dir, "reports")
        
    def save_simulation_event(self, event_data: Dict[str, Any], filename: str = None):
        """Save a simulation event to the events log."""
        if filename is None:
            filename = f"event_{int(time.time() * 1000)}.json"
            
        event_path = os.path.join(self.simulation_dir, "logs", "simulation_events", filename)
        with open(event_path, 'w') as f:
            json.dump(event_data, f, indent=2, default=str)
            
    def save_case_performance(self, case_data: Dict[str, Any], filename: str = None):
        """Save case performance data."""
        if filename is None:
            filename = f"case_performance_{int(time.time() * 1000)}.json"
            
        case_path = os.path.join(self.simulation_dir, "logs", "case_performance", filename)
        with open(case_path, 'w') as f:
            json.dump(case_data, f, indent=2, default=str)
            
    def save_behavioral_data(self, behavior_data: Dict[str, Any], filename: str = None):
        """Save behavioral pattern data."""
        if filename is None:
            filename = f"behavioral_{int(time.time() * 1000)}.json"
            
        behavior_path = os.path.join(self.simulation_dir, "logs", "behavioral_patterns", filename)
        with open(behavior_path, 'w') as f:
            json.dump(behavior_data, f, indent=2, default=str)
            
    def save_visualization(self, figure, filename: str, subdirectory: str = "individual_insects"):
        """Save a visualization to the appropriate subdirectory."""
        viz_path = os.path.join(self.simulation_dir, "visualizations", subdirectory)
        os.makedirs(viz_path, exist_ok=True)
        
        full_path = os.path.join(viz_path, filename)
        figure.savefig(full_path, dpi=300, bbox_inches='tight')
        return full_path
        
    def save_report(self, report_data: Dict[str, Any], report_type: str, filename: str = None):
        """Save a report to the reports directory."""
        if filename is None:
            filename = f"{report_type}_{int(time.time() * 1000)}.json"
            
        report_path = os.path.join(self.simulation_dir, "reports", report_type, filename)
        os.makedirs(os.path.dirname(report_path), exist_ok=True)
        with open(report_path, 'w') as f:
            json.dump(report_data, f, indent=2, default=str)
            
    def save_data(self, data: Dict[str, Any], data_type: str, filename: str = None):
        """Save data to the data directory."""
        if filename is None:
            filename = f"{data_type}_{int(time.time() * 1000)}.json"
            
        data_path = os.path.join(self.simulation_dir, "data", data_type, filename)
        os.makedirs(os.path.dirname(data_path), exist_ok=True)
        with open(data_path, 'w') as f:
            json.dump(data, f, indent=2, default=str)
            
    def create_simulation_summary(self, summary_data: Dict[str, Any]):
        """Create a comprehensive simulation summary."""
        summary_path = os.path.join(self.simulation_dir, "simulation_summary.json")
        
        summary = {
            "simulation_info": {
                "name": self.simulation_name,
                "timestamp": self.timestamp,
                "start_time": datetime.now().isoformat(),
                "directory": self.simulation_dir
            },
            "directory_structure": {
                "logs": "Simulation events, case performance, and behavioral data",
                "visualizations": "All generated plots and charts",
                "data": "Raw and processed simulation data",
                "reports": "Analysis reports and summaries"
            },
            "summary_data": summary_data
        }
        
        with open(summary_path, 'w') as f:
            json.dump(summary, f, indent=2, default=str)
            
    def cleanup_old_outputs(self, keep_last_n: int = 5):
        """Clean up old simulation outputs, keeping only the most recent N."""
        base_dir = get_output_dir()
        simulation_dirs = []
        
        for item in os.listdir(base_dir):
            if item.startswith(self.simulation_name) and os.path.isdir(os.path.join(base_dir, item)):
                simulation_dirs.append(item)
                
        # Sort by timestamp (newest first)
        simulation_dirs.sort(reverse=True)
        
        # Remove old directories
        for old_dir in simulation_dirs[keep_last_n:]:
            old_path = os.path.join(base_dir, old_dir)
            try:
                shutil.rmtree(old_path)
                print(f"Cleaned up old simulation directory: {old_dir}")
            except Exception as e:
                print(f"Error cleaning up {old_dir}: {e}")


def organize_existing_outputs():
    """
    Organize existing outputs into proper timestamped structure.
    This function can be called to reorganize outputs that were created
    without proper organization.
    """
    base_dir = get_output_dir()
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    organized_dir = os.path.join(base_dir, f"organized_outputs_{timestamp}")
    
    # Create organized structure
    os.makedirs(organized_dir, exist_ok=True)
    
    # Move existing files to organized structure
    for item in os.listdir(base_dir):
        item_path = os.path.join(base_dir, item)
        
        if os.path.isfile(item_path) and item.endswith('.png'):
            # Move visualization files
            viz_dir = os.path.join(organized_dir, "visualizations", "individual_insects")
            os.makedirs(viz_dir, exist_ok=True)
            shutil.move(item_path, os.path.join(viz_dir, item))
            
        elif os.path.isfile(item_path) and item.endswith('.json'):
            # Move data files
            data_dir = os.path.join(organized_dir, "data", "raw_simulation")
            os.makedirs(data_dir, exist_ok=True)
            shutil.move(item_path, os.path.join(data_dir, item))
            
        elif os.path.isdir(item_path) and item.startswith('insect_'):
            # Move existing insect directories
            logs_dir = os.path.join(organized_dir, "logs")
            os.makedirs(logs_dir, exist_ok=True)
            shutil.move(item_path, os.path.join(logs_dir, item))
            
    return organized_dir 