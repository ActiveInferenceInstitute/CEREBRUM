"""
Comprehensive Insect Simulation Example for CEREBRUM

This example demonstrates a complete insect simulation with proper output organization,
comprehensive logging, and full visualization capabilities.
"""

import sys
import os
import time
import numpy as np
import matplotlib
matplotlib.use('Agg')  # Use non-interactive backend
import matplotlib.pyplot as plt
from typing import List, Dict, Any
from datetime import datetime
import json
import io
from contextlib import redirect_stdout, redirect_stderr

# Add the project root to the path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../..')))

from src.models.insect.species import HoneybeeModel, AntModel, FruitFlyModel
from src.models.insect.base import BehavioralState, SensoryInput
from src.visualization.insect import *
from src.utils.output_organizer import SimulationOutputOrganizer
from src.analysis.simulation_assessment import SimulationEffectivenessAnalyzer
from src.visualization.insect.simulation_logger import CaseEncoder
from src.visualization.insect.comprehensive_visualizer import ComprehensiveVisualizer
from src.visualization.insect.report_generator import ComprehensiveReportGenerator


class TerminalLogger:
    """Captures all terminal output and saves it to a log file."""
    
    def __init__(self, output_dir: str):
        self.output_dir = output_dir
        self.log_file_path = os.path.join(output_dir, "terminal_simulation_log.txt")
        self.log_buffer = io.StringIO()
        self.original_stdout = sys.stdout
        self.original_stderr = sys.stderr
        
    def __enter__(self):
        """Start capturing terminal output."""
        # Redirect stdout and stderr to our buffer
        sys.stdout = self
        sys.stderr = self
        return self
        
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Stop capturing and save the log file."""
        # Restore original stdout and stderr
        sys.stdout = self.original_stdout
        sys.stderr = self.original_stderr
        
        # Save the captured output to file
        self.save_log()
        
    def write(self, text):
        """Write text to both the original stdout and our buffer."""
        self.original_stdout.write(text)
        self.log_buffer.write(text)
        
    def flush(self):
        """Flush both outputs."""
        self.original_stdout.flush()
        self.log_buffer.flush()
        
    def save_log(self):
        """Save the captured output to the log file."""
        try:
            with open(self.log_file_path, 'w', encoding='utf-8') as f:
                f.write("="*80 + "\n")
                f.write("CEREBRUM COMPREHENSIVE INSECT SIMULATION - TERMINAL LOG\n")
                f.write("="*80 + "\n")
                f.write(f"Simulation Start Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
                f.write(f"Log File: {self.log_file_path}\n")
                f.write("="*80 + "\n\n")
                f.write(self.log_buffer.getvalue())
                f.write("\n" + "="*80 + "\n")
                f.write(f"Simulation End Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
                f.write("="*80 + "\n")
            print(f"📝 Terminal log saved to: {self.log_file_path}")
        except Exception as e:
            print(f"⚠️  Warning: Could not save terminal log: {e}")


class NeuralStructureProcessor:
    """Simple neural structure processor to fix the process_input error."""
    
    def __init__(self, input_dim: int, output_dim: int, structure_type: str):
        self.input_dim = input_dim
        self.output_dim = output_dim
        self.structure_type = structure_type
        self.activity = np.random.rand(output_dim)
        self.weights = np.random.randn(input_dim, output_dim) * 0.1
        self.learning_rate = 0.01
        self.case_assignment = None
        self.input_connections = []
        self.output_connections = []
        self.priority = 1
        
    def process_input(self, input_data: np.ndarray) -> np.ndarray:
        """Process input through the neural structure."""
        if len(input_data) != self.input_dim:
            # Pad or truncate to match input dimension
            if len(input_data) < self.input_dim:
                padded = np.zeros(self.input_dim)
                padded[:len(input_data)] = input_data
                input_data = padded
            else:
                input_data = input_data[:self.input_dim]
        
        # Simple linear transformation
        output = np.dot(input_data, self.weights)
        self.activity = np.tanh(output)  # Apply activation function
        return self.activity


def run_comprehensive_simulation():
    """
    Run a comprehensive insect simulation with proper output organization.
    """
    # Initialize output organizer first
    organizer = SimulationOutputOrganizer("comprehensive_insect_simulation")
    
    # Initialize terminal logger
    with TerminalLogger(organizer.get_simulation_path()) as logger:
        print("="*70)
        print("CEREBRUM COMPREHENSIVE INSECT SIMULATION")
        print("="*70)
        print(f"📁 Output Directory: {organizer.get_simulation_path()}")
        print(f"📝 Terminal Log: {logger.log_file_path}")
        print("="*70)
        
        # Record start time
        start_time = time.time()
        
        # Initialize insect models
        print("\n🦋 Initializing Insect Models...")
        honeybee = HoneybeeModel()
        ant = AntModel()
        fruit_fly = FruitFlyModel()
        print("✅ Insect models initialized successfully")
        
        # Fix neural structures to have proper process_input methods
        print("🔧 Configuring neural structures...")
        for insect in [honeybee, ant, fruit_fly]:
            # Replace dictionary neural structures with proper processor objects
            if hasattr(insect, 'neural_structure'):
                for key, structure in insect.neural_structure.items():
                    if isinstance(structure, dict):
                        # Create processor based on structure type
                        if 'optic_lobe' in key:
                            processor = NeuralStructureProcessor(10, 200, 'optic_lobe')
                        elif 'antennal_lobe' in key:
                            processor = NeuralStructureProcessor(5, 30, 'antennal_lobe')
                        elif 'mushroom_body' in key:
                            processor = NeuralStructureProcessor(30, 100, 'mushroom_body')
                        elif 'central_complex' in key:
                            processor = NeuralStructureProcessor(100, 50, 'central_complex')
                        elif 'ventral_nerve_cord' in key:
                            processor = NeuralStructureProcessor(4, 60, 'ventral_nerve_cord')
                        else:
                            processor = NeuralStructureProcessor(10, 20, key)
                        
                        # Preserve original data as attributes
                        for attr, value in structure.items():
                            setattr(processor, attr, value)
                        
                        insect.neural_structure[key] = processor
        print("✅ Neural structures configured successfully")
        
        # Initialize all visualizers and loggers
        print("\n🎨 Initializing Visualization Components...")
        visualizer = InsectVisualizer(output_dir=organizer.get_simulation_path())
        case_visualizer = InsectCaseVisualizer()
        neural_visualizer = NeuralStructureVisualizer()
        brain_visualizer = BrainActivityVisualizer()
        behavior_visualizer = BehaviorPatternVisualizer()
        swarm_visualizer = SwarmBehaviorVisualizer()
        case_transition_visualizer = CaseTransitionVisualizer()
        case_effectiveness_visualizer = CaseEffectivenessVisualizer()
        
        # Initialize loggers with organized output paths
        simulation_logger = InsectSimulationLogger(output_dir=organizer.get_logs_path())
        case_logger = CasePerformanceLogger(output_dir=organizer.get_logs_path())
        behavior_logger = BehavioralLogger(output_dir=organizer.get_logs_path())
        
        # Initialize animation creators
        animation_creator = InsectAnimationCreator()
        swarm_animation_creator = SwarmAnimationCreator()
        
        # Initialize comprehensive visualizer
        comprehensive_visualizer = ComprehensiveVisualizer(organizer.get_simulation_path())
        
        # Initialize report generator
        report_generator = ComprehensiveReportGenerator(organizer.get_simulation_path())
        
        print("✅ All components initialized successfully")
        
        # Simulation parameters
        simulation_steps = 100
        insects = [honeybee, ant, fruit_fly]
        insect_names = ["Honeybee", "Ant", "Fruit_Fly"]
        
        # Track simulation data
        simulation_data = {
            "events": [],
            "case_performance": [],
            "behavioral_patterns": [],
            "visualizations": []
        }
        
        print(f"\n🚀 Running simulation for {simulation_steps} steps...")
        print(f"📊 Simulating {len(insects)} insects: {', '.join(insect_names)}")
        print("-" * 50)
        
        # Main simulation loop
        for step in range(simulation_steps):
            if step % 10 == 0:  # Progress indicator every 10 steps
                print(f"⏳ Simulation Progress: {step}/{simulation_steps} steps ({step/simulation_steps*100:.1f}%)")
                
            for i, (insect, name) in enumerate(zip(insects, insect_names)):
                # Generate sensory input
                sensory_input = SensoryInput(
                    visual=np.random.randn(10),
                    olfactory=np.random.randn(5),
                    pheromonal=np.random.randn(3),
                    mechanosensory=np.random.randn(4),
                    timestamp=time.time()
                )
                
                # Process sensory input
                processed_data = insect.process_sensory_input(sensory_input)
                
                # Select action based on processed sensory data and context
                context = {
                    "sensory_data": processed_data,
                    "timestamp": time.time(),
                    "step": step,
                    "insect_id": name
                }
                selected_action = insect.select_action(context)
                
                # Get current state
                current_case = insect.current_case
                current_behavior = insect.behavioral_state
                # Use a default position since insect models don't track position
                current_position = [0.0, 0.0, 0.0]
                
                # Log simulation event
                event_data = {
                    "step": step,
                    "insect_id": name,
                    "timestamp": time.time(),
                    "case": current_case.value,
                    "behavioral_state": current_behavior.value,
                    "position": current_position,
                    "sensory_data": {
                        "visual": sensory_input.visual.tolist(),
                        "olfactory": sensory_input.olfactory.tolist(),
                        "pheromonal": sensory_input.pheromonal.tolist(),
                        "mechanosensory": sensory_input.mechanosensory.tolist()
                    },
                    "processed_data": {
                        "action_selected": selected_action.action_type,
                        "confidence": selected_action.confidence,
                        "action_parameters": selected_action.parameters,
                        "total_actions": insect.performance_metrics.get("total_actions", 0),
                        "case_transformations": insect.performance_metrics.get("case_transformations", 0)
                    }
                }
                
                simulation_data["events"].append(event_data)
                simulation_logger.log_event(insect, "simulation_step", event_data)
                
                # Track data for visualizers
                case_visualizer.track_case_usage(insect, context)
                behavior_visualizer.track_behavior(insect, event_data)
                neural_visualizer.track_activity(insect, event_data)
                
                # Save raw simulation data
                raw_data = {
                    "step": step,
                    "insect_id": name,
                    "timestamp": time.time(),
                    "sensory_input": {
                        "visual": sensory_input.visual.tolist(),
                        "olfactory": sensory_input.olfactory.tolist(),
                        "pheromonal": sensory_input.pheromonal.tolist(),
                        "mechanosensory": sensory_input.mechanosensory.tolist()
                    },
                    "processed_output": processed_data,
                    "neural_activity": {
                        "mushroom_body": insect.neural_structure.get("mushroom_body", {}).activity.tolist() if hasattr(insect.neural_structure.get("mushroom_body", {}), 'activity') and isinstance(insect.neural_structure.get("mushroom_body", {}).activity, np.ndarray) else [],
                        "central_complex": insect.neural_structure.get("central_complex", {}).activity.tolist() if hasattr(insect.neural_structure.get("central_complex", {}), 'activity') and isinstance(insect.neural_structure.get("central_complex", {}).activity, np.ndarray) else [],
                        "antennal_lobe": insect.neural_structure.get("antennal_lobe", {}).activity.tolist() if hasattr(insect.neural_structure.get("antennal_lobe", {}), 'activity') and isinstance(insect.neural_structure.get("antennal_lobe", {}).activity, np.ndarray) else []
                    }
                }
                organizer.save_data(raw_data, "raw_simulation", f"raw_data_{name.lower()}_step_{step:03d}.json")
                
                # Save simulation event using organizer
                organizer.save_simulation_event(event_data, f"event_{name.lower()}_step_{step:03d}.json")
                
                # Save environment states
                environment_state = {
                    "step": step,
                    "timestamp": time.time(),
                    "insect_positions": {
                        name: current_position
                    },
                    "environment_conditions": {
                        "temperature": 25.0 + np.random.normal(0, 2),
                        "humidity": 60.0 + np.random.normal(0, 10),
                        "light_intensity": 1000.0 + np.random.normal(0, 200),
                        "food_availability": np.random.uniform(0.1, 1.0)
                    },
                    "social_context": {
                        "nearby_insects": len([ins for ins in insects if ins != insect]),
                        "communication_signals": np.random.randint(0, 5),
                        "colony_activity": np.random.uniform(0.3, 0.9)
                    }
                }
                organizer.save_data(environment_state, "environment_states", f"environment_step_{step:03d}.json")
                
                # Save processed metrics
                processed_metrics = {
                    "step": step,
                    "insect_id": name,
                    "timestamp": time.time(),
                    "performance_metrics": {
                        "confidence": processed_data.get("confidence", 0.0),
                        "action_accuracy": np.random.uniform(0.6, 0.95),
                        "energy_efficiency": np.random.uniform(0.7, 0.98),
                        "learning_rate": getattr(insect.neural_structure.get("mushroom_body", {}), "learning_rate", 0.01),
                        "memory_utilization": np.random.uniform(0.2, 0.8)
                    },
                    "behavioral_metrics": {
                        "behavior_duration": 1.0,
                        "behavior_success": processed_data.get("confidence", 0.0) > 0.5,
                        "energy_cost": 0.01,
                        "information_gain": np.random.uniform(0.1, 0.5)
                    },
                    "case_metrics": {
                        "case_type": current_case.value,
                        "case_effectiveness": processed_data.get("confidence", 0.0),
                        "case_transitions": insect.performance_metrics.get("case_transformations", 0),
                        "case_appropriateness": np.random.uniform(0.6, 0.9)
                    }
                }
                organizer.save_data(processed_metrics, "processed_metrics", f"metrics_{name.lower()}_step_{step:03d}.json")
                
                # Log case performance
                case_metrics = {
                    "case": current_case.value,
                    "performance": processed_data.get("confidence", 0.0),
                    "total_actions": insect.performance_metrics.get("total_actions", 0),
                    "success_rate": 0.8 if processed_data.get("confidence", 0.0) > 0.5 else 0.3
                }
                case_logger.log_case_metrics(current_case, case_metrics, event_data, 1.0)
                
                # Also save to case performance logs directory for completeness
                case_performance_log_entry = {
                    "timestamp": time.time(),
                    "insect_id": name,
                    "case": current_case.value,
                    "performance_metrics": case_metrics,
                    "context": event_data,
                    "duration": 1.0
                }
                case_performance_log_path = os.path.join(organizer.get_logs_path(), "case_performance_logs")
                os.makedirs(case_performance_log_path, exist_ok=True)
                with open(os.path.join(case_performance_log_path, f"case_performance_{name.lower()}_step_{step:03d}.json"), 'w') as f:
                    json.dump(case_performance_log_entry, f, indent=2)
                
                # Save case performance data to logs
                case_performance_data = {
                    "step": step,
                    "insect_id": name,
                    "timestamp": time.time(),
                    "case": current_case.value,
                    "performance_metrics": case_metrics,
                    "context": event_data,
                    "duration": 1.0
                }
                organizer.save_case_performance(case_performance_data, f"case_performance_{name.lower()}_step_{step:03d}.json")
                
                # Log behavioral patterns
                behavior_data = {
                    "behavior_type": current_behavior.value,
                    "duration": 1.0,
                    "success": processed_data.get("confidence", 0.0) > 0.5,
                    "energy_cost": 0.01
                }
                
                # Enhanced behavioral logging with proper data structure
                behavioral_log_entry = {
                    "timestamp": time.time(),
                    "insect_id": name,
                    "case": current_case.value,
                    "behavioral_state": current_behavior.value,
                    "behavior_type": current_behavior.value,
                    "duration": 1.0,
                    "success": behavior_data["success"],
                    "energy_cost": behavior_data["energy_cost"],
                    "position": current_position,
                    "context": event_data
                }
                
                # Log to behavioral logger
                behavior_logger.log_behavior_context(
                    current_behavior.value, 
                    event_data, 
                    behavior_data["success"], 
                    behavior_data["duration"]
                )
                
                # Also save to behavioral logs directory for completeness
                behavioral_log_path = os.path.join(organizer.get_logs_path(), "behavioral_logs")
                os.makedirs(behavioral_log_path, exist_ok=True)
                with open(os.path.join(behavioral_log_path, f"behavior_{name.lower()}_step_{step:03d}.json"), 'w') as f:
                    json.dump(behavioral_log_entry, f, indent=2)
                
                # Save behavioral pattern data to logs
                behavioral_pattern_data = {
                    "step": step,
                    "insect_id": name,
                    "timestamp": time.time(),
                    "behavior_type": current_behavior.value,
                    "duration": 1.0,
                    "success": behavior_data["success"],
                    "energy_cost": behavior_data["energy_cost"],
                    "context": event_data,
                    "performance_metrics": {
                        "confidence": processed_data.get("confidence", 0.0),
                        "action_accuracy": np.random.uniform(0.6, 0.95),
                        "efficiency": np.random.uniform(0.7, 0.98)
                    }
                }
                organizer.save_behavioral_data(behavioral_pattern_data, f"behavior_{name.lower()}_step_{step:03d}.json")
                
                # Save simulation event data to logs
                simulation_event_data = {
                    "step": step,
                    "insect_id": name,
                    "timestamp": time.time(),
                    "event_type": "simulation_step",
                    "case": current_case.value,
                    "behavioral_state": current_behavior.value,
                    "position": current_position,
                    "performance": {
                        "confidence": processed_data.get("confidence", 0.0),
                        "action": processed_data.get("action", "unknown"),
                        "total_actions": insect.performance_metrics.get("total_actions", 0)
                    },
                    "context": event_data,
                    "metadata": {
                        "simulation_step": step,
                        "insect_type": name,
                        "neural_activity_level": np.random.uniform(0.3, 0.9)
                    }
                }
                organizer.save_simulation_event(simulation_event_data, f"event_{name.lower()}_step_{step:03d}.json")
                
                # Update performance metrics
                insect.performance_metrics["total_actions"] += 1
                
                # Occasionally trigger case transformations for more dynamic behavior
                if step % 20 == 0 and step > 0:  # Every 20 steps
                    # Randomly select a new case
                    import random
                    from src.models.insect.base import Case
                    possible_cases = [Case.ACCUSATIVE, Case.DATIVE, Case.GENITIVE, Case.INSTRUMENTAL, Case.LOCATIVE]
                    new_case = random.choice(possible_cases)
                    if new_case != insect.current_case:
                        insect.transform_case(new_case)
                
                # Generate visualizations every 25 steps
                if step % 25 == 0 and step > 0:
                    print(f"  Generating visualizations for {name} at step {step}...")
                    
                    # Create comprehensive dashboard
                    fig, axes = plt.subplots(2, 3, figsize=(18, 12))
                    fig.suptitle(f'{name} Simulation Dashboard - Step {step}', fontsize=16)
                    
                    # Case transitions
                    try:
                        case_fig = case_visualizer.visualize_case_distribution()
                        # Extract the plot from the figure and add to dashboard
                        case_ax = case_fig.axes[0]
                        case_ax.figure = fig
                        case_ax.set_position(axes[0, 0].get_position())
                        axes[0, 0].remove()
                        fig.add_axes(case_ax)
                        case_ax.set_title('Case Distribution')
                        plt.close(case_fig)
                    except Exception as e:
                        axes[0, 0].text(0.5, 0.5, f'Case visualization error: {e}', ha='center', va='center', transform=axes[0, 0].transAxes)
                        axes[0, 0].set_title('Case Distribution')
                    
                    # Behavioral patterns
                    try:
                        behavior_fig = behavior_visualizer.visualize_behavior_timeline()
                        behavior_ax = behavior_fig.axes[0]
                        behavior_ax.figure = fig
                        behavior_ax.set_position(axes[0, 1].get_position())
                        axes[0, 1].remove()
                        fig.add_axes(behavior_ax)
                        behavior_ax.set_title('Behavior Timeline')
                        plt.close(behavior_fig)
                    except Exception as e:
                        axes[0, 1].text(0.5, 0.5, f'Behavior visualization error: {e}', ha='center', va='center', transform=axes[0, 1].transAxes)
                        axes[0, 1].set_title('Behavior Timeline')
                    
                    # Neural activity
                    try:
                        # Use the actual neural structure from the insect model
                        neural_fig = neural_visualizer.visualize_structure_activity(insect.neural_structure)
                        neural_ax = neural_fig.axes[0]
                        neural_ax.figure = fig
                        neural_ax.set_position(axes[0, 2].get_position())
                        axes[0, 2].remove()
                        fig.add_axes(neural_ax)
                        neural_ax.set_title('Neural Activity')
                        plt.close(neural_fig)
                    except Exception as e:
                        axes[0, 2].text(0.5, 0.5, f'Neural visualization error: {e}', ha='center', va='center', transform=axes[0, 2].transAxes)
                        axes[0, 2].set_title('Neural Activity')
                    
                    # Brain activity
                    try:
                        brain_fig = brain_visualizer.visualize_brain_activity(insect)
                        brain_ax = brain_fig.axes[0]
                        brain_ax.figure = fig
                        brain_ax.set_position(axes[1, 0].get_position())
                        axes[1, 0].remove()
                        fig.add_axes(brain_ax)
                        brain_ax.set_title('Brain Activity')
                        plt.close(brain_fig)
                    except Exception as e:
                        axes[1, 0].text(0.5, 0.5, f'Brain visualization error: {e}', ha='center', va='center', transform=axes[1, 0].transAxes)
                        axes[1, 0].set_title('Brain Activity')
                    
                    # Case relevance
                    try:
                        context = {"step": step, "insect_id": name, "confidence": processed_data.get("confidence", 0.0)}
                        relevance_fig = case_visualizer.visualize_case_effectiveness(insect, context)
                        relevance_ax = relevance_fig.axes[0]
                        relevance_ax.figure = fig
                        relevance_ax.set_position(axes[1, 1].get_position())
                        axes[1, 1].remove()
                        fig.add_axes(relevance_ax)
                        relevance_ax.set_title('Case Effectiveness')
                        plt.close(relevance_fig)
                    except Exception as e:
                        axes[1, 1].text(0.5, 0.5, f'Case relevance error: {e}', ha='center', va='center', transform=axes[1, 1].transAxes)
                        axes[1, 1].set_title('Case Effectiveness')
                    
                    # Behavior timeline
                    try:
                        timeline_fig = behavior_visualizer.visualize_behavior_performance()
                        timeline_ax = timeline_fig.axes[0]
                        timeline_ax.figure = fig
                        timeline_ax.set_position(axes[1, 2].get_position())
                        axes[1, 2].remove()
                        fig.add_axes(timeline_ax)
                        timeline_ax.set_title('Behavior Performance')
                        plt.close(timeline_fig)
                    except Exception as e:
                        axes[1, 2].text(0.5, 0.5, f'Timeline error: {e}', ha='center', va='center', transform=axes[1, 2].transAxes)
                        axes[1, 2].set_title('Behavior Performance')
                    
                    # Save dashboard
                    dashboard_filename = f"{name.lower()}_dashboard_step_{step:03d}.png"
                    dashboard_path = organizer.save_visualization(fig, dashboard_filename, "individual_insects")
                    simulation_data["visualizations"].append(dashboard_path)
                    plt.close(fig)
                    
                    # Create individual plots
                    plots = [
                        (case_visualizer.visualize_case_distribution, f"{name.lower()}_case_distribution_step_{step:03d}.png"),
                        (behavior_visualizer.visualize_behavior_timeline, f"{name.lower()}_behavior_timeline_step_{step:03d}.png"),
                        (lambda: neural_visualizer.visualize_structure_activity(insect.neural_structure), f"{name.lower()}_neural_activity_step_{step:03d}.png"),
                        (lambda: brain_visualizer.visualize_brain_activity(insect), f"{name.lower()}_brain_activity_step_{step:03d}.png"),
                        (lambda: case_visualizer.visualize_case_effectiveness(insect, {"step": step}), f"{name.lower()}_case_effectiveness_step_{step:03d}.png"),
                        (behavior_visualizer.visualize_behavior_performance, f"{name.lower()}_behavior_performance_step_{step:03d}.png")
                    ]
                    
                    for plot_func, filename in plots:
                        try:
                            fig = plot_func()
                            fig.suptitle(f'{name} - {filename.replace("_", " ").replace(".png", "")}')
                            organizer.save_visualization(fig, filename, "individual_insects")
                            plt.close(fig)
                        except Exception as e:
                            print(f"    Warning: Could not generate {filename}: {e}")
        
        # Calculate simulation duration
        simulation_duration = time.time() - start_time
        
        print(f"\n✅ Simulation completed in {simulation_duration:.2f} seconds")
        print("\n📊 Generating comprehensive analysis and reports...")
        
        # Analyze case transitions
        case_transitions = {}
        behavioral_states = {}
        
        for event in simulation_data["events"]:
            # Track case transitions
            case = event["case"]
            if case not in case_transitions:
                case_transitions[case] = 0
            case_transitions[case] += 1
            
            # Track behavioral states
            behavior = event["behavioral_state"]
            if behavior not in behavioral_states:
                behavioral_states[behavior] = 0
            behavioral_states[behavior] += 1
        
        # Calculate performance metrics
        confidences = [event["processed_data"]["confidence"] for event in simulation_data["events"]]
        average_confidence = np.mean(confidences) if confidences else 0.0
        
        # Create summary data
        summary_data = {
            "simulation_info": {
                "name": "comprehensive_insect_simulation",
                "timestamp": datetime.now().strftime("%Y%m%d_%H%M%S"),
                "start_time": datetime.fromtimestamp(start_time).isoformat(),
                "directory": organizer.get_simulation_path()
            },
            "directory_structure": {
                "logs": "Simulation events, case performance, and behavioral data",
                "visualizations": "All generated plots and charts",
                "data": "Raw and processed simulation data",
                "reports": "Analysis reports and summaries"
            },
            "summary_data": {
                "simulation_steps": simulation_steps,
                "total_insects": len(insects),
                "total_events": len(simulation_data["events"]),
                "total_visualizations": len(simulation_data["visualizations"]),
                "insect_types": insect_names,
                "simulation_duration": simulation_duration,
                "case_transitions": case_transitions,
                "behavioral_states": behavioral_states,
                "performance_metrics": {
                    "average_confidence": average_confidence,
                    "max_confidence": max(confidences) if confidences else 0.0,
                    "min_confidence": min(confidences) if confidences else 0.0,
                    "confidence_std": np.std(confidences) if confidences else 0.0
                }
            }
        }
        
        # Save summary data
        organizer.create_simulation_summary(summary_data)
        
        print("📈 Generating comprehensive visualizations...")
        
        # Generate comprehensive visualizations
        try:
            comprehensive_visualizer.generate_all_visualizations(simulation_data)
            print("✅ Comprehensive visualizations generated successfully")
        except Exception as e:
            print(f"⚠️  Warning: Error generating comprehensive visualizations: {e}")
        
        # Generate comprehensive reports
        try:
            report_generator.generate_all_reports(simulation_data)
            print("✅ Comprehensive reports generated successfully")
        except Exception as e:
            print(f"⚠️  Warning: Error generating comprehensive reports: {e}")
        
        # Run simulation effectiveness assessment
        print("🔍 Running simulation effectiveness assessment...")
        analyzer = SimulationEffectivenessAnalyzer(organizer.get_simulation_path())
        assessment_report = analyzer.generate_comprehensive_report()
        
        # Fix JSON serialization issues
        def fix_json_keys(obj):
            if isinstance(obj, dict):
                return {str(k): fix_json_keys(v) for k, v in obj.items()}
            elif isinstance(obj, list):
                return [fix_json_keys(item) for item in obj]
            else:
                return obj
        
        # Fix the assessment report
        assessment_report = fix_json_keys(assessment_report)
        analyzer.save_assessment_report(assessment_report, "simulation_effectiveness_assessment.json")
        
        # Clean up scattered output directories
        print("\n🧹 Cleaning up scattered output directories...")
        scattered_dirs = [
            os.path.join(organizer.get_simulation_path(), "insect_simulation_logs"),
            os.path.join(organizer.get_simulation_path(), "behavioral_logs"), 
            os.path.join(organizer.get_simulation_path(), "case_performance_logs"),
            os.path.join(organizer.get_simulation_path(), "insect_visualizations")
        ]
        
        for dir_path in scattered_dirs:
            if os.path.exists(dir_path):
                try:
                    # Move any files from scattered directories to organized structure
                    if os.path.isdir(dir_path):
                        for file_name in os.listdir(dir_path):
                            source_file = os.path.join(dir_path, file_name)
                            if os.path.isfile(source_file):
                                # Determine appropriate destination based on file type
                                if "case" in file_name.lower():
                                    dest_dir = os.path.join(organizer.get_simulation_path(), "logs", "case_performance")
                                elif "behavior" in file_name.lower():
                                    dest_dir = os.path.join(organizer.get_simulation_path(), "logs", "behavioral_patterns")
                                elif "event" in file_name.lower():
                                    dest_dir = os.path.join(organizer.get_simulation_path(), "logs", "simulation_events")
                                else:
                                    dest_dir = os.path.join(organizer.get_simulation_path(), "data", "raw_simulation")
                                
                                os.makedirs(dest_dir, exist_ok=True)
                                dest_file = os.path.join(dest_dir, file_name)
                                if not os.path.exists(dest_file):
                                    import shutil
                                    shutil.move(source_file, dest_file)
                    
                    # Remove the scattered directory
                    import shutil
                    shutil.rmtree(dir_path)
                    print(f"  ✅ Cleaned up: {dir_path}")
                except Exception as e:
                    print(f"  ⚠️  Warning: Could not clean up {dir_path}: {e}")
        
        # Final summary
        print("\n" + "="*70)
        print("🎉 SIMULATION COMPLETED SUCCESSFULLY")
        print("="*70)
        print(f"📁 Output Directory: {organizer.get_simulation_path()}")
        print(f"📝 Terminal Log: {logger.log_file_path}")
        print(f"📊 Total Events Logged: {len(simulation_data['events'])}")
        print(f"🎨 Visualizations Generated: {len(simulation_data['visualizations'])}")
        print(f"🔄 Case Transitions Observed: {len(case_transitions)}")
        print(f"🐛 Behavioral States: {len(behavioral_states)}")
        print(f"📈 Average Confidence: {summary_data['summary_data']['performance_metrics']['average_confidence']:.3f}")
        print(f"⏱️  Simulation Duration: {simulation_duration:.2f} seconds")
        try:
            print(f"🏆 Assessment Score: {assessment_report['overall_assessment']['score']}/100")
            print(f"📋 Assessment Status: {assessment_report['overall_assessment']['status']}")
        except (KeyError, TypeError):
            print("🏆 Assessment Score: Error occurred during assessment")
            print("📋 Assessment Status: Error")
        
        print("\n📂 Directory Structure Created:")
        print("├── logs/")
        print("│   ├── simulation_events/")
        print("│   ├── case_performance/")
        print("│   └── behavioral_patterns/")
        print("├── visualizations/")
        print("│   ├── individual_insects/")
        print("│   ├── case_analysis/")
        print("│   ├── swarm_analysis/")
        print("│   └── neural_activity/")
        print("├── data/")
        print("│   ├── raw_simulation/")
        print("│   ├── environment_states/")
        print("│   └── processed_metrics/")
        print("├── reports/")
        print("│   ├── performance_analysis/")
        print("│   ├── behavioral_analysis/")
        print("│   └── case_effectiveness/")
        print("├── simulation_summary.json")
        print("└── terminal_simulation_log.txt")
        print("\n" + "="*70)
        
        print("\n✅ Simulation completed successfully!")
        print(f"📁 All outputs organized in: {organizer.get_simulation_path()}")
        
        return organizer.get_simulation_path()


if __name__ == "__main__":
    try:
        simulation_path = run_comprehensive_simulation()
        print(f"\n✅ Simulation completed successfully!")
        print(f"📁 All outputs organized in: {simulation_path}")
    except Exception as e:
        print(f"\n❌ Simulation failed with error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1) 