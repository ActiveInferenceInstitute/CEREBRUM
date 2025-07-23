"""
Simplified Insect Simulation Example for CEREBRUM

This example demonstrates core insect simulation functionality with proper output organization
and basic visualization capabilities.
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

# Add the project root to the path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../..')))

from src.models.insect.species import HoneybeeModel, AntModel, FruitFlyModel
from src.models.insect.base import BehavioralState, SensoryInput
from src.visualization.insect import InsectSimulationLogger, CasePerformanceLogger, BehavioralLogger
from src.utils.output_organizer import SimulationOutputOrganizer
from src.analysis.simulation_assessment import SimulationEffectivenessAnalyzer


def run_simplified_simulation():
    """
    Run a simplified insect simulation with proper output organization.
    """
    print("="*70)
    print("CEREBRUM SIMPLIFIED INSECT SIMULATION")
    print("="*70)
    
    # Initialize output organizer
    organizer = SimulationOutputOrganizer("simplified_insect_simulation")
    print(f"Output Directory: {organizer.get_simulation_path()}")
    
    # Initialize insect models
    print("\nInitializing Insect Models...")
    honeybee = HoneybeeModel()
    ant = AntModel()
    fruit_fly = FruitFlyModel()
    
    # Initialize loggers
    print("Initializing Logging Components...")
    simulation_logger = InsectSimulationLogger()
    case_logger = CasePerformanceLogger()
    behavior_logger = BehavioralLogger()
    
    print("✅ All components initialized successfully")
    
    # Simulation parameters
    simulation_steps = 50
    insects = [honeybee, ant, fruit_fly]
    insect_names = ["Honeybee", "Ant", "Fruit_Fly"]
    
    # Track simulation data
    simulation_data = {
        "events": [],
        "case_performance": [],
        "behavioral_patterns": [],
        "visualizations": []
    }
    
    print(f"\nRunning simulation for {simulation_steps} steps...")
    
    # Main simulation loop
    for step in range(simulation_steps):
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
            
            # Get current state
            current_case = insect.current_case
            current_behavior = insect.behavioral_state
            current_position = [0.0, 0.0, 0.0]  # Default position
            
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
                    "action_selected": processed_data.get("action", "unknown"),
                    "confidence": processed_data.get("confidence", 0.0),
                    "total_actions": insect.performance_metrics.get("total_actions", 0),
                    "case_transformations": insect.performance_metrics.get("case_transformations", 0)
                }
            }
            
            simulation_data["events"].append(event_data)
            simulation_logger.log_event(insect, "simulation_step", event_data)
            
            # Log case performance
            case_metrics = {
                "case": current_case.value,
                "performance": processed_data.get("confidence", 0.0),
                "total_actions": insect.performance_metrics.get("total_actions", 0),
                "success_rate": 0.8 if processed_data.get("confidence", 0.0) > 0.5 else 0.3
            }
            case_logger.log_case_metrics(current_case, case_metrics, event_data, 1.0)
            
            # Log behavioral patterns
            behavior_data = {
                "behavior_type": current_behavior.value,
                "duration": 1.0,
                "success": processed_data.get("confidence", 0.0) > 0.5,
                "energy_cost": 0.01
            }
            behavior_logger.log_behavior_context(
                current_behavior.value, 
                event_data, 
                behavior_data["success"], 
                behavior_data["duration"]
            )
            
            # Update performance metrics
            insect.performance_metrics["total_actions"] += 1
            
            # Occasionally trigger case transformations
            if step % 10 == 0 and step > 0:
                # Simulate case transformation
                from src.core.model import Case
                cases = list(Case)
                new_case = np.random.choice(cases)
                if insect.transform_case(new_case):
                    insect.performance_metrics["case_transformations"] += 1
                    print(f"  {name} transformed case to {new_case.value}")
            
            # Generate simple visualizations every 25 steps
            if step % 25 == 0 and step > 0:
                print(f"  Generating visualizations for {name} at step {step}...")
                
                # Create simple performance plot
                fig, ax = plt.subplots(figsize=(10, 8))
                
                # Plot performance metrics
                metrics = insect.performance_metrics
                labels = list(metrics.keys())
                values = list(metrics.values())
                
                # Filter out non-numeric values
                numeric_labels = []
                numeric_values = []
                for label, value in zip(labels, values):
                    if isinstance(value, (int, float)):
                        numeric_labels.append(label)
                        numeric_values.append(value)
                
                if numeric_values:
                    ax.bar(numeric_labels, numeric_values)
                    ax.set_title(f'{name} Performance Metrics - Step {step}')
                    ax.set_ylabel('Value')
                    ax.tick_params(axis='x', rotation=45)
                    
                    # Save visualization
                    viz_filename = f"{name.lower()}_performance_step_{step:03d}.png"
                    viz_path = organizer.save_visualization(fig, viz_filename, "individual_insects")
                    simulation_data["visualizations"].append(viz_path)
                    plt.close(fig)
                else:
                    plt.close(fig)
    
    print(f"\nSimulation completed! Generated {len(simulation_data['visualizations'])} visualizations")
    
    # Generate comprehensive reports
    print("\nGenerating comprehensive reports...")
    
    # Simulation summary
    summary_data = {
        "simulation_steps": simulation_steps,
        "total_insects": len(insects),
        "total_events": len(simulation_data["events"]),
        "total_visualizations": len(simulation_data["visualizations"]),
        "insect_types": insect_names,
        "simulation_duration": time.time() - time.time(),  # Will be updated
        "case_transitions": {},
        "behavioral_states": {},
        "performance_metrics": {}
    }
    
    # Analyze case transitions
    case_transitions = {}
    for i in range(len(simulation_data["events"]) - 1):
        current_case = simulation_data["events"][i]["case"]
        next_case = simulation_data["events"][i + 1]["case"]
        if current_case != next_case:
            transition = f"{current_case}->{next_case}"
            case_transitions[transition] = case_transitions.get(transition, 0) + 1
    
    summary_data["case_transitions"] = case_transitions
    
    # Analyze behavioral states
    behavioral_states = {}
    for event in simulation_data["events"]:
        state = event["behavioral_state"]
        behavioral_states[state] = behavioral_states.get(state, 0) + 1
    
    summary_data["behavioral_states"] = behavioral_states
    
    # Calculate performance metrics
    confidences = [event["processed_data"]["confidence"] for event in simulation_data["events"]]
    summary_data["performance_metrics"] = {
        "average_confidence": np.mean(confidences),
        "max_confidence": np.max(confidences),
        "min_confidence": np.min(confidences),
        "confidence_std": np.std(confidences)
    }
    
    # Save simulation summary
    organizer.create_simulation_summary(summary_data)
    
    # Export simulation data
    organizer.save_report(simulation_data, "raw_simulation", "simulation_data.json")
    
    # Export case performance report
    case_report = case_logger.get_case_effectiveness_report()
    organizer.save_report(case_report, "case_effectiveness", "case_effectiveness_report.json")
    
    # Export behavioral analysis
    behavioral_report = behavior_logger.get_behavioral_analysis()
    organizer.save_report(behavioral_report, "behavioral_analysis", "behavioral_analysis_report.json")
    
    # Run effectiveness assessment
    print("\nRunning simulation effectiveness assessment...")
    analyzer = SimulationEffectivenessAnalyzer(organizer.get_simulation_path())
    assessment_report = analyzer.generate_comprehensive_report()
    analyzer.save_assessment_report(assessment_report, "simulation_effectiveness_assessment.json")
    
    # Print final summary
    print("\n" + "="*70)
    print("SIMULATION COMPLETED SUCCESSFULLY")
    print("="*70)
    print(f"Output Directory: {organizer.get_simulation_path()}")
    print(f"Total Events Logged: {len(simulation_data['events'])}")
    print(f"Visualizations Generated: {len(simulation_data['visualizations'])}")
    print(f"Case Transitions Observed: {len(case_transitions)}")
    print(f"Behavioral States: {len(behavioral_states)}")
    print(f"Average Confidence: {summary_data['performance_metrics']['average_confidence']:.3f}")
    print(f"Assessment Score: {assessment_report['overall_assessment']['score']}/100")
    print(f"Assessment Status: {assessment_report['overall_assessment']['status']}")
    
    print("\nDirectory Structure Created:")
    print("├── logs/")
    print("│   ├── simulation_events/")
    print("│   ├── case_performance/")
    print("│   └── behavioral_patterns/")
    print("├── visualizations/")
    print("│   └── individual_insects/")
    print("├── data/")
    print("│   └── raw_simulation/")
    print("├── reports/")
    print("│   ├── performance_analysis/")
    print("│   ├── behavioral_analysis/")
    print("│   └── case_effectiveness/")
    print("└── simulation_summary.json")
    
    print("\n" + "="*70)
    
    return organizer.get_simulation_path()


if __name__ == "__main__":
    try:
        simulation_path = run_simplified_simulation()
        print(f"\n✅ Simulation completed successfully!")
        print(f"📁 All outputs organized in: {simulation_path}")
    except Exception as e:
        print(f"\n❌ Simulation failed with error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1) 