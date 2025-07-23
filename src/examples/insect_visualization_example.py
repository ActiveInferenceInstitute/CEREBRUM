#!/usr/bin/env python3
"""
Insect Visualization Example

This example demonstrates the comprehensive visualization capabilities for insect models,
including case-based reasoning visualization, neural activity tracking, behavioral patterns,
and swarm dynamics with special emphasis on case relevance and effectiveness.
"""

import sys
import os
import time
import numpy as np
import matplotlib
matplotlib.use('Agg')  # Use non-interactive backend
import matplotlib.pyplot as plt
from typing import List, Dict, Any

# Add the src directory to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from src.models.insect import (
    InsectModel, 
    HoneybeeModel, 
    AntModel, 
    FruitFlyModel
)
from src.core.model import Case
from src.models.insect.base import BehavioralState
from src.visualization.insect import (
    InsectVisualizer,
    InsectSimulationVisualizer,
    CaseRelevanceVisualizer,
    NeuralStructureVisualizer,
    BrainActivityVisualizer,
    BehaviorPatternVisualizer,
    SwarmBehaviorVisualizer,
    InsectCaseVisualizer,
    CaseTransitionVisualizer,
    CaseEffectivenessVisualizer,
    InsectSimulationLogger,
    CasePerformanceLogger,
    BehavioralLogger,
    InsectAnimationCreator,
    SwarmAnimationCreator
)
from src.utils.path_utils import get_output_dir


def create_sample_environment() -> Dict[str, Any]:
    """Create a sample environment for the simulation."""
    return {
        'obstacles': [
            np.array([2.0, 2.0, 0.0]),
            np.array([-2.0, -1.0, 0.0]),
            np.array([1.0, -2.0, 0.0])
        ],
        'food_sources': [
            np.array([3.0, 3.0, 0.0]),
            np.array([-3.0, 3.0, 0.0]),
            np.array([0.0, -3.0, 0.0])
        ],
        'nest_location': np.array([0.0, 0.0, 0.0]),
        'temperature': 25.0,
        'humidity': 0.6,
        'light_level': 0.8
    }


def run_insect_simulation(insect: InsectModel, environment: Dict[str, Any], 
                         steps: int = 100) -> List[Dict[str, Any]]:
    """
    Run a simulation with an insect model and track all events.
    
    Args:
        insect: The insect model to simulate
        environment: Environment data
        steps: Number of simulation steps
        
    Returns:
        List of simulation events
    """
    events = []
    
    # Initialize insect position
    if hasattr(insect, 'position'):
        insect.position = np.array([0.0, 0.0, 0.0])
    
    for step in range(steps):
        # Create context based on current state
        context = {
            'step': step,
            'timestamp': time.time(),
            'environment': environment,
            'energy_level': 0.8 - (step * 0.001),  # Decreasing energy
            'food_available': step % 20 < 10,  # Periodic food availability
            'threat_level': 0.1 if step % 30 == 0 else 0.0,  # Periodic threats
            'colony_size': 50 + step // 10  # Growing colony
        }
        
        # Generate sensory input
        from src.models.insect.base import SensoryInput
        sensory_input = SensoryInput(
            visual=np.random.randn(10),
            olfactory=np.random.randn(5),
            pheromonal=np.random.randn(3),
            mechanosensory=np.random.randn(4),
            timestamp=time.time()
        )
        
        # Process sensory input
        processed_data = insect.process_sensory_input(sensory_input)
        
        # Select action based on current case and context
        action = insect.select_action(context)
        
        # Update behavioral state based on action
        if action.action_type == "foraging":
            insect.update_behavioral_state(BehavioralState.FORAGING)
        elif action.action_type == "navigating":
            insect.update_behavioral_state(BehavioralState.NAVIGATING)
        elif action.action_type == "communicating":
            insect.update_behavioral_state(BehavioralState.COMMUNICATING)
        else:
            insect.update_behavioral_state(BehavioralState.IDLE)
        
        # Randomly change case based on context
        if step % 15 == 0:  # Change case every 15 steps
            available_cases = [case for case in Case if case != insect.current_case]
            if available_cases:
                new_case = np.random.choice(available_cases)
                insect.transform_case(new_case)
        
        # Update position (simple movement)
        if hasattr(insect, 'position'):
            movement = np.random.randn(3) * 0.1
            insect.position += movement
        
        # Record event
        event = {
            'step': step,
            'timestamp': time.time(),
            'case': insect.current_case,
            'behavioral_state': insect.behavioral_state,
            'position': insect.position.tolist() if hasattr(insect, 'position') else [0.0, 0.0, 0.0],
            'action': action.action_type,
            'performance': insect.get_performance_summary(),
            'context': context,
            'sensory_data': {k: v.tolist() if isinstance(v, np.ndarray) else v for k, v in processed_data.items()}
        }
        
        events.append(event)
        
        # Add some delay for visualization
        time.sleep(0.01)
    
    return events


def demonstrate_individual_insect_visualization():
    """Demonstrate individual insect visualization capabilities."""
    print("=== Individual Insect Visualization Demo ===")
    
    # Create environment
    environment = create_sample_environment()
    
    # Create insect models
    honeybee = HoneybeeModel()
    ant = AntModel()
    fruit_fly = FruitFlyModel()
    
    insects = [honeybee, ant, fruit_fly]
    insect_names = ['Honeybee', 'Ant', 'Fruit Fly']
    
    # Initialize visualizers
    visualizer = InsectVisualizer()
    case_visualizer = InsectCaseVisualizer()
    case_relevance_visualizer = CaseRelevanceVisualizer()
    neural_visualizer = NeuralStructureVisualizer()
    brain_visualizer = BrainActivityVisualizer()
    behavior_visualizer = BehaviorPatternVisualizer()
    
    # Initialize loggers
    simulation_logger = InsectSimulationLogger()
    case_logger = CasePerformanceLogger()
    behavior_logger = BehavioralLogger()
    
    # Run simulations and create visualizations
    for insect, name in zip(insects, insect_names):
        print(f"\nRunning simulation for {name}...")
        
        # Run simulation
        events = run_insect_simulation(insect, environment, steps=50)
        
        # Update visualizer history
        for event in events:
            visualizer.update_history(insect, event['timestamp'])
            case_visualizer.track_case_usage(insect, event['context'])
            
            # Log events
            simulation_logger.log_event(insect, event['action'], event['context'])
            
            # Log case performance
            success_rate = 0.8 if event['action'] in ['foraging', 'navigating'] else 0.6
            energy_efficiency = 0.7 if event['behavioral_state'] == BehavioralState.FORAGING else 0.5
            information_gain = 0.6 if event['action'] == 'communicating' else 0.3
            
            # Filter performance metrics to only include numeric values
            performance_metrics = {
                k: v for k, v in event['performance'].items() 
                if isinstance(v, (int, float)) and not isinstance(v, bool)
            }
            
            case_logger.log_case_metrics(
                insect.current_case, 
                performance_metrics, 
                event['context'], 
                1.0
            )
            
            # Log behavioral patterns
            behavior_logger.log_behavior_context(
                event['action'],
                event['context'],
                success_rate > 0.5,
                1.0
            )
        
        # Create visualizations
        output_dir = get_output_dir()
        timestamp = time.strftime("%Y%m%d_%H%M%S")
        base_path = os.path.join(output_dir, f"insect_visualization_{name.lower().replace(' ', '_')}_{timestamp}")
        
        # Case transitions
        fig1 = visualizer.visualize_case_transitions()
        fig1.savefig(f"{base_path}_case_transitions.png", dpi=150, bbox_inches='tight')
        plt.close(fig1)
        
        # Behavioral patterns
        fig2 = visualizer.visualize_behavioral_patterns()
        fig2.savefig(f"{base_path}_behavioral_patterns.png", dpi=150, bbox_inches='tight')
        plt.close(fig2)
        
        # Neural activity
        fig3 = visualizer.visualize_neural_activity()
        fig3.savefig(f"{base_path}_neural_activity.png", dpi=150, bbox_inches='tight')
        plt.close(fig3)
        
        # Comprehensive dashboard
        fig4 = visualizer.create_comprehensive_dashboard(insect)
        fig4.savefig(f"{base_path}_dashboard.png", dpi=150, bbox_inches='tight')
        plt.close(fig4)
        
        # Case effectiveness
        context = {'food_available': True, 'threat_level': 0.1, 'energy_level': 0.7}
        fig5 = case_relevance_visualizer.visualize_case_relevance(insect, context)
        fig5.savefig(f"{base_path}_case_relevance.png", dpi=150, bbox_inches='tight')
        plt.close(fig5)
        
        # Brain activity
        fig6 = brain_visualizer.visualize_brain_activity(insect)
        fig6.savefig(f"{base_path}_brain_activity.png", dpi=150, bbox_inches='tight')
        plt.close(fig6)
        
        # Behavior timeline
        fig7 = behavior_visualizer.visualize_behavior_timeline()
        fig7.savefig(f"{base_path}_behavior_timeline.png", dpi=150, bbox_inches='tight')
        plt.close(fig7)
        
        print(f"Created visualizations for {name} in {base_path}")
    
    # Export logged data
    simulation_logger.export_data('json')
    case_logger.export_case_report('json')
    behavior_logger.export_behavioral_report('json')
    
    print("\nIndividual insect visualization demo completed!")


def demonstrate_swarm_visualization():
    """Demonstrate swarm visualization capabilities."""
    print("\n=== Swarm Visualization Demo ===")
    
    # Create environment
    environment = create_sample_environment()
    
    # Create swarm of insects
    swarm_size = 10
    insects = []
    
    for i in range(swarm_size):
        if i < 3:
            insect = HoneybeeModel()
        elif i < 7:
            insect = AntModel()
        else:
            insect = FruitFlyModel()
        
        # Set initial positions in a circle
        angle = 2 * np.pi * i / swarm_size
        radius = 2.0
        insect.position = np.array([radius * np.cos(angle), radius * np.sin(angle), 0.0])
        
        insects.append(insect)
    
    # Initialize swarm visualizer
    swarm_visualizer = SwarmBehaviorVisualizer()
    simulation_visualizer = InsectSimulationVisualizer()
    
    # Initialize animation creator
    animation_creator = SwarmAnimationCreator()
    
    # Track swarm over time
    swarm_history = []
    
    for step in range(50):
        # Update each insect
        insect_positions = []
        insect_states = []
        insect_cases = []
        
        for insect in insects:
            # Simple swarm behavior
            center = np.mean([insect.position for insect in insects], axis=0)
            direction_to_center = center - insect.position
            insect.position += direction_to_center * 0.1
            
            # Add some random movement
            insect.position += np.random.randn(3) * 0.05
            
            # Update case randomly
            if step % 10 == 0:
                available_cases = [case for case in Case if case != insect.current_case]
                if available_cases:
                    new_case = np.random.choice(available_cases)
                    insect.transform_case(new_case)
            
            # Update behavioral state
            if np.random.random() < 0.3:
                insect.update_behavioral_state(BehavioralState.FORAGING)
            elif np.random.random() < 0.3:
                insect.update_behavioral_state(BehavioralState.NAVIGATING)
            else:
                insect.update_behavioral_state(BehavioralState.IDLE)
            
            insect_positions.append(insect.position.copy())
            insect_states.append(insect.behavioral_state)
            insect_cases.append(insect.current_case)
        
        # Calculate swarm metrics
        positions = np.array(insect_positions)
        center_of_mass = np.mean(positions, axis=0)
        
        # Calculate dispersion (average distance from center)
        distances = np.linalg.norm(positions - center_of_mass, axis=1)
        dispersion = np.mean(distances)
        
        # Calculate cohesion (inverse of dispersion)
        cohesion = 1.0 / (1.0 + dispersion)
        
        # Track swarm state
        swarm_visualizer.track_swarm_state(insects, center_of_mass, dispersion, cohesion)
        
        # Record swarm history
        swarm_history.append({
            'timestamp': time.time(),
            'swarm_center': center_of_mass,
            'swarm_dispersion': dispersion,
            'swarm_cohesion': cohesion,
            'insect_positions': insect_positions,
            'insect_states': insect_states,
            'insect_cases': insect_cases,
            'collective_performance': np.mean([insect.get_performance_summary()['total_actions'] for insect in insects])
        })
        
        time.sleep(0.01)
    
    # Create swarm visualizations
    output_dir = get_output_dir()
    timestamp = time.strftime("%Y%m%d_%H%M%S")
    base_path = os.path.join(output_dir, f"swarm_visualization_{timestamp}")
    
    # Swarm dynamics
    fig1 = swarm_visualizer.visualize_swarm_dynamics()
    fig1.savefig(f"{base_path}_swarm_dynamics.png", dpi=150, bbox_inches='tight')
    plt.close(fig1)
    
    # Current swarm state
    fig2 = swarm_visualizer.visualize_swarm_state(insects, environment)
    fig2.savefig(f"{base_path}_swarm_state.png", dpi=150, bbox_inches='tight')
    plt.close(fig2)
    
    # Simulation state
    fig3 = simulation_visualizer.visualize_simulation_state(insects, environment, time.time())
    fig3.savefig(f"{base_path}_simulation_state.png", dpi=150, bbox_inches='tight')
    plt.close(fig3)
    
    # Create animations
    try:
        # Swarm animation
        anim1 = animation_creator.create_swarm_animation(swarm_history, environment)
        if anim1:
            anim1.save(f"{base_path}_swarm_animation.gif", writer='pillow', fps=5)
        
        # Collective behavior animation
        anim2 = animation_creator.create_collective_behavior_animation(swarm_history)
        if anim2:
            anim2.save(f"{base_path}_collective_behavior.gif", writer='pillow', fps=5)
        
        print(f"Created swarm animations in {base_path}")
    except Exception as e:
        print(f"Animation creation failed: {e}")
    
    print("Swarm visualization demo completed!")


def demonstrate_case_analysis():
    """Demonstrate comprehensive case analysis capabilities."""
    print("\n=== Case Analysis Demo ===")
    
    # Create environment
    environment = create_sample_environment()
    
    # Create insect for case analysis
    insect = HoneybeeModel()
    
    # Initialize case analysis visualizers
    case_visualizer = InsectCaseVisualizer()
    transition_visualizer = CaseTransitionVisualizer()
    effectiveness_visualizer = CaseEffectivenessVisualizer()
    
    # Test each case in different contexts
    contexts = [
        {'food_available': True, 'threat_level': 0.0, 'energy_level': 0.9},  # Optimal foraging
        {'food_available': False, 'threat_level': 0.8, 'energy_level': 0.3},  # High threat
        {'food_available': True, 'threat_level': 0.1, 'energy_level': 0.7},  # Normal conditions
        {'food_available': False, 'threat_level': 0.0, 'energy_level': 0.5},  # Low resources
    ]
    
    context_names = ['Optimal Foraging', 'High Threat', 'Normal Conditions', 'Low Resources']
    
    for context, context_name in zip(contexts, context_names):
        print(f"\nTesting cases in context: {context_name}")
        
        for case in Case:
            # Set case
            insect.transform_case(case)
            
            # Run short simulation
            events = run_insect_simulation(insect, environment, steps=20)
            
            # Calculate performance metrics
            total_actions = sum(event['performance']['total_actions'] for event in events)
            success_rate = 0.8 if case in [Case.ACCUSATIVE, Case.DATIVE] else 0.6
            energy_efficiency = 0.7 if case == Case.INSTRUMENTAL else 0.5
            information_gain = 0.6 if case == Case.DATIVE else 0.3
            
            # Track case performance
            effectiveness_visualizer.track_effectiveness(case, {'total_actions': total_actions}, context)
            
            # Track transitions
            if len(events) > 1:
                for i in range(len(events) - 1):
                    from_case = events[i]['case']
                    to_case = events[i + 1]['case']
                    if from_case != to_case:
                        transition_visualizer.track_transition(from_case, to_case, context, True)
            
            print(f"  {case.value}: {total_actions} actions, success rate: {success_rate:.2f}")
    
    # Create case analysis visualizations
    output_dir = get_output_dir()
    timestamp = time.strftime("%Y%m%d_%H%M%S")
    base_path = os.path.join(output_dir, f"case_analysis_{timestamp}")
    
    # Case distribution
    fig1 = case_visualizer.visualize_case_distribution()
    fig1.savefig(f"{base_path}_case_distribution.png", dpi=150, bbox_inches='tight')
    plt.close(fig1)
    
    # Case effectiveness
    context = {'food_available': True, 'threat_level': 0.1, 'energy_level': 0.7}
    fig2 = case_visualizer.visualize_case_effectiveness(insect, context)
    fig2.savefig(f"{base_path}_case_effectiveness.png", dpi=150, bbox_inches='tight')
    plt.close(fig2)
    
    # Transition patterns
    fig3 = transition_visualizer.visualize_transition_patterns()
    fig3.savefig(f"{base_path}_transition_patterns.png", dpi=150, bbox_inches='tight')
    plt.close(fig3)
    
    # Effectiveness trends
    fig4 = effectiveness_visualizer.visualize_effectiveness_trends()
    fig4.savefig(f"{base_path}_effectiveness_trends.png", dpi=150, bbox_inches='tight')
    plt.close(fig4)
    
    print("Case analysis demo completed!")


def main():
    """Main function to run all visualization demonstrations."""
    print("CEREBRUM Insect Visualization Example")
    print("=" * 50)
    
    try:
        # Demonstrate individual insect visualization
        demonstrate_individual_insect_visualization()
        
        # Demonstrate swarm visualization
        demonstrate_swarm_visualization()
        
        # Demonstrate case analysis
        demonstrate_case_analysis()
        
        print("\n" + "=" * 50)
        print("All visualization demonstrations completed successfully!")
        print(f"Check the output directory: {get_output_dir()}")
        
    except Exception as e:
        print(f"Error during visualization demo: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main() 