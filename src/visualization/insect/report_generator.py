"""
Comprehensive Report Generator for CEREBRUM Insect Simulations

This module generates detailed reports in both JSON and Markdown formats
for all simulation analysis areas.
"""

import json
import os
from datetime import datetime
from typing import Any, Dict

import numpy as np


class ComprehensiveReportGenerator:
    """Generates comprehensive reports in multiple formats."""
    
    def __init__(self, output_dir: str):
        self.output_dir = output_dir
        self.reports_dir = os.path.join(output_dir, "reports")
        os.makedirs(self.reports_dir, exist_ok=True)
        
    def generate_all_reports(self, simulation_data: Dict[str, Any]) -> Dict[str, str]:
        """Generate all reports in both JSON and Markdown formats."""
        reports = {}
        
        # Generate each type of report
        reports['performance'] = self.generate_performance_report(simulation_data)
        reports['behavioral'] = self.generate_behavioral_report(simulation_data)
        reports['case_analysis'] = self.generate_case_analysis_report(simulation_data)
        reports['neural_activity'] = self.generate_neural_activity_report(simulation_data)
        reports['swarm_analysis'] = self.generate_swarm_analysis_report(simulation_data)
        reports['comprehensive'] = self.generate_comprehensive_report(simulation_data)
        
        return reports
    
    def generate_performance_report(self, simulation_data: Dict[str, Any]) -> str:
        """Generate performance analysis report."""
        events = simulation_data.get("events", [])

        def _conf(event):
            return event.get("processed_data", {}).get("confidence", 0.0)

        # Calculate performance metrics from real event data only.
        confidences = [_conf(event) for event in events]
        insect_performance = {}

        for insect_name in set(event.get("insect_id", "unknown") for event in events):
            insect_events = [e for e in events if e.get("insect_id", "unknown") == insect_name]
            insect_confidences = [_conf(e) for e in insect_events]

            insect_performance[insect_name] = {
                "total_events": len(insect_events),
                "average_confidence": np.mean(insect_confidences),
                "max_confidence": np.max(insect_confidences),
                "min_confidence": np.min(insect_confidences),
                "confidence_std": np.std(insect_confidences),
                "success_rate": len([c for c in insect_confidences if c > 0.5]) / len(insect_confidences),
            }

        # Overall performance
        overall_performance = {
            "total_events": len(events),
            "average_confidence": np.mean(confidences),
            "max_confidence": np.max(confidences),
            "min_confidence": np.min(confidences),
            "confidence_std": np.std(confidences),
            "success_rate": len([c for c in confidences if c > 0.5]) / len(confidences),
            "insect_performance": insect_performance
        }
        
        # Save JSON report
        json_path = os.path.join(self.reports_dir, "performance_analysis", "performance_analysis_report.json")
        os.makedirs(os.path.dirname(json_path), exist_ok=True)
        with open(json_path, 'w') as f:
            json.dump(overall_performance, f, indent=2)
        
        # Generate Markdown report
        md_content = self._generate_performance_markdown(overall_performance)
        md_path = os.path.join(self.reports_dir, "performance_analysis", "performance_analysis_report.md")
        with open(md_path, 'w') as f:
            f.write(md_content)
        
        return json_path
    
    def _generate_performance_markdown(self, performance_data: Dict[str, Any]) -> str:
        """Generate Markdown content for performance report."""
        md = f"""# Performance Analysis Report

## Simulation Overview
- **Total Events**: {performance_data['total_events']}
- **Average Confidence**: {performance_data['average_confidence']:.3f}
- **Success Rate**: {performance_data['success_rate']:.1%}
- **Confidence Range**: {performance_data['min_confidence']:.3f} - {performance_data['max_confidence']:.3f}

## Individual Insect Performance

"""
        
        for insect_name, metrics in performance_data['insect_performance'].items():
            md += f"""### {insect_name}
- **Total Events**: {metrics['total_events']}
- **Average Confidence**: {metrics['average_confidence']:.3f}
- **Success Rate**: {metrics['success_rate']:.1%}

"""
        
        return md
    
    def generate_behavioral_report(self, simulation_data: Dict[str, Any]) -> str:
        """Generate behavioral analysis report."""
        events = simulation_data.get("events", [])
        
        # Analyze behavioral patterns
        behavioral_states = {}
        for event in events:
            state = event.get("behavioral_state", "unknown")
            behavioral_states[state] = behavioral_states.get(state, 0) + 1
        
        # Calculate behavioral metrics
        behavioral_analysis = {
            "total_events": len(events),
            "behavioral_states": behavioral_states,
            "behavior_success_rates": {},
            "energy_efficiency": {},
            "behavioral_transitions": {}
        }
        
        # Analyze success rates by behavior
        for state in behavioral_states.keys():
            state_events = [e for e in events if e.get("behavioral_state", "unknown") == state]
            if state_events:
                confidences = [e.get("processed_data", {}).get("confidence", 0.0) for e in state_events]
                behavioral_analysis["behavior_success_rates"][state] = {
                    "count": len(state_events),
                    "success_rate": len([c for c in confidences if c > 0.5]) / len(confidences),
                    "average_confidence": np.mean(confidences)
                }
        
        # Save JSON report
        json_path = os.path.join(self.reports_dir, "behavioral_analysis", "behavioral_analysis_report.json")
        os.makedirs(os.path.dirname(json_path), exist_ok=True)
        with open(json_path, 'w') as f:
            json.dump(behavioral_analysis, f, indent=2)
        
        # Generate Markdown report
        md_content = self._generate_behavioral_markdown(behavioral_analysis)
        md_path = os.path.join(self.reports_dir, "behavioral_analysis", "behavioral_analysis_report.md")
        with open(md_path, 'w') as f:
            f.write(md_content)
        
        return json_path
    
    def _generate_behavioral_markdown(self, behavioral_data: Dict[str, Any]) -> str:
        """Generate Markdown content for behavioral report."""
        md = f"""# Behavioral Analysis Report

## Overview
- **Total Events**: {behavioral_data['total_events']}
- **Unique Behavioral States**: {len(behavioral_data['behavioral_states'])}

## Behavioral State Distribution

"""
        
        for state, count in behavioral_data['behavioral_states'].items():
            percentage = (count / behavioral_data['total_events']) * 100
            md += f"- **{state}**: {count} events ({percentage:.1f}%)\n"
        
        md += "\n## Behavioral Success Rates\n\n"
        
        for state, metrics in behavioral_data['behavior_success_rates'].items():
            md += f"""### {state}
- **Event Count**: {metrics['count']}
- **Success Rate**: {metrics['success_rate']:.1%}
- **Average Confidence**: {metrics['average_confidence']:.3f}

"""
        
        return md
    
    def generate_case_analysis_report(self, simulation_data: Dict[str, Any]) -> str:
        """Generate case analysis report."""
        events = simulation_data.get("events", [])
        
        # Analyze case transitions
        case_transitions = {}
        for i in range(len(events) - 1):
            current_case = events[i].get("case", "unknown")
            next_case = events[i + 1].get("case", "unknown")
            if current_case != next_case:
                transition = f"{current_case}->{next_case}"
                case_transitions[transition] = case_transitions.get(transition, 0) + 1
        
        # Analyze case effectiveness (real confidence data only; no synthetic
        # "appropriateness" field fabricated from thin air).
        case_effectiveness = {}
        for case_name in set(event.get("case", "unknown") for event in events):
            case_events = [e for e in events if e.get("case", "unknown") == case_name]
            if case_events:
                case_confidences = [e.get("processed_data", {}).get("confidence", 0.0) for e in case_events]
                case_effectiveness[case_name] = {
                    "usage_count": len(case_events),
                    "average_confidence": np.mean(case_confidences),
                    "success_rate": len([c for c in case_confidences if c > 0.5]) / len(case_confidences),
                }
        
        case_analysis = {
            "total_events": len(events),
            "case_transitions": case_transitions,
            "case_effectiveness": case_effectiveness,
            "case_distribution": {}
        }
        
        # Calculate case distribution
        case_counts = {}
        for event in events:
            case = event.get("case", "unknown")
            case_counts[case] = case_counts.get(case, 0) + 1
        case_analysis["case_distribution"] = case_counts
        
        # Save JSON report
        json_path = os.path.join(self.reports_dir, "case_analysis", "case_analysis_report.json")
        os.makedirs(os.path.dirname(json_path), exist_ok=True)
        with open(json_path, 'w') as f:
            json.dump(case_analysis, f, indent=2)
        
        # Generate Markdown report
        md_content = self._generate_case_analysis_markdown(case_analysis)
        md_path = os.path.join(self.reports_dir, "case_analysis", "case_analysis_report.md")
        with open(md_path, 'w') as f:
            f.write(md_content)
        
        return json_path
    
    def _generate_case_analysis_markdown(self, case_data: Dict[str, Any]) -> str:
        """Generate Markdown content for case analysis report."""
        md = f"""# Case Analysis Report

## Overview
- **Total Events**: {case_data['total_events']}
- **Case Transitions**: {len(case_data['case_transitions'])}
- **Unique Cases**: {len(case_data['case_distribution'])}

## Case Distribution

"""
        
        for case, count in case_data['case_distribution'].items():
            percentage = (count / case_data['total_events']) * 100
            md += f"- **{case}**: {count} events ({percentage:.1f}%)\n"
        
        md += "\n## Case Transitions\n\n"
        
        for transition, count in case_data['case_transitions'].items():
            md += f"- **{transition}**: {count} occurrences\n"
        
        md += "\n## Case Effectiveness\n\n"
        
        for case, metrics in case_data['case_effectiveness'].items():
            md += f"""### {case}
- **Usage Count**: {metrics['usage_count']}
- **Success Rate**: {metrics['success_rate']:.1%}
- **Average Confidence**: {metrics['average_confidence']:.3f}


"""
        return md
    
    def generate_neural_activity_report(self, simulation_data: Dict[str, Any]) -> str:
        """Generate neural activity analysis report."""
        events = simulation_data.get("events", [])
        
        # Generate neural activity patterns for each insect.
        # No fabricated neural-region activity: event data only carries
        # confidence. We report honest event-derived summaries instead of
        # np.random.* values presented as measured neural activity.
        neural_activity = {
            "activity_patterns": {},
            "learning_progress": {},
            "memory_utilization": {}
        }
        
        insect_names = set(event.get("insect_id", "unknown") for event in events)
        for name in insect_names:
            insect_events = [e for e in events if e.get("insect_id", "unknown") == name]
            confidences = [e.get("processed_data", {}).get("confidence", 0.0) for e in insect_events]

            neural_activity["activity_patterns"][name] = {
                "mean_confidence": float(np.mean(confidences)) if confidences else 0.0,
                "max_confidence": float(np.max(confidences)) if confidences else 0.0,
                "min_confidence": float(np.min(confidences)) if confidences else 0.0,
                "sample_count": len(confidences),
                "note": "Neural-region activity is not present in event data; confidence summary shown.",
            }

            neural_activity["learning_progress"][name] = {
                "total_events": len(insect_events),
                "note": "No neural learning signal recorded; event counts shown.",
            }

            neural_activity["memory_utilization"][name] = {
                "note": "No memory-utilization measurements recorded.",
            }
        
        # Save JSON report
        json_path = os.path.join(self.reports_dir, "neural_activity", "neural_activity_report.json")
        os.makedirs(os.path.dirname(json_path), exist_ok=True)
        with open(json_path, 'w') as f:
            json.dump(neural_activity, f, indent=2)
        
        # Generate Markdown report
        md_content = self._generate_neural_activity_markdown(neural_activity)
        md_path = os.path.join(self.reports_dir, "neural_activity", "neural_activity_report.md")
        with open(md_path, 'w') as f:
            f.write(md_content)
        
        return json_path
    
    def _generate_neural_activity_markdown(self, neural_data: Dict[str, Any]) -> str:
        """Generate Markdown content for neural activity report."""
        md = """# Neural Activity Analysis Report

## Overview
This report analyzes the neural activity patterns, learning progress, and memory utilization across all insect models.

## Neural Activity Patterns

"""
        for insect_name, patterns in neural_data['activity_patterns'].items():
            md += f"""### {insect_name}
- **Mean Confidence**: {patterns['mean_confidence']:.3f}
- **Max Confidence**: {patterns['max_confidence']:.3f}
- **Min Confidence**: {patterns['min_confidence']:.3f}
- **Sample Count**: {patterns['sample_count']}
- **Note**: {patterns['note']}


"""
        
        md += "## Learning Progress\n\n"
        
        for insect_name, learning in neural_data['learning_progress'].items():
            md += f"""### {insect_name}
- **Total Events**: {learning['total_events']}
- **Note**: {learning['note']}


"""
        
        md += "## Memory Utilization\n\n"
        
        for insect_name, memory in neural_data['memory_utilization'].items():
            md += f"""### {insect_name}
- **Note**: {memory['note']}


"""
        
        return md
    
    def generate_swarm_analysis_report(self, simulation_data: Dict[str, Any]) -> str:
        """Generate swarm analysis report."""
        events = simulation_data.get("events", [])
        
        # Analyze swarm coordination. All metrics are derived from real event
        # data; no np.random.* fabrication.
        swarm_coordination = {}
        from collections import Counter as _Counter
        for step in range(0, 100, 25):  # Every 25 steps
            step_events = [e for e in events if e.get("step") == step]
            if step_events:
                confs = [e.get("processed_data", {}).get("confidence", 0.0) for e in step_events]
                states_in_step = [e.get("behavioral_state", "unknown") for e in step_events]
                mode_count = max(_Counter(states_in_step).values()) if states_in_step else 0
                behavioral_sync = mode_count / len(step_events) if step_events else 0.0
                swarm_coordination[f"step_{step}"] = {
                    "total_insects_active": len(step_events),
                    "average_confidence": float(np.mean(confs)),
                    "behavioral_synchronization": float(behavioral_sync),
                }
        
        # Collective behavior analysis (real event-derived summaries)
        all_confidences = [e.get("processed_data", {}).get("confidence", 0.0) for e in events]
        collective_behavior = {
            "average_confidence": float(np.mean(all_confidences)) if all_confidences else 0.0,
            "active_insects": len(set(e.get("insect_id", "unknown") for e in events)),
            "unique_behavioral_states": len(set(e.get("behavioral_state", "unknown") for e in events)),
        }
        
        swarm_analysis = {
            "total_events": len(events),
            "swarm_coordination": swarm_coordination,
            "collective_behavior": collective_behavior,
            "swarm_performance": {}
        }
        
        # Calculate swarm performance metrics from real data only
        sync_values = [v.get("behavioral_synchronization", 0.0) for v in swarm_coordination.values()]
        swarm_analysis["swarm_performance"] = {
            "overall_efficiency": collective_behavior["average_confidence"],
            "coordination_score": float(np.mean(sync_values)) if sync_values else 0.0,
            "average_confidence": collective_behavior["average_confidence"],
        }
        
        # Save JSON report
        json_path = os.path.join(self.reports_dir, "swarm_analysis", "swarm_analysis_report.json")
        os.makedirs(os.path.dirname(json_path), exist_ok=True)
        with open(json_path, 'w') as f:
            json.dump(swarm_analysis, f, indent=2)
        
        # Generate Markdown report
        md_content = self._generate_swarm_analysis_markdown(swarm_analysis)
        md_path = os.path.join(self.reports_dir, "swarm_analysis", "swarm_analysis_report.md")
        with open(md_path, 'w') as f:
            f.write(md_content)
        
        return json_path
    
    def _generate_swarm_analysis_markdown(self, swarm_data: Dict[str, Any]) -> str:
        """Generate Markdown content for swarm analysis report."""
        md = f"""# Swarm Analysis Report

## Overview
- **Total Events**: {swarm_data['total_events']}
- **Overall Efficiency (mean confidence)**: {swarm_data['swarm_performance']['overall_efficiency']:.3f}
- **Coordination Score**: {swarm_data['swarm_performance']['coordination_score']:.3f}

## Collective Behavior

- **Average Confidence**: {swarm_data['collective_behavior']['average_confidence']:.3f}
- **Active Insects**: {swarm_data['collective_behavior']['active_insects']}
- **Unique Behavioral States**: {swarm_data['collective_behavior']['unique_behavioral_states']}

## Swarm Coordination by Step


"""
        
        for step, coordination in swarm_data['swarm_coordination'].items():
            md += f"""### {step}
- **Active Insects**: {coordination['total_insects_active']}
- **Average Confidence**: {coordination['average_confidence']:.3f}
- **Behavioral Synchronization**: {coordination['behavioral_synchronization']:.3f}


"""
        
        return md
    
    def generate_comprehensive_report(self, simulation_data: Dict[str, Any]) -> str:
        """Generate comprehensive summary report."""
        events = simulation_data.get("events", [])
        
        # Calculate comprehensive metrics
        confidences = [event.get("processed_data", {}).get("confidence", 0.0) for event in events]
        
        comprehensive_summary = {
            "simulation_overview": {
                "total_events": len(events),
                "total_insects": len(set(event.get("insect_id", "unknown") for event in events)),
            },
            "performance_summary": {
                "average_confidence": np.mean(confidences),
                "success_rate": len([c for c in confidences if c > 0.5]) / len(confidences) if confidences else 0.0,
                "confidence_std": np.std(confidences)
            },
            "insect_summary": {},
            "case_summary": {},
            "behavioral_summary": {}
        }
        
        # Insect summary
        for insect_name in set(event.get("insect_id", "unknown") for event in events):
            insect_events = [e for e in events if e.get("insect_id", "unknown") == insect_name]
            insect_confidences = [e.get("processed_data", {}).get("confidence", 0.0) for e in insect_events]
            comprehensive_summary["insect_summary"][insect_name] = {
                "total_events": len(insect_events),
                "average_confidence": np.mean(insect_confidences),
                "success_rate": len([c for c in insect_confidences if c > 0.5]) / len(insect_confidences) if insect_confidences else 0.0
            }
        
        # Case summary
        case_counts = {}
        for event in events:
            case = event.get("case", "unknown")
            case_counts[case] = case_counts.get(case, 0) + 1
        comprehensive_summary["case_summary"] = case_counts
        
        # Behavioral summary
        behavioral_counts = {}
        for event in events:
            behavior = event.get("behavioral_state", "unknown")
            behavioral_counts[behavior] = behavioral_counts.get(behavior, 0) + 1
        comprehensive_summary["behavioral_summary"] = behavioral_counts
        
        # Save JSON report
        json_path = os.path.join(self.reports_dir, "comprehensive_summary.json")
        with open(json_path, 'w') as f:
            json.dump(comprehensive_summary, f, indent=2)
        
        # Generate Markdown report
        md_content = self._generate_comprehensive_markdown(comprehensive_summary)
        md_path = os.path.join(self.reports_dir, "comprehensive_summary.md")
        with open(md_path, 'w') as f:
            f.write(md_content)
        
        return json_path
    
    def _generate_comprehensive_markdown(self, summary_data: Dict[str, Any]) -> str:
        """Generate Markdown content for comprehensive report."""
        md = f"""# Comprehensive Simulation Summary Report

## Simulation Overview
- **Total Events**: {summary_data['simulation_overview']['total_events']}
- **Total Insects**: {summary_data['simulation_overview']['total_insects']}

## Performance Summary
- **Average Confidence**: {summary_data['performance_summary']['average_confidence']:.3f}
- **Success Rate**: {summary_data['performance_summary']['success_rate']:.1%}
- **Confidence Standard Deviation**: {summary_data['performance_summary']['confidence_std']:.3f}

## Individual Insect Performance

"""
        
        for insect_name, metrics in summary_data['insect_summary'].items():
            md += f"""### {insect_name}
- **Total Events**: {metrics['total_events']}
- **Average Confidence**: {metrics['average_confidence']:.3f}
- **Success Rate**: {metrics['success_rate']:.1%}

"""
        
        md += "## Case Distribution\n\n"
        
        for case, count in summary_data['case_summary'].items():
            percentage = (count / summary_data['simulation_overview']['total_events']) * 100
            md += f"- **{case}**: {count} events ({percentage:.1f}%)\n"
        
        md += "\n## Behavioral State Distribution\n\n"
        
        for behavior, count in summary_data['behavioral_summary'].items():
            percentage = (count / summary_data['simulation_overview']['total_events']) * 100
            md += f"- **{behavior}**: {count} events ({percentage:.1f}%)\n"
        
        md += f"""

---
*Report generated on {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}*
"""
        
        return md 