"""
Comprehensive Simulation Assessment for CEREBRUM Insect Models

This module provides detailed analysis and assessment of simulation effectiveness,
data quality, and system performance across all insect models and components.
"""

import os
import json
import numpy as np
import pandas as pd
from typing import Dict, List, Any, Optional, Tuple
from datetime import datetime
from collections import defaultdict, Counter
import matplotlib.pyplot as plt
import seaborn as sns

from src.utils.path_utils import get_output_dir
from src.core.model import Case
from src.models.insect.base import BehavioralState


class SimulationEffectivenessAnalyzer:
    """
    Comprehensive analyzer for simulation effectiveness and data quality.
    """
    
    def __init__(self, output_dir: Optional[str] = None):
        """
        Initialize the simulation analyzer.
        
        Args:
            output_dir: Directory containing simulation outputs
        """
        self.output_dir = output_dir or get_output_dir()
        self.assessment_results = {}
        
    def analyze_simulation_data_quality(self) -> Dict[str, Any]:
        """
        Analyze the quality and completeness of simulation data.
        
        Returns:
            Dictionary containing data quality metrics
        """
        quality_metrics = {
            "data_completeness": {},
            "data_consistency": {},
            "data_richness": {},
            "issues_found": []
        }
        
        # Check for required files and directories
        required_structure = [
            "insect_simulation_logs",
            "case_performance_logs", 
            "behavioral_logs",
            "insect_visualizations"
        ]
        
        for structure in required_structure:
            path = os.path.join(self.output_dir, structure)
            if os.path.exists(path):
                quality_metrics["data_completeness"][structure] = "present"
            else:
                quality_metrics["data_completeness"][structure] = "missing"
                quality_metrics["issues_found"].append(f"Missing directory: {structure}")
        
        # Analyze simulation events
        events_file = os.path.join(self.output_dir, "insect_simulation_logs", "simulation_events.json")
        if os.path.exists(events_file):
            try:
                with open(events_file, 'r') as f:
                    events_data = [json.loads(line) for line in f]
                
                quality_metrics["data_richness"]["total_events"] = len(events_data)
                quality_metrics["data_richness"]["unique_insects"] = len(set(e.get("insect_id", "") for e in events_data))
                quality_metrics["data_richness"]["event_types"] = list(set(e.get("event_type", "") for e in events_data))
                
                # Check data consistency
                required_fields = ["timestamp", "event_type", "insect_id", "case", "behavioral_state"]
                missing_fields = []
                for event in events_data[:10]:  # Sample first 10 events
                    for field in required_fields:
                        if field not in event:
                            missing_fields.append(field)
                
                if missing_fields:
                    quality_metrics["data_consistency"]["missing_fields"] = list(set(missing_fields))
                    quality_metrics["issues_found"].append(f"Missing fields in events: {missing_fields}")
                else:
                    quality_metrics["data_consistency"]["missing_fields"] = []
                    
            except Exception as e:
                quality_metrics["issues_found"].append(f"Error reading events file: {e}")
        else:
            quality_metrics["issues_found"].append("Simulation events file not found")
        
        # Analyze case performance data
        case_file = os.path.join(self.output_dir, "case_performance_logs", "case_report_20250723_130501.json")
        if os.path.exists(case_file):
            try:
                with open(case_file, 'r') as f:
                    case_data = json.load(f)
                
                quality_metrics["data_richness"]["case_effectiveness"] = len(case_data.get("case_effectiveness", {}))
                quality_metrics["data_richness"]["context_analysis"] = len(case_data.get("context_analysis", {}))
                
            except Exception as e:
                quality_metrics["issues_found"].append(f"Error reading case performance: {e}")
        
        return quality_metrics
    
    def analyze_behavioral_patterns(self) -> Dict[str, Any]:
        """
        Analyze behavioral patterns and state transitions.
        
        Returns:
            Dictionary containing behavioral analysis
        """
        behavioral_analysis = {
            "state_transitions": {},
            "behavioral_diversity": {},
            "pattern_consistency": {},
            "issues": []
        }
        
        events_file = os.path.join(self.output_dir, "insect_simulation_logs", "simulation_events.json")
        if not os.path.exists(events_file):
            behavioral_analysis["issues"].append("No simulation events found")
            return behavioral_analysis
        
        try:
            with open(events_file, 'r') as f:
                events_data = [json.loads(line) for line in f]
            
            # Analyze state transitions
            state_transitions = []
            for i in range(len(events_data) - 1):
                current_state = events_data[i].get("behavioral_state", "unknown")
                next_state = events_data[i + 1].get("behavioral_state", "unknown")
                state_transitions.append((current_state, next_state))
            
            transition_counts = Counter(state_transitions)
            behavioral_analysis["state_transitions"] = dict(transition_counts.most_common(10))
            
            # Analyze behavioral diversity
            all_states = [e.get("behavioral_state", "unknown") for e in events_data]
            behavioral_analysis["behavioral_diversity"]["unique_states"] = len(set(all_states))
            behavioral_analysis["behavioral_diversity"]["state_distribution"] = dict(Counter(all_states))
            
            # Check for pattern consistency
            if len(set(all_states)) == 1:
                behavioral_analysis["issues"].append("All insects remain in same behavioral state")
                behavioral_analysis["pattern_consistency"]["warning"] = "Low behavioral diversity"
            else:
                behavioral_analysis["pattern_consistency"]["status"] = "Good behavioral diversity"
                
        except Exception as e:
            behavioral_analysis["issues"].append(f"Error analyzing behavioral patterns: {e}")
        
        return behavioral_analysis
    
    def analyze_case_effectiveness(self) -> Dict[str, Any]:
        """
        Analyze case-based reasoning effectiveness.
        
        Returns:
            Dictionary containing case effectiveness analysis
        """
        case_analysis = {
            "case_usage": {},
            "case_transitions": {},
            "effectiveness_metrics": {},
            "issues": []
        }
        
        events_file = os.path.join(self.output_dir, "insect_simulation_logs", "simulation_events.json")
        if not os.path.exists(events_file):
            case_analysis["issues"].append("No simulation events found")
            return case_analysis
        
        try:
            with open(events_file, 'r') as f:
                events_data = [json.loads(line) for line in f]
            
            # Analyze case usage
            all_cases = [e.get("case", "unknown") for e in events_data]
            case_analysis["case_usage"]["total_cases"] = len(all_cases)
            case_analysis["case_usage"]["unique_cases"] = len(set(all_cases))
            case_analysis["case_usage"]["case_distribution"] = dict(Counter(all_cases))
            
            # Analyze case transitions
            case_transitions = []
            for i in range(len(events_data) - 1):
                current_case = events_data[i].get("case", "unknown")
                next_case = events_data[i + 1].get("case", "unknown")
                if current_case != next_case:
                    case_transitions.append((current_case, next_case))
            
            case_analysis["case_transitions"]["total_transitions"] = len(case_transitions)
            case_analysis["case_transitions"]["transition_types"] = dict(Counter(case_transitions))
            
            # Check for case effectiveness issues
            if len(set(all_cases)) == 1:
                case_analysis["issues"].append("No case transitions observed")
                case_analysis["effectiveness_metrics"]["warning"] = "Static case usage"
            else:
                case_analysis["effectiveness_metrics"]["status"] = "Dynamic case usage observed"
                
        except Exception as e:
            case_analysis["issues"].append(f"Error analyzing case effectiveness: {e}")
        
        return case_analysis
    
    def analyze_visualization_outputs(self) -> Dict[str, Any]:
        """
        Analyze visualization outputs and their quality.
        
        Returns:
            Dictionary containing visualization analysis
        """
        viz_analysis = {
            "visualization_count": 0,
            "visualization_types": {},
            "file_sizes": {},
            "issues": []
        }
        
        # Count visualization files
        png_files = []
        for root, dirs, files in os.walk(self.output_dir):
            for file in files:
                if file.endswith('.png'):
                    png_files.append(os.path.join(root, file))
        
        viz_analysis["visualization_count"] = len(png_files)
        
        if png_files:
            # Analyze file sizes
            file_sizes = [os.path.getsize(f) for f in png_files]
            viz_analysis["file_sizes"]["total_size_mb"] = sum(file_sizes) / (1024 * 1024)
            viz_analysis["file_sizes"]["average_size_kb"] = np.mean(file_sizes) / 1024
            
            # Categorize visualization types
            viz_types = defaultdict(int)
            for file_path in png_files:
                filename = os.path.basename(file_path)
                if "dashboard" in filename:
                    viz_types["dashboard"] += 1
                elif "case" in filename:
                    viz_types["case_analysis"] += 1
                elif "behavior" in filename:
                    viz_types["behavioral"] += 1
                elif "neural" in filename:
                    viz_types["neural"] += 1
                else:
                    viz_types["other"] += 1
            
            viz_analysis["visualization_types"] = dict(viz_types)
        else:
            viz_analysis["issues"].append("No visualization files found")
        
        return viz_analysis
    
    def generate_comprehensive_report(self) -> Dict[str, Any]:
        """
        Generate a comprehensive simulation effectiveness report.
        
        Returns:
            Complete assessment report
        """
        report = {
            "assessment_timestamp": datetime.now().isoformat(),
            "output_directory": self.output_dir,
            "data_quality": self.analyze_simulation_data_quality(),
            "behavioral_analysis": self.analyze_behavioral_patterns(),
            "case_effectiveness": self.analyze_case_effectiveness(),
            "visualization_analysis": self.analyze_visualization_outputs(),
            "overall_assessment": {},
            "recommendations": []
        }
        
        # Overall assessment
        total_issues = (
            len(report["data_quality"]["issues_found"]) +
            len(report["behavioral_analysis"]["issues"]) +
            len(report["case_effectiveness"]["issues"]) +
            len(report["visualization_analysis"]["issues"])
        )
        
        if total_issues == 0:
            report["overall_assessment"]["status"] = "EXCELLENT"
            report["overall_assessment"]["score"] = 100
        elif total_issues <= 3:
            report["overall_assessment"]["status"] = "GOOD"
            report["overall_assessment"]["score"] = 85
        elif total_issues <= 6:
            report["overall_assessment"]["status"] = "FAIR"
            report["overall_assessment"]["score"] = 70
        else:
            report["overall_assessment"]["status"] = "NEEDS_IMPROVEMENT"
            report["overall_assessment"]["score"] = 50
        
        report["overall_assessment"]["total_issues"] = total_issues
        
        # Generate recommendations
        if report["data_quality"]["issues_found"]:
            report["recommendations"].append("Fix data quality issues in simulation logging")
        
        if report["behavioral_analysis"]["issues"]:
            report["recommendations"].append("Improve behavioral state transitions")
        
        if report["case_effectiveness"]["issues"]:
            report["recommendations"].append("Enhance case-based reasoning dynamics")
        
        if report["visualization_analysis"]["issues"]:
            report["recommendations"].append("Ensure all visualization components are generating outputs")
        
        if not report["recommendations"]:
            report["recommendations"].append("Simulation is running effectively - continue monitoring")
        
        return report
    
    def save_assessment_report(self, report: Dict[str, Any], filename: str = None):
        """
        Save the assessment report to file.
        
        Args:
            report: Assessment report to save
            filename: Optional filename for the report
        """
        if filename is None:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            filename = f"simulation_assessment_{timestamp}.json"
        
        report_path = os.path.join(self.output_dir, filename)
        with open(report_path, 'w') as f:
            json.dump(report, f, indent=2, default=str)
        
        print(f"Assessment report saved to: {report_path}")
        return report_path


def run_comprehensive_assessment():
    """
    Run a comprehensive assessment of the current simulation outputs.
    
    Returns:
        Assessment report
    """
    analyzer = SimulationEffectivenessAnalyzer()
    report = analyzer.generate_comprehensive_report()
    
    # Print summary
    print("\n" + "="*60)
    print("CEREBRUM INSECT SIMULATION EFFECTIVENESS ASSESSMENT")
    print("="*60)
    print(f"Assessment Time: {report['assessment_timestamp']}")
    print(f"Output Directory: {report['output_directory']}")
    print(f"Overall Status: {report['overall_assessment']['status']}")
    print(f"Effectiveness Score: {report['overall_assessment']['score']}/100")
    print(f"Total Issues Found: {report['overall_assessment']['total_issues']}")
    
    print("\nKEY FINDINGS:")
    print("-" * 40)
    
    # Data Quality
    dq = report['data_quality']
    print(f"Data Quality: {len(dq['issues_found'])} issues")
    if dq['data_richness'].get('total_events'):
        print(f"  - Total Events: {dq['data_richness']['total_events']}")
    if dq['data_richness'].get('unique_insects'):
        print(f"  - Unique Insects: {dq['data_richness']['unique_insects']}")
    
    # Behavioral Analysis
    ba = report['behavioral_analysis']
    print(f"Behavioral Analysis: {len(ba['issues'])} issues")
    if ba['behavioral_diversity'].get('unique_states'):
        print(f"  - Unique States: {ba['behavioral_diversity']['unique_states']}")
    
    # Case Effectiveness
    ce = report['case_effectiveness']
    print(f"Case Effectiveness: {len(ce['issues'])} issues")
    if ce['case_usage'].get('unique_cases'):
        print(f"  - Unique Cases: {ce['case_usage']['unique_cases']}")
    
    # Visualizations
    va = report['visualization_analysis']
    print(f"Visualizations: {va['visualization_count']} files generated")
    if va['file_sizes'].get('total_size_mb'):
        print(f"  - Total Size: {va['file_sizes']['total_size_mb']:.2f} MB")
    
    print("\nRECOMMENDATIONS:")
    print("-" * 40)
    for i, rec in enumerate(report['recommendations'], 1):
        print(f"{i}. {rec}")
    
    print("\n" + "="*60)
    
    # Save report
    analyzer.save_assessment_report(report)
    
    return report


if __name__ == "__main__":
    run_comprehensive_assessment() 