"""
Simulation Logger for Insect Models

This module provides comprehensive logging and tracking capabilities for insect simulations,
including case performance, behavioral patterns, and simulation statistics.
"""

import json
import csv
import logging
import os
import time
from typing import Dict, Any, List, Optional, Union
from dataclasses import dataclass, field, asdict
from collections import defaultdict, deque
import numpy as np
from enum import Enum

from src.core.model import Case
from src.models.insect.base import InsectModel, BehavioralState
from src.utils.path_utils import get_output_dir


class CaseEncoder(json.JSONEncoder):
    """Custom JSON encoder for Case enums."""
    def default(self, obj):
        if isinstance(obj, Enum):
            return obj.value
        if isinstance(obj, np.ndarray):
            return obj.tolist()
        if isinstance(obj, np.integer):
            return int(obj)
        if isinstance(obj, np.floating):
            return float(obj)
        return super().default(obj)

logger = logging.getLogger(__name__)


@dataclass
class SimulationEvent:
    """Represents a simulation event."""
    timestamp: float
    event_type: str
    insect_id: str
    case: Case
    behavioral_state: BehavioralState
    position: List[float]
    performance: Dict[str, Any]
    context: Dict[str, Any]
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class CasePerformanceRecord:
    """Record of case performance."""
    case: Case
    timestamp: float
    performance_metrics: Dict[str, float]
    context_factors: Dict[str, Any]
    success_rate: float
    energy_efficiency: float
    information_gain: float


class InsectSimulationLogger:
    """
    Comprehensive logger for insect simulation data.
    """
    
    def __init__(self, output_dir: Optional[str] = None, max_history: int = 10000):
        """
        Initialize the simulation logger.
        
        Args:
            output_dir: Output directory for log files
            max_history: Maximum number of events to keep in memory
        """
        self.output_dir = output_dir or get_output_dir()
        self.insect_log_dir = os.path.join(self.output_dir, "insect_simulation_logs")
        os.makedirs(self.insect_log_dir, exist_ok=True)
        
        self.max_history = max_history
        self.events = deque(maxlen=max_history)
        self.case_performance = defaultdict(list)
        self.behavioral_patterns = defaultdict(list)
        self.simulation_statistics = defaultdict(list)
        
        # Create log files
        self.event_log_file = os.path.join(self.insect_log_dir, "simulation_events.json")
        self.case_log_file = os.path.join(self.insect_log_dir, "case_performance.csv")
        self.behavior_log_file = os.path.join(self.insect_log_dir, "behavioral_patterns.csv")
        self.statistics_file = os.path.join(self.insect_log_dir, "simulation_statistics.json")
        
        logger.info(f"Initialized InsectSimulationLogger in {self.insect_log_dir}")
    
    def log_event(self, insect: InsectModel, event_type: str, 
                 context: Dict[str, Any], metadata: Optional[Dict[str, Any]] = None):
        """
        Log a simulation event.
        
        Args:
            insect: The insect model
            event_type: Type of event
            context: Context information
            metadata: Additional metadata
        """
        event = SimulationEvent(
            timestamp=time.time(),
            event_type=event_type,
            insect_id=getattr(insect, 'species', 'unknown'),
            case=insect.current_case,
            behavioral_state=insect.behavioral_state,
            position=getattr(insect, 'position', [0.0, 0.0, 0.0]).tolist() if hasattr(insect, 'position') else [0.0, 0.0, 0.0],
            performance=insect.get_performance_summary(),
            context=context,
            metadata=metadata or {}
        )
        
        self.events.append(event)
        
        # Log to file
        self._write_event_to_file(event)
        
        logger.debug(f"Logged event: {event_type} for {event.insect_id}")
    
    def log_case_performance(self, insect: InsectModel, context: Dict[str, Any],
                           success_rate: float, energy_efficiency: float, 
                           information_gain: float):
        """
        Log case performance metrics.
        
        Args:
            insect: The insect model
            context: Context information
            success_rate: Success rate of the case
            energy_efficiency: Energy efficiency metric
            information_gain: Information gain metric
        """
        record = CasePerformanceRecord(
            case=insect.current_case,
            timestamp=time.time(),
            performance_metrics=insect.get_performance_summary(),
            context_factors=context,
            success_rate=success_rate,
            energy_efficiency=energy_efficiency,
            information_gain=information_gain
        )
        
        self.case_performance[insect.current_case].append(record)
        
        # Log to CSV
        self._write_case_performance_to_csv(record)
        
        logger.debug(f"Logged case performance for {insect.current_case.value}")
    
    def log_behavioral_pattern(self, insect: InsectModel, behavior_type: str,
                             duration: float, success: bool, energy_cost: float):
        """
        Log behavioral pattern information.
        
        Args:
            insect: The insect model
            behavior_type: Type of behavior
            duration: Duration of behavior
            success: Whether behavior was successful
            energy_cost: Energy cost of behavior
        """
        pattern = {
            'timestamp': time.time(),
            'insect_id': getattr(insect, 'species', 'unknown'),
            'case': insect.current_case.value,
            'behavioral_state': insect.behavioral_state.value,
            'behavior_type': behavior_type,
            'duration': duration,
            'success': success,
            'energy_cost': energy_cost,
            'position': getattr(insect, 'position', [0.0, 0.0, 0.0]).tolist() if hasattr(insect, 'position') else [0.0, 0.0, 0.0]
        }
        
        self.behavioral_patterns[behavior_type].append(pattern)
        
        # Log to CSV
        self._write_behavioral_pattern_to_csv(pattern)
        
        logger.debug(f"Logged behavioral pattern: {behavior_type}")
    
    def log_simulation_statistics(self, statistics: Dict[str, Any]):
        """
        Log simulation-wide statistics.
        
        Args:
            statistics: Simulation statistics
        """
        stats_entry = {
            'timestamp': time.time(),
            'statistics': statistics
        }
        
        self.simulation_statistics['overall'].append(stats_entry)
        
        # Log to JSON
        self._write_statistics_to_file(stats_entry)
        
        logger.debug("Logged simulation statistics")
    
    def _write_event_to_file(self, event: SimulationEvent):
        """Write event to JSON file."""
        try:
            # Convert event to dict
            event_dict = asdict(event)
            event_dict['case'] = event_dict['case'].value
            event_dict['behavioral_state'] = event_dict['behavioral_state'].value
            
            # Append to file
            with open(self.event_log_file, 'a') as f:
                f.write(json.dumps(event_dict, cls=CaseEncoder) + '\n')
        except Exception as e:
            logger.error(f"Error writing event to file: {e}")
    
    def _write_case_performance_to_csv(self, record: CasePerformanceRecord):
        """Write case performance to CSV file."""
        try:
            # Prepare CSV row
            row = {
                'timestamp': record.timestamp,
                'case': record.case.value,
                'total_actions': record.performance_metrics.get('total_actions', 0),
                'case_transformations': record.performance_metrics.get('case_transformations', 0),
                'avg_sensory_time': record.performance_metrics.get('avg_sensory_processing_time', 0),
                'avg_action_time': record.performance_metrics.get('avg_action_selection_time', 0),
                'success_rate': record.success_rate,
                'energy_efficiency': record.energy_efficiency,
                'information_gain': record.information_gain,
                'context_factors': json.dumps(record.context_factors)
            }
            
            # Write to CSV
            file_exists = os.path.exists(self.case_log_file)
            with open(self.case_log_file, 'a', newline='') as f:
                writer = csv.DictWriter(f, fieldnames=row.keys())
                if not file_exists:
                    writer.writeheader()
                writer.writerow(row)
        except Exception as e:
            logger.error(f"Error writing case performance to CSV: {e}")
    
    def _write_behavioral_pattern_to_csv(self, pattern: Dict[str, Any]):
        """Write behavioral pattern to CSV file."""
        try:
            # Prepare CSV row
            row = {
                'timestamp': pattern['timestamp'],
                'insect_id': pattern['insect_id'],
                'case': pattern['case'],
                'behavioral_state': pattern['behavioral_state'],
                'behavior_type': pattern['behavior_type'],
                'duration': pattern['duration'],
                'success': pattern['success'],
                'energy_cost': pattern['energy_cost'],
                'position_x': pattern['position'][0] if len(pattern['position']) > 0 else 0,
                'position_y': pattern['position'][1] if len(pattern['position']) > 1 else 0,
                'position_z': pattern['position'][2] if len(pattern['position']) > 2 else 0
            }
            
            # Write to CSV
            file_exists = os.path.exists(self.behavior_log_file)
            with open(self.behavior_log_file, 'a', newline='') as f:
                writer = csv.DictWriter(f, fieldnames=row.keys())
                if not file_exists:
                    writer.writeheader()
                writer.writerow(row)
        except Exception as e:
            logger.error(f"Error writing behavioral pattern to CSV: {e}")
    
    def _write_statistics_to_file(self, stats_entry: Dict[str, Any]):
        """Write statistics to JSON file."""
        try:
            with open(self.statistics_file, 'a') as f:
                f.write(json.dumps(stats_entry) + '\n')
        except Exception as e:
            logger.error(f"Error writing statistics to file: {e}")
    
    def get_simulation_summary(self) -> Dict[str, Any]:
        """
        Get a summary of the simulation data.
        
        Returns:
            Dictionary containing simulation summary
        """
        summary = {
            'total_events': len(self.events),
            'case_performance': {},
            'behavioral_patterns': {},
            'simulation_statistics': {}
        }
        
        # Case performance summary
        for case, records in self.case_performance.items():
            if records:
                avg_success_rate = np.mean([r.success_rate for r in records])
                avg_energy_efficiency = np.mean([r.energy_efficiency for r in records])
                avg_information_gain = np.mean([r.information_gain for r in records])
                
                summary['case_performance'][case.value] = {
                    'total_records': len(records),
                    'avg_success_rate': avg_success_rate,
                    'avg_energy_efficiency': avg_energy_efficiency,
                    'avg_information_gain': avg_information_gain
                }
        
        # Behavioral patterns summary
        for behavior_type, patterns in self.behavioral_patterns.items():
            if patterns:
                success_count = sum(1 for p in patterns if p['success'])
                total_count = len(patterns)
                avg_duration = np.mean([p['duration'] for p in patterns])
                avg_energy_cost = np.mean([p['energy_cost'] for p in patterns])
                
                summary['behavioral_patterns'][behavior_type] = {
                    'total_occurrences': total_count,
                    'success_rate': success_count / total_count if total_count > 0 else 0,
                    'avg_duration': avg_duration,
                    'avg_energy_cost': avg_energy_cost
                }
        
        # Simulation statistics summary
        if self.simulation_statistics['overall']:
            latest_stats = self.simulation_statistics['overall'][-1]['statistics']
            summary['simulation_statistics'] = latest_stats
        
        return summary
    
    def export_data(self, format: str = 'json') -> str:
        """
        Export all logged data.
        
        Args:
            format: Export format ('json' or 'csv')
            
        Returns:
            Path to exported file
        """
        timestamp = time.strftime("%Y%m%d_%H%M%S")
        
        if format == 'json':
            export_file = os.path.join(self.insect_log_dir, f"simulation_export_{timestamp}.json")
            
            export_data = {
                'summary': self.get_simulation_summary(),
                'events': [asdict(event) for event in self.events],
                'case_performance': {
                    case.value: [asdict(record) for record in records]
                    for case, records in self.case_performance.items()
                },
                'behavioral_patterns': {
                    behavior_type: patterns
                    for behavior_type, patterns in self.behavioral_patterns.items()
                }
            }
            
            with open(export_file, 'w') as f:
                json.dump(export_data, f, indent=2, cls=CaseEncoder)
        
        elif format == 'csv':
            export_file = os.path.join(self.insect_log_dir, f"simulation_export_{timestamp}.csv")
            
            # Combine all data into a single CSV
            with open(export_file, 'w', newline='') as f:
                writer = csv.writer(f)
                
                # Write header
                writer.writerow(['timestamp', 'data_type', 'insect_id', 'case', 'behavioral_state', 
                               'event_type', 'performance_metrics', 'context', 'metadata'])
                
                # Write events
                for event in self.events:
                    writer.writerow([
                        event.timestamp,
                        'event',
                        event.insect_id,
                        event.case.value,
                        event.behavioral_state.value,
                        event.event_type,
                        json.dumps(event.performance),
                        json.dumps(event.context),
                        json.dumps(event.metadata)
                    ])
        
        logger.info(f"Exported simulation data to {export_file}")
        return export_file


class CasePerformanceLogger:
    """
    Specialized logger for case performance analysis.
    """
    
    def __init__(self, output_dir: Optional[str] = None):
        """
        Initialize the case performance logger.
        
        Args:
            output_dir: Output directory for log files
        """
        self.output_dir = output_dir or get_output_dir()
        self.case_log_dir = os.path.join(self.output_dir, "case_performance_logs")
        os.makedirs(self.case_log_dir, exist_ok=True)
        
        self.case_metrics = defaultdict(list)
        self.context_analysis = defaultdict(list)
        
        logger.info(f"Initialized CasePerformanceLogger in {self.case_log_dir}")
    
    def log_case_metrics(self, case: Case, metrics: Dict[str, float], 
                        context: Dict[str, Any], duration: float):
        """
        Log detailed case performance metrics.
        
        Args:
            case: The case being evaluated
            metrics: Performance metrics
            context: Context information
            duration: Duration of case usage
        """
        entry = {
            'timestamp': time.time(),
            'case': case.value,
            'metrics': metrics,
            'context': context,
            'duration': duration
        }
        
        self.case_metrics[case].append(entry)
        
        # Analyze context factors
        for factor, value in context.items():
            self.context_analysis[factor].append({
                'timestamp': time.time(),
                'case': case.value,
                'factor_value': value,
                'performance': np.mean([v for v in metrics.values() if isinstance(v, (int, float))]) if metrics else 0.0
            })
    
    def get_case_effectiveness_report(self) -> Dict[str, Any]:
        """
        Generate comprehensive case effectiveness report.
        
        Returns:
            Dictionary containing case effectiveness analysis
        """
        report = {
            'case_effectiveness': {},
            'context_analysis': {},
            'recommendations': {}
        }
        
        # Analyze each case
        for case, metrics_list in self.case_metrics.items():
            if metrics_list:
                # Calculate effectiveness metrics - filter numeric values only
                numeric_performances = []
                for entry in metrics_list:
                    if entry['metrics']:
                        # Extract only numeric values from metrics
                        numeric_values = []
                        for value in entry['metrics'].values():
                            if isinstance(value, (int, float)):
                                numeric_values.append(float(value))
                        if numeric_values:
                            numeric_performances.append(np.mean(numeric_values))
                
                if numeric_performances:
                    avg_performance = np.mean(numeric_performances)
                    avg_duration = np.mean([entry['duration'] for entry in metrics_list])
                    
                    # Calculate success rate (assuming higher performance = success)
                    success_threshold = avg_performance * 0.8
                    success_count = sum(1 for perf in numeric_performances if perf >= success_threshold)
                    success_rate = success_count / len(numeric_performances)
                    
                    report['case_effectiveness'][case.value] = {
                        'total_usage': len(metrics_list),
                        'avg_performance': avg_performance,
                        'avg_duration': avg_duration,
                        'success_rate': success_rate,
                        'performance_std': np.std(numeric_performances)
                    }
                else:
                    # No numeric performance data available
                    report['case_effectiveness'][case.value] = {
                        'total_usage': len(metrics_list),
                        'avg_performance': 0.0,
                        'avg_duration': np.mean([entry['duration'] for entry in metrics_list]),
                        'success_rate': 0.0,
                        'performance_std': 0.0
                    }
        
        # Analyze context factors
        for factor, analysis_list in self.context_analysis.items():
            if analysis_list:
                factor_values = [entry['factor_value'] for entry in analysis_list]
                performances = [entry['performance'] for entry in analysis_list]
                
                # Calculate correlation between factor and performance
                if len(factor_values) > 1:
                    # Ensure all values are numeric
                    numeric_values = []
                    numeric_performances = []
                    for i, value in enumerate(factor_values):
                        if isinstance(value, (int, float)) and isinstance(performances[i], (int, float)):
                            numeric_values.append(float(value))
                            numeric_performances.append(float(performances[i]))
                    
                    if len(numeric_values) > 1:
                        correlation = np.corrcoef(numeric_values, numeric_performances)[0, 1]
                    else:
                        correlation = 0.0
                else:
                    correlation = 0.0
                
                # Filter out non-numeric values for averaging
                numeric_values = [v for v in factor_values if isinstance(v, (int, float))]
                numeric_performances = [p for p in performances if isinstance(p, (int, float))]
                
                report['context_analysis'][factor] = {
                    'total_occurrences': len(analysis_list),
                    'avg_value': np.mean(numeric_values) if numeric_values else 0.0,
                    'avg_performance': np.mean(numeric_performances) if numeric_performances else 0.0,
                    'correlation_with_performance': correlation
                }
        
        # Generate recommendations
        if report['case_effectiveness']:
            best_case = max(report['case_effectiveness'].items(), 
                          key=lambda x: x[1]['avg_performance'])
            
            report['recommendations'] = {
                'best_performing_case': best_case[0],
                'best_performance_score': best_case[1]['avg_performance'],
                'most_stable_case': min(report['case_effectiveness'].items(), 
                                      key=lambda x: x[1]['performance_std'])[0]
            }
        
        return report
    
    def export_case_report(self, format: str = 'json') -> str:
        """
        Export case performance report.
        
        Args:
            format: Export format ('json' or 'csv')
            
        Returns:
            Path to exported file
        """
        timestamp = time.strftime("%Y%m%d_%H%M%S")
        report = self.get_case_effectiveness_report()
        
        if format == 'json':
            export_file = os.path.join(self.case_log_dir, f"case_report_{timestamp}.json")
            with open(export_file, 'w') as f:
                json.dump(report, f, indent=2)
        
        elif format == 'csv':
            export_file = os.path.join(self.case_log_dir, f"case_report_{timestamp}.csv")
            
            with open(export_file, 'w', newline='') as f:
                writer = csv.writer(f)
                
                # Write case effectiveness
                writer.writerow(['Case Effectiveness'])
                writer.writerow(['Case', 'Total Usage', 'Avg Performance', 'Avg Duration', 'Success Rate', 'Performance STD'])
                
                for case, metrics in report['case_effectiveness'].items():
                    writer.writerow([
                        case,
                        metrics['total_usage'],
                        metrics['avg_performance'],
                        metrics['avg_duration'],
                        metrics['success_rate'],
                        metrics['performance_std']
                    ])
                
                writer.writerow([])
                writer.writerow(['Context Analysis'])
                writer.writerow(['Factor', 'Total Occurrences', 'Avg Value', 'Avg Performance', 'Correlation'])
                
                for factor, analysis in report['context_analysis'].items():
                    writer.writerow([
                        factor,
                        analysis['total_occurrences'],
                        analysis['avg_value'],
                        analysis['avg_performance'],
                        analysis['correlation_with_performance']
                    ])
        
        logger.info(f"Exported case report to {export_file}")
        return export_file


class BehavioralLogger:
    """
    Specialized logger for behavioral pattern analysis.
    """
    
    def __init__(self, output_dir: Optional[str] = None):
        """
        Initialize the behavioral logger.
        
        Args:
            output_dir: Output directory for log files
        """
        self.output_dir = output_dir or get_output_dir()
        self.behavior_log_dir = os.path.join(self.output_dir, "behavioral_logs")
        os.makedirs(self.behavior_log_dir, exist_ok=True)
        
        self.behavior_sequences = defaultdict(list)
        self.behavior_transitions = defaultdict(list)
        self.behavior_contexts = defaultdict(list)
        
        logger.info(f"Initialized BehavioralLogger in {self.behavior_log_dir}")
    
    def log_behavior_sequence(self, insect: InsectModel, behavior_sequence: List[str],
                            context_sequence: List[Dict[str, Any]], duration: float):
        """
        Log a sequence of behaviors.
        
        Args:
            insect: The insect model
            behavior_sequence: Sequence of behaviors
            context_sequence: Corresponding context for each behavior
            duration: Total duration of sequence
        """
        entry = {
            'timestamp': time.time(),
            'insect_id': getattr(insect, 'species', 'unknown'),
            'case': insect.current_case.value,
            'behavior_sequence': behavior_sequence,
            'context_sequence': context_sequence,
            'duration': duration,
            'sequence_length': len(behavior_sequence)
        }
        
        sequence_key = '->'.join(behavior_sequence)
        self.behavior_sequences[sequence_key].append(entry)
        
        # Log transitions
        for i in range(len(behavior_sequence) - 1):
            transition = f"{behavior_sequence[i]}->{behavior_sequence[i+1]}"
            self.behavior_transitions[transition].append({
                'timestamp': time.time(),
                'insect_id': getattr(insect, 'species', 'unknown'),
                'case': insect.current_case.value,
                'context': context_sequence[i] if i < len(context_sequence) else {}
            })
    
    def log_behavior_context(self, behavior_type: str, context: Dict[str, Any],
                           success: bool, duration: float):
        """
        Log behavior in specific context.
        
        Args:
            behavior_type: Type of behavior
            context: Context information
            success: Whether behavior was successful
            duration: Duration of behavior
        """
        entry = {
            'timestamp': time.time(),
            'behavior_type': behavior_type,
            'context': context,
            'success': success,
            'duration': duration
        }
        
        self.behavior_contexts[behavior_type].append(entry)
    
    def get_behavioral_analysis(self) -> Dict[str, Any]:
        """
        Generate behavioral pattern analysis.
        
        Returns:
            Dictionary containing behavioral analysis
        """
        analysis = {
            'behavior_sequences': {},
            'behavior_transitions': {},
            'context_effectiveness': {},
            'temporal_patterns': {}
        }
        
        # Analyze behavior sequences
        for sequence, entries in self.behavior_sequences.items():
            if entries:
                avg_duration = np.mean([entry['duration'] for entry in entries])
                avg_length = np.mean([entry['sequence_length'] for entry in entries])
                
                analysis['behavior_sequences'][sequence] = {
                    'total_occurrences': len(entries),
                    'avg_duration': avg_duration,
                    'avg_length': avg_length,
                    'frequency': len(entries) / sum(len(seq_entries) for seq_entries in self.behavior_sequences.values())
                }
        
        # Analyze behavior transitions
        for transition, entries in self.behavior_transitions.items():
            if entries:
                case_distribution = defaultdict(int)
                for entry in entries:
                    case_distribution[entry['case']] += 1
                
                analysis['behavior_transitions'][transition] = {
                    'total_occurrences': len(entries),
                    'case_distribution': dict(case_distribution),
                    'most_common_case': max(case_distribution.items(), key=lambda x: x[1])[0] if case_distribution else None
                }
        
        # Analyze context effectiveness
        for behavior_type, entries in self.behavior_contexts.items():
            if entries:
                success_rate = np.mean([entry['success'] for entry in entries])
                avg_duration = np.mean([entry['duration'] for entry in entries])
                
                # Analyze context factors
                context_factors = defaultdict(list)
                for entry in entries:
                    for factor, value in entry['context'].items():
                        # Ensure value is numeric for analysis
                        try:
                            if isinstance(value, (int, float)):
                                context_factors[factor].append(float(value))
                            elif isinstance(value, (list, tuple)) and len(value) > 0:
                                context_factors[factor].append(float(value[0]))
                            else:
                                context_factors[factor].append(0.0)
                        except (ValueError, TypeError):
                            context_factors[factor].append(0.0)
                
                context_analysis = {}
                for factor, values in context_factors.items():
                    if len(values) > 1:
                        try:
                            # Ensure both arrays are numeric
                            values_array = np.array(values, dtype=float)
                            success_array = np.array([float(entry['success']) for entry in entries], dtype=float)
                            correlation = np.corrcoef(values_array, success_array)[0, 1]
                            if np.isnan(correlation):
                                correlation = 0.0
                        except (ValueError, TypeError, np.linalg.LinAlgError):
                            correlation = 0.0
                    else:
                        correlation = 0.0
                    
                    context_analysis[factor] = {
                        'avg_value': np.mean(values) if values else 0.0,
                        'correlation_with_success': correlation
                    }
                
                analysis['context_effectiveness'][behavior_type] = {
                    'total_occurrences': len(entries),
                    'success_rate': success_rate,
                    'avg_duration': avg_duration,
                    'context_analysis': context_analysis
                }
        
        return analysis
    
    def export_behavioral_report(self, format: str = 'json') -> str:
        """
        Export behavioral analysis report.
        
        Args:
            format: Export format ('json' or 'csv')
            
        Returns:
            Path to exported file
        """
        timestamp = time.strftime("%Y%m%d_%H%M%S")
        analysis = self.get_behavioral_analysis()
        
        if format == 'json':
            export_file = os.path.join(self.behavior_log_dir, f"behavioral_report_{timestamp}.json")
            with open(export_file, 'w') as f:
                json.dump(analysis, f, indent=2)
        
        elif format == 'csv':
            export_file = os.path.join(self.behavior_log_dir, f"behavioral_report_{timestamp}.csv")
            
            with open(export_file, 'w', newline='') as f:
                writer = csv.writer(f)
                
                # Write behavior sequences
                writer.writerow(['Behavior Sequences'])
                writer.writerow(['Sequence', 'Total Occurrences', 'Avg Duration', 'Avg Length', 'Frequency'])
                
                for sequence, metrics in analysis['behavior_sequences'].items():
                    writer.writerow([
                        sequence,
                        metrics['total_occurrences'],
                        metrics['avg_duration'],
                        metrics['avg_length'],
                        metrics['frequency']
                    ])
                
                writer.writerow([])
                writer.writerow(['Behavior Transitions'])
                writer.writerow(['Transition', 'Total Occurrences', 'Most Common Case'])
                
                for transition, metrics in analysis['behavior_transitions'].items():
                    writer.writerow([
                        transition,
                        metrics['total_occurrences'],
                        metrics['most_common_case'] or 'N/A'
                    ])
        
        logger.info(f"Exported behavioral report to {export_file}")
        return export_file 