"""
Stigmergic Case Implementation

This module implements the [STIG] stigmergic case for indirect communication
through environmental modifications in insects, including trail marking,
nest building, and collective construction.
"""

from typing import Dict, Any, Optional, List, Tuple
import numpy as np
import logging
from dataclasses import dataclass, field
from enum import Enum

from src.core.model import Case

logger = logging.getLogger(__name__)


class StigmergicSignal(Enum):
    """Types of stigmergic signals in insect communication."""
    TRAIL_MARKER = "trail_marker"
    NEST_MARKER = "nest_marker"
    FOOD_MARKER = "food_marker"
    DANGER_MARKER = "danger_marker"
    TERRITORY_MARKER = "territory_marker"
    MATING_MARKER = "mating_marker"
    CONSTRUCTION_MARKER = "construction_marker"
    CLEANING_MARKER = "cleaning_marker"


@dataclass
class EnvironmentalModification:
    """Modification made to the environment."""
    signal_type: StigmergicSignal
    position: np.ndarray
    intensity: float
    duration: float
    decay_rate: float
    creator_id: str
    timestamp: float = field(default_factory=lambda: 0.0)


@dataclass
class StigmergicResponse:
    """Response to a stigmergic signal."""
    insect_id: str
    signal_type: StigmergicSignal
    response_type: str
    intensity: float
    confidence: float
    timestamp: float = field(default_factory=lambda: 0.0)


@dataclass
class CollectiveConstruction:
    """Collective construction project."""
    project_id: str
    project_type: str
    target_structure: Dict[str, Any]
    current_progress: float
    participants: List[str]
    completion_time: Optional[float] = None
    timestamp: float = field(default_factory=lambda: 0.0)


class StigmergicCase:
    """
    [STIG] Stigmergic Case for indirect communication through environmental modifications.
    
    This case specializes in modeling stigmergic communication, collective construction,
    and environmental modification behaviors in insects.
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """
        Initialize stigmergic case.
        
        Args:
            config: Configuration parameters
        """
        self.config = config or {}
        self._case = Case.NOMINATIVE  # Default case
        self.case_id = "STIG"
        
        # Signal parameters
        self.signal_templates = self._initialize_signal_templates()
        
        # Communication parameters
        self.signal_decay_rate = self.config.get('signal_decay_rate', 0.1)
        self.response_threshold = self.config.get('response_threshold', 0.3)
        self.collective_coordination = self.config.get('collective_coordination', 0.7)
        
        # Environmental modifications
        self.active_modifications: List[EnvironmentalModification] = []
        self.modification_history: List[EnvironmentalModification] = []
        
        # Collective projects
        self.active_projects: Dict[str, CollectiveConstruction] = {}
        self.completed_projects: List[CollectiveConstruction] = []
        
        # Response tracking
        self.response_history: List[StigmergicResponse] = []
        
        logger.info("Initialized StigmergicCase")
    
    def _initialize_signal_templates(self) -> Dict[StigmergicSignal, Dict[str, Any]]:
        """Initialize templates for different stigmergic signals."""
        return {
            StigmergicSignal.TRAIL_MARKER: {
                'base_intensity': 0.8,
                'decay_rate': 0.05,
                'duration': 3600.0,  # 1 hour
                'response_types': ['follow', 'reinforce', 'ignore'],
                'spatial_range': 0.5
            },
            StigmergicSignal.NEST_MARKER: {
                'base_intensity': 1.0,
                'decay_rate': 0.01,
                'duration': 86400.0,  # 24 hours
                'response_types': ['approach', 'build', 'maintain'],
                'spatial_range': 1.0
            },
            StigmergicSignal.FOOD_MARKER: {
                'base_intensity': 0.9,
                'decay_rate': 0.1,
                'duration': 1800.0,  # 30 minutes
                'response_types': ['approach', 'collect', 'recruit'],
                'spatial_range': 0.8
            },
            StigmergicSignal.DANGER_MARKER: {
                'base_intensity': 1.0,
                'decay_rate': 0.2,
                'duration': 900.0,  # 15 minutes
                'response_types': ['avoid', 'escape', 'alert'],
                'spatial_range': 2.0
            },
            StigmergicSignal.TERRITORY_MARKER: {
                'base_intensity': 0.7,
                'decay_rate': 0.02,
                'duration': 7200.0,  # 2 hours
                'response_types': ['respect', 'challenge', 'mark'],
                'spatial_range': 1.5
            },
            StigmergicSignal.MATING_MARKER: {
                'base_intensity': 0.6,
                'decay_rate': 0.15,
                'duration': 600.0,  # 10 minutes
                'response_types': ['approach', 'court', 'mate'],
                'spatial_range': 0.3
            },
            StigmergicSignal.CONSTRUCTION_MARKER: {
                'base_intensity': 0.8,
                'decay_rate': 0.03,
                'duration': 3600.0,  # 1 hour
                'response_types': ['build', 'reinforce', 'modify'],
                'spatial_range': 0.7
            },
            StigmergicSignal.CLEANING_MARKER: {
                'base_intensity': 0.5,
                'decay_rate': 0.08,
                'duration': 1200.0,  # 20 minutes
                'response_types': ['clean', 'maintain', 'organize'],
                'spatial_range': 0.4
            }
        }
        
        logger.info("Initialized StigmergicCase")
    
    @property
    def case(self) -> Case:
        """Get the current case."""
        return self._case
        
    @case.setter
    def case(self, value: Case):
        """Set the current case."""
        if not isinstance(value, Case):
            raise TypeError(f"Expected Case enum, got {type(value)}")
        self._case = value
    
    def create_signal(self, signal_type: StigmergicSignal, position: np.ndarray,
                     creator_id: str, intensity: Optional[float] = None) -> EnvironmentalModification:
        """
        Create a stigmergic signal in the environment.
        
        Args:
            signal_type: Type of signal to create
            position: Position where signal is placed
            creator_id: ID of the insect creating the signal
            intensity: Signal intensity (uses default if None)
            
        Returns:
            Environmental modification representing the signal
        """
        try:
            template = self.signal_templates.get(signal_type, {})
            
            # Use provided intensity or default from template
            signal_intensity = intensity if intensity is not None else template.get('base_intensity', 0.5)
            
            # Create environmental modification
            modification = EnvironmentalModification(
                signal_type=signal_type,
                position=position.copy(),
                intensity=signal_intensity,
                duration=template.get('duration', 3600.0),
                decay_rate=template.get('decay_rate', 0.1),
                creator_id=creator_id,
                timestamp=self._get_current_time()
            )
            
            # Add to active modifications
            self.active_modifications.append(modification)
            
            logger.debug(f"Created {signal_type.value} signal at position {position}")
            
            return modification
            
        except Exception as e:
            logger.error(f"Error creating signal: {e}")
            return EnvironmentalModification(
                signal_type=signal_type,
                position=position,
                intensity=0.0,
                duration=0.0,
                decay_rate=1.0,
                creator_id=creator_id,
                timestamp=self._get_current_time()
            )
    
    def detect_signals(self, position: np.ndarray, detection_range: float) -> List[EnvironmentalModification]:
        """
        Detect stigmergic signals within a range.
        
        Args:
            position: Position to detect from
            detection_range: Range of detection
            
        Returns:
            List of detected signals
        """
        try:
            detected_signals = []
            current_time = self._get_current_time()
            
            for modification in self.active_modifications:
                # Calculate distance to signal
                distance = np.linalg.norm(position - modification.position)
                
                # Check if within detection range
                if distance <= detection_range:
                    # Calculate current intensity (with decay)
                    time_elapsed = current_time - modification.timestamp
                    decayed_intensity = modification.intensity * np.exp(-modification.decay_rate * time_elapsed)
                    
                    # Check if signal is still active
                    if time_elapsed < modification.duration and decayed_intensity > 0.1:
                        # Create a copy with current intensity
                        detected_signal = EnvironmentalModification(
                            signal_type=modification.signal_type,
                            position=modification.position.copy(),
                            intensity=decayed_intensity,
                            duration=modification.duration,
                            decay_rate=modification.decay_rate,
                            creator_id=modification.creator_id,
                            timestamp=modification.timestamp
                        )
                        detected_signals.append(detected_signal)
            
            return detected_signals
            
        except Exception as e:
            logger.error(f"Error detecting signals: {e}")
            return []
    
    def respond_to_signal(self, insect_id: str, signal: EnvironmentalModification,
                         response_type: str) -> StigmergicResponse:
        """
        Generate a response to a stigmergic signal.
        
        Args:
            insect_id: ID of the responding insect
            signal: Signal to respond to
            response_type: Type of response
            
        Returns:
            Stigmergic response
        """
        try:
            template = self.signal_templates.get(signal.signal_type, {})
            valid_responses = template.get('response_types', [])
            
            # Validate response type
            if response_type not in valid_responses:
                response_type = 'ignore'
            
            # Calculate response intensity based on signal intensity
            response_intensity = signal.intensity * 0.8
            
            # Calculate confidence based on signal strength and response appropriateness
            confidence = min(signal.intensity, 1.0)
            
            # Create response
            response = StigmergicResponse(
                insect_id=insect_id,
                signal_type=signal.signal_type,
                response_type=response_type,
                intensity=response_intensity,
                confidence=confidence,
                timestamp=self._get_current_time()
            )
            
            # Record response
            self.response_history.append(response)
            
            logger.debug(f"Insect {insect_id} responded to {signal.signal_type.value} with {response_type}")
            
            return response
            
        except Exception as e:
            logger.error(f"Error responding to signal: {e}")
            return StigmergicResponse(
                insect_id=insect_id,
                signal_type=signal.signal_type,
                response_type='ignore',
                intensity=0.0,
                confidence=0.0,
                timestamp=self._get_current_time()
            )
    
    def start_collective_project(self, project_id: str, project_type: str,
                               target_structure: Dict[str, Any], 
                               initiator_id: str) -> CollectiveConstruction:
        """
        Start a collective construction project.
        
        Args:
            project_id: Unique identifier for the project
            project_type: Type of construction project
            target_structure: Target structure specifications
            initiator_id: ID of the insect initiating the project
            
        Returns:
            Collective construction project
        """
        try:
            project = CollectiveConstruction(
                project_id=project_id,
                project_type=project_type,
                target_structure=target_structure,
                current_progress=0.0,
                participants=[initiator_id],
                timestamp=self._get_current_time()
            )
            
            # Add to active projects
            self.active_projects[project_id] = project
            
            logger.info(f"Started collective project {project_id}: {project_type}")
            
            return project
            
        except Exception as e:
            logger.error(f"Error starting collective project: {e}")
            return CollectiveConstruction(
                project_id=project_id,
                project_type=project_type,
                target_structure={},
                current_progress=0.0,
                participants=[],
                timestamp=self._get_current_time()
            )
    
    def contribute_to_project(self, project_id: str, participant_id: str,
                            contribution: float) -> bool:
        """
        Contribute to a collective construction project.
        
        Args:
            project_id: ID of the project to contribute to
            participant_id: ID of the contributing insect
            contribution: Amount of contribution (0-1)
            
        Returns:
            True if contribution was successful
        """
        try:
            if project_id not in self.active_projects:
                return False
            
            project = self.active_projects[project_id]
            
            # Add participant if not already present
            if participant_id not in project.participants:
                project.participants.append(participant_id)
            
            # Update progress
            project.current_progress += contribution * self.collective_coordination
            
            # Check if project is complete
            if project.current_progress >= 1.0:
                project.current_progress = 1.0
                project.completion_time = self._get_current_time()
                
                # Move to completed projects
                self.completed_projects.append(project)
                del self.active_projects[project_id]
                
                logger.info(f"Completed collective project {project_id}")
            
            return True
            
        except Exception as e:
            logger.error(f"Error contributing to project: {e}")
            return False
    
    def get_nearby_projects(self, position: np.ndarray, search_range: float) -> List[CollectiveConstruction]:
        """
        Get collective projects within a range.
        
        Args:
            position: Position to search from
            search_range: Range to search within
            
        Returns:
            List of nearby projects
        """
        try:
            nearby_projects = []
            
            # Check active projects (assuming they have positions in target_structure)
            for project in self.active_projects.values():
                if 'position' in project.target_structure:
                    project_position = np.array(project.target_structure['position'])
                    distance = np.linalg.norm(position - project_position)
                    
                    if distance <= search_range:
                        nearby_projects.append(project)
            
            return nearby_projects
            
        except Exception as e:
            logger.error(f"Error getting nearby projects: {e}")
            return []
    
    def cleanup_expired_signals(self, current_time: float):
        """
        Remove expired signals from active modifications.
        
        Args:
            current_time: Current simulation time
        """
        expired_signals = []
        
        for modification in self.active_modifications:
            time_elapsed = current_time - modification.timestamp
            
            # Check if signal has expired
            if time_elapsed > modification.duration:
                expired_signals.append(modification)
            else:
                # Check if signal has decayed below threshold
                decayed_intensity = modification.intensity * np.exp(-modification.decay_rate * time_elapsed)
                if decayed_intensity < 0.1:
                    expired_signals.append(modification)
        
        # Remove expired signals
        for signal in expired_signals:
            self.active_modifications.remove(signal)
            self.modification_history.append(signal)
        
        if expired_signals:
            logger.debug(f"Cleaned up {len(expired_signals)} expired signals")
    
    def get_stigmergic_statistics(self) -> Dict[str, Any]:
        """
        Get statistics about stigmergic communication.
        
        Returns:
            Dictionary with stigmergic statistics
        """
        # Count signals by type
        signal_counts = {}
        for modification in self.modification_history:
            signal_type = modification.signal_type.value
            signal_counts[signal_type] = signal_counts.get(signal_type, 0) + 1
        
        # Count responses by type
        response_counts = {}
        for response in self.response_history:
            response_type = response.response_type
            response_counts[response_type] = response_counts.get(response_type, 0) + 1
        
        return {
            'active_signals': len(self.active_modifications), # Changed from active_signals to active_modifications
            'total_signals_created': len(self.modification_history),
            'signal_counts': signal_counts,
            'total_responses': len(self.response_history),
            'response_counts': response_counts,
            'active_projects': len(self.active_projects),
            'completed_projects': len(self.completed_projects),
            'average_signal_intensity': np.mean([m.intensity for m in self.modification_history]) if self.modification_history else 0.0,
            'average_response_confidence': np.mean([r.confidence for r in self.response_history]) if self.response_history else 0.0
        }
    
    def _get_current_time(self) -> float:
        """Get current simulation time."""
        import time
        return time.time() 