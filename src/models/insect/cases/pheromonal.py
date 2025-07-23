"""
Pheromonal Case Implementation

This module implements the [PHE] pheromonal case for chemical communication
in insects, including pheromone detection, generation, and processing.
"""

from typing import Dict, Any, Optional, List, Tuple
import numpy as np
import logging
from dataclasses import dataclass, field
from enum import Enum

from src.core.model import Case

logger = logging.getLogger(__name__)


class PheromoneType(Enum):
    """Types of pheromones in insect communication."""
    TRAIL = "trail"
    ALARM = "alarm"
    SEX = "sex"
    QUEEN = "queen"
    AGGREGATION = "aggregation"
    TERRITORIAL = "territorial"
    BROOD = "brood"
    NESTMATE = "nestmate"
    RECRUITMENT = "recruitment"
    PRIMER = "primer"
    RELEASER = "releaser"
    FORAGING = "foraging"


@dataclass
class ChemicalSignal:
    """Structured chemical signal for pheromone communication."""
    pheromone_type: PheromoneType
    concentration: float
    volatility: float  # Rate of evaporation
    source_position: Optional[np.ndarray] = None
    timestamp: float = field(default_factory=lambda: 0.0)
    species_specificity: float = 1.0  # How specific to the species


@dataclass
class ProcessedSignal:
    """Processed pheromone signal with behavioral implications."""
    signal_strength: float
    behavioral_response: str
    confidence: float
    urgency: float
    direction: Optional[np.ndarray] = None


@dataclass
class PheromoneOutput:
    """Output pheromone signal for transmission."""
    pheromone_type: PheromoneType
    concentration: float
    target_position: Optional[np.ndarray] = None
    duration: float = 60.0  # Duration in seconds
    timestamp: float = field(default_factory=lambda: 0.0)


class PheromonalCase:
    """
    [PHE] Pheromonal Case for chemical communication.
    
    This case specializes in processing, generating, and responding to
    pheromonal signals in insect communication systems.
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """
        Initialize pheromonal case.
        
        Args:
            config: Configuration parameters
        """
        self.config = config or {}
        self._case = Case.NOMINATIVE  # Default case
        self.case_id = "PHE"
        self.case_name = "pheromonal"
        
        # Pheromone processing parameters
        self.volatility_rates = {
            PheromoneType.TRAIL: 0.1,      # Slow evaporation
            PheromoneType.ALARM: 0.8,      # Fast evaporation
            PheromoneType.SEX: 0.3,        # Medium evaporation
            PheromoneType.QUEEN: 0.05,     # Very slow evaporation
            PheromoneType.AGGREGATION: 0.4, # Medium evaporation
            PheromoneType.TERRITORIAL: 0.2, # Slow evaporation
            PheromoneType.BROOD: 0.1,      # Slow evaporation
            PheromoneType.NESTMATE: 0.15,  # Slow evaporation
            PheromoneType.RECRUITMENT: 0.6, # Medium-fast evaporation
            PheromoneType.PRIMER: 0.02,    # Very slow evaporation
            PheromoneType.RELEASER: 0.7,   # Fast evaporation
            PheromoneType.FORAGING: 0.25   # Medium-slow evaporation
        }
        
        self.concentration_thresholds = {
            PheromoneType.TRAIL: 0.1,
            PheromoneType.ALARM: 0.05,
            PheromoneType.SEX: 0.01,
            PheromoneType.QUEEN: 0.001,
            PheromoneType.AGGREGATION: 0.2,
            PheromoneType.TERRITORIAL: 0.15,
            PheromoneType.BROOD: 0.3,
            PheromoneType.NESTMATE: 0.2,
            PheromoneType.RECRUITMENT: 0.1,
            PheromoneType.PRIMER: 0.001,
            PheromoneType.RELEASER: 0.05,
            PheromoneType.FORAGING: 0.1
        }
        
        self.receptor_specificities = {
            PheromoneType.TRAIL: 0.9,
            PheromoneType.ALARM: 0.95,
            PheromoneType.SEX: 0.99,
            PheromoneType.QUEEN: 0.8,
            PheromoneType.AGGREGATION: 0.7,
            PheromoneType.TERRITORIAL: 0.85,
            PheromoneType.BROOD: 0.9,
            PheromoneType.NESTMATE: 0.95,
            PheromoneType.RECRUITMENT: 0.8,
            PheromoneType.PRIMER: 0.6,
            PheromoneType.RELEASER: 0.9,
            PheromoneType.FORAGING: 0.8
        }
        
        # Behavioral response mappings
        self.behavioral_responses = {
            PheromoneType.TRAIL: "follow_trail",
            PheromoneType.ALARM: "escape",
            PheromoneType.SEX: "approach_mate",
            PheromoneType.QUEEN: "approach_queen",
            PheromoneType.AGGREGATION: "aggregate",
            PheromoneType.TERRITORIAL: "avoid_territory",
            PheromoneType.BROOD: "care_for_brood",
            PheromoneType.NESTMATE: "accept_nestmate",
            PheromoneType.RECRUITMENT: "join_recruitment",
            PheromoneType.PRIMER: "developmental_change",
            PheromoneType.RELEASER: "immediate_behavior",
            PheromoneType.FORAGING: "enhance_foraging"
        }
        
        # Pheromone memory for learning
        self.pheromone_memory = {}
        self.learning_rate = self.config.get('learning_rate', 0.1)
        
        # Current pheromone environment
        self.active_pheromones = {}
        
        logger.info("Initialized PheromonalCase")
    
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
    
    def process_chemical_signal(self, signal: ChemicalSignal) -> ProcessedSignal:
        """
        Process chemical signals with case-specific dynamics.
        
        Args:
            signal: Chemical signal to process
            
        Returns:
            Processed signal with behavioral implications
        """
        try:
            # Get pheromone-specific parameters
            volatility = self.volatility_rates.get(signal.pheromone_type, 0.5)
            threshold = self.concentration_thresholds.get(signal.pheromone_type, 0.1)
            specificity = self.receptor_specificities.get(signal.pheromone_type, 0.8)
            
            # Apply volatility decay
            time_elapsed = signal.timestamp - self._get_current_time()
            decayed_concentration = signal.concentration * np.exp(-volatility * time_elapsed)
            
            # Check if signal is above threshold
            if decayed_concentration < threshold:
                return ProcessedSignal(
                    signal_strength=0.0,
                    behavioral_response="ignore",
                    confidence=0.0,
                    urgency=0.0
                )
            
            # Calculate signal strength based on concentration and specificity
            signal_strength = decayed_concentration * specificity * signal.species_specificity
            
            # Determine behavioral response
            behavioral_response = self.behavioral_responses.get(signal.pheromone_type, "investigate")
            
            # Calculate urgency based on pheromone type
            urgency = self._calculate_urgency(signal.pheromone_type, signal_strength)
            
            # Calculate confidence based on signal strength and specificity
            confidence = min(signal_strength, 1.0)
            
            # Determine direction if source position is available
            direction = None
            if signal.source_position is not None:
                direction = signal.source_position / np.linalg.norm(signal.source_position)
            
            # Update pheromone memory for learning
            self._update_pheromone_memory(signal, signal_strength)
            
            return ProcessedSignal(
                signal_strength=signal_strength,
                behavioral_response=behavioral_response,
                confidence=confidence,
                urgency=urgency,
                direction=direction
            )
            
        except Exception as e:
            logger.error(f"Error processing chemical signal: {e}")
            return ProcessedSignal(
                signal_strength=0.0,
                behavioral_response="ignore",
                confidence=0.0,
                urgency=0.0
            )
    
    def generate_pheromone(self, pheromone_type: str, concentration: float, 
                          target_position: Optional[np.ndarray] = None) -> PheromoneOutput:
        """
        Generate pheromone output with appropriate dynamics.
        
        Args:
            pheromone_type: Type of pheromone to generate
            concentration: Concentration of pheromone
            target_position: Target position for pheromone deposition
            
        Returns:
            Pheromone output signal
        """
        try:
            # Convert string to enum
            if isinstance(pheromone_type, str):
                pheromone_type = PheromoneType(pheromone_type)
            
            # Get pheromone-specific parameters
            volatility = self.volatility_rates.get(pheromone_type, 0.5)
            duration = self._calculate_duration(volatility)
            
            # Create pheromone output
            output = PheromoneOutput(
                pheromone_type=pheromone_type,
                concentration=concentration,
                target_position=target_position,
                duration=duration,
                timestamp=self._get_current_time()
            )
            
            # Add to active pheromones
            pheromone_id = f"{pheromone_type.value}_{self._get_current_time()}"
            self.active_pheromones[pheromone_id] = output
            
            logger.debug(f"Generated {pheromone_type.value} pheromone with concentration {concentration}")
            
            return output
            
        except Exception as e:
            logger.error(f"Error generating pheromone: {e}")
            return PheromoneOutput(
                pheromone_type=PheromoneType.TRAIL,
                concentration=0.0,
                timestamp=self._get_current_time()
            )
    
    def _calculate_urgency(self, pheromone_type: PheromoneType, signal_strength: float) -> float:
        """
        Calculate urgency of behavioral response.
        
        Args:
            pheromone_type: Type of pheromone
            signal_strength: Strength of the signal
            
        Returns:
            Urgency value between 0 and 1
        """
        # Base urgency by pheromone type
        base_urgency = {
            PheromoneType.ALARM: 0.9,
            PheromoneType.SEX: 0.7,
            PheromoneType.QUEEN: 0.6,
            PheromoneType.RECRUITMENT: 0.8,
            PheromoneType.RELEASER: 0.9,
            PheromoneType.TRAIL: 0.5,
            PheromoneType.AGGREGATION: 0.4,
            PheromoneType.TERRITORIAL: 0.6,
            PheromoneType.BROOD: 0.3,
            PheromoneType.NESTMATE: 0.5,
            PheromoneType.PRIMER: 0.1,
            PheromoneType.FORAGING: 0.4
        }
        
        urgency = base_urgency.get(pheromone_type, 0.5)
        
        # Scale by signal strength
        urgency *= signal_strength
        
        return min(urgency, 1.0)
    
    def _calculate_duration(self, volatility: float) -> float:
        """
        Calculate duration of pheromone signal based on volatility.
        
        Args:
            volatility: Volatility rate of the pheromone
            
        Returns:
            Duration in seconds
        """
        # Inverse relationship: higher volatility = shorter duration
        base_duration = 60.0  # Base duration of 60 seconds
        return base_duration * (1.0 - volatility)
    
    def _update_pheromone_memory(self, signal: ChemicalSignal, signal_strength: float):
        """
        Update pheromone memory for learning.
        
        Args:
            signal: Chemical signal
            signal_strength: Strength of the processed signal
        """
        pheromone_key = f"{signal.pheromone_type.value}_{signal.timestamp}"
        
        if pheromone_key in self.pheromone_memory:
            # Update existing memory
            memory = self.pheromone_memory[pheromone_key]
            memory['count'] += 1
            memory['avg_strength'] = (
                (memory['avg_strength'] * (memory['count'] - 1) + signal_strength) / 
                memory['count']
            )
        else:
            # Create new memory entry
            self.pheromone_memory[pheromone_key] = {
                'pheromone_type': signal.pheromone_type,
                'count': 1,
                'avg_strength': signal_strength,
                'first_encounter': signal.timestamp,
                'last_encounter': signal.timestamp
            }
    
    def _get_current_time(self) -> float:
        """Get current simulation time."""
        import time
        return time.time()
    
    def get_pheromone_environment(self) -> Dict[str, Any]:
        """
        Get current pheromone environment state.
        
        Returns:
            Dictionary describing current pheromone environment
        """
        return {
            'active_pheromones': len(self.active_pheromones),
            'pheromone_types': list(set(p.pheromone_type.value for p in self.active_pheromones.values())),
            'memory_entries': len(self.pheromone_memory),
            'total_concentration': sum(p.concentration for p in self.active_pheromones.values())
        }
    
    def cleanup_expired_pheromones(self, current_time: float):
        """
        Remove expired pheromones from active environment.
        
        Args:
            current_time: Current simulation time
        """
        expired_ids = []
        
        for pheromone_id, pheromone in self.active_pheromones.items():
            if current_time - pheromone.timestamp > pheromone.duration:
                expired_ids.append(pheromone_id)
        
        for pheromone_id in expired_ids:
            del self.active_pheromones[pheromone_id]
        
        if expired_ids:
            logger.debug(f"Cleaned up {len(expired_ids)} expired pheromones")
    
    def get_learning_summary(self) -> Dict[str, Any]:
        """
        Get summary of pheromone learning.
        
        Returns:
            Dictionary with learning statistics
        """
        if not self.pheromone_memory:
            return {'total_encounters': 0, 'pheromone_types': []}
        
        pheromone_types = list(set(m['pheromone_type'].value for m in self.pheromone_memory.values()))
        total_encounters = sum(m['count'] for m in self.pheromone_memory.values())
        
        return {
            'total_encounters': total_encounters,
            'pheromone_types': pheromone_types,
            'memory_entries': len(self.pheromone_memory),
            'avg_encounters_per_type': total_encounters / len(pheromone_types) if pheromone_types else 0
        }
    
    def detect_signals(self, position: np.ndarray, detection_radius: float) -> List[ChemicalSignal]:
        """
        Detect pheromone signals within a given radius.
        
        Args:
            position: Current position
            detection_radius: Detection radius
            
        Returns:
            List of detected chemical signals
        """
        detected_signals = []
        current_time = self._get_current_time()
        
        for pheromone_id, pheromone in self.active_pheromones.items():
            # Check if pheromone is still active
            if current_time - pheromone.timestamp > pheromone.duration:
                continue
            
            # Calculate distance to pheromone
            if pheromone.target_position is not None:
                distance = np.linalg.norm(position - pheromone.target_position)
                
                if distance <= detection_radius:
                    # Create chemical signal from pheromone
                    signal = ChemicalSignal(
                        pheromone_type=pheromone.pheromone_type,
                        concentration=pheromone.concentration,
                        volatility=self.volatility_rates.get(pheromone.pheromone_type, 0.5),
                        source_position=pheromone.target_position,
                        timestamp=pheromone.timestamp
                    )
                    detected_signals.append(signal)
        
        return detected_signals 