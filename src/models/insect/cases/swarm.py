"""
Swarm Case Implementation

This module implements the [SWARM] case for collective behavior and swarm intelligence
in insects, including emergent behaviors, collective decision making, and swarm dynamics.
"""

from typing import Dict, Any, Optional, List, Tuple, Set
import numpy as np
import logging
from dataclasses import dataclass, field
from enum import Enum
from collections import defaultdict

from src.core.model import Case

logger = logging.getLogger(__name__)


class SwarmBehavior(Enum):
    """Types of swarm behaviors in insect collectives."""
    AGGREGATION = "aggregation"
    DISPERSION = "dispersion"
    SYNCHRONIZATION = "synchronization"
    COLLECTIVE_DECISION = "collective_decision"
    TASK_ALLOCATION = "task_allocation"
    FORAGING_COORDINATION = "foraging_coordination"
    NEST_BUILDING = "nest_building"
    DEFENSE = "defense"
    MIGRATION = "migration"
    THERMOREGULATION = "thermoregulation"


@dataclass
class SwarmMember:
    """Represents a member of the swarm."""
    id: str
    position: np.ndarray
    velocity: np.ndarray
    state: Dict[str, Any]
    role: str
    influence_radius: float
    timestamp: float = field(default_factory=lambda: 0.0)


@dataclass
class SwarmState:
    """Current state of the swarm."""
    center_of_mass: np.ndarray
    dispersion: float
    cohesion: float
    alignment: float
    total_members: int
    active_behaviors: List[SwarmBehavior]
    timestamp: float = field(default_factory=lambda: 0.0)


@dataclass
class CollectiveDecision:
    """Result of collective decision making."""
    decision: str
    confidence: float
    consensus_level: float
    minority_opinion: Optional[str] = None
    decision_time: float = field(default_factory=lambda: 0.0)


@dataclass
class SwarmAction:
    """Action to be taken by the swarm."""
    action_type: SwarmBehavior
    parameters: Dict[str, Any]
    target_positions: Optional[List[np.ndarray]] = None
    priority: float = 1.0
    timestamp: float = field(default_factory=lambda: 0.0)


class SwarmCase:
    """
    [SWARM] Swarm Case for collective behavior and swarm intelligence.
    
    This case specializes in modeling emergent behaviors, collective decision making,
    and swarm dynamics in insect societies.
    """
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """
        Initialize swarm case.
        
        Args:
            config: Configuration parameters
        """
        self.config = config or {}
        self._case = Case.NOMINATIVE  # Default case
        self.case_id = "SWARM"
        self.case_name = "swarm"
        
        # Swarm dynamics parameters
        self.cohesion_strength = self.config.get('cohesion_strength', 0.5)
        self.alignment_strength = self.config.get('alignment_strength', 0.3)
        self.separation_strength = self.config.get('separation_strength', 0.8)
        self.influence_decay = self.config.get('influence_decay', 0.1)
        
        # Decision making parameters
        self.consensus_threshold = self.config.get('consensus_threshold', 0.7)
        self.decision_timeout = self.config.get('decision_timeout', 60.0)
        self.minority_threshold = self.config.get('minority_threshold', 0.3)
        
        # Task allocation parameters
        self.task_specialization = self.config.get('task_specialization', 0.8)
        self.role_switching_rate = self.config.get('role_switching_rate', 0.1)
        
        # Swarm state
        self.members: Dict[str, SwarmMember] = {}
        self.swarm_state = SwarmState(
            center_of_mass=np.zeros(3),
            dispersion=0.0,
            cohesion=0.0,
            alignment=0.0,
            total_members=0,
            active_behaviors=[]
        )
        
        # Collective memory
        self.collective_memory = {
            'decisions': [],
            'successful_behaviors': defaultdict(int),
            'failed_behaviors': defaultdict(int),
            'environmental_changes': []
        }
        
        # Current collective decisions
        self.active_decisions = {}
        
        logger.info("Initialized SwarmCase")
    
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
    
    def add_member(self, member: SwarmMember):
        """
        Add a member to the swarm.
        
        Args:
            member: Swarm member to add
        """
        self.members[member.id] = member
        self._update_swarm_state()
        
        logger.debug(f"Added member {member.id} to swarm")
    
    def remove_member(self, member_id: str):
        """
        Remove a member from the swarm.
        
        Args:
            member_id: ID of member to remove
        """
        if member_id in self.members:
            del self.members[member_id]
            self._update_swarm_state()
            
            logger.debug(f"Removed member {member_id} from swarm")
    
    def update_member_state(self, member_id: str, new_state: Dict[str, Any]):
        """
        Update the state of a swarm member.
        
        Args:
            member_id: ID of the member
            new_state: New state information
        """
        if member_id in self.members:
            self.members[member_id].state.update(new_state)
            self.members[member_id].timestamp = self._get_current_time()
    
    def compute_swarm_dynamics(self) -> SwarmState:
        """
        Compute current swarm dynamics.
        
        Returns:
            Updated swarm state
        """
        if not self.members:
            return self.swarm_state
        
        # Compute center of mass
        positions = np.array([member.position for member in self.members.values()])
        self.swarm_state.center_of_mass = np.mean(positions, axis=0)
        
        # Compute dispersion
        distances_from_center = np.linalg.norm(positions - self.swarm_state.center_of_mass, axis=1)
        self.swarm_state.dispersion = np.mean(distances_from_center)
        
        # Compute cohesion (inverse of dispersion)
        self.swarm_state.cohesion = 1.0 / (1.0 + self.swarm_state.dispersion)
        
        # Compute alignment
        velocities = np.array([member.velocity for member in self.members.values()])
        if np.any(velocities):
            velocity_magnitudes = np.linalg.norm(velocities, axis=1)
            normalized_velocities = velocities / velocity_magnitudes[:, np.newaxis]
            mean_velocity = np.mean(normalized_velocities, axis=0)
            self.swarm_state.alignment = np.linalg.norm(mean_velocity)
        else:
            self.swarm_state.alignment = 0.0
        
        # Update member count
        self.swarm_state.total_members = len(self.members)
        
        # Update timestamp
        self.swarm_state.timestamp = self._get_current_time()
        
        return self.swarm_state
    
    def make_collective_decision(self, decision_id: str, options: List[str], 
                               member_preferences: Dict[str, str]) -> CollectiveDecision:
        """
        Make a collective decision based on member preferences.
        
        Args:
            decision_id: Unique identifier for the decision
            options: Available options to choose from
            member_preferences: Dictionary mapping member IDs to their preferences
            
        Returns:
            Collective decision result
        """
        try:
            # Count preferences
            preference_counts = defaultdict(int)
            for member_id, preference in member_preferences.items():
                if member_id in self.members and preference in options:
                    preference_counts[preference] += 1
            
            # Find the most preferred option
            if not preference_counts:
                return CollectiveDecision(
                    decision="no_decision",
                    confidence=0.0,
                    consensus_level=0.0
                )
            
            total_votes = sum(preference_counts.values())
            best_option = max(preference_counts, key=preference_counts.get)
            best_votes = preference_counts[best_option]
            
            # Calculate consensus level
            consensus_level = best_votes / total_votes
            
            # Check if consensus is reached
            if consensus_level >= self.consensus_threshold:
                decision = best_option
                confidence = consensus_level
            else:
                # No clear consensus, use weighted decision
                decision = best_option
                confidence = consensus_level
            
            # Check for minority opinion
            minority_opinion = None
            for option, votes in preference_counts.items():
                if option != best_option and votes / total_votes >= self.minority_threshold:
                    minority_opinion = option
                    break
            
            # Create decision result
            result = CollectiveDecision(
                decision=decision,
                confidence=confidence,
                consensus_level=consensus_level,
                minority_opinion=minority_opinion,
                decision_time=self._get_current_time()
            )
            
            # Store decision in collective memory
            self.collective_memory['decisions'].append({
                'decision_id': decision_id,
                'result': result,
                'timestamp': self._get_current_time()
            })
            
            # Store active decision
            self.active_decisions[decision_id] = result
            
            logger.info(f"Collective decision made: {decision} (confidence: {confidence:.2f})")
            
            return result
            
        except Exception as e:
            logger.error(f"Error making collective decision: {e}")
            return CollectiveDecision(
                decision="error",
                confidence=0.0,
                consensus_level=0.0
            )
    
    def allocate_tasks(self, available_tasks: List[str], member_capabilities: Dict[str, List[str]]) -> Dict[str, str]:
        """
        Allocate tasks among swarm members.
        
        Args:
            available_tasks: List of tasks to allocate
            member_capabilities: Dictionary mapping member IDs to their capabilities
            
        Returns:
            Dictionary mapping member IDs to assigned tasks
        """
        try:
            task_assignments = {}
            unassigned_tasks = available_tasks.copy()
            
            # First pass: assign tasks based on specialization
            for member_id, capabilities in member_capabilities.items():
                if member_id not in self.members:
                    continue
                
                # Find best matching task for this member
                best_task = None
                best_match_score = 0.0
                
                for task in unassigned_tasks:
                    # Calculate match score based on capabilities
                    match_score = self._calculate_task_match(task, capabilities)
                    
                    if match_score > best_match_score:
                        best_match_score = match_score
                        best_task = task
                
                # Assign task if good match found
                if best_task and best_match_score >= self.task_specialization:
                    task_assignments[member_id] = best_task
                    unassigned_tasks.remove(best_task)
                    
                    # Update member role
                    self.members[member_id].role = best_task
            
            # Second pass: assign remaining tasks randomly
            for member_id in self.members:
                if member_id not in task_assignments and unassigned_tasks:
                    task = unassigned_tasks.pop(0)
                    task_assignments[member_id] = task
                    self.members[member_id].role = task
            
            logger.info(f"Allocated {len(task_assignments)} tasks among swarm members")
            
            return task_assignments
            
        except Exception as e:
            logger.error(f"Error allocating tasks: {e}")
            return {}
    
    def coordinate_foraging(self, food_sources: List[np.ndarray], 
                          food_qualities: List[float]) -> Dict[str, np.ndarray]:
        """
        Coordinate foraging behavior among swarm members.
        
        Args:
            food_sources: List of food source positions
            food_qualities: List of food quality values
            
        Returns:
            Dictionary mapping member IDs to assigned food sources
        """
        try:
            foraging_assignments = {}
            
            if not food_sources or not self.members:
                return foraging_assignments
            
            # Calculate distances from each member to each food source
            member_positions = [member.position for member in self.members.values()]
            member_ids = list(self.members.keys())
            
            # Create distance matrix
            distances = np.zeros((len(member_ids), len(food_sources)))
            for i, member_pos in enumerate(member_positions):
                for j, food_pos in enumerate(food_sources):
                    distances[i, j] = np.linalg.norm(member_pos - food_pos)
            
            # Assign food sources based on distance and quality
            assigned_food = set()
            
            for member_idx, member_id in enumerate(member_ids):
                best_food_idx = None
                best_score = -float('inf')
                
                for food_idx, food_pos in enumerate(food_sources):
                    if food_idx in assigned_food:
                        continue
                    
                    # Calculate assignment score (inverse distance + quality)
                    distance = distances[member_idx, food_idx]
                    quality = food_qualities[food_idx]
                    score = quality / (1.0 + distance)
                    
                    if score > best_score:
                        best_score = score
                        best_food_idx = food_idx
                
                if best_food_idx is not None:
                    foraging_assignments[member_id] = food_sources[best_food_idx]
                    assigned_food.add(best_food_idx)
            
            logger.info(f"Coordinated foraging for {len(foraging_assignments)} members")
            
            return foraging_assignments
            
        except Exception as e:
            logger.error(f"Error coordinating foraging: {e}")
            return {}
    
    def synchronize_behavior(self, target_behavior: SwarmBehavior, 
                           synchronization_strength: float = 0.5) -> bool:
        """
        Synchronize behavior across the swarm.
        
        Args:
            target_behavior: Behavior to synchronize
            synchronization_strength: Strength of synchronization
            
        Returns:
            True if synchronization was successful
        """
        try:
            # Update active behaviors
            if target_behavior not in self.swarm_state.active_behaviors:
                self.swarm_state.active_behaviors.append(target_behavior)
            
            # Apply synchronization to all members
            for member in self.members.values():
                # Update member state to reflect synchronized behavior
                member.state['synchronized_behavior'] = target_behavior.value
                member.state['sync_strength'] = synchronization_strength
                
                # Apply behavior-specific updates
                if target_behavior == SwarmBehavior.AGGREGATION:
                    self._apply_aggregation_behavior(member, synchronization_strength)
                elif target_behavior == SwarmBehavior.DISPERSION:
                    self._apply_dispersion_behavior(member, synchronization_strength)
                elif target_behavior == SwarmBehavior.SYNCHRONIZATION:
                    self._apply_synchronization_behavior(member, synchronization_strength)
            
            logger.info(f"Synchronized {target_behavior.value} behavior across swarm")
            
            return True
            
        except Exception as e:
            logger.error(f"Error synchronizing behavior: {e}")
            return False
    
    def _apply_aggregation_behavior(self, member: SwarmMember, strength: float):
        """Apply aggregation behavior to a member."""
        # Move toward center of mass
        direction = self.swarm_state.center_of_mass - member.position
        if np.linalg.norm(direction) > 0:
            direction = direction / np.linalg.norm(direction)
            member.velocity += strength * direction * 0.1
    
    def _apply_dispersion_behavior(self, member: SwarmMember, strength: float):
        """Apply dispersion behavior to a member."""
        # Move away from center of mass
        direction = member.position - self.swarm_state.center_of_mass
        if np.linalg.norm(direction) > 0:
            direction = direction / np.linalg.norm(direction)
            member.velocity += strength * direction * 0.1
    
    def _apply_synchronization_behavior(self, member: SwarmMember, strength: float):
        """Apply synchronization behavior to a member."""
        # Align velocity with swarm average
        if self.swarm_state.alignment > 0:
            member.velocity = (1 - strength) * member.velocity + strength * self.swarm_state.alignment * 0.1
    
    def _calculate_task_match(self, task: str, capabilities: List[str]) -> float:
        """
        Calculate how well a member's capabilities match a task.
        
        Args:
            task: Task to match
            capabilities: Member's capabilities
            
        Returns:
            Match score between 0 and 1
        """
        # Simple matching based on capability overlap
        task_keywords = task.lower().split('_')
        capability_keywords = []
        for capability in capabilities:
            capability_keywords.extend(capability.lower().split('_'))
        
        matches = sum(1 for keyword in task_keywords if keyword in capability_keywords)
        return matches / len(task_keywords) if task_keywords else 0.0
    
    def _update_swarm_state(self):
        """Update swarm state after member changes."""
        self.compute_swarm_dynamics()
    
    def _get_current_time(self) -> float:
        """Get current simulation time."""
        import time
        return time.time()
    
    def get_swarm_statistics(self) -> Dict[str, Any]:
        """
        Get comprehensive swarm statistics.
        
        Returns:
            Dictionary with swarm statistics
        """
        return {
            'total_members': len(self.members),
            'center_of_mass': self.swarm_state.center_of_mass.tolist(),
            'dispersion': self.swarm_state.dispersion,
            'cohesion': self.swarm_state.cohesion,
            'alignment': self.swarm_state.alignment,
            'active_behaviors': [behavior.value for behavior in self.swarm_state.active_behaviors],
            'active_decisions': len(self.active_decisions),
            'collective_memory_size': len(self.collective_memory['decisions']),
            'successful_behaviors': dict(self.collective_memory['successful_behaviors']),
            'failed_behaviors': dict(self.collective_memory['failed_behaviors'])
        }
    
    def record_behavior_outcome(self, behavior: SwarmBehavior, success: bool):
        """
        Record the outcome of a swarm behavior.
        
        Args:
            behavior: The behavior that was performed
            success: Whether the behavior was successful
        """
        if success:
            self.collective_memory['successful_behaviors'][behavior.value] += 1
        else:
            self.collective_memory['failed_behaviors'][behavior.value] += 1
        
        logger.debug(f"Recorded {behavior.value} outcome: {'success' if success else 'failure'}")
    
    def get_behavior_success_rate(self, behavior: SwarmBehavior) -> float:
        """
        Get success rate for a specific behavior.
        
        Args:
            behavior: Behavior to check
            
        Returns:
            Success rate between 0 and 1
        """
        successes = self.collective_memory['successful_behaviors'][behavior.value]
        failures = self.collective_memory['failed_behaviors'][behavior.value]
        total = successes + failures
        
        return successes / total if total > 0 else 0.0 