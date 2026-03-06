"""
Tests for src/visualization/insect/animation_creator.py
"""

import pytest
import numpy as np
import os
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

from src.visualization.insect.animation_creator import (
    InsectAnimationCreator,
    SwarmAnimationCreator
)
from src.core.model import Case
from src.models.insect.base import BehavioralState

@pytest.fixture
def insect_anim_creator():
    return InsectAnimationCreator(figsize=(6, 4), dpi=50)

@pytest.fixture
def swarm_anim_creator():
    return SwarmAnimationCreator(figsize=(6, 4), dpi=50)

class TestInsectAnimationCreator:
    def test_create_insect_trajectory_animation(self, insect_anim_creator):
        history = [
            {"position": np.array([0, 0, 0]), "case": Case.NOMINATIVE, "behavioral_state": BehavioralState.FORAGING},
            {"position": np.array([1, 1, 0]), "case": Case.ACCUSATIVE, "behavioral_state": BehavioralState.NAVIGATING}
        ]
        environment = {"food_sources": [{"position": [2, 2, 0]}]}
        
        anim = insect_anim_creator.create_insect_trajectory_animation(
            history, environment, duration=1.0, fps=2
        )
        assert anim is not None

    def test_create_case_transition_animation(self, insect_anim_creator):
        history = [
            {"case": Case.NOMINATIVE, "context": {"health": 1.0}},
            {"case": Case.ACCUSATIVE, "context": {"health": 0.8}}
        ]
        
        anim = insect_anim_creator.create_case_transition_animation(
            history, duration=1.0, fps=2
        )
        assert anim is not None

    def test_create_neural_activity_animation(self, insect_anim_creator):
        history = [
            {"activity": {"mushroom_body": np.array([0.5, 0.5]), "central_complex": np.array([0.1])}},
            {"activity": {"mushroom_body": np.array([0.8, 0.2]), "central_complex": np.array([0.5])}}
        ]
        
        anim = insect_anim_creator.create_neural_activity_animation(
            history, duration=1.0, fps=2
        )
        assert anim is not None


class TestSwarmAnimationCreator:
    def test_create_swarm_animation(self, swarm_anim_creator):
        history = [
            {
                "insect_positions": [np.array([0, 0, 0]), np.array([-1, 1, 0])],
                "insect_states": [BehavioralState.FORAGING, BehavioralState.NAVIGATING],
                "swarm_center": np.array([-0.5, 0.5, 0])
            },
            {
                "insect_positions": [np.array([1, 1, 0]), np.array([-2, 2, 0])],
                "insect_states": [BehavioralState.FORAGING, BehavioralState.NAVIGATING],
                "swarm_center": np.array([-0.5, 1.5, 0])
            }
        ]
        environment = {
            "food_sources": [{"position": [5, 5, 0], "amount": 10}],
            "pheromone_trails": [{"position": [1, 1, 0], "strength": 0.5}]
        }
        
        anim = swarm_anim_creator.create_swarm_animation(
            history, environment, duration=1.0, fps=2
        )
        assert anim is not None

    def test_create_collective_behavior_animation(self, swarm_anim_creator):
        history = [
            {"clustering": 0.5, "alignment": 0.2},
            {"clustering": 0.8, "alignment": 0.6}
        ]
        
        anim = swarm_anim_creator.create_collective_behavior_animation(
            history, duration=1.0, fps=2
        )
        assert anim is not None
