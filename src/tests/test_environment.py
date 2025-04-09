import pytest
import numpy as np
from src.examples.environment import Environment, create_sample_environment, create_maze_environment

# Test fixtures can be added here later if needed, e.g.,
# @pytest.fixture
# def default_env():
#     return Environment()

def test_environment_initialization_defaults():
    """Test Environment initialization with default values."""
    env = Environment(width=10.0, height=15.0)
    assert env.width == 10.0
    assert env.height == 15.0
    assert isinstance(env.obstacles, list)
    assert len(env.obstacles) == 0
    assert env.obstacle_radius == 0.5
    np.testing.assert_array_equal(env.goal_position, np.array([8.0, 13.0])) # Default goal is width-2, height-2
    assert env.goal_radius == 1.0

def test_environment_initialization_custom():
    """Test Environment initialization with custom values."""
    obstacles = [[1.0, 1.0], [2.0, 2.0]]
    goal = [5.0, 5.0]
    env = Environment(width=10.0, height=10.0, obstacles=obstacles, goal_position=goal)
    assert env.width == 10.0
    assert env.height == 10.0
    assert env.obstacles == obstacles
    np.testing.assert_array_equal(env.goal_position, np.array(goal))

def test_is_in_bounds():
    """Test the is_in_bounds method."""
    env = Environment(width=10.0, height=10.0)
    assert env.is_in_bounds(np.array([5.0, 5.0])) == True
    assert env.is_in_bounds(np.array([0.0, 0.0])) == True
    assert env.is_in_bounds(np.array([10.0, 10.0])) == True
    assert env.is_in_bounds(np.array([-0.1, 5.0])) == False
    assert env.is_in_bounds(np.array([5.0, 10.1])) == False
    assert env.is_in_bounds(np.array([10.1, 10.1])) == False

def test_add_obstacle():
    """Test adding obstacles."""
    env = Environment()
    assert len(env.obstacles) == 0
    env.add_obstacle([3.0, 3.0])
    assert len(env.obstacles) == 1
    assert env.obstacles[0] == [3.0, 3.0]
    env.add_obstacle([4.0, 4.0])
    assert len(env.obstacles) == 2
    assert env.obstacles[1] == [4.0, 4.0]

def test_is_collision_boundary():
    """Test collision detection with boundaries."""
    env = Environment(width=10.0, height=10.0)
    agent_radius = 0.1
    assert env.is_collision(np.array([0.05, 5.0]), radius=agent_radius) == True  # Too close to left boundary
    assert env.is_collision(np.array([9.95, 5.0]), radius=agent_radius) == True  # Too close to right boundary
    assert env.is_collision(np.array([5.0, 0.05]), radius=agent_radius) == True  # Too close to bottom boundary
    assert env.is_collision(np.array([5.0, 9.95]), radius=agent_radius) == True  # Too close to top boundary
    assert env.is_collision(np.array([0.1, 0.1]), radius=agent_radius) == False # Just inside corner
    assert env.is_collision(np.array([5.0, 5.0]), radius=agent_radius) == False # Center

def test_is_collision_obstacle():
    """Test collision detection with obstacles."""
    obstacle = [5.0, 5.0]
    env = Environment(width=10.0, height=10.0, obstacles=[obstacle])
    env.obstacle_radius = 0.5
    agent_radius = 0.1

    # Collision (agent center within obstacle radius + agent radius)
    assert env.is_collision(np.array([5.0, 5.0]), radius=agent_radius) == True # Center of obstacle
    assert env.is_collision(np.array([5.0, 5.5]), radius=agent_radius) == True # Edge of combined radius
    assert env.is_collision(np.array([4.45, 5.0]), radius=agent_radius) == True # Edge of combined radius

    # No collision (agent center outside obstacle radius + agent radius)
    assert env.is_collision(np.array([5.0, 5.61]), radius=agent_radius) == False # Just outside
    assert env.is_collision(np.array([4.39, 5.0]), radius=agent_radius) == False # Just outside
    assert env.is_collision(np.array([1.0, 1.0]), radius=agent_radius) == False # Far away

def test_set_goal_position():
    """Test setting the goal position."""
    env = Environment()
    new_goal = [1.0, 2.0]
    env.set_goal_position(new_goal)
    np.testing.assert_array_equal(env.goal_position, np.array(new_goal))

def test_is_goal_reached():
    """Test goal checking logic."""
    goal = [10.0, 10.0]
    env = Environment(goal_position=goal)
    env.goal_radius = 1.0

    # Goal reached
    assert env.is_goal_reached(np.array([10.0, 10.0])) == True # Exact position
    assert env.is_goal_reached(np.array([10.0, 10.9])) == True # Within radius
    assert env.is_goal_reached(np.array([9.1, 10.0])) == True # Within radius

    # Goal not reached
    assert env.is_goal_reached(np.array([10.0, 11.1])) == False # Outside radius
    assert env.is_goal_reached(np.array([8.9, 10.0])) == False # Outside radius
    assert env.is_goal_reached(np.array([0.0, 0.0])) == False  # Far away

    # Test with custom threshold
    assert env.is_goal_reached(np.array([10.0, 11.1]), threshold=1.5) == True

def test_get_state_dict():
    """Test the get_state_dict method."""
    obstacles = [[1.0, 1.0]]
    goal = [5.0, 5.0]
    env = Environment(width=10.0, height=10.0, obstacles=obstacles, goal_position=goal)
    state = env.get_state_dict()

    assert isinstance(state, dict)
    assert state["width"] == 10.0
    assert state["height"] == 10.0
    assert state["obstacles"] == obstacles
    # Ensure it's a copy
    state["obstacles"].append([2.0, 2.0])
    assert env.obstacles == [[1.0, 1.0]]
    assert state["goal"] == goal # goal is stored as np.array, tolist() is called

# == Test Helper Functions ==

def test_create_sample_environment():
    """Test the create_sample_environment helper function."""
    env = create_sample_environment()
    assert isinstance(env, Environment)
    assert env.width == 20.0
    assert env.height == 20.0
    assert len(env.obstacles) == 5
    np.testing.assert_array_equal(env.goal_position, np.array([18.0, 18.0]))

def test_create_maze_environment():
    """Test the create_maze_environment helper function."""
    env = create_maze_environment()
    assert isinstance(env, Environment)
    assert env.width == 20.0
    assert env.height == 20.0
    # Check if number of obstacles matches the loops in the function
    # Horizontal: 7 + 7 + 7 = 21
    # Vertical: 3 + 3 + 3 + 3 = 12
    # Total = 33
    assert len(env.obstacles) == 33
    np.testing.assert_array_equal(env.goal_position, np.array([18.0, 18.0])) 