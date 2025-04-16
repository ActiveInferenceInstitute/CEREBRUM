"""Tests for FORMICA calculus operations."""

import pytest
from beyond_cerebrum.src.formalisms.structures import (
    FeatureStructure, Tree, TreeNode, Graph, GraphNode
)
from beyond_cerebrum.src.operations.calculus import (
    unify, project, apply, compose # compose will raise NotImplementedError
)
from beyond_cerebrum.src.formalisms.categories import Morphism # Abstract

# --- Fixtures ---

@pytest.fixture
def sample_fs1() -> FeatureStructure:
    return FeatureStructure({'cat': 'N', 'num': 'sg'})

@pytest.fixture
def sample_fs2() -> FeatureStructure:
    return FeatureStructure({'num': 'sg', 'case': 'acc'})

@pytest.fixture
def sample_tree() -> Tree:
    root = TreeNode[str](content="root_content")
    return Tree[str](root=root)

@pytest.fixture
def sample_graph() -> Graph:
    graph = Graph[str, str]()
    graph.add_node(GraphNode[str](id="n1", content="node1_content"))
    return graph

# --- Tests ---

def test_unify(sample_fs1, sample_fs2):
    """Test the unify operation."""
    unified = unify(sample_fs1, sample_fs2)
    assert unified is not None
    assert unified == {'cat': 'N', 'num': 'sg', 'case': 'acc'}

def test_project_feature_structure(sample_fs1):
    """Test project on FeatureStructure."""
    assert project(sample_fs1, 'cat') == 'N'
    assert project(sample_fs1, 'num') == 'sg'
    with pytest.raises(KeyError):
        project(sample_fs1, 'case')

def test_project_tree(sample_tree):
    """Test basic project on Tree."""
    assert project(sample_tree, 'root.content') == "root_content"
    with pytest.raises(NotImplementedError):
        project(sample_tree, 'root.children') # Path not supported by basic impl

def test_project_graph(sample_graph):
    """Test basic project on Graph."""
    assert project(sample_graph, 'nodes["n1"].content') == "node1_content"
    assert project(sample_graph, "nodes['n1'].content") == "node1_content" # Test single quotes
    with pytest.raises(KeyError): # Node exists, but wrong ID in path
         project(sample_graph, 'nodes["n2"].content')
    with pytest.raises(NotImplementedError): # Path format not supported
        project(sample_graph, 'nodes.n1.content')

def test_apply():
    """Test the apply operation with a simple function."""
    def simple_func(x: int) -> int:
        return x * 2
    result = apply(simple_func, 5)
    assert result == 10

def test_compose():
    """Test that compose raises NotImplementedError."""
    # Create dummy abstract Morphism instances (won't actually work)
    class DummyMorphism(Morphism):
        def source(self): return None
        def target(self): return None
        def compose(self, other): return None # Provide dummy method

    m1 = DummyMorphism()
    m2 = DummyMorphism()
    with pytest.raises(NotImplementedError, match="Category theory composition logic not implemented"): 
        compose(m1, m2) 