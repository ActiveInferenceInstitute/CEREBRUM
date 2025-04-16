"""Tests for FORMICA linguistic structures."""

import pytest
from beyond_cerebrum.src.formalisms.structures import (
    TreeNode,
    Tree,
    GraphNode,
    GraphEdge,
    Graph,
    FeatureStructure,
    SyntacticTree, 
    SemanticGraph,
    SyntacticTreeNode,
    SemanticGraphNode,
    SemanticGraphEdge
)
from beyond_cerebrum.src.formalisms.types import (
    SyntacticConstituent,
    SemanticConcept
)

# --- Tree Tests ---

def test_tree_creation():
    """Test basic Tree and TreeNode creation."""
    leaf1 = TreeNode[str](content="leaf1")
    leaf2 = TreeNode[str](content="leaf2")
    root = TreeNode[str](content="root", children=[leaf1, leaf2])
    tree = Tree[str](root=root)
    assert tree.root == root
    assert tree.root.children == [leaf1, leaf2]
    assert tree.validate() is True
    assert tree.get_complexity() == 3.0

def test_syntactic_tree():
    """Test specialized SyntacticTree."""
    np = SyntacticConstituent("NP")
    vp = SyntacticConstituent("VP")
    s = SyntacticConstituent("S")
    node_np = SyntacticTreeNode(content=np)
    node_vp = SyntacticTreeNode(content=vp)
    root_s = SyntacticTreeNode(content=s, children=[node_np, node_vp])
    syntactic_tree = SyntacticTree(root=root_s)
    assert syntactic_tree.validate() is True
    assert syntactic_tree.get_complexity() == 3.0
    assert isinstance(syntactic_tree.root.content, SyntacticConstituent)

# --- Graph Tests ---

def test_graph_creation():
    """Test basic Graph creation and operations."""
    graph = Graph[str, str]()
    node1 = GraphNode[str](id="n1", content="content1")
    node2 = GraphNode[str](id="n2", content="content2")
    graph.add_node(node1)
    graph.add_node(node2)
    
    edge1 = GraphEdge[str](source_id="n1", target_id="n2", content="connects")
    graph.add_edge(edge1)
    
    assert len(graph.nodes) == 2
    assert len(graph.edges) == 1
    assert graph.validate() is True
    assert graph.get_complexity() == 3.0 # 2 nodes + 1 edge
    assert graph.adjacency["n1"][0] == edge1
    assert graph.adjacency["n2"] == []

def test_graph_invalid_edge():
    """Test adding an edge with non-existent nodes."""
    graph = Graph[str, str]()
    node1 = GraphNode[str](id="n1", content="content1")
    graph.add_node(node1)
    edge_invalid = GraphEdge[str](source_id="n1", target_id="n_invalid", content="bad")
    with pytest.raises(ValueError):
        graph.add_edge(edge_invalid)
    assert graph.validate() is True # Still valid as the edge wasn't added

def test_semantic_graph():
    """Test specialized SemanticGraph."""
    concept1 = SemanticConcept("ACTION:eat")
    concept2 = SemanticConcept("PATIENT:apple")
    graph = SemanticGraph()
    node1 = SemanticGraphNode(id="c1", content=concept1)
    node2 = SemanticGraphNode(id="c2", content=concept2)
    graph.add_node(node1)
    graph.add_node(node2)
    edge1 = SemanticGraphEdge(source_id="c1", target_id="c2", content="has_patient")
    graph.add_edge(edge1)
    assert graph.validate() is True
    assert graph.get_complexity() == 3.0
    assert isinstance(graph.nodes["c1"].content, SemanticConcept)

# --- FeatureStructure Tests ---

def test_feature_structure_creation():
    """Test basic FeatureStructure creation."""
    fs = FeatureStructure({'category': 'N', 'number': 'sg'})
    assert fs['category'] == 'N'
    assert fs.get_complexity() == 2.0
    assert fs.validate() is True # Placeholder validation

def test_feature_structure_unify_simple():
    """Test simple unification success."""
    fs1 = FeatureStructure({'category': 'N', 'number': 'sg'})
    fs2 = FeatureStructure({'case': 'nom', 'number': 'sg'})
    unified = fs1.unify(fs2)
    assert unified is not None
    assert unified == {'category': 'N', 'number': 'sg', 'case': 'nom'}
    assert unified.get_complexity() == 3.0

def test_feature_structure_unify_clash():
    """Test unification failure due to value clash."""
    fs1 = FeatureStructure({'category': 'N', 'number': 'sg'})
    fs2 = FeatureStructure({'category': 'V', 'number': 'sg'})
    unified = fs1.unify(fs2)
    assert unified is None

def test_feature_structure_unify_nested():
    """Test unification with nested feature structures."""
    fs1 = FeatureStructure({'agreement': FeatureStructure({'number': 'pl'}), 'category': 'V'})
    fs2 = FeatureStructure({'agreement': FeatureStructure({'person': '3'}), 'tense': 'past'})
    unified = fs1.unify(fs2)
    assert unified is not None
    expected = {
        'agreement': {'number': 'pl', 'person': '3'},
        'category': 'V',
        'tense': 'past'
    }
    # Need to compare dicts directly as FeatureStructure equality might not be overloaded yet
    assert unified == expected
    # Test complexity calculation with nesting
    assert unified.get_complexity() == 5.0 # category, tense, agreement, number, person

def test_feature_structure_unify_nested_clash():
    """Test unification failure with nested clash."""
    fs1 = FeatureStructure({'agreement': FeatureStructure({'number': 'pl'})})
    fs2 = FeatureStructure({'agreement': FeatureStructure({'number': 'sg'})})
    unified = fs1.unify(fs2)
    assert unified is None 