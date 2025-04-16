"""
Tests for the composite linguistic structures defined in
 `beyond_cerebrum.src.formalisms.structures`.
"""

import pytest
from typing import List, Any

# Adjust the import path based on your project structure and how you run pytest
# This assumes you run pytest from the CEREBRUM project root
from beyond_cerebrum.src.formalisms.structures import (
    TreeNode, Tree, GraphNode, GraphEdge, Graph,
    FeatureStructure
)
from beyond_cerebrum.src.formalisms.types import (
    SyntacticLabel, SemanticConcept
)

# --- Test Fixtures --- 

@pytest.fixture
def simple_tree() -> Tree[str]:
    """Creates a simple tree: root(A) -> [child(B), child(C, leaf)]"""
    node_c = TreeNode('C')
    node_b = TreeNode('B')
    node_a = TreeNode('A', children=[node_b, node_c])
    return Tree(node_a)

@pytest.fixture
def simple_graph_undirected() -> Graph[str, str]:
    """Creates a simple undirected graph: A --edge1-- B, B --edge2-- C"""
    graph = Graph[str, str]()
    graph.add_node('A', 'Data A')
    graph.add_node('B', 'Data B')
    graph.add_node('C', 'Data C')
    graph.add_edge('A', 'B', 'edge1', directed=False)
    graph.add_edge('B', 'C', 'edge2', directed=False)
    return graph

@pytest.fixture
def simple_graph_directed() -> Graph[str, str]:
    """Creates a simple directed graph: A --edge1--> B <--edge2-- C"""
    graph = Graph[str, str]()
    graph.add_node('A', 'Data A')
    graph.add_node('B', 'Data B')
    graph.add_node('C', 'Data C')
    graph.add_edge('A', 'B', 'edge1', directed=True)
    graph.add_edge('C', 'B', 'edge2', directed=True)
    return graph

# --- TreeNode Tests --- 

def test_treenode_init():
    node = TreeNode('data')
    assert node.data == 'data'
    assert node.children == []

def test_treenode_init_with_children():
    child1 = TreeNode('child1')
    child2 = TreeNode('child2')
    parent = TreeNode('parent', children=[child1, child2])
    assert parent.data == 'parent'
    assert parent.children == [child1, child2]

def test_treenode_add_child():
    parent = TreeNode('parent')
    child = TreeNode('child')
    parent.add_child(child)
    assert parent.children == [child]

def test_treenode_repr():
    leaf_node = TreeNode(1)
    assert repr(leaf_node) == 'TreeNode(data=1, leaf)'
    parent_node = TreeNode(2, children=[leaf_node])
    assert repr(parent_node) == 'TreeNode(data=2, children=1)'

# --- Tree Tests --- 

def test_tree_init(simple_tree):
    assert simple_tree.root.data == 'A'
    assert len(simple_tree.root.children) == 2

def test_tree_init_invalid_root():
     with pytest.raises(TypeError):
         Tree("not a node") # type: ignore

def test_tree_repr(simple_tree):
    # Based on the updated TreeNode repr
    assert repr(simple_tree) == "Tree(root=TreeNode(data='A', children=2))"

def test_tree_traverse_depth_first(simple_tree):
    # Expected pre-order traversal: A, B, C
    expected_traversal = ['A', 'B', 'C']
    assert simple_tree.traverse_depth_first() == expected_traversal

def test_tree_traverse_depth_first_complex():
    n_f = TreeNode('F')
    n_g = TreeNode('G')
    n_d = TreeNode('D', children=[n_f, n_g])
    n_e = TreeNode('E')
    n_b = TreeNode('B', children=[n_d, n_e])
    n_c = TreeNode('C')
    n_a = TreeNode('A', children=[n_b, n_c])
    tree = Tree(n_a)
    # Expected: A, B, D, F, G, E, C
    expected = ['A', 'B', 'D', 'F', 'G', 'E', 'C']
    assert tree.traverse_depth_first() == expected

# --- GraphNode Tests --- 

def test_graphnode_init():
    node = GraphNode(id='node1', data=SemanticConcept('ConceptX'))
    assert node.id == 'node1'
    # Check runtime type (str) because SemanticConcept is a NewType
    assert isinstance(node.data, str) 
    # Can still check the value itself
    assert node.data == 'ConceptX'

def test_graphnode_repr():
    node = GraphNode(id=1, data={'value': 10})
    assert repr(node) == "GraphNode(id=1, data={'value': 10})"

# --- GraphEdge Tests --- 

def test_graphedge_init_undirected():
    edge = GraphEdge(u=1, v=2, data='relation', directed=False)
    assert edge.u == 1
    assert edge.v == 2
    assert edge.data == 'relation'
    assert not edge.directed

def test_graphedge_init_directed():
    edge = GraphEdge(u='A', v='B', data=1.0, directed=True)
    assert edge.u == 'A'
    assert edge.v == 'B'
    assert edge.data == 1.0
    assert edge.directed

def test_graphedge_repr():
    undirected = GraphEdge(1, 2, 'link')
    assert repr(undirected) == "GraphEdge(1--2, data='link')"
    directed = GraphEdge('X', 'Y', 'cause', directed=True)
    assert repr(directed) == "GraphEdge('X'->'Y', data='cause')"

# --- Graph Tests --- 

def test_graph_init():
    graph = Graph[int, str]()
    assert graph.nodes == {}
    assert graph.adj == {}

def test_graph_add_node():
    graph = Graph()
    graph.add_node('A', 'Data A')
    assert 'A' in graph.nodes
    assert isinstance(graph.nodes['A'], GraphNode)
    assert graph.nodes['A'].data == 'Data A'
    assert 'A' in graph.adj
    assert graph.adj['A'] == []

def test_graph_add_node_duplicate(capsys):
    graph = Graph()
    graph.add_node(1, 'First')
    graph.add_node(1, 'Second') # Attempt to add duplicate
    captured = capsys.readouterr()
    assert "Warning: Node 1 already exists" in captured.out
    assert graph.nodes[1].data == 'First' # Original data should remain

def test_graph_add_edge_undirected(simple_graph_undirected):
    graph = simple_graph_undirected
    # Check A's adjacency list
    assert len(graph.adj['A']) == 1
    edge_a = graph.adj['A'][0]
    assert edge_a.u == 'A' and edge_a.v == 'B' and edge_a.data == 'edge1' and not edge_a.directed
    # Check B's adjacency list
    assert len(graph.adj['B']) == 2
    edge_b1 = graph.adj['B'][0] # Should be edge to A
    edge_b2 = graph.adj['B'][1] # Should be edge to C
    assert (edge_b1.u == 'B' and edge_b1.v == 'A' and edge_b1.data == 'edge1')
    assert (edge_b2.u == 'B' and edge_b2.v == 'C' and edge_b2.data == 'edge2')
    # Check C's adjacency list
    assert len(graph.adj['C']) == 1
    edge_c = graph.adj['C'][0]
    assert edge_c.u == 'C' and edge_c.v == 'B' and edge_c.data == 'edge2'

def test_graph_add_edge_directed(simple_graph_directed):
    graph = simple_graph_directed
    # Check A's adjacency list (outgoing edge)
    assert len(graph.adj['A']) == 1
    edge_a = graph.adj['A'][0]
    assert edge_a.u == 'A' and edge_a.v == 'B' and edge_a.data == 'edge1' and edge_a.directed
    # Check B's adjacency list (no outgoing edges)
    assert len(graph.adj.get('B', [])) == 0 # B has incoming edges, but adj stores outgoing
    # Check C's adjacency list (outgoing edge)
    assert len(graph.adj['C']) == 1
    edge_c = graph.adj['C'][0]
    assert edge_c.u == 'C' and edge_c.v == 'B' and edge_c.data == 'edge2' and edge_c.directed

def test_graph_add_edge_invalid_node():
    graph = Graph()
    graph.add_node('A', 'Data A')
    with pytest.raises(ValueError):
        graph.add_edge('A', 'B', 'link') # Node B doesn't exist
    with pytest.raises(ValueError):
        graph.add_edge('X', 'A', 'link') # Node X doesn't exist

def test_graph_get_node(simple_graph_undirected):
    graph = simple_graph_undirected
    node_b = graph.get_node('B')
    assert node_b is not None
    assert node_b.id == 'B'
    assert node_b.data == 'Data B'
    assert graph.get_node('D') is None

def test_graph_get_neighbors_undirected(simple_graph_undirected):
    graph = simple_graph_undirected
    neighbors_a = graph.get_neighbors('A')
    assert len(neighbors_a) == 1
    assert neighbors_a[0].id == 'B'
    
    neighbors_b = graph.get_neighbors('B')
    neighbor_ids_b = {n.id for n in neighbors_b}
    assert neighbor_ids_b == {'A', 'C'}
    
    neighbors_c = graph.get_neighbors('C')
    assert len(neighbors_c) == 1
    assert neighbors_c[0].id == 'B'
    
    assert graph.get_neighbors('D') == []

def test_graph_get_neighbors_directed(simple_graph_directed):
    graph = simple_graph_directed
    # Neighbors of A (only B via outgoing edge)
    neighbors_a = graph.get_neighbors('A')
    assert len(neighbors_a) == 1
    assert neighbors_a[0].id == 'B'
    
    # Neighbors of B (A and C via incoming edges, but get_neighbors uses adj list)
    # Since adj list only stores outgoing, and B has none, it should be empty
    neighbors_b = graph.get_neighbors('B')
    assert neighbors_b == [] # Corrected: Only considers outgoing edges
    
    # Neighbors of C (only B via outgoing edge)
    neighbors_c = graph.get_neighbors('C')
    assert len(neighbors_c) == 1
    assert neighbors_c[0].id == 'B'

def test_graph_get_outgoing_edges(simple_graph_directed):
    graph = simple_graph_directed
    edges_a = graph.get_outgoing_edges('A')
    assert len(edges_a) == 1
    assert edges_a[0].v == 'B'
    
    edges_b = graph.get_outgoing_edges('B')
    assert edges_b == []
    
    edges_c = graph.get_outgoing_edges('C')
    assert len(edges_c) == 1
    assert edges_c[0].v == 'B'

def test_graph_repr_undirected(simple_graph_undirected):
    # 3 nodes, 2 unique edges (A-B, B-C)
    assert repr(simple_graph_undirected) == 'Graph(nodes=3, edges=2)'

def test_graph_repr_directed(simple_graph_directed):
    # 3 nodes, 2 directed edges (A->B, C->B)
    assert repr(simple_graph_directed) == 'Graph(nodes=3, edges=2)'

def test_graph_repr_empty():
    graph = Graph()
    assert repr(graph) == 'Graph(nodes=0, edges=0)' 

# --- FeatureStructure Tests ---

@pytest.fixture
def fs1() -> FeatureStructure[str, Any]:
    return FeatureStructure({'category': 'N', 'number': 'sg', 'agr': FeatureStructure({'person': 3})})

@pytest.fixture
def fs2() -> FeatureStructure[str, Any]:
    return FeatureStructure({'number': 'sg', 'case': 'nom', 'agr': FeatureStructure({'gender': 'm'})})

@pytest.fixture
def fs_conflict() -> FeatureStructure[str, Any]:
    return FeatureStructure({'number': 'pl'})

@pytest.fixture
def fs_nested_conflict() -> FeatureStructure[str, Any]:
    return FeatureStructure({'agr': FeatureStructure({'person': 1})})

@pytest.fixture
def fs_type_conflict() -> FeatureStructure[str, Any]:
     # 'agr' value is a dict, not FeatureStructure
    return FeatureStructure({'category': 'N', 'number': 'sg', 'agr': {'person': 3}}) 

def test_fs_init():
    fs = FeatureStructure({'a': 1, 'b': 'two'})
    assert fs['a'] == 1
    assert fs['b'] == 'two'
    assert isinstance(fs, dict)
    assert isinstance(fs, FeatureStructure)

def test_fs_repr():
    fs = FeatureStructure({'a': 1})
    assert repr(fs) == "FS({'a': 1})"
    nested_fs = FeatureStructure({'outer': fs})
    assert repr(nested_fs) == "FS({'outer': FS({'a': 1})})"

def test_fs_unify_simple_success(fs1):
    fs_other = FeatureStructure({'number': 'sg', 'new_feat': True})
    expected = FeatureStructure({'category': 'N', 'number': 'sg', 'new_feat': True, 'agr': FeatureStructure({'person': 3})})
    result = fs1.unify(fs_other)
    assert result == expected
    assert isinstance(result, FeatureStructure)
    # Ensure original is not modified
    assert 'new_feat' not in fs1

def test_fs_unify_nested_success(fs1, fs2):
    expected = FeatureStructure({
        'category': 'N', 
        'number': 'sg', 
        'case': 'nom', 
        'agr': FeatureStructure({'person': 3, 'gender': 'm'})
    })
    result = fs1.unify(fs2)
    assert result == expected
    assert isinstance(result, FeatureStructure)
    assert isinstance(result['agr'], FeatureStructure)
    
    result_comm = fs2.unify(fs1)
    assert result_comm == expected
    assert isinstance(result_comm, FeatureStructure)
    assert isinstance(result_comm['agr'], FeatureStructure)

def test_fs_unify_simple_conflict(fs1, fs_conflict):
    result = fs1.unify(fs_conflict)
    assert result is None

def test_fs_unify_nested_conflict(fs1, fs_nested_conflict):
    result = fs1.unify(fs_nested_conflict)
    assert result is None

def test_fs_unify_with_plain_dict_fails(fs1):
    # The FS.unify method expects another FS
    plain_dict = {'number': 'sg'}
    # FS.unify should return None if other is not FS
    assert fs1.unify(plain_dict) is None # type: ignore 

def test_fs_unify_type_conflict_value_vs_fs(fs1, fs_type_conflict):
    # fs1 has agr: FS({...}), fs_type_conflict has agr: dict{...}
    # The internal logic should handle dict vs FS unification
    result = fs1.unify(fs_type_conflict)
    # Expected behavior: unification fails because FeatureStructure is not equal to dict
    # Let's refine this expectation based on implementation: the current FeatureStructure.unify
    # wraps plain dicts before recursing, so it should unify if the contents allow.
    # Let's test the case where contents *don't* allow.
    fs_type_conflict_val = FeatureStructure({'agr': 1}) # agr is primitive vs FS
    result_val_conflict = fs1.unify(fs_type_conflict_val)
    assert result_val_conflict is None

    # Test case where dict and FS contents *can* unify
    fs_agr_dict = FeatureStructure({'agr': {'person': 3, 'gender': 'f'}})
    fs_agr_fs = FeatureStructure({'agr': FeatureStructure({'person': 3, 'number': 9})})
    # With stricter type checking, unifying a dict value with an FS value should fail.
    result = fs_agr_dict.unify(fs_agr_fs)
    assert result is None 