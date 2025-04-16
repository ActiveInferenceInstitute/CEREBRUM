"""
Implementation of composite linguistic structures for FORMICA.

This module provides concrete implementations for complex structures like 
trees and graphs used to represent syntactic, semantic, or other 
linguistic information, based on the types defined in `types.py`.
(Specification Section 2.2)
"""

from typing import TypeVar, Generic, List, Dict, Optional, Any
from .types import SyntacticLabel, SemanticConcept # Example imports, adjust as needed

# --- Type Variables --- 
NodeType = TypeVar('NodeType')
EdgeType = TypeVar('EdgeType')
NodeData = TypeVar('NodeData')
EdgeData = TypeVar('EdgeData')

# --- Tree Structure --- 

class TreeNode(Generic[NodeData]):
    """A node in a generic tree structure."""
    def __init__(self, data: NodeData, children: Optional[List['TreeNode[NodeData]']] = None):
        self.data = data
        self.children = children if children is not None else []

    def __repr__(self):
        # Indicate if node is a leaf or has children
        child_info = f", children={len(self.children)}" if self.children else ", leaf"
        return f"TreeNode(data={self.data!r}{child_info})"

    def add_child(self, child_node: 'TreeNode[NodeData]'):
        self.children.append(child_node)

# Example Usage: Syntactic Tree Node might use SyntacticLabel as NodeData
# syntax_node = TreeNode[SyntacticLabel](SyntacticLabel('VP'))

class Tree(Generic[NodeData]):
    """Represents a generic tree with a root node."""
    def __init__(self, root: TreeNode[NodeData]):
        if not isinstance(root, TreeNode):
             raise TypeError("Tree root must be a TreeNode instance.")
        self.root = root

    def __repr__(self):
        # Improved representation showing root node details
        return f"Tree(root={self.root!r})"

    def traverse_depth_first(self) -> List[NodeData]:
        """Performs a depth-first traversal (pre-order) and returns node data."""
        visited_data = []
        stack = [self.root]
        
        while stack:
            node = stack.pop()
            if node:
                visited_data.append(node.data)
                # Add children to stack in reverse order for correct pre-order visit
                stack.extend(reversed(node.children))
                
        return visited_data

# --- Graph Structure --- 

class GraphNode(Generic[NodeData]):
    """A node in a generic graph structure."""
    def __init__(self, id: Any, data: NodeData):
        self.id = id
        self.data = data

    def __repr__(self):
        return f"GraphNode(id={self.id!r}, data={self.data!r})"

class GraphEdge(Generic[EdgeData]):
    """An edge in a generic graph structure."""
    def __init__(self, u: Any, v: Any, data: EdgeData, directed: bool = False):
        self.u = u # Source node ID
        self.v = v # Target node ID
        self.data = data
        self.directed = directed

    def __repr__(self):
        arrow = "->" if self.directed else "--"
        return f"GraphEdge({self.u!r}{arrow}{self.v!r}, data={self.data!r})"

class Graph(Generic[NodeData, EdgeData]):
    """Represents a generic graph using adjacency list representation."""
    def __init__(self):
        self.nodes: Dict[Any, GraphNode[NodeData]] = {}
        # Changed adj list to store edges outgoing from the key node
        self.adj: Dict[Any, List[GraphEdge[EdgeData]]] = {} 

    def add_node(self, node_id: Any, data: NodeData):
        if node_id in self.nodes:
            # Update existing node data? Raise error? For now, warn and ignore.
            print(f"Warning: Node {node_id} already exists. Data not updated.")
            return # Don't overwrite existing node or adjacency list
        
        self.nodes[node_id] = GraphNode(node_id, data)
        self.adj[node_id] = [] # Initialize adjacency list

    def add_edge(self, u_id: Any, v_id: Any, data: EdgeData, directed: bool = False):
        if u_id not in self.nodes:
            raise ValueError(f"Source node {u_id} not in graph")
        if v_id not in self.nodes:
             raise ValueError(f"Target node {v_id} not in graph")

        edge = GraphEdge(u_id, v_id, data, directed)
        # Ensure the adjacency list exists before appending
        if u_id not in self.adj: self.adj[u_id] = []
        self.adj[u_id].append(edge)

        if not directed:
            # Add edge in the other direction for undirected graphs, avoiding duplicates
            # Check if reverse edge already exists implicitly via the forward edge
            reverse_edge = GraphEdge(v_id, u_id, data, directed=False)
            # Ensure the adjacency list exists for the target node
            if v_id not in self.adj: self.adj[v_id] = []
            # Avoid adding duplicate edges for undirected graphs
            if not any(e.u == v_id and e.v == u_id for e in self.adj[v_id]):
                 self.adj[v_id].append(reverse_edge)

    def get_node(self, node_id: Any) -> Optional[GraphNode[NodeData]]:
        return self.nodes.get(node_id)

    def get_neighbors(self, node_id: Any) -> List[GraphNode[NodeData]]:
        """Returns neighbor nodes (not edges) for a given node ID."""
        neighbor_nodes = []
        if node_id in self.adj:
            for edge in self.adj[node_id]:
                neighbor_id = edge.v if edge.u == node_id else edge.u
                if neighbor_id != node_id: # Avoid self-loops if represented symmetrically
                    neighbor_node = self.get_node(neighbor_id)
                    if neighbor_node: # Check if neighbor node exists
                        neighbor_nodes.append(neighbor_node)
        # Remove duplicates if graph is undirected and edges are stored symmetrically
        # Using dict keys for efficient uniqueness check based on node id
        return list({node.id: node for node in neighbor_nodes}.values())

    def get_outgoing_edges(self, node_id: Any) -> List[GraphEdge[EdgeData]]:
        """Returns the list of outgoing edges from a node."""
        return self.adj.get(node_id, [])

    def __repr__(self):
        # Calculate number of unique edges (especially for undirected graphs)
        edge_count = 0
        seen_edges = set()
        for u_id, edges in self.adj.items():
            for edge in edges:
                # For undirected, store edge as sorted tuple to count (u,v) and (v,u) once
                edge_key = tuple(sorted((edge.u, edge.v))) if not edge.directed else (edge.u, edge.v)
                if edge_key not in seen_edges:
                    edge_count += 1
                    seen_edges.add(edge_key)
                    
        return f"Graph(nodes={len(self.nodes)}, edges={edge_count})"

# Example Usage: Semantic Graph might use SemanticConcept as NodeData and Relation as EdgeData
# semantic_graph = Graph[SemanticConcept, str]()
# semantic_graph.add_node('concept1', SemanticConcept('Agent'))
# semantic_graph.add_node('concept2', SemanticConcept('Action'))
# semantic_graph.add_edge('concept1', 'concept2', 'performs', directed=True)

print("FORMICA composite structures module initialized.") 