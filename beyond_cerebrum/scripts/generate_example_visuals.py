"""
Example script to generate visualizations of FORMICA structures.

This script demonstrates how to use the visualization functions from
beyond_cerebrum.src.visualization.visualize to render Tree and Graph objects.

Ensure graphviz is installed (Python library and system package).
Run from the project root directory (CEREBRUM).
"""

import os
import sys

# Adjust path to import from src (assuming script is run from project root)
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
src_path = os.path.join(project_root, 'beyond_cerebrum', 'src')
if src_path not in sys.path:
    sys.path.insert(0, src_path)

try:
    from formalisms.structures import Tree, TreeNode, Graph, GraphNode, GraphEdge
    from visualization.visualize import render_tree, render_graph, DEFAULT_OUTPUT_DIR
except ImportError as e:
    print(f"Error importing FORMICA modules: {e}")
    print("Please ensure you are running this script from the project root directory (CEREBRUM)")
    print(f"Current sys.path: {sys.path}")
    sys.exit(1)

def create_sample_tree() -> Tree[str]:
    """Creates a simple sample tree."""
    n_f = TreeNode('F')
    n_g = TreeNode('G')
    n_d = TreeNode('D', children=[n_f, n_g])
    n_e = TreeNode('E')
    n_b = TreeNode('B', children=[n_d, n_e])
    n_c = TreeNode('C')
    n_a = TreeNode('A', children=[n_b, n_c])
    return Tree(n_a)

def create_sample_graph() -> Graph[str, str]:
    """Creates a simple sample graph (mixed directed/undirected)."""
    graph = Graph[str, str]()
    graph.add_node('N1', 'Start Node')
    graph.add_node('N2', 'Middle Node')
    graph.add_node('N3', 'End Node')
    graph.add_node('N4', 'Isolated Node')
    
    graph.add_edge('N1', 'N2', 'leads to', directed=True)
    graph.add_edge('N2', 'N3', 'connects') # Undirected by default
    graph.add_edge('N1', 'N3', 'shortcut', directed=True)
    return graph

if __name__ == "__main__":
    print("Generating example FORMICA visualizations...")

    # Create output directory if it doesn't exist
    abs_output_dir = os.path.join(project_root, DEFAULT_OUTPUT_DIR)
    os.makedirs(abs_output_dir, exist_ok=True)
    print(f"Output directory: {abs_output_dir}")

    # --- Render Tree ---
    print("\nRendering sample tree...")
    sample_tree = create_sample_tree()
    tree_filename = "sample_tree"
    render_tree(sample_tree, filename=tree_filename, output_dir=abs_output_dir)
    
    # --- Render Graph ---
    print("\nRendering sample graph...")
    sample_graph = create_sample_graph()
    graph_filename = "sample_graph"
    render_graph(sample_graph, filename=graph_filename, output_dir=abs_output_dir)

    print("\nVisualization generation complete.")
    print(f"Check the '{abs_output_dir}' directory for the generated PNG files.") 