"""
Visualization utilities for FORMICA structures and processes.

Requires the 'graphviz' library:
  pip install graphviz

Also requires the Graphviz system package to be installed:
  (e.g., on Debian/Ubuntu: sudo apt-get install graphviz)
"""

import graphviz
import os
from typing import TypeVar, Generic, Any, Optional

# Import necessary structures (adjust path as needed)
from ..formalisms.structures import Tree, TreeNode, Graph, GraphNode, GraphEdge

# Generic type variables used in structures
NodeData = TypeVar('NodeData')
EdgeData = TypeVar('EdgeData')

DEFAULT_OUTPUT_DIR = "output/visualizations" # Relative to project root

# --- Tree Visualization --- 

def _add_tree_nodes_edges(dot: graphviz.Digraph, node: TreeNode[NodeData], node_id: str):
    """Recursively adds nodes and edges for a tree to the Digraph object."""
    dot.node(node_id, label=str(node.data))
    for i, child in enumerate(node.children):
        child_id = f"{node_id}_{i}"
        dot.edge(node_id, child_id)
        _add_tree_nodes_edges(dot, child, child_id)

def render_tree(tree: Tree[NodeData], 
                filename: str = "tree.gv", 
                output_dir: str = DEFAULT_OUTPUT_DIR,
                view: bool = False):
    """Renders a Tree object to a file using Graphviz.

    Args:
        tree: The Tree object to render.
        filename: The base name for the output file (e.g., 'syntax_tree'). 
                  A '.gv' extension will be added.
        output_dir: The directory to save the output files.
        view: If True, attempts to open the rendered output.
    """
    if not tree.root:
        print("Cannot render an empty tree.")
        return

    # Ensure the output directory exists
    os.makedirs(output_dir, exist_ok=True)
    filepath = os.path.join(output_dir, filename)
    
    dot = graphviz.Digraph(comment='FORMICA Tree')
    dot.attr(rankdir='TB') # Top-to-bottom layout

    _add_tree_nodes_edges(dot, tree.root, "root")

    try:
        dot.render(filepath, view=view, format='png') # Render to PNG
        print(f"Tree visualization saved to {filepath}.png")
    except graphviz.ExecutableNotFound:
        print("Graphviz executable not found. Please install Graphviz.")
        print(f"Graphviz source saved to {filepath}.gv")
        dot.save(filepath + ".gv")
    except Exception as e:
        print(f"Error during rendering: {e}")
        print(f"Graphviz source saved to {filepath}.gv")
        dot.save(filepath + ".gv")

# --- Graph Visualization --- 

def render_graph(graph: Graph[NodeData, EdgeData], 
                 filename: str = "graph.gv", 
                 output_dir: str = DEFAULT_OUTPUT_DIR, 
                 view: bool = False):
    """Renders a Graph object to a file using Graphviz.

    Args:
        graph: The Graph object to render.
        filename: The base name for the output file (e.g., 'semantic_graph'). 
                  A '.gv' extension will be added.
        output_dir: The directory to save the output files.
        view: If True, attempts to open the rendered output.
    """
    is_directed = False # Check if any edge is directed
    
    # Determine if it should be Digraph or Graph based on edges
    # (Simple check: if any edge is directed, use Digraph)
    for edges in graph.adj.values():
        for edge in edges:
            if edge.directed:
                is_directed = True
                break
        if is_directed:
            break
            
    if is_directed:
        dot = graphviz.Digraph(comment='FORMICA Directed Graph')
    else:
        dot = graphviz.Graph(comment='FORMICA Undirected Graph')

    # Add nodes
    for node_id, node_obj in graph.nodes.items():
        dot.node(str(node_id), label=f"{node_id}\n({str(node_obj.data)})" ) # Include data in label

    # Add edges
    seen_edges = set() # To avoid duplicates in undirected graphs
    for u_id, edges in graph.adj.items():
        for edge in edges:
            u_str, v_str = str(edge.u), str(edge.v)
            edge_key = tuple(sorted((u_str, v_str))) if not edge.directed else (u_str, v_str)
            
            if edge_key not in seen_edges:
                dot.edge(u_str, v_str, label=str(edge.data))
                seen_edges.add(edge_key)

    # Ensure the output directory exists
    os.makedirs(output_dir, exist_ok=True)
    filepath = os.path.join(output_dir, filename)

    try:
        dot.render(filepath, view=view, format='png') # Render to PNG
        print(f"Graph visualization saved to {filepath}.png")
    except graphviz.ExecutableNotFound:
        print("Graphviz executable not found. Please install Graphviz.")
        print(f"Graphviz source saved to {filepath}.gv")
        dot.save(filepath + ".gv")
    except Exception as e:
        print(f"Error during rendering: {e}")
        print(f"Graphviz source saved to {filepath}.gv")
        dot.save(filepath + ".gv")

# --- Animation/GIF Generation (Placeholder) ---

def render_process_gif(states: list[Any], # List of structures representing states
                       render_function: callable, # e.g., render_graph or render_tree
                       filename: str = "process.gif",
                       output_dir: str = DEFAULT_OUTPUT_DIR,
                       duration: float = 0.5): # Duration per frame in seconds
    """Renders a sequence of states (e.g., graphs or trees) as a GIF.
    
    Requires 'imageio' library: pip install imageio[ffmpeg]
    Requires the render_function to save intermediate PNG files.
    """
    # This is a conceptual placeholder. Implementation requires:
    # 1. A way to capture intermediate states of FORMICA operations.
    # 2. Generating individual frames (PNGs) for each state using render_tree/render_graph.
    # 3. Using imageio to combine frames into a GIF.
    
    print(f"Placeholder: GIF generation for {len(states)} states requested.")
    print(" - Requires 'imageio' library.")
    print(" - Requires intermediate state capture mechanism.")
    print(" - Requires modification of render functions for frame generation.")
    
    # Example conceptual steps (requires imageio):
    # import imageio
    # import tempfile
    # with tempfile.TemporaryDirectory() as tmpdir:
    #     image_files = []
    #     for i, state in enumerate(states):
    #         frame_filename = os.path.join(tmpdir, f"frame_{i:03d}")
    #         # Assuming render_function can save to a specific path without .gv extension
    #         render_function(state, filename=frame_filename, output_dir=tmpdir, view=False) 
    #         image_files.append(frame_filename + ".png")
    #     
    #     # Ensure output dir exists
    #     os.makedirs(output_dir, exist_ok=True)
    #     gif_path = os.path.join(output_dir, filename)
    #     
    #     with imageio.get_writer(gif_path, mode='I', duration=duration) as writer:
    #         for image_file in image_files:
    #             image = imageio.imread(image_file)
    #             writer.append_data(image)
    #     print(f"GIF saved to {gif_path}")

    raise NotImplementedError("GIF generation is not fully implemented yet.")

print("FORMICA visualization module initialized.") 