"""
LEXICON Animated Graph

Provides animation functions for LEXICON knowledge graphs.
"""

import logging
import shutil
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple, Union

# Configure matplotlib to use non-GUI backend for headless environments
import matplotlib
matplotlib.use('Agg')  # Use Anti-Grain Geometry backend (non-interactive)

logger = logging.getLogger(__name__)

def create_graph_animation(graph_data: Dict[str, Any], output_dir: Path) -> List[Path]:
    """
    Create animated visualizations of the knowledge graph.
    
    Args:
        graph_data: Graph data dictionary
        output_dir: Output directory for animations
        
    Returns:
        List of created animation file paths
    """
    # Check if visualization dependencies are available
    try:
        import matplotlib.pyplot as plt
        import networkx as nx
        import numpy as np
        import imageio
    except ImportError as e:
        logger.warning(f"Animation dependencies not available: {e}")
        return []
    
    # Create visualizations directory
    vis_dir = output_dir / "visualizations"
    vis_dir.mkdir(parents=True, exist_ok=True)
    
    created_files = []
    
    # Create graph construction animation
    construction_path = vis_dir / "graph_construction.gif"
    try:
        if _create_graph_construction_animation(graph_data, construction_path):
            created_files.append(construction_path)
    except Exception as e:
        logger.error(f"Failed to create graph construction animation: {e}")
    
    # Create case evolution animation
    case_path = vis_dir / "case_evolution.gif"
    try:
        if _create_case_evolution_animation(graph_data, case_path):
            created_files.append(case_path)
    except Exception as e:
        logger.error(f"Failed to create case evolution animation: {e}")
    
    # Create polarity animation
    polarity_path = vis_dir / "polarity_animation.gif"
    try:
        if _create_polarity_animation(graph_data, polarity_path):
            created_files.append(polarity_path)
    except Exception as e:
        logger.error(f"Failed to create polarity animation: {e}")
    
    if created_files:
        logger.info(f"Created {len(created_files)} animated visualizations")
    else:
        logger.warning("No animated visualizations were created")
    
    return created_files

def _create_graph_construction_animation(graph_data: Dict[str, Any], output_path: Path) -> bool:
    """
    Create an animation showing the construction of the graph.
    
    Args:
        graph_data: Graph data dictionary
        output_path: Output file path
        
    Returns:
        True if successful, False otherwise
    """
    try:
        import matplotlib.pyplot as plt
        import networkx as nx
        import numpy as np
        import imageio
        
        # Extract nodes and edges
        nodes = graph_data.get("nodes", [])
        edges = graph_data.get("edges", [])
        
        if not nodes:
            logger.warning("No nodes to animate")
            return False
        
        # Create frames directory
        frames_dir = output_path.parent / "frames"
        frames_dir.mkdir(exist_ok=True)
        
        # Create base graph
        G = nx.Graph()
        
        # Create frames showing nodes being added
        node_frames = []
        current_nodes = []
        
        # Add nodes incrementally (up to 20 frames)
        step_size = max(1, len(nodes) // 20)
        for i in range(0, len(nodes), step_size):
            # Add batch of nodes
            batch = nodes[i:i+step_size]
            current_nodes.extend(batch)
            
            # Create frame
            frame_path = frames_dir / f"nodes_{i:04d}.png"
            _create_graph_frame(current_nodes, [], frame_path)
            
            if frame_path.exists():
                node_frames.append(frame_path)
        
        # Add edges incrementally (up to 20 more frames)
        edge_frames = []
        current_edges = []
        
        step_size = max(1, len(edges) // 20)
        for i in range(0, len(edges), step_size):
            # Add batch of edges
            batch = edges[i:i+step_size]
            current_edges.extend(batch)
            
            # Create frame
            frame_path = frames_dir / f"edges_{i:04d}.png"
            _create_graph_frame(current_nodes, current_edges, frame_path)
            
            if frame_path.exists():
                edge_frames.append(frame_path)
        
        # Combine frames into GIF
        all_frames = node_frames + edge_frames
        if not all_frames:
            logger.warning("No frames were created for animation")
            return False
        
        # Create GIF with variable duration
        # First frames (nodes being added) are faster
        # Last frames (edges being added) are slower
        with imageio.get_writer(output_path, mode='I', duration=0.3) as writer:
            # Add node frames with faster speed
            for frame_path in node_frames:
                image = imageio.imread(frame_path)
                writer.append_data(image)
            
            # Add edge frames with normal speed
            for frame_path in edge_frames:
                image = imageio.imread(frame_path)
                writer.append_data(image)
                
            # Add final frame with longer duration
            if edge_frames:
                image = imageio.imread(edge_frames[-1])
                for _ in range(5):  # Show final frame longer
                    writer.append_data(image)
        
        # Clean up frames
        for frame in all_frames:
            if frame.exists():
                frame.unlink()
        
        if frames_dir.exists():
            frames_dir.rmdir()
        
        return True
    
    except Exception as e:
        logger.error(f"Failed to create graph construction animation: {e}")
        return False

def _create_graph_frame(nodes: List[Dict], edges: List[Dict], output_path: Path) -> None:
    """
    Create a single frame of the graph animation.
    
    Args:
        nodes: List of nodes to include
        edges: List of edges to include
        output_path: Output file path
    """
    try:
        import matplotlib.pyplot as plt
        import networkx as nx
        import numpy as np
        
        # Create graph
        G = nx.Graph()
        
        # Add nodes with attributes
        for node in nodes:
            node_id = node.get("id", "")
            if not node_id:
                continue
                
            G.add_node(
                node_id,
                label=node.get("label", ""),
                type=node.get("type", "unknown")
            )
        
        # Add edges with attributes
        for edge in edges:
            source = edge.get("source", "")
            target = edge.get("target", "")
            if not source or not target:
                continue
                
            # Only add edge if both nodes exist
            if source in G.nodes and target in G.nodes:
                G.add_edge(
                    source,
                    target,
                    type=edge.get("type", "unknown")
                )
        
        if not G.nodes:
            return
            
        # Define node colors based on type
        node_colors = []
        for node_id in G.nodes:
            node_type = G.nodes[node_id].get("type", "unknown")
            if node_type == "entity":
                node_colors.append("skyblue")
            elif node_type == "claim":
                node_colors.append("lightgreen")
            else:
                node_colors.append("gray")
        
        # Create visualization
        plt.figure(figsize=(10, 8))
        
        # Use spring layout for graph visualization
        # Use fixed seed for consistent layout across frames
        pos = nx.spring_layout(G, seed=42)
        
        # Draw nodes
        nx.draw_networkx_nodes(
            G, pos, 
            node_color=node_colors,
            node_size=100,
            alpha=0.8
        )
        
        # Draw edges
        nx.draw_networkx_edges(
            G, pos, 
            width=1.0,
            alpha=0.5
        )
        
        plt.title(f"Knowledge Graph Construction ({len(nodes)} nodes, {len(edges)} edges)")
        plt.axis("off")
        plt.tight_layout()
        
        # Save visualization
        plt.savefig(output_path, dpi=150)
        plt.close()
        
    except Exception as e:
        logger.error(f"Failed to create animation frame: {e}")

def _create_case_evolution_animation(graph_data: Dict[str, Any], output_path: Path) -> bool:
    """
    Create an animation showing the evolution of grammatical cases.
    
    Args:
        graph_data: Graph data dictionary
        output_path: Output file path
        
    Returns:
        True if successful, False otherwise
    """
    try:
        import matplotlib.pyplot as plt
        import numpy as np
        import imageio
        
        # Extract nodes with case information
        nodes = graph_data.get("nodes", [])
        
        # Extract case information
        cases = {}
        for node in nodes:
            case = node.get("case", "none")
            if not case:
                case = "none"
            cases[case] = cases.get(case, 0) + 1
        
        # Skip if no case data
        if not cases or all(case == "none" for case in cases):
            logger.warning("No case data to animate")
            return False
        
        # Create frames directory with proper cleanup
        frames_dir = output_path.parent / "case_frames"
        
        # Clean up existing frames directory if it exists
        if frames_dir.exists():
            try:
                shutil.rmtree(frames_dir)
            except Exception as e:
                logger.warning(f"Could not remove existing frames directory: {e}")
        
        # Create fresh frames directory
        try:
            frames_dir.mkdir(parents=True, exist_ok=True)
        except Exception as e:
            logger.error(f"Could not create frames directory: {e}")
            return False
        
        # Create frames showing cases being highlighted
        frames = []
        
        # Sort cases by frequency
        sorted_cases = sorted(cases.items(), key=lambda x: x[1], reverse=True)
        
        # Create a frame for each case
        for i, (case, count) in enumerate(sorted_cases):
            # Skip "none" case
            if case == "none":
                continue
                
            # Create frame highlighting this case
            frame_path = frames_dir / f"case_{i:04d}.png"
            try:
                _create_case_frame(sorted_cases, case, frame_path)
                
                if frame_path.exists():
                    frames.append(frame_path)
            except Exception as e:
                logger.warning(f"Failed to create case frame for {case}: {e}")
        
        # Create summary frame
        summary_path = frames_dir / "case_summary.png"
        try:
            _create_case_frame(sorted_cases, None, summary_path)
            
            if summary_path.exists():
                # Add summary frame at beginning and end
                frames = [summary_path] + frames + [summary_path]
        except Exception as e:
            logger.warning(f"Failed to create case summary frame: {e}")
        
        # Combine frames into GIF
        if not frames:
            logger.warning("No frames were created for case animation")
            return False
        
        try:
            with imageio.get_writer(output_path, mode='I', duration=1.0) as writer:
                for frame_path in frames:
                    if frame_path.exists():
                        image = imageio.imread(frame_path)
                        writer.append_data(image)
            
            logger.info(f"Created case evolution animation with {len(frames)} frames")
            return True
            
        except Exception as e:
            logger.error(f"Failed to create GIF animation: {e}")
            return False
        
    except ImportError as e:
        logger.warning(f"Animation dependencies not available: {e}")
        return False
    except Exception as e:
        logger.error(f"Error creating case evolution animation: {e}")
        return False

def _create_case_frame(cases: List[Tuple[str, int]], highlight_case: Optional[str], output_path: Path) -> None:
    """
    Create a single frame of the case animation.
    
    Args:
        cases: List of (case, count) tuples
        highlight_case: Case to highlight, or None for summary
        output_path: Output file path
    """
    try:
        import matplotlib.pyplot as plt
        import numpy as np
        
        # Extract data
        labels = [case for case, _ in cases]
        values = [count for _, count in cases]
        
        # Create colors
        colors = []
        for case, _ in cases:
            if highlight_case is None:
                # Summary frame - all cases colored
                if case == "none":
                    colors.append("gray")
                else:
                    colors.append("skyblue")
            else:
                # Highlight frame - only highlighted case colored
                if case == highlight_case:
                    colors.append("red")
                elif case == "none":
                    colors.append("gray")
                else:
                    colors.append("skyblue")
        
        # Create visualization
        plt.figure(figsize=(10, 6))
        
        # Create bar chart
        plt.bar(labels, values, color=colors)
        
        # Add title
        if highlight_case:
            plt.title(f"Highlighting Case: {highlight_case}")
        else:
            plt.title("Grammatical Case Distribution")
        
        plt.xlabel("Case")
        plt.ylabel("Count")
        plt.xticks(rotation=45)
        plt.tight_layout()
        
        # Save visualization
        plt.savefig(output_path)
        plt.close()
        
    except Exception as e:
        logger.error(f"Failed to create case frame: {e}")

def _create_polarity_animation(graph_data: Dict[str, Any], output_path: Path) -> bool:
    """
    Create an animation showing the distribution of claim polarities.
    
    Args:
        graph_data: Graph data dictionary
        output_path: Output file path
        
    Returns:
        True if successful, False otherwise
    """
    try:
        import matplotlib.pyplot as plt
        import numpy as np
        import imageio
        
        # Extract claims and their polarities
        claims = [n for n in graph_data.get("nodes", []) if n.get("type") == "claim"]
        
        # Skip if no claims
        if not claims:
            logger.warning("No claims to animate")
            return False
        
        # Group claims by polarity
        polarities = {}
        for claim in claims:
            polarity = claim.get("polarity", "neutral")
            if polarity not in polarities:
                polarities[polarity] = []
            polarities[polarity].append(claim)
        
        # Skip if no polarity data
        if not polarities:
            logger.warning("No polarity data to animate")
            return False
        
        # Create frames directory with proper cleanup
        frames_dir = output_path.parent / "polarity_frames"
        
        # Clean up existing frames directory if it exists
        if frames_dir.exists():
            try:
                shutil.rmtree(frames_dir)
            except Exception as e:
                logger.warning(f"Could not remove existing polarity frames directory: {e}")
        
        # Create fresh frames directory
        try:
            frames_dir.mkdir(parents=True, exist_ok=True)
        except Exception as e:
            logger.error(f"Could not create polarity frames directory: {e}")
            return False
        
        # Create frames showing polarities being added
        frames = []
        
        # First, create summary frame
        summary_path = frames_dir / "polarity_summary.png"
        try:
            _create_polarity_frame(polarities, None, summary_path)
            
            if summary_path.exists():
                frames.append(summary_path)
        except Exception as e:
            logger.warning(f"Failed to create polarity summary frame: {e}")
        
        # Create a frame for each polarity
        for i, polarity in enumerate(polarities.keys()):
            # Create frame highlighting this polarity
            frame_path = frames_dir / f"polarity_{i:04d}.png"
            try:
                _create_polarity_frame(polarities, polarity, frame_path)
                
                if frame_path.exists():
                    frames.append(frame_path)
            except Exception as e:
                logger.warning(f"Failed to create polarity frame for {polarity}: {e}")
        
        # Add summary frame at end
        if summary_path.exists():
            frames.append(summary_path)
        
        # Combine frames into GIF
        if not frames:
            logger.warning("No frames were created for polarity animation")
            return False
        
        try:
            with imageio.get_writer(output_path, mode='I', duration=1.5) as writer:
                for frame_path in frames:
                    if frame_path.exists():
                        image = imageio.imread(frame_path)
                        writer.append_data(image)
            
            logger.info(f"Created polarity animation with {len(frames)} frames")
            return True
            
        except Exception as e:
            logger.error(f"Failed to create polarity GIF animation: {e}")
            return False
        
    except ImportError as e:
        logger.warning(f"Animation dependencies not available: {e}")
        return False
    except Exception as e:
        logger.error(f"Error creating polarity animation: {e}")
        return False

def _create_polarity_frame(polarities: Dict[str, List[Dict]], highlight_polarity: Optional[str], output_path: Path) -> None:
    """
    Create a single frame of the polarity animation.
    
    Args:
        polarities: Dictionary mapping polarity to claims
        highlight_polarity: Polarity to highlight, or None for summary
        output_path: Output file path
    """
    try:
        import matplotlib.pyplot as plt
        import numpy as np
        
        # Extract data
        labels = list(polarities.keys())
        values = [len(claims) for claims in polarities.values()]
        
        # Create colors
        colors = []
        for polarity in labels:
            if highlight_polarity is None:
                # Summary frame - standard colors
                if polarity == "positive":
                    colors.append("green")
                elif polarity == "negative":
                    colors.append("red")
                else:
                    colors.append("gray")
            else:
                # Highlight frame - only highlighted polarity fully colored
                if polarity == highlight_polarity:
                    if polarity == "positive":
                        colors.append("green")
                    elif polarity == "negative":
                        colors.append("red")
                    else:
                        colors.append("gray")
                else:
                    if polarity == "positive":
                        colors.append("lightgreen")
                    elif polarity == "negative":
                        colors.append("lightcoral")
                    else:
                        colors.append("lightgray")
        
        # Create visualization
        plt.figure(figsize=(10, 6))
        
        # Create bar chart
        plt.bar(labels, values, color=colors)
        
        # Add title
        if highlight_polarity:
            plt.title(f"Highlighting Polarity: {highlight_polarity}")
        else:
            plt.title("Claim Polarity Distribution")
        
        plt.xlabel("Polarity")
        plt.ylabel("Count")
        plt.tight_layout()
        
        # Save visualization
        plt.savefig(output_path)
        plt.close()
        
    except Exception as e:
        logger.error(f"Failed to create polarity frame: {e}") 