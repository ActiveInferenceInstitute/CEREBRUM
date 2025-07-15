"""
LEXICON Graph Visualizer

Provides visualization functions for LEXICON knowledge graphs.
"""

import logging
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple, Union

# Configure matplotlib to use non-GUI backend for headless environments
import matplotlib
matplotlib.use('Agg')  # Use Anti-Grain Geometry backend (non-interactive)

logger = logging.getLogger(__name__)

class GraphVisualizer:
    """
    Creates visualizations of LEXICON knowledge graphs.
    """
    
    def __init__(self):
        """Initialize the graph visualizer."""
        self._check_visualization_dependencies()
    
    def _check_visualization_dependencies(self) -> bool:
        """
        Check if required visualization libraries are available.
        
        Returns:
            bool: True if all dependencies are available, False otherwise
        """
        try:
            import matplotlib
            import networkx
            import numpy
            return True
        except ImportError as e:
            logger.warning(f"Visualization dependencies missing: {e}")
            logger.warning("Graph visualization requires matplotlib, networkx, and numpy")
            return False
    
    def visualize_graph(self, graph_data: Dict[str, Any], output_dir: Path) -> List[Path]:
        """
        Create visualizations of the graph.
        
        Args:
            graph_data: Graph data containing nodes and edges
            output_dir: Output directory
            
        Returns:
            List of paths to generated visualization files
        """
        visualization_files = []
        
        try:
            # Ensure output_dir is a Path object
            if not isinstance(output_dir, Path):
                output_dir = Path(output_dir)
                
            # Ensure output directory exists
            output_dir.mkdir(parents=True, exist_ok=True)
            
            # Check if graph data is valid
            if not graph_data or not isinstance(graph_data, dict):
                logger.error("Invalid graph data - must be a dictionary")
                return []
                
            if "nodes" not in graph_data or not graph_data["nodes"]:
                logger.warning("No nodes found in graph data")
                return []
            
            # Create main graph visualization
            graph_vis_path = output_dir / "graph_visualization.png"
            self._create_graph_visualization(graph_data, graph_vis_path)
            if graph_vis_path.exists():
                visualization_files.append(graph_vis_path)
                logger.info(f"Created graph visualization: {graph_vis_path}")
            
            # Create case distribution chart
            case_dist_path = output_dir / "case_distribution.png"
            self._create_case_distribution(graph_data, case_dist_path)
            if case_dist_path.exists():
                visualization_files.append(case_dist_path)
                logger.info(f"Created case distribution chart: {case_dist_path}")
            
            # Create entity network visualization
            entity_network_path = output_dir / "entity_network.png"
            self._create_entity_network(graph_data, entity_network_path)
            if entity_network_path.exists():
                visualization_files.append(entity_network_path)
                logger.info(f"Created entity network visualization: {entity_network_path}")
            
            # Create polarity distribution visualization
            polarity_path = output_dir / "polarity_distribution.png"
            self._create_polarity_distribution(graph_data, polarity_path)
            if polarity_path.exists():
                visualization_files.append(polarity_path)
                logger.info(f"Created polarity distribution chart: {polarity_path}")
            
            # Create entity-claim relationship diagram
            relationships_path = output_dir / "entity_claim_relationships.png"
            self._create_entity_claim_relationships(graph_data, relationships_path)
            if relationships_path.exists():
                visualization_files.append(relationships_path)
                logger.info(f"Created entity-claim relationship diagram: {relationships_path}")
            
            if not visualization_files:
                logger.warning("No visualizations were created - check graph data and dependencies")
            else:
                logger.info(f"Created {len(visualization_files)} graph visualizations in {output_dir}")
            
            return visualization_files
            
        except ImportError as e:
            logger.warning(f"Could not create visualizations due to missing dependencies: {e}")
            logger.warning("Install matplotlib, networkx, and numpy for graph visualizations")
            return []
        except Exception as e:
            logger.error(f"Error creating visualizations: {e}")
            import traceback
            logger.debug(traceback.format_exc())
            return []
    
    def _create_graph_visualization(self, graph_data: Dict[str, Any], output_path: Path) -> None:
        """
        Create a visualization of the full knowledge graph.
        
        Args:
            graph_data: Graph data containing nodes and edges
            output_path: Output file path
        """
        try:
            import matplotlib.pyplot as plt
            import networkx as nx
            import numpy as np
            
            # Create graph
            G = nx.Graph()
            
            # Add nodes with attributes
            for node in graph_data["nodes"]:
                node_id = node.get("id", "")
                if not node_id:
                    continue
                    
                G.add_node(
                    node_id,
                    label=node.get("label", ""),
                    type=node.get("type", "unknown"),
                    confidence=node.get("confidence", 0.5)
                )
            
            # Add edges with attributes
            for edge in graph_data["edges"]:
                source = edge.get("source", "")
                target = edge.get("target", "")
                if not source or not target or source not in G.nodes or target not in G.nodes:
                    continue
                    
                G.add_edge(
                    source,
                    target,
                    type=edge.get("type", "unknown"),
                    weight=edge.get("weight", 1.0)
                )
            
            if not G.nodes:
                logger.warning("No nodes to visualize in graph")
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
            
            # Define node sizes based on confidence
            node_sizes = []
            for node_id in G.nodes:
                confidence = G.nodes[node_id].get("confidence", 0.5)
                size = 100 + (confidence * 300)  # Scale size by confidence
                node_sizes.append(size)
            
            # Create visualization
            plt.figure(figsize=(12, 10))
            
            # Use spring layout for graph visualization
            pos = nx.spring_layout(G, seed=42)
            
            # Draw nodes
            nx.draw_networkx_nodes(
                G, pos, 
                node_color=node_colors,
                node_size=node_sizes,
                alpha=0.8
            )
            
            # Draw edges
            nx.draw_networkx_edges(
                G, pos, 
                width=1.0,
                alpha=0.5
            )
            
            # Draw labels
            labels = {node_id: G.nodes[node_id].get("label", "")[:20] for node_id in G.nodes}
            nx.draw_networkx_labels(
                G, pos,
                labels=labels,
                font_size=8,
                font_family="sans-serif"
            )
            
            plt.title("LEXICON Knowledge Graph")
            plt.axis("off")
            plt.tight_layout()
            
            # Save visualization
            plt.savefig(output_path, dpi=300, bbox_inches="tight")
            plt.close()
            
        except Exception as e:
            logger.error(f"Failed to create graph visualization: {e}")
            if output_path.exists():
                output_path.unlink()
    
    def _create_case_distribution(self, graph_data: Dict[str, Any], output_path: Path) -> None:
        """
        Create a visualization of grammatical case distribution.
        
        Args:
            graph_data: Graph data containing nodes and edges
            output_path: Output file path
        """
        try:
            import matplotlib.pyplot as plt
            import numpy as np
            
            # Extract cases
            cases = {}
            for node in graph_data["nodes"]:
                case = node.get("case", "none")
                if not case:
                    case = "none"
                cases[case] = cases.get(case, 0) + 1
            
            if not cases:
                logger.warning("No case data to visualize")
                return
            
            # Create visualization
            plt.figure(figsize=(10, 6))
            
            # Sort cases by frequency
            sorted_cases = sorted(cases.items(), key=lambda x: x[1], reverse=True)
            labels = [case for case, count in sorted_cases]
            values = [count for case, count in sorted_cases]
            
            plt.bar(labels, values)
            plt.title("Grammatical Case Distribution")
            plt.xlabel("Case")
            plt.ylabel("Count")
            plt.xticks(rotation=45)
            plt.tight_layout()
            
            # Save visualization
            plt.savefig(output_path)
            plt.close()
            
        except Exception as e:
            logger.error(f"Failed to create case distribution visualization: {e}")
            if output_path.exists():
                output_path.unlink()
    
    def _create_entity_network(self, graph_data: Dict[str, Any], output_path: Path) -> None:
        """
        Create a visualization of the entity network.
        
        Args:
            graph_data: Graph data containing nodes and edges
            output_path: Output file path
        """
        try:
            import matplotlib.pyplot as plt
            import networkx as nx
            import numpy as np
            
            # Create graph with only entity nodes
            G = nx.Graph()
            
            # Add entity nodes
            entity_nodes = [node for node in graph_data["nodes"] if node.get("type") == "entity"]
            
            for node in entity_nodes:
                node_id = node.get("id", "")
                if not node_id:
                    continue
                    
                G.add_node(
                    node_id,
                    label=node.get("label", ""),
                    entity_type=node.get("entity_type", "unknown"),
                    confidence=node.get("confidence", 0.5)
                )
            
            # Add edges between entities that share connections
            added_edges = set()
            for edge in graph_data["edges"]:
                source = edge.get("source", "")
                target = edge.get("target", "")
                
                if not source or not target:
                    continue
                    
                # Find source and target nodes
                source_node = next((node for node in entity_nodes if node.get("id") == source), None)
                target_node = next((node for node in entity_nodes if node.get("id") == target), None)
                
                if source_node and target_node:
                    edge_key = tuple(sorted([source, target]))
                    if edge_key not in added_edges:
                        G.add_edge(source, target, weight=edge.get("weight", 1.0))
                        added_edges.add(edge_key)
            
            if not G.nodes:
                logger.warning("No entities to visualize in network")
                return
                
            # Create visualization
            plt.figure(figsize=(12, 10))
            
            # Use spring layout for visualization
            pos = nx.spring_layout(G, seed=42)
            
            # Draw nodes
            nx.draw_networkx_nodes(
                G, pos, 
                node_color="skyblue",
                node_size=200,
                alpha=0.8
            )
            
            # Draw edges
            nx.draw_networkx_edges(
                G, pos, 
                width=1.0,
                alpha=0.5
            )
            
            # Draw labels
            labels = {node_id: G.nodes[node_id].get("label", "")[:20] for node_id in G.nodes}
            nx.draw_networkx_labels(
                G, pos,
                labels=labels,
                font_size=8,
                font_family="sans-serif"
            )
            
            plt.title("Entity Network")
            plt.axis("off")
            plt.tight_layout()
            
            # Save visualization
            plt.savefig(output_path)
            plt.close()
            
        except Exception as e:
            logger.error(f"Failed to create entity network visualization: {e}")
            if output_path.exists():
                output_path.unlink()
    
    def _create_polarity_distribution(self, graph_data: Dict[str, Any], output_path: Path) -> None:
        """
        Create visualization of claim polarity distribution.
        
        Args:
            graph_data: Graph data containing nodes and edges
            output_path: Output file path
        """
        try:
            import matplotlib.pyplot as plt
            import numpy as np
            
            # Extract claims and their polarities
            claims = [n for n in graph_data["nodes"] if n.get("type") == "claim"]
            polarities = {}
            
            # Count polarities
            for claim in claims:
                polarity = claim.get("polarity", "neutral")
                polarities[polarity] = polarities.get(polarity, 0) + 1
            
            if not polarities:
                logger.warning("No polarity data to visualize")
                return
            
            # Create visualization
            labels = list(polarities.keys())
            values = list(polarities.values())
            colors = ['green' if p == 'positive' else 'red' if p == 'negative' else 'gray' for p in labels]
            
            plt.figure(figsize=(10, 6))
            plt.bar(labels, values, color=colors)
            plt.title('Claim Polarity Distribution')
            plt.xlabel('Polarity')
            plt.ylabel('Count')
            plt.tight_layout()
            
            # Save visualization
            plt.savefig(output_path)
            plt.close()
            
        except Exception as e:
            logger.error(f"Failed to create polarity distribution: {e}")
            if output_path.exists():
                output_path.unlink()
    
    def _create_entity_claim_relationships(self, graph_data: Dict[str, Any], output_path: Path) -> None:
        """
        Create visualization of entity-claim relationships.
        
        Args:
            graph_data: Graph data containing nodes and edges
            output_path: Output file path
        """
        try:
            import matplotlib.pyplot as plt
            import networkx as nx
            import numpy as np
            
            # Create bipartite graph
            G = nx.Graph()
            
            # Add entity nodes
            entities = {}
            for node in graph_data["nodes"]:
                if node.get("type") == "entity":
                    node_id = node.get("id", "")
                    if node_id:
                        G.add_node(node_id, 
                            label=node.get("label", ""),
                            bipartite=0,  # Entities
                            node_type="entity"
                        )
                        entities[node_id] = node
            
            # Add claim nodes
            claims = {}
            for node in graph_data["nodes"]:
                if node.get("type") == "claim":
                    node_id = node.get("id", "")
                    if node_id:
                        G.add_node(node_id, 
                            label=node.get("label", ""),
                            bipartite=1,  # Claims
                            node_type="claim",
                            polarity=node.get("polarity", "neutral")
                        )
                        claims[node_id] = node
            
            # Add edges between entities and claims
            for edge in graph_data["edges"]:
                source = edge.get("source", "")
                target = edge.get("target", "")
                
                if not source or not target:
                    continue
                
                # Add edge if it connects an entity and a claim
                if (source in entities and target in claims) or (source in claims and target in entities):
                    G.add_edge(source, target)
            
            if not G.nodes:
                logger.warning("No nodes to visualize in entity-claim relationships")
                return
            
            # Create visualization
            plt.figure(figsize=(12, 10))
            
            # Get node sets
            entity_nodes = [n for n, d in G.nodes(data=True) if d["node_type"] == "entity"]
            claim_nodes = [n for n, d in G.nodes(data=True) if d["node_type"] == "claim"]
            
            # Position nodes using bipartite layout
            pos = nx.bipartite_layout(G, entity_nodes)
            
            # Draw entities
            nx.draw_networkx_nodes(
                G, pos,
                nodelist=entity_nodes,
                node_color="skyblue",
                node_shape="o",
                node_size=200,
                alpha=0.8
            )
            
            # Draw claims with color based on polarity
            claim_colors = []
            for claim_id in claim_nodes:
                polarity = G.nodes[claim_id].get("polarity", "neutral")
                if polarity == "positive":
                    claim_colors.append("green")
                elif polarity == "negative":
                    claim_colors.append("red")
                else:
                    claim_colors.append("gray")
            
            nx.draw_networkx_nodes(
                G, pos,
                nodelist=claim_nodes,
                node_color=claim_colors,
                node_shape="s",
                node_size=300,
                alpha=0.8
            )
            
            # Draw edges
            nx.draw_networkx_edges(
                G, pos,
                width=1.0,
                alpha=0.5
            )
            
            # Draw labels
            labels = {node_id: G.nodes[node_id].get("label", "")[:20] for node_id in G.nodes}
            nx.draw_networkx_labels(
                G, pos,
                labels=labels,
                font_size=8,
                font_family="sans-serif"
            )
            
            plt.title("Entity-Claim Relationships")
            plt.axis("off")
            plt.tight_layout()
            
            # Save visualization
            plt.savefig(output_path)
            plt.close()
            
        except Exception as e:
            logger.error(f"Failed to create entity-claim relationship visualization: {e}")
            if output_path.exists():
                output_path.unlink() 