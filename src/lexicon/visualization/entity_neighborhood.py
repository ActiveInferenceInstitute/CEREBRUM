"""
LEXICON Entity Neighborhood Visualizer

Provides visualization of entity neighborhoods within the knowledge graph.
"""

import logging
import re
from pathlib import Path
import uuid
from typing import Dict, List, Any, Optional, Union

logger = logging.getLogger(__name__)

def _sanitize_filename(text: str, max_length: int = 30) -> str:
    """
    Sanitize text for use in filenames by removing/replacing problematic characters.
    
    Args:
        text: Text to sanitize
        max_length: Maximum length of resulting filename
        
    Returns:
        Sanitized filename-safe string
    """
    # Convert to lowercase
    sanitized = text.lower()
    
    # Replace spaces with underscores
    sanitized = sanitized.replace(" ", "_")
    
    # Remove or replace special characters that cause filesystem issues
    # Keep only alphanumeric, underscores, and hyphens
    sanitized = re.sub(r'[^a-z0-9_\-]', '', sanitized)
    
    # Remove multiple consecutive underscores/hyphens
    sanitized = re.sub(r'[_\-]+', '_', sanitized)
    
    # Remove leading/trailing underscores
    sanitized = sanitized.strip('_')
    
    # Ensure we have something left
    if not sanitized:
        sanitized = "unknown_entity"
    
    # Truncate to max length
    if len(sanitized) > max_length:
        sanitized = sanitized[:max_length].rstrip('_')
    
    return sanitized

def generate_entity_neighborhood_visualizations(graph_data, output_dir):
    """
    Generate visualizations of entity neighborhoods.
    
    Args:
        graph_data: Graph data dictionary
        output_dir: Output directory for visualizations
        
    Returns:
        List of paths to generated visualization files
    """
    try:
        # Check for required visualization libraries
        import matplotlib.pyplot as plt
        import networkx as nx
        import numpy as np
        
        # Create the output directory if it doesn't exist
        if not isinstance(output_dir, Path):
            output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        
        # Create the visualizer
        visualizer = EntityNeighborhoodVisualizer(graph_data)
        
        # Generate all entity neighborhoods
        visualization_paths = visualizer.visualize_all_entity_neighborhoods(output_dir)
        
        # Also create a visualization of all entity connections
        connections_path = visualizer._create_entity_connections_visualization(output_dir)
        if connections_path and connections_path.exists():
            visualization_paths.append(connections_path)
            
        logger.info(f"Successfully generated {len(visualization_paths)} entity neighborhood visualizations")
        return visualization_paths
    except ImportError as e:
        logger.error(f"Failed to generate entity neighborhood visualizations - missing dependencies: {str(e)}")
        logger.warning("Install matplotlib, networkx, and numpy for entity neighborhood visualizations")
        return []
    except Exception as e:
        logger.error(f"Failed to generate entity neighborhood visualizations: {str(e)}")
        import traceback
        logger.debug(traceback.format_exc())
        return []

class EntityNeighborhoodVisualizer:
    """
    Visualizes neighborhoods of entities in the knowledge graph.
    """
    
    def __init__(self, graph_data):
        """
        Initialize the entity neighborhood visualizer.
        
        Args:
            graph_data: Knowledge graph data from LEXICON processing
        """
        self.graph_data = graph_data
    
    def visualize_entity_neighborhood(self, entity_id, output_dir):
        """
        Generate visualization for a specific entity neighborhood.
        
        Args:
            entity_id: ID of the entity
            output_dir: Output directory for visualization
            
        Returns:
            Path to generated visualization, or None if visualization failed
        """
        try:
            import matplotlib.pyplot as plt
            import networkx as nx
            import numpy as np
            
            # Find entity in graph
            entity_node = None
            for node in self.graph_data["nodes"]:
                if node.get("id") == entity_id:
                    entity_node = node
                    break
            
            if not entity_node:
                logger.warning(f"Entity {entity_id} not found in graph")
                return None
            
            entity_label = entity_node.get("label", "Unknown Entity")
            
            # Create graph
            G = nx.Graph()
            
            # Add central entity node
            G.add_node(entity_id, 
                      label=entity_label,
                      type=entity_node.get("type", "entity"),
                      is_central=True)
            
            # Find directly connected nodes
            connected_nodes = []
            for edge in self.graph_data["edges"]:
                if edge.get("source") == entity_id:
                    target_id = edge.get("target")
                    target_node = next((n for n in self.graph_data["nodes"] if n.get("id") == target_id), None)
                    if target_node:
                        connected_nodes.append(target_node)
                        G.add_node(target_id,
                                  label=target_node.get("label", ""),
                                  type=target_node.get("type", "unknown"),
                                  is_central=False)
                        G.add_edge(entity_id, target_id, type=edge.get("type", "unknown"))
                
                elif edge.get("target") == entity_id:
                    source_id = edge.get("source")
                    source_node = next((n for n in self.graph_data["nodes"] if n.get("id") == source_id), None)
                    if source_node:
                        connected_nodes.append(source_node)
                        G.add_node(source_id,
                                  label=source_node.get("label", ""),
                                  type=source_node.get("type", "unknown"),
                                  is_central=False)
                        G.add_edge(source_id, entity_id, type=edge.get("type", "unknown"))
            
            # Skip if no connections
            if not connected_nodes:
                logger.warning(f"Entity {entity_id} has no connections")
                return None
            
            # Add connections between other nodes (if they exist)
            for i, node1 in enumerate(connected_nodes):
                for node2 in connected_nodes[i+1:]:
                    node1_id = node1.get("id")
                    node2_id = node2.get("id")
                    
                    # Check if there's an edge between these nodes
                    for edge in self.graph_data["edges"]:
                        if (edge.get("source") == node1_id and edge.get("target") == node2_id) or \
                           (edge.get("source") == node2_id and edge.get("target") == node1_id):
                            G.add_edge(node1_id, node2_id, type=edge.get("type", "unknown"))
            
            # Create visualization
            plt.figure(figsize=(10, 8))
            
            # Use circular layout with entity at center
            pos = nx.spring_layout(G, seed=42)
            
            # Define node colors based on type
            node_colors = []
            node_sizes = []
            for node_id in G.nodes:
                is_central = G.nodes[node_id].get("is_central", False)
                node_type = G.nodes[node_id].get("type", "unknown")
                
                # Central entity is highlighted
                if is_central:
                    node_colors.append("red")
                    node_sizes.append(500)
                # Other entities
                elif node_type == "entity":
                    node_colors.append("skyblue")
                    node_sizes.append(300)
                # Claims
                elif node_type == "claim":
                    node_colors.append("lightgreen")
                    node_sizes.append(300)
                # Other nodes
                else:
                    node_colors.append("gray")
                    node_sizes.append(200)
            
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
                width=1.5,
                alpha=0.7
            )
            
            # Draw labels
            labels = {node_id: G.nodes[node_id].get("label", "")[:20] for node_id in G.nodes}
            nx.draw_networkx_labels(
                G, pos,
                labels=labels,
                font_size=9,
                font_family="sans-serif"
            )
            
            # Create title with entity name
            plt.title(f"Entity Neighborhood: {entity_label}")
            plt.axis("off")
            plt.tight_layout()
            
            # Create output path
            entity_slug = _sanitize_filename(entity_label)
            output_path = output_dir / f"entity_{entity_slug}_{entity_id[:8]}.png"
            
            # Save visualization
            plt.savefig(output_path, dpi=200, bbox_inches="tight")
            plt.close()
            
            logger.info(f"Created entity neighborhood visualization for '{entity_label}'")
            return output_path
            
        except Exception as e:
            logger.error(f"Failed to visualize entity neighborhood for {entity_id}: {e}")
            return None
    
    def visualize_all_entity_neighborhoods(self, output_dir):
        """
        Generate visualizations for all entity neighborhoods.
        
        Args:
            output_dir: Output directory for visualizations
            
        Returns:
            List of paths to generated visualization files
        """
        visualization_paths = []
        
        # Get all entity nodes
        entity_nodes = [node for node in self.graph_data["nodes"] 
                        if node.get("type") == "entity"]
        
        if not entity_nodes:
            logger.warning("No entities found for neighborhood visualization")
            return []
            
        # Create visualizations for each entity
        for entity in entity_nodes:
            entity_id = entity.get("id")
            if entity_id:
                try:
                    vis_path = self.visualize_entity_neighborhood(entity_id, output_dir)
                    if vis_path and vis_path.exists():
                        visualization_paths.append(vis_path)
                except Exception as e:
                    logger.error(f"Failed to visualize entity {entity_id}: {str(e)}")
                    continue
        
        # Create entity connections visualization
        try:
            connections_path = self._create_entity_connections_visualization(output_dir)
            if connections_path and connections_path.exists():
                visualization_paths.append(connections_path)
        except Exception as e:
            logger.error(f"Failed to create entity connections visualization: {str(e)}")
        
        logger.info(f"Created {len(visualization_paths)} entity neighborhood visualizations")
        return visualization_paths
    
    def _create_entity_connections_visualization(self, output_dir):
        """
        Create a visualization showing connections between entities.
        
        Args:
            output_dir: Output directory for visualization
            
        Returns:
            Path to generated visualization
        """
        try:
            import matplotlib.pyplot as plt
            import networkx as nx
            import numpy as np
            
            # Create graph with only entity nodes
            G = nx.Graph()
            
            # Add entity nodes
            entities = {}
            for node in self.graph_data["nodes"]:
                if node.get("type") == "entity":
                    node_id = node.get("id", "")
                    if node_id:
                        G.add_node(
                            node_id,
                            label=node.get("label", ""),
                            entity_type=node.get("entity_type", "unknown")
                        )
                        entities[node_id] = node
            
            # Add edges between entities that are connected by claims
            for source_id, source_entity in entities.items():
                for target_id, target_entity in entities.items():
                    if source_id == target_id:
                        continue
                    
                    # Find claims that connect these entities
                    connecting_claims = []
                    for claim_node in self.graph_data["nodes"]:
                        if claim_node.get("type") != "claim":
                            continue
                        
                        claim_id = claim_node.get("id", "")
                        if not claim_id:
                            continue
                        
                        # Check if this claim connects to both entities
                        connects_source = False
                        connects_target = False
                        
                        for edge in self.graph_data["edges"]:
                            if edge.get("source") == source_id and edge.get("target") == claim_id:
                                connects_source = True
                            if edge.get("source") == target_id and edge.get("target") == claim_id:
                                connects_target = True
                            if edge.get("target") == source_id and edge.get("source") == claim_id:
                                connects_source = True
                            if edge.get("target") == target_id and edge.get("source") == claim_id:
                                connects_target = True
                        
                        if connects_source and connects_target:
                            connecting_claims.append(claim_node)
                    
                    # Add edge if there are connecting claims
                    if connecting_claims:
                        G.add_edge(
                            source_id,
                            target_id,
                            weight=len(connecting_claims),
                            claims=connecting_claims
                        )
            
            # Skip if no connections
            if not G.edges:
                logger.warning("No entity connections to visualize")
                return None
            
            # Create visualization
            plt.figure(figsize=(12, 10))
            
            # Use spring layout with some repulsion
            pos = nx.spring_layout(G, k=1.5, seed=42)
            
            # Get node sizes based on degree
            node_sizes = [100 + (G.degree(n) * 50) for n in G.nodes]
            
            # Draw nodes
            nx.draw_networkx_nodes(
                G, pos, 
                node_color="skyblue",
                node_size=node_sizes,
                alpha=0.8
            )
            
            # Draw edges with varying width based on number of connecting claims
            edge_widths = [G[u][v].get("weight", 1) for u, v in G.edges()]
            nx.draw_networkx_edges(
                G, pos, 
                width=edge_widths,
                alpha=0.6
            )
            
            # Draw labels
            labels = {node_id: G.nodes[node_id].get("label", "")[:15] for node_id in G.nodes}
            nx.draw_networkx_labels(
                G, pos,
                labels=labels,
                font_size=8,
                font_family="sans-serif"
            )
            
            plt.title("Entity Connections Through Claims")
            plt.axis("off")
            plt.tight_layout()
            
            # Save visualization
            output_path = output_dir / "entity_connections.png"
            plt.savefig(output_path, dpi=200, bbox_inches="tight")
            plt.close()
            
            logger.info(f"Created entity connections visualization")
            return output_path
            
        except Exception as e:
            logger.error(f"Failed to create entity connections visualization: {e}")
            return None 