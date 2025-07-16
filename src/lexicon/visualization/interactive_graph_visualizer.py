"""
LEXICON Interactive Graph Visualizer

Advanced interactive visualization system for knowledge graphs with:
- Hierarchical clustering for large graphs
- Interactive filtering and exploration
- Real-time layout algorithms
- Export capabilities
- Performance optimization for large datasets
"""

import math
import logging
from typing import List, Dict, Any, Set, Tuple, Optional, Union
from dataclasses import dataclass
import json
from collections import defaultdict, Counter
import colorsys

from ..core.config import LexiconConfig
from ..core.logging import get_logger

try:
    import plotly.graph_objects as go
    import plotly.express as px
    from plotly.subplots import make_subplots
    import plotly.io as pio
    PLOTLY_AVAILABLE = True
except ImportError:
    PLOTLY_AVAILABLE = False

try:
    import networkx as nx
    from networkx.algorithms import community
    NETWORKX_AVAILABLE = True
except ImportError:
    NETWORKX_AVAILABLE = False

try:
    import numpy as np
    from sklearn.cluster import DBSCAN
    from sklearn.manifold import TSNE
    from sklearn.decomposition import PCA
    SKLEARN_AVAILABLE = True
except ImportError:
    SKLEARN_AVAILABLE = False


@dataclass
class GraphCluster:
    """Represents a cluster of related nodes."""
    cluster_id: int
    nodes: List[str]
    centroid: Tuple[float, float]
    size: int
    dominant_case: str
    cluster_type: str  # 'entity', 'relation', 'mixed'
    cohesion_score: float


@dataclass
class VisualizationConfig:
    """Configuration for graph visualization."""
    max_nodes_full_layout: int = 100
    clustering_threshold: int = 50
    node_size_range: Tuple[int, int] = (10, 50)
    edge_width_range: Tuple[float, float] = (0.5, 5.0)
    enable_physics: bool = True
    show_node_labels: bool = True
    show_edge_labels: bool = False
    color_by_case: bool = True
    layout_algorithm: str = "force_directed"  # 'force_directed', 'circular', 'hierarchical'


class InteractiveGraphVisualizer:
    """
    Advanced interactive graph visualizer for LEXICON knowledge graphs.
    
    Features:
    - Automatic clustering for large graphs
    - Interactive filtering and exploration
    - Multiple layout algorithms
    - Performance optimization
    - Export capabilities
    """
    
    def __init__(self, config: Optional[LexiconConfig] = None):
        """
        Initialize the interactive graph visualizer.
        
        Args:
            config: LEXICON configuration object
        """
        self.config = config
        self.logger = get_logger("visualization.interactive_graph_visualizer")
        
        # Visualization configuration
        self.viz_config = VisualizationConfig()
        
        # Color schemes for different cases
        self.case_colors = {
            "nominative": "#FF6B6B",    # Red
            "accusative": "#4ECDC4",    # Teal
            "genitive": "#45B7D1",      # Blue
            "dative": "#96CEB4",        # Green
            "ablative": "#FFEAA7",      # Yellow
            "locative": "#DDA0DD",      # Plum
            "instrumental": "#FFB347",   # Orange
            "vocative": "#98D8C8",      # Mint
            "unknown": "#BDC3C7"        # Gray
        }
        
        # Relation type colors
        self.relation_colors = {
            "causal_relationship": "#E74C3C",
            "temporal_relationship": "#3498DB", 
            "spatial_relationship": "#2ECC71",
            "similarity_relationship": "#F39C12",
            "part_whole_relationship": "#9B59B6",
            "proximity_relationship": "#95A5A6",
            "default": "#34495E"
        }
        
        # Layout algorithms
        self.layout_algorithms = {
            "force_directed": self._force_directed_layout,
            "circular": self._circular_layout,
            "hierarchical": self._hierarchical_layout,
            "clustered": self._clustered_layout
        }
        
        # Performance tracking
        self.metrics = {
            "nodes_rendered": 0,
            "edges_rendered": 0,
            "clusters_created": 0,
            "layout_time": 0.0,
            "render_time": 0.0
        }
    
    def create_interactive_visualization(self, 
                                       entities: List[Dict[str, Any]], 
                                       relations: List[Dict[str, Any]] = None,
                                       title: str = "LEXICON Knowledge Graph",
                                       filter_options: Dict[str, Any] = None) -> go.Figure:
        """
        Create an interactive visualization of the knowledge graph.
        
        Args:
            entities: List of entity dictionaries
            relations: List of relation dictionaries
            title: Title for the visualization
            filter_options: Options for filtering nodes/edges
            
        Returns:
            Plotly figure with interactive graph
        """
        if not PLOTLY_AVAILABLE:
            self.logger.error("Plotly not available for interactive visualization")
            return None
        
        if not entities:
            self.logger.warning("No entities provided for visualization")
            return self._create_empty_figure(title)
        
        self.logger.info(f"Creating interactive visualization with {len(entities)} entities")
        
        # Apply filters if specified
        if filter_options:
            entities = self._apply_filters(entities, filter_options)
            if relations:
                relations = self._filter_relations(relations, entities, filter_options)
        
        # Determine if clustering is needed
        use_clustering = len(entities) > self.viz_config.clustering_threshold
        
        if use_clustering:
            return self._create_clustered_visualization(entities, relations, title)
        else:
            return self._create_full_visualization(entities, relations, title)
    
    def _create_full_visualization(self, entities: List[Dict], 
                                 relations: List[Dict] = None,
                                 title: str = "Knowledge Graph") -> go.Figure:
        """Create full detailed visualization for smaller graphs."""
        import time
        start_time = time.time()
        
        # Create NetworkX graph for layout
        G = self._build_networkx_graph(entities, relations)
        
        # Calculate layout
        layout_start = time.time()
        if len(entities) > self.viz_config.max_nodes_full_layout:
            pos = self._clustered_layout(G)
        else:
            pos = self.layout_algorithms[self.viz_config.layout_algorithm](G)
        self.metrics["layout_time"] = time.time() - layout_start
        
        # Create Plotly figure
        fig = go.Figure()
        
        # Add edges
        if relations:
            self._add_edges_to_figure(fig, relations, entities, pos)
        
        # Add nodes
        self._add_nodes_to_figure(fig, entities, pos)
        
        # Configure layout
        self._configure_figure_layout(fig, title, len(entities), len(relations or []))
        
        self.metrics["nodes_rendered"] = len(entities)
        self.metrics["edges_rendered"] = len(relations or [])
        self.metrics["render_time"] = time.time() - start_time
        
        return fig
    
    def _create_clustered_visualization(self, entities: List[Dict], 
                                      relations: List[Dict] = None,
                                      title: str = "Clustered Knowledge Graph") -> go.Figure:
        """Create clustered visualization for large graphs."""
        import time
        start_time = time.time()
        
        # Perform clustering
        clusters = self._cluster_entities(entities, relations)
        
        # Create cluster-level visualization
        fig = self._create_cluster_overview(clusters, title)
        
        # Add drill-down capability
        self._add_cluster_drill_down(fig, clusters, entities, relations)
        
        self.metrics["clusters_created"] = len(clusters)
        self.metrics["render_time"] = time.time() - start_time
        
        return fig
    
    def _cluster_entities(self, entities: List[Dict], 
                         relations: List[Dict] = None) -> List[GraphCluster]:
        """Cluster entities for large graph visualization."""
        if not SKLEARN_AVAILABLE or not NETWORKX_AVAILABLE:
            # Fallback: simple case-based clustering
            return self._simple_case_clustering(entities)
        
        # Build feature vectors for clustering
        features = []
        entity_map = {}
        
        for i, entity in enumerate(entities):
            # Features: case one-hot, confidence, text length, position features
            case_vector = [0] * len(self.case_colors)
            if entity.get("case") in self.case_colors:
                case_idx = list(self.case_colors.keys()).index(entity["case"])
                case_vector[case_idx] = 1
            
            feature_vector = case_vector + [
                entity.get("confidence", 0.5),
                len(entity.get("text", "")),
                hash(entity.get("text", "")) % 1000 / 1000.0  # Text hash feature
            ]
            
            features.append(feature_vector)
            entity_map[i] = entity
        
        # Perform DBSCAN clustering
        features_array = np.array(features)
        clustering = DBSCAN(eps=0.5, min_samples=3).fit(features_array)
        
        # Group entities by cluster
        cluster_groups = defaultdict(list)
        for i, cluster_id in enumerate(clustering.labels_):
            cluster_groups[cluster_id].append(i)
        
        # Create cluster objects
        clusters = []
        for cluster_id, entity_indices in cluster_groups.items():
            if cluster_id == -1:  # Noise points
                continue
                
            cluster_entities = [entities[i] for i in entity_indices]
            
            # Calculate cluster properties
            cases = [e.get("case", "unknown") for e in cluster_entities]
            dominant_case = Counter(cases).most_common(1)[0][0]
            
            # Calculate centroid (for visualization)
            if len(entity_indices) > 1:
                centroid_features = np.mean(features_array[entity_indices], axis=0)
                centroid = (float(centroid_features[0]), float(centroid_features[1]))
            else:
                centroid = (0.0, 0.0)
            
            # Calculate cohesion score
            cohesion = self._calculate_cluster_cohesion(cluster_entities, relations)
            
            cluster = GraphCluster(
                cluster_id=cluster_id,
                nodes=[e["text"] for e in cluster_entities],
                centroid=centroid,
                size=len(cluster_entities),
                dominant_case=dominant_case,
                cluster_type="entity",
                cohesion_score=cohesion
            )
            clusters.append(cluster)
        
        return clusters
    
    def _simple_case_clustering(self, entities: List[Dict]) -> List[GraphCluster]:
        """Simple clustering based on grammatical cases."""
        case_groups = defaultdict(list)
        
        for entity in entities:
            case = entity.get("case", "unknown")
            case_groups[case].append(entity)
        
        clusters = []
        for cluster_id, (case, cluster_entities) in enumerate(case_groups.items()):
            if len(cluster_entities) < 2:
                continue
                
            cluster = GraphCluster(
                cluster_id=cluster_id,
                nodes=[e["text"] for e in cluster_entities],
                centroid=(cluster_id * 2.0, cluster_id * 2.0),
                size=len(cluster_entities),
                dominant_case=case,
                cluster_type="entity",
                cohesion_score=0.8  # Default cohesion for case-based clustering
            )
            clusters.append(cluster)
        
        return clusters
    
    def _calculate_cluster_cohesion(self, cluster_entities: List[Dict], 
                                   relations: List[Dict] = None) -> float:
        """Calculate cohesion score for a cluster."""
        if not relations or len(cluster_entities) < 2:
            return 0.5
        
        cluster_texts = {e["text"] for e in cluster_entities}
        internal_relations = 0
        total_possible = len(cluster_entities) * (len(cluster_entities) - 1) / 2
        
        for relation in relations:
            source = relation.get("source_entity", "")
            target = relation.get("target_entity", "")
            
            if source in cluster_texts and target in cluster_texts:
                internal_relations += 1
        
        return internal_relations / total_possible if total_possible > 0 else 0.5
    
    def _create_cluster_overview(self, clusters: List[GraphCluster], 
                                title: str) -> go.Figure:
        """Create overview visualization of clusters."""
        fig = go.Figure()
        
        # Add cluster nodes
        x_coords = []
        y_coords = []
        sizes = []
        colors = []
        hover_texts = []
        
        for cluster in clusters:
            x_coords.append(cluster.centroid[0])
            y_coords.append(cluster.centroid[1])
            sizes.append(min(50, max(10, cluster.size * 5)))  # Scale size
            colors.append(self.case_colors.get(cluster.dominant_case, "#BDC3C7"))
            
            hover_text = (f"Cluster {cluster.cluster_id}<br>"
                         f"Size: {cluster.size}<br>"
                         f"Dominant Case: {cluster.dominant_case}<br>"
                         f"Cohesion: {cluster.cohesion_score:.2f}")
            hover_texts.append(hover_text)
        
        fig.add_trace(go.Scatter(
            x=x_coords,
            y=y_coords,
            mode='markers',
            marker=dict(
                size=sizes,
                color=colors,
                line=dict(width=2, color='white'),
                opacity=0.8
            ),
            text=hover_texts,
            hoverinfo='text',
            name='Clusters'
        ))
        
        # Add cluster connections
        self._add_cluster_connections(fig, clusters)
        
        # Configure layout
        fig.update_layout(
            title=title,
            showlegend=False,
            hovermode='closest',
            margin=dict(b=20, l=5, r=5, t=40),
            annotations=[
                dict(
                    text="Click clusters to drill down",
                    showarrow=False,
                    xref="paper", yref="paper",
                    x=0.005, y=-0.002,
                    xanchor='left', yanchor='bottom',
                    font=dict(size=12)
                )
            ],
            xaxis=dict(showgrid=False, zeroline=False, showticklabels=False),
            yaxis=dict(showgrid=False, zeroline=False, showticklabels=False),
            plot_bgcolor='white'
        )
        
        return fig
    
    def _add_cluster_connections(self, fig: go.Figure, clusters: List[GraphCluster]):
        """Add connections between clusters."""
        # For simplicity, connect clusters that are geographically close
        for i, cluster1 in enumerate(clusters):
            for j, cluster2 in enumerate(clusters[i+1:], i+1):
                distance = math.sqrt(
                    (cluster1.centroid[0] - cluster2.centroid[0])**2 +
                    (cluster1.centroid[1] - cluster2.centroid[1])**2
                )
                
                if distance < 3.0:  # Threshold for connection
                    fig.add_trace(go.Scatter(
                        x=[cluster1.centroid[0], cluster2.centroid[0]],
                        y=[cluster1.centroid[1], cluster2.centroid[1]],
                        mode='lines',
                        line=dict(width=1, color='gray'),
                        opacity=0.3,
                        showlegend=False,
                        hoverinfo='none'
                    ))
    
    def _build_networkx_graph(self, entities: List[Dict], 
                             relations: List[Dict] = None) -> 'nx.Graph':
        """Build NetworkX graph from entities and relations."""
        if not NETWORKX_AVAILABLE:
            return None
        
        G = nx.Graph()
        
        # Add nodes
        for entity in entities:
            G.add_node(
                entity["text"],
                case=entity.get("case", "unknown"),
                confidence=entity.get("confidence", 0.5),
                **entity
            )
        
        # Add edges
        if relations:
            for relation in relations:
                source = relation.get("source_entity", "")
                target = relation.get("target_entity", "")
                
                if source and target and G.has_node(source) and G.has_node(target):
                    G.add_edge(
                        source, 
                        target,
                        relation_type=relation.get("relation_type", "unknown"),
                        confidence=relation.get("confidence", 0.5),
                        **relation
                    )
        
        return G
    
    def _force_directed_layout(self, G: 'nx.Graph') -> Dict[str, Tuple[float, float]]:
        """Generate force-directed layout."""
        if not NETWORKX_AVAILABLE:
            return {}
        
        return nx.spring_layout(G, k=1, iterations=50)
    
    def _circular_layout(self, G: 'nx.Graph') -> Dict[str, Tuple[float, float]]:
        """Generate circular layout."""
        if not NETWORKX_AVAILABLE:
            return {}
        
        return nx.circular_layout(G)
    
    def _hierarchical_layout(self, G: 'nx.Graph') -> Dict[str, Tuple[float, float]]:
        """Generate hierarchical layout based on cases."""
        if not NETWORKX_AVAILABLE:
            return {}
        
        # Group nodes by case
        case_groups = defaultdict(list)
        for node in G.nodes():
            case = G.nodes[node].get("case", "unknown")
            case_groups[case].append(node)
        
        pos = {}
        y_level = 0
        level_height = 2.0
        
        for case, nodes in case_groups.items():
            # Arrange nodes in this case level horizontally
            x_spacing = 2.0
            start_x = -(len(nodes) - 1) * x_spacing / 2
            
            for i, node in enumerate(nodes):
                pos[node] = (start_x + i * x_spacing, y_level)
            
            y_level += level_height
        
        return pos
    
    def _clustered_layout(self, G: 'nx.Graph') -> Dict[str, Tuple[float, float]]:
        """Generate layout with community detection."""
        if not NETWORKX_AVAILABLE:
            return self._force_directed_layout(G)
        
        try:
            # Detect communities
            communities = community.greedy_modularity_communities(G)
            
            pos = {}
            cluster_centers = []
            
            # Place community centers in a circle
            n_communities = len(communities)
            for i in range(n_communities):
                angle = 2 * math.pi * i / n_communities
                center_x = 5 * math.cos(angle)
                center_y = 5 * math.sin(angle)
                cluster_centers.append((center_x, center_y))
            
            # Position nodes within each community
            for i, community_nodes in enumerate(communities):
                center_x, center_y = cluster_centers[i]
                
                # Create subgraph for this community
                subgraph = G.subgraph(community_nodes)
                sub_pos = nx.spring_layout(subgraph, k=0.5, iterations=30)
                
                # Offset positions relative to community center
                for node, (x, y) in sub_pos.items():
                    pos[node] = (center_x + x, center_y + y)
            
            return pos
            
        except Exception as e:
            self.logger.warning(f"Clustered layout failed, falling back to spring layout: {e}")
            return self._force_directed_layout(G)
    
    def _add_nodes_to_figure(self, fig: go.Figure, entities: List[Dict], 
                           pos: Dict[str, Tuple[float, float]]):
        """Add nodes to the Plotly figure."""
        x_coords = []
        y_coords = []
        colors = []
        sizes = []
        hover_texts = []
        node_texts = []
        
        for entity in entities:
            text = entity["text"]
            if text not in pos:
                continue
                
            x, y = pos[text]
            x_coords.append(x)
            y_coords.append(y)
            
            # Color by case
            case = entity.get("case", "unknown")
            colors.append(self.case_colors.get(case, "#BDC3C7"))
            
            # Size by confidence
            confidence = entity.get("confidence", 0.5)
            size = self.viz_config.node_size_range[0] + (
                confidence * (self.viz_config.node_size_range[1] - self.viz_config.node_size_range[0])
            )
            sizes.append(size)
            
            # Hover text
            hover_text = (f"Entity: {text}<br>"
                         f"Case: {case}<br>"
                         f"Confidence: {confidence:.2f}")
            
            if "case_rationale" in entity:
                hover_text += f"<br>Rationale: {entity['case_rationale'][:100]}..."
            
            hover_texts.append(hover_text)
            
            # Node label
            if self.viz_config.show_node_labels:
                node_texts.append(text[:20] + "..." if len(text) > 20 else text)
            else:
                node_texts.append("")
        
        fig.add_trace(go.Scatter(
            x=x_coords,
            y=y_coords,
            mode='markers+text' if self.viz_config.show_node_labels else 'markers',
            marker=dict(
                size=sizes,
                color=colors,
                line=dict(width=1, color='white'),
                opacity=0.8
            ),
            text=node_texts,
            textposition="middle center",
            textfont=dict(size=8),
            hovertext=hover_texts,
            hoverinfo='text',
            name='Entities'
        ))
    
    def _add_edges_to_figure(self, fig: go.Figure, relations: List[Dict], 
                           entities: List[Dict], pos: Dict[str, Tuple[float, float]]):
        """Add edges to the Plotly figure."""
        entity_text_to_pos = {entity["text"]: pos.get(entity["text"]) 
                             for entity in entities if entity["text"] in pos}
        
        for relation in relations:
            source = relation.get("source_entity", "")
            target = relation.get("target_entity", "")
            
            if source not in entity_text_to_pos or target not in entity_text_to_pos:
                continue
            
            if entity_text_to_pos[source] is None or entity_text_to_pos[target] is None:
                continue
            
            x0, y0 = entity_text_to_pos[source]
            x1, y1 = entity_text_to_pos[target]
            
            # Edge width by confidence
            confidence = relation.get("confidence", 0.5)
            width = self.viz_config.edge_width_range[0] + (
                confidence * (self.viz_config.edge_width_range[1] - self.viz_config.edge_width_range[0])
            )
            
            # Edge color by relation type
            relation_type = relation.get("relation_type", "default")
            color = self.relation_colors.get(relation_type, self.relation_colors["default"])
            
            fig.add_trace(go.Scatter(
                x=[x0, x1, None],
                y=[y0, y1, None],
                mode='lines',
                line=dict(width=width, color=color),
                opacity=0.6,
                showlegend=False,
                hoverinfo='none'
            ))
    
    def _configure_figure_layout(self, fig: go.Figure, title: str, 
                               node_count: int, edge_count: int):
        """Configure the overall figure layout."""
        fig.update_layout(
            title=f"{title}<br><sub>Nodes: {node_count}, Edges: {edge_count}</sub>",
            showlegend=False,
            hovermode='closest',
            margin=dict(b=20, l=5, r=5, t=80),
            annotations=[
                dict(
                    text="Hover over nodes for details. Colors represent grammatical cases.",
                    showarrow=False,
                    xref="paper", yref="paper",
                    x=0.005, y=-0.002,
                    xanchor='left', yanchor='bottom',
                    font=dict(size=10)
                )
            ],
            xaxis=dict(showgrid=False, zeroline=False, showticklabels=False),
            yaxis=dict(showgrid=False, zeroline=False, showticklabels=False),
            plot_bgcolor='white',
            height=700
        )
        
        # Add case legend
        self._add_case_legend(fig)
    
    def _add_case_legend(self, fig: go.Figure):
        """Add color legend for grammatical cases."""
        legend_traces = []
        
        for i, (case, color) in enumerate(self.case_colors.items()):
            if case == "unknown":
                continue
                
            # Add invisible trace for legend
            fig.add_trace(go.Scatter(
                x=[None], y=[None],
                mode='markers',
                marker=dict(size=10, color=color),
                name=case.capitalize(),
                showlegend=True
            ))
        
        fig.update_layout(
            legend=dict(
                orientation="v",
                yanchor="top",
                y=1,
                xanchor="left", 
                x=1.02,
                bgcolor="rgba(255,255,255,0.8)",
                bordercolor="rgba(0,0,0,0.2)",
                borderwidth=1
            )
        )
    
    def _apply_filters(self, entities: List[Dict], 
                      filter_options: Dict[str, Any]) -> List[Dict]:
        """Apply filtering options to entities."""
        filtered_entities = entities.copy()
        
        # Filter by case
        if "cases" in filter_options and filter_options["cases"]:
            allowed_cases = set(filter_options["cases"])
            filtered_entities = [e for e in filtered_entities 
                               if e.get("case") in allowed_cases]
        
        # Filter by confidence threshold
        if "min_confidence" in filter_options:
            min_conf = filter_options["min_confidence"]
            filtered_entities = [e for e in filtered_entities 
                               if e.get("confidence", 0) >= min_conf]
        
        # Filter by text length
        if "min_text_length" in filter_options:
            min_len = filter_options["min_text_length"]
            filtered_entities = [e for e in filtered_entities
                               if len(e.get("text", "")) >= min_len]
        
        return filtered_entities
    
    def _filter_relations(self, relations: List[Dict], entities: List[Dict],
                         filter_options: Dict[str, Any]) -> List[Dict]:
        """Filter relations based on remaining entities."""
        entity_texts = {e["text"] for e in entities}
        
        filtered_relations = [
            r for r in relations
            if (r.get("source_entity") in entity_texts and 
                r.get("target_entity") in entity_texts)
        ]
        
        # Filter by relation confidence
        if "min_relation_confidence" in filter_options:
            min_conf = filter_options["min_relation_confidence"]
            filtered_relations = [r for r in filtered_relations
                                if r.get("confidence", 0) >= min_conf]
        
        return filtered_relations
    
    def _create_empty_figure(self, title: str) -> go.Figure:
        """Create empty figure with message."""
        fig = go.Figure()
        fig.add_annotation(
            text="No entities to visualize",
            xref="paper", yref="paper",
            x=0.5, y=0.5,
            showarrow=False,
            font=dict(size=20)
        )
        fig.update_layout(title=title)
        return fig
    
    def _add_cluster_drill_down(self, fig: go.Figure, clusters: List[GraphCluster],
                               entities: List[Dict], relations: List[Dict] = None):
        """Add drill-down capability to cluster visualization."""
        # This would typically involve JavaScript callbacks in a full web app
        # For now, we'll add this as metadata that can be used by the calling code
        cluster_data = []
        
        for cluster in clusters:
            cluster_entities = [e for e in entities if e["text"] in cluster.nodes]
            cluster_relations = []
            
            if relations:
                cluster_texts = set(cluster.nodes)
                cluster_relations = [
                    r for r in relations
                    if (r.get("source_entity") in cluster_texts and
                        r.get("target_entity") in cluster_texts)
                ]
            
            cluster_data.append({
                "cluster_id": cluster.cluster_id,
                "entities": cluster_entities,
                "relations": cluster_relations
            })
        
        # Store cluster data in figure metadata
        fig.update_layout(
            meta={"cluster_data": cluster_data}
        )
    
    def export_visualization(self, fig: go.Figure, 
                           output_path: str, 
                           format: str = "html") -> bool:
        """
        Export visualization to file.
        
        Args:
            fig: Plotly figure to export
            output_path: Path for output file
            format: Export format ('html', 'png', 'pdf', 'svg')
            
        Returns:
            Success status
        """
        if not PLOTLY_AVAILABLE:
            self.logger.error("Plotly not available for export")
            return False
        
        try:
            if format.lower() == "html":
                fig.write_html(output_path)
            elif format.lower() == "png":
                fig.write_image(output_path, format="png")
            elif format.lower() == "pdf":
                fig.write_image(output_path, format="pdf")
            elif format.lower() == "svg":
                fig.write_image(output_path, format="svg")
            else:
                self.logger.error(f"Unsupported export format: {format}")
                return False
            
            self.logger.info(f"Visualization exported to {output_path}")
            return True
            
        except Exception as e:
            self.logger.error(f"Export failed: {e}")
            return False
    
    def get_visualization_metrics(self) -> Dict[str, Any]:
        """Get metrics about the visualization process."""
        return self.metrics.copy()
    
    def create_multi_view_dashboard(self, entities: List[Dict], 
                                   relations: List[Dict] = None) -> go.Figure:
        """Create a multi-view dashboard with different perspectives."""
        from plotly.subplots import make_subplots
        
        # Create subplots
        fig = make_subplots(
            rows=2, cols=2,
            subplot_titles=("Full Graph", "Case Distribution", 
                          "Confidence Analysis", "Relation Types"),
            specs=[[{"type": "scatter"}, {"type": "bar"}],
                   [{"type": "histogram"}, {"type": "pie"}]]
        )
        
        # 1. Full graph view (simplified)
        G = self._build_networkx_graph(entities, relations)
        pos = self._force_directed_layout(G) if G else {}
        
        if pos:
            x_coords = [pos[e["text"]][0] for e in entities if e["text"] in pos]
            y_coords = [pos[e["text"]][1] for e in entities if e["text"] in pos]
            colors = [self.case_colors.get(e.get("case", "unknown"), "#BDC3C7") 
                     for e in entities if e["text"] in pos]
            
            fig.add_trace(
                go.Scatter(
                    x=x_coords, y=y_coords,
                    mode='markers',
                    marker=dict(size=8, color=colors),
                    showlegend=False
                ),
                row=1, col=1
            )
        
        # 2. Case distribution
        case_counts = Counter(e.get("case", "unknown") for e in entities)
        fig.add_trace(
            go.Bar(
                x=list(case_counts.keys()),
                y=list(case_counts.values()),
                marker_color=[self.case_colors.get(case, "#BDC3C7") 
                            for case in case_counts.keys()],
                showlegend=False
            ),
            row=1, col=2
        )
        
        # 3. Confidence distribution
        confidences = [e.get("confidence", 0.5) for e in entities]
        fig.add_trace(
            go.Histogram(
                x=confidences,
                nbinsx=20,
                marker_color="lightblue",
                showlegend=False
            ),
            row=2, col=1
        )
        
        # 4. Relation types (if available)
        if relations:
            relation_counts = Counter(r.get("relation_type", "unknown") for r in relations)
            fig.add_trace(
                go.Pie(
                    labels=list(relation_counts.keys()),
                    values=list(relation_counts.values()),
                    showlegend=False
                ),
                row=2, col=2
            )
        
        fig.update_layout(
            title="LEXICON Knowledge Graph Dashboard",
            height=800
        )
        
        return fig 