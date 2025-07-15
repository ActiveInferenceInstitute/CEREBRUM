"""
LEXICON Graph Assembler

Assembles processed segments into a knowledge graph.
"""

import time
import json
import uuid
from typing import List, Dict, Any, Optional, Set, Tuple
from dataclasses import dataclass, field
import logging
from datetime import datetime
import hashlib

from ..core.config import LexiconConfig
from ..core.logging import get_logger, LoggingTimer
from ..core.exceptions import ProcessingError, GraphError
from ..paraphrase.generator import ParaphrasedSegment
from .entity_linker import EntityLinker
from .cid_generator import generate_cid

from src.llm.OpenRouter import OpenRouterClient, OpenRouterConfig


@dataclass
class GraphNode:
    """A node in the knowledge graph."""
    id: str
    type: str
    text: str
    case: Optional[str] = None
    confidence: float = 1.0
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary representation."""
        return {
            "id": self.id,
            "type": self.type,
            "text": self.text,
            "case": self.case,
            "confidence": self.confidence,
            "metadata": self.metadata
        }


@dataclass
class GraphEdge:
    """An edge in the knowledge graph."""
    id: str
    source: str
    target: str
    type: str
    confidence: float = 1.0
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary representation."""
        return {
            "id": self.id,
            "source": self.source,
            "target": self.target,
            "type": self.type,
            "confidence": self.confidence,
            "metadata": self.metadata
        }


@dataclass
class KnowledgeGraph:
    """A knowledge graph built from processed segments."""
    id: str
    nodes: List[GraphNode] = field(default_factory=list)
    edges: List[GraphEdge] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def add_node(self, node: GraphNode) -> str:
        """
        Add a node to the graph.
        
        Args:
            node: Node to add
            
        Returns:
            Node ID
        """
        self.nodes.append(node)
        return node.id
    
    def add_edge(self, edge: GraphEdge) -> str:
        """
        Add an edge to the graph.
        
        Args:
            edge: Edge to add
            
        Returns:
            Edge ID
        """
        self.edges.append(edge)
        return edge.id
    
    def get_node(self, node_id: str) -> Optional[GraphNode]:
        """
        Get a node by ID.
        
        Args:
            node_id: Node ID
            
        Returns:
            Node or None if not found
        """
        for node in self.nodes:
            if node.id == node_id:
                return node
        return None
    
    def find_nodes(self, type: Optional[str] = None, case: Optional[str] = None) -> List[GraphNode]:
        """
        Find nodes by type and case.
        
        Args:
            type: Node type (optional)
            case: Node case (optional)
            
        Returns:
            List of matching nodes
        """
        results = []
        for node in self.nodes:
            if (type is None or node.type == type) and (case is None or node.case == case):
                results.append(node)
        return results
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary representation."""
        return {
            "id": self.id,
            "nodes": [node.to_dict() for node in self.nodes],
            "edges": [edge.to_dict() for edge in self.edges],
            "metadata": self.metadata
        }
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "KnowledgeGraph":
        """
        Create a knowledge graph from dictionary representation.
        
        Args:
            data: Dictionary representation
            
        Returns:
            Knowledge graph
        """
        graph = cls(id=data["id"], metadata=data.get("metadata", {}))
        
        # Add nodes
        for node_data in data.get("nodes", []):
            node = GraphNode(
                id=node_data["id"],
                type=node_data["type"],
                text=node_data["text"],
                case=node_data.get("case"),
                confidence=node_data.get("confidence", 1.0),
                metadata=node_data.get("metadata", {})
            )
            graph.nodes.append(node)
        
        # Add edges
        for edge_data in data.get("edges", []):
            edge = GraphEdge(
                id=edge_data["id"],
                source=edge_data["source"],
                target=edge_data["target"],
                type=edge_data["type"],
                confidence=edge_data.get("confidence", 1.0),
                metadata=edge_data.get("metadata", {})
            )
            graph.edges.append(edge)
        
        return graph


class GraphAssembler:
    """
    Knowledge graph assembler for LEXICON.
    
    Assembles processed segments into a structured knowledge graph:
    - Entity nodes (NOM, ACC, etc.)
    - Claim nodes
    - Relation edges
    - Context nodes
    """
    
    def __init__(self, openrouter: OpenRouterClient, config: LexiconConfig):
        """
        Initialize the graph assembler.
        
        Args:
            openrouter: OpenRouter client for LLM operations
            config: LEXICON configuration
        """
        self.openrouter = openrouter
        self.config = config
        self.logger = get_logger("graph.assembler")
        
        # Initialize entity linker
        self.entity_linker = EntityLinker(self.openrouter, self.config)
        
        # Track performance metrics
        self.metrics = {
            "segments_processed": 0,
            "nodes_created": 0,
            "edges_created": 0,
            "entity_links": 0,
            "relation_extractions": 0,
            "total_duration": 0.0
        }
    
    def build_graph(self, segments: List[ParaphrasedSegment]) -> Dict[str, Any]:
        """
        Build a knowledge graph from processed segments.
        
        Args:
            segments: List of paraphrased segments
            
        Returns:
            Dictionary representation of the knowledge graph
        """
        start_time = time.time()
        self.logger.info(f"Building knowledge graph for {len(segments)} segments")
        
        # Create a new graph
        graph_id = generate_cid("graph", str(time.time()))
        graph = KnowledgeGraph(
            id=graph_id,
            metadata={
                "timestamp": datetime.now().isoformat(),
                "segment_count": len(segments)
            }
        )
        
        # Track processed entities to avoid duplicates
        entity_map = {}  # text -> node_id
        
        # Process each segment
        for i, segment in enumerate(segments):
            try:
                self._process_segment(segment, graph, entity_map)
                
                # Log progress for large batches
                if len(segments) > 10 and (i + 1) % 10 == 0:
                    self.logger.info(
                        f"Processed {i + 1}/{len(segments)} segments, "
                        f"{len(graph.nodes)} nodes, {len(graph.edges)} edges"
                    )
                
            except Exception as e:
                self.logger.error(f"Failed to process segment {segment.segment_id}: {str(e)}")
        
        # Add links between related entities
        try:
            self._link_entities(graph)
        except Exception as e:
            self.logger.error(f"Entity linking failed: {str(e)}")
        
        # Add graph metadata
        duration = time.time() - start_time
        graph.metadata.update({
            "duration_seconds": duration,
            "node_count": len(graph.nodes),
            "edge_count": len(graph.edges)
        })
        
        # Update metrics
        self.metrics["segments_processed"] += len(segments)
        self.metrics["nodes_created"] += len(graph.nodes)
        self.metrics["edges_created"] += len(graph.edges)
        self.metrics["total_duration"] += duration
        
        self.logger.info(
            f"Graph building completed: {len(graph.nodes)} nodes, {len(graph.edges)} edges "
            f"in {duration:.2f}s"
        )
        
        return graph.to_dict()
    
    def _process_segment(self, segment: ParaphrasedSegment, graph: KnowledgeGraph, 
                        entity_map: Dict[str, str]) -> None:
        """
        Process a segment and add its contents to the graph.
        
        Args:
            segment: Processed segment
            graph: Knowledge graph to update
            entity_map: Map of entity text to node ID
        """
        # Create segment node (context)
        segment_node = GraphNode(
            id=segment.segment_id,
            type="segment",
            text=segment.text,
            case="locative",
            confidence=1.0,
            metadata={
                "speaker": segment.speaker,
                "timestamp": segment.timestamp
            }
        )
        graph.add_node(segment_node)
        
        # Process entities by case
        self._add_case_entities(segment, "nominative", "entity", graph, entity_map, segment_node.id)
        self._add_case_entities(segment, "accusative", "claim", graph, entity_map, segment_node.id)
        self._add_case_entities(segment, "genitive", "source", graph, entity_map, segment_node.id)
        self._add_case_entities(segment, "dative", "recipient", graph, entity_map, segment_node.id)
        self._add_case_entities(segment, "locative", "context", graph, entity_map, segment_node.id)
        self._add_case_entities(segment, "instrumental", "tool", graph, entity_map, segment_node.id)
        self._add_case_entities(segment, "ablative", "origin", graph, entity_map, segment_node.id)
        self._add_case_entities(segment, "vocative", "address", graph, entity_map, segment_node.id)
        
        # Add paraphrases as related nodes
        for i, paraphrase in enumerate(segment.paraphrases):
            paraphrase_id = f"{segment.segment_id}_p{i+1}"
            paraphrase_node = GraphNode(
                id=paraphrase_id,
                type="paraphrase",
                text=paraphrase["text"],
                case="genitive",  # Paraphrases are related by possession/source
                confidence=paraphrase.get("quality", 0.7),
                metadata={
                    "original_segment": segment.segment_id,
                    "paraphrase_index": i + 1
                }
            )
            graph.add_node(paraphrase_node)
            
            # Connect paraphrase to segment
            paraphrase_edge = GraphEdge(
                id=f"{paraphrase_id}_edge",
                source=segment.segment_id,
                target=paraphrase_id,
                type="has_paraphrase",
                confidence=paraphrase.get("quality", 0.7)
            )
            graph.add_edge(paraphrase_edge)
    
    def _add_case_entities(self, segment: ParaphrasedSegment, case: str, node_type: str,
                         graph: KnowledgeGraph, entity_map: Dict[str, str], segment_id: str) -> None:
        """
        Add entities of a specific case to the graph.
        
        Args:
            segment: Processed segment
            case: Case name
            node_type: Node type
            graph: Knowledge graph to update
            entity_map: Map of entity text to node ID
            segment_id: Parent segment node ID
        """
        entities = getattr(segment, case)
        
        for entity in entities:
            entity_text = entity.get("text", "").strip()
            if not entity_text:
                continue
                
            # Generate a consistent ID for this entity
            entity_key = f"{entity_text.lower()}:{node_type}"
            
            # Check if we've seen this entity before
            if entity_key in entity_map:
                # Reuse existing node ID
                entity_id = entity_map[entity_key]
            else:
                # Create a new node
                entity_id = generate_cid(node_type, entity_text)
                entity_node = GraphNode(
                    id=entity_id,
                    type=node_type,
                    text=entity_text,
                    case=case,
                    confidence=entity.get("confidence", 0.7),
                    metadata={
                        "source": entity.get("source", "unknown"),
                        "rule": entity.get("rule", "")
                    }
                )
                graph.add_node(entity_node)
                entity_map[entity_key] = entity_id
            
            # Connect entity to segment
            edge_type = f"has_{node_type}"
            edge_id = f"{segment_id}_{entity_id}_edge"
            
            edge = GraphEdge(
                id=edge_id,
                source=segment_id,
                target=entity_id,
                type=edge_type,
                confidence=entity.get("confidence", 0.7)
            )
            graph.add_edge(edge)
    
    def _link_entities(self, graph: KnowledgeGraph) -> None:
        """
        Create links between related entities in the graph.
        
        Args:
            graph: Knowledge graph to update
        """
        # Find entity nodes
        entity_nodes = graph.find_nodes(type="entity")
        claim_nodes = graph.find_nodes(type="claim")
        
        # Track links to avoid duplicates
        linked_pairs = set()
        
        # Link entities to claims
        for entity in entity_nodes:
            for claim in claim_nodes:
                # Check if entity is mentioned in claim
                if entity.text.lower() in claim.text.lower():
                    pair_key = f"{entity.id}:{claim.id}"
                    if pair_key not in linked_pairs:
                        # Add connection
                        edge_id = f"{entity.id}_{claim.id}_edge"
                        edge = GraphEdge(
                            id=edge_id,
                            source=entity.id,
                            target=claim.id,
                            type="mentioned_in",
                            confidence=0.7,
                            metadata={
                                "relationship": "mention"
                            }
                        )
                        graph.add_edge(edge)
                        linked_pairs.add(pair_key)
                        self.metrics["entity_links"] += 1
        
        # Use entity linker to find additional relationships
        try:
            # Get all entity texts
            entity_texts = [(node.id, node.text) for node in entity_nodes]
            
            if entity_texts:
                # Extract relationships
                relationships = self.entity_linker.extract_relationships(entity_texts)
                
                # Add relationships to graph
                for rel in relationships:
                    source_id = rel["source"]
                    target_id = rel["target"]
                    rel_type = rel["type"]
                    
                    pair_key = f"{source_id}:{target_id}:{rel_type}"
                    if pair_key not in linked_pairs:
                        # Add connection
                        edge_id = f"{source_id}_{target_id}_{rel_type}_edge"
                        edge = GraphEdge(
                            id=edge_id,
                            source=source_id,
                            target=target_id,
                            type=rel_type,
                            confidence=rel.get("confidence", 0.6),
                            metadata=rel.get("metadata", {})
                        )
                        graph.add_edge(edge)
                        linked_pairs.add(pair_key)
                        self.metrics["relation_extractions"] += 1
        
        except Exception as e:
            self.logger.error(f"Relationship extraction failed: {str(e)}")
    
    def get_metrics(self) -> Dict[str, Any]:
        """
        Get performance metrics for the graph assembler.
        
        Returns:
            Dictionary of performance metrics
        """
        # Calculate averages
        if self.metrics["segments_processed"] > 0:
            avg_nodes = self.metrics["nodes_created"] / self.metrics["segments_processed"]
            avg_edges = self.metrics["edges_created"] / self.metrics["segments_processed"]
            avg_time = self.metrics["total_duration"] / self.metrics["segments_processed"]
        else:
            avg_nodes = 0.0
            avg_edges = 0.0
            avg_time = 0.0
            
        return {
            **self.metrics,
            "avg_nodes_per_segment": avg_nodes,
            "avg_edges_per_segment": avg_edges,
            "avg_time_per_segment": avg_time,
        } 