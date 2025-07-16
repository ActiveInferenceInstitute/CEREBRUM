"""
LEXICON Result Analyzer

Provides analysis and reporting functions for LEXICON processing results.
"""

import logging
from pathlib import Path
from typing import Dict, List, Any, Optional

from .graph_visualizer import GraphVisualizer
from .animated_graph import create_graph_animation
from .entity_neighborhood import generate_entity_neighborhood_visualizations


def create_text_analysis_files(result: Dict[str, Any], vis_dir: Path) -> None:
    """
    Create text-based analysis files.
    
    Args:
        result: Processing result
        vis_dir: Visualizations directory
    """
    logger = logging.getLogger("lexicon_analyzer")
    
    # Save graph statistics
    stats_file = vis_dir / "graph_statistics.txt"
    with open(stats_file, 'w') as f:
        f.write(f"Graph Statistics\n")
        f.write(f"===============\n\n")
        f.write(f"Total nodes: {result['stats']['nodes']}\n")
        f.write(f"Total edges: {result['stats']['edges']}\n\n")
        
        # Node types
        node_types = {}
        for node in result["graph"]["nodes"]:
            node_type = node.get("type", "unknown")
            node_types[node_type] = node_types.get(node_type, 0) + 1
        
        f.write("Node Types:\n")
        for node_type, count in node_types.items():
            f.write(f"  - {node_type}: {count}\n")
        f.write("\n")
        
        # Edge types
        edge_types = {}
        for edge in result["graph"]["edges"]:
            edge_type = edge.get("type", "unknown")
            edge_types[edge_type] = edge_types.get(edge_type, 0) + 1
        
        f.write("Edge Types:\n")
        for edge_type, count in edge_types.items():
            f.write(f"  - {edge_type}: {count}\n")
        f.write("\n")
        
        # Cases
        cases = {}
        for node in result["graph"]["nodes"]:
            case = node.get("case", "none")
            cases[case] = cases.get(case, 0) + 1
        
        f.write("Cases:\n")
        for case, count in cases.items():
            f.write(f"  - {case}: {count}\n")
            
    logger.info(f"Generated statistics file: {stats_file}")
    
    # Save entity list with enhanced context
    entities_file = vis_dir / "entities.txt"
    with open(entities_file, 'w') as f:
        f.write(f"Entities Analysis\n")
        f.write(f"================\n\n")
        
        entities = [n for n in result["graph"]["nodes"] if n.get("type") == "entity"]
        sorted_entities = sorted(entities, key=lambda e: e.get("confidence", 0), reverse=True)
        
        # Categorize entities
        entity_categories = {}
        for entity in sorted_entities:
            category = entity.get("entity_type", "Uncategorized")
            if category not in entity_categories:
                entity_categories[category] = []
            entity_categories[category].append(entity)
        
        f.write("Entity Categories:\n")
        for category, category_entities in entity_categories.items():
            f.write(f"\n{category}:\n")
            for i, entity in enumerate(category_entities):
                entity_text = entity.get('label', entity.get('text', 'Unknown'))
                f.write(f"{i+1}. {entity_text}\n")
                f.write(f"   - Case: {entity.get('case', 'none')}\n")
                f.write(f"   - Confidence: {entity.get('confidence', 'N/A')}\n")
                
                # Find related claims
                related_claims = []
                for edge in result["graph"]["edges"]:
                    if edge.get("source") == entity.get("id") and edge.get("type") == "relates_to":
                        related_claim = next((n for n in result["graph"]["nodes"] if n.get("id") == edge.get("target") and n.get("type") == "claim"), None)
                        if related_claim:
                            related_claims.append(related_claim)
                
                if related_claims:
                    f.write("   - Related Claims:\n")
                    for claim in related_claims:
                        claim_text = claim.get('label', claim.get('text', 'Unknown'))
                        f.write(f"     * {claim_text} (Confidence: {claim.get('confidence', 'N/A')})\n")
                f.write("\n")
                
    logger.info(f"Generated entities analysis file: {entities_file}")
    
    # Save claims list with enhanced analysis
    claims_file = vis_dir / "claims.txt"
    with open(claims_file, 'w') as f:
        f.write(f"Claims Analysis\n")
        f.write(f"===============\n\n")
        
        claims = [n for n in result["graph"]["nodes"] if n.get("type") == "claim"]
        sorted_claims = sorted(claims, key=lambda c: c.get("confidence", 0), reverse=True)
        
        # Categorize claims by polarity and case
        claim_categories = {
            "Positive": [],
            "Negative": [],
            "Neutral": []
        }
        
        for claim in sorted_claims:
            polarity = claim.get("polarity", "Neutral").lower()
            if "negative" in polarity:
                claim_categories["Negative"].append(claim)
            elif "positive" in polarity:
                claim_categories["Positive"].append(claim)
            else:
                claim_categories["Neutral"].append(claim)
        
        for category, category_claims in claim_categories.items():
            if not category_claims:
                continue
            
            f.write(f"\n{category} Claims:\n")
            for i, claim in enumerate(category_claims):
                claim_text = claim.get('label', claim.get('text', 'Unknown'))
                f.write(f"{i+1}. {claim_text}\n")
                f.write(f"   - Case: {claim.get('case', 'none')}\n")
                f.write(f"   - Confidence: {claim.get('confidence', 'N/A')}\n")
                
                # Find related entities
                related_entities = []
                for edge in result["graph"]["edges"]:
                    if edge.get("source") == claim.get("id") and edge.get("type") == "relates_to":
                        related_entity = next((n for n in result["graph"]["nodes"] if n.get("id") == edge.get("target") and n.get("type") == "entity"), None)
                        if related_entity:
                            related_entities.append(related_entity)
                
                if related_entities:
                    f.write("   - Related Entities:\n")
                    for entity in related_entities:
                        entity_text = entity.get('label', entity.get('text', 'Unknown'))
                        f.write(f"     * {entity_text} (Type: {entity.get('entity_type', 'Unknown')})\n")
                f.write("\n")
    
    logger.info(f"Generated claims analysis file: {claims_file}")


def create_all_visualizations(result: Dict[str, Any], output_dir: Path) -> None:
    """
    Create comprehensive visualizations of the graph.
    
    Args:
        result: Processing result
        output_dir: Output directory
    """
    # Create visualizations directory
    vis_dir = output_dir / "visualizations"
    vis_dir.mkdir(parents=True, exist_ok=True)
    logger = logging.getLogger("lexicon_visualizer")
    logger.info(f"Creating visualizations in: {vis_dir}")
    
    # Create text-based analysis files
    create_text_analysis_files(result, vis_dir)
    
    # Create graphical visualizations
    try:
        # Create static visualizations
        visualizer = GraphVisualizer()
        visualization_paths = visualizer.visualize_graph(result["graph"], vis_dir)
        
        # Log each visualization file with its size
        if visualization_paths:
            for viz_path in visualization_paths:
                if viz_path and viz_path.exists():
                    file_size = viz_path.stat().st_size / 1024  # Size in KB
                    logger.info(f"Generated visualization: {viz_path} ({file_size:.2f} KB)")
        
        # Create animated visualizations
        animation_paths = create_graph_animation(result["graph"], vis_dir)
        
        # Log each animation file with its size
        if animation_paths:
            for anim_path in animation_paths:
                if anim_path and anim_path.exists():
                    file_size = anim_path.stat().st_size / 1024  # Size in KB
                    logger.info(f"Generated animation: {anim_path} ({file_size:.2f} KB)")
        
    except ImportError as e:
        logger.error(f"Could not create graphical visualizations: {str(e)}")
        logger.warning("Install matplotlib, networkx, numpy, and imageio for graphical visualizations")

    # Generate entity neighborhood visualizations
    try:
        neighborhood_vis_dir = vis_dir / "entity_neighborhoods"
        neighborhood_vis_dir.mkdir(parents=True, exist_ok=True)
        logger.info(f"Creating entity neighborhood visualizations in: {neighborhood_vis_dir}")
        
        # Generate entity neighborhood visualizations
        entity_neighborhood_paths = generate_entity_neighborhood_visualizations(
            result["graph"], 
            neighborhood_vis_dir
        )
        
        # Log each entity neighborhood visualization with its size
        for path in entity_neighborhood_paths:
            if path and path.exists():
                file_size = path.stat().st_size / 1024  # Size in KB
                logger.info(f"Generated entity neighborhood visualization: {path} ({file_size:.2f} KB)")
                
        logger.info(f"Created {len(entity_neighborhood_paths)} entity neighborhood visualizations")
    except Exception as e:
        logger.error(f"Could not create entity neighborhood visualizations: {str(e)}")
        logger.warning("Ensure networkx, matplotlib, and other dependencies are installed")


def analyze_results(result: Dict[str, Any], output_dir: Path, logger: logging.Logger) -> None:
    """
    Analyze and display results.
    
    Args:
        result: Processing result
        output_dir: Output directory
        logger: Logger instance
    """
    # Log basic stats
    logger.info(f"Graph contains {result['stats']['nodes']} nodes and {result['stats']['edges']} edges")
    
    # Extract entities and claims
    entities = [n for n in result["graph"]["nodes"] if n.get("type") == "entity"]
    claims = [n for n in result["graph"]["nodes"] if n.get("type") == "claim"]
    
    # Log entity and claim counts
    logger.info(f"Found {len(entities)} entities and {len(claims)} claims")
    
    # Print top entities
    print("\nTop Entities:")
    for i, entity in enumerate(sorted(entities, key=lambda e: e.get("confidence", 0), reverse=True)[:10]):
        entity_text = entity.get('label', entity.get('text', 'Unknown'))
        print(f"{i+1}. {entity_text} (Case: {entity.get('case', 'none')}, Confidence: {entity.get('confidence', 'N/A')})")
    
    # Print top claims
    print("\nTop Claims:")
    for i, claim in enumerate(sorted(claims, key=lambda c: c.get("confidence", 0), reverse=True)[:10]):
        claim_text = claim.get('label', claim.get('text', 'Unknown'))
        print(f"{i+1}. {claim_text} (Case: {claim.get('case', 'none')}, Confidence: {claim.get('confidence', 'N/A')})")
    
    # Ensure output directory is a Path
    if not isinstance(output_dir, Path):
        output_dir = Path(output_dir)
    
    # Create visualizations with enhanced error handling
    try:
        create_all_visualizations(result, output_dir)
    except Exception as e:
        logger.error(f"Error during visualization: {e}")
        import traceback
        logger.debug(traceback.format_exc())
    
    # Print output location
    print(f"\nResults saved to: {output_dir}")
    print(f"Visualizations available in: {output_dir / 'visualizations'}")


def print_result_summary(result: Dict[str, Any]) -> None:
    """
    Print a summary of processing results to console.
    
    Args:
        result: Processing result
    """
    # Extract entities and claims
    entities = [n for n in result["graph"]["nodes"] if n.get("type") == "entity"]
    claims = [n for n in result["graph"]["nodes"] if n.get("type") == "claim"]
    
    # Print key entities
    print("\nKey entities identified:")
    for i, entity in enumerate(sorted(entities, key=lambda e: e.get("confidence", 0), reverse=True)[:10]):
        entity_text = entity.get('label', entity.get('text', 'Unknown'))
        print(f"{i+1}. {entity_text} (Case: {entity.get('case', 'none')}, Confidence: {entity.get('confidence', 'N/A')})")
    
    # Print key claims
    print("\nKey claims identified:")
    for i, claim in enumerate(sorted(claims, key=lambda c: c.get("confidence", 0), reverse=True)[:10]):
        claim_text = claim.get('label', claim.get('text', 'Unknown'))
        print(f"{i+1}. {claim_text} (Case: {claim.get('case', 'none')}, Confidence: {claim.get('confidence', 'N/A')})") 