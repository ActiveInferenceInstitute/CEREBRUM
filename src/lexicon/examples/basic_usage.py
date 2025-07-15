#!/usr/bin/env python3
"""
LEXICON Basic Usage Example

This script demonstrates the basic usage of the LEXICON system
by processing a sample text through the complete pipeline.
"""

import os
import sys
import json
import time
from pathlib import Path
import logging
from datetime import datetime

# Add parent directory to path to allow running this script directly
sys.path.insert(0, str(Path(__file__).resolve().parent.parent.parent.parent))

from src.lexicon.core.engine import LexiconEngine
from src.lexicon.core.config import LexiconConfig

# Sample text for processing
SAMPLE_TEXT = """
The CEREBRUM project integrates case-based reasoning with Bayesian representations for unified cognitive modeling. 
Dr. Emily Chen, the lead researcher, presented her findings at the AI Conference in Boston last month.
The team demonstrated how their system can process natural language and generate structured knowledge graphs.
Using the LEXICON module, they transformed unstructured transcripts into case-declined semantic networks.
The results showed a 92% improvement in information retrieval compared to traditional methods.
"""

def setup_logging(output_dir):
    """
    Set up logging configuration.
    
    Args:
        output_dir: Output directory for log files
        
    Returns:
        Logger instance
    """
    # Create logs directory
    log_dir = output_dir / "logs"
    log_dir.mkdir(parents=True, exist_ok=True)
    
    # Create log file with timestamp
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    log_file = log_dir / f"example_{timestamp}.log"
    
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            logging.StreamHandler(),
            logging.FileHandler(log_file)
        ]
    )
    
    logger = logging.getLogger("lexicon_example")
    logger.info(f"Logging to {log_file}")
    
    return logger

def create_output_dir():
    """
    Create timestamped output directory.
    
    Returns:
        Path to output directory
    """
    # Create base output directory
    base_output_dir = Path("output/lexicon")
    base_output_dir.mkdir(parents=True, exist_ok=True)
    
    # Create timestamped subdirectory
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    output_dir = base_output_dir / f"example_{timestamp}"
    output_dir.mkdir(parents=True, exist_ok=True)
    
    return output_dir

def save_input_text(text, output_dir):
    """
    Save input text to file.
    
    Args:
        text: Input text
        output_dir: Output directory
        
    Returns:
        Path to saved file
    """
    input_file = output_dir / "input.txt"
    with open(input_file, 'w') as f:
        f.write(text)
    
    return input_file

def save_metadata(metadata, output_dir):
    """
    Save metadata to file.
    
    Args:
        metadata: Run metadata
        output_dir: Output directory
    """
    metadata_file = output_dir / "metadata.json"
    with open(metadata_file, 'w') as f:
        json.dump(metadata, f, indent=2)

def visualize_results(result, output_dir):
    """
    Create visualizations of the results.
    
    Args:
        result: Processing result
        output_dir: Output directory
    """
    # Create visualizations directory
    vis_dir = output_dir / "visualizations"
    vis_dir.mkdir(parents=True, exist_ok=True)
    
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
    
    # Save entity list
    entities_file = vis_dir / "entities.txt"
    with open(entities_file, 'w') as f:
        f.write(f"Entities\n")
        f.write(f"========\n\n")
        
        entities = [n for n in result["graph"]["nodes"] if n.get("type") == "entity"]
        sorted_entities = sorted(entities, key=lambda e: e.get("confidence", 0), reverse=True)
        
        for i, entity in enumerate(sorted_entities):
            f.write(f"{i+1}. {entity['text']}\n")
            f.write(f"   - Type: {entity.get('entity_type', 'Unknown')}\n")
            f.write(f"   - Case: {entity.get('case', 'none')}\n")
            f.write(f"   - Confidence: {entity.get('confidence', 'N/A')}\n\n")
    
    # Save claims list
    claims_file = vis_dir / "claims.txt"
    with open(claims_file, 'w') as f:
        f.write(f"Claims\n")
        f.write(f"======\n\n")
        
        claims = [n for n in result["graph"]["nodes"] if n.get("type") == "claim"]
        sorted_claims = sorted(claims, key=lambda c: c.get("confidence", 0), reverse=True)
        
        for i, claim in enumerate(sorted_claims):
            f.write(f"{i+1}. {claim['text']}\n")
            f.write(f"   - Polarity: {claim.get('polarity', 'Unknown')}\n")
            f.write(f"   - Case: {claim.get('case', 'none')}\n")
            f.write(f"   - Confidence: {claim.get('confidence', 'N/A')}\n\n")
    
    # Create graphical visualizations
    try:
        from src.lexicon.visualization.graph_visualizer import GraphVisualizer
        from src.lexicon.visualization.animated_graph import create_graph_animation
        
        # Create static visualizations
        visualizer = GraphVisualizer()
        visualizer.visualize_graph(result["graph"], output_dir)
        
        # Create animated visualizations
        create_graph_animation(result["graph"], output_dir)
        
        logging.info("Created graphical visualizations")
    except ImportError as e:
        logging.warning(f"Could not create graphical visualizations: {str(e)}")
        logging.warning("Install matplotlib, networkx, numpy, and imageio for graphical visualizations")

def main():
    """Run the LEXICON example."""
    # Create output directory
    output_dir = create_output_dir()
    
    # Set up logging
    logger = setup_logging(output_dir)
    logger.info("Starting LEXICON basic usage example")
    
    # Save input text
    input_file = save_input_text(SAMPLE_TEXT, output_dir)
    logger.info(f"Saved input text to {input_file}")
    
    # Create metadata
    metadata = {
        "timestamp": datetime.now().isoformat(),
        "example_type": "basic_usage",
        "input_text_length": len(SAMPLE_TEXT)
    }
    
    # Initialize LEXICON with custom configuration
    config = LexiconConfig(
        output_dir=output_dir,
        cache_dir=output_dir / "cache",
        log_level="INFO"
    )
    
    try:
        # Initialize the LEXICON engine
        logger.info("Initializing LEXICON engine")
        start_time = time.time()
        engine = LexiconEngine(config)
        
        # Process the sample text
        logger.info("Processing sample text")
        result = engine.process_text(SAMPLE_TEXT, {"source": "example"})
        
        # Calculate processing time
        processing_time = time.time() - start_time
        metadata["processing_time"] = processing_time
        metadata["stats"] = result["stats"]
        logger.info(f"Processing completed in {processing_time:.2f} seconds")
        
        # Save results to a more readable format
        output_path = output_dir / "example_result.json"
        with open(output_path, 'w') as f:
            json.dump(result, f, indent=2)
        
        logger.info(f"Results saved to {output_path}")
        
        # Save metadata
        save_metadata(metadata, output_dir)
        
        # Create visualizations
        visualize_results(result, output_dir)
        
        # Print a sample of the graph
        print("\nSample of generated graph nodes:")
        for i, node in enumerate(result["graph"]["nodes"][:5]):
            print(f"{i+1}. [{node['type']}] {node['text']} (Case: {node.get('case', 'none')})")
        
        print("\nSample of generated graph edges:")
        for i, edge in enumerate(result["graph"]["edges"][:5]):
            print(f"{i+1}. {edge['source']} --[{edge['type']}]--> {edge['target']}")
        
        # Print output location
        print(f"\nResults saved to: {output_dir}")
        print(f"Visualizations available in: {output_dir / 'visualizations'}")
        
    except Exception as e:
        logger.error(f"Error running LEXICON: {str(e)}", exc_info=True)
        
        # Save error information
        error_info = {
            "error": str(e),
            "traceback": traceback.format_exc() if 'traceback' in sys.modules else None
        }
        error_path = output_dir / "error.json"
        with open(error_path, 'w') as f:
            json.dump(error_info, f, indent=2)
        
        sys.exit(1)
    
    logger.info("LEXICON example completed successfully")

if __name__ == "__main__":
    import traceback  # Import here to avoid issues if not used
    main() 