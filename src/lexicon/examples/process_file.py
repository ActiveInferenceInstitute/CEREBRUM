#!/usr/bin/env python3
"""
LEXICON File Processing Example

This script demonstrates how to use LEXICON to process a file,
either text or audio, and generate a knowledge graph.
"""

import os
import sys
import json
import argparse
import time
import shutil
from pathlib import Path
import logging
from datetime import datetime

# Add parent directory to path to allow running this script directly
sys.path.insert(0, str(Path(__file__).resolve().parent.parent.parent.parent))

from src.lexicon.core.engine import LexiconEngine
from src.lexicon.core.config import LexiconConfig

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
    log_file = log_dir / f"file_process_{timestamp}.log"
    
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            logging.StreamHandler(),
            logging.FileHandler(log_file)
        ]
    )
    
    logger = logging.getLogger("lexicon_file_example")
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
    output_dir = base_output_dir / f"file_process_{timestamp}"
    output_dir.mkdir(parents=True, exist_ok=True)
    
    return output_dir

def create_sample_file(output_dir):
    """
    Create a sample transcript file for demonstration.
    
    Args:
        output_dir: Output directory
        
    Returns:
        Path to sample file
    """
    sample_dir = output_dir / "input"
    sample_dir.mkdir(parents=True, exist_ok=True)
    
    sample_file = sample_dir / "sample_transcript.txt"
    
    sample_content = """
INTERVIEWER: Welcome to our podcast. Today we're discussing cognitive modeling with Dr. Emily Chen.

DR. CHEN: Thank you for having me. I'm excited to talk about our work on the CEREBRUM project.

INTERVIEWER: Could you explain what CEREBRUM stands for and what it aims to achieve?

DR. CHEN: CEREBRUM stands for Case-Enabled Reasoning Engine with Bayesian Representations for Unified Modeling. 
It's a framework that integrates case-based reasoning with Bayesian networks to create more human-like cognitive models.

INTERVIEWER: That sounds fascinating. How does the LEXICON component fit into this?

DR. CHEN: LEXICON, which stands for Linguistic Entity eXtraction and Iterative Case-Oriented Navigation, 
is a critical module that transforms unstructured language into structured knowledge graphs. 
It applies our 8-case linguistic system to categorize entities and relationships.

INTERVIEWER: Can you give an example of how this works in practice?

DR. CHEN: Sure. When processing a transcript like this one, LEXICON identifies entities like "CEREBRUM project" 
as nominative cases (agents), actions or claims as accusative cases, and contextual information like 
"at the AI Conference" as locative cases. This creates a rich semantic network that's much more useful 
than traditional keyword extraction.

INTERVIEWER: What kind of applications do you see for this technology?

DR. CHEN: The applications are quite broad. From academic research analysis to business intelligence, 
anywhere there's a need to process large volumes of unstructured text and extract meaningful insights.
"""
    
    with open(sample_file, 'w') as f:
        f.write(sample_content)
    
    return sample_file

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
    """Run the LEXICON file processing example."""
    parser = argparse.ArgumentParser(description="Process a file with LEXICON")
    parser.add_argument("--file", help="Path to file to process (creates sample if not provided)")
    parser.add_argument("--model", default="anthropic/claude-3.5-sonnet", help="OpenRouter model to use")
    parser.add_argument("--log-level", default="INFO", help="Logging level", 
                        choices=["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"])
    parser.add_argument("--format", help="Input format", 
                        choices=["text", "podcast:vtt", "meeting:transcript", "twitter:thread"])
    args = parser.parse_args()
    
    # Create output directory
    output_dir = create_output_dir()
    
    # Set up logging
    logger = setup_logging(output_dir)
    logger.info("Starting LEXICON file processing example")
    
    # Create metadata
    metadata = {
        "timestamp": datetime.now().strftime("%Y-%m-%dT%H:%M:%S"),
        "model": args.model,
        "format": args.format,
        "log_level": args.log_level
    }
    
    # Use provided file or create a sample
    input_dir = output_dir / "input"
    input_dir.mkdir(parents=True, exist_ok=True)
    
    if args.file:
        file_path = Path(args.file)
        if not file_path.exists():
            logger.error(f"File not found: {file_path}")
            sys.exit(1)
        
        # Copy file to input directory
        input_file = input_dir / file_path.name
        shutil.copy(file_path, input_file)
        logger.info(f"Copied input file to {input_file}")
        
        metadata["input_file"] = file_path.name
        file_path = input_file
    else:
        # Create sample file
        file_path = create_sample_file(output_dir)
        logger.info(f"Created sample transcript file at {file_path}")
        metadata["input_file"] = "sample_transcript.txt"
    
    # Initialize LEXICON with custom configuration
    config = LexiconConfig(
        output_dir=output_dir,
        cache_dir=output_dir / "cache",
        log_level=args.log_level,
        default_model=args.model
    )
    
    try:
        # Initialize the LEXICON engine
        logger.info("Initializing LEXICON engine")
        start_time = time.time()
        engine = LexiconEngine(config)
        
        # Process the file
        logger.info(f"Processing file: {file_path}")
        process_metadata = {"format": args.format, "source": "file_example"} if args.format else {"source": "file_example"}
        result = engine.process_file(file_path, process_metadata)
        
        # Calculate processing time
        processing_time = time.time() - start_time
        metadata["processing_time"] = processing_time
        metadata["stats"] = result["stats"]
        logger.info(f"Processing completed in {processing_time:.2f} seconds")
        
        # Save results to a more readable format
        output_path = output_dir / "file_result.json"
        with open(output_path, 'w') as f:
            json.dump(result, f, indent=2)
        
        logger.info(f"Results saved to {output_path}")
        
        # Save metadata
        save_metadata(metadata, output_dir)
        
        # Create visualizations
        visualize_results(result, output_dir)
        
        # Print some interesting findings
        print("\nKey entities identified:")
        entities = [n for n in result["graph"]["nodes"] if n.get("type") == "entity"]
        for i, entity in enumerate(sorted(entities, key=lambda e: e.get("confidence", 0), reverse=True)[:10]):
            print(f"{i+1}. {entity['text']} (Case: {entity.get('case', 'none')}, Confidence: {entity.get('confidence', 'N/A')})")
        
        print("\nKey claims identified:")
        claims = [n for n in result["graph"]["nodes"] if n.get("type") == "claim"]
        for i, claim in enumerate(sorted(claims, key=lambda c: c.get("confidence", 0), reverse=True)[:10]):
            print(f"{i+1}. {claim['text']} (Case: {claim.get('case', 'none')}, Confidence: {claim.get('confidence', 'N/A')})")
        
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
    
    logger.info("LEXICON file processing example completed successfully")

if __name__ == "__main__":
    import traceback  # Import here to avoid issues if not used
    main() 