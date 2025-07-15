#!/usr/bin/env python3
"""
LEXICON - Linguistic Entity eXtraction for Integrated CONtext analysis

Run script for LEXICON processing pipeline.
"""

import os
import sys
import json
import time
import uuid
import logging
import argparse
import pprint
import shutil
import traceback
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Any, Optional, Union
from dotenv import load_dotenv

# Add parent directory to path to allow running this script directly
sys.path.insert(0, str(Path(__file__).resolve().parent.parent.parent))

from src.lexicon.core.engine import LexiconEngine
from src.lexicon.core.config import LexiconConfig
from src.lexicon.visualization.entity_neighborhood import generate_entity_neighborhood_visualizations

# Load environment variables from top-level .env file
env_path = Path(__file__).resolve().parent.parent.parent / '.env'
load_dotenv(dotenv_path=env_path)

class LexiconConfig:
    """Configuration for LEXICON engine."""
    
    def __init__(self, output_dir=None, cache_dir=None, log_level="INFO", default_model="anthropic/claude-3.5-sonnet"):
        """
        Initialize LEXICON configuration.
        
        Args:
            output_dir: Output directory
            cache_dir: Cache directory
            log_level: Logging level
            default_model: Default model to use
        """
        self.output_dir = output_dir
        self.cache_dir = cache_dir
        self.log_level = log_level
        self.default_model = default_model
        self.openrouter_api_key = None  # Will use environment variable
        self.timeout_seconds = 90.0
        self.base_api_url = "https://openrouter.ai/api/v1"
        self.enable_detailed_logging = True
        self.enable_caching = True
        self.max_retries = 3
        self.retry_delay = 2.0

def setup_logging(level, output_dir):
    """
    Set up logging configuration.
    
    Args:
        level: Logging level
        output_dir: Output directory for log files
        
    Returns:
        Logger instance
    """
    # Create logs directory
    log_dir = output_dir / "logs"
    log_dir.mkdir(parents=True, exist_ok=True)
    
    # Create log file with timestamp
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    log_file = log_dir / f"lexicon_run_{timestamp}.log"
    
    logging.basicConfig(
        level=getattr(logging, level),
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            logging.StreamHandler(),
            logging.FileHandler(log_file)
        ]
    )
    
    logger = logging.getLogger("lexicon_runner")
    logger.info(f"Logging to {log_file}")
    
    return logger

def create_output_dir(input_name=None, is_batch=False):
    """
    Create timestamped output directory.
    
    Args:
        input_name: Optional name of input file to include in directory name
        is_batch: Whether this is a batch processing run
        
    Returns:
        Path to output directory
    """
    # Always use project root output directory (never system root /output)
    base_output_dir = Path(__file__).resolve().parent.parent.parent / "output"
    base_output_dir.mkdir(parents=True, exist_ok=True)
    logging.info(f"Using project output directory: {base_output_dir}")
    
    # Create timestamped subdirectory directly under output (not under /lexicon/)
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    
    if is_batch:
        # Use a single directory for batch processing
        output_dir = base_output_dir / f"lexicon_batch_{timestamp}"
    elif input_name:
        # Clean up input name for directory name
        input_name = Path(input_name).stem
        output_dir = base_output_dir / f"lexicon_{input_name}_{timestamp}"
    else:
        # Default directory name with timestamp
        output_dir = base_output_dir / f"lexicon_{timestamp}"
    
    output_dir.mkdir(parents=True, exist_ok=True)
    logging.info(f"Created output directory: {output_dir}")
    
    # Create standard subdirectories
    (output_dir / "logs").mkdir(exist_ok=True)
    (output_dir / "input").mkdir(exist_ok=True)
    (output_dir / "cache").mkdir(exist_ok=True)
    (output_dir / "visualizations").mkdir(exist_ok=True)
    
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
    input_dir = output_dir / "input"
    input_file = input_dir / "input.txt"
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

def visualize_graph(result, output_dir):
    """
    Create detailed visualizations of the graph.
    
    Args:
        result: Processing result
        output_dir: Output directory
    """
    # Create visualizations directory
    vis_dir = output_dir / "visualizations"
    vis_dir.mkdir(parents=True, exist_ok=True)
    logger = logging.getLogger("lexicon_visualizer")
    logger.info(f"Creating visualizations in: {vis_dir}")
    
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
    
    # Create graphical visualizations
    try:
        from src.lexicon.visualization.graph_visualizer import GraphVisualizer
        from src.lexicon.visualization.animated_graph import create_graph_animation
        
        # Create static visualizations
        visualizer = GraphVisualizer()
        visualization_paths = visualizer.visualize_graph(result["graph"], vis_dir)
        
        # Log each visualization file with its size
        if visualization_paths:
            for viz_path in visualization_paths:
                if viz_path and viz_path.exists():
                    file_size = os.path.getsize(viz_path) / 1024  # Size in KB
                    logger.info(f"Generated visualization: {viz_path} ({file_size:.2f} KB)")
        
        # Create animated visualizations
        animation_paths = create_graph_animation(result["graph"], vis_dir)
        
        # Log each animation file with its size
        if animation_paths:
            for anim_path in animation_paths:
                if anim_path and anim_path.exists():
                    file_size = os.path.getsize(anim_path) / 1024  # Size in KB
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
                file_size = os.path.getsize(path) / 1024  # Size in KB
                logger.info(f"Generated entity neighborhood visualization: {path} ({file_size:.2f} KB)")
                
        logger.info(f"Created {len(entity_neighborhood_paths)} entity neighborhood visualizations")
    except Exception as e:
        logger.error(f"Could not create entity neighborhood visualizations: {str(e)}")
        logger.warning("Ensure networkx, matplotlib, and other dependencies are installed")

def analyze_results(result, output_dir, logger):
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
        visualize_graph(result, output_dir)
    except Exception as e:
        logger.error(f"Error during visualization: {e}")
        import traceback
        logger.debug(traceback.format_exc())
    
    # Print output location
    print(f"\nResults saved to: {output_dir}")
    print(f"Visualizations available in: {output_dir / 'visualizations'}")

def process_file(input_file, output_dir, model="anthropic/claude-3.5-sonnet", log_level="INFO"):
    """
    Process a single input file.
    
    Args:
        input_file: Path to input file
        output_dir: Output directory
        model: Model to use
        log_level: Logging level
        
    Returns:
        Processing result
    """
    # Set up logging
    log_dir = output_dir / "logs"
    log_dir.mkdir(parents=True, exist_ok=True)
    log_file = log_dir / "process.log"
    
    logging.basicConfig(
        level=getattr(logging, log_level),
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            logging.StreamHandler(),
            logging.FileHandler(log_file)
        ]
    )
    
    logger = logging.getLogger(f"lexicon_{input_file.stem}")
    logger.info(f"Processing file: {input_file}")
    
    # Copy input file to output directory
    input_copy_path = output_dir / "input" / input_file.name
    with open(input_file, 'r') as src, open(input_copy_path, 'w') as dest:
        content = src.read()
        dest.write(content)
    
    # Determine format type based on filename
    format_type = "text"
    if "podcast" in input_file.name.lower():
        format_type = "podcast:vtt"
    elif "therapy" in input_file.name.lower():
        format_type = "meeting:transcript"
    
    # Initialize configuration
    config = LexiconConfig(
        output_dir=output_dir,
        cache_dir=output_dir / "cache",
        log_level=log_level,
        default_model=model
    )
    
    # Initialize engine
    logger.info("Initializing LEXICON engine")
    engine = LexiconEngine(config)
    
    # Process the file
    with open(input_file, 'r') as f:
        content = f.read()
    
    # Create metadata
    metadata = {
        "format": format_type,
        "session_id": str(uuid.uuid4()),
        "timestamp": datetime.now().isoformat(),
        "file_path": str(input_file),
        "file_name": input_file.name,
        "file_type": input_file.suffix,
        "output_dir": str(output_dir)
    }
    
    # Process the content
    logger.info(f"Processing content with format: {format_type}")
    result = engine.process_text(content, metadata)
    
    # Save result
    result_path = output_dir / "result.json"
    with open(result_path, 'w') as f:
        json.dump(result, f, indent=2)
    
    logger.info(f"Processing complete. Results saved to {result_path}")
    return result

def process_all_inputs(model="anthropic/claude-3.5-sonnet", log_level="INFO"):
    """
    Process all input files in the input directory.
    
    Args:
        model: Model to use
        log_level: Logging level
    """
    input_dir = Path(__file__).resolve().parent / "input"
    if not input_dir.exists():
        print(f"Input directory '{input_dir}' not found. Creating it...")
        input_dir.mkdir(parents=True)
        print(f"Please place input files in the '{input_dir}' directory and run again.")
        return
    
    # Get all markdown files in the input directory
    input_files = list(input_dir.glob("*.md"))
    
    if not input_files:
        print(f"No markdown files found in '{input_dir}' directory.")
        return
    
    # Create base output directory with batch processing flag
    base_output_dir = create_output_dir(is_batch=True)
    
    # Track results for batch summary
    batch_results = {}
    
    # Process each input file
    for input_file in input_files:
        print(f"Processing {input_file.name}...")
        
        # Create subdirectory for this input file
        file_output_dir = base_output_dir / input_file.stem
        file_output_dir.mkdir(parents=True, exist_ok=True)
        
        # Create standard subdirectories for this input
        (file_output_dir / "logs").mkdir(exist_ok=True)
        (file_output_dir / "input").mkdir(exist_ok=True)
        (file_output_dir / "cache").mkdir(exist_ok=True)
        (file_output_dir / "visualizations").mkdir(exist_ok=True)
        
        try:
            # Process the file
            result = process_file(input_file, file_output_dir, model, log_level)
            
            # Add visualization step that was missing in batch processing
            logger = logging.getLogger(f"lexicon_batch_{input_file.stem}")
            analyze_results(result, file_output_dir, logger)
            
            batch_results[input_file.name] = {"status": "success", "result": result}
        except Exception as e:
            print(f"Error processing {input_file.name}: {e}")
            batch_results[input_file.name] = {"status": "error", "error": str(e)}
    
    # Save batch summary
    summary_path = base_output_dir / f"batch_summary_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    with open(summary_path, 'w') as f:
        json.dump(batch_results, f, indent=2)
    
    print(f"Batch processing complete. Results saved to {base_output_dir}")
    return batch_results

def main():
    """Run LEXICON from the command line."""
    parser = argparse.ArgumentParser(description="Run LEXICON pipeline")
    parser.add_argument("--input", "-i", help="Input file or text content")
    parser.add_argument("--model", "-m", help="OpenRouter model", default="anthropic/claude-3.5-sonnet")
    parser.add_argument("--log-level", "-l", help="Logging level", default="INFO", 
                        choices=["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"])
    parser.add_argument("--format", "-f", help="Input format", 
                        choices=["text", "podcast:vtt", "meeting:transcript", "twitter:thread"])
    parser.add_argument("--batch", "-b", action="store_true", help="Process all files in input directory")
    args = parser.parse_args()
    
    # Process all input files if batch mode is enabled
    if args.batch:
        process_all_inputs(args.model, args.log_level)
        return
    
    # Create timestamped output directory
    output_dir = create_output_dir()
    
    # Set up logging
    logger = setup_logging(args.log_level, output_dir)
    logger.info(f"Starting LEXICON (output_dir={output_dir})")
    
    # Initialize configuration
    config = LexiconConfig(
        output_dir=output_dir,
        cache_dir=output_dir / "cache",
        log_level=args.log_level,
        default_model=args.model
    )
    
    # Create metadata
    metadata = {
        "timestamp": datetime.now().isoformat(),
        "model": args.model,
        "format": args.format,
        "log_level": args.log_level
    }
    
    try:
        # Initialize engine
        logger.info("Initializing LEXICON engine")
        start_time = time.time()
        engine = LexiconEngine(config)
        
        # Process input
        if args.input:
            input_path = Path(args.input)
            if input_path.exists():
                # Process file
                logger.info(f"Processing file: {input_path}")
                metadata["input_type"] = "file"
                metadata["input_file"] = str(input_path)
                
                # Copy input file to output directory
                input_dir = output_dir / "input"
                input_copy = input_dir / input_path.name
                shutil.copy(input_path, input_copy)
                
                # Process the file
                process_metadata = {"format": args.format} if args.format else {}
                result = engine.process_file(input_path, process_metadata)
            else:
                # Process text
                logger.info("Processing text input")
                metadata["input_type"] = "text"
                
                # Save input text
                input_file = save_input_text(args.input, output_dir)
                metadata["input_text_length"] = len(args.input)
                
                # Process the text
                process_metadata = {"format": args.format} if args.format else {}
                result = engine.process_text(args.input, process_metadata)
        else:
            # Use sample text
            logger.info("No input provided, using sample text")
            metadata["input_type"] = "sample"
            
            sample_text = """
            The CEREBRUM project integrates case-based reasoning with Bayesian networks.
            Dr. Emily Chen presented the findings at the AI Conference in Boston.
            The system can process natural language and generate knowledge graphs.
            """
            
            # Save sample text
            input_file = save_input_text(sample_text, output_dir)
            metadata["input_text_length"] = len(sample_text)
            
            # Process the text
            result = engine.process_text(sample_text)
        
        # Calculate processing time
        processing_time = time.time() - start_time
        metadata["processing_time"] = processing_time
        logger.info(f"Processing completed in {processing_time:.2f} seconds")
        
        # Update metadata with result stats
        metadata["stats"] = result["stats"]
        
        # Save full result
        output_path = output_dir / "lexicon_result.json"
        with open(output_path, 'w') as f:
            json.dump(result, f, indent=2)
        
        logger.info(f"Full results saved to {output_path}")
        
        # Save metadata
        save_metadata(metadata, output_dir)
        
        # Analyze and visualize results
        analyze_results(result, output_dir, logger)
        
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
    
    logger.info("LEXICON completed successfully")

if __name__ == "__main__":
    import traceback  # Import here to avoid issues if not used
    main() 