#!/usr/bin/env python3
"""
LEXICON - Linguistic Entity eXtraction for Integrated CONtext analysis

Simplified run script that orchestrates the LEXICON processing pipeline
using modular functions from across the codebase.
"""

import sys
import time
import argparse
from pathlib import Path
from datetime import datetime
from typing import Dict, Any, Optional

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).resolve().parent.parent.parent))

# Import modular components
from src.lexicon.core import (
    LexiconEngine, LexiconConfig,
    create_output_dir, save_input_text, save_metadata, save_result, save_error_info,
    configure_cuda_environment, setup_environment_logging, validate_environment
)
from src.lexicon.core.logging import setup_logging
from src.lexicon.ingest import process_all_inputs
from src.lexicon.visualization import analyze_results

# Load environment variables
from dotenv import load_dotenv
env_path = Path(__file__).resolve().parent.parent.parent / '.env'
load_dotenv(dotenv_path=env_path)


def process_text_input(text: str, args: argparse.Namespace) -> Dict[str, Any]:
    """
    Process text input through LEXICON pipeline.
    
    Args:
        text: Input text to process
        args: Command line arguments
        
    Returns:
        Processing result
    """
    # Create output directory
    output_dir = create_output_dir()
    
    # Set up logging
    logger = setup_logging(LexiconConfig(log_level=args.log_level, output_dir=output_dir))
    logger.info(f"Processing text input (output_dir={output_dir})")
    
    # Save input text
    save_input_text(text, output_dir)
    
    # Create configuration
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
        "log_level": args.log_level,
        "input_type": "text",
        "input_text_length": len(text)
    }
    
    try:
        # Initialize and run engine
        start_time = time.time()
        engine = LexiconEngine(config)
        
        # Process the text
        process_metadata = {"format": args.format} if args.format else {}
        result = engine.process_text(text, process_metadata)
        
        # Update metadata
        metadata.update({
            "processing_time": time.time() - start_time,
            "stats": result["stats"]
        })
        
        # Save results
        save_result(result, output_dir, "lexicon_result.json")
        save_metadata(metadata, output_dir)
        
        # Analyze and visualize
        analyze_results(result, output_dir, logger)
        
        return result
        
    except Exception as e:
        logger.error(f"Error processing text: {str(e)}", exc_info=True)
        save_error_info(e, output_dir, metadata)
        raise


def process_file_input(file_path: str, args: argparse.Namespace) -> Dict[str, Any]:
    """
    Process file input through LEXICON pipeline.
    
    Args:
        file_path: Path to input file
        args: Command line arguments
        
    Returns:
        Processing result
    """
    input_path = Path(file_path)
    if not input_path.exists():
        raise FileNotFoundError(f"Input file not found: {file_path}")
    
    # Create output directory
    output_dir = create_output_dir(input_path.name)
    
    # Set up logging
    logger = setup_logging(LexiconConfig(log_level=args.log_level, output_dir=output_dir))
    logger.info(f"Processing file: {input_path} (output_dir={output_dir})")
    
    # Create configuration
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
        "log_level": args.log_level,
        "input_type": "file",
        "input_file": str(input_path)
    }
    
    try:
        # Initialize and run engine
        start_time = time.time()
        engine = LexiconEngine(config)
        
        # Process the file
        process_metadata = {"format": args.format} if args.format else {}
        result = engine.process_file(input_path, process_metadata)
        
        # Update metadata
        metadata.update({
            "processing_time": time.time() - start_time,
            "stats": result["stats"]
        })
        
        # Save metadata
        save_metadata(metadata, output_dir)
        
        # Analyze and visualize
        analyze_results(result, output_dir, logger)
        
        return result
        
    except Exception as e:
        logger.error(f"Error processing file: {str(e)}", exc_info=True)
        save_error_info(e, output_dir, metadata)
        raise


def process_sample_text(args: argparse.Namespace) -> Dict[str, Any]:
    """
    Process sample text for demonstration.
    
    Args:
        args: Command line arguments
        
    Returns:
        Processing result
    """
    sample_text = """
    The CEREBRUM project integrates case-based reasoning with Bayesian networks.
    Dr. Emily Chen presented the findings at the AI Conference in Boston.
    The system can process natural language and generate knowledge graphs.
    """
    
    print("No input provided, using sample text for demonstration.")
    return process_text_input(sample_text, args)


def main():
    """Main entry point for LEXICON processing."""
    # Set up environment
    setup_environment_logging()
    
    # Configure CUDA if available
    configure_cuda_environment()
    
    # Validate environment
    if not validate_environment():
        print("Environment validation failed. Please check your configuration.")
        sys.exit(1)
    
    # Parse arguments
    parser = argparse.ArgumentParser(description="Run LEXICON pipeline")
    parser.add_argument("--input", "-i", help="Input file or text content")
    parser.add_argument("--model", "-m", help="OpenRouter model", 
                        default="anthropic/claude-3.5-sonnet")
    parser.add_argument("--log-level", "-l", help="Logging level", default="INFO", 
                        choices=["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"])
    parser.add_argument("--format", "-f", help="Input format", 
                        choices=["text", "podcast:vtt", "meeting:transcript", "twitter:thread"])
    parser.add_argument("--batch", "-b", action="store_true", 
                        help="Process all files in input directory")
    parser.add_argument("--process-samples", "-s", action="store_true",
                        help="Process the sample input files (chemistry_methods.md and therapy_session.md)")
    
    args = parser.parse_args()
    
    try:
        if args.process_samples:
            # Process the sample input files directly
            print("Processing sample input files from src/lexicon/input/...")
            base_dir = create_output_dir(is_batch=True)
            batch_results = process_all_inputs(base_dir, args.model, args.log_level)
            print(f"Sample processing complete. Processed {len(batch_results)} files.")
            print(f"Results available in: {base_dir}")
            
            # Print summary of results
            for filename, result in batch_results.items():
                if result["status"] == "success":
                    output_dir = result.get("output_dir", "unknown")
                    print(f"  ✓ {filename}: {output_dir}")
                else:
                    print(f"  ✗ {filename}: {result.get('error', 'Unknown error')}")
            
        elif args.batch:
            # Batch processing
            print("Starting batch processing...")
            base_dir = create_output_dir(is_batch=True)
            batch_results = process_all_inputs(base_dir, args.model, args.log_level)
            print(f"Batch processing complete. Processed {len(batch_results)} files.")
            
        elif args.input:
            input_path = Path(args.input)
            if input_path.exists():
                # File processing
                result = process_file_input(args.input, args)
                print(f"File processing complete. Results in: {result.get('output_dir', 'output directory')}")
            else:
                # Text processing
                result = process_text_input(args.input, args)
                print("Text processing complete.")
        else:
            # Sample processing
            result = process_sample_text(args)
            print("Sample processing complete.")
            
    except KeyboardInterrupt:
        print("\nProcessing interrupted by user.")
        sys.exit(0)
    except Exception as e:
        print(f"Processing failed: {str(e)}")
        sys.exit(1)


if __name__ == "__main__":
    main() 