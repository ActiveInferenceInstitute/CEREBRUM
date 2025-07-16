"""
LEXICON Batch Processor

Handles batch processing of multiple files through the LEXICON pipeline.
"""

import logging
from pathlib import Path
from typing import Dict, List, Any, Optional

from ..core.engine import LexiconEngine
from ..core.config import LexiconConfig
from ..core.session import create_batch_input_subdirectories, save_batch_summary
from ..visualization.analyzer import analyze_results


def detect_format_from_filename(filename: str) -> str:
    """
    Detect format type based on filename patterns.
    
    Args:
        filename: Name of the file
        
    Returns:
        Format type string
    """
    filename_lower = filename.lower()
    
    if "podcast" in filename_lower:
        return "podcast:vtt"
    elif "therapy" in filename_lower or "meeting" in filename_lower:
        return "meeting:transcript"
    elif "twitter" in filename_lower or "thread" in filename_lower:
        return "twitter:thread"
    elif "chemistry" in filename_lower:
        return "text"
    else:
        return "text"


def process_single_file_in_batch(input_file: Path, output_dir: Path, 
                                config: LexiconConfig) -> Dict[str, Any]:
    """
    Process a single file as part of batch processing.
    
    Args:
        input_file: Path to input file
        output_dir: Output directory for this file
        config: LEXICON configuration
        
    Returns:
        Processing result
    """
    logger = logging.getLogger(f"lexicon_batch_{input_file.stem}")
    logger.info(f"Processing file: {input_file}")
    
    # Copy input file to output directory
    input_copy_path = output_dir / "input" / input_file.name
    input_copy_path.parent.mkdir(parents=True, exist_ok=True)
    with open(input_file, 'r', encoding='utf-8') as src, open(input_copy_path, 'w', encoding='utf-8') as dest:
        content = src.read()
        dest.write(content)
    
    # Determine format type based on filename
    format_type = detect_format_from_filename(input_file.name)
    
    # Update config with correct output directory
    config.output_dir = output_dir
    config.cache_dir = output_dir / "cache"
    
    # Initialize engine
    logger.info("Initializing LEXICON engine")
    engine = LexiconEngine(config)
    
    # Process the file
    with open(input_file, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Create metadata
    metadata = {
        "format": format_type,
        "file_path": str(input_file),
        "file_name": input_file.name,
        "file_type": input_file.suffix,
        "output_dir": str(output_dir),
        "batch_processing": True
    }
    
    # Process the content
    logger.info(f"Processing content with format: {format_type}")
    result = engine.process_text(content, metadata)
    
    # Save result
    result_path = output_dir / "result.json"
    import json
    with open(result_path, 'w', encoding='utf-8') as f:
        json.dump(result, f, indent=2)
    
    logger.info(f"Processing complete. Results saved to {result_path}")
    return result


def generate_health_summary(batch_results: Dict[str, Any]) -> Dict[str, Any]:
    """Generate health summary for batch processing."""
    total_files = len(batch_results)
    successful = sum(1 for r in batch_results.values() if r['status'] == 'success')
    errors = total_files - successful
    avg_entities = sum(r['stats']['entities'] for r in batch_results.values() if 'stats' in r) / max(successful, 1)
    avg_claims = sum(r['stats']['claims'] for r in batch_results.values() if 'stats' in r) / max(successful, 1)
    warnings = [r.get('warning', []) for r in batch_results.values() if 'warning' in r]
    return {
        'success_rate': successful / total_files * 100,
        'errors': errors,
        'avg_entities': avg_entities,
        'avg_claims': avg_claims,
        'warnings': warnings
    }


def process_all_inputs(base_dir: Path, model: str = "anthropic/claude-3.5-sonnet", 
                      log_level: str = "INFO") -> Dict[str, Any]:
    """
    Process all input files in the input directory.
    
    Args:
        base_dir: Base directory for batch processing
        model: Model to use
        log_level: Logging level
        
    Returns:
        Dictionary of batch results
    """
    # First check if there are files in the src/lexicon/input directory
    src_input_dir = Path(__file__).resolve().parent.parent / "input"
    batch_input_dir = base_dir / "input"
    
    # Look for input files in both locations
    input_files = []
    
    # Check src/lexicon/input directory first
    if src_input_dir.exists():
        md_files = list(src_input_dir.glob("*.md"))
        if md_files:
            print(f"Found {len(md_files)} markdown files in src/lexicon/input/")
            input_files.extend(md_files)
    
    # Also check batch input directory
    if batch_input_dir.exists():
        additional_files = list(batch_input_dir.glob("*.md"))
        if additional_files:
            print(f"Found {len(additional_files)} additional markdown files in batch input directory")
            input_files.extend(additional_files)
    
    # If no files found, create input directory and inform user
    if not input_files:
        batch_input_dir.mkdir(parents=True, exist_ok=True)
        print(f"No markdown files found in either:")
        print(f"  - {src_input_dir}")
        print(f"  - {batch_input_dir}")
        print(f"Please place input files in one of these directories and run again.")
        return {}
    
    print(f"Processing {len(input_files)} input files...")
    
    # Create batch subdirectories using new function
    batch_dirs = create_batch_input_subdirectories(base_dir, input_files)
    
    # Track results for batch summary
    batch_results = {}
    
    # Process each input file
    for input_file in input_files:
        print(f"Processing {input_file.name}...")
        
        file_output_dir = batch_dirs[input_file.stem]
        
        # Initialize configuration for this file
        config = LexiconConfig(
            output_dir=file_output_dir,
            cache_dir=file_output_dir / "cache",
            log_level=log_level,
            default_model=model
        )
        
        try:
            # Process the file
            result = process_single_file_in_batch(input_file, file_output_dir, config)
            
            # Add visualization step
            logger = logging.getLogger(f"lexicon_batch_{input_file.stem}")
            analyze_results(result, file_output_dir, logger)
            
            batch_results[input_file.name] = {
                "status": "success", 
                "result": result,
                "output_dir": str(file_output_dir)
            }
        except Exception as e:
            print(f"Error processing {input_file.name}: {e}")
            batch_results[input_file.name] = {"status": "error", "error": str(e)}
    
    # Save batch summary
    summary_path = save_batch_summary(batch_results, base_dir)
    
    print(f"Batch processing complete. Results saved to {base_dir}")
    print(f"Batch summary saved to {summary_path}")
    
    return batch_results


def find_input_files(base_dir: Path, extensions: List[str] = None) -> List[Path]:
    """
    Find input files in the base directory.
    
    Args:
        base_dir: Base directory to search
        extensions: List of file extensions to include (default: ['.md'])
        
    Returns:
        List of input file paths
    """
    if extensions is None:
        extensions = ['.md']
    
    input_dir = base_dir / "input"
    if not input_dir.exists():
        return []
    
    input_files = []
    for ext in extensions:
        input_files.extend(input_dir.glob(f"*{ext}"))
    
    return sorted(input_files) 