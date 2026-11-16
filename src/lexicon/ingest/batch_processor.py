"""
LEXICON Batch Processor

Handles batch processing of multiple files through the LEXICON pipeline.
"""

import logging
from dataclasses import replace
from pathlib import Path
from typing import Any, Dict, List, Optional

from ..core.config import LexiconConfig
from ..core.engine import LexiconEngine
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


def process_all_inputs(
    base_dir: Path,
    model: str = "qwen/qwen3.8-27b",
    log_level: str = "INFO",
    *,
    input_dirs: Optional[List[Path]] = None,
    config: Optional[LexiconConfig] = None,
) -> Dict[str, Any]:
    """Process markdown inputs, retaining one outcome per resolved source path.

    Explicit input directories replace the two default discovery roots. A supplied
    config carries provider settings; output/cache paths, model and log level are
    owned by this batch and its per-file directories.
    """
    base_dir = Path(base_dir).resolve()
    batch_input_dir = base_dir / "input"
    roots = input_dirs if input_dirs is not None else [
        Path(__file__).resolve().parent.parent / "input",
        batch_input_dir,
    ]
    input_files = sorted({
        path.resolve()
        for root in roots
        for path in Path(root).glob("*.md")
        if path.is_file()
    })
    if not input_files:
        if input_dirs is None:
            batch_input_dir.mkdir(parents=True, exist_ok=True)
        print("No markdown files found in:")
        for root in roots:
            print(f"  - {root}")
        return {}
    
    print(f"Processing {len(input_files)} input files...")
    
    # Create batch subdirectories using new function
    batch_dirs = create_batch_input_subdirectories(base_dir, input_files)
    
    # Track results for batch summary
    batch_results = {}
    
    # Process each input file
    for input_file in input_files:
        source_id = str(input_file)
        print(f"Processing {source_id}...")
        
        # Look up by full path; batch dirs are keyed this way so files that share
        # a stem never collide with each other's output directory.
        file_output_dir = batch_dirs[source_id]

        try:
            config_values = {
                "output_dir": file_output_dir,
                "cache_dir": file_output_dir / "cache",
                "log_level": log_level,
                "default_model": model,
            }
            file_config = (
                replace(config, **config_values)
                if config is not None else LexiconConfig(**config_values)
            )
            result = process_single_file_in_batch(input_file, file_output_dir, file_config)
            if result.get("status") != "success":
                batch_results[source_id] = {
                    "status": "error",
                    "error": result.get("error") or "Pipeline did not report success",
                    "result": result,
                    "output_dir": str(file_output_dir),
                }
                print(f"Error processing {source_id}: {batch_results[source_id]['error']}")
                continue

            logger = logging.getLogger(f"lexicon_batch_{input_file.stem}")
            analyze_results(result, file_output_dir, logger)
            batch_results[source_id] = {
                "status": "success",
                "result": result,
                "output_dir": str(file_output_dir),
            }
        except Exception as e:
            print(f"Error processing {source_id}: {e}")
            batch_results[source_id] = {
                "status": "error",
                "error": str(e),
                "output_dir": str(file_output_dir),
            }
    
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