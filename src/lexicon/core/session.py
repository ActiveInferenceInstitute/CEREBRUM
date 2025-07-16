"""
LEXICON Session Management

Handles session management including output directories, input handling, and metadata.
"""

import os
import json
import shutil
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Any, Optional, Union


def create_output_dir(input_name: Optional[str] = None, is_batch: bool = False) -> Path:
    """
    Create timestamped output directory.
    
    Args:
        input_name: Optional name of input file to include in directory name
        is_batch: Whether this is a batch processing run
        
    Returns:
        Path to output directory
    """
    # Always use project root output directory (never system root /output)
    base_output_dir = Path(__file__).resolve().parent.parent.parent.parent / "output"
    base_output_dir.mkdir(parents=True, exist_ok=True)
    
    # Create timestamped subdirectory directly under output/
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    
    if is_batch:
        # Create main batch directory under output/
        output_dir = base_output_dir / f"lexicon_batch_{timestamp}"
        output_dir.mkdir(parents=True, exist_ok=True)
        
        # Create standard subdirectories for batch processing
        (output_dir / "logs").mkdir(exist_ok=True)
        (output_dir / "input").mkdir(exist_ok=True)
        (output_dir / "cache").mkdir(exist_ok=True)
        
        return output_dir
    elif input_name:
        # Clean up input name for directory name
        input_name = Path(input_name).stem
        output_dir = base_output_dir / f"lexicon_{input_name}_{timestamp}"
    else:
        # Default directory name with timestamp
        output_dir = base_output_dir / f"lexicon_{timestamp}"
    
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Create standard subdirectories
    (output_dir / "logs").mkdir(exist_ok=True)
    (output_dir / "input").mkdir(exist_ok=True)
    (output_dir / "cache").mkdir(exist_ok=True)
    (output_dir / "visualizations").mkdir(exist_ok=True)
    
    return output_dir


def save_input_text(text: str, output_dir: Path) -> Path:
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


def save_metadata(metadata: Dict[str, Any], output_dir: Path) -> None:
    """
    Save metadata to file.
    
    Args:
        metadata: Run metadata
        output_dir: Output directory
    """
    metadata_file = output_dir / "metadata.json"
    with open(metadata_file, 'w') as f:
        json.dump(metadata, f, indent=2)


def copy_input_file(input_path: Path, output_dir: Path) -> Path:
    """
    Copy input file to output directory.
    
    Args:
        input_path: Path to input file
        output_dir: Output directory
        
    Returns:
        Path to copied file
    """
    input_dir = output_dir / "input"
    input_copy = input_dir / input_path.name
    shutil.copy(input_path, input_copy)
    return input_copy


def save_result(result: Dict[str, Any], output_dir: Path, filename: str = "result.json") -> Path:
    """
    Save processing result to file.
    
    Args:
        result: Processing result
        output_dir: Output directory
        filename: Result filename
        
    Returns:
        Path to saved result file
    """
    result_path = output_dir / filename
    with open(result_path, 'w') as f:
        json.dump(result, f, indent=2)
    
    return result_path


def save_error_info(error: Exception, output_dir: Path, metadata: Optional[Dict[str, Any]] = None) -> Path:
    """
    Save error information to file.
    
    Args:
        error: Exception that occurred
        output_dir: Output directory
        metadata: Optional metadata
        
    Returns:
        Path to error file
    """
    import traceback
    
    error_info = {
        "error": str(error),
        "type": error.__class__.__name__,
        "traceback": traceback.format_exc()
    }
    
    if metadata:
        error_info["metadata"] = metadata
    
    error_path = output_dir / "error.json"
    with open(error_path, 'w') as f:
        json.dump(error_info, f, indent=2)
    
    return error_path


def create_batch_input_subdirectories(base_dir: Path, input_files: List[Path]) -> Dict[str, Path]:
    """
    Create subdirectories for batch processing with proper structure.
    
    Args:
        base_dir: Base directory for batch processing
        input_files: List of input files
        
    Returns:
        Dictionary mapping file stems to their output directories
    """
    batch_dirs = {}
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    
    for input_file in input_files:
        # Create subdirectory for this input file under base_dir
        file_stem = input_file.stem
        file_output_dir = base_dir / f"{file_stem}_{timestamp}"
        file_output_dir.mkdir(parents=True, exist_ok=True)
        
        # Create standard subdirectories for this input
        (file_output_dir / "logs").mkdir(exist_ok=True)
        (file_output_dir / "input").mkdir(exist_ok=True)
        (file_output_dir / "cache").mkdir(exist_ok=True)
        (file_output_dir / "visualizations").mkdir(exist_ok=True)
        
        batch_dirs[file_stem] = file_output_dir
    
    return batch_dirs


def create_batch_subdirectories(base_dir: Path, input_files: List[Path]) -> Dict[str, Path]:
    """
    Create subdirectories for batch processing.
    
    DEPRECATED: Use create_batch_input_subdirectories instead.
    
    Args:
        base_dir: Base directory for batch processing
        input_files: List of input files
        
    Returns:
        Dictionary mapping file stems to their output directories
    """
    return create_batch_input_subdirectories(base_dir, input_files)


def save_batch_summary(batch_results: Dict[str, Any], output_dir: Path) -> Path:
    """
    Save batch processing summary.
    
    Args:
        batch_results: Results from batch processing
        output_dir: Output directory
        
    Returns:
        Path to batch summary file
    """
    summary_path = output_dir / f"batch_summary_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    with open(summary_path, 'w') as f:
        json.dump(batch_results, f, indent=2)
    
    return summary_path 