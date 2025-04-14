#!/usr/bin/env python3
"""
Animation Fix Script for CEREBRUM
This script will regenerate all GIF animations that might be blank or corrupted.
"""

import os
import sys
import logging
import importlib
import numpy as np
import shutil
from pathlib import Path
import re
import imageio

# Set up logging
logging.basicConfig(level=logging.INFO, 
                   format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger("cerebrum-animation-fix")

def setup_paths():
    """Set up Python path to include the project root"""
    # Get the absolute path of the current script
    script_path = os.path.dirname(os.path.abspath(__file__))
    
    # Navigate up to the project root (assuming script is in src/utils)
    project_root = os.path.abspath(os.path.join(script_path, "../.."))
    
    # Add project root to Python path if not already there
    if project_root not in sys.path:
        sys.path.insert(0, project_root)
        logger.info(f"Added {project_root} to Python path")

def generate_test_data():
    """Generate synthetic test data for linear regression"""
    from src.utils.data_generator import DataGenerator
    
    # Create a data generator
    data_gen = DataGenerator()
    
    # Generate linear regression data
    X, y = data_gen.linear_data(n_samples=50, noise_level=1.5)
    
    return X, y

def ensure_consistent_folders(output_dir, case_dict):
    """
    Ensure consistent folder structure by standardizing to full case names
    and moving files from abbreviation folders if needed
    
    Parameters:
    -----------
    output_dir : str
        Base output directory
    case_dict : dict
        Dictionary mapping case abbreviations to full case names
    """
    # Check for and fix inconsistent folder structure
    for abbrev, full_name in case_dict.items():
        abbrev_dir = os.path.join(output_dir, abbrev.lower())
        full_dir = os.path.join(output_dir, full_name.lower())
        
        # Ensure the full name directory exists
        os.makedirs(full_dir, exist_ok=True)
        
        # If the abbreviation directory exists and is different from the full name directory
        if os.path.exists(abbrev_dir) and abbrev_dir != full_dir:
            # Move all files from abbreviation directory to full name directory
            if os.path.isdir(abbrev_dir):
                logger.info(f"Moving files from {abbrev_dir} to {full_dir}")
                try:
                    for item in os.listdir(abbrev_dir):
                        src_path = os.path.join(abbrev_dir, item)
                        dst_path = os.path.join(full_dir, item)
                        if not os.path.exists(dst_path):
                            if os.path.isfile(src_path):
                                shutil.copy2(src_path, dst_path)
                            elif os.path.isdir(src_path):
                                shutil.copytree(src_path, dst_path, dirs_exist_ok=True)
                    
                    # Remove the abbreviation directory after copying files
                    shutil.rmtree(abbrev_dir)
                    logger.info(f"Removed {abbrev_dir} after moving files")
                except Exception as e:
                    logger.error(f"Error moving files from {abbrev_dir} to {full_dir}: {str(e)}")

def regenerate_animations():
    """Regenerate all GIF animations for linear regression cases"""
    # Import necessary modules (after paths are set up)
    from src.models.base import Case
    from src.tests.linear_regression_cases import (
        test_vocative_case,
        test_nominative_case,
        test_accusative_case,
        test_instrumental_case,
        test_genitive_case,
        test_locative_case,
        test_ablative_case,
        test_dative_case
    )
    
    # Generate test data
    logger.info("Generating consistent test data for all cases")
    X, y = generate_test_data()
    
    # Ensure test data is properly shaped and contains no NaN values
    X = np.array(X)
    y = np.array(y)
    
    # Check for and remove any NaN values
    if np.isnan(X).any() or np.isnan(y).any():
        logger.warning("Test data contains NaN values. Cleaning data...")
        # Find indices without NaN
        valid_indices = ~(np.isnan(X).any(axis=1) | np.isnan(y).any(axis=1))
        X = X[valid_indices]
        y = y[valid_indices]
    
    linear_test_data = (X, y)
    
    # Define output directory
    output_dir = os.path.join(
        Path(__file__).parent.parent.parent, 
        "src/tests/output/linear_regression"
    )
    
    # Make sure output directory exists
    os.makedirs(output_dir, exist_ok=True)
    
    # Map case abbreviations to full names for folder standardization
    case_mapping = {
        "voc": "vocative",
        "nom": "nominative",
        "acc": "accusative",
        "ins": "instrumental",
        "gen": "genitive",
        "loc": "locative",
        "abl": "ablative",
        "dat": "dative"
    }
    
    # Ensure consistent folder structure first
    ensure_consistent_folders(output_dir, case_mapping)
    
    # List of test functions to run
    test_functions = [
        (test_vocative_case, "vocative"),
        (test_nominative_case, "nominative"),
        (test_accusative_case, "accusative"),
        (test_instrumental_case, "instrumental"),
        (test_genitive_case, "genitive"),
        (test_locative_case, "locative"),
        (test_ablative_case, "ablative"),
        (test_dative_case, "dative")
    ]
    
    # List to track success/failure
    results = []
    
    # Run each test function to regenerate animations
    for test_func, case_name in test_functions:
        logger.info(f"Regenerating animations for {case_name.upper()} case")
        try:
            # Always use the full case name for the directory
            case_output_dir = os.path.join(output_dir, case_name.lower())
            os.makedirs(case_output_dir, exist_ok=True)
            
            # Report on existing animation files before cleanup
            all_existing_files = list(Path(case_output_dir).glob("*.gif"))
            if all_existing_files:
                logger.info(f"Found {len(all_existing_files)} existing animation files for {case_name} case")
                for gif_file in all_existing_files:
                    file_size = gif_file.stat().st_size
                    logger.info(f"  - {gif_file.name}: {file_size/1024:.1f} KB")
            
            # Clean existing GIF files that appear to be empty or small
            gif_pattern = os.path.join(case_output_dir, "*.gif")
            for gif_file in Path(case_output_dir).glob("*.gif"):
                # Check if file exists and is empty or very small (potentially corrupted)
                if gif_file.exists() and gif_file.stat().st_size < 1000:  # Less than 1KB
                    logger.info(f"Removing small/corrupted GIF: {gif_file} ({gif_file.stat().st_size/1024:.1f} KB)")
                    os.remove(gif_file)
            
            # Run the test function to regenerate animations
            logger.info(f"Running {case_name} test function to regenerate animations")
            test_func(linear_test_data, output_dir)
            
            # Check for frames directories that might have been created as fallbacks
            frame_dirs = list(Path(case_output_dir).glob("frames_*"))
            if frame_dirs:
                logger.info(f"Found {len(frame_dirs)} frame directories (fallback output) for {case_name} case")
                for frame_dir in frame_dirs:
                    frame_files = list(frame_dir.glob("*.png"))
                    if frame_files:
                        logger.info(f"  - {frame_dir.name}: {len(frame_files)} frames")
                        
                        # Try to generate GIF from frames directly if GIF file doesn't exist or is empty
                        gif_name = frame_dir.name.replace("frames_", "") + ".gif"
                        gif_path = os.path.join(case_output_dir, gif_name)
                        
                        if not os.path.exists(gif_path) or os.path.getsize(gif_path) < 1000:
                            logger.info(f"Attempting to create GIF directly from frames: {gif_path}")
                            try:
                                # Sort frames by name to ensure correct order
                                sorted_frames = sorted(frame_files)
                                frames = [imageio.imread(frame) for frame in sorted_frames]
                                
                                # Save as GIF
                                imageio.mimsave(gif_path, frames, fps=5, loop=0, optimize=False)
                                
                                if os.path.exists(gif_path) and os.path.getsize(gif_path) > 0:
                                    logger.info(f"Successfully created GIF from frames: {gif_path} ({os.path.getsize(gif_path)/1024:.1f} KB)")
                            except Exception as e:
                                logger.error(f"Failed to create GIF from frames: {str(e)}")
            
            # Verify animation files were generated successfully
            animation_files = list(Path(case_output_dir).glob("*.gif"))
            if not animation_files:
                logger.warning(f"No animation files found for {case_name} case")
                results.append((case_name, "No animations found"))
            else:
                # Check file sizes to ensure they're valid
                valid_animations = [f for f in animation_files if f.stat().st_size > 0]
                if len(valid_animations) < len(animation_files):
                    logger.warning(f"Some animations for {case_name} case appear to be empty")
                    results.append((case_name, f"{len(valid_animations)}/{len(animation_files)} valid animations"))
                else:
                    logger.info(f"Successfully regenerated {len(valid_animations)} animations for {case_name} case")
                    for anim_file in valid_animations:
                        logger.info(f"  - {anim_file.name}: {anim_file.stat().st_size/1024:.1f} KB")
                    results.append((case_name, "Success"))
                
        except Exception as e:
            logger.error(f"Error regenerating animations for {case_name} case: {str(e)}")
            import traceback
            logger.error(traceback.format_exc())
            results.append((case_name, f"Error: {str(e)}"))
    
    # Log summary of results
    logger.info("\n" + "="*50)
    logger.info("ANIMATION REGENERATION SUMMARY")
    logger.info("="*50)
    for case_name, status in results:
        logger.info(f"{case_name.upper():15} : {status}")
    logger.info("="*50)

def ensure_consistent_animation_paths():
    """
    Scan all case test files and ensure animation paths are consistent.
    This prevents issues where animations might be saved to incorrect locations.
    """
    logger.info("Checking for consistent animation paths in case test files")
    
    # Get list of all test case files
    case_files_dir = os.path.join(
        Path(__file__).parent.parent, 
        "tests/linear_regression_cases"
    )
    
    # Get all _case.py files
    case_files = list(Path(case_files_dir).glob("*_case.py"))
    logger.info(f"Found {len(case_files)} case test files to check")
    
    # Map of case names to expected directory names
    case_name_mapping = {
        "nominative": "nominative",
        "vocative": "vocative",
        "accusative": "accusative",
        "genitive": "genitive",
        "dative": "dative",
        "ablative": "ablative",
        "locative": "locative",
        "instrumental": "instrumental",
        # Common abbreviations
        "nom": "nominative", 
        "voc": "vocative",
        "acc": "accusative",
        "gen": "genitive",
        "dat": "dative",
        "abl": "ablative",
        "loc": "locative",
        "ins": "instrumental"
    }
    
    # Track issues found
    issues_found = 0
    
    # Check each file for animation path patterns
    for case_file in case_files:
        try:
            with open(case_file, 'r') as f:
                content = f.read()
            
            # Extract case name from filename
            case_name = case_file.stem.replace('_case', '')
            expected_dir = case_name_mapping.get(case_name, case_name)
            
            # Look for inconsistent case directory references
            for name, full_name in case_name_mapping.items():
                if name != expected_dir and name != full_name:
                    # Look for patterns like: os.path.join(case_dir, "animation.gif")
                    # or os.path.join(output_dir, "name", "animation.gif")
                    pattern = f'os.path.join\\([^,]+, *"{name}"'
                    if re.search(pattern, content):
                        logger.warning(f"File {case_file.name} references incorrect directory '{name}' instead of '{expected_dir}'")
                        issues_found += 1
            
            # Check for animation paths that don't use case_dir
            animation_paths = re.findall(r'(\w+)_path *=.*\.gif', content)
            case_dir_usages = re.findall(r'os\.path\.join\(case_dir', content)
            
            if len(animation_paths) > len(case_dir_usages):
                logger.warning(f"File {case_file.name} may have animation paths not using case_dir variable")
                issues_found += 1
                
        except Exception as e:
            logger.error(f"Error analyzing {case_file.name}: {str(e)}")
    
    if issues_found:
        logger.warning(f"Found {issues_found} potential issues with animation paths")
    else:
        logger.info("All animation paths appear to be consistent")
    
    return issues_found == 0

if __name__ == "__main__":
    logger.info("Starting animation fix script")
    setup_paths()
    
    # First, check for animation path consistency issues
    paths_consistent = ensure_consistent_animation_paths()
    if not paths_consistent:
        logger.warning("Animation path inconsistencies found, but proceeding with regeneration")
    
    # Then regenerate all animations
    regenerate_animations()
    
    logger.info("Animation fix script completed") 