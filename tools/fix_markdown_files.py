#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
CEREBRUM Markdown Files Fixer

This script directly edits the Markdown files in the CEREBRUM project to ensure they
follow consistent formatting standards for headings, image references, and other elements.
This will eliminate the need for runtime fixing during PDF generation.

Usage:
  python fix_markdown_files.py [--cerebrum-dir CEREBRUM] [--verbose]
"""

import sys
import logging
import argparse
from pathlib import Path
import markdown_utils

# Configure logging
logging.basicConfig(
    level=logging.INFO, 
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)

def find_all_markdown_files(cerebrum_dir):
    """Find all markdown files in the CEREBRUM directory."""
    logging.info(f"Finding all markdown files in {cerebrum_dir}")
    return list(cerebrum_dir.glob("*.md"))

def fix_main_document(cerebrum_dir, main_file):
    """Fix the main CEREBRUM document."""
    logging.info(f"Fixing main document: {main_file}")
    
    # Ensure file exists
    if not main_file.exists():
        logging.error(f"Main file not found: {main_file}")
        return False
    
    # First make sure the figures directory exists
    figures_dir = markdown_utils.ensure_figures_directory(cerebrum_dir)
    
    # Fix image references in the main file
    markdown_utils.fix_image_references(main_file, output_to_figures=True)
    
    # Extract figure information for later use in creating the figures supplement
    figure_info = markdown_utils.extract_figure_info(main_file)
    
    # Create the figures supplement file 
    figures_supplement = markdown_utils.create_figures_supplement(cerebrum_dir, figure_info)
    
    # Remove images from main text (they will be in the figures supplement)
    markdown_utils.remove_images_from_main_text(main_file)
    
    # Standardize heading levels in the main document with is_main_text=True
    markdown_utils.standardize_heading_levels(main_file, is_main_text=True)
    
    return True

def fix_supplement_files(cerebrum_dir):
    """Find and fix all supplement files."""
    logging.info("Fixing supplement files")
    
    # Define canonical order and identifiers
    canonical_supplements = [
        {"file": "Supplement_1_Mathematical_Formalization.md", "number": 1},
        {"file": "Supplement_2_Novel_Linguistic_Cases.md", "number": 2},
        {"file": "Supplement_3_Practical_Applications.md", "number": 3},
        {"file": "Supplement_4_Related_Work.md", "number": 4},
        {"file": "Supplement_5_Category_Theory.md", "number": 5},
        {"file": "Supplement_6_Future_Directions.md", "number": 6},
    ]

    # Track processed files
    processed_files = []
    
    # Process canonical supplements first
    for supp in canonical_supplements:
        file_path = cerebrum_dir / supp["file"]
        if file_path.exists():
            # Fix image references
            markdown_utils.fix_image_references(file_path)
            # Standardize heading levels
            markdown_utils.standardize_heading_levels(file_path)
            processed_files.append(file_path)
            logging.info(f"Fixed canonical supplement: {file_path.name}")
        else:
            logging.warning(f"Canonical supplement {supp['file']} not found")
    
    # Find and process any other supplements that aren't in the canonical list
    supplement_pattern = "Supplement_*.md"
    all_supplements = list(cerebrum_dir.glob(supplement_pattern))
    
    for file_path in all_supplements:
        if file_path not in processed_files and file_path.name != "Supplement_0_Figures.md":
            # Fix image references
            markdown_utils.fix_image_references(file_path)
            # Standardize heading levels
            markdown_utils.standardize_heading_levels(file_path)
            processed_files.append(file_path)
            logging.info(f"Fixed additional supplement: {file_path.name}")
    
    return processed_files

def fix_title_page(cerebrum_dir):
    """Fix the title page file if it exists."""
    title_page = cerebrum_dir / "title_page.md"
    
    if title_page.exists():
        logging.info("Fixing title page")
        # Make sure it has a proper title
        markdown_utils.standardize_heading_levels(title_page)
        return True
    else:
        logging.warning("Title page not found")
        return False

def main():
    # Parse arguments
    parser = argparse.ArgumentParser(description="Fix CEREBRUM Markdown Files")
    parser.add_argument("--cerebrum-dir", default="CEREBRUM",
                      help="Directory containing CEREBRUM files (default: CEREBRUM)")
    parser.add_argument("--main-file", default="CEREBRUM_main_text.md",
                      help="Main Markdown file (default: CEREBRUM_main_text.md)")
    parser.add_argument("--verbose", "-v", action="store_true",
                      help="Enable verbose logging")

    args = parser.parse_args()

    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)

    # 1. Setup paths
    project_root = markdown_utils.get_project_root()
    cerebrum_dir = project_root / args.cerebrum_dir
    main_file = cerebrum_dir / args.main_file
    
    if not cerebrum_dir.exists():
        logging.error(f"CEREBRUM directory not found: {cerebrum_dir}")
        return 1
    
    # 2. Fix title page if it exists
    fix_title_page(cerebrum_dir)
    
    # 3. Fix main document
    if not fix_main_document(cerebrum_dir, main_file):
        logging.error("Failed to fix main document")
        return 1
    
    # 4. Fix supplement files
    fixed_supplements = fix_supplement_files(cerebrum_dir)
    logging.info(f"Fixed {len(fixed_supplements)} supplement files")
    
    logging.info("All Markdown files have been fixed and standardized")
    logging.info("You can now run render_markdown.py to generate the PDF without additional fixes")
    
    return 0

if __name__ == "__main__":
    sys.exit(main()) 