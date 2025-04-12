#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
CEREBRUM PDF Generator

This script renders the CEREBRUM document suite into a cohesive PDF file.
It handles:
1. Title page
2. Main CEREBRUM.md content with proper image insertion
3. Figures section with one figure per page
4. All supplements in the correct order

Requirements:
- pandoc
- A LaTeX distribution with xelatex
"""

import subprocess
import os
import sys
import logging
import re
import argparse
from pathlib import Path
import markdown_utils

# Configure logging
logging.basicConfig(
    level=logging.INFO, 
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)

def run_mermaid_renderer(project_root):
    """Run the Mermaid diagram rendering script if available."""
    mermaid_script_path = project_root / "tools" / "render_mermaids.py"
    
    if not mermaid_script_path.exists():
        logging.warning(f"Mermaid rendering script not found at: {mermaid_script_path}")
        return False
        
    logging.info("Rendering Mermaid diagrams...")
    try:
        result = subprocess.run(
            ["python3", str(mermaid_script_path)],
            check=True,
            capture_output=True,
            text=True,
            cwd=str(project_root)
        )
        logging.info("Mermaid diagrams rendered successfully")
        return True
    except subprocess.CalledProcessError as e:
        logging.error(f"Mermaid rendering failed: {e}")
        logging.error(f"Error output: {e.stderr}")
        return False

def prepare_supplement_files(cerebrum_dir):
    """Find supplement files in canonical order."""
    # Define canonical order and identifiers
    canonical_supplements = [
        {"file": "Supplement_1_Mathematical_Formalization.md", "number": 1},
        {"file": "Supplement_2_Novel_Linguistic_Cases.md", "number": 2},
        {"file": "Supplement_3_Practical_Applications.md", "number": 3},
        {"file": "Supplement_4_Related_Work.md", "number": 4},
        {"file": "Supplement_5_Category_Theory.md", "number": 5},
        {"file": "Supplement_6_Future_Directions.md", "number": 6},
    ]

    supplement_files = []
    found_canonical_files = set()
    for supp in canonical_supplements:
        file_path = cerebrum_dir / supp["file"]
        if file_path.exists():
            supplement_files.append((file_path, supp["number"]))
            found_canonical_files.add(file_path.name)
        else:
            logging.warning(f"Canonical supplement {supp['file']} not found")

    # Discover extra supplements
    extra_files = list(cerebrum_dir.glob("Supplement_*.md"))
    next_extra_number = len(canonical_supplements) + 1
    for file_path in extra_files:
        if file_path.name not in found_canonical_files:
            match = re.search(r'Supplement_(\d+)_', file_path.name)
            num = int(match.group(1)) if match else next_extra_number
            if not match:
                next_extra_number += 1
            supplement_files.append((file_path, num))
            logging.warning(f"Found non-canonical supplement: {file_path.name}, assigned number {num}")

    # Sort by number
    supplement_files.sort(key=lambda x: x[1])

    # Extract just the file paths in order
    processed_paths = [file_path for file_path, number in supplement_files]
    
    return processed_paths

def generate_pdf(title_page, main_content, figures_supplement, supplements, output_pdf, cerebrum_dir):
    """Generate the PDF using pandoc with improved TOC and structure."""
    logging.info("Generating PDF...")
    
    # Title page existence check is less critical now, but good to keep
    if not title_page.exists():
        logging.warning(f"Title page file {title_page} not found, relying on default metadata.")
    
    # Get the base name for output files
    output_base = output_pdf.with_suffix('')
    output_tex = output_base.with_suffix('.tex')
    
    # Build pandoc command
    # NOTE: title_page is NOT included as a direct input file anymore
    pandoc_cmd = [
        "pandoc",
        # Input files in order: Title Page, Main Content, Figures Supplement, Other Supplements
        str(title_page), # Explicitly add title page first
        str(main_content),
    ]
    
    # Add figures supplement if available
    if figures_supplement and figures_supplement.exists():
        pandoc_cmd.append(str(figures_supplement))
    
    # Add all other supplements in proper order
    for supp in supplements:
        # Skip the figures supplement as it was already added
        if supp.name == "Supplement_0_Figures.md":
            continue
        pandoc_cmd.append(str(supp))
    
    # Add output and formatting options
    pandoc_cmd.extend([
        # Output file
        "-o", str(output_pdf),
        
        # PDF engine
        "--pdf-engine=xelatex",
        
        # Simple document structure
        "--standalone",
        "--file-scope", # Treat each input file in its own scope for headings
        
        # Add table of contents with depth of 2 (chapters and sections)
        "--toc",
        "--toc-depth=2",
        
        # Resource path for images - include both figures and output directories
        "--resource-path", f"{cerebrum_dir}:{cerebrum_dir}/figures:{cerebrum_dir}/output",
        
        # Ensure proper document class
        "-V", "documentclass=report",
        
        # Use chapters for top-level sections
        "-V", "chapters=true",
        
        # Metadata for title page generation (pandoc template uses these)
        # These can be overridden by YAML metadata block in main_content if present
        # Removed: "-V", "title-meta=CEREBRUM",
        # Removed: "-V", "titlepage=true", # Instructs template to create a title page
        
        # Formatting
        "-V", "papersize=letter",
        "-V", "geometry=margin=1in",
        "-V", "fontsize=11pt",
        "-V", "linestretch=1.15",
        "-V", "linkcolor=black",
        "-V", "urlcolor=black",
        
        # Add necessary LaTeX packages and formatting commands
        # Removed \\pretocmd{\\chapter}{\\clearpage}{}{}
        "-V", "header-includes=\\usepackage{etoolbox}\\usepackage{needspace}\\pretocmd{\\section}{\\needspace{4\\baselineskip}}{}{}"
    ])
    
    # Run pandoc
    try:
        logging.info(f"Running PDF generation: {' '.join(pandoc_cmd)}")
        result_pdf = subprocess.run(
            pandoc_cmd,
            check=True,
            capture_output=True,
            text=True,
            cwd=str(cerebrum_dir)
        )
        logging.info(f"PDF generated successfully: {output_pdf}")
        
        # Generate .tex file with a second pandoc command
        tex_cmd = pandoc_cmd.copy()
        # Replace the output file
        out_index = tex_cmd.index("-o") + 1
        tex_cmd[out_index] = str(output_tex)
        
        logging.info(f"Running TEX generation: {' '.join(tex_cmd)}")
        result_tex = subprocess.run(
            tex_cmd,
            check=True,
            capture_output=True,
            text=True,
            cwd=str(cerebrum_dir)
        )
        logging.info(f"TEX file generated successfully: {output_tex}")
        
        return True
    except subprocess.CalledProcessError as e:
        logging.error(f"PDF/TEX generation failed: {e}")
        logging.error(f"STDOUT: {e.stdout}")
        logging.error(f"STDERR: {e.stderr}")
        return False

def main():
    # Parse arguments
    parser = argparse.ArgumentParser(description="Generate CEREBRUM PDF")
    parser.add_argument("--cerebrum-dir", default="CEREBRUM",
                      help="Directory containing CEREBRUM files (default: CEREBRUM)")
    parser.add_argument("--main-file", default="CEREBRUM_main_text.md",
                      help="Main Markdown file (default: CEREBRUM_main_text.md)")
    parser.add_argument("--output-file", default="CEREBRUM.pdf",
                      help="Output PDF filename (default: CEREBRUM.pdf)")
    parser.add_argument("--skip-mermaid", action="store_true",
                      help="Skip running the Mermaid rendering script")
    parser.add_argument("--verbose", "-v", action="store_true",
                      help="Enable verbose logging")
    parser.add_argument("--keep-temp", action="store_true",
                      help="Keep temporary files after generation")
    parser.add_argument("--fix-markdown", action="store_true",
                      help="Run the markdown fixer before generating PDF")

    args = parser.parse_args()

    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)

    # 1. Setup paths
    project_root = markdown_utils.get_project_root()
    cerebrum_dir = project_root / args.cerebrum_dir
    main_file = cerebrum_dir / args.main_file
    title_page = cerebrum_dir / "title_page.md"
    figures_supplement = cerebrum_dir / "Supplement_0_Figures.md"
    
    # Construct the output path within the CEREBRUM/output directory
    output_dir = cerebrum_dir / "output"
    output_dir.mkdir(parents=True, exist_ok=True) # Ensure output directory exists
    output_file = output_dir / args.output_file 

    if not cerebrum_dir.exists():
        logging.error(f"CEREBRUM directory not found: {cerebrum_dir}")
        return 1
    
    if not main_file.exists():
        logging.error(f"Main file not found: {main_file}")
        # Try to find CEREBRUM.md and rename it
        original_file = cerebrum_dir / "CEREBRUM.md"
        if original_file.exists():
            logging.info(f"Found original file {original_file}, copying to {main_file}")
            import shutil
            shutil.copy2(original_file, main_file)
        else:
            return 1
    
    if not title_page.exists():
        logging.error(f"Title page not found: {title_page}")
        return 1
    
    # 2. If requested, run the markdown fixer script first
    if args.fix_markdown:
        logging.info("Running markdown fixer script")
        try:
            fix_script = project_root / "tools" / "fix_markdown_files.py"
            result = subprocess.run(
                [sys.executable, str(fix_script), "--cerebrum-dir", str(cerebrum_dir), "--main-file", args.main_file],
                check=True,
                capture_output=True,
                text=True
            )
            logging.info("Markdown files fixed successfully")
        except subprocess.CalledProcessError as e:
            logging.error(f"Markdown fixer failed: {e}")
            logging.error(f"Error output: {e.stderr}")
            # Continue with PDF generation anyway
    
    # 3. Run Mermaid renderer if available and not skipped
    if not args.skip_mermaid:
        run_mermaid_renderer(project_root)
    
    # 4. Ensure figures directory exists and contains all figure images
    figures_dir = markdown_utils.ensure_figures_directory(cerebrum_dir)
    
    # 5. Prepare other supplement files (find and sort in canonical order)
    supplements = prepare_supplement_files(cerebrum_dir)
    logging.info(f"Found {len(supplements)} supplement files to include")
    
    # 6. Generate the PDF
    success = generate_pdf(title_page, main_file, figures_supplement, supplements, output_file, cerebrum_dir)
    
    # 7. Clean up intermediate files unless specified to keep them
    if success and not args.keep_temp:
        markdown_utils.cleanup_intermediate_files(cerebrum_dir)
    
    return 0 if success else 1

if __name__ == "__main__":
    sys.exit(main()) 