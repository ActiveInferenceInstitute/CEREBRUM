#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Renders the CEREBRUM/CEREBRUM.md file into a PDF file named 
CEREBRUM/CEREBRUM.pdf in the same directory.

This script uses pandoc to perform the conversion and assumes that 
pandoc and a suitable LaTeX distribution (like TeX Live or MiKTeX, 
preferably with xelatex) are installed and available in the system's PATH.

The script determines the project root based on its own location 
(expected to be in '<root>/tools/'). It then constructs the necessary 
paths relative to this root.

It ensures that images referenced with relative paths like 'output/Figure_1.png' 
within CEREBRUM.md are correctly embedded in the final PDF by setting the
appropriate resource path for pandoc.
"""

import subprocess
import os
import sys
import logging
import re
import shutil
import tempfile
import traceback
import glob
import argparse

# Configure logging for clear output
logging.basicConfig(
    level=logging.INFO, 
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)

def get_project_root():
    """
    Determines the project root directory based on the script's location.
    Assumes the script resides in '<project_root>/tools/'.
    """
    script_dir = os.path.dirname(os.path.abspath(__file__))
    project_root = os.path.dirname(script_dir)
    logging.debug(f"Determined project root: {project_root}")
    return project_root

def remove_duplicate_figures(markdown_abs_path):
    """
    Remove duplicate figure references without captions from the markdown file.
    """
    with open(markdown_abs_path, 'r') as f:
        content = f.read()
    
    # Pattern to match figure images without proper captions
    # This pattern looks for ![Figure X](path) that isn't followed by a proper caption
    duplicate_pattern = r'!\[Figure (\d+)\]\(output/Figure_\1\.png\)\s*\n\s*\n(?!!\[Figure \1:)'
    
    # Replace duplicate figures with empty string
    cleaned_content = re.sub(duplicate_pattern, '', content)
    
    # Write back to file if changes were made
    if cleaned_content != content:
        with open(markdown_abs_path, 'w') as f:
            f.write(cleaned_content)
        logging.info(f"Removed duplicate figure references from {markdown_abs_path}")
    
    return cleaned_content != content

def render_markdown_to_pdf(markdown_abs_path, appendix_files, output_pdf_abs_path, resource_abs_path):
    """
    Renders a main Markdown file and multiple appendix Markdown files to PDF using pandoc,
    handling relative resource paths.
    
    Args:
        markdown_abs_path (str): Absolute path to the input main Markdown file.
        appendix_files (list): List of absolute paths to the input appendix Markdown files.
        output_pdf_abs_path (str): Absolute path for the output PDF file.
        resource_abs_path (str): Absolute path to the directory relative to which
                                 resource paths inside the markdown file should be
                                 resolved (e.g., for images like 'output/figure.png').
    
    Returns:
        bool: True if rendering was successful, False otherwise.
    """
    if not os.path.exists(markdown_abs_path):
        logging.error(f"Input Markdown file not found: {markdown_abs_path}")
        return False
    
    for appendix_path in appendix_files:
        if not os.path.exists(appendix_path):
            logging.error(f"Input Appendix file not found: {appendix_path}")
            return False
    
    if not os.path.isdir(os.path.dirname(output_pdf_abs_path)):
        logging.error(f"Output directory does not exist: {os.path.dirname(output_pdf_abs_path)}")
        return False

    # First, remove any duplicate figure references without captions
    remove_duplicate_figures(markdown_abs_path)
    logging.info("Processing Markdown files and preparing for PDF generation...")

    # Using xelatex is often more robust for complex documents, unicode, and fonts
    pdf_engine = "xelatex" 
    logging.info(f"Using PDF engine: {pdf_engine}")
    
    # Create a simple straightforward approach with a single pandoc command
    main_command = ["pandoc", markdown_abs_path]
    
    # Create a temporary file with just the \appendix command
    appendix_marker_file = os.path.join(os.path.dirname(output_pdf_abs_path), "appendix_marker.tex")
    with open(appendix_marker_file, 'w') as f:
        f.write("\n\\clearpage\n\\appendix\n")
    
    # Add the appendix marker after the main content
    main_command.append("--include-after-body=" + appendix_marker_file)
    
    # Add all appendix files
    for appendix_file in appendix_files:
        main_command.append(appendix_file)
    
    # Add output options
    main_command.extend([
        "--resource-path", resource_abs_path,
        "-o", output_pdf_abs_path,
        f"--pdf-engine={pdf_engine}",
        "--standalone",
        "--toc",
        "--toc-depth=3",
        "--number-sections",
        "--top-level-division=chapter",
        
        # Title page variables
        "-V", "title=Case-Enabled Reasoning Engine with Bayesian Representations for Unified Modeling (CEREBRUM)",
        "-V", "author=Daniel Ari Friedman",
        "-V", "date=Version 1.0 (2025-04-07)",
        "-V", "institute=Active Inference Institute",
        "-V", "abstract-title=Abstract",
        "-V", "doi=10.5281/zenodo.15170908",
        
        # Add metadata for PDF info
        "--metadata=author-meta:Daniel Ari Friedman",
        "--metadata=title-meta:CEREBRUM: Case-Enabled Reasoning Engine with Bayesian Representations for Unified Modeling",
        "--metadata=doi:10.5281/zenodo.15170908",
        
        # Move TOC after title page
        "-V", "toc-title=Contents",
        
        # Page layout and formatting
        "-V", "documentclass=article",
        "-V", "papersize=letter",
        "-V", "geometry=margin=1in",
        "-V", "fontsize=11pt",
        "-V", "linestretch=1.15",
        "-V", "linkcolor=black",
        "-V", "urlcolor=black",
        "-V", "links-as-notes=true",
    ])
    
    # Add LaTeX packages directly
    latex_packages = [
        "\\usepackage{amsmath}",
        "\\usepackage{amssymb}",
        "\\usepackage{mathtools}",
        "\\usepackage{bm}",
        "\\usepackage{caption}",
        "\\usepackage{booktabs}",
        "\\usepackage{tabularx}",
        "\\let\\oldtabular\\tabular",
        "\\let\\endoldtabular\\endtabular",
        "\\renewenvironment{tabular}{\\tiny\\oldtabular}{\\endoldtabular}",
        "\\usepackage{titling}",
        "\\usepackage{titlesec}",
        "\\pretitle{\\begin{center}\\LARGE\\bfseries}",
        "\\posttitle{\\end{center}\\vspace{0.5em}}",
        "\\preauthor{\\begin{center}\\large}",
        "\\postauthor{\\end{center}}",
        "\\predate{\\begin{center}\\large}",
        "\\postdate{\\end{center}\\vspace{2em}}",
        "\\renewcommand{\\maketitlehookd}{\\vspace{1em}\\begin{center}\\footnotesize -.-. . .-. . -... .-. ..- -- ---... / -.-. .- ... . -....- . -. .- -... .-.. . -.. / .-. . .- ... --- -. .. -. --. / . -. --. .. -. . / .-- .. - .... / -... .- -.-- . ... .. .- -. / .-. . .--. .-. . ... . -. - .- - .. --- -. ... / ..-. --- .-. / ..- -. .. ..-. .. . -.. / -- --- -.. . .-.. .. -. --. \\end{center}\\vspace{1em}}",
        "\\usepackage{hyperref}",
        "\\AtBeginDocument{\\let\\oldmaketitle\\maketitle\\renewcommand{\\maketitle}{\\oldmaketitle\\begin{center}\\large DOI: \\href{https://doi.org/10.5281/zenodo.15170908}{10.5281/zenodo.15170908}\\end{center}\\maketitlehookd}}",
        "\\usepackage{appendix}",
        "\\renewcommand{\\appendixname}{Appendix}",
    ]
    
    # Add each package as a separate header-includes
    for pkg in latex_packages:
        main_command.extend(["-V", f"header-includes={pkg}"])
    
    logging.info("Starting PDF generation with single pandoc command...")
    logging.info(f"Pandoc command: {' '.join(main_command)}")
    
    try:
        # Execute the pandoc command
        project_root = os.path.dirname(resource_abs_path)
        logging.info("Generating PDF...")
        result = subprocess.run(
            main_command,
            check=True,
            capture_output=True,
            text=True,
            cwd=project_root
        )
        logging.info(f"Successfully generated PDF: {output_pdf_abs_path}")
        
        # Clean up temporary files
        try:
            os.unlink(appendix_marker_file)
        except Exception as e:
            logging.warning(f"Could not delete temporary file {appendix_marker_file}: {e}")
        
        return True
        
    except FileNotFoundError as e:
        logging.error(f"Error: Command not found: {e}")
        logging.error("Please ensure pandoc and LaTeX are installed and in your system's PATH.")
        return False
    except subprocess.CalledProcessError as e:
        logging.error(f"Command failed with return code {e.returncode}.")
        logging.error(f"Command executed: {' '.join(e.cmd)}")
        logging.error("Error output:\n" + e.stderr)
        if e.stdout:
            logging.error("Standard output:\n" + e.stdout)
        return False
    except Exception as e:
        logging.error(f"An unexpected error occurred during rendering: {e}")
        traceback.print_exc()
        return False

def run_mermaid_script(script_path, project_root_dir):
    """
    Runs the render_mermaids.py script from the project root directory.

    Args:
        script_path (str): Absolute path to the render_mermaids.py script.
        project_root_dir (str): Absolute path to the project root directory.

    Returns:
        bool: True if the script ran successfully, False otherwise.
    """
    logging.info(f"Running Mermaid rendering script: {script_path}")
    try:
        result = subprocess.run(
            ["python3", script_path], # Use absolute path for script
            check=True,
            capture_output=True,
            text=True,
            cwd=project_root_dir # Set CWD to project root
        )
        logging.info("Mermaid rendering script finished successfully.")
        logging.debug(f"Mermaid script stdout:\n{result.stdout}")
        if result.stderr:
            logging.debug(f"Mermaid script stderr:\n{result.stderr}")
        return True
    except FileNotFoundError:
        logging.error("Error: 'python3' command not found. Cannot run Mermaid script.")
        return False
    except subprocess.CalledProcessError as e:
        logging.error(f"Mermaid rendering script failed with return code {e.returncode}.")
        logging.error(f"Command executed: {' '.join(e.cmd)}")
        if e.stdout:
            logging.error(f"Mermaid script stdout:\n{e.stdout}")
        if e.stderr:
            logging.error(f"Mermaid script stderr:\n{e.stderr}")
        return False
    except Exception as e:
        logging.error(f"An unexpected error occurred while running Mermaid script: {e}")
        return False

def update_cerebrum_md(cerebrum_path, figure_paths):
    """Update CEREBRUM.md to include the rendered PNG images, ensuring no duplicates."""
    with open(cerebrum_path, 'r') as f:
        content = f.read()
    
    # Create a backup of the original file
    backup_path = cerebrum_path.with_suffix('.md.bak')
    shutil.copy2(cerebrum_path, backup_path)
    print(f"Created backup of CEREBRUM.md at {backup_path}")
    
    # Track which figures have already been processed to avoid duplicates
    processed_figures = set()
    
    # First, fix any double labeled figures (Figure X: Figure X:)
    double_label_pattern = r'!\[Figure (\d+): Figure \1: ([^\]]+)\]'
    content = re.sub(double_label_pattern, r'![Figure \1: \2]', content)
    
    # Pre-process: Check for existing figure references with captions
    # Find all existing proper figure references with captions
    caption_pattern = r'!\[Figure (\d+): [^\]]+\]\(output/Figure_\1\.png\)'
    caption_matches = re.finditer(caption_pattern, content)
    for match in caption_matches:
        fig_num = match.group(1)
        processed_figures.add(fig_num)
        print(f"Found existing captioned figure reference for Figure {fig_num}")
    
    # For each figure, find reference and add image only if not already properly captioned
    for figure_num, image_path in figure_paths.items():
        # Skip if we've already processed this figure
        if figure_num in processed_figures:
            print(f"Figure {figure_num} already has a proper caption, skipping")
            continue
            
        # Create relative path from CEREBRUM.md to the image
        rel_image_path = os.path.relpath(image_path, cerebrum_path.parent)
        
        # Pattern to find figure reference (various formats possible)
        fig_patterns = [
            f"Figure {figure_num}[^\\n]*\\n",
            f"Figure {figure_num}:[^\\n]*\\n",
            f"Figure {figure_num}\\.[^\\n]*\\n"
        ]
        
        match_found = False
        for pattern in fig_patterns:
            matches = list(re.finditer(pattern, content))
            if matches:
                # Only process the first match for each figure number
                match = matches[0]
                
                # Extract just the caption text, without the "Figure X: " prefix
                caption_text = match.group(0).strip()
                if caption_text.startswith(f"Figure {figure_num}: "):
                    caption_text = caption_text[len(f"Figure {figure_num}: "):]
                elif caption_text.startswith(f"Figure {figure_num}."):
                    caption_text = caption_text[len(f"Figure {figure_num}."):]
                
                # Check if image reference already exists to avoid duplicates
                next_content = content[match.end():match.end()+200]  # Look further ahead
                if f"![Figure {figure_num}]" in next_content:
                    print(f"Image reference for Figure {figure_num} already exists, skipping")
                    match_found = True
                    processed_figures.add(figure_num)
                    break
                
                # Insert image reference after the figure title
                img_ref = f"\n\n![Figure {figure_num}]({rel_image_path})\n\n"
                insert_pos = match.end()
                content = content[:insert_pos] + img_ref + content[insert_pos:]
                print(f"Added image reference for Figure {figure_num}")
                match_found = True
                processed_figures.add(figure_num)
                break
        
        if not match_found:
            print(f"Warning: Could not find reference for Figure {figure_num} in CEREBRUM.md")
    
    # Final cleanup: Remove any duplicate figure references without proper captions
    duplicate_pattern = r'!\[Figure (\d+)\]\(output/Figure_\1\.png\)\s*\n\s*\n(?!!\[Figure \1:)'
    content = re.sub(duplicate_pattern, '', content)
    
    # Write the updated content back to the file
    with open(cerebrum_path, 'w') as f:
        f.write(content)
    
    print(f"Updated {cerebrum_path} with image references")
    return True

def find_appendix_files(cerebrum_dir, appendix_pattern="*_APPENDIX.md"):
    """
    Find all appendix files in the specified directory matching the given pattern.
    
    Args:
        cerebrum_dir (str): Directory where appendix files are located.
        appendix_pattern (str): Glob pattern to match appendix files.
        
    Returns:
        list: List of absolute paths to appendix files, sorted alphabetically.
    """
    appendix_pattern_full = os.path.join(cerebrum_dir, appendix_pattern)
    appendix_files = glob.glob(appendix_pattern_full)
    
    # Sort files alphabetically to maintain consistent ordering
    appendix_files.sort()
    
    if not appendix_files:
        logging.warning(f"No appendix files found matching pattern: {appendix_pattern_full}")
    else:
        logging.info(f"Found {len(appendix_files)} appendix files: {', '.join(os.path.basename(f) for f in appendix_files)}")
    
    return appendix_files

def determine_appendix_order(appendix_files, explicit_order=None):
    """
    Determine the order of appendix files based on optional explicit ordering,
    with fallback to alphabetical order.
    
    Args:
        appendix_files (list): List of appendix file paths.
        explicit_order (list, optional): List of filenames in desired order.
        
    Returns:
        list: Ordered list of appendix file paths.
    """
    if not appendix_files:
        return []
        
    if explicit_order:
        # Map filenames to their full paths
        filename_to_path = {os.path.basename(f): f for f in appendix_files}
        
        # Build ordered list from explicit order, skipping any not found
        ordered_files = []
        for filename in explicit_order:
            if filename in filename_to_path:
                ordered_files.append(filename_to_path[filename])
                logging.info(f"Using explicitly ordered appendix: {filename}")
            else:
                logging.warning(f"Explicitly ordered appendix file not found: {filename}")
        
        # Add any remaining files not in explicit order
        for f in appendix_files:
            if os.path.basename(f) not in explicit_order:
                ordered_files.append(f)
                logging.info(f"Adding additional appendix not in explicit order: {os.path.basename(f)}")
        
        return ordered_files
    else:
        # Default to alphabetical ordering
        logging.info("Using alphabetical ordering for appendix files")
        return sorted(appendix_files)

def prepare_appendix_files(appendix_files):
    """
    Prepare appendix files by ensuring they have proper headings.
    Adds appendix designation to headings if needed and ensures appendices are properly numbered.
    
    Args:
        appendix_files (list): List of absolute paths to appendix files.
    """
    if not appendix_files:
        logging.info("No appendix files to prepare")
        return

    logging.info(f"Preparing {len(appendix_files)} appendix files...")
    
    # Use letter designations for appendices (A, B, C, ...)
    appendix_letters = [chr(65 + i) for i in range(len(appendix_files))]
    
    # Define mapping of expected appendix numbering
    # This ensures consistent numbering across the appendices
    expected_numbering = {
        "MATH_APPENDIX.md": "1",
        "NOVEL_CASES_APPENDIX.md": "2",
        "PRACTICAL_APPENDIX.md": "3"
    }
    
    for i, (appendix_file, letter) in enumerate(zip(appendix_files, appendix_letters)):
        filename = os.path.basename(appendix_file)
        number = expected_numbering.get(filename, str(i+1))
        
        with open(appendix_file, 'r') as f:
            content = f.read()
        
        # Make sure each appendix starts with a proper heading
        first_heading_match = re.search(r'^# (.+?)$', content, re.MULTILINE)
        if first_heading_match:
            heading_text = first_heading_match.group(1)
            
            # Check if heading already contains properly formatted appendix designation
            appendix_pattern = rf'Appendix {letter} {number}:'
            if re.search(appendix_pattern, heading_text):
                logging.info(f"Heading in {filename} already has correct appendix designation: '{heading_text}'")
            else:
                # Extract existing title without any appendix designation
                title_text = re.sub(r'^Appendix [A-Z] \d+:\s*', '', heading_text)
                title_text = re.sub(r'^Appendix [A-Z]:\s*', '', title_text)
                title_text = re.sub(r'^Appendix \d+:\s*', '', title_text)
                title_text = re.sub(r'^Appendix:\s*', '', title_text)
                
                # Create new heading with proper appendix designation
                new_heading = f"Appendix {letter} {number}: {title_text}"
                modified_content = content.replace(first_heading_match.group(0), f"# {new_heading}")
                
                with open(appendix_file, 'w') as f:
                    f.write(modified_content)
                logging.info(f"Updated heading in {filename}: '{heading_text}' → '{new_heading}'")
        else:
            # If no heading found, add one
            new_heading = f"# Appendix {letter} {number}"
            modified_content = f"{new_heading}\n\n{content}"
            
            with open(appendix_file, 'w') as f:
                f.write(modified_content)
            logging.warning(f"Added missing heading to {filename}: '{new_heading}'")
        
        # Remove any existing \appendix commands as they're now handled elsewhere
        if '\\appendix' in content:
            modified_content = content.replace('\\appendix', '')
            with open(appendix_file, 'w') as f:
                f.write(modified_content)
            logging.info(f"Removed '\\appendix' command from {filename}")

def parse_arguments():
    """Parse command line arguments for the script."""
    parser = argparse.ArgumentParser(description="Generate PDF from CEREBRUM Markdown files")
    parser.add_argument("--cerebrum-dir", default="CEREBRUM", 
                      help="Directory containing CEREBRUM.md and appendix files (default: CEREBRUM)")
    parser.add_argument("--main-file", default="CEREBRUM.md",
                      help="Main Markdown file to process (default: CEREBRUM.md)")
    parser.add_argument("--output-file", default="CEREBRUM.pdf",
                      help="Output PDF filename (default: CEREBRUM.pdf)")
    parser.add_argument("--appendix-pattern", default="*_APPENDIX.md",
                      help="Glob pattern to identify appendix files (default: *_APPENDIX.md)")
    parser.add_argument("--appendix-order", nargs="*",
                      help="Explicit order of appendix files (by filename)")
    parser.add_argument("--skip-mermaid", action="store_true",
                      help="Skip running the Mermaid rendering script")
    parser.add_argument("--debug", action="store_true",
                      help="Enable debug logging")
    
    return parser.parse_args()

if __name__ == "__main__":
    # Parse command line arguments
    args = parse_arguments()
    
    # Set debug logging if requested
    if args.debug:
        logging.getLogger().setLevel(logging.DEBUG)
        logging.debug("Debug logging enabled")
    
    project_root = get_project_root()
    cerebrum_dir = os.path.join(project_root, args.cerebrum_dir)
    
    # --- Run Mermaid Script First (unless skipped) ---
    if not args.skip_mermaid:
        mermaid_script_name = "render_mermaids.py"
        mermaid_script_path = os.path.join(project_root, "tools", mermaid_script_name)

        if not os.path.exists(mermaid_script_path):
            logging.error(f"Mermaid script not found at: {mermaid_script_path}")
            sys.exit(1)

        if not run_mermaid_script(mermaid_script_path, project_root):
            logging.error("Aborting PDF generation due to Mermaid script failure.")
            sys.exit(1)
    else:
        logging.info("Skipping Mermaid rendering as requested")
    # --------------------------------

    # --- Proceed with PDF Generation ---
    # Define paths relative to the determined project root
    markdown_relative_path = os.path.join(args.cerebrum_dir, args.main_file)
    output_pdf_relative_path = os.path.join(args.cerebrum_dir, args.output_file)
    # The resource path is the directory containing the 'output' folder referenced in the markdown
    resource_relative_path = args.cerebrum_dir

    # Construct absolute paths based on the project root
    markdown_abs_path = os.path.join(project_root, markdown_relative_path)
    output_pdf_abs_path = os.path.join(project_root, output_pdf_relative_path)
    resource_abs_path = os.path.join(project_root, resource_relative_path)
    
    # Find appendix files
    all_appendix_files = find_appendix_files(cerebrum_dir, args.appendix_pattern)
    
    # Define explicit ordering of appendices (if not provided via command line)
    if not args.appendix_order:
        # IMPORTANT: This is the canonical order of appendices.
        # The numbering and lettering should match:
        # - Appendix A 1: MATH_APPENDIX.md (Mathematical Formalization)
        # - Appendix B 2: NOVEL_CASES_APPENDIX.md (Novel Linguistic Cases)
        # - Appendix C 3: PRACTICAL_APPENDIX.md (Practical Applications)
        # - Appendix D 4: RELATED_WORK_APPENDIX.md (Related Work - Detailed Analysis)
        # - Appendix E 5: CATEGORY_THEORY_APPENDIX.md (Category-Theoretic Formalization)
        # - Appendix F 6: FUTURE_DIRECTIONS_APPENDIX.md (Future Directions for CEREBRUM Research)
        args.appendix_order = [
            "MATH_APPENDIX.md",             # Appendix A 1: First appendix
            "NOVEL_CASES_APPENDIX.md",      # Appendix B 2: Second appendix
            "PRACTICAL_APPENDIX.md",        # Appendix C 3: Third appendix
            "RELATED_WORK_APPENDIX.md",     # Appendix D 4: Fourth appendix
            "CATEGORY_THEORY_APPENDIX.md",  # Appendix E 5: Fifth appendix
            "FUTURE_DIRECTIONS_APPENDIX.md" # Appendix F 6: Sixth appendix
        ]
        logging.info(f"Using canonical appendix order: {', '.join(args.appendix_order)}")
    
    # Order the appendix files
    ordered_appendix_files = determine_appendix_order(all_appendix_files, args.appendix_order)
    
    # Prepare appendix files
    prepare_appendix_files(ordered_appendix_files)

    logging.info(f"Project Root: {project_root}")
    logging.info(f"Markdown Source: {markdown_abs_path}")
    logging.info(f"PDF Output: {output_pdf_abs_path}")
    logging.info(f"Resource Path: {resource_abs_path}")
    logging.info(f"Appendix Files ({len(ordered_appendix_files)}):")
    for i, f in enumerate(ordered_appendix_files):
        logging.info(f"  {i+1}. {os.path.basename(f)}")

    if render_markdown_to_pdf(markdown_abs_path, ordered_appendix_files, output_pdf_abs_path, resource_abs_path):
        logging.info("PDF generation completed successfully.")
        sys.exit(0) # Indicate success
    else:
        logging.error("PDF generation failed.")
        sys.exit(1) # Indicate failure 