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

    # Using xelatex is often more robust for complex documents, unicode, and fonts
    pdf_engine = "xelatex" 
    
    # LaTeX packages needed for math symbols and environments
    header_includes = [
        "\\usepackage{amsmath}", # For general math enhancements
        "\\usepackage{amssymb}",  # For symbols like \mathbb
        "\\usepackage{mathtools}", # Enhanced math tools for better equation rendering
        "\\usepackage{bm}",      # For bold math symbols
        "\\usepackage{caption}",  # For better caption control
        "\\usepackage{booktabs}", # For better table formatting
        "\\usepackage{tabularx}", # For tables with wrapping text
        # Command to make table font smaller (changed from footnotesize to tiny)
        "\\let\\oldtabular\\tabular",
        "\\let\\endoldtabular\\endtabular",
        "\\renewenvironment{tabular}{\\tiny\\oldtabular}{\\endoldtabular}",
        # Improve front matter styling
        "\\usepackage{titling}",  # For title page customization
        "\\usepackage{titlesec}", # For section title formatting
        "\\pretitle{\\begin{center}\\LARGE\\bfseries}",
        "\\posttitle{\\end{center}\\vspace{0.5em}}",
        "\\preauthor{\\begin{center}\\large}",
        "\\postauthor{\\end{center}}",
        "\\predate{\\begin{center}\\large}",
        "\\postdate{\\end{center}\\vspace{2em}}",
        # Add a rule after the title
        "\\renewcommand{\\maketitlehookd}{\\vspace{1em}\\begin{center}\\footnotesize -.-. . .-. . -... .-. ..- -- ---... / -.-. .- ... . -....- . -. .- -... .-.. . -.. / .-. . .- ... --- -. .. -. --. / . -. --. .. -. . / .-- .. - .... / -... .- -.-- . ... .. .- -. / .-. . .--. .-. . ... . -. - .- - .. --- -. ... / ..-. --- .-. / ..- -. .. ..-. .. . -.. / -- --- -.. . .-.. .. -. --. \\end{center}\\vspace{1em}}",
        # Add DOI information
        "\\usepackage{hyperref}",
        # Move DOI display to appear before the title hook (before divider)
        "\\AtBeginDocument{\\let\\oldmaketitle\\maketitle\\renewcommand{\\maketitle}{\\oldmaketitle\\begin{center}\\large DOI: \\href{https://doi.org/10.5281/zenodo.15170908}{10.5281/zenodo.15170908}\\end{center}\\maketitlehookd}}"
    ]

    # Construct the pandoc command using absolute paths
    command = [
        "pandoc",
        markdown_abs_path,
    ]
    
    # Add all appendix files to the command
    for appendix_path in appendix_files:
        command.append(appendix_path)
    
    # Add other pandoc options
    command.extend([
        "--resource-path", resource_abs_path,  # Crucial for resolving relative image paths
        "-o", output_pdf_abs_path, # Output PDF file
        f"--pdf-engine={pdf_engine}", # Use specified PDF engine
        "--standalone",             # Create a standalone document
        "--toc",                    # Generate Table of Contents
        "--toc-depth=3",           # Control TOC depth
        "--number-sections",        # Number sections (e.g., 1., 1.1, 1.1.1)
        
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
        
        # Improved math rendering
        "--mathjax",
    ])

    # Add each header include as a separate -V flag
    for include in header_includes:
        command.extend(["-V", f"header-includes={include}"])

    logging.info(f"Attempting to render PDF...")
    logging.info(f"Pandoc command: {' '.join(command)}")

    try:
        # Execute the pandoc command
        # We run it from the project root for consistency, although absolute paths make CWD less critical
        project_root = os.path.dirname(resource_abs_path) # Assuming resource path is <root>/CEREBRUM
        result = subprocess.run(
            command,
            check=True,             # Raise CalledProcessError if pandoc fails
            capture_output=True,    # Capture stdout and stderr
            text=True,              # Decode stdout/stderr as text
            cwd=project_root        # Set working directory (optional with abs paths, but good practice)
        )
        logging.info(f"Successfully rendered PDF: {output_pdf_abs_path}")
        # Log stdout/stderr only if debug level is enabled or if there's notable output
        if result.stdout:
             logging.debug("Pandoc stdout:\n" + result.stdout)
        if result.stderr:
             logging.debug("Pandoc stderr:\n" + result.stderr) # Often contains LaTeX warnings
        return True
        
    except FileNotFoundError:
        logging.error(f"Error: 'pandoc' command not found.")
        logging.error("Please ensure pandoc is installed and in your system's PATH.")
        logging.error("You also need a LaTeX distribution (like TeX Live or MiKTeX) with the 'xelatex' engine.")
        return False
    except subprocess.CalledProcessError as e:
        logging.error(f"Pandoc failed with return code {e.returncode}.")
        logging.error(f"Command executed: {' '.join(e.cmd)}")
        logging.error("Pandoc stderr:\n" + e.stderr)
        if e.stdout: # Log stdout too, might contain useful info
            logging.error("Pandoc stdout:\n" + e.stdout)
        logging.error("Check the pandoc output above for LaTeX errors. Missing packages or font issues are common.")
        return False
    except Exception as e:
        logging.error(f"An unexpected error occurred during rendering: {e}")
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

if __name__ == "__main__":
    project_root = get_project_root()
    
    # --- Run Mermaid Script First ---
    mermaid_script_name = "render_mermaids.py"
    mermaid_script_path = os.path.join(project_root, "tools", mermaid_script_name)

    if not os.path.exists(mermaid_script_path):
        logging.error(f"Mermaid script not found at: {mermaid_script_path}")
        sys.exit(1)

    if not run_mermaid_script(mermaid_script_path, project_root):
        logging.error("Aborting PDF generation due to Mermaid script failure.")
        sys.exit(1)
    # --------------------------------

    # --- Proceed with PDF Generation ---
    # Define paths relative to the determined project root
    markdown_relative_path = os.path.join("CEREBRUM", "CEREBRUM.md")
    appendix1_relative_path = os.path.join("CEREBRUM", "MATH_APPENDIX.md")
    appendix2_relative_path = os.path.join("CEREBRUM", "NOVEL_CASES_APPENDIX.md")
    output_pdf_relative_path = os.path.join("CEREBRUM", "CEREBRUM.pdf")
    # The resource path is the directory containing the 'output' folder referenced in the markdown
    resource_relative_path = "CEREBRUM"

    # Construct absolute paths based on the project root
    markdown_abs_path = os.path.join(project_root, markdown_relative_path)
    appendix1_abs_path = os.path.join(project_root, appendix1_relative_path)
    appendix2_abs_path = os.path.join(project_root, appendix2_relative_path)
    output_pdf_abs_path = os.path.join(project_root, output_pdf_relative_path)
    resource_abs_path = os.path.join(project_root, resource_relative_path)
    
    # Create a list of appendix files
    appendix_files = [appendix1_abs_path, appendix2_abs_path]

    logging.info(f"Project Root: {project_root}")
    logging.info(f"Markdown Source: {markdown_abs_path}")
    logging.info(f"Appendix 1 (Math): {appendix1_abs_path}")
    logging.info(f"Appendix 2 (Novel Cases): {appendix2_abs_path}")
    logging.info(f"PDF Output: {output_pdf_abs_path}")
    logging.info(f"Resource Path: {resource_abs_path}")

    if render_markdown_to_pdf(markdown_abs_path, appendix_files, output_pdf_abs_path, resource_abs_path):
        logging.info("PDF generation completed successfully.")
        sys.exit(0) # Indicate success
    else:
        logging.error("PDF generation failed.")
        sys.exit(1) # Indicate failure 