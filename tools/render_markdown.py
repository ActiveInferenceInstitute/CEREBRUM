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
    
    # Create a simpler two-step process
    # 1. First, convert main markdown to PDF
    # 2. Then create appendix PDF and combine them
    
    # Step 1: Convert main markdown to PDF
    main_command = [
        "pandoc",
        markdown_abs_path,
        "--resource-path", resource_abs_path,
        "-o", f"{output_pdf_abs_path}.main.pdf",
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
    ]
    
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
    
    # Step 2: Prepare command for appendices
    if appendix_files:
        appendix_command = [
            "pandoc",
        ]
        
        for appendix_file in appendix_files:
            appendix_command.append(appendix_file)
            
        appendix_command.extend([
            "--resource-path", resource_abs_path,
            "-o", f"{output_pdf_abs_path}.appendix.tex",
            "--standalone",
            "--number-sections",
            "--top-level-division=chapter",
            "-V", "header-includes=\\usepackage{appendix}"
        ])
        
        # Add necessary LaTeX packages for appendices
        for pkg in latex_packages:
            appendix_command.extend(["-V", f"header-includes={pkg}"])
            
        # Add a special header to mark these as appendices
        appendix_command.extend([
            "-B", "\\appendix\n"
        ])
    
    logging.info("Starting PDF generation process...")
    logging.info(f"Main content command: {' '.join(main_command)}")
    if appendix_files:
        logging.info(f"Appendix command: {' '.join(appendix_command)}")
    
    try:
        # Execute main markdown to PDF conversion
        project_root = os.path.dirname(resource_abs_path)
        logging.info("Generating main document PDF...")
        result = subprocess.run(
            main_command,
            check=True,
            capture_output=True,
            text=True,
            cwd=project_root
        )
        logging.info(f"Generated main document PDF: {output_pdf_abs_path}.main.pdf")
        
        # If we have appendices, generate those too
        if appendix_files:
            logging.info("Generating appendix content...")
            try:
                appendix_result = subprocess.run(
                    appendix_command,
                    check=True,
                    capture_output=True,
                    text=True,
                    cwd=project_root
                )
                logging.info(f"Generated appendix content: {output_pdf_abs_path}.appendix.tex")
                
                # Now combine the PDFs
                logging.info("Combining main document and appendices...")
                
                # Create a LaTeX document to combine them
                combine_tex = f"""
\\documentclass{{article}}
\\usepackage{{pdfpages}}
\\begin{{document}}
\\includepdf[pages=-]{{"{os.path.basename(output_pdf_abs_path)}.main.pdf"}}
\\appendix
\\include{{"{os.path.basename(output_pdf_abs_path)}.appendix"}}
\\end{{document}}
"""
                
                with open(os.path.join(project_root, f"{output_pdf_abs_path}.combine.tex"), 'w') as f:
                    f.write(combine_tex)
                
                # Compile the combined document
                logging.info("Compiling final PDF...")
                final_command = [
                    pdf_engine,
                    f"{os.path.basename(output_pdf_abs_path)}.combine.tex"
                ]
                
                final_result = subprocess.run(
                    final_command,
                    check=True,
                    capture_output=True,
                    text=True,
                    cwd=os.path.dirname(output_pdf_abs_path)
                )
                
                # Run twice to ensure TOC is updated correctly
                final_result = subprocess.run(
                    final_command,
                    check=True,
                    capture_output=True,
                    text=True,
                    cwd=os.path.dirname(output_pdf_abs_path)
                )
                
                # Move the combined PDF to the final output location
                shutil.move(
                    os.path.join(os.path.dirname(output_pdf_abs_path), f"{os.path.basename(output_pdf_abs_path)}.combine.pdf"),
                    output_pdf_abs_path
                )
                
                logging.info(f"Successfully generated combined PDF: {output_pdf_abs_path}")
            except Exception as e:
                logging.error(f"Error processing appendices: {e}")
                # If appendix processing fails, just use the main PDF
                shutil.move(
                    f"{output_pdf_abs_path}.main.pdf",
                    output_pdf_abs_path
                )
                logging.info(f"Using main document as final PDF due to appendix processing error: {output_pdf_abs_path}")
        else:
            # If no appendices, just use the main PDF
            shutil.move(
                f"{output_pdf_abs_path}.main.pdf",
                output_pdf_abs_path
            )
            logging.info(f"No appendices to process, using main document as final PDF: {output_pdf_abs_path}")
        
        # Clean up temporary files
        temp_files = [
            f"{output_pdf_abs_path}.main.pdf",
            f"{output_pdf_abs_path}.appendix.tex",
            f"{output_pdf_abs_path}.combine.tex",
            f"{output_pdf_abs_path}.combine.aux",
            f"{output_pdf_abs_path}.combine.log"
        ]
        
        for temp_file in temp_files:
            if os.path.exists(temp_file):
                try:
                    os.unlink(temp_file)
                except Exception as e:
                    logging.warning(f"Could not delete temporary file {temp_file}: {e}")
        
        logging.info("PDF generation completed successfully!")
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

def prepare_appendix_files(appendix_files):
    """
    Prepare appendix files by ensuring they have proper headings.
    Our new approach doesn't require \appendix commands in the Markdown files,
    but we still need to make sure the headings are properly formatted.
    """
    if not appendix_files:
        logging.info("No appendix files to prepare")
        return

    logging.info(f"Preparing {len(appendix_files)} appendix files...")
    for appendix_file in appendix_files:
        with open(appendix_file, 'r') as f:
            content = f.read()
        
        # Make sure each appendix starts with a proper heading
        first_heading_match = re.search(r'^# (.+?)$', content, re.MULTILINE)
        if first_heading_match:
            heading_text = first_heading_match.group(1)
            if not "appendix" in heading_text.lower():
                # Add "Appendix" to the heading if it's not already there
                new_heading = f"# {heading_text} (Appendix)"
                modified_content = content.replace(first_heading_match.group(0), new_heading)
                
                with open(appendix_file, 'w') as f:
                    f.write(modified_content)
                logging.info(f"Updated heading in {appendix_file}: '{heading_text}' → '{heading_text} (Appendix)'")
            else:
                logging.info(f"Heading in {appendix_file} already contains 'Appendix': '{heading_text}'")
        else:
            logging.warning(f"Could not find a top-level heading in {appendix_file}")
        
        # Remove any existing \appendix commands as they're now handled elsewhere
        if '\\appendix' in content:
            modified_content = content.replace('\\appendix', '')
            with open(appendix_file, 'w') as f:
                f.write(modified_content)
            logging.info(f"Removed '\\appendix' command from {appendix_file}")

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
    
    # Prepare appendix files
    prepare_appendix_files(appendix_files)

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