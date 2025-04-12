import os
import glob
import re
import logging
import subprocess
import tempfile
import shutil

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

# --- Configuration ---
COMPONENTS_DIR = "paper/components/main_text"
# Figure and Supplemental paths are relative to the base 'paper/components'
BASE_COMPONENTS_DIR = "paper/components"
FIGURES_DIR = os.path.join(BASE_COMPONENTS_DIR, "figures")
SUPPLEMENTAL_DIR = os.path.join(BASE_COMPONENTS_DIR, "supplemental_sections")
OUTPUT_DIR = "paper/output"
os.makedirs(OUTPUT_DIR, exist_ok=True) # Ensure output directory exists early

OUTPUT_MD_FILENAME = "assembled_paper.md"
OUTPUT_HTML_FILENAME = "assembled_paper.html"
OUTPUT_PDF_FILENAME = "assembled_paper.pdf"
PANDOC_CMD = "pandoc"
MERMAID_CMD = "mmdc" # Command for Mermaid CLI

# --- Helper Functions ---

def extract_number(filename):
    """Extracts the leading number from a filename like '1_title.md'."""
    base = os.path.basename(filename)
    match = re.match(r"(\d+)_.*\.md", base)
    if match:
        return int(match.group(1))
    logging.warning(f"Could not extract leading number from component filename: {base}. Placing it at the end.")
    return float('inf')

def extract_figure_number(filename):
    """Extracts the number from a filename like 'Figure_1.md' or 'Figure_10.md'."""
    base = os.path.basename(filename)
    match = re.match(r"Figure_(\d+)\.md", base, re.IGNORECASE)
    if match:
        return int(match.group(1))
    # Return None if no match, handle downstream
    return None

def extract_figure_caption(figure_file_path):
    """
    Extracts the full figure caption from a Figure_X.md file.
    Returns the caption text or None if caption cannot be found.
    """
    try:
        with open(figure_file_path, 'r', encoding='utf-8') as f:
            content = f.read()
            
            # Get the figure number from the file name
            figure_number = extract_figure_number(figure_file_path)
            
            # First try to find the paragraph after the mermaid code block
            caption_match = re.search(r"```\s*\n(.*?)$", content, re.DOTALL | re.MULTILINE)
            
            if caption_match:
                caption_text = caption_match.group(1).strip()
                # Remove any "Figure X:" prefix from the caption
                caption_text = re.sub(r'^Figure\s+\d+[\.:]\s*', '', caption_text, flags=re.IGNORECASE)
                return caption_text
            
            # Alternative approach: look for the last paragraph in the file
            paragraphs = content.split('\n\n')
            last_paragraph = paragraphs[-1].strip()
            if last_paragraph:
                # Remove any "Figure X:" prefix from the caption
                caption_text = re.sub(r'^Figure\s+\d+[\.:]\s*', '', last_paragraph, flags=re.IGNORECASE)
                return caption_text
                
            logging.warning(f"Could not extract caption from {figure_file_path}")
            return None
    except Exception as e:
        logging.error(f"Error reading figure caption from {figure_file_path}: {e}")
        return None

def fix_figure_references(content):
    """
    Replaces figure references in format @fig:figX with "Figure X".
    """
    # Replace @fig:figX with Figure X
    content = re.sub(r'@fig:fig(\d+)', r'Figure \1', content)
    
    # Also handle other reference formats if needed
    content = re.sub(r'\(#fig:fig(\d+)\)', r'(Figure \1)', content)
    
    return content

def map_figure_captions():
    """
    Creates a mapping of figure numbers to their full captions.
    Returns a dictionary {figure_number: caption}.
    """
    caption_map = {}
    figure_md_files = glob.glob(os.path.join(FIGURES_DIR, "Figure_*.md"))
    
    for figure_md_path in figure_md_files:
        figure_number = extract_figure_number(figure_md_path)
        if figure_number is not None:
            caption = extract_figure_caption(figure_md_path)
            if caption:
                caption_map[figure_number] = caption
                logging.debug(f"Extracted caption for Figure {figure_number}")
            else:
                logging.warning(f"Failed to extract caption for Figure {figure_number}")
    
    logging.info(f"Extracted {len(caption_map)} figure captions")
    return caption_map

def map_figure_id_to_number(all_components_files):
    """
    Creates a mapping of figure IDs (e.g., #fig:fig6) to their correct sequence numbers
    based on the order they appear in the document.
    Returns a dictionary {figure_id: sequence_number}.
    """
    id_to_number = {}
    current_figure_number = 1

    for file_path in all_components_files:
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Find all figure references with ID tags: ![...](Figure_X.png){#fig:figY}
            # This pattern will match the ID and the referenced PNG
            figure_matches = re.finditer(r'!\[.*?\]\((Figure_(\d+)\.png)\)\{(#fig:fig(\d+))\}', content)
            
            for match in figure_matches:
                image_path = match.group(1)  # Figure_X.png
                image_num = int(match.group(2))  # X from Figure_X.png
                fig_id = match.group(3)  # #fig:figY
                ref_num = int(match.group(4))  # Y from #fig:figY
                
                if fig_id not in id_to_number:
                    id_to_number[fig_id] = current_figure_number
                    current_figure_number += 1
                    logging.info(f"Mapping figure ID {fig_id} to sequence number {id_to_number[fig_id]} (image: {image_path})")
        
        except Exception as e:
            logging.error(f"Error processing file {file_path} for figure ID mapping: {e}")
    
    return id_to_number

# --- Mermaid Rendering ---
def render_mermaid_figures():
    """
    Finds Mermaid definitions in Figure_*.md files in FIGURES_DIR and renders them to PNG
    images in the OUTPUT_DIR using the Mermaid CLI (mmdc).
    Returns True if all figures are rendered successfully, False otherwise.
    """
    logging.info(f"Looking for figure definition Markdown files in: {FIGURES_DIR}")
    figure_md_files = glob.glob(os.path.join(FIGURES_DIR, "Figure_*.md"))

    if not figure_md_files:
        logging.warning(f"No figure Markdown files (e.g., 'Figure_*.md') found in {FIGURES_DIR}. Skipping Mermaid rendering.")
        return True # Nothing to render is considered success in this context

    logging.info(f"Found {len(figure_md_files)} figure definition files. Attempting to render PNGs.")
    all_successful = True

    # Sort for consistent processing order
    figure_md_files.sort(key=lambda f: extract_figure_number(f) if extract_figure_number(f) is not None else float('inf'))

    # Ensure output directory exists
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    logging.info(f"Ensuring output directory exists: {os.path.abspath(OUTPUT_DIR)}")

    for figure_md_path in figure_md_files:
        figure_basename = os.path.basename(figure_md_path)
        figure_number = extract_figure_number(figure_basename)

        if figure_number is None:
            logging.warning(f"Could not extract figure number from {figure_basename}. Skipping rendering.")
            continue # Skip this file

        output_png_filename = f"Figure_{figure_number}.png"
        output_png_path = os.path.join(OUTPUT_DIR, output_png_filename)
        
        # Log absolute paths for debugging
        logging.info(f"Processing figure: {os.path.abspath(figure_md_path)}")
        logging.info(f"Output will be saved to: {os.path.abspath(output_png_path)}")

        try:
            with open(figure_md_path, 'r', encoding='utf-8') as infile:
                content = infile.read()

            # Extract content within ```mermaid ... ``` block
            match = re.search(r"```mermaid(.*?)```", content, re.DOTALL | re.IGNORECASE)
            if not match:
                logging.warning(f"No ```mermaid``` block found in {figure_basename}. Skipping.")
                continue # Skip if no mermaid block

            mermaid_code = match.group(1).strip()
            if not mermaid_code:
                 logging.warning(f"Empty ```mermaid``` block found in {figure_basename}. Skipping.")
                 continue

            # Log the extracted mermaid code for debugging
            logging.debug(f"Extracted mermaid code from {figure_basename}:\n{mermaid_code[:100]}...")

            # Use temporary file for mmdc input with explicit cleanup
            tmp_mmd_path = os.path.join(tempfile.gettempdir(), f"figure_{figure_number}.mmd")
            try:
                with open(tmp_mmd_path, 'w', encoding='utf-8') as tmp_mmd:
                    tmp_mmd.write(mermaid_code)
                
                # Construct mmdc command with absolute paths
                mmdc_command = [
                    MERMAID_CMD,
                    "-i", os.path.abspath(tmp_mmd_path),
                    "-o", os.path.abspath(output_png_path),
                    "-b", "transparent",  # Use transparent background
                    "-w", "1200",  # Set width (adjust as needed)
                ]

                logging.info(f"Running command: {' '.join(mmdc_command)}")
                result = subprocess.run(mmdc_command, check=False, capture_output=True, text=True, encoding='utf-8')

                if result.returncode != 0:
                    logging.error(f"Mermaid CLI failed for {figure_basename} (Exit code: {result.returncode}):")
                    logging.error(f"Stderr: {result.stderr.strip()}")
                    logging.error(f"Stdout: {result.stdout.strip()}")
                    all_successful = False
                else:
                    # Verify the output file exists and has content
                    if os.path.exists(output_png_path) and os.path.getsize(output_png_path) > 0:
                        logging.info(f"Successfully rendered {output_png_filename} ({os.path.getsize(output_png_path)} bytes)")
                    else:
                        logging.error(f"Mermaid CLI reported success but no valid PNG was created at {output_png_path}")
                        all_successful = False
                        
                    if result.stderr: 
                        logging.warning(f"Mermaid CLI stderr for {figure_basename}: {result.stderr.strip()}")
            finally:
                # Clean up temporary file
                if os.path.exists(tmp_mmd_path):
                    try:
                        os.remove(tmp_mmd_path)
                    except Exception as e_rem:
                        logging.warning(f"Could not remove temporary file {tmp_mmd_path}: {e_rem}")

        except FileNotFoundError as e:
            if "mmdc" in str(e):
                logging.error(f"Mermaid CLI command '{MERMAID_CMD}' not found. Please ensure it's installed (e.g., 'npm install -g @mermaid-js/mermaid-cli') and in your PATH.")
            else:
                logging.error(f"File not found error: {e}")
            all_successful = False
            break
        except Exception as e:
            logging.error(f"An unexpected error occurred while processing {figure_basename}: {e}")
            all_successful = False

    return all_successful

# --- Main Assembly Logic ---
def assemble_markdown():
    """
    Finds, sorts, and assembles Markdown components into a single output Markdown file.
    Properly updates figure references based on their order in the document.
    Returns the path to the assembled file on success, None otherwise.
    """
    output_md_path = os.path.join(OUTPUT_DIR, OUTPUT_MD_FILENAME)

    # --- Find Main Text Components ---
    logging.info(f"Looking for main text component Markdown files in: {COMPONENTS_DIR}")
    main_component_files = [f for f in glob.glob(os.path.join(COMPONENTS_DIR, "*.md"))
                           if os.path.isfile(f) and re.match(r"\d+_.*\.md", os.path.basename(f))]

    if not main_component_files:
        logging.error(f"No main text component Markdown files (e.g., '1_*.md') found in {COMPONENTS_DIR}. Cannot assemble paper.")
        return None

    logging.info(f"Found {len(main_component_files)} main text component files. Sorting them numerically.")
    main_component_files.sort(key=extract_number)

    # --- Find Supplemental Components --- 
    logging.info(f"Looking for supplemental Markdown files in: {SUPPLEMENTAL_DIR}")
    supplemental_files = [f for f in glob.glob(os.path.join(SUPPLEMENTAL_DIR, "Supplement_*.md"))
                         if os.path.isfile(f)]

    if not supplemental_files:
        logging.warning(f"No supplemental Markdown files found in {SUPPLEMENTAL_DIR}. Continuing without supplements.")
    else:
        logging.info(f"Found {len(supplemental_files)} supplemental files.")
        def extract_supplement_number(filename):
            base = os.path.basename(filename)
            match = re.match(r"Supplement_(\d+)_.*\.md", base, re.IGNORECASE)
            if match:
                return int(match.group(1))
            return float('inf') 
        supplemental_files.sort(key=extract_supplement_number)

    # Combine all files to analyze for figure references
    all_files_to_assemble = main_component_files + supplemental_files
    
    # --- Map Figure IDs to sequence numbers ---
    logging.info("Mapping figure IDs to sequence numbers...")
    figure_id_map = map_figure_id_to_number(all_files_to_assemble)
    
    # --- Extract Figure Captions ---
    logging.info("Extracting figure captions from Figure_*.md files...")
    caption_map = map_figure_captions()

    # --- Copy rendered figure files to ensure they're accessible ---
    logging.info("Ensuring rendered figures are available in the output directory...")
    figure_files = glob.glob(os.path.join(OUTPUT_DIR, "Figure_*.png"))
    if figure_files:
        logging.info(f"Found {len(figure_files)} rendered figure files in output directory.")
    else:
        logging.warning("No rendered figure files found in output directory.")

    # --- Assemble Content (Main + Supplemental) ---
    assembled_content = []
    logging.info("Reading and assembling main and supplemental components...")

    # Track if we're in the main components or supplements
    in_supplements = False

    for component_file in all_files_to_assemble:
        try:
            # Check if this is a supplement file and we haven't seen supplements yet
            is_supplement = "Supplement_" in os.path.basename(component_file)
            if is_supplement and not in_supplements:
                # Add a page break before the first supplement
                assembled_content.append("\n\n\\newpage\n\n")
                in_supplements = True
            elif is_supplement and in_supplements:
                # Add a page break between supplements
                assembled_content.append("\n\n\\newpage\n\n")
            
            with open(component_file, 'r', encoding='utf-8') as infile:
                logging.info(f"Reading component: {os.path.basename(component_file)}")
                content = infile.read()
                
                # Fix @fig:figX references to "Figure X"
                content = fix_figure_references(content)
                
                # Update image paths to use the correct relative path for Pandoc
                # Ensure figures are referenced correctly regardless of the document location
                for fig_id, seq_num in figure_id_map.items():
                    # Extract the reference number from #fig:figY
                    ref_match = re.match(r'#fig:fig(\d+)', fig_id)
                    if ref_match:
                        ref_num = ref_match.group(1)
                        
                        # Find all image tags that use this figure ID
                        img_pattern = rf'!\[(.*?)\]\((Figure_\d+\.png)\)\{{{fig_id}\}}'
                        img_matches = re.finditer(img_pattern, content)
                        
                        for img_match in img_matches:
                            alt_text = img_match.group(1)
                            img_path = img_match.group(2)
                            
                            # Extract image number from filename (e.g., 9 from Figure_9.png)
                            img_num_match = re.match(r'Figure_(\d+)\.png', img_path)
                            if img_num_match:
                                img_num = int(img_num_match.group(1))
                                
                                # Get the caption for this image
                                if img_num in caption_map:
                                    # Clean the caption by removing any "Figure X:" prefix
                                    caption = caption_map[img_num]
                                    clean_caption = re.sub(r'^Figure\s+\d+[\.:]\s*', '', caption, flags=re.IGNORECASE)
                                    
                                    # Create new image tag with proper sequence number
                                    # Use relative path to the figure in OUTPUT_DIR
                                    new_img_tag = f'![Figure {seq_num}. {clean_caption}]({img_path})'
                                    original_tag = img_match.group(0)
                                    content = content.replace(original_tag, new_img_tag)
                                    
                                    logging.debug(f"Replaced figure reference: {original_tag} -> {new_img_tag}")
                
                assembled_content.append(content)
                assembled_content.append("\n\n") # Add space between files
        except Exception as e:
            logging.error(f"Error reading component file {component_file}: {e}")
            return None # Stop assembly on error

    # --- Combine content ---
    if not assembled_content:
        logging.error("No content assembled.")
        return None

    full_content = "".join(assembled_content).strip()

    logging.info(f"Writing assembled content to: {output_md_path}")
    try:
        with open(output_md_path, 'w', encoding='utf-8') as outfile:
            outfile.write(full_content)
        logging.info("Markdown assembly completed successfully.")
        return output_md_path
    except Exception as e:
        logging.error(f"Error writing assembled file {output_md_path}: {e}")
        return None

# --- Conversion Functions ---

def convert_to_html(md_path, html_path):
    """Converts a Markdown file to HTML using Pandoc."""
    # Use absolute paths for robustness
    md_abs_path = os.path.abspath(md_path)
    html_abs_path = os.path.abspath(html_path)
    
    # Define the directory containing the Markdown file for running pandoc
    base_dir = os.path.dirname(md_abs_path)

    logging.info(f"Converting {os.path.basename(md_path)} to {os.path.basename(html_path)} using Pandoc.")
    # Copy all figure PNGs to be alongside the HTML for proper rendering
    logging.info("Ensuring figures are accessible for HTML conversion...")

    command = [
        PANDOC_CMD,
        md_abs_path,
        "-o", html_abs_path,
        "--standalone",
        "--toc",
        "--toc-depth=3",
        "--metadata", "title=CEREBRUM: Case-Enabled Reasoning Engine with Bayesian Representations for Unified Modeling",
    ]
    try:
        # Run pandoc in the output directory where both the MD file and figures are located
        result = subprocess.run(command, check=True, capture_output=True, text=True, encoding='utf-8', cwd=base_dir)
        logging.info(f"Successfully created {html_path}")
        if result.stderr:
            logging.warning(f"Pandoc stderr (HTML conversion): {result.stderr.strip()}")
        
        # Verify the HTML file exists and has reasonable content
        if os.path.exists(html_abs_path) and os.path.getsize(html_abs_path) > 0:
            logging.info(f"Verified HTML output exists: {html_abs_path} ({os.path.getsize(html_abs_path)} bytes)")
        else:
            logging.error(f"HTML output file is missing or empty: {html_abs_path}")
            return False
            
        return True
    except FileNotFoundError:
        logging.error(f"Pandoc command '{PANDOC_CMD}' not found. Please ensure Pandoc is installed and in your PATH.")
        return False
    except subprocess.CalledProcessError as e:
        logging.error(f"Pandoc failed during HTML conversion (Exit code: {e.returncode}):")
        logging.error(f"Command: {' '.join(e.cmd)}")
        logging.error(f"Stderr: {e.stderr.strip()}")
        return False
    except Exception as e:
         logging.error(f"An unexpected error occurred during HTML conversion: {e}")
         return False

def convert_to_pdf(md_path, pdf_path):
    """Converts a Markdown file to PDF using Pandoc (requires LaTeX)."""
    # Use absolute paths for robustness
    md_abs_path = os.path.abspath(md_path)
    pdf_abs_path = os.path.abspath(pdf_path)
    base_dir = os.path.dirname(md_abs_path) # Directory containing the assembled MD

    logging.info(f"Converting {os.path.basename(md_path)} to {os.path.basename(pdf_path)} using Pandoc (via XeLaTeX).")
    
    logging.info("Ensuring figures are accessible for PDF conversion...")

    command = [
        PANDOC_CMD,
        md_abs_path, # Input file
        "-o", pdf_abs_path, # Output file
        "--pdf-engine=xelatex",
        "--toc",
        "--toc-depth=3",
        "--standalone",
        # Simple styling options
        "-V", "colorlinks=true",
        "-V", "linkcolor=blue",
        "-V", "urlcolor=blue",
        "-V", "geometry=margin=1in",
        "-V", "mainfont=DejaVu Serif",
        "-V", "monofont=DejaVu Sans Mono",
    ]
    try:
        # Run pandoc from the directory containing the assembled MD file
        # This makes relative image paths work correctly
        result = subprocess.run(command, check=True, capture_output=True, text=True, encoding='utf-8', cwd=base_dir)
        logging.info(f"Successfully created {pdf_path}")
        if result.stderr:
            logging.warning(f"Pandoc stderr (PDF conversion): {result.stderr.strip()}")
        
        # Verify the PDF file exists and has reasonable content
        if os.path.exists(pdf_abs_path) and os.path.getsize(pdf_abs_path) > 0:
            logging.info(f"Verified PDF output exists: {pdf_abs_path} ({os.path.getsize(pdf_abs_path)} bytes)")
        else:
            logging.error(f"PDF output file is missing or empty: {pdf_abs_path}")
            return False
            
        return True
    except FileNotFoundError:
        logging.error(f"Pandoc command '{PANDOC_CMD}' not found. Please ensure Pandoc is installed and in your PATH.")
        return False
    except subprocess.CalledProcessError as e:
        logging.error(f"Pandoc failed during PDF conversion (Exit code: {e.returncode}):")
        logging.error(f"Command: {' '.join(e.cmd)}")
        logging.error(f"Stderr: {e.stderr.strip()}")
        if "LaTeX error" in e.stderr:
             logging.error("This often indicates a problem with the LaTeX installation or LaTeX code within the Markdown.")
        # Removed mermaid filter specific error checking
        return False
    except Exception as e:
         logging.error(f"An unexpected error occurred during PDF conversion: {e}")
         return False

# --- Execution ---

if __name__ == "__main__":
    logging.info("--- Starting Paper Assembly Process ---")

    # 1. Render Mermaid figures to PNG
    logging.info("--- Step 1: Rendering Mermaid Figures ---")
    render_success = render_mermaid_figures()
    if render_success:
        logging.info("All Mermaid figures rendered successfully.")
    else:
        logging.warning("Some Mermaid figures failed to render, but continuing with assembly process.")
    
    # 2. Assemble main Markdown components
    logging.info("--- Step 2: Assembling Markdown Components ---")
    assembled_md_path = assemble_markdown()

    if assembled_md_path:
        logging.info("Markdown assembly successful.")
        # --- Define output paths ---
        output_html_path = os.path.join(OUTPUT_DIR, OUTPUT_HTML_FILENAME)
        output_pdf_path = os.path.join(OUTPUT_DIR, OUTPUT_PDF_FILENAME)

        # 3. Convert to HTML
        logging.info("--- Step 3: Converting to HTML ---")
        if convert_to_html(assembled_md_path, output_html_path):
            logging.info("HTML conversion successful.")
        else:
            logging.error("HTML conversion failed.")
            # Continue to PDF regardless of HTML result

        # 4. Convert to PDF
        logging.info("--- Step 4: Converting to PDF ---")
        if convert_to_pdf(assembled_md_path, output_pdf_path):
             logging.info("PDF conversion successful.")
        else:
             logging.error("PDF conversion failed.")

    else:
        logging.error("Markdown assembly failed. Skipping HTML and PDF conversion.")

    logging.info("--- Paper Assembly Process Finished ---") 