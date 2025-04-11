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
import shutil
import glob
import argparse
from pathlib import Path

# Configure logging
logging.basicConfig(
    level=logging.INFO, 
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)

def get_project_root():
    """Determine the project root directory (parent of the tools directory)."""
    script_dir = Path(__file__).parent.absolute()
    return script_dir.parent

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

def ensure_figures_directory(cerebrum_dir):
    """Ensure the figures directory exists and contains all figure images."""
    figures_dir = cerebrum_dir / "figures"
    figures_dir.mkdir(exist_ok=True)
    
    # Check for figures in output directory and move them to figures directory
    output_dir = cerebrum_dir / "output"
    if output_dir.exists():
        figure_files = list(output_dir.glob("Figure_*.png"))
        for fig_file in figure_files:
            target_file = figures_dir / fig_file.name
            if not target_file.exists() or (target_file.stat().st_mtime < fig_file.stat().st_mtime):
                shutil.copy2(fig_file, target_file)
                logging.info(f"Copied figure: {fig_file.name} to figures directory")
    
    return figures_dir

def fix_image_references(markdown_file, output_to_figures=True, remove_images=False):
    """Ensure images are properly referenced in the markdown file.
    
    Args:
        markdown_file: Path to the markdown file to update
        output_to_figures: Whether to update paths from output/ to figures/
        remove_images: If True, removes image markdown but keeps figure references
    """
    logging.info(f"Fixing image references in {markdown_file}")
    
    with open(markdown_file, 'r') as f:
        content = f.read()
    
    # 1. Fix double labeled figures
    content = re.sub(
        r'!\[Figure (\d+): Figure \1: ([^\]]+)\]',
        r'![Figure \1: \2]', 
        content
    )
    
    # 2. Remove duplicate figure references without captions
    content = re.sub(
        r'(!\[Figure (\d+)\]\(output/Figure_\2\.png\))\s*\n\s*\n\1',
        r'\1',
        content
    )
    
    if output_to_figures:
        # 3. Update figure paths from output/ to figures/
        content = re.sub(
            r'(!\[Figure (\d+)[^\]]*\])\(output/Figure_(\d+)\.png\)',
            r'\1(figures/Figure_\3.png)',
            content
        )
    
    # 4. Ensure figures with captions have proper format
    figure_pattern = r'Figure (\d+): ([^\n]+)\n'
    for match in re.finditer(figure_pattern, content):
        fig_num = match.group(1)
        caption = match.group(2).strip()
        
        # Check for image reference after the caption
        end_pos = match.end()
        next_100_chars = content[end_pos:end_pos+100]
        
        if not re.search(rf'!\[Figure {fig_num}[^\]]*\]', next_100_chars):
            # Add image reference if missing
            img_path = f"figures/Figure_{fig_num}.png" if output_to_figures else f"output/Figure_{fig_num}.png"
            img_ref = f"\n\n![Figure {fig_num}: {caption}]({img_path})\n\n"
            content = content[:end_pos] + img_ref + content[end_pos:]
    
    # 5. Remove image references if requested (for main text)
    if remove_images:
        # Replace image markdown with empty string but keep figure references as text
        content = re.sub(
            r'!\[Figure (\d+)(?:: [^\]]+)?\]\((figures|output)/Figure_\d+\.png\)\s*\n*',
            r'',
            content
        )
        
        # Also remove standalone images without captions
        content = re.sub(
            r'!\[[^\]]*\]\((figures|output)/Figure_\d+\.png\)\s*\n*',
            r'',
            content
        )
        
        # Remove any double newlines that might have been created
        content = re.sub(r'\n{3,}', r'\n\n', content)
    
    # Write the updated content
    with open(markdown_file, 'w') as f:
        f.write(content)
    
    return True

def standardize_heading_levels(file_path):
    """Ensure each markdown file only uses # for title and ## for sections"""
    logging.info(f"Standardizing heading levels in {file_path}")
    
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # First, make sure the file starts with a single # heading (title)
        heading_match = re.match(r'^#\s+(.+?)(?:\n|\r\n)', content.lstrip())
        if not heading_match:
            # If no title found, create one from filename
            filename = file_path.stem
            title = filename.replace('_', ' ').replace('Supplement', '').strip()
            content = f"# {title}\n\n{content}"
            logging.info(f"Added missing title to {file_path.name}: # {title}")
        
        # For supplements, ensure the first heading is a top-level heading (#)
        if "Supplement_" in file_path.name:
            # Convert any initial ## heading to # heading if no # heading exists
            if not re.match(r'^#\s+', content.lstrip()):
                content = re.sub(r'^##\s+(.+?)(\n|\r\n)', r'# \1\2', content.lstrip(), count=1)
                logging.info(f"Converted initial ## heading to # in {file_path.name}")
                
            # Convert all ### (and deeper) headings to ## 
            content = re.sub(r'^(#{3,})\s+', r'## ', content, flags=re.MULTILINE)
        
        # Write the standardized content
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
            
        return True
    except Exception as e:
        logging.error(f"Error standardizing headings in {file_path.name}: {e}")
        return False

def prepare_supplement_files(cerebrum_dir):
    """Find and prepare supplement files with consistent headings."""
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
    processed_paths = []
    for file_path, number in supplement_files:
        # Standardize heading levels (# for title, ## for sections)
        standardize_heading_levels(file_path)
        # Update image references to use figures directory
        fix_image_references(file_path)
        # Add to processed list
        processed_paths.append(file_path)
        logging.info(f"Processed supplement: {file_path.name}")

    return processed_paths

def extract_figure_info(main_content_file):
    """Extract figure numbers and captions from the main content file."""
    logging.info(f"Extracting figure information from {main_content_file}")
    figure_info = []
    
    # Default complete captions for any figures that might not have proper captions
    default_captions = {
        "1": "Foundation Domains of CEREBRUM. The diagram shows the four key domains (Cognitive Systems Modeling, Active Inference, Linguistic Case Systems, and Intelligence Production) and their integration through the CEREBRUM core.",
        "2": "Case Relationships - Model and Linguistic Parallels. The diagram illustrates parallel case relationships between a generative model and linguistic examples, demonstrating how model cases mirror grammatical roles in natural language.",
        "3": "Cognitive Model Case Framework. The hierarchical organization of case types in CEREBRUM, showing primary, source, and contextual declensions with their functional relationships to the core generative model.",
        "4": "Generative Model Integration in Intelligence Case Management. Illustrates how CEREBRUM's generative model core orchestrates intelligence production and case management through case-specific transformations.",
        "5": "Model Workflows as Case Transformations - Sequence Diagram. Illustrates the temporal sequence of case transformations as models transition through different functional roles in an intelligence workflow.",
        "6": "Intelligence Production Workflow with Case-Bearing Models. Illustrates the intelligence production cycle, showing the stages where models with different case assignments participate.",
        "7": "CEREBRUM Category Theory Framework. Demonstrates the category-theoretic formalization of case relationships and transformations between cognitive models.",
        "8": "Category Theory Framework (Alternative View). Further illustrates the category-theoretic components and properties within CEREBRUM.",
        "9": "Morphosyntactic Alignments in Model Relationships. Shows how CEREBRUM implements different alignment patterns for model relationships based on linguistic morphosyntactic structures.",
        "10": "Alignment Patterns in Model Ecosystems. Illustrates the practical implementation of alignment patterns and how they affect model interactions and transformations.",
        "11": "Implementation in Intelligence Production - State Diagram. Provides a state-based view of the intelligence workflow highlighting model case assignments at each stage.",
        "12": "Alternative State-Based Visualization of Model Workflows. Provides another perspective on model transitions and transformations in intelligence production contexts.",
        "13": "Active Inference Integration Framework. Shows how active inference principles are integrated with case transformations through precision-weighted message passing and free energy minimization.",
        "14": "Message Passing Rules in the CEREBRUM Framework. Details the message passing protocols that enable communication between models with different case assignments.",
        "15": "Model Case Calculus Framework. Presents the formal mathematical relationships and transformation rules that govern case transitions in the CEREBRUM framework."
    }
    
    try:
        with open(main_content_file, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Dictionary to collect captions for figures
        figure_captions = {}
        
        # Look for captions in image markdown syntax
        # Pattern: ![Figure X: Caption](figures/Figure_X.png)
        md_caption_pattern = r'!\[Figure (\d+)(?:: ([^\]]+))?\]\((figures|output)/Figure_\1\.png\)'
        for match in re.finditer(md_caption_pattern, content):
            fig_num = match.group(1)
            if match.group(2) and len(match.group(2).strip()) > 5:
                caption = match.group(2).strip()
                if not caption.startswith('Figure') and not caption.startswith('output/') and not caption.startswith('figures/'):
                    figure_captions[fig_num] = caption
        
        # Look for captions following image markdown (common pattern)
        # Pattern: ![Figure X](figures/Figure_X.png)
        # 
        # ![Caption text](figures/Figure_X.png)
        standalone_img_pattern = r'!\[Figure (\d+)\]\((figures|output)/Figure_\1\.png\)\s*\n\s*\n\s*!\[([^\]]+)\]\((figures|output)/Figure_\1\.png\)'
        for match in re.finditer(standalone_img_pattern, content):
            fig_num = match.group(1)
            caption = match.group(3).strip()
            if caption and len(caption) > 10 and not caption.startswith('Figure') and not caption.startswith('output/') and not caption.startswith('figures/'):
                figure_captions[fig_num] = caption
        
        # Look for descriptive text about figures
        # Pattern: Figure X illustrates/shows/etc.
        desc_patterns = [
            r'Figure (\d+) (?:illustrates|shows|depicts|presents|represents|demonstrates) ([^\.]+)',
            r'Figure (\d+)[^\.\n]*?(?:illustrates|shows|depicts|presents|represents|demonstrates) ([^\.]+)'
        ]
        
        for pattern in desc_patterns:
            for match in re.finditer(pattern, content, re.IGNORECASE):
                fig_num = match.group(1)
                desc = match.group(2).strip()
                if desc and len(desc) > 10:
                    figure_captions[fig_num] = desc
        
        # Also look for explicit caption lines directly after figure references
        # Pattern: Figure X
        # Some caption text.
        figure_ref_pattern = r'Figure (\d+)\s*\n\s*([^\.\n]+)'
        for match in re.finditer(figure_ref_pattern, content):
            fig_num = match.group(1)
            line_after = match.group(2).strip()
            # Only use if it's a substantial caption and not a reference to another figure
            if line_after and len(line_after) > 15 and not re.search(r'Figure \d+', line_after):
                figure_captions[fig_num] = line_after
        
        # Extract all figure references in numerical order
        figure_ref_set = set()
        for match in re.finditer(r'Figure (\d+)', content):
            figure_ref_set.add(match.group(1))
        
        # Look for explicit figure captions in the format "Figure X: Caption"
        caption_pattern = r'Figure (\d+): ([^\.\n]+)'
        for match in re.finditer(caption_pattern, content):
            fig_num = match.group(1)
            caption = match.group(2).strip()
            if caption and len(caption) > 10:
                figure_captions[fig_num] = caption
        
        # Create figure info objects for all referenced figures
        for fig_num in sorted(figure_ref_set, key=int):
            # Get the best caption available, or use a default
            if fig_num in figure_captions:
                caption = figure_captions[fig_num]
            else:
                # Look one more time for any descriptive text near figure mentions
                fig_pos = content.find(f"Figure {fig_num}")
                if fig_pos > 0:
                    surrounding = content[max(0, fig_pos-100):min(len(content), fig_pos+500)]
                    cap_match = re.search(r'Figure \d+[^\.\n]*?\. ([^\.]+)', surrounding)
                    if cap_match:
                        potential_caption = cap_match.group(1).strip()
                        if len(potential_caption) > 15:
                            caption = potential_caption
                        else:
                            # Use our predefined default caption
                            caption = default_captions.get(fig_num, f"Figure {fig_num}")
                    else:
                        # Use our predefined default caption
                        caption = default_captions.get(fig_num, f"Figure {fig_num}")
                else:
                    # Use our predefined default caption
                    caption = default_captions.get(fig_num, f"Figure {fig_num}")
            
            # Clean up any remaining markdown or path references
            caption = re.sub(r'(output|figures)/Figure_\d+\.png', '', caption)
            caption = re.sub(r'\]\([^\)]+\)', '', caption)
            caption = caption.strip()
            
            # Ensure caption doesn't start with "Figure X:"
            caption = re.sub(r'^Figure \d+:', '', caption).strip()
            
            # Ensure the caption is substantial - if not, use our default
            if not caption or len(caption) < 15:
                caption = default_captions.get(fig_num, f"Figure {fig_num}")
                
            # Add to figure info array
            figure_info.append({
                "number": int(fig_num),
                "caption": caption,
                "path": f"figures/Figure_{fig_num}.png"
            })
        
        # Sort figures by number
        figure_info.sort(key=lambda x: x["number"])
        
        logging.info(f"Found {len(figure_info)} unique figures")
        return figure_info
    
    except Exception as e:
        logging.error(f"Error extracting figure information: {e}")
        return []

def create_figures_supplement(cerebrum_dir, figure_info):
    """Create a supplement file with all figures, one per page."""
    figures_file = cerebrum_dir / "Supplement_0_Figures.md"
    
    try:
        # Create the figures supplement content
        content = "# Figures\n\n"
        content += "This supplement contains all figures referenced in the main text, presented one per page for detailed viewing.\n\n"
        
        for fig in figure_info:
            # Use the integer for display but convert back to string for file path
            fig_num = fig["number"]
            caption = fig["caption"]
            path = fig["path"]
            
            # Clean any markdown or path fragments from captions
            caption = re.sub(r'figures/Figure_\d+\.png', '', caption)
            caption = re.sub(r'!\[Figure \d+:? ?', '', caption)
            caption = re.sub(r'\]$', '', caption)
            caption = caption.strip()
            
            # If caption is empty or too short, use a default
            if not caption or len(caption) < 10:
                caption = f"Figure {fig_num}"
            
            # Use full caption instead of just the figure number
            content += f"## Figure {fig_num}: {caption}\n\n"
            content += f"![Figure {fig_num}: {caption}]({path})\n\n"
            content += "\\pagebreak\n\n"  # Add pagebreak after each figure
        
        # Write the figures supplement
        with open(figures_file, 'w', encoding='utf-8') as f:
            f.write(content)
        
        logging.info(f"Created figures supplement at {figures_file}")
        return figures_file
    
    except Exception as e:
        logging.error(f"Error creating figures supplement: {e}")
        return None

def generate_pdf(title_page, main_content, figures_supplement, supplements, output_pdf, cerebrum_dir):
    """Generate the PDF using pandoc with simplified TOC and structure."""
    logging.info("Generating PDF...")
    
    # Check if title_page exists
    if not title_page.exists():
        logging.error(f"Title page not found: {title_page}")
        return False
    
    # Get the base name for output files
    output_base = output_pdf.with_suffix('')
    output_tex = output_base.with_suffix('.tex')
    
    # Build pandoc command
    pandoc_cmd = [
        "pandoc",
        # Input files in order: Title, Main Content, Figures Supplement, Other Supplements
        str(title_page),
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
        
        # Add table of contents with depth of 2
        "--toc",
        "--toc-depth=2",
        
        # Resource path for images - include both figures and output directories
        "--resource-path", f"{cerebrum_dir}:{cerebrum_dir}/figures:{cerebrum_dir}/output",
        
        # Formatting
        "-V", "papersize=letter",
        "-V", "geometry=margin=1in",
        "-V", "fontsize=11pt",
        "-V", "linestretch=1.15",
        "-V", "linkcolor=black",
        "-V", "urlcolor=black",
        "-V", "documentclass=article",
        
        # Insert page breaks before top-level headings (supplements and sections)
        "-V", "header-includes=\\usepackage{etoolbox}\\pretocmd{\\section}{\\clearpage}{}{}"
    ])
    
    # Run pandoc
    try:
        logging.info(f"Running: {' '.join(pandoc_cmd)}")
        result = subprocess.run(
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
        # Add --standalone to ensure complete .tex file
        if "--standalone" not in tex_cmd:
            tex_cmd.append("--standalone")
        
        logging.info(f"Generating TEX file: {' '.join(tex_cmd)}")
        result = subprocess.run(
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
        logging.error(f"Error output: {e.stderr}")
        return False

def cleanup_intermediate_files(cerebrum_dir):
    """Clean up intermediate files created during PDF generation."""
    logging.info("Cleaning up intermediate files...")
    
    # Patterns of files to remove, excluding .tex files
    patterns = [
        "*.aux", "*.log", "*.out", "*.toc", 
        "*.bak", "*.lof", "*.lot"
    ]
    
    total_removed = 0
    for pattern in patterns:
        files = list(cerebrum_dir.glob(pattern))
        for file in files:
            try:
                file.unlink()
                total_removed += 1
            except Exception as e:
                logging.warning(f"Could not remove {file}: {e}")
    
    # Also look in the output directory
    output_dir = cerebrum_dir / "output"
    if output_dir.exists():
        for pattern in patterns:
            files = list(output_dir.glob(pattern))
            for file in files:
                try:
                    file.unlink()
                    total_removed += 1
                except Exception as e:
                    logging.warning(f"Could not remove {file}: {e}")
    
    logging.info(f"Removed {total_removed} intermediate files")

def remove_images_from_main_text(main_file):
    """Completely remove all image references from the main text file while preserving figure mentions.
    
    This function ensures that only textual references to figures remain in the main text,
    with all actual image markdown completely removed.
    """
    logging.info(f"Removing image references from {main_file}")
    
    with open(main_file, 'r') as f:
        content = f.read()
    
    # Multiple passes to ensure all references are removed
    
    # Pass 1: Remove all standard image markdown references
    content = re.sub(
        r'!\[.*?\]\([^)]+\)',
        '',
        content
    )
    
    # Pass 2: Remove any remaining image paths and brackets
    content = re.sub(
        r'\]\(figures?/[^)]*?\)',
        '',
        content
    )
    content = re.sub(
        r'\]\(output/[^)]*?\)',
        '',
        content
    )
    
    # Pass 3: Remove any leftover fragments like "Figure_X.png"
    content = re.sub(
        r'Figure_\d+\.png',
        '',
        content
    )
    
    # Pass 4: Remove any lines that just have brackets or fragments
    content = re.sub(
        r'^\s*\][^\n]*$',
        '',
        content,
        flags=re.MULTILINE
    )
    
    # Pass 5: Clean up any fragments of image captions without proper markdown
    content = re.sub(
        r'!\[Figure \d+:.*?$',
        '',
        content,
        flags=re.MULTILINE
    )
    
    # Pass 6: Remove reference to figures directory
    content = re.sub(
        r'figures/|output/',
        '',
        content
    )
    
    # Pass 7: Clean up lines with just brackets
    content = re.sub(
        r'^\s*\]\s*$',
        '',
        content,
        flags=re.MULTILINE
    )
    
    # Final pass: Clean up excessive whitespace and newlines
    content = re.sub(r'\n{3,}', '\n\n', content)
    content = re.sub(r'\s+\n', '\n', content)
    
    # Make sure figure references are still properly formatted
    content = re.sub(
        r'(Figure \d+)[:]?[ ]*\n',
        r'\1: ',
        content
    )
    
    with open(main_file, 'w') as f:
        f.write(content)
    
    logging.info(f"Successfully removed image references from {main_file}")
    return True

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
    parser.add_argument("--keep-images", action="store_true",
                      help="Keep images in main text instead of moving to figures supplement only")

    args = parser.parse_args()

    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)

    # 1. Setup paths
    project_root = get_project_root()
    cerebrum_dir = project_root / args.cerebrum_dir
    main_file = cerebrum_dir / args.main_file
    title_page = cerebrum_dir / "title_page.md"
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
            shutil.copy2(original_file, main_file)
        else:
            return 1
    
    if not title_page.exists():
        logging.error(f"Title page not found: {title_page}")
        return 1
    
    # 2. Run Mermaid renderer if available and not skipped
    if not args.skip_mermaid:
        run_mermaid_renderer(project_root)
    
    # 3. Ensure figures directory exists and contains all figure images
    figures_dir = ensure_figures_directory(cerebrum_dir)
    
    # 4. First, update image paths to use figures directory
    fix_image_references(main_file, output_to_figures=True, remove_images=False)
    
    # 5. Extract figure information from main file (before removing images)
    figure_info = extract_figure_info(main_file)
    
    # 6. If not keeping images in main text, remove them completely
    if not args.keep_images:
        remove_images_from_main_text(main_file)
    
    # 7. Create a figures supplement
    figures_supplement = create_figures_supplement(cerebrum_dir, figure_info)
    
    # 8. Prepare other supplement files (find, sort, ensure consistent headings)
    supplements = prepare_supplement_files(cerebrum_dir)
    logging.info(f"Found and prepared {len(supplements)} supplement files in order:")
    for supp_path in supplements:
        logging.info(f"  - {supp_path.name}")
    
    # 9. Generate the PDF with the figures supplement inserted after main text
    success = generate_pdf(title_page, main_file, figures_supplement, supplements, output_file, cerebrum_dir)
    
    # 10. Clean up intermediate files unless specified to keep them
    if success and not args.keep_temp:
        cleanup_intermediate_files(cerebrum_dir)
    
    return 0 if success else 1

if __name__ == "__main__":
    sys.exit(main()) 