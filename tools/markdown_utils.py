#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
CEREBRUM Markdown Utilities

This module contains utility functions for processing Markdown files in the CEREBRUM project.
It handles fixing image references, standardizing headings, and other common operations.
"""

import re
import logging
import shutil
from pathlib import Path

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

def standardize_heading_levels(file_path, is_main_text=False):
    """
    Ensure each markdown file only uses # for title and ## for sections with correct numbering.
    
    Args:
        file_path: Path to the markdown file to update
        is_main_text: Whether this is the main text file (to handle differently)
    """
    logging.info(f"Standardizing heading levels in {file_path}")
    
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # For the main text file, add "Main text" as the top-level title
        if is_main_text:
            # Check if there's already a top-level heading
            has_top_heading = re.search(r'^#\s+(.+?)(?:\n|\r\n)', content.lstrip())
            
            if has_top_heading:
                # If it's not "Main text", rename it
                if not re.search(r'^#\s+Main text', content.lstrip()):
                    content = re.sub(
                        r'^#\s+(.+?)(\n|\r\n)', 
                        r'# Main text\n\n## \1\2', 
                        content.lstrip(),
                        count=1
                    )
                    logging.info(f"Added 'Main text' as top-level heading in {file_path.name}")
            else:
                # If no title found, add "Main text" and assume "Overview" is the first section
                overview_match = re.search(r'^(.*?)\s*\n\s*Overview\s*\n', content)
                if overview_match:
                    prefix = overview_match.group(1)
                    content = prefix + "\n# Main text\n\n## Overview\n" + content[len(prefix)+len("Overview")+2:]
                else:
                    # Just add Main text at the beginning
                    content = f"# Main text\n\n{content}"
                logging.info(f"Added 'Main text' as top-level heading in {file_path.name}")
                
            # Convert any remaining top-level headings to second-level
            lines = content.split('\n')
            processed_lines = []
            
            for i, line in enumerate(lines):
                if i > 0 and line.startswith('# ') and not line.startswith('# Main text'):
                    # Convert non-main text top-level headings to second-level
                    processed_lines.append('## ' + line[2:])
                else:
                    processed_lines.append(line)
            
            content = '\n'.join(processed_lines)
        
        # For supplements, standardize the heading format
        elif "Supplement_" in file_path.name:
            # Extract supplement number from filename
            match = re.search(r'Supplement_(\d+)_', file_path.name)
            if match:
                supplement_num = match.group(1)
                
                # Check if the file already has a top-level heading
                heading_match = re.match(r'^#\s+(.+?)(?:\n|\r\n)', content.lstrip())
                if heading_match:
                    heading_text = heading_match.group(1).strip()
                    
                    # Check if heading already has "Supplement X:" prefix
                    if not re.match(rf'^Supplement {supplement_num}:', heading_text):
                        # Replace the heading with a properly formatted one
                        content = re.sub(
                            r'^#\s+(.+?)(\n|\r\n)', 
                            f'# Supplement {supplement_num}: \\1\\2', 
                            content.lstrip(),
                            count=1
                        )
                        logging.info(f"Added supplement prefix to heading in {file_path.name}")
                else:
                    # If no title found, create one from filename
                    filename = file_path.stem
                    title = filename.replace(f'Supplement_{supplement_num}_', '').replace('_', ' ').strip()
                    content = f"# Supplement {supplement_num}: {title}\n\n{content}"
                    logging.info(f"Added missing title to {file_path.name}: # Supplement {supplement_num}: {title}")
                
                # Make all the section headings use ## (second-level) but keep the actual section numbers
                # Remove any existing chapter-level numbering from second-level headings
                content = re.sub(
                    r'^##\s+\d+\.\d+\s+', 
                    r'## ', 
                    content, 
                    flags=re.MULTILINE
                )
                
                # Convert all ### (and deeper) headings to ## 
                content = re.sub(r'^(#{3,})\s+', r'## ', content, flags=re.MULTILINE)
                
                # Add section numbers (but don't double-add if they already exist)
                lines = content.split('\n')
                processed_lines = []
                main_section_counter = 0
                
                for line in lines:
                    if line.startswith('# '):
                        # Keep the top-level heading as is
                        processed_lines.append(line)
                    elif line.startswith('## '):
                        # Check if this section already has a section number 
                        existing_section_number = re.search(r'^##\s+(\d+\.\d+)\s+', line)
                        
                        if existing_section_number:
                            # Already has section numbering - keep it
                            processed_lines.append(line)
                        else:
                            # No section numbering - add it
                            main_section_counter += 1
                            title_text = line[3:].strip()
                            new_line = f"## {supplement_num}.{main_section_counter} {title_text}"
                            processed_lines.append(new_line)
                    else:
                        # Not a heading, keep as is
                        processed_lines.append(line)
                
                content = '\n'.join(processed_lines)
            else:
                # Handle figures supplement or other supplements without numbers
                heading_match = re.match(r'^#\s+(.+?)(?:\n|\r\n)', content.lstrip())
                if not heading_match:
                    # If no title found, create one from filename
                    filename = file_path.stem
                    title = filename.replace('_', ' ').strip()
                    content = f"# {title}\n\n{content}"
                    logging.info(f"Added missing title to {file_path.name}: # {title}")
        else:
            # For non-supplement files (like title page), just ensure they have a top-level heading
            heading_match = re.match(r'^#\s+(.+?)(?:\n|\r\n)', content.lstrip())
            if not heading_match:
                # If no title found, create one from filename
                filename = file_path.stem
                title = filename.replace('_', ' ').strip()
                content = f"# {title}\n\n{content}"
                logging.info(f"Added missing title to {file_path.name}: # {title}")
        
        # Write the standardized content
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(content)
            
        return True
    except Exception as e:
        logging.error(f"Error standardizing headings in {file_path.name}: {e}")
        return False

def extract_figure_info(main_content_file):
    """
    Extract figure numbers and captions from the main content file.
    Returns a clean list of figure information with proper captions.
    """
    logging.info(f"Extracting figure information from {main_content_file}")
    
    # Default captions for figures that might not have proper captions
    default_captions = {
        "1": "Foundation Domains of CEREBRUM",
        "2": "Case Relationships - Model and Linguistic Parallels",
        "3": "Cognitive Model Case Framework",
        "4": "Generative Model Integration in Intelligence Case Management",
        "5": "Model Workflows as Case Transformations - Sequence Diagram",
        "6": "Intelligence Production Workflow with Case-Bearing Models",
        "7": "CEREBRUM Category Theory Framework",
        "8": "Category Theory Framework (Alternative View)",
        "9": "Morphosyntactic Alignments in Model Relationships",
        "10": "Alignment Patterns in Model Ecosystems",
        "11": "Implementation in Intelligence Production - State Diagram",
        "12": "Alternative State-Based Visualization of Model Workflows",
        "13": "Active Inference Integration Framework",
        "14": "Message Passing Rules in the CEREBRUM Framework",
        "15": "Model Case Calculus Framework"
    }
    
    # Collection to hold figure information
    figure_info = []
    
    try:
        with open(main_content_file, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Find all Figure X references and extract unique figure numbers
        all_figure_refs = re.findall(r'Figure (\d+)', content)
        figure_numbers = sorted(set(all_figure_refs), key=int)
        
        # For each figure number, find the best caption available
        for fig_num in figure_numbers:
            # Always start with default caption - we'll replace it only if we find a good one
            str_fig_num = str(fig_num)
            caption = default_captions.get(str_fig_num, f"Figure {fig_num}")
            
            # Try to extract a better caption from the content using several methods
            extracted_caption = ""
            
            # Method 1: Look for explicit image captions with proper markdown
            pattern1 = rf'!\[Figure {fig_num}: ([^\]]+?)\]\((figures|output)/Figure_{fig_num}\.png\)'
            match = re.search(pattern1, content)
            if match and len(match.group(1).strip()) > 5:
                extracted_caption = match.group(1).strip()
            
            # Method 2: Look for textual Figure X: Description patterns
            if not extracted_caption:
                pattern2 = rf'Figure {fig_num}:\s*([^\.]+?)\.'
                match = re.search(pattern2, content)
                if match and len(match.group(1).strip()) > 5:
                    extracted_caption = match.group(1).strip()
            
            # Method 3: Look for sentences describing the figure
            if not extracted_caption:
                pattern3 = rf'Figure {fig_num}\s+(?:illustrates|shows|depicts|presents|demonstrates)\s+([^\.]+)'
                match = re.search(pattern3, content, re.IGNORECASE)
                if match and len(match.group(1).strip()) > 5:
                    extracted_caption = match.group(1).strip()
            
            # Clean up the extracted caption to check if it's usable
            if extracted_caption:
                # Remove any file paths
                cleaned_caption = re.sub(r'(figures|output)/Figure_\d+\.png', '', extracted_caption)
                # Remove markdown syntax
                cleaned_caption = re.sub(r'!\[.*?\]', '', cleaned_caption)
                cleaned_caption = re.sub(r'\([^)]*\)', '', cleaned_caption)
                cleaned_caption = re.sub(r'\[[^\]]*\]', '', cleaned_caption)
                # Remove figure references from the caption itself
                cleaned_caption = re.sub(rf'(^|\s)Figure {fig_num}[:\s]', '', cleaned_caption)
                # Clean up whitespace
                cleaned_caption = re.sub(r'\s+', ' ', cleaned_caption.strip())
                
                # Check if the cleaned caption has actual content and doesn't contain
                # paths or markdown fragments
                if (len(cleaned_caption) > 10 and 
                    not re.search(r'(figures|output)/Figure_\d+', cleaned_caption) and
                    not re.search(r'\[[^\]]*$|\][^\[]*$|\(.*$|\).*$', cleaned_caption) and
                    not cleaned_caption.startswith('Figure') and
                    not cleaned_caption.endswith('Figure')):
                    caption = cleaned_caption
            
            # Ensure first letter is capitalized
            if caption and caption[0].islower():
                caption = caption[0].upper() + caption[1:]
            
            # Final check - if caption itself looks like a filename or path, use default
            if re.search(r'(^|\s)Figure_\d+\.png', caption) or caption.count('/') > 0:
                caption = default_captions.get(str_fig_num, f"Figure {fig_num}")
            
            # Add figure info to our collection
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
    """
    Create a supplement file with all figures, one per page.
    Uses a clean, minimal approach to avoid syntax issues.
    """
    figures_file = cerebrum_dir / "Supplement_0_Figures.md"
    figures_dir = cerebrum_dir / "figures"
    figures_dir.mkdir(exist_ok=True)
    
    try:
        # Create the figures supplement content
        content = "# Supplement 0: Figures\n\n"
        content += "This supplement contains all figures referenced in the main text, presented one per page for detailed viewing.\n\n"
        
        # Ensure all figures exist by copying from output if needed
        for fig in figure_info:
            fig_num = fig["number"]
            fig_path = figures_dir / f"Figure_{fig_num}.png"
            
            if not fig_path.exists():
                output_dir = cerebrum_dir / "output"
                src_path = output_dir / f"Figure_{fig_num}.png"
                
                if src_path.exists():
                    shutil.copy2(src_path, fig_path)
                    logging.info(f"Copied figure {fig_num} from output to figures directory")
                else:
                    logging.warning(f"Figure {fig_num} not found in either figures or output directory")
        
        # Clean up figure captions before creating the supplement
        for fig in figure_info:
            # Make sure there are no markdown syntax characters in captions
            caption = fig["caption"]
            # Remove any remaining markdown syntax or figure paths
            caption = re.sub(r'(figures|output)/Figure_\d+\.png', '', caption)
            caption = re.sub(r'!\[.*?\]', '', caption)
            caption = re.sub(r'\(.*?\)', '', caption)
            caption = re.sub(r'\[.*?\]', '', caption)
            # Clean up the caption and truncate if necessary
            caption = caption.strip()
            if len(caption) < 5:
                caption = f"Figure {fig['number']}"
            elif len(caption) > 200:
                caption = caption[:200] + "..."
            
            # Ensure the caption doesn't have any markdown syntax
            fig["caption"] = caption
        
        # Now build the supplement content with clean, consistent formatting
        for fig in figure_info:
            fig_num = fig["number"]
            caption = fig["caption"]
            
            # Add a section header for the figure (use ## level to make them not appear in TOC)
            content += f"## Figure {fig_num}: {caption}\n\n"
            
            # Add the image reference with proper markdown
            content += f"![Figure {fig_num}](figures/Figure_{fig_num}.png)\n\n"
            
            # Add a page break
            content += "\\pagebreak\n\n"
        
        # Write the figures supplement
        with open(figures_file, 'w', encoding='utf-8') as f:
            f.write(content)
        
        logging.info(f"Created figures supplement at {figures_file}")
        return figures_file
    
    except Exception as e:
        logging.error(f"Error creating figures supplement: {e}")
        return None

def remove_images_from_main_text(main_file):
    """
    Completely remove all image references from the main text file while preserving figure mentions.
    
    This function ensures that only textual references to figures remain in the main text,
    with all actual image markdown completely removed.
    """
    logging.info(f"Removing image references from {main_file}")
    
    try:
        with open(main_file, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Save original content for verification
        original_content = content
        
        # 1. First, completely remove all standard markdown image references
        content = re.sub(
            r'!\[[^\]]*?\]\([^)]*?\)',
            '',
            content
        )
        
        # 2. Remove any image reference fragments that might remain
        content = re.sub(
            r'\]\((figures|output)/Figure_\d+\.png\)',
            '',
            content
        )
        
        # 3. Remove any lingering markdown image syntax fragments
        content = re.sub(
            r'!\[Figure \d+[^\]]*',
            '',
            content
        )
        
        # 4. Remove any Figure_X.png references
        content = re.sub(
            r'Figure_\d+\.png',
            '',
            content
        )
        
        # 5. Clean up any lines with just brackets or parentheses
        content = re.sub(
            r'^\s*[\[\]\(\)]+\s*$',
            '',
            content,
            flags=re.MULTILINE
        )
        
        # 6. Replace multiple newlines with double newlines (normalize spacing)
        content = re.sub(r'\n{3,}', '\n\n', content)
        
        # 7. Make sure Figure references are properly formatted
        # Convert "Figure X" followed by newline to "Figure X: "
        content = re.sub(
            r'(Figure \d+)\s*\n',
            r'\1: ',
            content
        )
        
        # Write the cleaned content back to the file
        with open(main_file, 'w', encoding='utf-8') as f:
            f.write(content)
        
        # Log statistics
        img_count = len(re.findall(r'!\[', original_content))
        logging.info(f"Removed approximately {img_count} image references from {main_file}")
        
        return True
    except Exception as e:
        logging.error(f"Error removing image references: {e}")
        return False

def ensure_figures_directory(cerebrum_dir):
    """
    Ensure the figures directory exists and contains copies of all figure images.
    
    This function creates the figures directory if it doesn't exist and ensures
    all figures from the output directory are copied to the figures directory.
    """
    logging.info("Setting up figures directory")
    
    # Create figures directory if it doesn't exist
    figures_dir = cerebrum_dir / "figures"
    figures_dir.mkdir(exist_ok=True)
    
    # Check for output directory
    output_dir = cerebrum_dir / "output"
    if not output_dir.exists():
        logging.warning("Output directory not found, some figures may be missing")
        return figures_dir
    
    # Copy all figure images from output to figures directory
    figure_pattern = re.compile(r'Figure_(\d+)\.png')
    copied_count = 0
    
    for fig_file in output_dir.glob("Figure_*.png"):
        figure_match = figure_pattern.match(fig_file.name)
        if not figure_match:
            continue
            
        target_file = figures_dir / fig_file.name
        
        # Copy if target doesn't exist or source is newer
        if (not target_file.exists() or 
            target_file.stat().st_mtime < fig_file.stat().st_mtime):
            try:
                shutil.copy2(fig_file, target_file)
                copied_count += 1
            except Exception as e:
                logging.error(f"Failed to copy {fig_file.name}: {e}")
    
    if copied_count > 0:
        logging.info(f"Copied {copied_count} figures to figures directory")
    
    return figures_dir

def get_project_root():
    """Determine the project root directory (parent of the tools directory)."""
    script_dir = Path(__file__).parent.absolute()
    return script_dir.parent

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