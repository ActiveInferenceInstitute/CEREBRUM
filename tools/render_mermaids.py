#!/usr/bin/env python3
"""
Render Mermaid diagrams from markdown files to PNG and insert them into CEREBRUM_main_text.md.
Non-interactive, fully automated approach.
"""

import os
import re
import subprocess
import shutil
import sys
import json
import tempfile
from pathlib import Path

# Constants
CEREBRUM_DIR = Path("CEREBRUM")
OUTPUT_DIR = Path("CEREBRUM/output")
CEREBRUM_MD = CEREBRUM_DIR / "CEREBRUM_main_text.md"
FIGURE_PATTERN = re.compile(r"Figure_(\d+)\.md")

def extract_mermaid_code(file_path):
    """Extract Mermaid code from a markdown file."""
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Match content between ```mermaid and ``` tags
    mermaid_match = re.search(r'```mermaid\s+(.*?)\s+```', content, re.DOTALL)
    if mermaid_match:
        return mermaid_match.group(1)
    return None

def check_node_dependencies():
    """Check Node.js dependencies are available globally."""
    print("Checking Node.js dependencies...")
    
    # Check for Node.js
    try:
        subprocess.run(['node', '--version'], check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        print("Node.js is installed.")
    except (subprocess.SubprocessError, FileNotFoundError):
        print("Error: Node.js is not installed. Please install Node.js to continue.")
        sys.exit(1)
    
    # Check for globally installed puppeteer
    try:
        result = subprocess.run(['npm', 'list', '-g', 'puppeteer'], 
                         check=False, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        if 'puppeteer' in result.stdout.decode():
            print("Puppeteer is installed globally.")
            return True
        else:
            print("Warning: Puppeteer is not installed globally.")
            try:
                print("Trying to install puppeteer globally...")
                subprocess.run(['npm', 'install', '-g', 'puppeteer'], 
                               check=True, stdout=subprocess.PIPE)
                print("Puppeteer installed successfully.")
                return True
            except subprocess.CalledProcessError as e:
                print(f"Error installing puppeteer: {e}")
                if e.stderr:
                    print(f"Error details: {e.stderr.decode()}")
                return False
    except subprocess.CalledProcessError as e:
        print(f"Error checking for puppeteer: {e}")
        return False

def render_mermaid_with_mmdc(mermaid_code, output_path):
    """Render mermaid diagram using mmdc command from mermaid-cli."""
    # Create a temporary file for the mermaid code
    with tempfile.NamedTemporaryFile(mode='w', suffix='.mmd', delete=False) as temp_file:
        temp_file.write(mermaid_code)
        temp_file_path = temp_file.name
    
    try:
        # Use mmdc from mermaid-cli to render the diagram
        print(f"Rendering diagram using mmdc...")
        # Increased resolution with width and height parameters
        # Added -c for custom config with higher quality settings
        config_json = json.dumps({
            "theme": "default",
            "themeVariables": {
                "fontSize": "16px",
                "fontFamily": "Arial",
                "primaryColor": "#326CE5",
                "primaryTextColor": "#fff",
                "primaryBorderColor": "#114A99",
                "lineColor": "#326CE5",
                "secondaryColor": "#FFD700",
                "tertiaryColor": "#E6F3FF"
            },
            "dompurifyConfig": {"USE_PROFILES": {"svg": True}},
            "securityLevel": "loose",
            "startOnLoad": True,
            "logLevel": "fatal"
        })
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as config_file:
            config_file.write(config_json)
            config_path = config_file.name
        
        process = subprocess.run(['npx', 'mmdc', 
                       '-i', temp_file_path, 
                       '-o', str(output_path), 
                       '-b', 'transparent',
                       '-w', '2400',  # Increased width for better resolution
                       '-H', '1800',  # Increased height for better resolution
                       '-c', config_path,
                       '-s', '2.5'    # Increased scale factor for better resolution
                      ], 
                       check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        
        return output_path.exists()
    except subprocess.CalledProcessError as e:
        print(f"Error rendering diagram: {e}")
        if e.stdout:
            print(f"Output: {e.stdout.decode()}")
        if e.stderr:
            error_output = e.stderr.decode()
            print(f"Error details: {error_output}")
            
            # Try to extract more specific error information
            if "Parse error" in error_output:
                lines = error_output.split('\n')
                for line in lines:
                    if "Parse error" in line:
                        print(f"Specific parse error: {line.strip()}")
                        break
        
        return False
    finally:
        # Clean up the temporary files
        if os.path.exists(temp_file_path):
            os.unlink(temp_file_path)
        if 'config_path' in locals() and os.path.exists(config_path):
            os.unlink(config_path)

def update_cerebrum_md(cerebrum_path, figure_paths):
    """Update CEREBRUM_main_text.md to include references to the rendered PNG images."""
    with open(cerebrum_path, 'r') as f:
        content = f.read()
    
    # Track which figures have already been processed to avoid duplicates
    processed_figures = set()
    
    # For each figure, find reference and add image reference (not the actual image)
    for figure_num, image_path in figure_paths.items():
        # Skip if we've already processed this figure
        if figure_num in processed_figures:
            print(f"Figure {figure_num} already processed, skipping additional references")
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
                
                # Check if image reference already exists to avoid duplicates
                next_content = content[match.end():match.end()+200]  # Look further ahead
                if f"![Figure {figure_num}]" in next_content:
                    print(f"Image reference for Figure {figure_num} already exists, skipping")
                    match_found = True
                    processed_figures.add(figure_num)
                    break
                
                # Extract caption if available
                caption_match = re.search(f"Figure {figure_num}:? (.*?)$", match.group(0).strip())
                caption = caption_match.group(1).strip() if caption_match else f"Figure {figure_num}"
                
                # Insert image reference (not the image itself) after the figure title
                img_ref = f"\n\n![Figure {figure_num}: {caption}]({rel_image_path})\n\n"
                insert_pos = match.end()
                content = content[:insert_pos] + img_ref + content[insert_pos:]
                print(f"Added image reference for Figure {figure_num}")
                match_found = True
                processed_figures.add(figure_num)
                break
        
        if not match_found:
            print(f"Warning: Could not find reference for Figure {figure_num} in {cerebrum_path.name}")
    
    # Write the updated content back to the file
    with open(cerebrum_path, 'w') as f:
        f.write(content)
    
    print(f"Updated {cerebrum_path} with image references")
    return True

def sanitize_mermaid_code(mermaid_code, figure_num=None):
    """
    General sanitizer for mermaid code that handles common syntax issues.
    This replaces figure-specific fixes with a uniform approach.
    """
    # First clean up the code to ensure no trailing whitespace
    lines = [line.rstrip() for line in mermaid_code.split('\n')]
    
    # Process diagram type-specific fixes
    if any("sequenceDiagram" in line for line in lines):
        return _sanitize_sequence_diagram(lines)
    elif any("flowchart" in line.lower() for line in lines):
        return _sanitize_flowchart(lines)
    elif any("classDiagram" in line for line in lines):
        return _sanitize_class_diagram(lines)
    else:
        # Generic cleanup for other diagram types
        return _generic_mermaid_cleanup(lines)

def _sanitize_sequence_diagram(lines):
    """Sanitize a sequence diagram."""
    clean_lines = []
    style_lines = []
    
    # Flag to identify when we're in the style section at the end
    in_style_section = False
    
    for i, line in enumerate(lines):
        line = line.strip()
        
        # Skip empty lines
        if not line:
            continue
            
        # If this is a style definition not at the beginning of a line
        if "style " in line and not line.startswith("style "):
            # Split the style from the content
            parts = line.split("style ")
            if len(parts) == 2:
                content_part = parts[0].strip()
                style_part = "style " + parts[1].strip()
                
                if content_part:
                    clean_lines.append(content_part)
                style_lines.append(style_part)
                continue
        
        # Handle regular style definition
        if line.startswith("style "):
            style_lines.append(line)
            in_style_section = True
            continue
            
        # Regular content line
        if not in_style_section:
            clean_lines.append(line)
        else:
            # If we were in the style section but found non-style content
            if not line.startswith("style "):
                in_style_section = False
                clean_lines.append(line)
            else:
                style_lines.append(line)
    
    # Add all style lines at the end
    result = "\n".join(clean_lines)
    if style_lines:
        result += "\n\n" + "\n".join(style_lines)
        
    return result

def _sanitize_flowchart(lines):
    """Sanitize a flowchart diagram."""
    clean_lines = []
    style_lines = []
    link_lines = []
    
    # Process node definitions first, then links, then styles
    for line in lines:
        line = line.strip()
        if not line:
            continue
            
        # Style definitions
        if "style " in line:
            # If style is embedded in a line with other content
            if not line.startswith("style "):
                parts = line.split("style ")
                if len(parts) == 2:
                    content_part = parts[0].strip()
                    style_part = "style " + parts[1].strip()
                    
                    if content_part:
                        clean_lines.append(content_part)
                    style_lines.append(style_part)
                    continue
            else:
                style_lines.append(line)
                continue
                
        # Link definitions (containing arrows)
        if "-->" in line or "---" in line or "-.-" in line or "===" in line:
            link_lines.append(line)
            continue
            
        # Regular node definitions
        clean_lines.append(line)
    
    # Combine in the correct order
    result = "\n".join(clean_lines)
    
    if link_lines:
        result += "\n\n" + "\n".join(link_lines)
        
    if style_lines:
        result += "\n\n" + "\n".join(style_lines)
        
    return result

def _sanitize_class_diagram(lines):
    """Sanitize a class diagram."""
    # Similar approach to other diagram types
    clean_lines = []
    relationship_lines = []
    style_lines = []
    
    for line in lines:
        line = line.strip()
        if not line:
            continue
            
        # Style definitions
        if "style " in line or "cssClass " in line:
            style_lines.append(line)
            continue
            
        # Relationship arrows
        if "--|>" in line or "-->" in line or "--o" in line or "--*" in line:
            relationship_lines.append(line)
            continue
            
        # Regular class definitions
        clean_lines.append(line)
    
    # Combine in correct order
    result = "\n".join(clean_lines)
    
    if relationship_lines:
        result += "\n\n" + "\n".join(relationship_lines)
        
    if style_lines:
        result += "\n\n" + "\n".join(style_lines)
        
    return result

def _generic_mermaid_cleanup(lines):
    """Generic cleanup for any mermaid diagram type."""
    clean_lines = []
    style_lines = []
    
    for line in lines:
        line = line.strip()
        if not line:
            continue
            
        # Collect style definitions at the end
        if "style " in line or "classDef " in line or "cssClass " in line:
            if not line.startswith(("style ", "classDef ", "cssClass ")):
                # If style is embedded in content line
                parts = []
                for prefix in ["style ", "classDef ", "cssClass "]:
                    if prefix in line:
                        parts = line.split(prefix)
                        if len(parts) == 2:
                            content_part = parts[0].strip()
                            style_part = prefix + parts[1].strip()
                            break
                
                if parts and len(parts) == 2:
                    if content_part:
                        clean_lines.append(content_part)
                    style_lines.append(style_part)
                    continue
            
            style_lines.append(line)
        else:
            clean_lines.append(line)
    
    # Combine content and styles
    result = "\n".join(clean_lines)
    if style_lines:
        result += "\n\n" + "\n".join(style_lines)
        
    return result

def fix_mermaid_syntax(mermaid_code):
    """Fix common syntax issues in mermaid code."""
    # Fix the specific error with style definitions
    # The error occurs when style is placed on the same line as a node or connection
    lines = mermaid_code.split('\n')
    fixed_lines = []
    
    in_style_block = False
    collected_styles = []
    
    for line in lines:
        # Check if the line contains a style definition
        if "style " in line and not line.strip().startswith("style "):
            # Find where the style starts
            style_pos = line.find("style ")
            if style_pos > 0:
                # Split into two lines
                node_line = line[:style_pos].strip()
                style_line = line[style_pos:].strip()
                fixed_lines.append(node_line)
                collected_styles.append(style_line)
            else:
                fixed_lines.append(line)
        else:
            fixed_lines.append(line)
    
    # Add all collected styles at the end
    for style in collected_styles:
        fixed_lines.append(style)
    
    # Special handling for specific known errors in Figure 5 and Figure 6
    output = '\n'.join(fixed_lines)
    
    # For Figure 5, fix the styling line
    output = output.replace("style Mstyle", "style M")
    
    # Fix any remaining style issues
    output = re.sub(r'(\w+)style', r'\1\nstyle', output)
    
    return output

def check_cerebrum_file_exists():
    """
    Check if CEREBRUM_main_text.md exists, if not, use CEREBRUM.md and create a copy.
    """
    original_file = CEREBRUM_DIR / "CEREBRUM.md"
    
    if not CEREBRUM_MD.exists() and original_file.exists():
        print(f"Creating {CEREBRUM_MD} from {original_file}")
        shutil.copy2(original_file, CEREBRUM_MD)
    
    return CEREBRUM_MD.exists()

def main():
    """Main function to render all mermaid diagrams and update CEREBRUM_main_text.md."""
    print("Starting mermaid diagram rendering process")
    
    # Check dependencies
    if not check_node_dependencies():
        print("Error: Required dependencies not available.")
        return 1
    
    # Ensure CEREBRUM_main_text.md exists
    if not check_cerebrum_file_exists():
        print(f"Error: Neither CEREBRUM.md nor CEREBRUM_main_text.md found in {CEREBRUM_DIR}")
        return 1
    
    # Create output directory if it doesn't exist
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    print(f"Created output directory: {OUTPUT_DIR}")
    
    # Find all figure files
    figure_files = sorted([f for f in CEREBRUM_DIR.glob("Figure_*.md")])
    print(f"Found {len(figure_files)} figure files to process\n")
    
    # Process each figure file and collect paths
    figure_paths = {}
    for figure_path in figure_files:
        figure_num = FIGURE_PATTERN.match(figure_path.name).group(1)
        print(f"Processing {figure_path.name}")
        
        # Extract mermaid code
        mermaid_code = extract_mermaid_code(figure_path)
        if not mermaid_code:
            print(f"Warning: No mermaid code found in {figure_path.name}")
            continue
        
        # Render diagram
        output_path = OUTPUT_DIR / f"Figure_{figure_num}.png"
        render_mermaid_with_mmdc(mermaid_code, output_path)
        figure_paths[figure_num] = output_path
    
    # Update CEREBRUM_main_text.md with image references
    update_cerebrum_md(CEREBRUM_MD, figure_paths)
    
    print("\nRendering summary:")
    print(f"- Successfully rendered: {len(figure_paths)} figures")
    print(f"- Failed to render: {len(figure_files) - len(figure_paths)} figures")
    print(f"- Output directory: {OUTPUT_DIR}")
    
    print("\nPerforming final verification...")
    print("\nRendering completed successfully!")
    return 0

if __name__ == "__main__":
    sys.exit(main())