#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
CEREBRUM Graphical Abstract Generator

This script creates a graphical abstract in both PNG and PDF formats for the
CEREBRUM project. It generates a conference poster-style layout with:
- Project title and author information
- Brief abstract
- Grid display of 15 key project images
- Project DOI and other metadata

Requirements:
- PIL/Pillow (for image processing)
- reportlab (for PDF generation)
- matplotlib (for layout)
"""

import os
import sys
import logging
import argparse
from pathlib import Path
import glob
from PIL import Image, ImageDraw, ImageFont
from reportlab.lib.pagesizes import A3
from reportlab.pdfgen import canvas
from reportlab.lib import colors
from reportlab.lib.units import inch
from reportlab.platypus import Paragraph, Frame
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.enums import TA_CENTER
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import numpy as np

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)

# Hard-coded project information
PROJECT_INFO = {
    "title": "CEREBRUM: Case-Enabled Reasoning Engine with Bayesian Representations for Unified Modeling",
    "author": "Daniel Ari Friedman",
    "doi": "10.5281/zenodo.15170908",
    "abstract": (
        "The Case-Enabled Reasoning Engine with Bayesian Representations for Unified Modeling (CEREBRUM) "
        "provides a robust framework for leveraging case studies within computational cognitive science, "
        "combining Bayesian inference with structured knowledge representations. CEREBRUM enables "
        "researchers to model complex cognitive phenomena by integrating empirical evidence, "
        "theoretical constructs, and computational mechanisms into a unified system, facilitating "
        "the advancement of active inference theories and applications."
    )
}

def get_project_root():
    """
    Determines the project root directory based on the script's location.
    Assumes the script resides in '<project_root>/tools/'.
    """
    script_dir = os.path.dirname(os.path.abspath(__file__))
    project_root = os.path.dirname(script_dir)
    logging.debug(f"Determined project root: {project_root}")
    return project_root

def find_images(project_root, max_images=15):
    """
    Find up to 15 images in the project to include in the abstract.
    Prioritizes figures and diagrams.
    
    Args:
        project_root: Path to project root
        max_images: Maximum number of images to include
        
    Returns:
        List of image paths
    """
    # Priority search locations
    search_paths = [
        os.path.join(project_root, "paper", "output", "Figure_*.png"),  # Add paper/output path with priority
        os.path.join(project_root, "CEREBRUM", "output", "*.png"),
        os.path.join(project_root, "CEREBRUM", "figures", "*.png"),
        os.path.join(project_root, "docs", "images", "*.png"),
        os.path.join(project_root, "**", "*.png")
    ]
    
    images = []
    for path in search_paths:
        found = glob.glob(path, recursive=True)
        images.extend(found)
        if len(images) >= max_images:
            break
    
    # Ensure we have exactly max_images
    if len(images) > max_images:
        images = images[:max_images]
    
    # If we don't have enough images, log a warning
    if len(images) < max_images:
        logging.warning(f"Found only {len(images)} images, fewer than the requested {max_images}")
    
    logging.info(f"Found {len(images)} images to include in the abstract")
    return images

def create_png_abstract(image_paths, output_path, dpi=300):
    """
    Create a graphical abstract as a PNG image using matplotlib.
    
    Args:
        image_paths: List of paths to images to include
        output_path: Path to save the resulting PNG
        dpi: Resolution for the output image
    """
    # Create figure with a 3:2 aspect ratio
    fig = plt.figure(figsize=(16, 11), dpi=dpi)
    
    # Create gridspec layout
    gs = gridspec.GridSpec(3, 1, height_ratios=[1, 0.5, 3], figure=fig)
    
    # Header area for title and author
    header_ax = fig.add_subplot(gs[0])
    header_ax.axis('off')
    header_ax.text(0.5, 0.7, PROJECT_INFO["title"], 
                  fontsize=24, fontweight='bold', ha='center',
                  wrap=True)
    header_ax.text(0.5, 0.3, f"Author: {PROJECT_INFO['author']} • DOI: {PROJECT_INFO['doi']}", 
                  fontsize=16, ha='center')
    
    # Abstract area
    abstract_ax = fig.add_subplot(gs[1])
    abstract_ax.axis('off')
    abstract_ax.text(0.5, 0.5, PROJECT_INFO["abstract"], 
                    fontsize=14, ha='center', va='center',
                    wrap=True)
    
    # Image grid area
    image_grid = gridspec.GridSpecFromSubplotSpec(3, 5, subplot_spec=gs[2], 
                                               wspace=0.1, hspace=0.1)
    
    # Calculate how many images to place
    num_images = min(len(image_paths), 15)
    
    # Place images in the grid
    for i in range(num_images):
        row = i // 5
        col = i % 5
        
        img_ax = fig.add_subplot(image_grid[row, col])
        img_ax.axis('off')
        
        try:
            # Open and display the image
            img = plt.imread(image_paths[i])
            img_ax.imshow(img)
            
            # Add figure number or caption if available
            image_name = os.path.basename(image_paths[i])
            if "Figure" in image_name:
                img_ax.set_title(f"{image_name.split('.')[0]}", fontsize=11)
        except Exception as e:
            logging.error(f"Error loading image {image_paths[i]}: {e}")
            img_ax.text(0.5, 0.5, f"Image {i+1}\nLoad Error", 
                       ha='center', va='center', fontsize=10)
    
    # Add CEREBRUM footer at the bottom of the figure
    fig.text(0.5, 0.02, "CEREBRUM • Active Inference Institute • 2025", 
             ha="center", fontsize=12, fontweight='bold')
    
    # Save figure
    plt.tight_layout(rect=[0, 0.03, 1, 0.97])
    plt.savefig(output_path, dpi=dpi, bbox_inches='tight')
    logging.info(f"Created PNG abstract: {output_path}")
    plt.close()

def create_pdf_abstract(image_paths, output_path):
    """
    Create a graphical abstract as a PDF using reportlab.
    
    Args:
        image_paths: List of paths to images to include
        output_path: Path to save the resulting PDF
    """
    # Create PDF
    c = canvas.Canvas(output_path, pagesize=A3)
    width, height = A3
    
    # Add title
    styles = getSampleStyleSheet()
    title_style = ParagraphStyle(
        'TitleStyle',
        parent=styles['Title'],
        fontSize=24,
        alignment=TA_CENTER,
        spaceAfter=10
    )
    title_frame = Frame(inch, height-2*inch, width-2*inch, 1.5*inch)
    title_para = Paragraph(PROJECT_INFO["title"], title_style)
    title_frame.addFromList([title_para], c)
    
    # Add author and DOI
    author_style = ParagraphStyle(
        'AuthorStyle',
        parent=styles['Normal'],
        fontSize=16,
        alignment=TA_CENTER,
        spaceAfter=20
    )
    author_text = f"Author: {PROJECT_INFO['author']} • DOI: {PROJECT_INFO['doi']}"
    author_frame = Frame(inch, height-3*inch, width-2*inch, 0.5*inch)
    author_para = Paragraph(author_text, author_style)
    author_frame.addFromList([author_para], c)
    
    # Add abstract
    abstract_style = ParagraphStyle(
        'AbstractStyle',
        parent=styles['Normal'],
        fontSize=14,
        alignment=TA_CENTER,
        spaceAfter=20
    )
    abstract_frame = Frame(2*inch, height-4.5*inch, width-4*inch, 1.5*inch)
    abstract_para = Paragraph(PROJECT_INFO["abstract"], abstract_style)
    abstract_frame.addFromList([abstract_para], c)
    
    # Add images in a 3x5 grid
    num_images = min(len(image_paths), 15)
    if num_images > 0:
        grid_width = width - 2*inch
        grid_height = height - 6*inch
        img_width = grid_width / 5
        img_height = grid_height / 3
        
        for i in range(num_images):
            row = i // 5
            col = i % 5
            
            x = inch + col * img_width
            y = height - 5*inch - row * img_height - img_height
            
            try:
                img = Image.open(image_paths[i])
                # Resize image to fit in the grid cell (preserving aspect ratio)
                img_aspect = img.width / img.height
                cell_aspect = img_width / img_height
                
                if img_aspect > cell_aspect:
                    # Image is wider than cell
                    new_width = img_width - 10
                    new_height = new_width / img_aspect
                else:
                    # Image is taller than cell
                    new_height = img_height - 30
                    new_width = new_height * img_aspect
                
                # Save a temporary resized image (reportlab requires a file)
                temp_img_path = f"/tmp/temp_img_{i}.png"
                img.save(temp_img_path)
                
                # Calculate centered position
                x_centered = x + (img_width - new_width) / 2
                y_centered = y + (img_height - new_height) / 2
                
                c.drawImage(temp_img_path, x_centered, y_centered, width=new_width, height=new_height)
                
                # Add figure caption if available
                image_name = os.path.basename(image_paths[i])
                if "Figure" in image_name:
                    c.setFont("Helvetica", 11)
                    c.drawCentredString(x + img_width/2, y_centered - 15, f"{image_name.split('.')[0]}")
                
                # Clean up temp file
                os.remove(temp_img_path)
                
            except Exception as e:
                logging.error(f"Error adding image {image_paths[i]} to PDF: {e}")
                c.setFont("Helvetica", 11)
                c.drawCentredString(x + img_width/2, y + img_height/2, f"Image {i+1} - Load Error")
    
    # Add footer
    c.setFont("Helvetica-Bold", 12)
    c.drawCentredString(width/2, inch/2, "CEREBRUM • Active Inference Institute • 2025")
    
    # Save the PDF
    c.save()
    logging.info(f"Created PDF abstract: {output_path}")

def main():
    parser = argparse.ArgumentParser(description="Generate a graphical abstract for CEREBRUM")
    parser.add_argument("--output-dir", help="Directory to save output files")
    args = parser.parse_args()
    
    # Get project root
    project_root = get_project_root()
    
    # Determine output directory
    if args.output_dir:
        output_dir = args.output_dir
    else:
        output_dir = os.path.join(project_root, "CEREBRUM", "output")
    
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    # Output file paths
    png_output = os.path.join(output_dir, "CEREBRUM_GraphicalAbstract.png")
    pdf_output = os.path.join(output_dir, "CEREBRUM_GraphicalAbstract.pdf")
    
    # Find images to include
    image_paths = find_images(project_root)
    
    if not image_paths:
        logging.error("No images found in the project. Cannot create graphical abstract.")
        return 1
    
    # Create PNG abstract
    try:
        create_png_abstract(image_paths, png_output)
    except Exception as e:
        logging.error(f"Error creating PNG abstract: {e}")
        return 1
    
    # Create PDF abstract
    try:
        create_pdf_abstract(image_paths, pdf_output)
    except Exception as e:
        logging.error(f"Error creating PDF abstract: {e}")
        return 1
    
    logging.info("Graphical abstract created successfully!")
    logging.info(f"PNG: {png_output}")
    logging.info(f"PDF: {pdf_output}")
    
    return 0

if __name__ == "__main__":
    sys.exit(main()) 