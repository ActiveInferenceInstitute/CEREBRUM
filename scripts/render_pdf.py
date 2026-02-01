#!/usr/bin/env python3
"""
Render PDF - Thin Orchestration Script

This script wraps the PDF generation utilities to render CEREBRUM
documents into a cohesive PDF file.

Usage:
    python scripts/render_pdf.py [--cerebrum-dir CEREBRUM] [--output OUTPUT.pdf]
"""

import sys
import logging
import argparse
from pathlib import Path

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.utils.markdown import get_project_root

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger(__name__)


def main():
    parser = argparse.ArgumentParser(description="Render CEREBRUM PDF")
    parser.add_argument("--cerebrum-dir", default="CEREBRUM",
                        help="Directory containing CEREBRUM files")
    parser.add_argument("--output", "-o", default="CEREBRUM_complete.pdf",
                        help="Output PDF file")
    parser.add_argument("--verbose", "-v", action="store_true",
                        help="Enable verbose logging")
    
    args = parser.parse_args()
    
    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)
    
    project_root = get_project_root()
    cerebrum_dir = project_root / args.cerebrum_dir
    
    if not cerebrum_dir.exists():
        logger.error(f"CEREBRUM directory not found: {cerebrum_dir}")
        return 1
    
    # Import dynamically to allow the module to initialize paths
    try:
        # Note: Full PDF rendering requires pandoc and LaTeX
        from src.utils.markdown.utils import cleanup_intermediate_files
        
        logger.info(f"PDF rendering would generate: {args.output}")
        logger.info("Note: Full PDF rendering requires pandoc and LaTeX")
        logger.info("Run the original tools/render_markdown.py for now")
        
        return 0
        
    except ImportError as e:
        logger.error(f"Import error: {e}")
        return 1


if __name__ == "__main__":
    sys.exit(main())
