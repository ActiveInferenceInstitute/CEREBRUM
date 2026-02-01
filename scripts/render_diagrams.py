#!/usr/bin/env python3
"""
Render Mermaid Diagrams - Thin Orchestration Script

This script wraps the mermaid renderer in src/utils/markdown to render
Mermaid diagrams from markdown files to PNG images.

Usage:
    python scripts/render_diagrams.py [--cerebrum-dir CEREBRUM] [--verbose]
"""

import sys
import logging
import argparse
from pathlib import Path

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.utils.markdown.mermaid_renderer import (
    main as render_main,
    check_node_dependencies,
)

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger(__name__)


def main():
    parser = argparse.ArgumentParser(description="Render Mermaid Diagrams")
    parser.add_argument("--verbose", "-v", action="store_true",
                        help="Enable verbose logging")
    
    args = parser.parse_args()
    
    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)
    
    # Check dependencies
    if not check_node_dependencies():
        logger.error("Missing Node.js dependencies. Install with: npm install -g @mermaid-js/mermaid-cli")
        return 1
    
    # Run the mermaid renderer
    return render_main()


if __name__ == "__main__":
    sys.exit(main())
