"""
LEXICON File Watcher

Monitors directories for new files to process.
"""

import os
import time
import threading
from pathlib import Path
from typing import Dict, List, Callable, Optional, Set
from datetime import datetime
import logging

logger = logging.getLogger("lexicon.ingest.file_watcher")


class FileWatcher:
    """
    File system watcher for monitoring new files.
    
    Watches directories for new files and triggers processing callbacks.
    """
    
    def __init__(self, directory: Path, patterns: List[str], 
               callback: Callable[[Path], None], interval: float = 5.0):
        """
        Initialize the file watcher.
        
        Args:
            directory: Directory to watch
            patterns: List of glob patterns to match (e.g., "*.txt", "*.mp3")
            callback: Function to call for each new file (takes file path as argument)
            interval: Polling interval in seconds
        """
        self.directory = Path(directory)
        self.patterns = patterns
        self.callback = callback
        self.interval = interval
        
        # Track known files
        self.known_files: Set[Path] = set()
        
        # Initialization state
        self.is_running = False
        self.thread = None
        
        logger.info(f"File watcher initialized for {self.directory} with patterns: {self.patterns}")
    
    def start(self) -> None:
        """Start the file watcher."""
        if self.is_running:
            return
            
        # Initialize known files
        self._scan_existing_files()
            
        # Start thread
        self.is_running = True
        self.thread = threading.Thread(target=self._watch_thread, daemon=True)
        self.thread.start()
        
        logger.info(f"File watcher started: {len(self.known_files)} existing files")
    
    def stop(self) -> None:
        """Stop the file watcher."""
        if not self.is_running:
            return
            
        self.is_running = False
        if self.thread and self.thread.is_alive():
            self.thread.join(timeout=2.0)
            
        logger.info("File watcher stopped")
    
    def _scan_existing_files(self) -> None:
        """Scan and record existing files."""
        all_files = set()
        
        # Find all files matching patterns
        for pattern in self.patterns:
            matching_files = set(self.directory.glob(pattern))
            all_files.update(matching_files)
        
        self.known_files = all_files
    
    def _watch_thread(self) -> None:
        """Watch thread implementation."""
        while self.is_running:
            try:
                # Find all files matching patterns
                current_files = set()
                for pattern in self.patterns:
                    matching_files = set(self.directory.glob(pattern))
                    current_files.update(matching_files)
                
                # Find new files
                new_files = current_files - self.known_files
                
                # Process new files
                for file_path in new_files:
                    try:
                        logger.info(f"New file detected: {file_path}")
                        self.callback(file_path)
                    except Exception as e:
                        logger.error(f"Error processing file {file_path}: {str(e)}")
                    
                    # Add to known files even if processing failed
                    self.known_files.add(file_path)
                
                # Sleep before next check
                time.sleep(self.interval)
                
            except Exception as e:
                logger.error(f"Error in file watcher: {str(e)}")
                time.sleep(self.interval)
    
    def get_status(self) -> Dict:
        """
        Get watcher status.
        
        Returns:
            Dictionary with status information
        """
        return {
            "directory": str(self.directory),
            "patterns": self.patterns,
            "is_running": self.is_running,
            "known_files": len(self.known_files),
            "interval": self.interval
        }
    
    def watch_file(self, file_path: Path) -> None:
        """
        Process a specific file.
        
        Args:
            file_path: Path to file
        """
        if not file_path.exists():
            raise FileNotFoundError(f"File not found: {file_path}")
            
        logger.info(f"Processing specific file: {file_path}")
        self.callback(file_path)
        self.known_files.add(file_path) 