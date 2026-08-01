"""
LEXICON File Watcher

Monitors directories for new files to process.
"""

import logging
import threading
import time
from pathlib import Path
from typing import Callable, Dict, List, Set

logger = logging.getLogger("lexicon.ingest.file_watcher")


class FileWatcher:
    """
    File system watcher for monitoring new files.
    
    Watches directories for new files and triggers processing callbacks.
    """
    
    def __init__(self, directory: Path, patterns: List[str], 
               callback: Callable[[Path], None], interval: float = 5.0,
               retry_attempts: int = 3, retry_delay: float = 1.0):
        """
        Initialize the file watcher.
        
        Args:
            directory: Directory to watch
            patterns: List of glob patterns to match (e.g., "*.txt", "*.mp3")
            callback: Function to call for each new file (takes file path as argument)
            interval: Polling interval in seconds
            retry_attempts: Number of times to retry a failed callback before giving up
            retry_delay: Delay in seconds between retry attempts
        """
        self.directory = Path(directory)
        self.patterns = patterns
        self.callback = callback
        self.interval = interval
        self.retry_attempts = max(1, int(retry_attempts))
        self.retry_delay = float(retry_delay)
        
        # Track known files (guarded by a lock so the watching thread and callers
        # such as ``watch_file`` / ``get_status`` can safely read/update it).
        self.known_files: Set[Path] = set()
        self._lock = threading.Lock()
        
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
        
        with self._lock:
            self.known_files = all_files
    
    def _mark_known(self, file_path: Path) -> None:
        """Record a file as known (thread-safe)."""
        with self._lock:
            self.known_files.add(file_path)
    
    def _process_with_retry(self, file_path: Path) -> None:
        """
        Invoke the callback, retrying transient failures.

        Only after the callback succeeds (or retries are exhausted) is the file
        recorded as known, so a transient error does not permanently drop it.

        Args:
            file_path: File to process
        """
        for attempt in range(1, self.retry_attempts + 1):
            try:
                logger.info(f"Processing new file: {file_path} (attempt {attempt}/{self.retry_attempts})")
                self.callback(file_path)
                self._mark_known(file_path)
                return
            except Exception as e:
                if attempt == self.retry_attempts:
                    logger.error(f"Error processing file {file_path} after {attempt} attempts: {str(e)}")
                else:
                    logger.warning(f"Processing {file_path} failed on attempt {attempt}: {str(e)}; retrying")
                    time.sleep(self.retry_delay)
        # Exhausted all retries - record as known to avoid hot-looping the file.
        self._mark_known(file_path)
    
    def _watch_thread(self) -> None:
        """Watch thread implementation."""
        while self.is_running:
            try:
                # Find all files matching patterns
                current_files = set()
                for pattern in self.patterns:
                    matching_files = set(self.directory.glob(pattern))
                    current_files.update(matching_files)
                
                # Find new files (under lock to stay consistent with known_files)
                with self._lock:
                    new_files = current_files - self.known_files
                
                # Process new files with retry
                for file_path in new_files:
                    self._process_with_retry(file_path)
                
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
        with self._lock:
            known_count = len(self.known_files)
        return {
            "directory": str(self.directory),
            "patterns": self.patterns,
            "is_running": self.is_running,
            "known_files": known_count,
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
        self._process_with_retry(file_path)