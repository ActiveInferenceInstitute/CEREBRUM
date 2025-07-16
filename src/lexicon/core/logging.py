"""
LEXICON Structured Logging

Configures structured logging for LEXICON components.
"""

import os
import sys
import logging
import json
import time
from datetime import datetime
from pathlib import Path
from typing import Dict, Any, Optional, Union

from .config import LexiconConfig


class StructuredLogFormatter(logging.Formatter):
    """Formatter for structured JSON logs."""
    
    def format(self, record: logging.LogRecord) -> str:
        """Format log record as JSON."""
        log_data = {
            "timestamp": datetime.fromtimestamp(record.created).isoformat(),
            "level": record.levelname,
            "name": record.name,
            "message": record.getMessage(),
            "path": record.pathname,
            "line": record.lineno,
            "function": record.funcName
        }
        
        # Include exception info if available
        if record.exc_info:
            log_data["exception"] = {
                "type": str(record.exc_info[0].__name__),
                "message": str(record.exc_info[1]),
            }
        
        # Include any custom fields
        if hasattr(record, "extra") and record.extra:
            log_data.update(record.extra)
        
        return json.dumps(log_data)


class ContextAdapter(logging.LoggerAdapter):
    """Logger adapter that adds context to log records."""
    
    def process(self, msg, kwargs):
        """Process the logging message and keyword arguments."""
        # Ensure 'extra' exists in kwargs
        if 'extra' not in kwargs:
            kwargs['extra'] = {}
        
        # Add context to extra if not already present
        if hasattr(self, 'extra'):
            for key, value in self.extra.items():
                if key not in kwargs['extra']:
                    kwargs['extra'][key] = value
        
        return msg, kwargs


def setup_logging(
    config: LexiconConfig,
    log_file: Optional[Union[str, Path]] = None
) -> logging.Logger:
    """
    Set up logging for LEXICON.
    
    Args:
        config: LEXICON configuration
        log_file: Path to log file, defaults to output_dir/lexicon_{timestamp}.log
        
    Returns:
        Configured logger instance
    """
    # Create root logger
    logger = logging.getLogger("lexicon")
    logger.setLevel(getattr(logging, config.log_level))
    logger.handlers = []  # Remove any existing handlers
    
    # Create console handler with colored output if possible
    console_handler = logging.StreamHandler(sys.stdout)
    console_handler.setLevel(getattr(logging, config.log_level))
    
    try:
        from colorlog import ColoredFormatter
        console_formatter = ColoredFormatter(
            "%(log_color)s%(asctime)s - %(name)s - %(levelname)s - %(message)s%(reset)s",
            datefmt=None,
            reset=True,
            log_colors={
                'DEBUG': 'cyan',
                'INFO': 'green',
                'WARNING': 'yellow',
                'ERROR': 'red',
                'CRITICAL': 'red,bg_white',
            }
        )
    except ImportError:
        console_formatter = logging.Formatter(
            '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
        )
    console_handler.setFormatter(console_formatter)
    
    # Create file handler if log_file is provided or use default
    if log_file is None:
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        log_file = config.output_dir / f"lexicon_{timestamp}.log"
    
    log_path = Path(log_file)
    log_path.parent.mkdir(parents=True, exist_ok=True)
    
    file_handler = logging.FileHandler(log_path)
    file_handler.setLevel(getattr(logging, config.log_level))
    
    # Always use JSON formatter for file logging
    json_formatter = StructuredLogFormatter()
    file_handler.setFormatter(json_formatter)
    
    # Add handlers
    logger.addHandler(console_handler)
    logger.addHandler(file_handler)
    
    # Log setup complete
    logger.info(
        f"LEXICON logging initialized (level={config.log_level}, "
        f"log_file={log_path})"
    )
    
    return logger


def get_logger(name: str, context: Dict[str, Any] = None) -> logging.Logger:
    """
    Get a logger with optional context.
    
    Args:
        name: Logger name
        context: Optional context dictionary
        
    Returns:
        Logger instance with context
    """
    logger = logging.getLogger(f"lexicon.{name}")
    
    if context:
        return ContextAdapter(logger, context)
    
    return logger


class LoggingTimer:
    """Context manager for timing operations and logging results."""
    
    def __init__(self, logger: logging.Logger, operation: str, **context):
        """Initialize with logger and operation name."""
        self.logger = logger
        self.operation = operation
        self.context = context
        self.start_time = None
        
    def __enter__(self):
        """Start timer when entering context."""
        self.start_time = time.time()
        self.logger.debug(f"Starting {self.operation}", extra=self.context)
        return self
        
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Log timing information when exiting context."""
        duration = time.time() - self.start_time
        
        if exc_type:
            self.logger.error(
                f"{self.operation} failed after {duration:.2f}s: {exc_val}",
                extra=self.context,
                exc_info=(exc_type, exc_val, exc_tb)
            )
        else:
            self.logger.debug(
                f"Completed {self.operation} in {duration:.2f}s",
                extra={**self.context, "duration_seconds": duration}
            ) 