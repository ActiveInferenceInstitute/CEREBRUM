"""
LEXICON Custom Exceptions

Exception classes for LEXICON module.
"""

class LexiconError(Exception):
    """Base exception for all LEXICON errors."""
    pass


class ConfigurationError(LexiconError):
    """Configuration related errors."""
    pass


class APIError(LexiconError):
    """Errors related to API calls."""
    pass


class ModelError(LexiconError):
    """Errors related to model calls or responses."""
    pass


class InputError(LexiconError):
    """Errors related to input validation or processing."""
    pass


class GraphError(LexiconError):
    """Errors related to graph operations."""
    pass


class StorageError(LexiconError):
    """Errors related to data storage or retrieval."""
    pass


class ProcessingError(LexiconError):
    """Errors related to text processing operations."""
    pass


class RateLimitError(APIError):
    """Rate limit exceeded errors."""
    pass


class AuthenticationError(APIError):
    """Authentication related errors."""
    pass


class TimeoutError(APIError):
    """Timeout related errors."""
    pass


class ParsingError(ProcessingError):
    """Errors related to parsing operations."""
    pass 