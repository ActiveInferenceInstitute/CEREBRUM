"""
LEXICON Content ID Generator

Generates consistent and unique IDs for graph entities.
"""

import hashlib
import time
import uuid
from typing import Optional


def generate_cid(entity_type: str, content: str, namespace: Optional[str] = None) -> str:
    """
    Generate a consistent content ID based on entity type and content.
    
    Args:
        entity_type: Type of entity (node, edge, etc.)
        content: Content to hash
        namespace: Optional namespace
        
    Returns:
        Content ID string
    """
    # Create consistent base string
    base = f"{namespace or 'lexicon'}:{entity_type}:{content}"
    
    # Generate MD5 hash (suitable for our use case)
    hash_obj = hashlib.md5(base.encode())
    hash_hex = hash_obj.hexdigest()
    
    # Format as CID
    return f"cid:{hash_hex[:16]}"


def generate_timestamp_id(prefix: str) -> str:
    """
    Generate a timestamp-based ID.
    
    Args:
        prefix: ID prefix
        
    Returns:
        Timestamp-based ID
    """
    timestamp = int(time.time() * 1000)
    return f"{prefix}_{timestamp}"


def generate_uuid() -> str:
    """
    Generate a UUID-based ID.
    
    Returns:
        UUID-based ID
    """
    return f"uuid:{str(uuid.uuid4())}"


def validate_cid(cid: str) -> bool:
    """
    Validate a content ID.
    
    Args:
        cid: Content ID to validate
        
    Returns:
        True if valid, False otherwise
    """
    if not cid.startswith("cid:"):
        return False
        
    # Check if the hash part is valid hexadecimal
    hash_part = cid[4:]
    return all(c in "0123456789abcdef" for c in hash_part.lower()) 