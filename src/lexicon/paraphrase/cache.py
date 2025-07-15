"""
LEXICON Paraphrase Cache

Local file-based caching for paraphrases.
"""

import os
import json
import time
from typing import List, Dict, Any, Optional
from pathlib import Path
import logging

logger = logging.getLogger("lexicon.paraphrase.cache")


def get_cache(cache_file: Path) -> Optional[List[Dict[str, Any]]]:
    """
    Get cached paraphrases if available.
    
    Args:
        cache_file: Path to cache file
        
    Returns:
        List of paraphrases or None if not cached
    """
    if not cache_file.exists():
        return None
        
    try:
        with open(cache_file, 'r', encoding='utf-8') as f:
            cache_data = json.load(f)
            
        # Check cache expiration
        if "timestamp" in cache_data:
            cache_age = time.time() - cache_data["timestamp"]
            # Cache expires after 7 days (604800 seconds)
            if cache_age > 604800:
                logger.debug(f"Cache expired: {cache_file}")
                return None
        
        # Return paraphrases
        if "paraphrases" in cache_data:
            logger.debug(f"Cache hit: {cache_file}")
            return cache_data["paraphrases"]
        else:
            return None
            
    except Exception as e:
        logger.warning(f"Failed to read cache: {str(e)}")
        return None


def save_to_cache(cache_file: Path, paraphrases: List[Dict[str, Any]]) -> bool:
    """
    Save paraphrases to cache.
    
    Args:
        cache_file: Path to cache file
        paraphrases: Paraphrases to cache
        
    Returns:
        True if successful, False otherwise
    """
    try:
        cache_data = {
            "timestamp": time.time(),
            "paraphrases": paraphrases
        }
        
        # Create directory if it doesn't exist
        os.makedirs(cache_file.parent, exist_ok=True)
        
        # Write cache file
        with open(cache_file, 'w', encoding='utf-8') as f:
            json.dump(cache_data, f, indent=2)
            
        logger.debug(f"Saved to cache: {cache_file}")
        return True
        
    except Exception as e:
        logger.warning(f"Failed to save cache: {str(e)}")
        return False


def clear_cache(cache_dir: Path, max_age: Optional[float] = None) -> int:
    """
    Clear expired cache files.
    
    Args:
        cache_dir: Directory containing cache files
        max_age: Maximum age in seconds (default: 7 days)
        
    Returns:
        Number of files cleared
    """
    if max_age is None:
        max_age = 604800  # 7 days in seconds
        
    if not cache_dir.exists():
        return 0
        
    cleared_count = 0
    current_time = time.time()
    
    try:
        for cache_file in cache_dir.glob("*.json"):
            try:
                # Check file age
                file_age = current_time - cache_file.stat().st_mtime
                if file_age > max_age:
                    cache_file.unlink()
                    cleared_count += 1
                    
            except Exception as e:
                logger.warning(f"Failed to process cache file {cache_file}: {str(e)}")
        
        logger.info(f"Cleared {cleared_count} expired cache files")
        return cleared_count
        
    except Exception as e:
        logger.warning(f"Failed to clear cache: {str(e)}")
        return cleared_count


def get_cache_stats(cache_dir: Path) -> Dict[str, Any]:
    """
    Get cache statistics.
    
    Args:
        cache_dir: Directory containing cache files
        
    Returns:
        Dictionary with cache statistics
    """
    stats = {
        "total_files": 0,
        "total_size_bytes": 0,
        "oldest_file_age": 0,
        "newest_file_age": float('inf')
    }
    
    if not cache_dir.exists():
        return stats
        
    try:
        current_time = time.time()
        files = list(cache_dir.glob("*.json"))
        
        stats["total_files"] = len(files)
        
        for cache_file in files:
            # Update size
            file_size = cache_file.stat().st_size
            stats["total_size_bytes"] += file_size
            
            # Update age
            file_age = current_time - cache_file.stat().st_mtime
            stats["oldest_file_age"] = max(stats["oldest_file_age"], file_age)
            stats["newest_file_age"] = min(stats["newest_file_age"], file_age)
        
        # Convert to infinity if no files
        if stats["newest_file_age"] == float('inf'):
            stats["newest_file_age"] = 0
            
        return stats
        
    except Exception as e:
        logger.warning(f"Failed to get cache stats: {str(e)}")
        return stats 