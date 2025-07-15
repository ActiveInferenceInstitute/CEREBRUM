"""
Twitter Thread Parser

Parses Twitter thread format into processed segments.
"""

import re
from typing import List, Dict, Any, Optional, Tuple
from ...nlp.preprocessor import ProcessedSegment

def parse_twitter(text: str) -> List[ProcessedSegment]:
    """
    Parse Twitter thread format.
    
    Expects format with tweet numbers and/or usernames:
    Tweet 1: Text content
    @username: Text content
    
    Args:
        text: Raw Twitter thread content
        
    Returns:
        List of processed segments
    """
    segments = []
    
    # Look for tweet patterns
    tweet_patterns = [
        r'Tweet (\d+):\s*(.*)',  # Tweet 1: content
        r'@([a-zA-Z0-9_]+):\s*(.*)'  # @username: content
    ]
    
    # Try to split by tweet markers
    current_segment_id = 1
    lines = text.split('\n')
    i = 0
    
    while i < len(lines):
        line = lines[i].strip()
        if not line:
            i += 1
            continue
        
        # Try to match tweet patterns
        tweet_match = None
        for pattern in tweet_patterns:
            match = re.match(pattern, line)
            if match:
                tweet_match = match
                break
        
        if tweet_match:
            # Found a tweet marker
            if pattern == tweet_patterns[0]:  # Tweet number format
                speaker = f"Tweet {tweet_match.group(1)}"
                content = tweet_match.group(2)
            else:  # Username format
                speaker = f"@{tweet_match.group(1)}"
                content = tweet_match.group(2)
            
            # Collect any continuation lines
            i += 1
            while i < len(lines) and not any(re.match(p, lines[i].strip()) for p in tweet_patterns):
                if lines[i].strip():
                    content += " " + lines[i].strip()
                i += 1
                
            # Create segment
            if content:
                segment = ProcessedSegment(
                    segment_id=f"twitter_{current_segment_id}",
                    text=content,
                    speaker=speaker,
                    metadata={"format": "twitter:thread"}
                )
                segments.append(segment)
                current_segment_id += 1
        else:
            # No match, treat as plain text
            content = line
            
            # Collect any continuation lines until next tweet marker
            i += 1
            while i < len(lines) and not any(re.match(p, lines[i].strip()) for p in tweet_patterns):
                if lines[i].strip():
                    content += " " + lines[i].strip()
                i += 1
                
            # Create segment
            if content:
                segment = ProcessedSegment(
                    segment_id=f"twitter_{current_segment_id}",
                    text=content,
                    metadata={"format": "twitter:thread"}
                )
                segments.append(segment)
                current_segment_id += 1
    
    return segments 