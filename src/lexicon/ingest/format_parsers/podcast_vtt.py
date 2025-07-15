"""
Podcast VTT Parser

Parses podcast VTT format into processed segments.
"""

import re
from typing import List, Dict, Any, Optional, Tuple
from ...nlp.preprocessor import ProcessedSegment

def parse_vtt(text: str) -> List[ProcessedSegment]:
    """
    Parse VTT subtitle format.
    
    Args:
        text: Raw VTT content
        
    Returns:
        List of processed segments
    """
    segments = []
    
    # Split by VTT cue blocks
    cue_pattern = r'(\d\d:\d\d:\d\d\.\d\d\d --> \d\d:\d\d:\d\d\.\d\d\d)([\s\S]*?)(?=\d\d:\d\d:\d\d\.\d\d\d|$)'
    matches = re.findall(cue_pattern, text)
    
    for i, (timestamp, content) in enumerate(matches):
        # Parse timestamp
        start_time = timestamp.split(' --> ')[0]
        hours, minutes, rest = start_time.split(':')
        seconds, milliseconds = rest.split('.')
        time_seconds = (int(hours) * 3600) + (int(minutes) * 60) + int(seconds) + (int(milliseconds) / 1000)
        
        # Clean content
        clean_content = content.strip()
        
        # Extract speaker if present (e.g., "SPEAKER: Text")
        speaker = None
        speaker_match = re.match(r'^([A-Z][A-Z\s]+):\s*(.*)', clean_content)
        if speaker_match:
            speaker = speaker_match.group(1)
            clean_content = speaker_match.group(2)
        
        # Create segment
        if clean_content:
            segment = ProcessedSegment(
                segment_id=f"vtt_{i+1}",
                text=clean_content,
                speaker=speaker,
                timestamp=time_seconds,
                metadata={"format": "podcast:vtt", "original_timestamp": timestamp}
            )
            segments.append(segment)
    
    return segments 