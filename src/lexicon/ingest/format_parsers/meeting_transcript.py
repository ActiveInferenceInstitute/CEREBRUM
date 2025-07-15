"""
Meeting Transcript Parser

Parses meeting transcript format into processed segments.
"""

import re
from typing import List, Dict, Any, Optional, Tuple
from ...nlp.preprocessor import ProcessedSegment

def parse_meeting(text: str) -> List[ProcessedSegment]:
    """
    Parse meeting transcript format.
    
    Expects format with speaker labels like:
    SPEAKER: Text content
    
    Args:
        text: Raw meeting transcript content
        
    Returns:
        List of processed segments
    """
    segments = []
    
    # Split by lines and group by speaker
    current_speaker = None
    current_text = []
    current_segment_id = 1
    
    for line in text.split('\n'):
        line = line.strip()
        if not line:
            # Empty line - if we have accumulated text, create a segment
            if current_speaker and current_text:
                segment = ProcessedSegment(
                    segment_id=f"meeting_{current_segment_id}",
                    text=' '.join(current_text),
                    speaker=current_speaker,
                    metadata={"format": "meeting:transcript"}
                )
                segments.append(segment)
                current_segment_id += 1
                current_text = []
            continue
        
        # Check for speaker pattern (e.g., "JOHN SMITH:" or "MODERATOR:")
        speaker_match = re.match(r'^([A-Z][A-Z\s\.]+):\s*(.*)', line)
        if speaker_match:
            # New speaker - if we have accumulated text, create a segment
            if current_speaker and current_text:
                segment = ProcessedSegment(
                    segment_id=f"meeting_{current_segment_id}",
                    text=' '.join(current_text),
                    speaker=current_speaker,
                    metadata={"format": "meeting:transcript"}
                )
                segments.append(segment)
                current_segment_id += 1
                current_text = []
            
            # Set new speaker and start accumulating text
            current_speaker = speaker_match.group(1)
            content = speaker_match.group(2)
            if content:
                current_text.append(content)
        else:
            # Continue with current speaker
            current_text.append(line)
    
    # Add final segment if there's any remaining text
    if current_speaker and current_text:
        segment = ProcessedSegment(
            segment_id=f"meeting_{current_segment_id}",
            text=' '.join(current_text),
            speaker=current_speaker,
            metadata={"format": "meeting:transcript"}
        )
        segments.append(segment)
    
    return segments 