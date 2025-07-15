# LEXICON Format Parsers

This directory contains parsers for different input formats used by the LEXICON system.

## Overview

The format parsers convert different input formats into a standardized `ProcessedSegment` structure that can be used by the LEXICON pipeline.

## Available Parsers

- **podcast_vtt.py**: Parser for VTT subtitle format commonly used in podcast transcripts
- **meeting_transcript.py**: Parser for meeting transcript format with speaker labels
- **twitter_thread.py**: Parser for Twitter thread format with tweet markers

## Usage

The parsers are used internally by the LEXICON engine, but you can also use them directly:

```python
from src.lexicon.ingest.format_parsers.podcast_vtt import parse_vtt
from src.lexicon.ingest.format_parsers.meeting_transcript import parse_meeting
from src.lexicon.ingest.format_parsers.twitter_thread import parse_twitter

# Parse VTT format
vtt_text = """
00:00:00.000 --> 00:00:05.000
Welcome to our podcast.

00:00:05.000 --> 00:00:10.000
Today we're discussing CEREBRUM.
"""
segments = parse_vtt(vtt_text)

# Parse meeting transcript format
meeting_text = """
MODERATOR: Welcome to the meeting.
SPEAKER1: Thank you for having me.
"""
segments = parse_meeting(meeting_text)

# Parse Twitter thread format
twitter_text = """
Tweet 1: This is the first tweet.
@user2: This is a reply.
"""
segments = parse_twitter(twitter_text)
```

## Format Specifications

### Podcast VTT Format

VTT (WebVTT) is a subtitle format with timestamps:

```
00:00:00.000 --> 00:00:05.000
SPEAKER: Text content

00:00:05.000 --> 00:00:10.000
SPEAKER: More content
```

The parser extracts:
- Timestamp information
- Speaker labels (if present)
- Text content

### Meeting Transcript Format

Meeting transcript format uses speaker labels at the beginning of lines:

```
SPEAKER1: Text spoken by the first speaker.
SPEAKER2: Text spoken by the second speaker.
```

The parser extracts:
- Speaker labels
- Text content

### Twitter Thread Format

Twitter thread format represents a series of tweets:

```
Tweet 1: Content of the first tweet.
@username: Content of a reply tweet.
```

The parser extracts:
- Tweet numbers or usernames
- Tweet content

## Adding New Parsers

To add a new parser:

1. Create a new file named after the format (e.g., `new_format.py`)
2. Implement a `parse_*` function that takes text input and returns a list of `ProcessedSegment` objects
3. Import the parser in `__init__.py`
4. Add format detection to the LEXICON preprocessor 