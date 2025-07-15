# LEXICON Examples

This directory contains example scripts demonstrating how to use the LEXICON system.

## Overview

The examples demonstrate different ways to use LEXICON:

1. **basic_usage.py**: Simple example of processing text through the LEXICON pipeline
2. **process_file.py**: Example of processing a file (text or audio) through the LEXICON pipeline

## Running Examples

You can run the examples directly:

```bash
# Run basic usage example
python3 basic_usage.py

# Run file processing example
python3 process_file.py

# Run file processing with a specific file
python3 process_file.py --file path/to/file.txt
```

## Example Details

### basic_usage.py

Demonstrates:
- Initializing the LEXICON engine
- Processing a sample text
- Accessing the resulting knowledge graph
- Visualizing the results

Output:
- Timestamped directory in `output/lexicon/`
- Graph JSON file
- Text visualizations
- PNG visualizations (if matplotlib is installed)
- GIF animations (if imageio is installed)

### process_file.py

Demonstrates:
- Initializing the LEXICON engine
- Processing a file (creates a sample if none provided)
- Format detection
- Visualizing the results

Command-line options:
- `--file`: Path to file to process (optional)
- `--model`: OpenRouter model to use (default: anthropic/claude-3.5-sonnet)
- `--log-level`: Logging level (default: INFO)
- `--format`: Input format (optional)

Output:
- Timestamped directory in `output/lexicon/`
- Graph JSON file
- Text visualizations
- PNG visualizations (if matplotlib is installed)
- GIF animations (if imageio is installed)

## Requirements

The examples require:
- Python 3.8+
- OpenRouter API key (set as environment variable `OPENROUTER_API_KEY`)
- Optional: matplotlib, networkx, numpy, imageio (for visualizations)

## Creating Your Own Examples

When creating your own examples:

1. Import the LEXICON engine:
   ```python
   from src.lexicon.core.engine import LexiconEngine
   from src.lexicon.core.config import LexiconConfig
   ```

2. Initialize the engine:
   ```python
   config = LexiconConfig()
   engine = LexiconEngine(config)
   ```

3. Process text or files:
   ```python
   # Process text
   result = engine.process_text("Your text here")
   
   # Process file
   result = engine.process_file("path/to/file.txt")
   ```

4. Access the graph:
   ```python
   graph = result["graph"]
   nodes = graph["nodes"]
   edges = graph["edges"]
   ``` 