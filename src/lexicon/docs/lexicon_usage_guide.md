# LEXICON Usage Guide

LEXICON transforms text into knowledge graphs using grammatical cases.

## Quick Start

```bash
# Process text
python3 src/lexicon/run.py --input "Text here"

# Process file
python3 src/lexicon/run.py --input path/to/file.txt

# Batch process
python3 src/lexicon/run.py --batch
```

### Python API

```python
from src.lexicon.core.engine import LexiconEngine

engine = LexiconEngine()
result = engine.process_text("Text")
result = engine.process_file("file.txt")
graph = result["graph"]
```

## Output Structure

Timestamped directories in `output/lexicon/`:

```
lexicon_[timestamp]/
├── input/  # Input files
├── logs/   # Logs
├── cache/  # Cache
├── visualizations/  # Graphs, charts, animations
├── result.json  # Results
└── metadata.json  # Metadata
```

For batch:
```
batch_process_[timestamp]/
├── [file1]/  # Per-file results
├── [file2]/
└── batch_summary.json
```

## Visualizations

Requires matplotlib, networkx, numpy, imageio.

- Static: graph_visualization.png, case_distribution.png, etc.
- Animated: graph_construction.gif, etc.
- Text: entities.txt, claims.txt

## Pipeline

1. Ingest
2. NLP (split, POS, NER)
3. Case assignment
4. Paraphrase
5. Graph assembly
6. Visualize

## Configuration

```bash
export OPENROUTER_API_KEY=your_key
```

```python
config = LexiconConfig(default_model="claude-3.5-sonnet")
```

## Inputs

- Text: .txt, .md
- Audio: .mp3, .wav
- Formats: meeting, podcast VTT, Twitter

## Results

```json
{
  "status": "success",
  "metadata": {...},
  "stats": {...},
  "graph": {"nodes": [...], "edges": [...]},
  "entities": [...],
  "claims": [...]
}
```

## Performance

- ~30-60s for 1000 words
- Cache for speedup

## Troubleshooting

- Set API key
- Install viz packages
- Check permissions
- Use --log-level DEBUG

Logs in output/logs/.

## Integration

Uses CEREBRUM cases and transformations.

See tech_overview.md for details. 