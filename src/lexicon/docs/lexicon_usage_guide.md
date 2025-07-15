# LEXICON Usage Guide

**Linguistic Entity eXtraction and Iterative Case-Oriented Navigation**

LEXICON is CEREBRUM's linguistic analysis engine that transforms unstructured text into comprehensive knowledge graphs using grammatical case theory.

## Quick Start

### Basic Usage

```bash
# Process text directly
python3 src/lexicon/run.py --input "The CEREBRUM project integrates case-based reasoning with Bayesian networks."

# Process a file
python3 src/lexicon/run.py --input path/to/file.txt

# Process all files in input directory
python3 src/lexicon/run.py --batch
```

### Python API

```python
from src.lexicon.core.engine import LexiconEngine
from src.lexicon.core.config import LexiconConfig

# Initialize engine
engine = LexiconEngine()

# Process text
result = engine.process_text("Your text here")

# Process file
result = engine.process_file("path/to/file.txt")

# Access results
graph = result["graph"]
entities = result["entities"]
claims = result["claims"]
```

## Output Directory Structure

LEXICON automatically creates timestamped output directories under `output/lexicon/` in the project root. **Never uses system root `/output`**.

### Single Processing Run

```
output/lexicon/lexicon_20250715_143022/
├── input/
│   └── input.txt                 # Copy of input text
├── logs/
│   └── lexicon_run_20250715_143022.log
├── cache/
│   └── paraphrases/              # LLM response cache
├── visualizations/
│   ├── graph_visualization.png   # Main graph layout
│   ├── case_distribution.png     # Case frequency chart
│   ├── entity_network.png        # Entity-only network
│   ├── polarity_distribution.png # Claim polarity analysis
│   ├── entity_claim_relationships.png # Bipartite graph
│   ├── graph_construction.gif    # Animated graph building
│   ├── case_evolution.gif        # Case assignment animation
│   ├── polarity_animation.gif    # Polarity changes
│   ├── entities.txt              # Detailed entity analysis
│   ├── claims.txt                # Detailed claims analysis
│   ├── graph_statistics.txt      # Graph metrics
│   └── entity_neighborhoods/     # Individual entity visualizations
├── result.json                   # Complete processing results
└── metadata.json                 # Session metadata
```

### File Processing Run

```
output/lexicon/therapy_session_20250715_132020/
├── input/
│   └── therapy_session.md        # Copy of original file
├── logs/
├── cache/
├── visualizations/
├── result.json
└── metadata.json
```

### Batch Processing Run

```
output/lexicon/batch_process_20250715_132020/
├── therapy_session/              # Individual file results
│   ├── input/
│   ├── logs/
│   ├── cache/
│   ├── visualizations/
│   └── result.json
├── meeting_notes/                # Another file's results
│   └── [same structure]
├── batch_summary_20250715_132321.json  # Overall batch results
└── logs/
```

## Visualization System

LEXICON generates comprehensive visualizations automatically. Requires `matplotlib`, `networkx`, `numpy`, and `imageio`.

### Installation

```bash
pip install matplotlib networkx numpy imageio
```

### Static Visualizations

1. **graph_visualization.png**: Complete knowledge graph with entities (blue circles) and claims (green squares). Node size indicates confidence.

2. **case_distribution.png**: Bar chart showing distribution of grammatical cases across all entities.

3. **entity_network.png**: Network showing only entities and their relationships, useful for understanding entity connections.

4. **polarity_distribution.png**: Bar chart of claim polarities (positive=green, negative=red, neutral=gray).

5. **entity_claim_relationships.png**: Bipartite graph showing connections between entities and claims.

### Animated Visualizations

1. **graph_construction.gif**: Shows the graph being built progressively, first adding nodes then edges.

2. **case_evolution.gif**: Highlights different grammatical cases in sequence.

3. **polarity_animation.gif**: Shows claim polarity distribution changes.

### Text-Based Analysis

1. **entities.txt**: Detailed entity categorization with confidence scores and related claims.

2. **claims.txt**: Claims organized by polarity with supporting evidence and related entities.

3. **graph_statistics.txt**: Graph metrics including node/edge counts and type distributions.

### Entity Neighborhoods

Individual PNG files for each entity showing its local neighborhood with directly connected nodes.

## Processing Pipeline

### 1. Entity Detection

LEXICON uses multiple strategies:
- **Named Entity Recognition**: SpaCy-based NER for people, places, organizations
- **Contextual Entities**: LLM-based extraction for domain-specific entities
- **Relational Entities**: Entities identified through their relationships

### 2. Claim Detection

- **Explicit Claims**: Direct statements and assertions
- **Implicit Claims**: Inferred meanings and assumptions
- **Emotional Claims**: Sentiment and emotional content

### 3. Case Assignment

Each entity receives a grammatical case based on its role:
- **NOM (Nominative)**: Agents/subjects
- **ACC (Accusative)**: Objects/recipients
- **GEN (Genitive)**: Possessive/source relations
- **DAT (Dative)**: Recipients of actions
- **LOC (Locative)**: Situational context
- **INS (Instrumental)**: Tools/means
- **ABL (Ablative)**: Origins/sources
- **VOC (Vocative)**: Direct address

### 4. Graph Assembly

Creates a knowledge graph with:
- Nodes: Entities and claims with metadata
- Edges: Relationships between nodes
- Attributes: Confidence scores, cases, polarities

## Configuration

### Environment Variables

```bash
export OPENROUTER_API_KEY=your_api_key_here
```

### Python Configuration

```python
from src.lexicon.core.config import LexiconConfig

config = LexiconConfig(
    default_model="anthropic/claude-3.5-sonnet",
    output_dir=Path("custom/output"),
    log_level="DEBUG",
    enable_detailed_logging=True
)

engine = LexiconEngine(config)
```

## Supported Input Formats

### Text Files
- `.txt`: Plain text
- `.md`: Markdown
- `.json`: JSON content
- `.html`: HTML content
- `.csv`: CSV content

### Audio Files (with transcription)
- `.mp3`, `.wav`, `.m4a`, `.ogg`, `.flac`

### Specialized Formats
- **Meeting Transcripts**: Speaker-labeled dialogue
- **Podcast VTT**: Video subtitle format
- **Twitter Threads**: Social media content

## Result Structure

### Complete Result JSON

```json
{
  "status": "success",
  "session_id": "uuid",
  "timestamp": "2025-07-15T13:20:20.651116",
  "metadata": {
    "format": "text",
    "file_path": "/path/to/input",
    "output_dir": "/path/to/output"
  },
  "stats": {
    "nodes": 66,
    "edges": 17,
    "entities": 44,
    "claims": 22
  },
  "graph": {
    "nodes": [...],
    "edges": [...]
  },
  "entities": [...],
  "claims": [...]
}
```

### Node Structure

```json
{
  "id": "uuid",
  "label": "Entity Text",
  "type": "entity|claim",
  "category": "person|organization|concept|etc",
  "confidence": 0.9,
  "case": "nominative|accusative|etc",
  "data": {
    "text": "Original text",
    "metadata": {...}
  }
}
```

### Edge Structure

```json
{
  "id": "uuid",
  "source": "source_node_id",
  "target": "target_node_id",
  "type": "mentions|relates_to|etc",
  "weight": 1.0,
  "metadata": {...}
}
```

## Performance Notes

- **Typical Processing Time**: 30-60 seconds for 1000 words
- **Visualization Generation**: 5-10 seconds additional
- **Memory Usage**: ~500MB for typical documents
- **Cache Benefits**: 50-80% speedup for similar content

## Troubleshooting

### Common Issues

1. **Missing API Key**: Set `OPENROUTER_API_KEY` environment variable
2. **Visualization Errors**: Install required packages (`matplotlib`, `networkx`, `numpy`, `imageio`)
3. **Permission Errors**: Ensure write access to project directory
4. **Import Errors**: Verify Python path includes project root

### Debug Mode

```bash
python3 src/lexicon/run.py --input "text" --log-level DEBUG
```

### Log Locations

Check `output/lexicon/[session]/logs/` for detailed processing logs.

## Integration with CEREBRUM

LEXICON is designed to work seamlessly with other CEREBRUM components:

- **Case System**: Uses CEREBRUM's 8-case grammatical framework
- **Model Integration**: Compatible with CEREBRUM model definitions
- **Visualization**: Integrates with CEREBRUM visualization systems
- **Transformations**: Supports CEREBRUM transformation pipelines

## Next Steps

- Explore generated visualizations to understand your data
- Use the JSON output for further analysis
- Integrate with your existing workflows
- Experiment with different input formats and models

---

For technical support and advanced usage, see:
- [LEXICON Technical Overview](src/lexicon/docs/lexicon_tech_overview.md)
- [LEXICON README](src/lexicon/README.md)
- [CEREBRUM Documentation](docs/README.md) 