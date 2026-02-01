# LEXICON

**Linguistic Entity eXtraction and Iterative Case-Oriented Navigation**

LEXICON is a high-performance analysis engine that transforms unstructured language artifacts into tightly-typed, case-declined knowledge graphs. It operationalizes CEREBRUM's linguistic-case semantics through a specialized pipeline.

## Overview

LEXICON processes text through a pipeline that:

1. **Ingests** text or audio (with ASR) via multiple interfaces
2. **Processes** through fast, low-friction NLP preprocessing
3. **Declines** entities, claims, and relations with CEREBRUM's 8-case system
4. **Emits** editable, interoperable graphs (JSON + simple GraphQL) with micro-summaries

## Installation

LEXICON is part of the CEREBRUM project. To use it:

1. Clone the repository:

   ```
   git clone https://github.com/cerebrum/cerebrum.git
   cd cerebrum
   ```

2. Install dependencies (using [uv](https://docs.astral.sh/uv/)):

   ```
   uv pip install -e .
   ```

3. Set up your OpenRouter API key:

   ```
   export OPENROUTER_API_KEY=your_api_key_here
   ```

## Usage

### Command Line

Run LEXICON from the command line:

```bash
# Process text directly
python3 src/lexicon/run.py --input "Your text here"

# Process a file
python3 src/lexicon/run.py --input path/to/file.txt

# Process all files in batch mode
python3 src/lexicon/run.py --batch
```

**Output Directory**: LEXICON automatically creates timestamped output directories under `output/lexicon/` in the project root. Each run creates:

- `output/lexicon/lexicon_YYYYMMDD_HHMMSS/` for single text processing
- `output/lexicon/filename_YYYYMMDD_HHMMSS/` for file processing
- `output/lexicon/batch_process_YYYYMMDD_HHMMSS/` for batch processing

Each output directory contains:

- `input/` - Copy of input files
- `logs/` - Processing logs
- `cache/` - Cached data for performance
- `visualizations/` - Generated graph visualizations
- `result.json` - Complete processing results

### Python API

```python
from src.lexicon.core.engine import LexiconEngine
from src.lexicon.core.config import LexiconConfig

# Initialize with default configuration
engine = LexiconEngine()

# Process text
result = engine.process_text("The CEREBRUM project integrates case-based reasoning with Bayesian networks.")

# Or process a file
result = engine.process_file("path/to/file.txt")

# Access the graph
graph = result["graph"]
nodes = graph["nodes"]
edges = graph["edges"]
```

### Visualizations

LEXICON automatically generates comprehensive visualizations of the knowledge graphs:

**Static Visualizations**:

- `graph_visualization.png` - Complete knowledge graph layout
- `case_distribution.png` - Distribution of grammatical cases
- `entity_network.png` - Entity relationship network
- `polarity_distribution.png` - Claim polarity analysis
- `entity_claim_relationships.png` - Bipartite entity-claim graph

**Animated Visualizations**:

- `graph_construction.gif` - Animated graph building process
- `case_evolution.gif` - Case assignment progression
- `polarity_animation.gif` - Polarity distribution changes

**Entity Neighborhoods**:

- Individual visualizations for each entity's local neighborhood
- Connection maps showing entity relationships

**Requirements**: Visualizations require `matplotlib`, `networkx`, `numpy`, and `imageio`. Install with:

```bash
uv pip install matplotlib networkx numpy imageio
```

## Examples

Example scripts are available in the `src/lexicon/examples` directory:

- `basic_usage.py`: Simple example of processing text
- `process_file.py`: Example of processing a file

Run an example:

```bash
python3 src/lexicon/examples/basic_usage.py
```

## Testing

Run the tests to verify the system is working correctly:

```bash
python3 -m src.lexicon.tests.test_components
python3 -m src.lexicon.tests.test_end_to_end
```

## Architecture

LEXICON consists of several key components:

- **Core**: Main engine and configuration
- **NLP**: Preprocessing pipeline for text analysis
- **Declension**: Case tagging system
- **Paraphrase**: Micro-paraphrase generation
- **Graph**: Knowledge graph assembly
- **Ingest**: Input processing for various formats
- **Visualization**: Graph visualization and animation

## Case System

LEXICON uses CEREBRUM's 8-case linguistic system:

- **NOM (Nominative)**: Agents/subjects in statements
- **ACC (Accusative)**: Objects in statements
- **GEN (Genitive)**: Possessive/source relations
- **DAT (Dative)**: Recipients of actions
- **LOC (Locative)**: Situational context
- **INS (Instrumental)**: Transformative tools/means
- **ABL (Ablative)**: Origins/sources
- **VOC (Vocative)**: Direct address/summon

## Output Structure

Each LEXICON run produces a comprehensive output directory with:

```
output/lexicon/[session_name_timestamp]/
├── input/                    # Original input files
├── logs/                     # Processing logs
├── cache/                    # Cached data for performance
├── visualizations/           # Generated visualizations
│   ├── graph_visualization.png
│   ├── case_distribution.png
│   ├── entity_network.png
│   ├── polarity_distribution.png
│   ├── entity_claim_relationships.png
│   ├── graph_construction.gif
│   ├── case_evolution.gif
│   ├── polarity_animation.gif
│   ├── entities.txt          # Detailed entity analysis
│   ├── claims.txt            # Detailed claims analysis
│   ├── graph_statistics.txt  # Graph metrics
│   └── entity_neighborhoods/ # Individual entity visualizations
├── result.json               # Complete processing results
└── metadata.json             # Session metadata
```

## License

MIT License
