# Lexicon Context

## Purpose

Linguistic Entity eXtraction and Iterative Case-Oriented Navigation (LEXICON).
Transforms unstructured language into case-declined knowledge graphs.

## Architecture

```text
lexicon/
├── core/           # LexiconEngine, config, exceptions
├── nlp/            # spaCy integration
├── ingest/         # Text data loading and parsing
├── declension/     # Grammatical case transformations
├── graph/          # Knowledge graph assembly
├── visualization/  # Lexicon-specific viz
├── docs/           # Documentation
├── examples/       # Usage examples
└── tests/          # Module tests
```

## Key Exports

```python
from src.lexicon import (
    LexiconEngine,
    LexiconConfig,
    LexiconError,
    CaseTagger,
    GraphAssembler
)
```

## Usage

```python
from src.lexicon import LexiconEngine, LexiconConfig

config = LexiconConfig(language="en")
engine = LexiconEngine(config)

# Process text
result = engine.process("The model predicts outcomes accurately.")

# Get case-tagged entities
entities = engine.get_entities()
```

## Integration

- Agents use `lexicon` to analyze text inputs or generate linguistically structured outputs
- Key entry point: `run.py` (for CLI or high-level execution)
- Depends on `src.core` for Case types
