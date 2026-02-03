# LLM Context

## Purpose

Large Language Model integration for enhanced CEREBRUM capabilities.

## Architecture

```text
llm/
├── __init__.py              # Main exports and lazy loading
├── config.py                # Configuration management
├── llm_utils.py             # Shared utilities
├── openrouter_analysis_engine.py  # Multi-model analysis
├── openrouter_example.py    # Usage examples
├── OpenRouter/              # OpenRouter provider
└── ollama/                  # Ollama local provider
```

## Key Exports

```python
from src.llm import (
    # Research Assistants
    PerplexityResearcher,
    OpenAIResearcher,
    WebResearcher,
    ResearchAssistant,
    
    # Analysis Engines
    StrategicAnalyzer,
    ProductEvaluator,
    OpenRouterAnalysisEngine,
    
    # Utilities
    LLMUtils,
    EvaluationResultsManager
)
```

## Key Components

| Component | Purpose |
| --------- | ------- |
| `PerplexityResearcher` | Real-time internet research |
| `OpenRouterAnalysisEngine` | Multi-model analysis workflows |
| `LLMUtils` | Rate limiting, retry, validation |
| `config.py` | API key and model configuration |

## Features

- Multi-provider support (OpenAI, Perplexity, OpenRouter, Ollama)
- Real-time internet search with citations
- Rate limiting and retry mechanisms
- Response validation and parsing

## Requirements

Set environment variable:

```bash
export OPENROUTER_API_KEY="your-api-key"
```

## Usage

```python
from src.llm import OpenRouterAnalysisEngine

engine = OpenRouterAnalysisEngine()
result = engine.analyze("Describe the CEREBRUM framework")
```
