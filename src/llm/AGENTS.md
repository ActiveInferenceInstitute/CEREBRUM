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
    LLMUtils,
    OpenRouterAnalysisEngine,
    AnalysisResult,
    OpenRouterClient,
    OpenRouterConfig,
    Conversation,
)
```

The `src.llm.ollama` submodule additionally provides `OllamaClient`,
`list_models`, `pull_model`, `check_ollama_running`, and `get_default_model`.

> **Note**: research-assistant style components (`PerplexityResearcher`,
> `OpenAIResearcher`, `WebResearcher`, `ResearchAssistant`,
> `StrategicAnalyzer`, `ProductEvaluator`, `EvaluationResultsManager`) are
> described in `docs/` as planned components but are **not yet implemented**
> in this package.

## Key Components

| Component | Purpose |
| --------- | ------- |
| `OpenRouterClient` | OpenRouter provider client (chat, streaming, conversations) |
| `OpenRouterAnalysisEngine` | Multi-model analysis workflows |
| `LLMUtils` | Rate limiting, retry, validation |
| `OllamaClient` | Local Ollama provider client |
| `config.py` | API key and model configuration |

## Features

- Multi-provider support (OpenRouter, Ollama)
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
