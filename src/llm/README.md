# LLM Integration Module

This directory provides Large Language Model integration for CEREBRUM.

## Contents

- **`OpenRouter/`**: OpenRouter API integration (cloud)
- **`ollama/`**: Local Ollama LLM integration
- **`config.py`**: LLM configuration settings
- **`llm_utils.py`**: Shared LLM utilities
- **`openrouter_analysis_engine.py`**: Analysis engine using LLMs
- **`openrouter_example.py`**: Usage examples
- **`docs/`**: Detailed LLM documentation

## Quick Start

### Ollama (Local)

```python
from src.llm.ollama import OllamaClient, check_ollama_running

if check_ollama_running():
    client = OllamaClient(model="llama3.2")
    response = client.generate("What is CEREBRUM?")
    print(response.text)
```

### OpenRouter (Cloud)

Requires API key: `export OPENROUTER_API_KEY=your_key_here`

```python
from src.llm.OpenRouter.openrouter import OpenRouterClient, OpenRouterConfig, quick_chat
from src.llm.config import get_model_name

# One-shot chat
response = quick_chat("What is CEREBRUM?")
print(response)

# Full client with configuration
client = OpenRouterClient(OpenRouterConfig(default_model=get_model_name()))
response = client.simple_chat("Explain case-based reasoning")
print(response)
```

### Multi-Model Analysis

```python
from src.llm import OpenRouterAnalysisEngine

engine = OpenRouterAnalysisEngine()
result = engine.analyze("Describe the CEREBRUM framework")
```

See [openrouter_example.py](openrouter_example.py) for a comprehensive walkthrough (chat, streaming, conversations, document analysis, multi-model usage) and `docs/` for detailed documentation.
