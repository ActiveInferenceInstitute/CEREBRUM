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
from src.llm import LLMClient
client = LLMClient()
response = client.generate("prompt")
```
