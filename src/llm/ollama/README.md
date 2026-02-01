# Ollama Integration

Local LLM inference using [Ollama](https://ollama.ai).

## Setup

1. Install Ollama: <https://ollama.ai>
2. Start the server: `ollama serve`
3. Pull a model: `ollama pull llama3.2`

## Quick Start

```python
from src.llm.ollama import OllamaClient, check_ollama_running

# Check server is running
if check_ollama_running():
    client = OllamaClient(model="llama3.2")
    
    # Text generation
    response = client.generate("Explain active inference")
    print(response.text)
    
    # Chat completion
    from src.llm.ollama.client import ChatMessage
    messages = [
        ChatMessage(role="system", content="You are a helpful assistant."),
        ChatMessage(role="user", content="What is CEREBRUM?")
    ]
    response = client.chat(messages)
    print(response.text)
```

## Features

- **generate()**: Text completion
- **chat()**: Multi-turn conversation
- **embeddings()**: Text embeddings
- **analyze_cerebrum_model()**: CEREBRUM-specific analysis
- **list_models()**: List available models
- **pull_model()**: Download models

## Recommended Models

| Model | Size | Best For |
|-------|------|----------|
| llama3.2 | 4.7GB | General purpose |
| mistral | 4.1GB | Fast, good quality |
| codellama | 3.8GB | Code generation |
| phi | 2.7GB | Small/fast inference |
