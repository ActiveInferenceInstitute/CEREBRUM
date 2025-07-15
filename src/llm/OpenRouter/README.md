# OpenRouter Python Client - Production Ready

Production-ready Python wrapper for OpenRouter API providing access to multiple AI models with comprehensive error handling, retry logic, circuit breaker patterns, and performance monitoring.

## Features

🚀 **Production-Ready Features:**
- Exponential backoff retry logic with circuit breaker
- Rate limiting detection and graceful handling  
- Comprehensive logging and performance metrics
- Model validation and availability checking
- Connection pooling and timeout management

🔧 **Full LLM Functionality:**
- Synchronous and asynchronous chat completions
- Streaming responses with real-time processing
- Multi-turn conversation management
- Model comparison and benchmarking
- Token usage tracking and cost estimation

📊 **Monitoring & Analytics:**
- Request/response statistics and timing
- Error tracking and categorization
- Performance monitoring and resource usage
- Circuit breaker state management

## Quick Start

### Basic Usage

```python
from src.llm.OpenRouter import OpenRouterClient

# Simple chat
client = OpenRouterClient()
response = client.simple_chat("Hello, how are you?")
print(response)
```

### Advanced Configuration

```python
from src.llm.OpenRouter import OpenRouterClient, OpenRouterConfig, RetryConfig

# Production configuration
config = OpenRouterConfig(
    retry_config=RetryConfig(
        max_retries=5,
        base_delay=1.0,
        max_delay=30.0
    ),
    enable_logging=True,
    log_level="INFO",
    timeout_seconds=60
)

client = OpenRouterClient(config)
```

### Conversation Management

```python
# Multi-turn conversations
conversation = client.create_conversation()
conversation.add_system_message("You are a helpful assistant.")

response1 = conversation.send_message("What is machine learning?")
response2 = conversation.send_message("Can you give me an example?")

# Get conversation history
history = conversation.get_history()
```

### Streaming Responses

```python
# Real-time streaming
messages = [{"role": "user", "content": "Tell me a story"}]

for chunk in client.chat_completion_stream(messages):
    if chunk.choices[0].delta.content:
        print(chunk.choices[0].delta.content, end='', flush=True)
```

### Async Operations

```python
import asyncio

async def async_example():
    messages = [{"role": "user", "content": "Hello!"}]
    
    # Async completion
    response = await client.async_chat_completion(messages)
    print(response.choices[0].message.content)
    
    # Async streaming
    async for chunk in client.async_chat_completion_stream(messages):
        if chunk.choices[0].delta.content:
            print(chunk.choices[0].delta.content, end='')

asyncio.run(async_example())
```

## Configuration

### RetryConfig Options

```python
RetryConfig(
    max_retries=5,          # Maximum retry attempts
    base_delay=1.0,         # Initial delay between retries
    max_delay=30.0,         # Maximum delay between retries
    exponential_base=2.0,   # Exponential backoff multiplier
    jitter=True             # Add randomization to delays
)
```

### CircuitBreakerConfig Options

```python
CircuitBreakerConfig(
    failure_threshold=5,     # Failures before opening circuit
    recovery_timeout=60,     # Seconds before attempting recovery
    expected_exception=Exception  # Exception types to track
)
```

### OpenRouterConfig Options

```python
OpenRouterConfig(
    retry_config=RetryConfig(),
    circuit_breaker_config=CircuitBreakerConfig(),
    enable_logging=True,
    log_level="INFO",
    timeout_seconds=30,
    max_concurrent_requests=10,
    enable_stats=True
)
```

## Available Models

The client automatically validates models and provides access to all OpenRouter models:

```python
# Get available models
models = client.get_available_models()
print(f"Available models: {len(models)}")

# Validate specific model
is_valid = client.validate_model("openai/gpt-4")
print(f"GPT-4 available: {is_valid}")
```

Common models include:
- `openai/gpt-4`
- `openai/gpt-3.5-turbo`
- `anthropic/claude-3-sonnet`
- `anthropic/claude-instant-v1`
- `google/gemini-pro`
- `meta-llama/llama-2-70b-chat`

## Error Handling

The client provides comprehensive error handling:

```python
from src.llm.OpenRouter import OpenRouterError, RateLimitError, ModelNotFoundError

try:
    response = client.simple_chat("Hello", model="invalid/model")
except ModelNotFoundError as e:
    print(f"Model not found: {e}")
except RateLimitError as e:
    print(f"Rate limit exceeded: {e}")
except OpenRouterError as e:
    print(f"OpenRouter error: {e}")
```

## Performance Monitoring

```python
# Get client statistics
stats = client.get_stats()
print(f"Total requests: {stats['total_requests']}")
print(f"Success rate: {stats['success_rate']:.1f}%")
print(f"Average latency: {stats['avg_latency_ms']:.0f}ms")

# Circuit breaker status
print(f"Circuit breaker state: {client.circuit_breaker.state}")
```

## Testing

The client includes comprehensive tests with proper output organization:

```bash
# Run functionality tests (requires API key)
python3 test_openrouter_functionality.py

# Run structure tests (no API key required)
python3 test_openrouter_structure.py

# Run both test suites
python3 test_openrouter_functionality.py && python3 test_openrouter_structure.py
```

### Test Output Organization

All test results are saved to the `test_output/` directory:

- `test_output/openrouter_test_results.json` - Functionality test results
- `test_output/openrouter_structure_test_results.json` - Structure test results

### Test Coverage

**Functionality Tests:**
- Client initialization and configuration
- Chat completion (sync/async/streaming)
- Conversation management
- Document analysis methods
- Model validation
- Error handling and retry logic
- Performance monitoring
- Flexible prompt composition

**Structure Tests:**
- Import validation
- Class definitions and method signatures
- Configuration options
- Error handling structure
- Analysis engine integration

## Examples

### Demo Script

```bash
# Run comprehensive demo
python examples/openrouter_demo.py
```

### Benchmark Suite

```bash
# Run performance benchmarks
python examples/openrouter_benchmark.py
```

## Environment Setup

1. **Get API Key:** Visit [OpenRouter](https://openrouter.ai/keys) to get your API key

2. **Set Environment Variable:**
   ```bash
   export OPENROUTER_API_KEY="your_api_key_here"
   ```

3. **Install Dependencies:**
   ```bash
   pip install aiohttp requests
   ```

## Architecture

The OpenRouter client is built with production-ready patterns:

- **Retry Logic:** Exponential backoff with jitter for transient failures
- **Circuit Breaker:** Fail-fast pattern to prevent cascade failures  
- **Connection Pooling:** Efficient HTTP connection management
- **Request Batching:** Optimal throughput for high-volume usage
- **Error Classification:** Intelligent handling of different error types
- **Performance Metrics:** Comprehensive monitoring and alerting

## Logging

Configure logging for production monitoring:

```python
import logging

# Enable detailed logging
logging.basicConfig(level=logging.INFO)

# OpenRouter client will log:
# - Request/response timing
# - Error details and retry attempts  
# - Circuit breaker state changes
# - Performance metrics
```

## Support

- **Documentation:** This README and inline code documentation
- **Examples:** See `examples/` directory for usage patterns
- **Tests:** Reference `tests/test_openrouter_consolidated.py` for all capabilities
- **OpenRouter API:** [Official Documentation](https://openrouter.ai/docs)

## Contributing

When contributing to the OpenRouter client:

1. Follow the existing code patterns and type hints
2. Add comprehensive tests for new functionality
3. Update documentation for any API changes
4. Ensure production-ready error handling and logging
5. Maintain backward compatibility where possible

## License

This OpenRouter client is part of the Corym Library project and follows the same licensing terms. 