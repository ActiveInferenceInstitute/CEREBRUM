#!/usr/bin/env python3
"""
OpenRouter Example Script

Comprehensive example demonstrating all OpenRouter functionality with real LLM calls,
flexible prompt composition, and production-ready features.

This script showcases:
1. Basic chat completion
2. Streaming responses
3. Conversation management
4. Document analysis
5. Multi-model usage
6. Error handling
7. Performance monitoring

Usage:
    python openrouter_example.py

Requirements:
    - OPENROUTER_API_KEY environment variable set
"""

import asyncio

from .OpenRouter.openrouter import (
    OpenRouterClient, 
    OpenRouterConfig, 
    RetryConfig, 
    CircuitBreakerConfig,
    Conversation,
    quick_chat
)
from .openrouter_analysis_engine import OpenRouterAnalysisEngine
from .config import get_model_name, get_model_config, validate_api_key, get_api_key


def check_api_key():
    """Check if API key is available."""
    if not validate_api_key():
        print("❌ OPENROUTER_API_KEY environment variable is required")
        print("   Get your API key at: https://openrouter.ai/keys")
        return False
    
    # Get default model
    default_model = get_model_name()
    print(f"✅ Using model: {default_model}")
    return True


def example_basic_chat():
    """Example: Basic chat completion."""
    print("\n" + "="*60)
    print("🔤 BASIC CHAT COMPLETION")
    print("="*60)
    
    # Initialize client with configured model
    model_config = get_model_config()
    config = OpenRouterConfig(
        default_model=model_config["name"],
        temperature=model_config["temperature"],
        max_tokens=model_config["max_tokens"]
    )
    client = OpenRouterClient(config)
    
    # Simple chat
    response = client.simple_chat("Hello, how are you today?")
    print(f"🤖 Response: {response}")
    
    # Chat with specific model
    response = client.simple_chat(
        "What is the capital of France?", 
        model=get_model_name()
    )
    print(f"🗺️  Response: {response}")
    
    return client


def example_chat_completion():
    """Example: Full chat completion API."""
    print("\n" + "="*60)
    print("💬 FULL CHAT COMPLETION API")
    print("="*60)
    
    # Initialize client with configured model
    model_config = get_model_config()
    config = OpenRouterConfig(
        default_model=model_config["name"],
        temperature=model_config["temperature"],
        max_tokens=model_config["max_tokens"]
    )
    client = OpenRouterClient(config)
    
    # Structured conversation
    messages = [
        {"role": "system", "content": "You are an expert in artificial intelligence."},
        {"role": "user", "content": "Explain machine learning in simple terms."}
    ]
    
    response = client.chat_completion(
        messages=messages,
        model=get_model_name(),
        temperature=0.7,
        max_tokens=300
    )
    
    print(f"🤖 AI Expert Response: {response.choices[0].message.content}")
    
    # Continue conversation
    messages.append({"role": "assistant", "content": response.choices[0].message.content})
    messages.append({"role": "user", "content": "What are the main types of machine learning?"})
    
    response2 = client.chat_completion(messages, model=get_model_name())
    print(f"🤖 Follow-up Response: {response2.choices[0].message.content}")


def example_streaming():
    """Example: Streaming responses."""
    print("\n" + "="*60)
    print("🌊 STREAMING RESPONSES")
    print("="*60)
    
    # Initialize client with configured model
    model_config = get_model_config()
    config = OpenRouterConfig(
        default_model=model_config["name"],
        temperature=model_config["temperature"],
        max_tokens=model_config["max_tokens"]
    )
    client = OpenRouterClient(config)
    
    messages = [
        {"role": "system", "content": "You are a creative storyteller."},
        {"role": "user", "content": "Tell me a short story about a robot learning to paint."}
    ]
    
    print("🤖 Streaming response:")
    full_response = ""
    for chunk in client.chat_completion_stream(
        messages=messages,
        model=get_model_name(),
        max_tokens=200
    ):
        if chunk.choices[0].delta.content:
            content = chunk.choices[0].delta.content
            full_response += content
            print(content, end='', flush=True)
    
    print(f"\n\n📝 Full response length: {len(full_response)} characters")


async def example_async_operations():
    """Example: Async operations."""
    print("\n" + "="*60)
    print("⚡ ASYNC OPERATIONS")
    print("="*60)
    
    # Initialize client with configured model
    model_config = get_model_config()
    config = OpenRouterConfig(
        default_model=model_config["name"],
        temperature=model_config["temperature"],
        max_tokens=model_config["max_tokens"]
    )
    client = OpenRouterClient(config)
    
    # Async chat completion
    messages = [
        {"role": "system", "content": "You are a helpful assistant."},
        {"role": "user", "content": "What are the benefits of renewable energy?"}
    ]
    
    response = await client.async_chat_completion(
        messages=messages,
        model=get_model_name(),
        temperature=0.5,
        max_tokens=250
    )
    
    print(f"🤖 Async Response: {response.choices[0].message.content}")
    
    # Async streaming
    print("\n🌊 Async streaming:")
    full_response = ""
    async for chunk in client.async_chat_completion_stream(
        messages=[{"role": "user", "content": "Explain quantum computing briefly."}],
        model=get_model_name(),
        max_tokens=150
    ):
        if chunk.choices[0].delta.content:
            content = chunk.choices[0].delta.content
            full_response += content
            print(content, end='', flush=True)
    
    print(f"\n\n📝 Async streaming complete: {len(full_response)} characters")


def example_conversation_management():
    """Example: Conversation management."""
    print("\n" + "="*60)
    print("💭 CONVERSATION MANAGEMENT")
    print("="*60)
    
    # Initialize client with configured model
    model_config = get_model_config()
    config = OpenRouterConfig(
        default_model=model_config["name"],
        temperature=model_config["temperature"],
        max_tokens=model_config["max_tokens"]
    )
    client = OpenRouterClient(config)
    
    # Create conversation
    conversation = client.create_conversation()
    conversation.add_system_message("You are a knowledgeable travel guide.")
    
    # Multi-turn conversation
    response1 = conversation.send_message("What are the best places to visit in Japan?", model=get_model_name())
    print(f"🗾 Travel Guide: {response1}")
    
    response2 = conversation.send_message("Tell me more about Tokyo specifically.", model=get_model_name())
    print(f"🗾 Follow-up: {response2}")
    
    # Get conversation history
    history = conversation.get_history()
    print(f"\n📚 Conversation has {len(history)} messages")
    
    # Streaming conversation
    conversation2 = client.create_conversation()
    conversation2.add_system_message("You are a creative poet.")
    
    print("\n🎭 Streaming poetry:")
    for chunk in conversation2.send_message_stream("Write a haiku about technology.", model=get_model_name()):
        print(chunk, end='', flush=True)
    print()


def example_document_analysis():
    """Example: Document analysis methods."""
    print("\n" + "="*60)
    print("📄 DOCUMENT ANALYSIS")
    print("="*60)
    
    # Initialize client with configured model
    model_config = get_model_config()
    config = OpenRouterConfig(
        default_model=model_config["name"],
        temperature=model_config["temperature"],
        max_tokens=model_config["max_tokens"]
    )
    client = OpenRouterClient(config)
    
    # Sample document content
    document_content = """
    Artificial Intelligence (AI) represents one of the most transformative technologies 
    of the 21st century. It encompasses machine learning, deep learning, natural language 
    processing, and computer vision. AI systems can now perform tasks that traditionally 
    required human intelligence, such as recognizing speech, making decisions, and solving 
    complex problems.
    
    Machine learning, a subset of AI, enables computers to learn from data without being 
    explicitly programmed. Deep learning uses neural networks with multiple layers to 
    analyze various factors of data. These technologies have applications in healthcare, 
    finance, transportation, and many other industries.
    
    The future of AI holds tremendous potential for improving human lives, but also 
    presents challenges related to ethics, privacy, and job displacement. Responsible 
    AI development requires careful consideration of these factors.
    """
    
    # Extract keywords
    keywords = client.get_document_keywords(document_content, max_keywords=5)
    print(f"🔑 Keywords: {keywords}")
    
    # Generate overview
    overview = client.get_document_overview(document_content)
    print(f"📋 Overview: {overview}")
    
    # Quality assessment
    quality = client.get_content_quality_assessment(document_content)
    print(f"📊 Quality Assessment:")
    for metric, score in quality.items():
        if isinstance(score, (int, float)):
            print(f"   {metric}: {score}/100")
        elif isinstance(score, list):
            print(f"   {metric}: {', '.join(score)}")
    
    # Comprehensive insights
    insights = client.get_document_insights(document_content, "ai_document.md")
    print(f"\n📈 Document Insights:")
    print(f"   Filename: {insights['filename']}")
    print(f"   Keywords: {insights['keywords']}")
    print(f"   Overview: {insights['overview']}")
    print(f"   Overall Quality: {insights['quality_assessment']['overall_quality']}/100")


def example_flexible_prompt_composition():
    """Example: Flexible prompt composition."""
    print("\n" + "="*60)
    print("🎨 FLEXIBLE PROMPT COMPOSITION")
    print("="*60)
    
    # Initialize client with configured model
    model_config = get_model_config()
    config = OpenRouterConfig(
        default_model=model_config["name"],
        temperature=model_config["temperature"],
        max_tokens=model_config["max_tokens"]
    )
    client = OpenRouterClient(config)
    
    # Different types of prompts
    prompts = [
        # Simple question
        "What is the weather like today?",
        
        # Complex analytical question
        "Analyze the impact of social media on modern communication, considering both positive and negative aspects.",
        
        # Creative writing
        "Write a short story about a time traveler who visits ancient Egypt.",
        
        # Technical explanation
        "Explain how blockchain technology works, including consensus mechanisms and cryptographic principles.",
        
        # Multi-step reasoning
        "If a company has 200 employees and 30% work remotely, how many work in the office? Show your reasoning.",
        
        # Role-based prompt
        "As a financial advisor, explain the benefits of compound interest to a young investor.",
        
        # Structured output request
        "List the top 5 programming languages for web development and explain why each is popular."
    ]
    
    for i, prompt in enumerate(prompts, 1):
        print(f"\n🎯 Prompt {i}: {prompt[:50]}...")
        try:
            response = client.simple_chat(prompt, model=get_model_name())
            print(f"🤖 Response: {response[:100]}...")
        except Exception as e:
            print(f"❌ Error: {e}")


def example_multi_model_usage():
    """Example: Using different models."""
    print("\n" + "="*60)
    print("🤖 MULTI-MODEL USAGE")
    print("="*60)
    
    # Initialize client with configured model
    model_config = get_model_config()
    config = OpenRouterConfig(
        default_model=model_config["name"],
        temperature=model_config["temperature"],
        max_tokens=model_config["max_tokens"]
    )
    client = OpenRouterClient(config)
    
    # Test different models
    models_to_test = [
        get_model_name("kimi"),
        get_model_name("deepseek")
    ]
    
    test_prompt = "Explain the concept of artificial intelligence in one sentence."
    
    for model in models_to_test:
        print(f"\n🤖 Testing model: {model}")
        try:
            response = client.simple_chat(test_prompt, model=model)
            print(f"✅ Response: {response}")
        except Exception as e:
            print(f"❌ Error with {model}: {e}")


def example_error_handling():
    """Example: Error handling and monitoring."""
    print("\n" + "="*60)
    print("🛡️ ERROR HANDLING & MONITORING")
    print("="*60)
    
    # Initialize client with configured model
    model_config = get_model_config()
    config = OpenRouterConfig(
        default_model=model_config["name"],
        temperature=model_config["temperature"],
        max_tokens=model_config["max_tokens"]
    )
    client = OpenRouterClient(config)
    
    # Test with invalid model
    print("🧪 Testing invalid model:")
    try:
        response = client.simple_chat("Hello", model="invalid/model/name")
        print("Unexpected success with invalid model")
    except Exception as e:
        print(f"✅ Expected error: {type(e).__name__}")
    
    # Get client statistics
    stats = client.get_stats()
    print(f"\n📊 Client Statistics:")
    for key, value in stats.items():
        print(f"   {key}: {value}")
    
    # Circuit breaker status
    print(f"\n🔌 Circuit Breaker State: {client.circuit_breaker.state}")
    print(f"   Failures: {client.circuit_breaker.failure_count}")


def example_analysis_engine():
    """Example: OpenRouter analysis engine."""
    print("\n" + "="*60)
    print("🔬 ANALYSIS ENGINE")
    print("="*60)
    
    # Mock config for testing
    class MockConfig:
        def __init__(self):
            self.api_key = get_api_key()
            self.model = get_model_name()
            model_config = get_model_config()
            self.temperature = model_config["temperature"]
            self.max_tokens = model_config["max_tokens"]
    
    config = MockConfig()
    
    # Initialize analysis engine
    engine = OpenRouterAnalysisEngine(config, "example_analysis")
    
    # Sample content for analysis
    tech_content = """
    Quantum computing is a revolutionary technology that leverages quantum mechanical 
    phenomena like superposition and entanglement to perform computations. Unlike 
    classical computers that use bits (0 or 1), quantum computers use quantum bits 
    (qubits) that can exist in multiple states simultaneously.
    """
    
    product_content = """
    A quantum-powered machine learning platform that accelerates drug discovery 
    by simulating molecular interactions at the quantum level. The platform can 
    predict drug efficacy and side effects more accurately than traditional methods.
    """
    
    # Perform tech-product feasibility analysis
    print("🔬 Analyzing tech-product feasibility...")
    analysis = engine.analyze_tech_product_feasibility(
        tech_content=tech_content,
        product_content=product_content,
        tech_name="Quantum Computing",
        product_name="Quantum ML Drug Discovery Platform"
    )
    
    print(f"📊 Analysis completed:")
    print(f"   Technology: {analysis['tech_name']}")
    print(f"   Product: {analysis['product_name']}")
    print(f"   Analysis length: {len(analysis['analysis'])} characters")
    print(f"   Timestamp: {analysis['analysis_timestamp']}")


def example_quick_chat():
    """Example: Quick chat function."""
    print("\n" + "="*60)
    print("⚡ QUICK CHAT")
    print("="*60)
    
    # Simple one-liner chat
    response = quick_chat("What is the meaning of life?", model=get_model_name())
    print(f"🤖 Quick Response: {response}")


def main():
    """Main example execution."""
    print("🚀 OpenRouter Example Script")
    print("="*60)
    
    if not check_api_key():
        return
    
    try:
        # Run all examples
        client = example_basic_chat()
        example_chat_completion()
        example_streaming()
        asyncio.run(example_async_operations())
        example_conversation_management()
        example_document_analysis()
        example_flexible_prompt_composition()
        example_multi_model_usage()
        example_error_handling()
        example_analysis_engine()
        example_quick_chat()
        
        print("\n" + "="*60)
        print("✅ All examples completed successfully!")
        print("="*60)
        
        # Final statistics
        if 'client' in locals():
            stats = client.get_stats()
            print(f"\n📈 Final Statistics:")
            print(f"   Total Requests: {stats['total_requests']}")
            print(f"   Success Rate: {stats['success_rate']:.1f}%")
            print(f"   Circuit Breaker State: {stats['circuit_breaker_state']}")
        
    except Exception as e:
        print(f"\n❌ Example execution failed: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main() 