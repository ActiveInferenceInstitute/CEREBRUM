#!/usr/bin/env python3
"""
OpenRouter Functionality Test Suite

Comprehensive test script to verify that all OpenRouter methods work correctly
with real LLM calls, including flexible prompt composition and all available functionality.

This script tests:
1. Basic client initialization and configuration
2. All chat completion methods (sync, async, streaming)
3. Conversation management
4. Document analysis methods
5. Model validation and availability
6. Error handling and retry logic
7. Performance monitoring
8. Flexible prompt composition

Usage:
    python test_openrouter_functionality.py

Requirements:
    - OPENROUTER_API_KEY environment variable set
    - Internet connection for API calls
"""

import sys
import asyncio
import json
import time
from pathlib import Path
from typing import Dict, List
from datetime import datetime

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from src.llm.OpenRouter.openrouter import (
    OpenRouterClient, 
    OpenRouterConfig, 
    RetryConfig, 
    CircuitBreakerConfig,
    Conversation,
    quick_chat
)
from src.llm.openrouter_analysis_engine import OpenRouterAnalysisEngine
from src.llm.config import get_model_name, get_model_config, validate_api_key, get_api_key


class OpenRouterTester:
    """Comprehensive tester for OpenRouter functionality."""
    
    def __init__(self):
        """Initialize the tester."""
        self.results = {
            "tests_run": 0,
            "tests_passed": 0,
            "tests_failed": 0,
            "errors": [],
            "performance_metrics": {}
        }
        self.start_time = time.time()
        
        # Check for API key
        if not validate_api_key():
            raise ValueError("OPENROUTER_API_KEY environment variable is required")
        
        # Get default model
        self.default_model = get_model_name()
        print(f"Using model: {self.default_model}")
        
        print("🚀 OpenRouter Functionality Test Suite")
        print("=" * 50)
    
    def run_test(self, test_name: str, test_func, *args, **kwargs):
        """Run a test and record results."""
        self.results["tests_run"] += 1
        print(f"\n🧪 Running: {test_name}")
        
        try:
            result = test_func(*args, **kwargs)
            print(f"✅ PASSED: {test_name}")
            self.results["tests_passed"] += 1
            return result
        except Exception as e:
            print(f"❌ FAILED: {test_name}")
            print(f"   Error: {str(e)}")
            self.results["tests_failed"] += 1
            self.results["errors"].append({
                "test": test_name,
                "error": str(e),
                "timestamp": datetime.now().isoformat()
            })
            return None
    
    async def run_async_test(self, test_name: str, test_func, *args, **kwargs):
        """Run an async test and record results."""
        self.results["tests_run"] += 1
        print(f"\n🧪 Running: {test_name}")
        
        try:
            result = await test_func(*args, **kwargs)
            print(f"✅ PASSED: {test_name}")
            self.results["tests_passed"] += 1
            return result
        except Exception as e:
            print(f"❌ FAILED: {test_name}")
            print(f"   Error: {str(e)}")
            self.results["tests_failed"] += 1
            self.results["errors"].append({
                "test": test_name,
                "error": str(e),
                "timestamp": datetime.now().isoformat()
            })
            return None
    
    def test_client_initialization(self):
        """Test basic client initialization."""
        # Test with default config
        client = OpenRouterClient()
        assert client is not None
        assert hasattr(client, 'client')
        assert hasattr(client, 'async_client')
        assert hasattr(client, 'circuit_breaker')
        
        # Test with custom config
        model_config = get_model_config()
        config = OpenRouterConfig(
            default_model=model_config["name"],
            temperature=model_config["temperature"],
            max_tokens=model_config["max_tokens"],
            retry_config=RetryConfig(max_retries=3),
            circuit_breaker_config=CircuitBreakerConfig(failure_threshold=3),
            enable_logging=True
        )
        client = OpenRouterClient(config)
        assert client.config.default_model == model_config["name"]
        assert client.config.temperature == model_config["temperature"]
        
        return client
    
    def test_simple_chat(self, client: OpenRouterClient):
        """Test simple chat functionality."""
        # Test basic chat
        response = client.simple_chat("Hello, how are you?")
        assert response is not None
        assert len(response) > 0
        assert isinstance(response, str)
        
        # Test with different model
        response = client.simple_chat(
            "What is 2+2?", 
            model=self.default_model
        )
        assert response is not None
        assert len(response) > 0
        
        return response
    
    def test_chat_completion(self, client: OpenRouterClient):
        """Test full chat completion API."""
        messages = [
            {"role": "system", "content": "You are a helpful assistant."},
            {"role": "user", "content": "Explain quantum computing in simple terms."}
        ]
        
        response = client.chat_completion(
            messages=messages,
            model=self.default_model,
            temperature=0.7,
            max_tokens=200
        )
        
        assert response is not None
        assert hasattr(response, 'choices')
        assert len(response.choices) > 0
        assert hasattr(response.choices[0], 'message')
        assert hasattr(response.choices[0].message, 'content')
        
        content = response.choices[0].message.content
        assert content is not None
        assert len(content) > 0
        
        return response
    
    def test_chat_completion_stream(self, client: OpenRouterClient):
        """Test streaming chat completion."""
        messages = [
            {"role": "system", "content": "You are a helpful assistant."},
            {"role": "user", "content": "Write a short story about a robot."}
        ]
        
        full_response = ""
        for chunk in client.chat_completion_stream(
            messages=messages,
            model=self.default_model,
            max_tokens=150
        ):
            if chunk.choices[0].delta.content:
                content = chunk.choices[0].delta.content
                full_response += content
                print(content, end='', flush=True)
        
        print()  # New line after streaming
        assert len(full_response) > 0
        
        return full_response
    
    async def test_async_chat_completion(self, client: OpenRouterClient):
        """Test async chat completion."""
        messages = [
            {"role": "system", "content": "You are a helpful assistant."},
            {"role": "user", "content": "What are the benefits of renewable energy?"}
        ]
        
        response = await client.async_chat_completion(
            messages=messages,
            model=self.default_model,
            temperature=0.5,
            max_tokens=250
        )
        
        assert response is not None
        assert hasattr(response, 'choices')
        assert len(response.choices) > 0
        
        content = response.choices[0].message.content
        assert content is not None
        assert len(content) > 0
        
        return response
    
    async def test_async_chat_completion_stream(self, client: OpenRouterClient):
        """Test async streaming chat completion."""
        messages = [
            {"role": "system", "content": "You are a helpful assistant."},
            {"role": "user", "content": "Explain machine learning algorithms."}
        ]
        
        full_response = ""
        async for chunk in client.async_chat_completion_stream(
            messages=messages,
            model=self.default_model,
            max_tokens=200
        ):
            if chunk.choices[0].delta.content:
                content = chunk.choices[0].delta.content
                full_response += content
                print(content, end='', flush=True)
        
        print()  # New line after streaming
        assert len(full_response) > 0
        
        return full_response
    
    def test_conversation_management(self, client: OpenRouterClient):
        """Test conversation management functionality."""
        import traceback
        try:
            # Create conversation
            conversation = client.create_conversation()
            assert conversation is not None
            assert isinstance(conversation, Conversation)

            # Add system message
            conversation.add_system_message("You are an expert in artificial intelligence.")

            # Send messages
            response1 = conversation.send_message("What is deep learning?", model=self.default_model)
            assert response1 is not None
            assert len(response1) > 0

            response2 = conversation.send_message("How does it differ from traditional machine learning?", model=self.default_model)
            assert response2 is not None
            assert len(response2) > 0

            # Check history
            history = conversation.get_history()
            assert len(history) >= 5  # system + 2 user + 2 assistant messages

            # Test streaming conversation
            conversation2 = client.create_conversation()
            conversation2.add_system_message("You are a creative writer.")

            full_response = ""
            for chunk in conversation2.send_message_stream("Write a haiku about technology.", model=self.default_model):
                full_response += chunk
                print(chunk, end='', flush=True)

            print()  # New line after streaming
            assert len(full_response) > 0

            return conversation
        except Exception as e:
            print(f"Conversation Management Exception: {e}")
            traceback.print_exc()
            raise
    
    def test_model_validation(self, client: OpenRouterClient):
        """Test model validation and availability."""
        # Test static model list
        models = client.get_available_models()
        assert isinstance(models, list)
        assert len(models) > 0
        assert "openai/gpt-3.5-turbo" in models
        
        # Test model validation
        is_valid = client.validate_model(self.default_model)
        assert is_valid is True
        
        # Test invalid model
        is_invalid = client.validate_model("invalid/model/name")
        assert is_invalid is False
        
        # Test API model fetching (may fail if API is down)
        try:
            api_models = client.get_available_models_from_api()
            assert isinstance(api_models, list)
            print(f"   Retrieved {len(api_models)} models from API")
        except Exception as e:
            print(f"   API model fetch failed (expected): {e}")
        
        return models
    
    def test_document_analysis_methods(self, client: OpenRouterClient):
        """Test document analysis methods."""
        sample_content = """
        Artificial Intelligence (AI) is a branch of computer science that aims to create 
        intelligent machines that work and react like humans. Some of the activities 
        computers with artificial intelligence are designed for include speech recognition, 
        learning, planning, and problem solving. AI has applications in various fields 
        including healthcare, finance, transportation, and entertainment.
        
        Machine learning is a subset of AI that enables computers to learn and improve 
        from experience without being explicitly programmed. Deep learning, a subset of 
        machine learning, uses neural networks with multiple layers to analyze various 
        factors of data.
        """
        
        # Test keyword extraction
        keywords = client.get_document_keywords(sample_content, max_keywords=5)
        assert isinstance(keywords, list)
        assert len(keywords) <= 5
        assert all(isinstance(k, str) for k in keywords)
        print(f"   Extracted keywords: {keywords}")
        
        # Test document overview
        overview = client.get_document_overview(sample_content)
        assert isinstance(overview, str)
        assert len(overview) > 0
        assert len(overview) <= 200
        print(f"   Overview: {overview}")
        
        # Test quality assessment
        quality = client.get_content_quality_assessment(sample_content)
        assert isinstance(quality, dict)
        assert "clarity" in quality
        assert "completeness" in quality
        assert "overall_quality" in quality
        assert all(0 <= score <= 100 for score in [
            quality["clarity"], quality["completeness"], quality["overall_quality"]
        ])
        print(f"   Quality scores: {quality}")
        
        # Test comprehensive insights
        insights = client.get_document_insights(sample_content, "ai_document.md")
        assert isinstance(insights, dict)
        assert "keywords" in insights
        assert "overview" in insights
        assert "quality_assessment" in insights
        assert "filename" in insights
        assert insights["filename"] == "ai_document.md"
        
        return insights
    
    def test_flexible_prompt_composition(self, client: OpenRouterClient):
        """Test flexible prompt composition capabilities."""
        
        # Test different prompt styles
        prompts = [
            # Simple question
            "What is the capital of France?",
            
            # Complex analytical question
            "Analyze the impact of climate change on global agriculture, considering economic, social, and environmental factors.",
            
            # Creative writing prompt
            "Write a short story about a time traveler who visits ancient Rome.",
            
            # Technical explanation
            "Explain how blockchain technology works, including consensus mechanisms and cryptographic principles.",
            
            # Multi-step reasoning
            "If a company has 100 employees and 20% work remotely, how many work in the office? Show your reasoning step by step."
        ]
        
        responses = []
        for i, prompt in enumerate(prompts):
            print(f"   Testing prompt {i+1}: {prompt[:50]}...")
            response = client.simple_chat(prompt, model=self.default_model)
            assert response is not None
            assert len(response) > 0
            responses.append(response)
        
        # Test with different models
        models_to_test = ["anthropic/claude-3-haiku"]
        for model in models_to_test:
            try:
                response = client.simple_chat("Hello, how are you?", model=model)
                assert response is not None
                print(f"   Model {model} works correctly")
            except Exception as e:
                print(f"   Model {model} failed (may not be available): {e}")
        
        return responses
    
    def test_error_handling_and_retry(self, client: OpenRouterClient):
        """Test error handling and retry logic."""
        # Test with invalid model (should fail gracefully)
        try:
            response = client.simple_chat("Hello", model="invalid/model/name")
            print("   Invalid model test - unexpected success")
        except Exception as e:
            print(f"   Invalid model test - expected failure: {type(e).__name__}")
        
        # Test circuit breaker functionality
        circuit_breaker = client.circuit_breaker
        assert circuit_breaker.state == "CLOSED"
        
        # Test stats tracking
        stats = client.get_stats()
        assert isinstance(stats, dict)
        assert "total_requests" in stats
        assert "successful_requests" in stats
        assert "failed_requests" in stats
        assert "success_rate" in stats
        
        print(f"   Client stats: {stats}")
        
        return stats
    
    def test_quick_chat_function(self):
        """Test the quick_chat convenience function."""
        response = quick_chat("What is the meaning of life?", model=self.default_model)
        assert response is not None
        assert len(response) > 0
        assert isinstance(response, str)
        
        return response
    
    def test_analysis_engine_integration(self):
        """Test the OpenRouter analysis engine integration."""
        # Create a mock config for testing
        class MockConfig:
            def __init__(self):
                self.api_key = get_api_key()
                self.model = get_model_name()
                model_config = get_model_config()
                self.temperature = model_config["temperature"]
                self.max_tokens = model_config["max_tokens"]
        
        config = MockConfig()
        
        # Test engine initialization
        engine = OpenRouterAnalysisEngine(config, "test_analysis")
        assert engine is not None
        assert hasattr(engine, 'client')
        assert hasattr(engine, 'make_llm_call')
        
        # Test basic LLM call
        response = engine.make_llm_call("What is artificial intelligence?")
        assert response is not None
        assert len(response) > 0
        
        # Test tech-product analysis
        tech_content = "Quantum computing is a revolutionary technology that uses quantum mechanical phenomena."
        product_content = "A quantum-powered machine learning platform for drug discovery."
        
        analysis = engine.analyze_tech_product_feasibility(
            tech_content=tech_content,
            product_content=product_content,
            tech_name="Quantum Computing",
            product_name="Quantum ML Platform"
        )
        
        assert analysis is not None
        assert "tech_name" in analysis
        assert "product_name" in analysis
        assert "analysis" in analysis
        assert not analysis.get("error", False)
        
        return engine
    
    def print_summary(self):
        """Print test summary."""
        end_time = time.time()
        duration = end_time - self.start_time
        
        print("\n" + "=" * 50)
        print("📊 TEST SUMMARY")
        print("=" * 50)
        print(f"Tests Run: {self.results['tests_run']}")
        print(f"Tests Passed: {self.results['tests_passed']}")
        print(f"Tests Failed: {self.results['tests_failed']}")
        print(f"Success Rate: {(self.results['tests_passed'] / max(self.results['tests_run'], 1)) * 100:.1f}%")
        print(f"Duration: {duration:.2f} seconds")
        
        if self.results['errors']:
            print(f"\n❌ ERRORS ({len(self.results['errors'])}):")
            for error in self.results['errors']:
                print(f"   - {error['test']}: {error['error']}")
        
        # Save results
        results_file = Path("test_output/openrouter_test_results.json")
        results_file.parent.mkdir(exist_ok=True)
        with open(results_file, 'w') as f:
            json.dump(self.results, f, indent=2)
        
        print(f"\n📄 Results saved to: {results_file}")
        
        return self.results


async def main():
    """Main test execution function."""
    tester = OpenRouterTester()
    
    try:
        # Basic functionality tests
        client = tester.run_test("Client Initialization", tester.test_client_initialization)
        
        if client:
            tester.run_test("Simple Chat", tester.test_simple_chat, client)
            tester.run_test("Chat Completion", tester.test_chat_completion, client)
            tester.run_test("Chat Completion Stream", tester.test_chat_completion_stream, client)
            await tester.run_async_test("Async Chat Completion", tester.test_async_chat_completion, client)
            await tester.run_async_test("Async Chat Completion Stream", tester.test_async_chat_completion_stream, client)
            tester.run_test("Conversation Management", tester.test_conversation_management, client)
            tester.run_test("Model Validation", tester.test_model_validation, client)
            tester.run_test("Document Analysis Methods", tester.test_document_analysis_methods, client)
            tester.run_test("Flexible Prompt Composition", tester.test_flexible_prompt_composition, client)
            tester.run_test("Error Handling and Retry", tester.test_error_handling_and_retry, client)
        
        # Standalone function tests
        tester.run_test("Quick Chat Function", tester.test_quick_chat_function)
        
        # Analysis engine tests
        tester.run_test("Analysis Engine Integration", tester.test_analysis_engine_integration)
        
    except Exception as e:
        print(f"❌ Test suite failed with error: {e}")
        tester.results["errors"].append({
            "test": "Test Suite",
            "error": str(e),
            "timestamp": datetime.now().isoformat()
        })
    
    # Print summary
    results = tester.print_summary()
    
    # Exit with appropriate code
    if results["tests_failed"] > 0:
        print("\n❌ Some tests failed!")
        sys.exit(1)
    else:
        print("\n✅ All tests passed!")
        sys.exit(0)


if __name__ == "__main__":
    asyncio.run(main()) 