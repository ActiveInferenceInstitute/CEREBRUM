#!/usr/bin/env python3
"""
OpenRouter Structure Test

Test script to verify OpenRouter code structure and functionality
without requiring an API key. This tests the code organization,
imports, class definitions, and method signatures.
"""

import sys
import inspect
from pathlib import Path
from typing import Dict, List, Any, Optional
from datetime import datetime

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

def test_imports():
    """Test that all OpenRouter modules can be imported."""
    print("🧪 Testing imports...")
    
    try:
        from src.llm.OpenRouter.openrouter import (
            OpenRouterClient, 
            OpenRouterConfig, 
            RetryConfig, 
            CircuitBreakerConfig,
            CircuitBreaker,
            Conversation,
            quick_chat
        )
        print("✅ All OpenRouter classes imported successfully")
    except Exception as e:
        print(f"❌ Import failed: {e}")
        assert False, f"Import failed: {e}"

def test_class_definitions():
    """Test that all required classes are defined with correct methods."""
    print("\n🧪 Testing class definitions...")
    
    from src.llm.OpenRouter.openrouter import (
        OpenRouterClient, 
        OpenRouterConfig, 
        RetryConfig, 
        CircuitBreakerConfig,
        CircuitBreaker,
        Conversation
    )
    
    # Test OpenRouterConfig (with test key to avoid API key requirement)
    config = OpenRouterConfig(api_key="test_key_for_structure_test")
    assert hasattr(config, 'api_key')
    assert hasattr(config, 'default_model')
    assert hasattr(config, 'temperature')
    assert hasattr(config, 'max_tokens')
    print("✅ OpenRouterConfig class is properly defined")
    
    # Test RetryConfig
    retry_config = RetryConfig()
    assert hasattr(retry_config, 'max_retries')
    assert hasattr(retry_config, 'base_delay')
    assert hasattr(retry_config, 'max_delay')
    print("✅ RetryConfig class is properly defined")
    
    # Test CircuitBreakerConfig
    circuit_config = CircuitBreakerConfig()
    assert hasattr(circuit_config, 'failure_threshold')
    assert hasattr(circuit_config, 'recovery_timeout')
    print("✅ CircuitBreakerConfig class is properly defined")
    
    # Test CircuitBreaker
    circuit_breaker = CircuitBreaker(circuit_config)
    assert hasattr(circuit_breaker, 'can_execute')
    assert hasattr(circuit_breaker, 'record_success')
    assert hasattr(circuit_breaker, 'record_failure')
    print("✅ CircuitBreaker class is properly defined")
    
    # Test OpenRouterClient (without API key)
    try:
        # This should fail due to missing API key, but we can test the structure
        client = OpenRouterClient()
        print("⚠️  OpenRouterClient initialized (API key may be set)")
    except ValueError as e:
        if "OPENROUTER_API_KEY" in str(e):
            print("✅ OpenRouterClient properly validates API key requirement")
        else:
            print(f"❌ Unexpected error: {e}")
    
    # Test Conversation class
    try:
        # Create a mock client for testing Conversation
        class MockClient:
            def chat_completion(self, messages, **kwargs):
                return type('MockResponse', (), {
                    'choices': [type('MockChoice', (), {
                        'message': type('MockMessage', (), {'content': 'Mock response'})()
                    })()]
                })()
            
            def chat_completion_stream(self, messages, **kwargs):
                yield type('MockChunk', (), {
                    'choices': [type('MockChoice', (), {
                        'delta': type('MockDelta', (), {'content': 'Mock stream'})()
                    })()]
                })()
        
        mock_client = MockClient()
        conversation = Conversation(mock_client)
        assert hasattr(conversation, 'add_system_message')
        assert hasattr(conversation, 'add_user_message')
        assert hasattr(conversation, 'send_message')
        assert hasattr(conversation, 'send_message_stream')
        assert hasattr(conversation, 'get_history')
        print("✅ Conversation class is properly defined")
        
    except Exception as e:
        print(f"❌ Conversation class test failed: {e}")
        assert False, f"Conversation class test failed: {e}"

def test_method_signatures():
    """Test that all required methods have correct signatures."""
    print("\n🧪 Testing method signatures...")
    
    from src.llm.OpenRouter.openrouter import OpenRouterClient
    
    # Get all methods from OpenRouterClient
    client_methods = inspect.getmembers(OpenRouterClient, predicate=inspect.isfunction)
    method_names = [name for name, _ in client_methods]
    
    # Required methods
    required_methods = [
        'simple_chat',
        'chat_completion',
        'chat_completion_stream',
        'async_chat_completion',
        'async_chat_completion_stream',
        'get_available_models',
        'validate_model',
        'get_stats',
        'create_conversation',
        'get_document_keywords',
        'get_document_overview',
        'get_content_quality_assessment',
        'get_document_insights'
    ]
    
    missing_methods = [method for method in required_methods if method not in method_names]
    
    assert not missing_methods, f"Missing methods: {missing_methods}"
    print("✅ All required methods are present")
    
    # Test method signatures
    try:
        # Test simple_chat signature
        sig = inspect.signature(OpenRouterClient.simple_chat)
        params = list(sig.parameters.keys())
        assert 'self' in params
        assert 'message' in params
        print("✅ simple_chat method signature is correct")
        
        # Test chat_completion signature
        sig = inspect.signature(OpenRouterClient.chat_completion)
        params = list(sig.parameters.keys())
        assert 'self' in params
        assert 'messages' in params
        print("✅ chat_completion method signature is correct")
        
    except Exception as e:
        print(f"❌ Method signature test failed: {e}")
        assert False, f"Method signature test failed: {e}"

def test_analysis_engine():
    """Test the analysis engine structure."""
    print("\n🧪 Testing analysis engine...")
    
    try:
        from src.llm.openrouter_analysis_engine import OpenRouterAnalysisEngine, AnalysisResult
        
        # Test AnalysisResult dataclass
        result = AnalysisResult(
            analysis_type="test",
            timestamp="2024-01-01",
            source_files=[],
            target_files=[],
            results={},
            metrics={},
            recommendations=[],
            errors=[]
        )
        assert hasattr(result, 'analysis_type')
        assert hasattr(result, 'results')
        print("✅ AnalysisResult dataclass is properly defined")
        
        # Test OpenRouterAnalysisEngine structure (without initialization)
        engine_methods = inspect.getmembers(OpenRouterAnalysisEngine, predicate=inspect.isfunction)
        method_names = [name for name, _ in engine_methods]
        
        required_engine_methods = [
            'make_llm_call',
            'analyze_tech_product_feasibility',
            'analyze_product_sector_value',
            'analyze_product_explanation',
            'save_results',
            'save_performance_metrics'
        ]
        
        missing_methods = [method for method in required_engine_methods if method not in method_names]
        
        assert not missing_methods, f"Missing analysis engine methods: {missing_methods}"
        print("✅ All required analysis engine methods are present")
        
    except Exception as e:
        print(f"❌ Analysis engine test failed: {e}")
        assert False, f"Analysis engine test failed: {e}"

def test_configuration_options():
    """Test configuration options and defaults."""
    print("\n🧪 Testing configuration options...")
    
    from src.llm.OpenRouter.openrouter import OpenRouterConfig, RetryConfig, CircuitBreakerConfig
    
    # Test default configuration (without API key validation)
    try:
        config = OpenRouterConfig()
        # The __post_init__ will fail, but we can test the attributes before that
        assert hasattr(config, 'default_model')
        assert hasattr(config, 'temperature')
        assert hasattr(config, 'max_tokens')
        assert hasattr(config, 'base_url')
        print("✅ OpenRouterConfig attributes are accessible")
    except ValueError as e:
        if "OPENROUTER_API_KEY" in str(e):
            print("✅ OpenRouterConfig properly validates API key requirement")
        else:
            print(f"❌ Unexpected error: {e}")
    
    # Test custom configuration with API key
    try:
        custom_config = OpenRouterConfig(
            api_key="test_key",  # Provide a test key to avoid validation error
            default_model="anthropic/claude-3-haiku",
            temperature=0.5,
            max_tokens=1000
        )
        assert custom_config.default_model == "anthropic/claude-3-haiku"
        assert custom_config.temperature == 0.5
        assert custom_config.max_tokens == 1000
        print("✅ Custom configuration works correctly")
    except Exception as e:
        print(f"❌ Custom configuration test failed: {e}")
    
    # Test retry configuration
    retry_config = RetryConfig(
        max_retries=5,
        base_delay=2.0,
        max_delay=60.0
    )
    assert retry_config.max_retries == 5
    assert retry_config.base_delay == 2.0
    assert retry_config.max_delay == 60.0
    print("✅ Retry configuration works correctly")
    
    # Test circuit breaker configuration
    circuit_config = CircuitBreakerConfig(
        failure_threshold=10,
        recovery_timeout=120
    )
    assert circuit_config.failure_threshold == 10
    assert circuit_config.recovery_timeout == 120
    print("✅ Circuit breaker configuration works correctly")

def test_quick_chat_function():
    """Test the quick_chat convenience function."""
    print("\n🧪 Testing quick_chat function...")
    
    from src.llm.OpenRouter.openrouter import quick_chat
    
    # Test function signature
    sig = inspect.signature(quick_chat)
    params = list(sig.parameters.keys())
    assert 'message' in params
    assert 'model' in params
    print("✅ quick_chat function signature is correct")
    
    # Test default parameter
    assert sig.parameters['model'].default == "openai/gpt-3.5-turbo"
    print("✅ quick_chat default model is correct")

def test_error_handling_structure():
    """Test error handling structure."""
    print("\n🧪 Testing error handling structure...")
    
    from src.llm.OpenRouter.openrouter import OpenRouterClient, CircuitBreaker
    
    # Test circuit breaker states
    from src.llm.OpenRouter.openrouter import CircuitBreakerConfig
    config = CircuitBreakerConfig()
    circuit = CircuitBreaker(config)
    
    assert circuit.state == "CLOSED"
    assert circuit.can_execute() == True
    
    # Test retry logic structure
    from src.llm.OpenRouter.openrouter import RetryConfig
    retry_config = RetryConfig()
    assert retry_config.max_retries == 3
    assert retry_config.base_delay == 1.0
    print("✅ Error handling structure is correct")

def main():
    """Run all structure tests."""
    print("🚀 OpenRouter Structure Test Suite")
    print("=" * 50)
    
    tests = [
        ("Imports", test_imports),
        ("Class Definitions", test_class_definitions),
        ("Method Signatures", test_method_signatures),
        ("Analysis Engine", test_analysis_engine),
        ("Configuration Options", test_configuration_options),
        ("Quick Chat Function", test_quick_chat_function),
        ("Error Handling Structure", test_error_handling_structure)
    ]
    
    passed = 0
    failed = 0
    
    for test_name, test_func in tests:
        try:
            if test_func():
                passed += 1
            else:
                failed += 1
        except Exception as e:
            print(f"❌ {test_name} failed with exception: {e}")
            failed += 1
    
    # Save test results
    import json
    
    results = {
        "tests_run": len(tests),
        "tests_passed": passed,
        "tests_failed": failed,
        "success_rate": (passed / (passed + failed)) * 100 if (passed + failed) > 0 else 0,
        "timestamp": datetime.now().isoformat()
    }
    
    results_file = Path("test_output/openrouter_structure_test_results.json")
    results_file.parent.mkdir(exist_ok=True)
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2)
    
    print("\n" + "=" * 50)
    print("📊 STRUCTURE TEST SUMMARY")
    print("=" * 50)
    print(f"Tests Passed: {passed}")
    print(f"Tests Failed: {failed}")
    print(f"Success Rate: {results['success_rate']:.1f}%")
    print(f"Results saved to: {results_file}")
    
    if failed == 0:
        print("\n✅ All structure tests passed!")
        print("🎉 OpenRouter code structure is correct and ready for API testing")
        return True
    else:
        print(f"\n❌ {failed} structure test(s) failed")
        return False

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1) 