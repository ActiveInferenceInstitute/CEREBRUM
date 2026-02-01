"""
Tests for Ollama integration module.
"""

import pytest
from unittest.mock import patch, MagicMock
import json

from src.llm.ollama.client import (
    OllamaClient,
    OllamaResponse,
    ChatMessage,
    OllamaError,
    OllamaConnectionError
)
from src.llm.ollama.utils import (
    check_ollama_running,
    list_models,
    get_default_model,
    format_model_size
)


class TestOllamaClient:
    """Test OllamaClient class."""
    
    def test_client_initialization_defaults(self):
        """Test client initializes with default values."""
        client = OllamaClient()
        assert client.base_url == "http://localhost:11434"
        assert client.default_model == "llama3.2"
        assert client.timeout == 120
    
    def test_client_initialization_custom(self):
        """Test client with custom parameters."""
        client = OllamaClient(
            base_url="http://custom:8080",
            model="mistral",
            timeout=60
        )
        assert client.base_url == "http://custom:8080"
        assert client.default_model == "mistral"
        assert client.timeout == 60
    
    @patch('requests.get')
    def test_is_available_true(self, mock_get):
        """Test is_available returns True when server responds."""
        mock_get.return_value.status_code = 200
        client = OllamaClient()
        assert client.is_available() is True
    
    @patch('requests.get')
    def test_is_available_false(self, mock_get):
        """Test is_available returns False when server unreachable."""
        mock_get.side_effect = Exception("Connection refused")
        client = OllamaClient()
        assert client.is_available() is False
    
    @patch('requests.get')
    def test_list_models(self, mock_get):
        """Test listing models."""
        mock_get.return_value.status_code = 200
        mock_get.return_value.json.return_value = {
            "models": [
                {"name": "llama3.2", "size": 4700000000},
                {"name": "mistral", "size": 4100000000}
            ]
        }
        
        client = OllamaClient()
        models = client.list_models()
        
        assert len(models) == 2
        assert models[0]["name"] == "llama3.2"
    
    @patch('requests.post')
    def test_generate_success(self, mock_post):
        """Test successful text generation."""
        mock_post.return_value.status_code = 200
        mock_post.return_value.json.return_value = {
            "model": "llama3.2",
            "response": "Active inference is a framework...",
            "done": True,
            "total_duration": 1000000000,
            "eval_count": 50
        }
        
        client = OllamaClient()
        response = client.generate("What is active inference?")
        
        assert isinstance(response, OllamaResponse)
        assert response.text == "Active inference is a framework..."
        assert response.model == "llama3.2"
        assert response.done is True
    
    @patch('requests.post')
    def test_generate_with_options(self, mock_post):
        """Test generation with custom options."""
        mock_post.return_value.status_code = 200
        mock_post.return_value.json.return_value = {
            "model": "mistral",
            "response": "Test response",
            "done": True
        }
        
        client = OllamaClient()
        response = client.generate(
            prompt="Test",
            model="mistral",
            temperature=0.5,
            max_tokens=100,
            system="You are helpful."
        )
        
        # Verify the call was made with correct parameters
        call_args = mock_post.call_args
        assert call_args[1]["json"]["model"] == "mistral"
        assert call_args[1]["json"]["system"] == "You are helpful."
        assert call_args[1]["json"]["options"]["temperature"] == 0.5
    
    @patch('requests.post')
    def test_chat_success(self, mock_post):
        """Test chat completion."""
        mock_post.return_value.status_code = 200
        mock_post.return_value.json.return_value = {
            "model": "llama3.2",
            "message": {"content": "CEREBRUM is a framework..."},
            "done": True
        }
        
        client = OllamaClient()
        messages = [
            ChatMessage(role="user", content="What is CEREBRUM?")
        ]
        response = client.chat(messages)
        
        assert response.text == "CEREBRUM is a framework..."
    
    @patch('requests.post')
    def test_embeddings(self, mock_post):
        """Test embedding generation."""
        mock_post.return_value.status_code = 200
        mock_post.return_value.json.return_value = {
            "embedding": [0.1, 0.2, 0.3, 0.4, 0.5]
        }
        
        client = OllamaClient()
        embeddings = client.embeddings("Test text")
        
        assert len(embeddings) == 1
        assert len(embeddings[0]) == 5


class TestOllamaResponse:
    """Test OllamaResponse dataclass."""
    
    def test_tokens_per_second_calculation(self):
        """Test tokens per second calculation."""
        response = OllamaResponse(
            text="Test",
            model="llama3.2",
            done=True,
            eval_count=100,
            eval_duration=2000000000  # 2 seconds in nanoseconds
        )
        
        assert response.tokens_per_second == 50.0
    
    def test_tokens_per_second_none(self):
        """Test tokens per second returns None when data missing."""
        response = OllamaResponse(
            text="Test",
            model="llama3.2",
            done=True
        )
        
        assert response.tokens_per_second is None


class TestOllamaUtils:
    """Test utility functions."""
    
    @patch('src.llm.ollama.utils.requests.get')
    def test_check_ollama_running_true(self, mock_get):
        """Test check_ollama_running returns True."""
        mock_get.return_value.status_code = 200
        assert check_ollama_running() is True
    
    @patch('src.llm.ollama.utils.requests.get')
    def test_check_ollama_running_false(self, mock_get):
        """Test check_ollama_running returns False on error."""
        import requests.exceptions
        mock_get.side_effect = requests.exceptions.ConnectionError("Connection refused")
        assert check_ollama_running() is False
    
    @patch('src.llm.ollama.utils.requests.get')
    def test_list_models_success(self, mock_get):
        """Test list_models returns model list."""
        mock_get.return_value.status_code = 200
        mock_get.return_value.json.return_value = {
            "models": [{"name": "llama3.2"}]
        }
        
        models = list_models()
        assert len(models) == 1
    
    @patch('src.llm.ollama.utils.requests.get')
    def test_list_models_error(self, mock_get):
        """Test list_models returns empty list on error."""
        import requests.exceptions
        mock_get.side_effect = requests.exceptions.ConnectionError("Error")
        models = list_models()
        assert models == []
    
    def test_format_model_size_gb(self):
        """Test formatting size in GB."""
        assert format_model_size(4700000000) == "4.7 GB"
    
    def test_format_model_size_mb(self):
        """Test formatting size in MB."""
        assert format_model_size(50000000) == "50.0 MB"
    
    def test_format_model_size_kb(self):
        """Test formatting size in KB."""
        assert format_model_size(5000) == "5.0 KB"
    
    @patch('src.llm.ollama.utils.list_models')
    def test_get_default_model_available(self, mock_list):
        """Test get_default_model with available models."""
        mock_list.return_value = [
            {"name": "mistral"},
            {"name": "llama3.2"}
        ]
        
        model = get_default_model()
        assert model == "llama3.2"  # Preferred model
    
    @patch('src.llm.ollama.utils.list_models')
    def test_get_default_model_fallback(self, mock_list):
        """Test get_default_model falls back to first available."""
        mock_list.return_value = [
            {"name": "custom-model"}
        ]
        
        model = get_default_model()
        assert model == "custom-model"
    
    @patch('src.llm.ollama.utils.list_models')
    def test_get_default_model_none_available(self, mock_list):
        """Test get_default_model suggests default when none available."""
        mock_list.return_value = []
        
        model = get_default_model()
        assert model == "llama3.2"
