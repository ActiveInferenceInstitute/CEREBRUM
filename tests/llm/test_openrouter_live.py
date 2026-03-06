"""
Tests for src/llm/OpenRouter/openrouter.py — live API test.

Tests a real OpenRouter API call using the OPENROUTER_API_KEY environment variable.
Marked with @pytest.mark.live so they only run when opted in.

Run with:
    OPENROUTER_API_KEY=sk-or-... python -m pytest tests/llm/test_openrouter_live.py -v -m live
"""

import os
import pytest

from src.llm.OpenRouter.openrouter import OpenRouterClient, OpenRouterConfig


@pytest.fixture
def api_key():
    key = os.environ.get("OPENROUTER_API_KEY")
    if not key:
        pytest.skip("OPENROUTER_API_KEY not set")
    return key


@pytest.fixture
def client(api_key):
    config = OpenRouterConfig(api_key=api_key)
    return OpenRouterClient(config)


@pytest.mark.live
class TestOpenRouterLive:
    def test_simple_completion(self, client):
        """Test a real API call to OpenRouter."""
        response = client.chat_completion(
            messages=[{"role": "user", "content": "Say 'hello' and nothing else."}],
            model="openai/gpt-4o-mini",
        )
        assert response is not None
        assert "choices" in response or "content" in str(response)

    def test_config_has_api_key(self, api_key):
        config = OpenRouterConfig(api_key=api_key)
        assert config.api_key == api_key


class TestOpenRouterConfig:
    """Test OpenRouterConfig without API calls."""

    def test_default_config(self):
        config = OpenRouterConfig(api_key="test-key")
        assert config.api_key == "test-key"

    def test_client_init(self):
        config = OpenRouterConfig(api_key="test-key")
        client = OpenRouterClient(config)
        assert client is not None
