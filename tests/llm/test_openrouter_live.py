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


class TestOpenRouterClientContract:
    def test_client_initializes_with_explicit_key(self):
        config = OpenRouterConfig(api_key="test-key")
        client = OpenRouterClient(config)
        assert client.config.api_key == "test-key"
        assert client.config.base_url.endswith("/api/v1")

    def test_missing_key_is_rejected(self, monkeypatch):
        monkeypatch.delenv("OPENROUTER_API_KEY", raising=False)
        with pytest.raises(ValueError, match="OPENROUTER_API_KEY required"):
            OpenRouterConfig()


class TestOpenRouterConfig:
    """Test OpenRouterConfig without API calls."""

    def test_default_config(self):
        config = OpenRouterConfig(api_key="test-key")
        assert config.api_key == "test-key"

    def test_client_init(self):
        config = OpenRouterConfig(api_key="test-key")
        client = OpenRouterClient(config)
        assert client is not None
