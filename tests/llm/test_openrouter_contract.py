"""
Offline contract tests for src/llm/OpenRouter/openrouter.py.

These tests construct clients and configs with a placeholder key (``test-key``) and
assert the missing-key rejection path, so they need no API key and make no network
calls. They require the optional ``llm`` extra and skip when it is absent:

    uv pip install -e ".[dev,llm]"
    uv run python -m pytest tests/llm/test_openrouter_contract.py -v

A test that talks to the live service belongs behind ``@pytest.mark.live``; the
default run deselects that marker (see ``pytest.ini``), so opt in with ``-m live``.
"""

import os
import pytest

try:
    from src.llm.OpenRouter.openrouter import OpenRouterClient, OpenRouterConfig
except ImportError as exc:  # optional llm extra (requests, openai, ...) is absent
    pytest.skip(f"llm extra not installed: {exc}", allow_module_level=True)


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
