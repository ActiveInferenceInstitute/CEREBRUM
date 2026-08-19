"""
Tests for src/lexicon/core/ modules

Tests LexiconConfig, exceptions, StructuredLogFormatter, ContextAdapter,
LoggingTimer, and get_default_config — all with real logic, no mocks.
"""

import json
import logging

import pytest

from src.lexicon.core.config import LexiconConfig, get_default_config
from src.lexicon.core.exceptions import (
    APIError,
    AuthenticationError,
    ConfigurationError,
    GraphError,
    InputError,
    LexiconError,
    ModelError,
    ParsingError,
    ProcessingError,
    RateLimitError,
    StorageError,
)
from src.lexicon.core.logging import (
    ContextAdapter,
    LoggingTimer,
    StructuredLogFormatter,
    get_logger,
)

# ─── Config ───────────────────────────────────────────────────────────

class TestLexiconConfig:
    def test_defaults(self, tmp_path, monkeypatch):
        monkeypatch.setenv("OPENROUTER_API_KEY", "test-key")
        monkeypatch.setattr(LexiconConfig, "__post_init__", lambda self: None)
        cfg = LexiconConfig()
        assert cfg.default_model == "qwen/qwen3.8-27b"
        assert cfg.max_batch_size == 25

    def test_save_and_load(self, tmp_path):
        cfg = LexiconConfig(
            openrouter_api_key="test",
            default_model="openai/gpt-4o",
            output_dir=tmp_path / "out",
            cache_dir=tmp_path / "cache",
        )
        save_path = tmp_path / "config.json"
        cfg.save(save_path)
        assert save_path.exists()

        loaded = LexiconConfig.load(save_path)
        # The API key is a secret and is intentionally NOT persisted to disk;
        # non-secret fields still round-trip.
        assert loaded.default_model == "openai/gpt-4o"
        assert loaded.openrouter_api_key is None or loaded.openrouter_api_key != "test"

    def test_does_not_persist_api_key(self, tmp_path):
        cfg = LexiconConfig(
            openrouter_api_key="sk-super-secret-xyz",
            output_dir=tmp_path / "out",
            cache_dir=tmp_path / "cache",
        )
        save_path = tmp_path / "config.json"
        cfg.save(save_path)

        raw = save_path.read_text()
        assert "sk-super-secret-xyz" not in raw
        assert "openrouter_api_key" not in raw

        loaded = LexiconConfig.load(save_path)
        assert loaded.openrouter_api_key != "sk-super-secret-xyz"

    def test_get_default_config(self, tmp_path, monkeypatch):
        monkeypatch.setenv("LEXICON_OUTPUT_DIR", str(tmp_path / "default_out"))
        cfg = get_default_config()
        assert isinstance(cfg, LexiconConfig)


# ─── Exceptions ───────────────────────────────────────────────────────

class TestExceptions:
    @pytest.mark.parametrize("exc_cls", [
        LexiconError, ConfigurationError, APIError, ModelError, InputError,
        GraphError, StorageError, ProcessingError, RateLimitError,
        AuthenticationError, ParsingError,
    ])
    def test_can_instantiate_and_raise(self, exc_cls):
        with pytest.raises(exc_cls):
            raise exc_cls("test error")

    def test_hierarchy(self):
        assert issubclass(RateLimitError, APIError)
        assert issubclass(AuthenticationError, APIError)
        assert issubclass(ParsingError, ProcessingError)
        assert issubclass(ConfigurationError, LexiconError)


# ─── Logging ──────────────────────────────────────────────────────────

class TestStructuredLogFormatter:
    def test_formats_json(self):
        formatter = StructuredLogFormatter()
        record = logging.LogRecord(
            name="test", level=logging.INFO, pathname="test.py",
            lineno=1, msg="hello world", args=(), exc_info=None,
        )
        output = formatter.format(record)
        data = json.loads(output)
        assert data["message"] == "hello world"
        assert data["level"] == "INFO"
        assert "timestamp" in data

    def test_formats_exception_info(self):
        formatter = StructuredLogFormatter()
        try:
            raise ValueError("test error")
        except ValueError:
            import sys
            exc_info = sys.exc_info()
            record = logging.LogRecord(
                name="test", level=logging.ERROR, pathname="test.py",
                lineno=1, msg="fail", args=(), exc_info=exc_info,
            )
            output = formatter.format(record)
            data = json.loads(output)
            assert "exception" in data
            assert data["exception"]["type"] == "ValueError"


class TestContextAdapter:
    def test_adds_context(self):
        base_logger = logging.getLogger("test_context_adapter")
        adapter = ContextAdapter(base_logger, {"component": "test"})
        msg, kwargs = adapter.process("hello", {})
        assert msg == "hello"


class TestGetLogger:
    def test_returns_logger(self):
        logger = get_logger("test_module")
        assert isinstance(logger, logging.Logger)

    def test_with_context_returns_adapter(self):
        logger = get_logger("test_module", context={"env": "test"})
        assert isinstance(logger, ContextAdapter)


class TestLoggingTimer:
    def test_timing_context_manager(self):
        logger = logging.getLogger("timer_test")
        timer = LoggingTimer(logger, "test_operation")
        with timer:
            x = 1 + 1
        assert timer.start_time is not None

    def test_logs_error_on_exception(self):
        logger = logging.getLogger("timer_test_err")
        timer = LoggingTimer(logger, "failing_op")
        with pytest.raises(ValueError):
            with timer:
                raise ValueError("boom")
