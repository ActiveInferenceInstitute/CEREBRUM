"""
Tests for src/llm/llm_utils.py

Tests all 12 LLMUtils methods with real logic — no mocks.
"""

import pytest

from src.llm.llm_utils import LLMUtils


@pytest.fixture
def utils():
    return LLMUtils()


class TestCountTokens:
    def test_basic_count(self):
        assert LLMUtils.count_tokens("hello world") > 0

    def test_empty_string(self):
        assert LLMUtils.count_tokens("") == 0

    def test_longer_text(self):
        text = "This is a somewhat longer text that should produce more tokens."
        assert LLMUtils.count_tokens(text) > 5


class TestCountWords:
    def test_basic(self):
        assert LLMUtils.count_words("hello world") == 2

    def test_empty(self):
        assert LLMUtils.count_words("") == 0


class TestCountLines:
    def test_single_line(self):
        assert LLMUtils.count_lines("hello") == 1

    def test_multi_line(self):
        assert LLMUtils.count_lines("line1\nline2\nline3") == 3


class TestGetTextStats:
    def test_returns_all_keys(self):
        stats = LLMUtils.get_text_stats("Hello world. How are you?")
        assert "characters" in stats
        assert "words" in stats
        assert "lines" in stats
        assert "paragraphs" in stats
        assert "sentences" in stats
        assert "estimated_tokens" in stats

    def test_correct_word_count(self):
        stats = LLMUtils.get_text_stats("one two three")
        assert stats["words"] == 3

    def test_sentence_count(self):
        stats = LLMUtils.get_text_stats("Hello. World! How?")
        assert stats["sentences"] == 3


class TestValidateApiResponse:
    def test_error_response(self, utils):
        result = utils.validate_api_response({"error": "rate limited"})
        assert result["status"] == "error"
        assert "rate limited" in result["error"]

    def test_no_choices(self, utils):
        result = utils.validate_api_response({"choices": []})
        assert result["status"] == "error"

    def test_no_message(self, utils):
        result = utils.validate_api_response({"choices": [{}]})
        assert result["status"] == "error"

    def test_valid_response(self, utils):
        response = {
            "choices": [{"message": {"content": "Hello there"}}],
            "model": "test-model",
            "usage": {"tokens": 10}
        }
        result = utils.validate_api_response(response)
        assert result["status"] == "success"
        assert result["content"] == "Hello there"
        assert result["model"] == "test-model"

    def test_handles_unexpected_structure(self, utils):
        result = utils.validate_api_response(None)
        assert result["status"] == "error"


class TestExtractJsonFromText:
    def test_json_code_block(self):
        text = '```json\n{"key": "value"}\n```'
        result = LLMUtils.extract_json_from_text(text)
        assert result == {"key": "value"}

    def test_plain_json(self):
        text = '{"name": "test", "count": 42}'
        result = LLMUtils.extract_json_from_text(text)
        assert result["name"] == "test"

    def test_no_json(self):
        result = LLMUtils.extract_json_from_text("no json here")
        assert result is None

    def test_json_array(self):
        text = '[1, 2, 3]'
        result = LLMUtils.extract_json_from_text(text)
        assert result == [1, 2, 3]


class TestFormatCitations:
    def test_empty_citations(self, utils):
        result = utils.format_citations([])
        assert "No citations" in result

    def test_string_citations(self, utils):
        result = utils.format_citations(["Source A", "Source B"])
        assert "Source A" in result
        assert "Source B" in result

    def test_dict_citations(self, utils):
        citations = [{"title": "Paper 1", "url": "http://example.com", "text": "Good paper"}]
        result = utils.format_citations(citations)
        assert "Paper 1" in result
        assert "http://example.com" in result


class TestChunkText:
    def test_short_text_single_chunk(self, utils):
        chunks = utils.chunk_text("short text", max_chunk_size=100)
        assert len(chunks) == 1

    def test_long_text_multiple_chunks(self, utils):
        text = "This is a sentence. " * 100
        chunks = utils.chunk_text(text, max_chunk_size=100, overlap=20)
        assert len(chunks) > 1

    def test_chunk_size_respected(self, utils):
        text = "Word " * 500
        chunks = utils.chunk_text(text, max_chunk_size=200, overlap=0)
        for chunk in chunks:
            assert len(chunk) <= 210  # Allow small overshoot for word boundary


class TestGeneratePromptTemplate:
    def test_research_template(self, utils):
        prompt = utils.generate_prompt_template("research")
        assert "researcher" in prompt.lower()

    def test_analysis_template(self, utils):
        prompt = utils.generate_prompt_template("analysis")
        assert "analyst" in prompt.lower()

    def test_with_context(self, utils):
        prompt = utils.generate_prompt_template("research", context="quantum computing")
        assert "quantum computing" in prompt

    def test_with_constraints(self, utils):
        prompt = utils.generate_prompt_template("analysis", constraints=["Be concise", "Use data"])
        assert "Be concise" in prompt

    def test_with_examples(self, utils):
        prompt = utils.generate_prompt_template("writing", examples=["Example 1"])
        assert "Example 1" in prompt

    def test_unknown_type(self, utils):
        prompt = utils.generate_prompt_template("unknown_type")
        assert "helpful" in prompt.lower()


class TestCreateSystemMessage:
    def test_basic_message(self, utils):
        msg = utils.create_system_message("researcher", "Find relevant papers")
        assert msg["role"] == "system"
        assert "researcher" in msg["content"]
        assert "Find relevant papers" in msg["content"]

    def test_with_personality(self, utils):
        msg = utils.create_system_message("analyst", "Analyze data", personality="Be thorough")
        assert "Be thorough" in msg["content"]


class TestLogLlmInteraction:
    def test_logs_interaction(self, utils):
        # Should not raise
        utils.log_llm_interaction(
            model="test-model",
            prompt="Hello",
            response="World",
            metadata={"test": True}
        )

    def test_logs_without_metadata(self, utils):
        utils.log_llm_interaction(
            model="test-model",
            prompt="Hello",
            response="World"
        )


class TestRateLimitHandler:
    def test_success_first_try(self, utils):
        def good_func():
            return "success"
        result = utils.rate_limit_handler(good_func)
        assert result == "success"

    def test_non_request_error_raises(self, utils):
        def bad_func():
            raise ValueError("not a request error")
        with pytest.raises(ValueError):
            utils.rate_limit_handler(bad_func)
