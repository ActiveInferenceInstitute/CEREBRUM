"""Paraphrase cache/protocol regressions using the actual SDK on loopback."""

import hashlib
import json
from dataclasses import replace

import pytest

try:
    import openai
    from src.lexicon.core.config import LexiconConfig
    from src.lexicon.declension.tagger import CasedSegment
    from src.lexicon.paraphrase.cache import save_to_cache
    from src.lexicon.paraphrase.generator import ParaphraseGenerator
except ImportError as exc:
    pytest.skip(f"lexicon/llm extras not installed: {exc}", allow_module_level=True)

from tests.openrouter_http import OpenRouterHTTP, completion_reply, error_reply, openrouter_client


@pytest.fixture
def segment():
    return CasedSegment(
        segment_id="segment-1",
        text="Alice sent the report to Bob.",
        nominative=[{"text": "Alice", "confidence": 0.9}],
    )


def generator_for(tmp_path, client):
    return ParaphraseGenerator(client, LexiconConfig(
        output_dir=tmp_path / "out",
        cache_dir=tmp_path / "cache",
        default_model="loopback/primary",
        fallback_models={},
    ))


def generated_reply():
    return completion_reply('[{"text": "Bob received Alice\'s report.", "quality": 0.9}]')


def assessed_reply(quality=0.8):
    return completion_reply(json.dumps({"assessments": [{"id": 1, "quality": quality}]}))


@pytest.mark.parametrize("response", ["[]", "```json\n[]\n```", '{"paraphrases": []}'])
def test_legitimate_empty_generation_is_cached(tmp_path, segment, response):
    with OpenRouterHTTP([completion_reply(response)]) as server, openrouter_client(server) as client:
        generator = generator_for(tmp_path, client)
        assert generator.generate_for_segment(segment).paraphrases == []
        assert generator.generate_for_segment(segment).paraphrases == []
        assert len(server.requests) == 1
        assert generator.get_metrics()["cache_hits"] == 1
        assert generator.get_metrics()["cache_misses"] == 1


@pytest.mark.parametrize("response", [
    "",
    "Here are some ideas, not JSON.",
    "```json\n[broken]\n```",
    "{}",
    '[{"quality": 0.8}]',
    '[{"text": "   "}]',
    '[{"text": "candidate", "quality": true}]',
    '[{"text": "candidate", "quality": NaN}]',
])
def test_invalid_generation_is_not_cached_as_success(tmp_path, segment, response):
    with OpenRouterHTTP([completion_reply(response), completion_reply("[]")]) as server, openrouter_client(
        server,
    ) as client:
        generator = generator_for(tmp_path, client)
        with pytest.raises(ValueError):
            generator.generate_for_segment(segment)
        assert list(generator.cache_dir.glob("*.json")) == []
        assert generator.generate_for_segment(segment).paraphrases == []
        assert generator.generate_for_segment(segment).paraphrases == []
        assert len(server.requests) == 2
        assert generator.get_metrics()["cache_hits"] == 1


def test_provider_generation_failure_can_recover_without_cache_poisoning(tmp_path, segment):
    with OpenRouterHTTP([error_reply(503), completion_reply("[]")]) as server, openrouter_client(server) as client:
        generator = generator_for(tmp_path, client)
        with pytest.raises(openai.APIStatusError):
            generator.generate_for_segment(segment)
        assert list(generator.cache_dir.glob("*.json")) == []
        assert generator.generate_for_segment(segment).paraphrases == []
        assert len(server.requests) == 2
        assert generator.get_metrics()["cache_hits"] == 0


@pytest.mark.parametrize("quality_reply", [
    error_reply(503),
    completion_reply("unparseable assessment"),
    completion_reply('{"assessments": []}'),
    completion_reply('{"assessments": [{"id": 2, "quality": 0.8}]}'),
    completion_reply('{"assessments": [{"id": 1, "quality": 0.8}, {"id": 1, "quality": 0.9}]}'),
    completion_reply('{"assessments": [{"id": 1, "quality": "0.8"}]}'),
    completion_reply('{"assessments": [{"id": 1, "quality": 1.1}]}'),
])
def test_quality_failure_does_not_cache_unassessed_paraphrases(tmp_path, segment, quality_reply):
    replies = [generated_reply(), quality_reply, generated_reply(), assessed_reply()]
    with OpenRouterHTTP(replies) as server, openrouter_client(server) as client:
        generator = generator_for(tmp_path, client)
        with pytest.raises((ValueError, openai.APIStatusError)):
            generator.generate_for_segment(segment)
        assert list(generator.cache_dir.glob("*.json")) == []
        recovered = generator.generate_for_segment(segment)
        assert recovered.paraphrases == [{"text": "Bob received Alice's report.", "quality": 0.8}]
        assert generator.generate_for_segment(segment).paraphrases == recovered.paraphrases
        assert len(server.requests) == 4
        assert generator.get_metrics()["cache_hits"] == 1


def test_successful_rejection_of_all_paraphrases_is_cached(tmp_path, segment):
    with OpenRouterHTTP([generated_reply(), assessed_reply(0.2)]) as server, openrouter_client(server) as client:
        generator = generator_for(tmp_path, client)
        assert generator.generate_for_segment(segment).paraphrases == []
        assert generator.generate_for_segment(segment).paraphrases == []
        assert len(server.requests) == 2
        assert generator.get_metrics()["llm_calls"] == 2
        assert generator.get_metrics()["cache_hits"] == 1


def test_quality_assessments_use_ids_not_response_order(tmp_path, segment):
    generation = completion_reply('[{"text": "first"}, {"text": "second"}]')
    quality = completion_reply('```json\n{"assessments": [{"id": 2, "quality": 0.9}, {"id": 1, "quality": 0.2}]}\n```')
    with OpenRouterHTTP([generation, quality]) as server, openrouter_client(server) as client:
        generator = generator_for(tmp_path, client)
        assert generator.generate_for_segment(segment).paraphrases == [{"text": "second", "quality": 0.9}]


@pytest.mark.parametrize("changed_context", ["model", "task_model", "focus", "temperature", "max_tokens", "fallbacks"])
def test_request_context_changes_miss_existing_cache(tmp_path, segment, changed_context):
    with OpenRouterHTTP([completion_reply("[]"), completion_reply("[]")]) as server, openrouter_client(server) as client:
        generator = generator_for(tmp_path, client)
        generator.generate_for_segment(segment)
        changed_segment = segment
        if changed_context == "model":
            generator.config.default_model = "loopback/different"
        elif changed_context == "task_model":
            generator.config.fallback_models["paraphrase"] = "loopback/task-specific"
        elif changed_context == "focus":
            changed_segment = replace(segment, nominative=[], accusative=[{"text": "report", "confidence": 0.9}])
        elif changed_context == "temperature":
            client.config.temperature = 0
        elif changed_context == "max_tokens":
            client.config.max_tokens = 123
        else:
            client.config.fallback_models = ["loopback/fallback"]
        assert generator.generate_for_segment(changed_segment).paraphrases == []
        assert generator.generate_for_segment(changed_segment).paraphrases == []
        assert len(server.requests) == 2
        assert generator.get_metrics()["cache_hits"] == 1
        if changed_context in {"model", "task_model"}:
            assert server.requests[0]["model"] != server.requests[1]["model"]
        elif changed_context == "focus":
            assert server.requests[0]["messages"] != server.requests[1]["messages"]
        elif changed_context in {"temperature", "max_tokens"}:
            assert server.requests[0][changed_context] != server.requests[1][changed_context]


def test_endpoint_changes_do_not_reuse_other_provider_cache(tmp_path, segment):
    with OpenRouterHTTP([completion_reply("[]")]) as first, OpenRouterHTTP([completion_reply("[]")]) as second:
        with openrouter_client(first) as client:
            generator_for(tmp_path, client).generate_for_segment(segment)
        with openrouter_client(second) as client:
            generator = generator_for(tmp_path, client)
            assert generator.generate_for_segment(segment).paraphrases == []
            assert generator.get_metrics()["cache_misses"] == 1
        assert len(first.requests) == len(second.requests) == 1


def test_unbound_legacy_cache_is_not_reused(tmp_path, segment):
    with OpenRouterHTTP([completion_reply("[]")]) as server, openrouter_client(server) as client:
        generator = generator_for(tmp_path, client)
        old_key = hashlib.md5(segment.text.encode("utf-8")).hexdigest()
        old_file = generator.cache_dir / f"{old_key}.json"
        save_to_cache(old_file, [{"text": "wrong request's cached result", "quality": 0.9}])
        before = old_file.read_bytes()
        assert generator.generate_for_segment(segment).paraphrases == []
        assert len(server.requests) == 1
        assert old_file.read_bytes() == before


def test_batch_failure_is_explicit_and_later_attempt_can_recover(tmp_path, segment):
    with OpenRouterHTTP([error_reply(503), completion_reply("[]")]) as server, openrouter_client(server) as client:
        generator = generator_for(tmp_path, client)
        failed = generator.generate([segment])[0]
        assert failed.segment_id == segment.segment_id
        assert "503" in failed.metadata["error"]
        assert list(generator.cache_dir.glob("*.json")) == []
        recovered = generator.generate([segment])[0]
        assert recovered.paraphrases == []
        assert "error" not in recovered.metadata
        assert len(server.requests) == 2
