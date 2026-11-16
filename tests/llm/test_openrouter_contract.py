"""Real-SDK contract tests against bounded loopback HTTP/SSE responses only."""

import asyncio
import time
from dataclasses import replace
from email.utils import formatdate

import pytest

try:
    import openai
    from src.llm.OpenRouter.openrouter import OpenRouterConfig
except ImportError as exc:
    pytest.skip(f"llm extra not installed: {exc}", allow_module_level=True)

from tests.openrouter_http import (
    OpenRouterHTTP,
    async_openrouter_client,
    completion_reply,
    error_reply,
    openrouter_client,
    stream_reply,
)


MESSAGES = [{"role": "user", "content": "loopback request"}]


def test_missing_key_is_rejected(monkeypatch):
    monkeypatch.delenv("OPENROUTER_API_KEY", raising=False)
    with pytest.raises(ValueError, match="OPENROUTER_API_KEY required"):
        OpenRouterConfig()


def test_repeated_404_fallbacks_are_successful_logical_requests():
    replies = [reply for _ in range(4) for reply in (error_reply(404), completion_reply())]
    with OpenRouterHTTP(replies) as server, openrouter_client(
        server, fallback_models=["loopback/available"], failure_threshold=2,
    ) as client:
        for _ in range(4):
            assert client.simple_chat("request") == "ok"
        assert [request["model"] for request in server.requests] == [
            model for _ in range(4) for model in ("loopback/primary", "loopback/available")
        ]
        stats = client.get_stats()
        assert stats["total_requests"] == stats["successful_requests"] == 4
        assert stats["failed_requests"] == stats["retried_requests"] == 0
        assert stats["circuit_breaker_failures"] == 0
        assert stats["circuit_breaker_state"] == "CLOSED"


def test_exhausted_404_chain_does_not_open_transport_circuit():
    with OpenRouterHTTP([error_reply(404), error_reply(404)]) as server, openrouter_client(
        server, fallback_models=["loopback/also-missing"], failure_threshold=1,
    ) as client:
        with pytest.raises(openai.NotFoundError):
            client.simple_chat("request")
        assert len(server.requests) == 2
        stats = client.get_stats()
        assert stats["total_requests"] == stats["failed_requests"] == 1
        assert stats["successful_requests"] == stats["circuit_breaker_failures"] == 0
        assert stats["circuit_breaker_state"] == "CLOSED"


@pytest.mark.parametrize("asynchronous", [False, True])
def test_wrapper_owns_retry_budget(asynchronous):
    with OpenRouterHTTP([error_reply(503)] * 3) as server:
        def check(client):
            assert len(server.requests) == 3
            stats = client.get_stats()
            assert stats["total_requests"] == stats["failed_requests"] == 1
            assert stats["retried_requests"] == 2
            assert stats["successful_requests"] == 0
            assert stats["circuit_breaker_failures"] == 1

        async def run_async():
            async with async_openrouter_client(server, max_retries=2) as client:
                with pytest.raises(openai.APIStatusError):
                    await client.async_chat_completion(MESSAGES)
                check(client)

        if asynchronous:
            asyncio.run(run_async())
        else:
            with openrouter_client(server, max_retries=2) as client:
                with pytest.raises(openai.APIStatusError):
                    client.chat_completion(MESSAGES)
                check(client)


@pytest.mark.parametrize("asynchronous", [False, True])
@pytest.mark.parametrize("streaming", [False, True])
@pytest.mark.parametrize("status,headers,retry,minimum_delay", [
    (408, {}, True, 0),
    (409, {}, True, 0),
    (429, {"Retry-After": "0.03"}, True, 0.025),
    (503, {"retry-after-ms": "30", "Retry-After": "10"}, True, 0.025),
    (400, {"x-should-retry": "true"}, True, 0),
    (503, {"x-should-retry": "false"}, False, 0),
    (429, {"Retry-After": "121"}, False, 0),
    (429, {"retry-after-ms": "1e999"}, False, 0),
])
def test_wrapper_preserves_sdk_status_and_cooldown_contract(
    asynchronous, streaming, status, headers, retry, minimum_delay,
):
    failure = replace(error_reply(status), headers=headers)
    success = stream_reply("ok") if streaming else completion_reply()
    with OpenRouterHTTP([failure, success]) as server:
        def check(client):
            assert len(server.requests) == (2 if retry else 1)
            stats = client.get_stats()
            assert stats["total_requests"] == 1
            assert stats["retried_requests"] == int(retry)
            assert stats["successful_requests"] == int(retry)
            assert stats["failed_requests"] == int(not retry)
            if retry:
                assert server.request_times[1] - server.request_times[0] >= minimum_delay

        async def run_async():
            async with async_openrouter_client(server, max_retries=1) as client:
                async def consume():
                    if streaming:
                        chunks = [chunk async for chunk in client.async_chat_completion_stream(MESSAGES)]
                        assert chunks[0].choices[0].delta.content == "ok"
                    else:
                        assert (await client.async_chat_completion(MESSAGES)).choices[0].message.content == "ok"
                if retry:
                    await consume()
                else:
                    with pytest.raises(openai.APIStatusError):
                        await consume()
                check(client)

        if asynchronous:
            asyncio.run(run_async())
        else:
            with openrouter_client(server, max_retries=1) as client:
                def consume():
                    if streaming:
                        assert list(client.chat_completion_stream(MESSAGES))[0].choices[0].delta.content == "ok"
                    else:
                        assert client.chat_completion(MESSAGES).choices[0].message.content == "ok"
                if retry:
                    consume()
                else:
                    with pytest.raises(openai.APIStatusError):
                        consume()
                check(client)


@pytest.mark.parametrize("header,expected", [
    ("30", 30),
    ("90", 90),
    ("bad", 0),
    ("-1", 0),
    ("inf", 0),
])
def test_retry_delay_uses_public_sdk_response_headers(header, expected):
    failure = replace(error_reply(429), headers={"Retry-After": header})
    with OpenRouterHTTP([failure]) as server, openrouter_client(server) as client:
        with pytest.raises(openai.RateLimitError) as captured:
            client.chat_completion(MESSAGES)
        assert client._calculate_delay(0, captured.value) == expected


def test_retry_delay_accepts_http_date_from_real_sdk_response():
    deadline = int(time.time()) + 30
    failure = replace(error_reply(429), headers={"Retry-After": formatdate(deadline, usegmt=True)})
    with OpenRouterHTTP([failure]) as server, openrouter_client(server) as client:
        with pytest.raises(openai.RateLimitError) as captured:
            client.chat_completion(MESSAGES)
        before = time.time()
        delay = client._calculate_delay(0, captured.value)
        after = time.time()
        assert deadline - after <= delay <= deadline - before


@pytest.mark.parametrize("asynchronous", [False, True])
def test_healthy_response_clears_prior_closed_state_failures(asynchronous):
    with OpenRouterHTTP([error_reply(503), completion_reply()]) as server:
        def check(client):
            stats = client.get_stats()
            assert stats["total_requests"] == 2
            assert stats["failed_requests"] == stats["successful_requests"] == 1
            assert stats["circuit_breaker_state"] == "CLOSED"
            assert stats["circuit_breaker_failures"] == 0
            assert client.circuit_breaker.last_failure_time is None

        async def run_async():
            async with async_openrouter_client(server) as client:
                with pytest.raises(openai.APIStatusError):
                    await client.async_chat_completion(MESSAGES)
                assert (await client.async_chat_completion(MESSAGES)).choices[0].message.content == "ok"
                check(client)

        if asynchronous:
            asyncio.run(run_async())
        else:
            with openrouter_client(server) as client:
                with pytest.raises(openai.APIStatusError):
                    client.chat_completion(MESSAGES)
                assert client.simple_chat("request") == "ok"
                check(client)


@pytest.mark.parametrize("initial_failure", [
    error_reply(503),
    stream_reply(done=False, truncate=True),
])
def test_sync_stream_retries_only_before_delivery(initial_failure):
    with OpenRouterHTTP([initial_failure, stream_reply("delivered")]) as server, openrouter_client(
        server, max_retries=1,
    ) as client:
        stream = client.chat_completion_stream(MESSAGES)
        assert server.requests == []
        assert client.get_stats()["total_requests"] == 0
        assert next(stream).choices[0].delta.content == "delivered"
        assert client.get_stats()["successful_requests"] == 0
        assert list(stream) == []
        stats = client.get_stats()
        assert len(server.requests) == 2
        assert stats["total_requests"] == stats["successful_requests"] == 1
        assert stats["retried_requests"] == 1
        assert stats["failed_requests"] == stats["circuit_breaker_failures"] == 0


@pytest.mark.parametrize("initial_failure", [
    error_reply(503),
    stream_reply(done=False, truncate=True),
])
def test_async_stream_retries_only_before_delivery(initial_failure):
    with OpenRouterHTTP([initial_failure, stream_reply("delivered")]) as server:
        async def run():
            async with async_openrouter_client(server, max_retries=1) as client:
                stream = client.async_chat_completion_stream(MESSAGES)
                assert server.requests == []
                assert client.get_stats()["total_requests"] == 0
                assert (await anext(stream)).choices[0].delta.content == "delivered"
                assert client.get_stats()["successful_requests"] == 0
                assert [chunk async for chunk in stream] == []
                stats = client.get_stats()
                assert len(server.requests) == 2
                assert stats["total_requests"] == stats["successful_requests"] == 1
                assert stats["retried_requests"] == 1
                assert stats["failed_requests"] == stats["circuit_breaker_failures"] == 0
        asyncio.run(run())


def test_sync_stream_never_replays_delivered_content():
    with OpenRouterHTTP([stream_reply("once", done=False, truncate=True)]) as server, openrouter_client(
        server, max_retries=2,
    ) as client:
        stream = client.chat_completion_stream(MESSAGES)
        assert next(stream).choices[0].delta.content == "once"
        with pytest.raises(openai.APIConnectionError):
            next(stream)
        assert len(server.requests) == 1
        stats = client.get_stats()
        assert stats["failed_requests"] == stats["circuit_breaker_failures"] == 1
        assert stats["successful_requests"] == stats["retried_requests"] == 0


def test_async_stream_never_replays_delivered_content():
    with OpenRouterHTTP([stream_reply("once", done=False, truncate=True)]) as server:
        async def run():
            async with async_openrouter_client(server, max_retries=2) as client:
                stream = client.async_chat_completion_stream(MESSAGES)
                assert (await anext(stream)).choices[0].delta.content == "once"
                with pytest.raises(openai.APIConnectionError):
                    await anext(stream)
                assert len(server.requests) == 1
                stats = client.get_stats()
                assert stats["failed_requests"] == stats["circuit_breaker_failures"] == 1
                assert stats["successful_requests"] == stats["retried_requests"] == 0
        asyncio.run(run())


@pytest.mark.parametrize("asynchronous", [False, True])
def test_open_circuit_rejects_stream_without_http(asynchronous):
    with OpenRouterHTTP([error_reply(503)]) as server:
        def check(client):
            assert len(server.requests) == 1
            stats = client.get_stats()
            assert stats["circuit_breaker_state"] == "OPEN"
            assert stats["total_requests"] == stats["failed_requests"] == 2
            assert stats["circuit_breaker_trips"] == 1
            assert stats["successful_requests"] == 0

        async def run_async():
            async with async_openrouter_client(server, failure_threshold=1) as client:
                with pytest.raises(openai.APIStatusError):
                    await client.async_chat_completion(MESSAGES)
                stream = client.async_chat_completion_stream(MESSAGES)
                with pytest.raises(RuntimeError, match="Circuit breaker is OPEN"):
                    await anext(stream)
                check(client)

        if asynchronous:
            asyncio.run(run_async())
        else:
            with openrouter_client(server, failure_threshold=1) as client:
                with pytest.raises(openai.APIStatusError):
                    client.chat_completion(MESSAGES)
                with pytest.raises(RuntimeError, match="Circuit breaker is OPEN"):
                    next(client.chat_completion_stream(MESSAGES))
                check(client)


@pytest.mark.parametrize("asynchronous", [False, True])
@pytest.mark.parametrize("streaming", [False, True])
def test_explicit_zero_temperature_reaches_sdk_payload(asynchronous, streaming):
    reply = stream_reply("zero") if streaming else completion_reply("zero")
    with OpenRouterHTTP([reply]) as server:
        async def run_async():
            async with async_openrouter_client(server, temperature=0.9) as client:
                if streaming:
                    chunks = [chunk async for chunk in client.async_chat_completion_stream(
                        MESSAGES, temperature=0,
                    )]
                    assert "".join(chunk.choices[0].delta.content or "" for chunk in chunks) == "zero"
                else:
                    result = await client.async_chat_completion(MESSAGES, temperature=0)
                    assert result.choices[0].message.content == "zero"

        if asynchronous:
            asyncio.run(run_async())
        else:
            with openrouter_client(server, temperature=0.9) as client:
                if streaming:
                    chunks = list(client.chat_completion_stream(MESSAGES, temperature=0))
                    assert "".join(chunk.choices[0].delta.content or "" for chunk in chunks) == "zero"
                else:
                    result = client.chat_completion(MESSAGES, temperature=0)
                    assert result.choices[0].message.content == "zero"
        assert len(server.requests) == 1
        assert server.paths == ["/v1/chat/completions"]
        assert server.requests[0]["temperature"] == 0


def test_closing_sync_stream_releases_connection_without_success():
    with OpenRouterHTTP([stream_reply("first", done=False, hold_open=True)]) as server, openrouter_client(
        server,
    ) as client:
        stream = client.chat_completion_stream(MESSAGES)
        assert next(stream).choices[0].delta.content == "first"
        stream.close()
        assert server.disconnected.wait(2)
        stats = client.get_stats()
        assert stats["successful_requests"] == stats["failed_requests"] == 0
        assert stats["retried_requests"] == 0


@pytest.mark.parametrize("cancel", [False, True])
def test_closing_async_stream_releases_connection_without_success(cancel):
    with OpenRouterHTTP([stream_reply("first", done=False, hold_open=True)]) as server:
        async def run():
            async with async_openrouter_client(server) as client:
                stream = client.async_chat_completion_stream(MESSAGES)
                assert (await anext(stream)).choices[0].delta.content == "first"
                if cancel:
                    pending = asyncio.create_task(anext(stream))
                    await asyncio.sleep(0)
                    pending.cancel()
                    with pytest.raises(asyncio.CancelledError):
                        await pending
                else:
                    await stream.aclose()
                assert await asyncio.to_thread(server.disconnected.wait, 2)
                stats = client.get_stats()
                assert stats["successful_requests"] == stats["failed_requests"] == 0
                assert stats["retried_requests"] == 0
        asyncio.run(run())
