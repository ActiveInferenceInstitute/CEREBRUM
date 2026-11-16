"""Loopback HTTP/SSE fixtures for the real OpenAI SDK, never live providers."""

import asyncio
import json
import os
import socket
import threading
from collections import deque
from contextlib import asynccontextmanager, contextmanager
from dataclasses import dataclass
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer


@dataclass(frozen=True)
class Reply:
    status: int
    body: bytes
    content_type: str = "application/json"
    truncate: bool = False
    hold_open: bool = False


def completion_reply(content="ok"):
    return Reply(200, json.dumps({
        "id": "completion-loopback",
        "object": "chat.completion",
        "created": 0,
        "model": "loopback/model",
        "choices": [{
            "index": 0,
            "message": {"role": "assistant", "content": content},
            "finish_reason": "stop",
        }],
    }).encode("utf-8"))


def error_reply(status):
    return Reply(status, json.dumps({
        "error": {"message": f"loopback HTTP {status}", "type": "api_error"},
    }).encode("utf-8"))


def stream_reply(*parts, done=True, truncate=False, hold_open=False):
    events = []
    for part in parts:
        chunk = {
            "id": "chunk-loopback",
            "object": "chat.completion.chunk",
            "created": 0,
            "model": "loopback/model",
            "choices": [{"index": 0, "delta": {"content": part}, "finish_reason": None}],
        }
        events.append(f"data: {json.dumps(chunk)}\n\n")
    if done:
        events.append("data: [DONE]\n\n")
    return Reply(200, "".join(events).encode("utf-8"), "text/event-stream", truncate, hold_open)


class OpenRouterHTTP:
    """Serve a finite script or a request-dependent protocol response on loopback."""

    def __init__(self, replies=(), *, responder=None):
        self._replies = deque(replies)
        self._responder = responder
        self._lock = threading.Lock()
        self.requests = []
        self.paths = []
        self.errors = []
        self.disconnected = threading.Event()
        self._server = None
        self._thread = None

    def __enter__(self):
        owner = self

        class Handler(BaseHTTPRequestHandler):
            protocol_version = "HTTP/1.1"

            def log_message(self, format, *args):
                pass

            def do_POST(self):
                try:
                    payload = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
                    with owner._lock:
                        owner.requests.append(payload)
                        owner.paths.append(self.path)
                        if owner._responder is None:
                            reply = owner._replies.popleft()
                    if owner._responder is not None:
                        reply = owner._responder(payload)
                except Exception as error:
                    with owner._lock:
                        owner.errors.append(str(error) or type(error).__name__)
                    reply = error_reply(500)

                self.send_response(reply.status)
                self.send_header("Content-Type", reply.content_type)
                self.send_header("Connection", "close")
                if not reply.hold_open:
                    self.send_header("Content-Length", str(len(reply.body) + int(reply.truncate)))
                self.end_headers()
                self.close_connection = True
                try:
                    self.wfile.write(reply.body)
                    self.wfile.flush()
                    if reply.hold_open:
                        self.connection.settimeout(3)
                        if self.connection.recv(1) == b"":
                            owner.disconnected.set()
                        else:
                            owner.errors.append("Unexpected bytes while waiting for stream closure")
                except (BrokenPipeError, ConnectionResetError):
                    owner.disconnected.set()
                except socket.timeout:
                    owner.errors.append("Consumer did not close its stream connection")

        self._server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        self._server.daemon_threads = False
        self._thread = threading.Thread(target=self._server.serve_forever, daemon=True)
        self._thread.start()
        self.base_url = f"http://127.0.0.1:{self._server.server_port}/v1"
        self._proxy_environment = {name: os.environ.get(name) for name in ("NO_PROXY", "no_proxy")}
        for name in self._proxy_environment:
            os.environ[name] = "127.0.0.1,localhost"
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        try:
            self._server.shutdown()
            self._server.server_close()
            self._thread.join(timeout=5)
            if exc_type is None:
                assert not self._thread.is_alive(), "Loopback server did not stop"
                assert not self.errors, self.errors
        finally:
            for name, value in self._proxy_environment.items():
                if value is None:
                    os.environ.pop(name, None)
                else:
                    os.environ[name] = value


def make_client(server, *, max_retries=0, failure_threshold=2, **kwargs):
    from src.llm.OpenRouter.openrouter import (
        CircuitBreakerConfig, OpenRouterClient, OpenRouterConfig, RetryConfig,
    )

    return OpenRouterClient(OpenRouterConfig(
        api_key="loopback-test-key",
        base_url=server.base_url,
        default_model="loopback/primary",
        request_timeout=1,
        enable_logging=False,
        retry_config=RetryConfig(max_retries=max_retries, base_delay=0, jitter=False),
        circuit_breaker_config=CircuitBreakerConfig(
            failure_threshold=failure_threshold, recovery_timeout=3600,
        ),
        **kwargs,
    ))


@contextmanager
def openrouter_client(server, **kwargs):
    client = make_client(server, **kwargs)
    try:
        yield client
    finally:
        client.client.close()
        asyncio.run(client.async_client.close())


@asynccontextmanager
async def async_openrouter_client(server, **kwargs):
    client = make_client(server, **kwargs)
    try:
        yield client
    finally:
        client.client.close()
        await client.async_client.close()
