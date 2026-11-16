"""Batch identity and exit-status regressions through the real CLI and SDK."""

import json
import os
from pathlib import Path
import subprocess
import sys

import pytest

pytest.importorskip("openai", reason="llm extra is required for the real batch CLI")
pytest.importorskip("dotenv", reason="lexicon extra is required for the real batch CLI")

from tests.openrouter_http import OpenRouterHTTP, completion_reply


REPO_ROOT = Path(__file__).resolve().parents[2]


def run_batch(tmp_path, server, mode, roots):
    output_dir = tmp_path / "batch-output"
    env = {
        **os.environ,
        "PYTHONPATH": str(REPO_ROOT),
        "PYTHONDONTWRITEBYTECODE": "1",
        "PYTHON_DOTENV_DISABLED": "1",
        "OPENROUTER_API_KEY": "loopback-test-key",
        "HF_HUB_OFFLINE": "1",
        "HF_DATASETS_OFFLINE": "1",
        "TRANSFORMERS_OFFLINE": "1",
        "HF_HOME": str(tmp_path / "hf-cache"),
        "SENTENCE_TRANSFORMERS_HOME": str(tmp_path / "sentence-cache"),
        "TORCH_HOME": str(tmp_path / "torch-cache"),
        "XDG_CACHE_HOME": str(tmp_path / "xdg-cache"),
        "MPLCONFIGDIR": str(tmp_path / "mpl-cache"),
        "CUDA_VISIBLE_DEVICES": "",
        "NO_PROXY": "127.0.0.1,localhost",
        "no_proxy": "127.0.0.1,localhost",
    }
    command = [
        sys.executable, "-B", "-m", "src.lexicon.run", mode,
        "--output-dir", str(output_dir),
        "--api-base-url", server.base_url,
        "--model", "loopback/batch",
        "--log-level", "ERROR",
    ]
    for root in roots:
        command.extend(["--input-dir", str(root)])
    completed = subprocess.run(
        command, cwd=tmp_path, env=env, text=True, capture_output=True, timeout=90,
    )
    summaries = list(output_dir.glob("batch_summary_*.json"))
    assert len(summaries) == 1, completed.stdout + completed.stderr
    return completed, json.loads(summaries[0].read_text(encoding="utf-8"))


def input_pair(tmp_path, contents):
    files = [tmp_path / "first" / "notes.md", tmp_path / "second" / "notes.md"]
    for path, content in zip(files, contents):
        path.parent.mkdir()
        path.write_bytes(content)
    return files


@pytest.mark.parametrize("mode", ["--batch", "--process-samples"])
@pytest.mark.parametrize("statuses,contents,exit_code", [
    (["success", "success"], [b"", b"\n"], 0),
    (["success", "error"], [b"", b"\xff"], 1),
    (["error", "success"], [b"\xff", b""], 1),
    (["error", "error"], [b"\xff", b"\xfe"], 1),
])
def test_same_basename_outcomes_survive_summary_and_control_exit(
    tmp_path, mode, statuses, contents, exit_code,
):
    files = input_pair(tmp_path, contents)
    with OpenRouterHTTP(responder=lambda request: completion_reply("[]")) as server:
        completed, summary = run_batch(
            tmp_path, server, mode,
            [files[0].parent, files[1].parent, files[0].parent],
        )
        assert completed.returncode == exit_code, completed.stdout + completed.stderr
        assert set(summary) == {str(path.resolve()) for path in files}
        output_dirs = [Path(summary[str(path.resolve())]["output_dir"]) for path in files]
        assert output_dirs[0] != output_dirs[1]
        for path, content, expected in zip(files, contents, statuses):
            outcome = summary[str(path.resolve())]
            assert outcome["status"] == expected
            owned_output = Path(outcome["output_dir"])
            assert owned_output.is_relative_to(tmp_path / "batch-output")
            if expected == "success":
                assert (owned_output / "input" / path.name).read_bytes() == content
                persisted = json.loads((owned_output / "result.json").read_text(encoding="utf-8"))
                assert persisted == outcome["result"]
                assert persisted["status"] == "success"
                assert persisted["metadata"]["file_path"] == str(path.resolve())
            else:
                assert outcome["error"]
        if "success" in statuses:
            assert server.requests
            assert {request["model"] for request in server.requests} == {"loopback/batch"}
        else:
            assert server.requests == []


def test_pipeline_error_result_is_retained_and_other_input_continues(tmp_path):
    files = input_pair(tmp_path, [b"RETURN_PIPELINE_ERROR", b""])

    def respond(request):
        prompt = request["messages"][-1]["content"]
        if "RETURN_PIPELINE_ERROR" in prompt and "Extract all important entities" in prompt:
            # A real provider protocol error reaches the engine's error-result path:
            # graph construction cannot use a list-valued entity text as a node.
            return completion_reply('[{"text": ["invalid entity"], "category": "concept", "confidence": 0.9}]')
        return completion_reply("[]")

    with OpenRouterHTTP(responder=respond) as server:
        completed, summary = run_batch(tmp_path, server, "--batch", [path.parent for path in files])
        assert completed.returncode == 1, completed.stdout + completed.stderr
        assert set(summary) == {str(path.resolve()) for path in files}
        failed = summary[str(files[0].resolve())]
        succeeded = summary[str(files[1].resolve())]
        assert failed["status"] == failed["result"]["status"] == "error"
        assert failed["error"] == failed["result"]["error"]
        assert failed["error"]
        assert succeeded["status"] == succeeded["result"]["status"] == "success"
        persisted_failure = json.loads((Path(failed["output_dir"]) / "result.json").read_text(encoding="utf-8"))
        assert persisted_failure == failed["result"]
