"""Exercise the manuscript CLI with real inputs and unavailable render tools."""

import os
from pathlib import Path
import subprocess
import sys

import pytest


SCRIPT = Path(__file__).resolve().parents[1] / "paper" / "assemble_paper.py"


@pytest.mark.parametrize("failure", ["assembly", "conversion", "figure"])
def test_manuscript_failure_exits_nonzero(tmp_path, failure):
    if failure != "assembly":
        components = tmp_path / "paper" / "components" / "main_text"
        components.mkdir(parents=True)
        (components / "1_title.md").write_text("# Test manuscript\n", encoding="utf-8")
    if failure == "figure":
        figures = tmp_path / "paper" / "components" / "figures"
        figures.mkdir()
        (figures / "Figure_987654321.md").write_text(
            "```mermaid\ngraph TD\nA --> B\n```\n", encoding="utf-8"
        )

    # No machine-installed Pandoc/Mermaid can turn these failure cases into successes.
    result = subprocess.run(
        [sys.executable, str(SCRIPT)],
        cwd=tmp_path,
        env={**os.environ, "PATH": ""},
        capture_output=True,
        text=True,
        check=False,
    )

    assert result.returncode == 1, result.stderr
    assert "Paper Assembly Process Finished" in result.stderr
    if failure == "assembly":
        assert "Markdown assembly failed. Skipping HTML and PDF conversion." in result.stderr
        assert "Converting to HTML" not in result.stderr
    else:
        assert "Markdown assembly successful." in result.stderr
        assert "HTML conversion failed." in result.stderr
        assert "PDF conversion failed." in result.stderr
    if failure == "figure":
        assert "Some Mermaid figures failed to render" in result.stderr
