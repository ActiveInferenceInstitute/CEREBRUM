"""Live integration test for the unified LEXICON pipeline (opt-in).

Requires OPENROUTER_API_KEY and network access. Skipped otherwise.
Never run in the default suite: deselect with `-m "not live"`.
"""

import os
import uuid

import pytest

pytestmark = pytest.mark.live

pytest.importorskip("requests")

CASE_SLOTS = (
    "nominative",
    "accusative",
    "genitive",
    "dative",
    "locative",
    "instrumental",
    "ablative",
    "vocative",
)


@pytest.fixture(scope="module")
def engine():
    if not os.environ.get("OPENROUTER_API_KEY"):
        pytest.skip("OPENROUTER_API_KEY not set")
    from src.lexicon.core.config import LexiconConfig
    from src.lexicon.core.engine import LexiconEngine

    return LexiconEngine(LexiconConfig())


def test_process_text_unified_pipeline(engine):
    """process_text runs the unified NLPPreprocessor + CaseTagger pipeline."""
    result = engine.process_text(
        "Alice met Bob in Paris. She gave him the book from Oxford."
    )

    assert result["status"] == "success"
    # Entity/claim/graph outputs (the pre-existing shipped path)
    assert result["entities"], "entities should be detected"
    assert all("text" in e for e in result["entities"])
    # Unified component pipeline outputs
    assert result.get("segments"), "NLP preprocessor segments should be present"
    texts = [s["text"] for s in result["segments"]]
    assert any("Alice" in t for t in texts)
    assert result.get("cased_segments"), "cased segments should be present"
    for cased in result["cased_segments"]:
        for case in (
            "nominative",
            "accusative",
            "genitive",
            "dative",
            "locative",
            "instrumental",
            "ablative",
            "vocative",
        ):
            assert case in cased
    # In a simple subject-verb-object text, at least one of the eight case
    # slots should be populated somewhere. (Which slot the LLM fills varies
    # by model and run; we assert the mechanism, not a specific tagging.)
    assert any(
        any(cased[case] for case in CASE_SLOTS)
        for cased in result["cased_segments"]
    )
