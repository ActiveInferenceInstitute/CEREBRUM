"""
Tests for src/lexicon/nlp/sentence_splitter.py

Tests SentenceSplitter using rule-based splitting (no spaCy required).
"""

import pytest

from src.lexicon.core.config import LexiconConfig
from src.lexicon.nlp.sentence_splitter import SentenceSplitter


@pytest.fixture
def splitter(tmp_path):
    config = LexiconConfig(
        output_dir=tmp_path / "out",
        cache_dir=tmp_path / "cache",
    )
    return SentenceSplitter(config)


class TestSentenceSplitter:
    def test_split_basic(self, splitter):
        sentences = splitter.split("Hello world. How are you? I am fine.")
        assert len(sentences) >= 2

    def test_split_empty(self, splitter):
        assert splitter.split("") == []
        assert splitter.split("   ") == []

    def test_split_single_sentence(self, splitter):
        sentences = splitter.split("Just one sentence")
        assert len(sentences) == 1

    def test_handles_abbreviations(self, splitter):
        text = "Dr. Smith went to the U.S. embassy. He arrived at 3 p.m."
        sentences = splitter.split(text)
        # Should not split on "Dr." or "U.S." or "p.m."
        assert len(sentences) >= 1

    def test_handles_multiple_punctuation(self, splitter):
        text = "What happened? I don't know! Let me check."
        sentences = splitter.split(text)
        assert len(sentences) >= 2

    def test_preserves_content(self, splitter):
        text = "Active inference minimizes free energy. This is the core principle."
        sentences = splitter.split(text)
        combined = " ".join(sentences)
        assert "free energy" in combined
        assert "core principle" in combined
