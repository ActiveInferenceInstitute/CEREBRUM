"""
Tests for validated lexicon defect fixes.

Covers, with real logic and no mocks:
- deterministic fallback case assignment (structured_case_determiner)
- entity deduplication without transitive over-merging (entity_deduplicator)
- stable content-derived graph node IDs (graph/assembler)
- safe coreference re-segmentation (nlp/coreference_resolver)
- unique per-file batch output dirs (core/session)
- file watcher retry + thread-safe known_files bookkeeping (ingest/file_watcher)
- paraphrase cache treats an empty cached list as a hit (paraphrase/cache)
"""

import pytest

from src.lexicon.core.config import LexiconConfig
from src.lexicon.declension.structured_case_determiner import StructuredCaseDeterminer
from src.lexicon.graph.assembler import GraphAssembler
from src.lexicon.nlp.entity_deduplicator import EntityDeduplicator
from src.lexicon.nlp.coreference_resolver import CoreferenceResolver
from src.lexicon.nlp.preprocessor import ProcessedSegment
from src.lexicon.core.session import create_batch_input_subdirectories
from src.lexicon.ingest.file_watcher import FileWatcher
from src.lexicon.paraphrase import cache as paraphrase_cache


@pytest.fixture
def config(tmp_path):
    return LexiconConfig(
        output_dir=tmp_path / "out",
        cache_dir=tmp_path / "cache",
    )


# ─── Finding 1: deterministic fallback case assignment ───────────────────

class TestDeterministicCaseAssignment:
    def test_fallback_is_deterministic(self, config):
        determiner = StructuredCaseDeterminer(openrouter=None, config=config)
        # This entity falls through the heuristic branches into the default
        # "locative" branch that used to diversify via random.choice.
        first = determiner._create_fallback_assignment("zqxv")
        second = determiner._create_fallback_assignment("zqxv")
        assert first.case == second.case
        assert first.case in {"nominative", "accusative", "instrumental"}

    def test_fallback_stable_across_instances(self, config):
        a = StructuredCaseDeterminer(openrouter=None, config=config)
        b = StructuredCaseDeterminer(openrouter=None, config=config)
        assert (a._create_fallback_assignment("zqxv").case
                == b._create_fallback_assignment("zqxv").case)


# ─── Findings 9: dedup without transitive over-merge ─────────────────────

class TestEntityDeducp:
    def _dedup(self, entities):
        dedup = EntityDeduplicator()
        dedup._semantic_model = None  # keep offline/deterministic
        return dedup.deduplicate_entities(entities)

    def test_exact_text_duplicates_merge(self):
        out = self._dedup([
            {"text": "IBM", "confidence": 0.9},
            {"text": "ibm", "confidence": 0.8},  # same entity, case diff
            {"text": "Google", "confidence": 0.9},
        ])
        texts = {e["text"].lower() for e in out}
        assert "ibm" in texts
        assert len(out) == 2  # ibm + google

    def test_distinct_entities_kept_separate(self):
        # "Google" and "Google Research" share a word but are NOT the same
        # referent - transitively they must not all be fused.
        out = self._dedup([
            {"text": "IBM", "confidence": 0.9},
            {"text": "Google", "confidence": 0.9},
            {"text": "Google Research", "confidence": 0.8},
        ])
        texts = {e["text"].lower() for e in out}
        assert "google" in texts
        assert "google research" in texts
        assert len(out) == 3


# ─── Finding 10: stable graph node IDs / graph id ─────────────────────────

class TestStableGraphIds:
    def test_node_id_deterministic(self, config):
        assembler = GraphAssembler(openrouter=None, config=config)
        assert (assembler.node_id("entity", "Neural Network")
                == assembler.node_id("entity", "Neural Network"))

    def test_node_id_differs_by_content(self, config):
        assembler = GraphAssembler(openrouter=None, config=config)
        assert (assembler.node_id("entity", "Neural Network")
                != assembler.node_id("entity", "Random Forest"))

    def test_graph_id_stable_for_same_content(self, config):
        from src.lexicon.graph.assembler import KnowledgeGraph
        from src.lexicon.graph.cid_generator import generate_cid
        from src.lexicon.paraphrase.generator import ParaphrasedSegment

        seg = ParaphrasedSegment(segment_id="s1", text="same content")
        assembler = GraphAssembler(openrouter=None, config=config)
        g1 = assembler.build_graph([seg, seg])
        # Deterministic for identical content (not wall-clock derived).
        assert g1["id"] == generate_cid("graph", "\x1f".join(["same content"] * 2))


# ─── Finding 4: safe coreference re-segmentation ─────────────────────────

class TestCorefResegmentation:
    def _segments(self):
        return [
            ProcessedSegment(segment_id="1", text="John went to the store."),
            ProcessedSegment(segment_id="2", text="He bought milk."),
        ]

    def test_unshifted_offsets_exact(self):
        segs = self._segments()
        joined = "John went to the store. He bought milk."
        portions = CoreferenceResolver._map_segments_to_resolved(segs, joined, joined)
        assert portions == ["John went to the store.", "He bought milk."]

    def test_length_shift_does_not_corrupt_segments(self):
        segs = self._segments()
        joined = "John went to the store. He bought milk."
        # Pronoun "He" -> "John" changes overall length; the old code would slice
        # the second segment by the ORIGINAL offset and truncate it.
        resolved = "John went to the store. John bought milk."
        portions = CoreferenceResolver._map_segments_to_resolved(segs, joined, resolved)
        assert len(portions) == 2
        assert portions[0] == "John went to the store."
        # Second segment must contain its full content (no truncation/garbage).
        assert "bought milk" in portions[1]


# ─── Finding 8: unique per-file batch dirs ────────────────────────────────

class TestBatchSubdirs:
    def test_same_stem_files_get_unique_dirs(self, tmp_path):
        base = tmp_path / "batch"
        f1 = tmp_path / "a" / "notes.md"
        f2 = tmp_path / "b" / "notes.md"  # same stem, different directory
        f1.parent.mkdir()
        f2.parent.mkdir()
        f1.write_text("one")
        f2.write_text("two")

        dirs = create_batch_input_subdirectories(base, [f1, f2])
        assert dirs[str(f1)] != dirs[str(f2)]
        assert dirs[str(f1)].exists()
        assert dirs[str(f2)].exists()


# ─── Finding 7: file watcher retry + thread-safe known_files ─────────────

class TestFileWatcher:
    def test_retries_failing_callback(self, tmp_path):
        calls = {"n": 0}

        def cb(path):
            calls["n"] += 1
            if calls["n"] < 3:
                raise RuntimeError("transient failure")

        f = tmp_path / "a.txt"
        f.write_text("hi")
        watcher = FileWatcher(tmp_path, ["*.txt"], cb, retry_attempts=3, retry_delay=0)
        watcher._process_with_retry(f)
        assert calls["n"] == 3  # retried until success
        assert f in watcher.known_files

    def test_success_marks_known(self, tmp_path):
        seen = []
        f = tmp_path / "b.txt"
        f.write_text("hi")
        watcher = FileWatcher(tmp_path, ["*.txt"], seen.append, retry_attempts=2, retry_delay=0)
        watcher._process_with_retry(f)
        assert seen == [f]
        assert f in watcher.known_files

    def test_status_reads_known_files_thread_safely(self, tmp_path):
        f = tmp_path / "c.txt"
        f.write_text("hi")
        watcher = FileWatcher(tmp_path, ["*.txt"], lambda p: None, retry_attempts=2, retry_delay=0)
        watcher._mark_known(f)
        assert watcher.get_status()["known_files"] == 1


# ─── Finding 11: paraphrase cache empty-list-as-hit ───────────────────────

class TestParaphraseCacheEmpty:
    def test_empty_list_is_not_a_miss(self, tmp_path):
        cache_file = tmp_path / "seg.json"
        assert paraphrase_cache.save_to_cache(cache_file, []) is True
        got = paraphrase_cache.get_cache(cache_file)
        # An empty cached result must be returned as [] (a hit), not None (miss).
        assert got is not None
        assert got == []
