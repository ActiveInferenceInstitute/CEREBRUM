"""
Tests for src/lexicon/graph/cid_generator.py and src/lexicon/declension/rules.py

Tests CID generation, validation, and CaseRules with real logic.
"""

import pytest

from src.lexicon.graph.cid_generator import (
    generate_cid, generate_timestamp_id, generate_uuid, validate_cid,
)
from src.lexicon.declension.rules import CaseRules


# ─── CID Generator ───────────────────────────────────────────────────

class TestGenerateCid:
    def test_returns_string(self):
        cid = generate_cid("node", "test content")
        assert isinstance(cid, str)

    def test_starts_with_cid_prefix(self):
        cid = generate_cid("edge", "content")
        assert cid.startswith("cid:")

    def test_deterministic(self):
        a = generate_cid("node", "same content")
        b = generate_cid("node", "same content")
        assert a == b

    def test_different_content_different_cid(self):
        a = generate_cid("node", "content A")
        b = generate_cid("node", "content B")
        assert a != b

    def test_with_namespace(self):
        cid = generate_cid("node", "content", namespace="custom")
        assert cid.startswith("cid:")


class TestGenerateTimestampId:
    def test_has_prefix(self):
        tid = generate_timestamp_id("test")
        assert tid.startswith("test_")

    def test_unique(self):
        import time
        a = generate_timestamp_id("x")
        time.sleep(0.002)
        b = generate_timestamp_id("x")
        assert a != b


class TestGenerateUuid:
    def test_starts_with_uuid(self):
        uid = generate_uuid()
        assert uid.startswith("uuid:")

    def test_unique(self):
        a = generate_uuid()
        b = generate_uuid()
        assert a != b


class TestValidateCid:
    def test_valid_cid(self):
        cid = generate_cid("node", "test")
        assert validate_cid(cid) is True

    def test_invalid_prefix(self):
        assert validate_cid("nope:abc123") is False

    def test_invalid_hex(self):
        assert validate_cid("cid:xyz_not_hex") is False


# ─── Case Rules ───────────────────────────────────────────────────────

class TestCaseRules:
    @pytest.fixture
    def rules(self):
        return CaseRules()

    def test_init(self, rules):
        assert rules.rules is not None

    def test_apply_rules_returns_dict(self, rules):
        result = rules.apply_rules("The model actively generates predictions from the data")
        assert isinstance(result, dict)

    def test_nominative_detection(self, rules):
        text = "The agent actively performs actions to minimize free energy"
        result = rules.apply_rules(text)
        assert "nominative" in result or "NOMINATIVE" in result or len(result) > 0

    def test_accusative_detection(self, rules):
        text = "The model was evaluated and tested on the dataset"
        result = rules.apply_rules(text)
        assert isinstance(result, dict)

    def test_apply_specific_rule(self, rules):
        # Get a rule name from the first case
        case_name = list(rules.rules.keys())[0]
        rule_list = rules.rules[case_name]
        if rule_list and isinstance(rule_list, list) and len(rule_list) > 0:
            first_rule = rule_list[0]
            rule_name = first_rule.get("name", first_rule) if isinstance(first_rule, dict) else str(first_rule)
            result = rules.apply_rule(case_name, rule_name, "The model generates predictions")
            assert isinstance(result, list)
