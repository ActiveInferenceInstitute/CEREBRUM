"""Tests for FORMICA transformation operations (real API)."""

import pytest

from beyond_cerebrum.src.formalisms.structures import Tree, TreeNode, Graph
from beyond_cerebrum.src.formalisms.types import SyntacticLabel
from beyond_cerebrum.src.operations.transformations import (
    parse_syntax_to_semantics,
    generate_syntax_from_semantics,
    pragmatic_enrichment,
)


@pytest.fixture
def sample_tree() -> Tree:
    root = TreeNode[SyntacticLabel](data=SyntacticLabel("S"), children=[])
    return Tree[SyntacticLabel](root=root)


@pytest.fixture
def sample_graph() -> Graph:
    return Graph()


@pytest.fixture
def sample_context() -> dict:
    return {"user": "test_user"}


def test_parse_syntax_to_semantics_returns_graph(sample_tree, sample_context):
    result = parse_syntax_to_semantics(sample_tree, sample_context)
    assert isinstance(result, Graph)
    assert len(result.nodes) == 0


def test_generate_syntax_from_semantics_returns_minimal_tree(sample_graph):
    result = generate_syntax_from_semantics(sample_graph, {"language": "en"})
    assert isinstance(result, Tree)
    assert result.root is not None
    assert result.root.data == SyntacticLabel("ROOT_PLACEHOLDER")


def test_pragmatic_enrichment_returns_input_unchanged(sample_graph, sample_context):
    enriched = pragmatic_enrichment(sample_graph, sample_context)
    assert enriched is sample_graph
