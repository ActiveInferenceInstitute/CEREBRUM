"""Generalized Notation Notation (GNN) specification loading and conformance checks.

The discrete generative model of :mod:`src.core.active_inference` and its
canonical concrete instantiation in :mod:`src.models.insect.base` are specified
in ``models/case_conditioned_active_inference.gnn.md`` using Generalized
Notation Notation (GNN v1.1). This module parses that repository specification
without depending on the upstream ``gnn`` package, exposes the declared
structure, and compares it with the parameters and case configurations the
framework actually uses.

The specification file is a repository artifact, so this module resolves it
relative to the checkout root; when the package is installed outside its
checkout, :func:`load_spec` reports the missing file instead of guessing.
"""

from __future__ import annotations

import ast
import re
from collections.abc import Mapping
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Final

import numpy as np

REPO_ROOT: Final[Path] = Path(__file__).resolve().parents[2]
SPEC_PATH: Final[Path] = REPO_ROOT / "models" / "case_conditioned_active_inference.gnn.md"

EXPECTED_SECTION: Final[str] = "ActInfCerebrumCaseConditioned"
EXPECTED_MODEL_ID: Final[str] = "cerebrum_active_inference_model"
EXPECTED_EDGES: Final[frozenset[tuple[str, str, str]]] = frozenset(
    {
        ("D_state", ">", "s_state"),
        ("s_state", ">", "A_observation"),
        ("A_observation", ">", "o_observation"),
        ("s_state", ">", "B_state"),
        ("u_control", ">", "B_state"),
        ("B_state", ">", "s_state"),
        ("G", ">", "π_control"),
        ("π_control", ">", "u_control"),
        ("F", "-", "s_state"),
    }
)
CASE_NAMES: Final[tuple[str, ...]] = (
    "nominative",
    "accusative",
    "genitive",
    "dative",
    "instrumental",
    "locative",
    "ablative",
    "vocative",
)
EXPECTED_FOCUS: Final[dict[str, str]] = {
    "nominative": "likelihood",
    "accusative": "parameters",
    "genitive": "outputs",
    "dative": "inputs",
    "instrumental": "operations",
    "locative": "contexts",
    "ablative": "historical",
    "vocative": "identity",
}
EXPECTED_TARGETS: Final[dict[str, tuple[str, ...]]] = {
    "likelihood": ("likelihood_precision",),
    "parameters": ("prior_precision",),
    "outputs": ("posterior_precision",),
    "inputs": ("likelihood_precision",),
    "operations": ("likelihood_precision", "prior_precision"),
    "contexts": ("posterior_precision",),
    "historical": ("prior_precision",),
    "identity": ("prior_precision",),
}
PRECISION_ATTRIBUTES: Final[tuple[str, ...]] = (
    "likelihood_precision",
    "prior_precision",
    "posterior_precision",
)


class GNNSpecError(ValueError):
    """Raised when the GNN specification is missing or malformed."""


@dataclass(frozen=True)
class GNNSpec:
    """The parsed structure of a GNN specification file."""

    section: str
    version: str
    model_name: str
    variables: Mapping[str, tuple[int, ...]]
    connections: tuple[tuple[str, str, str], ...]
    initial_parameterization: Mapping[str, str]
    model_parameters: Mapping[str, str]

    def shape(self, name: str) -> tuple[int, ...]:
        """Return the declared dimensions of a state-space variable."""

        try:
            return self.variables[name]
        except KeyError as exc:
            raise GNNSpecError(f"{name} is not declared in StateSpaceBlock") from exc

    def integer_parameter(self, key: str) -> int:
        """Return a declared integer model parameter."""

        raw = self.text_parameter(key)
        try:
            return int(raw)
        except ValueError as exc:
            raise GNNSpecError(f"model parameter {key} must be an integer, got {raw!r}") from exc

    def text_parameter(self, key: str) -> str:
        """Return a declared model parameter as text."""

        try:
            return self.model_parameters[key]
        except KeyError as exc:
            raise GNNSpecError(f"model parameter {key} is not declared") from exc

    def case_focus(self) -> dict[str, str]:
        """Return the declared per-case precision focus mapping."""

        return {
            case: self.text_parameter(f"case_precision_focus_{case}") for case in CASE_NAMES
        }

    def case_targets(self) -> dict[str, tuple[str, ...]]:
        """Return the declared focus-to-rescaled-matrix mapping."""

        targets: dict[str, tuple[str, ...]] = {}
        for focus in EXPECTED_TARGETS:
            raw = self.text_parameter(f"case_precision_target_{focus}")
            targets[focus] = tuple(part for part in raw.split("+") if part)
        return targets

    def edges(self) -> frozenset[tuple[str, str, str]]:
        """Return the declared connections as ``(source, operator, target)`` triples."""

        return frozenset(self.connections)


_SECTION_PATTERN: Final = re.compile(r"^##\s+(?P<name>.+?)\s*$")
_IDENTIFIER: Final = r"[^\W\d]\w*"
_DECLARATION_PATTERN: Final = re.compile(rf"^(?P<name>{_IDENTIFIER})\[(?P<dims>[^\]]*)\]")
_CONNECTION_PATTERN: Final = re.compile(
    rf"^(?P<left>{_IDENTIFIER})\s*(?P<operator>>|-)\s*(?P<right>{_IDENTIFIER})$"
)
_ASSIGNMENT_PATTERN: Final = re.compile(rf"^(?P<name>{_IDENTIFIER})\s*=")

_REQUIRED_SECTIONS: Final[tuple[str, ...]] = (
    "GNNSection",
    "GNNVersionAndFlags",
    "ModelName",
    "StateSpaceBlock",
    "Connections",
    "InitialParameterization",
    "ModelParameters",
)


def _split_sections(text: str) -> dict[str, str]:
    sections: dict[str, str] = {}
    current: str | None = None
    lines: list[str] = []
    for raw_line in text.splitlines():
        match = _SECTION_PATTERN.match(raw_line)
        if match is not None:
            if current is not None:
                sections[current] = "\n".join(lines)
            current = match.group("name")
            lines = []
            continue
        if current is not None:
            lines.append(raw_line)
    if current is not None:
        sections[current] = "\n".join(lines)
    return sections


def _parse_variables(block: str) -> dict[str, tuple[int, ...]]:
    variables: dict[str, tuple[int, ...]] = {}
    for raw_line in block.splitlines():
        line = raw_line.split("#", 1)[0].strip()
        if not line:
            continue
        match = _DECLARATION_PATTERN.match(line)
        if match is None:
            raise GNNSpecError(f"unparsable StateSpaceBlock declaration: {raw_line!r}")
        dimensions: list[int] = []
        for token in match.group("dims").split(","):
            token = token.strip()
            if not token or token.startswith("type="):
                continue
            try:
                dimensions.append(int(token))
            except ValueError as exc:
                raise GNNSpecError(f"non-integer dimension in {raw_line!r}") from exc
        name = match.group("name")
        if name in variables:
            raise GNNSpecError(f"duplicate StateSpaceBlock declaration: {name}")
        variables[name] = tuple(dimensions)
    if not variables:
        raise GNNSpecError("StateSpaceBlock declares no variables")
    return variables


def _parse_connections(block: str) -> tuple[tuple[str, str, str], ...]:
    connections: list[tuple[str, str, str]] = []
    for raw_line in block.splitlines():
        line = raw_line.split("#", 1)[0].strip()
        if not line:
            continue
        match = _CONNECTION_PATTERN.match(line)
        if match is None:
            raise GNNSpecError(f"unparsable Connections entry: {raw_line!r}")
        connections.append((match.group("left"), match.group("operator"), match.group("right")))
    if not connections:
        raise GNNSpecError("Connections declares no edges")
    return tuple(connections)


def _parse_initial_parameterization(block: str) -> dict[str, str]:
    entries: dict[str, str] = {}
    lines = block.splitlines()
    index = 0
    while index < len(lines):
        line = lines[index].split("#", 1)[0].strip()
        index += 1
        if not line:
            continue
        match = _ASSIGNMENT_PATTERN.match(line)
        if match is None:
            raise GNNSpecError(f"unparsable InitialParameterization entry: {line!r}")
        name = match.group("name")
        value = line[match.end() :].strip()
        depth = value.count("{") - value.count("}")
        while depth > 0 and index < len(lines):
            continuation = lines[index].split("#", 1)[0].strip()
            index += 1
            value = f"{value} {continuation}"
            depth += continuation.count("{") - continuation.count("}")
        if depth != 0:
            raise GNNSpecError(f"unterminated InitialParameterization value for {name}")
        if name in entries:
            raise GNNSpecError(f"duplicate InitialParameterization entry: {name}")
        entries[name] = value
    if not entries:
        raise GNNSpecError("InitialParameterization declares no matrices")
    return entries


def _parse_model_parameters(block: str) -> dict[str, str]:
    parameters: dict[str, str] = {}
    for raw_line in block.splitlines():
        line = raw_line.split("#", 1)[0].strip()
        if not line:
            continue
        key, separator, value = line.partition(":")
        if not separator or not key.strip() or not value.strip():
            raise GNNSpecError(f"unparsable ModelParameters entry: {raw_line!r}")
        parameters[key.strip()] = value.strip()
    if not parameters:
        raise GNNSpecError("ModelParameters declares no parameters")
    return parameters


def parse_spec(text: str) -> GNNSpec:
    """Parse GNN specification text into a :class:`GNNSpec`."""

    sections = _split_sections(text)
    missing = [section for section in _REQUIRED_SECTIONS if section not in sections]
    if missing:
        raise GNNSpecError(f"missing required sections: {', '.join(missing)}")
    return GNNSpec(
        section=sections["GNNSection"].strip(),
        version=sections["GNNVersionAndFlags"].strip(),
        model_name=sections["ModelName"].strip(),
        variables=_parse_variables(sections["StateSpaceBlock"]),
        connections=_parse_connections(sections["Connections"]),
        initial_parameterization=_parse_initial_parameterization(
            sections["InitialParameterization"]
        ),
        model_parameters=_parse_model_parameters(sections["ModelParameters"]),
    )


def load_spec(path: Path | None = None) -> GNNSpec:
    """Load and parse the repository GNN specification."""

    spec_path = SPEC_PATH if path is None else path
    try:
        text = spec_path.read_text(encoding="utf-8")
    except OSError as exc:
        raise GNNSpecError(f"cannot read GNN specification at {spec_path}") from exc
    return parse_spec(text)


def declared_matrix(spec: GNNSpec, name: str) -> np.ndarray:
    """Return an ``InitialParameterization`` entry as a numeric array."""

    raw = spec.initial_parameterization.get(name)
    if raw is None:
        raise GNNSpecError(f"{name} is not declared in InitialParameterization")
    literal = raw.replace("{", "[").replace("}", "]")
    try:
        value = ast.literal_eval(literal)
    except (SyntaxError, ValueError) as exc:
        raise GNNSpecError(f"{name} is not a valid numeric literal: {raw!r}") from exc
    try:
        return np.asarray(value, dtype=float)
    except (TypeError, ValueError) as exc:
        raise GNNSpecError(f"{name} is not numeric: {raw!r}") from exc


def structural_issues(spec: GNNSpec) -> tuple[str, ...]:
    """Check the specification's internal structural contract."""

    issues: list[str] = []
    if spec.section != EXPECTED_SECTION:
        issues.append(f"unexpected GNNSection: {spec.section!r}")
    if not spec.version.startswith("GNN v"):
        issues.append(f"unexpected GNN version flag: {spec.version!r}")
    if spec.text_parameter("model_id") != EXPECTED_MODEL_ID:
        issues.append(f"unexpected model_id: {spec.text_parameter('model_id')!r}")

    states = spec.shape("s_state")[0]
    expected_shapes = {
        "s_state": (states, 1),
        "o_observation": (states, 1),
        "A_observation": (states, states),
        "B_state": (states, states, 3),
        "D_state": (states,),
        "u_control": (3, 1),
        "π_control": (3,),
    }
    for name, expected in expected_shapes.items():
        declared = spec.shape(name)
        if declared != expected:
            issues.append(f"{name} declares shape {declared}, expected {expected}")

    if spec.integer_parameter("num_factors") != 1:
        issues.append("the specification must declare exactly one hidden-state factor")
    if spec.integer_parameter("num_modalities") != 1:
        issues.append("the specification must declare exactly one observation modality")
    if spec.integer_parameter("num_actions") != 3:
        issues.append("the specification must declare the three default controls")
    if spec.integer_parameter("num_hidden_states") != states:
        issues.append("num_hidden_states must match the declared factor cardinality")
    if spec.integer_parameter("num_obs") != states:
        issues.append("num_obs must match the declared observation cardinality")
    if spec.text_parameter("conditional_independence") != "true":
        issues.append("the single-factor model must declare conditional_independence: true")
    if spec.text_parameter("observation_axis_order") != "state,observation":
        issues.append("the declared observation axis order must be state,observation")
    if spec.text_parameter("transition_axis_order") != "previous_state,action,next_state":
        issues.append("the declared transition axis order must be previous_state,action,next_state")

    declared_edges = spec.edges()
    if declared_edges != EXPECTED_EDGES:
        missing = sorted(EXPECTED_EDGES - declared_edges)
        extra = sorted(declared_edges - EXPECTED_EDGES)
        if missing:
            issues.append(f"missing declared connections: {missing}")
        if extra:
            issues.append(f"undeclared connections: {extra}")

    likelihood = declared_matrix(spec, "A_observation")
    if not np.array_equal(likelihood, np.eye(states)):
        issues.append("the declared likelihood must be the identity over hidden states")
    transition = declared_matrix(spec, "B_state")
    expected_transition = np.zeros((states, states, 3))
    for control in range(3):
        expected_transition[:, :, control] = np.eye(states)
    if not np.array_equal(transition, expected_transition):
        issues.append("the declared default transition must be the identity for every control")
    prior = declared_matrix(spec, "D_state")
    if not np.allclose(prior, np.full(states, 1 / states)):
        issues.append("the declared default prior must be uniform over hidden states")

    declared_focus = spec.case_focus()
    if declared_focus != EXPECTED_FOCUS:
        issues.append(f"unexpected case precision focus mapping: {declared_focus!r}")
    declared_targets = spec.case_targets()
    if declared_targets != EXPECTED_TARGETS:
        issues.append(f"unexpected case precision target mapping: {declared_targets!r}")
    return tuple(issues)


def parameter_issues(spec: GNNSpec, parameters: Mapping[str, Any]) -> tuple[str, ...]:
    """Compare the declared structure with model ``parameters``.

    The runtime stores ``observation_matrix[state, observation]`` and
    ``transition_matrix[previous_state, action, next_state]``; the declared
    matrices are converted with the documented permutations before comparison.
    """

    issues: list[str] = []
    states = spec.shape("s_state")[0]
    expected_keys = ("transition_matrix", "observation_matrix", "n_states", "n_actions")
    missing = [key for key in expected_keys if key not in parameters]
    if missing:
        return (f"parameters are missing: {', '.join(missing)}",)

    if parameters["n_states"] != states:
        issues.append(f"n_states is {parameters['n_states']}, expected {states}")
    if parameters["n_actions"] != 3:
        issues.append(f"n_actions is {parameters['n_actions']}, expected 3")
    if parameters.get("n_observations", states) != states:
        issues.append(f"n_observations is {parameters['n_observations']}, expected {states}")

    runtime_likelihood = np.asarray(parameters["observation_matrix"], dtype=float)
    declared_likelihood = declared_matrix(spec, "A_observation")
    if runtime_likelihood.shape != (states, states):
        issues.append(f"observation_matrix has shape {runtime_likelihood.shape}, expected {(states, states)}")
    elif not np.array_equal(runtime_likelihood.T, declared_likelihood):
        issues.append("observation_matrix is not the declared likelihood (state, observation order)")

    runtime_transition = np.asarray(parameters["transition_matrix"], dtype=float)
    declared_transition = declared_matrix(spec, "B_state")
    if runtime_transition.shape != (states, 3, states):
        issues.append(
            f"transition_matrix has shape {runtime_transition.shape}, expected {(states, 3, states)}"
        )
    elif not np.array_equal(np.transpose(runtime_transition, (2, 0, 1)), declared_transition):
        issues.append(
            "transition_matrix is not the declared transition "
            "(previous_state, action, next_state order)"
        )
    return tuple(issues)


def case_focus_issues(spec: GNNSpec, focus_by_case: Mapping[str, str]) -> tuple[str, ...]:
    """Compare the declared case precision focus mapping with the framework's."""

    declared = spec.case_focus()
    issues: list[str] = []
    for case, focus in declared.items():
        observed = focus_by_case.get(case)
        if observed is None:
            issues.append(f"case {case} is not configured by the framework")
        elif observed != focus:
            issues.append(f"case {case} focus is {observed!r}, expected {focus!r}")
    unexpected = sorted(set(focus_by_case) - set(declared))
    if unexpected:
        issues.append(f"framework cases missing from the specification: {unexpected}")
    return tuple(issues)


def conformance_issues(
    spec: GNNSpec, parameters: Mapping[str, Any], focus_by_case: Mapping[str, str]
) -> tuple[str, ...]:
    """Return every conformance issue for the declared specification."""

    return (
        structural_issues(spec)
        + parameter_issues(spec, parameters)
        + case_focus_issues(spec, focus_by_case)
    )
