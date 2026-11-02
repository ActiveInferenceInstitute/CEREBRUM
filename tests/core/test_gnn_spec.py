"""GNN specification conformance and pinned-upstream validation tests.

These tests keep three things in agreement: the GNN specification in
``models/``, the parameters and case configurations the framework actually uses
(``src/core/active_inference.py``, ``src/models/insect/base.py``), and the
pinned upstream revision recorded in ``.github/gnn-pin.json``. The upstream
parser checks run against a clean export of the pinned revision, so in-progress
edits in a sibling GNN checkout cannot contaminate the result; they skip with an
explicit reason when the checkout or its interpreter is unavailable, and fail
instead of skipping when the checkout was selected explicitly (CI sets
``GNN_CHECKOUT``, so a missing or unusable checkout is a red check there).
"""

from __future__ import annotations

import io
import json
import os
import re
import subprocess
import tarfile
from pathlib import Path
from typing import Any, Dict, Iterator

import numpy as np
import pytest

from src.core.active_inference import ActiveInferenceModel
from src.core.gnn import (
    EXPECTED_EDGES,
    EXPECTED_FOCUS,
    EXPECTED_TARGETS,
    PRECISION_ATTRIBUTES,
    SPEC_PATH,
    GNNSpecError,
    case_focus_issues,
    conformance_issues,
    declared_matrix,
    load_spec,
    parameter_issues,
    parse_spec,
    structural_issues,
)
from src.core.model import Case
from src.models.insect.base import InsectModel

REPO_ROOT = Path(__file__).resolve().parents[2]
PIN_PATH = REPO_ROOT / ".github" / "gnn-pin.json"
CHECKOUT_ENV = "GNN_CHECKOUT"
DEFAULT_CHECKOUT = REPO_ROOT.parent / "Generalized_Notation_Notation"
UPSTREAM_TIMEOUT_SECONDS = 300


def _checkout() -> Path:
    override = os.environ.get(CHECKOUT_ENV)
    return Path(override) if override else DEFAULT_CHECKOUT


def _spec_text() -> str:
    return SPEC_PATH.read_text(encoding="utf-8")


def _replace_block(text: str, section: str, body: str) -> str:
    pattern = re.compile(
        rf"(^## {re.escape(section)}\n)(.*?)(?=^## |\Z)", re.MULTILINE | re.DOTALL
    )
    replaced, count = pattern.subn(lambda match: match.group(1) + body, text, count=1)
    assert count == 1, f"section {section} not found"
    return replaced


def _pin() -> Dict[str, Any]:
    return json.loads(PIN_PATH.read_text(encoding="utf-8"))


def _focus_by_case(model: ActiveInferenceModel) -> Dict[str, str]:
    return {
        case.name.lower(): configuration["precision_focus"]
        for case, configuration in model._case_configurations.items()
    }


def _unavailable(message: str) -> None:
    """Skip when no checkout was selected; fail when one was selected explicitly.

    ``GNN_CHECKOUT`` is the explicit selection, and CI sets it after
    provisioning the pinned revision. An unusable selected checkout must be a
    failure, never a skip: a skipped check cannot fail and so proves nothing.
    """

    if os.environ.get(CHECKOUT_ENV):
        pytest.fail(f"{CHECKOUT_ENV} selects a checkout, but {message}")
    pytest.skip(message)


@pytest.fixture(scope="module")
def pinned_upstream(tmp_path_factory: pytest.TempPathFactory) -> Iterator[Any]:
    """Yield ``(checkout, exported_tree, revision)`` for the pinned revision."""

    checkout = _checkout()
    if not (checkout / ".git").exists():
        _unavailable(f"no GNN checkout exists at {checkout}")
    revision = str(_pin()["revision"])
    interpreter = checkout / ".venv" / "bin" / "python"
    if not interpreter.is_file():
        _unavailable(f"the GNN checkout at {checkout} has no .venv/bin/python interpreter")
    archive = subprocess.run(
        ["git", "-C", str(checkout), "archive", revision, "src/gnn", "pyproject.toml"],
        capture_output=True,
        check=False,
    )
    if archive.returncode != 0:
        _unavailable(
            f"pinned revision {revision} is not available in {checkout}: "
            f"{archive.stderr.decode(errors='replace').strip()}"
        )
    tree = tmp_path_factory.mktemp("gnn-pinned")
    with tarfile.open(fileobj=io.BytesIO(archive.stdout)) as bundle:
        bundle.extractall(tree, filter="data")
    yield checkout, tree, revision


def test_spec_is_structurally_conformant() -> None:
    spec = load_spec()
    assert spec.section == "ActInfCerebrumCaseConditioned"
    assert structural_issues(spec) == ()
    assert spec.edges() == EXPECTED_EDGES
    assert spec.case_focus() == EXPECTED_FOCUS
    assert spec.case_targets() == EXPECTED_TARGETS


def test_insect_default_parameters_match_the_specification() -> None:
    spec = load_spec()
    insect = InsectModel(species="spec-check")
    assert parameter_issues(spec, insect.parameters) == ()
    assert insect.parameters["n_states"] == spec.integer_parameter("num_hidden_states")
    assert insect.parameters["n_actions"] == spec.integer_parameter("num_actions")
    assert np.array_equal(
        declared_matrix(spec, "A_observation"), np.asarray(insect.parameters["observation_matrix"]).T
    )
    assert np.array_equal(
        declared_matrix(spec, "B_state"),
        np.transpose(np.asarray(insect.parameters["transition_matrix"]), (2, 0, 1)),
    )


def test_case_configurations_match_the_specification() -> None:
    spec = load_spec()
    model = ActiveInferenceModel(name="spec-check")
    assert case_focus_issues(spec, _focus_by_case(model)) == ()
    assert set(_focus_by_case(model)) == set(EXPECTED_FOCUS)


def test_full_conformance_is_clean() -> None:
    spec = load_spec()
    insect = InsectModel(species="spec-check")
    model = ActiveInferenceModel(name="spec-check")
    assert conformance_issues(spec, insect.parameters, _focus_by_case(model)) == ()


def test_declared_prior_is_the_initialized_posterior() -> None:
    insect = InsectModel(species="spec-check")
    assert np.allclose(insect.posterior_means, np.full(5, 0.2))
    assert np.allclose(declared_matrix(load_spec(), "D_state"), insect.posterior_means)


def test_observation_axis_order_matches_the_declaration() -> None:
    parameters: Dict[str, Any] = {
        "observation_matrix": np.array([[0.9, 0.1], [0.2, 0.8]]),  # A[state, observation]
        "transition_matrix": np.zeros((2, 3, 2)),
        "n_states": 2,
        "n_actions": 3,
        "n_observations": 2,
    }
    model = ActiveInferenceModel(name="axis-check", parameters=parameters)
    assert np.allclose(model.posterior_means, np.full(2, 0.5))
    result = model.update_posterior(np.array([1.0, 0.0]))
    assert result["status"] == "success"
    # A[state, observation] convention: likelihood = (A[0, 0], A[1, 0]) = (0.9, 0.2).
    assert np.allclose(model.posterior_means, np.array([0.9, 0.2]) / 1.1)
    # predict_observation uses the same stored orientation.
    assert np.allclose(model.predict_observation(np.array([0.3, 0.7])), np.array([0.41, 0.59]))


def test_transition_axis_order_matches_the_declaration() -> None:
    transition = np.zeros((2, 3, 2))  # transition[previous_state, action, next_state]
    transition[1, 1, 0] = 1.0  # from state 0 under control 1 -> state 1
    transition[0, 1, 1] = 1.0  # from state 1 under control 1 -> state 0
    transition[0, 2, 0] = 1.0  # identity under control 2
    transition[1, 2, 1] = 1.0
    parameters: Dict[str, Any] = {
        "observation_matrix": np.eye(2),
        "transition_matrix": transition,
        "n_states": 2,
        "n_actions": 3,
        "n_observations": 2,
    }
    model = ActiveInferenceModel(name="axis-check", parameters=parameters)
    model.posterior_means = np.array([0.3, 0.7])
    assert np.allclose(model.predict_next_state(1), np.array([0.7, 0.3]))
    assert np.allclose(model.predict_next_state(2), np.array([0.3, 0.7]))


@pytest.mark.parametrize("case", list(Case))
def test_case_transformations_scale_the_declared_matrices(case: Case) -> None:
    spec = load_spec()
    model = ActiveInferenceModel(name="case-check")
    base = {
        attribute: getattr(model, f"_base_{attribute}").copy()
        for attribute in PRECISION_ATTRIBUTES
    }
    intermediate = Case.ACCUSATIVE if case is not Case.ACCUSATIVE else Case.DATIVE
    model.case = intermediate
    before = {attribute: getattr(model, attribute).copy() for attribute in PRECISION_ATTRIBUTES}
    model.case = case
    targets = spec.case_targets()[spec.case_focus()[case.name.lower()]]
    precision = model.get_precision(case)
    for attribute in PRECISION_ATTRIBUTES:
        observed = getattr(model, attribute)
        if attribute in targets:
            assert np.allclose(observed, base[attribute] * precision), attribute
        else:
            # Non-target matrices keep the value from the previous transition.
            assert np.allclose(observed, before[attribute]), attribute


def test_pin_record_matches_the_specification() -> None:
    pin = _pin()
    assert pin["schema"] == "cerebrum-gnn-pin-v1"
    assert pin["tag"] == "v5.0.3"
    assert pin["version"] == "5.0.3"
    assert pin["revision"] == "8f293d9ba810e6eda5c09842d9c7ed9ffa5f92c1"
    assert len(str(pin["revision"])) == 40
    assert pin["spec_directory"] == "models"
    assert pin["spec_files"] == ["models/case_conditioned_active_inference.gnn.md"]
    for spec_file in pin["spec_files"]:
        assert (REPO_ROOT / str(spec_file)).is_file()


def test_pin_record_revision_is_the_declared_tag(pinned_upstream: Any) -> None:
    checkout, _, revision = pinned_upstream
    resolved = subprocess.run(
        ["git", "-C", str(checkout), "rev-parse", f"{_pin()['tag']}^{{commit}}"],
        capture_output=True,
        text=True,
        check=False,
    )
    if resolved.returncode != 0:
        pytest.skip(f"tag {_pin()['tag']} is not available in {checkout}")
    assert resolved.stdout.strip() == revision


def test_upstream_parser_accepts_the_specification(pinned_upstream: Any) -> None:
    checkout, tree, _ = pinned_upstream
    result = subprocess.run(
        [
            str(checkout / ".venv" / "bin" / "python"),
            "-m",
            "gnn.cli",
            "validate",
            str(SPEC_PATH),
            "--strict",
        ],
        capture_output=True,
        text=True,
        env={**os.environ, "PYTHONPATH": str(tree / "src")},
        timeout=UPSTREAM_TIMEOUT_SECONDS,
        check=False,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert "valid" in result.stdout


def test_upstream_export_is_the_pinned_version(pinned_upstream: Any) -> None:
    checkout, tree, revision = pinned_upstream
    result = subprocess.run(
        [
            str(checkout / ".venv" / "bin" / "python"),
            "-c",
            "import gnn; print(gnn.__file__); print(getattr(gnn, '__version__', ''))",
        ],
        capture_output=True,
        text=True,
        env={**os.environ, "PYTHONPATH": str(tree / "src")},
        timeout=UPSTREAM_TIMEOUT_SECONDS,
        check=False,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    module_path, version = result.stdout.strip().splitlines()
    assert str(tree) in module_path, "the export must shadow any installed gnn package"
    assert version == _pin()["version"], f"expected {revision} to report version {_pin()['version']}"


def test_structural_drift_is_reported() -> None:
    text = _spec_text()
    drifted = text.replace("num_actions: 3", "num_actions: 4")
    assert structural_issues(parse_spec(drifted)) != ()
    dropped = text.replace("u_control>B_state\n", "")
    assert any(
        "missing declared connections" in issue
        for issue in structural_issues(parse_spec(dropped))
    )
    extra = text.replace("B_state>s_state\n", "B_state>s_state\nF>D_state\n")
    assert any(
        "undeclared connections" in issue for issue in structural_issues(parse_spec(extra))
    )
    focus = text.replace(
        "case_precision_focus_nominative: likelihood", "case_precision_focus_nominative: inputs"
    )
    assert any(
        "unexpected case precision focus" in issue
        for issue in structural_issues(parse_spec(focus))
    )
    target = text.replace(
        "case_precision_target_identity: prior_precision",
        "case_precision_target_identity: likelihood_precision",
    )
    assert any(
        "unexpected case precision target" in issue
        for issue in structural_issues(parse_spec(target))
    )
    likelihood = text.replace(
        "A_observation={\n  (1.0,0.0,0.0,0.0,0.0),", "A_observation={\n  (0.9,0.1,0.0,0.0,0.0),"
    )
    assert any(
        "declared likelihood" in issue for issue in structural_issues(parse_spec(likelihood))
    )
    transition = text.replace("(1.0,1.0,1.0),", "(0.5,0.5,1.0),", 1)
    assert any(
        "declared default transition" in issue
        for issue in structural_issues(parse_spec(transition))
    )
    prior = text.replace("D_state={0.2,0.2,0.2,0.2,0.2}", "D_state={0.5,0.1,0.1,0.1,0.2}")
    assert any("declared default prior" in issue for issue in structural_issues(parse_spec(prior)))


def test_parameter_and_case_drift_is_reported() -> None:
    spec = load_spec()
    insect = InsectModel(species="spec-check")
    assert parameter_issues(spec, {}) == ("parameters are missing: transition_matrix, observation_matrix, n_states, n_actions",)
    drifted: Dict[str, Any] = dict(insect.parameters)
    drifted["n_states"] = 4
    assert any("n_states is 4" in issue for issue in parameter_issues(spec, drifted))
    drifted = dict(insect.parameters)
    drifted["n_actions"] = 2
    assert any("n_actions is 2" in issue for issue in parameter_issues(spec, drifted))
    drifted = dict(insect.parameters)
    drifted["n_observations"] = 4
    assert any("n_observations is 4" in issue for issue in parameter_issues(spec, drifted))
    drifted = dict(insect.parameters)
    drifted["observation_matrix"] = np.zeros((5, 5))
    assert any("not the declared likelihood" in issue for issue in parameter_issues(spec, drifted))
    drifted = dict(insect.parameters)
    drifted["observation_matrix"] = np.zeros((3, 5))
    assert any("observation_matrix has shape" in issue for issue in parameter_issues(spec, drifted))
    drifted = dict(insect.parameters)
    drifted["transition_matrix"] = np.zeros((5, 3, 5))
    assert any("not the declared transition" in issue for issue in parameter_issues(spec, drifted))
    drifted = dict(insect.parameters)
    drifted["transition_matrix"] = np.zeros((5, 3, 3))
    assert any("transition_matrix has shape" in issue for issue in parameter_issues(spec, drifted))
    model = ActiveInferenceModel(name="spec-check")
    focus = _focus_by_case(model)
    focus["nominative"] = "inputs"
    assert any("focus is 'inputs'" in issue for issue in case_focus_issues(spec, focus))
    incomplete = dict(focus)
    del incomplete["vocative"]
    assert any("not configured by the framework" in issue for issue in case_focus_issues(spec, incomplete))
    extra = dict(focus, instrumental_case="operations")
    assert any("missing from the specification" in issue for issue in case_focus_issues(spec, extra))


def test_missing_and_malformed_specifications_fail_loudly(tmp_path: Path) -> None:
    with pytest.raises(GNNSpecError, match="cannot read"):
        load_spec(tmp_path / "absent.gnn.md")
    with pytest.raises(GNNSpecError, match="missing required sections"):
        parse_spec("## GNNSection\nOnly\n")
    with pytest.raises(GNNSpecError, match="no variables"):
        parse_spec(_replace_block(_spec_text(), "StateSpaceBlock", "# none\n"))
    with pytest.raises(GNNSpecError, match="unparsable StateSpaceBlock"):
        parse_spec(_spec_text().replace("D_state[5,type=float]", "D_state = uniform"))
    with pytest.raises(GNNSpecError, match="non-integer dimension"):
        parse_spec(_spec_text().replace("D_state[5,type=float]", "D_state[n,type=float]"))
    with pytest.raises(GNNSpecError, match="duplicate StateSpaceBlock"):
        parse_spec(_spec_text().replace("D_state[5,type=float]", "s_state[5,1,type=int]"))
    with pytest.raises(GNNSpecError, match="no edges"):
        parse_spec(_replace_block(_spec_text(), "Connections", "# none\n"))
    with pytest.raises(GNNSpecError, match="unparsable Connections"):
        parse_spec(_spec_text().replace("D_state>s_state", "D_state s_state"))
    with pytest.raises(GNNSpecError, match="no matrices"):
        parse_spec(_replace_block(_spec_text(), "InitialParameterization", "# none\n"))
    with pytest.raises(GNNSpecError, match="unparsable InitialParameterization"):
        parse_spec(_spec_text().replace("D_state={0.2,0.2,0.2,0.2,0.2}", "D_state ~ 0.2"))
    with pytest.raises(GNNSpecError, match="unterminated"):
        parse_spec(_spec_text().replace("D_state={0.2,0.2,0.2,0.2,0.2}", "D_state={0.2,0.2"))
    with pytest.raises(GNNSpecError, match="duplicate InitialParameterization"):
        parse_spec(
            _spec_text().replace(
                "D_state={0.2,0.2,0.2,0.2,0.2}",
                "D_state={0.2,0.2,0.2,0.2,0.2}\nD_state={0.2,0.2,0.2,0.2,0.2}",
            )
        )
    with pytest.raises(GNNSpecError, match="unparsable ModelParameters"):
        parse_spec(_spec_text().replace("num_actions: 3", "num_actions"))
    with pytest.raises(GNNSpecError, match="no parameters"):
        parse_spec(_replace_block(_spec_text(), "ModelParameters", "# none\n"))


def test_spec_accessors_report_missing_declarations() -> None:
    spec = load_spec()
    with pytest.raises(GNNSpecError, match="not declared in StateSpaceBlock"):
        spec.shape("s_absent")
    with pytest.raises(GNNSpecError, match="not declared"):
        spec.text_parameter("absent")
    with pytest.raises(GNNSpecError, match="must be an integer"):
        spec.integer_parameter("model_id")
    with pytest.raises(GNNSpecError, match="not declared in InitialParameterization"):
        declared_matrix(spec, "E_absent")
    with pytest.raises(GNNSpecError, match="not a valid numeric literal"):
        declared_matrix(
            parse_spec(
                _spec_text().replace("D_state={0.2,0.2,0.2,0.2,0.2}", "D_state={x}")
            ),
            "D_state",
        )
    with pytest.raises(GNNSpecError, match="not numeric"):
        declared_matrix(
            parse_spec(
                _spec_text().replace(
                    "D_state={0.2,0.2,0.2,0.2,0.2}", "D_state={'a','b','c','d','e'}"
                )
            ),
            "D_state",
        )
