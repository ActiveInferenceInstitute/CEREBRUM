import pytest
from typing import Any, Dict
from src.core.model import Model, Case
from src.transformations.case_transformations import (
    transform_case,
    revert_case,
    apply_morphosyntactic_alignment,
    create_case_relationship,
    convert_message_between_cases
)

# Helper Mock Model Class
class MockModel(Model):
    def __init__(self, name: str, fe: float = 0.0):
        super().__init__(name=name)
        self._fe = fe
        self._update_calls = {}

    def free_energy(self) -> float:
        return self._fe

    # Implement dummy update methods to avoid NotImplementedError during alignment tests
    def _update_nominative(self, data: Any) -> Dict[str, Any]: self._update_calls["NOM"] = data; return {}
    def _update_accusative(self, data: Any) -> Dict[str, Any]: self._update_calls["ACC"] = data; return {}
    def _update_genitive(self, data: Any) -> Dict[str, Any]: self._update_calls["GEN"] = data; return {}
    def _update_dative(self, data: Any) -> Dict[str, Any]: self._update_calls["DAT"] = data; return {}
    def _update_instrumental(self, data: Any) -> Dict[str, Any]: self._update_calls["INS"] = data; return {}
    def _update_locative(self, data: Any) -> Dict[str, Any]: self._update_calls["LOC"] = data; return {}
    def _update_ablative(self, data: Any) -> Dict[str, Any]: self._update_calls["ABL"] = data; return {}
    def _update_vocative(self, data: Any) -> Dict[str, Any]: self._update_calls["VOC"] = data; return {}

# Tests for transform_case
def test_transform_case():
    model = Model(name="Test")
    assert model.case == Case.NOMINATIVE
    transformed_model = transform_case(model, Case.ACCUSATIVE)
    assert transformed_model == model # Should be the same instance
    assert model.case == Case.ACCUSATIVE
    assert model._prior_case == Case.NOMINATIVE

# Tests for revert_case
def test_revert_case():
    model = Model(name="Test")
    model.case = Case.GENITIVE
    assert model.case == Case.GENITIVE
    assert model._prior_case == Case.NOMINATIVE

    reverted_model = revert_case(model)
    assert reverted_model == model
    assert model.case == Case.NOMINATIVE
    assert model._prior_case == Case.GENITIVE # prior case updates on revert too

def test_revert_case_no_prior():
    model = Model(name="Test")
    assert model._prior_case is None
    reverted_model = revert_case(model)
    assert reverted_model == model
    assert model.case == Case.NOMINATIVE # Should remain unchanged

# Tests for apply_morphosyntactic_alignment
def test_alignment_nominative_accusative():
    subj = MockModel(name="Subj", fe=-1.0)
    obj = MockModel(name="Obj", fe=1.0)
    other = MockModel(name="Other", fe=2.0) # High FE -> likely object
    instr = MockModel(name="Instr", fe=0.0) # Zero FE -> nominative
    models = [subj, obj, other, instr]

    result = apply_morphosyntactic_alignment(models, alignment_type="nominative_accusative", subject=subj, object=obj)

    assert subj.case == Case.NOMINATIVE
    assert obj.case == Case.ACCUSATIVE
    assert other.case == Case.ACCUSATIVE # High FE heuristic
    assert instr.case == Case.NOMINATIVE # Corrected expectation: Low/Zero FE -> Nominative

    assert sorted([m.name for m in result["NOMINATIVE"]]) == sorted([subj.name, instr.name]) # Updated result check
    assert sorted([m.name for m in result["ACCUSATIVE"]]) == sorted([obj.name, other.name])
    assert "INSTRUMENTAL" not in result or len(result["INSTRUMENTAL"]) == 0 # Should not be assigned

def test_alignment_ergative_absolutive():
    subj = MockModel(name="Subj")
    obj = MockModel(name="Obj")
    intrans_subj = MockModel(name="IntransSubj")
    other = MockModel(name="Other")

    # Transitive case (subject + object)
    models_trans = [subj, obj, other]
    result_trans = apply_morphosyntactic_alignment(models_trans, alignment_type="ergative_absolutive", subject=subj, object=obj)
    assert subj.case == Case.INSTRUMENTAL # Subject of transitive -> Instrumental (Ergative)
    assert obj.case == Case.ACCUSATIVE   # Object -> Accusative (Absolutive)
    assert other.case == Case.LOCATIVE   # Other -> Locative
    assert result_trans["INSTRUMENTAL"] == [subj]
    assert result_trans["ACCUSATIVE"] == [obj]
    assert result_trans["LOCATIVE"] == [other]

    # Intransitive case (subject only)
    models_intrans = [intrans_subj, other]
    result_intrans = apply_morphosyntactic_alignment(models_intrans, alignment_type="ergative_absolutive", subject=intrans_subj)
    assert intrans_subj.case == Case.NOMINATIVE # Subject of intransitive -> Nominative (Absolutive)
    assert other.case == Case.LOCATIVE      # Other -> Locative (reset case from previous test)
    assert result_intrans["NOMINATIVE"] == [intrans_subj]
    assert result_intrans["LOCATIVE"] == [other]

def test_alignment_tripartite():
    subj = MockModel(name="Subj")
    obj = MockModel(name="Obj")
    intrans_subj = MockModel(name="IntransSubj")
    other = MockModel(name="Other")

    # Transitive case
    models_trans = [subj, obj, other]
    result_trans = apply_morphosyntactic_alignment(models_trans, alignment_type="tripartite", subject=subj, object=obj)
    assert subj.case == Case.NOMINATIVE # Subject of transitive
    assert obj.case == Case.ACCUSATIVE  # Object
    assert other.case == Case.DATIVE    # Other
    assert result_trans["NOMINATIVE"] == [subj]
    assert result_trans["ACCUSATIVE"] == [obj]
    assert result_trans["DATIVE"] == [other]

    # Intransitive case
    models_intrans = [intrans_subj, other]
    result_intrans = apply_morphosyntactic_alignment(models_intrans, alignment_type="tripartite", subject=intrans_subj)
    assert intrans_subj.case == Case.ABLATIVE # Subject of intransitive
    assert other.case == Case.DATIVE     # Other (reset case)
    assert result_intrans["ABLATIVE"] == [intrans_subj]
    assert result_intrans["DATIVE"] == [other]

def test_alignment_default():
    m1 = MockModel("M1")
    m2 = MockModel("M2")
    models = [m1, m2]
    result = apply_morphosyntactic_alignment(models, alignment_type="unknown")
    assert m1.case == Case.NOMINATIVE
    assert m2.case == Case.NOMINATIVE
    assert result["NOMINATIVE"] == models

# Tests for create_case_relationship
@pytest.mark.parametrize(
    "rel_type, expected_source_case, expected_target_case",
    [
        ("generates", Case.NOMINATIVE, Case.DATIVE),
        ("updates", Case.NOMINATIVE, Case.ACCUSATIVE),
        ("receives_from", Case.DATIVE, Case.GENITIVE),
        ("implements", Case.INSTRUMENTAL, Case.ACCUSATIVE),
        ("contextualizes", Case.LOCATIVE, Case.NOMINATIVE),
        ("derives_from", Case.NOMINATIVE, Case.ABLATIVE),
        ("addresses", Case.NOMINATIVE, Case.VOCATIVE),
        ("unknown_relation", Case.NOMINATIVE, Case.ACCUSATIVE), # Default
    ]
)
def test_create_case_relationship(rel_type, expected_source_case, expected_target_case):
    source = Model(name="Source")
    target = Model(name="Target")

    s_res, t_res = create_case_relationship(source, target, rel_type)

    assert s_res == source
    assert t_res == target
    assert source.case == expected_source_case
    assert target.case == expected_target_case

    # Check connections
    assert (target, rel_type) in source.connections
    assert (source, f"inverse_{rel_type}") in target.connections

# Tests for convert_message_between_cases (Stub)
def test_convert_message_stub():
    # This function is currently a stub. Test that it returns the original data
    # plus the expected conversion metadata using enum names.
    message = {"data": 123, "other_key": "value"}
    source = Case.NOMINATIVE
    target = Case.ACCUSATIVE
    converted = convert_message_between_cases(message, source, target)

    # Check original data is preserved
    assert converted["data"] == 123
    assert converted["other_key"] == "value"

    # Check added metadata (using enum names)
    assert converted["converted_from"] == source.name # Expecting name, not value
    assert converted["converted_to"] == target.name   # Expecting name, not value
    assert converted["case"] == target.name           # Expecting name, not value
    assert len(converted) == len(message) + 3 # Ensure no unexpected keys added 