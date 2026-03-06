"""
Tests for case_transformations module to raise coverage from 86% → 90%+.
Covers: ergative_absolutive alignment (L102-103), convert_message_between_cases
with GEN→DAT, INS→NOM, and LOC→NOM conversions (L264-284).
"""

import pytest
from src.core.model import Model, Case
from src.transformations.case_transformations import (
    transform_case,
    revert_case,
    apply_morphosyntactic_alignment,
    create_case_relationship,
    convert_message_between_cases,
)


@pytest.fixture
def model_a():
    return Model(name="model_a")


@pytest.fixture
def model_b():
    return Model(name="model_b")


@pytest.fixture
def model_c():
    return Model(name="model_c")


class TestMorphosyntacticAlignment:
    def test_nom_acc_with_free_energy_high(self, model_a, model_b, model_c):
        """Test that models with high free energy get ACCUSATIVE."""
        # model_c has no free_energy method → should go INSTRUMENTAL
        result = apply_morphosyntactic_alignment(
            [model_a, model_b, model_c],
            alignment_type="nominative_accusative",
            subject=model_a,
            object=model_b,
        )
        assert model_a in result["NOMINATIVE"]
        assert model_b in result["ACCUSATIVE"]
        # model_c should be INSTRUMENTAL (no free_energy callable)
        assert model_c in result["INSTRUMENTAL"]

    def test_ergative_absolutive_transitive(self, model_a, model_b, model_c):
        """Ergative-absolutive: transitive subject → INSTRUMENTAL."""
        result = apply_morphosyntactic_alignment(
            [model_a, model_b, model_c],
            alignment_type="ergative_absolutive",
            subject=model_a,
            object=model_b,
        )
        assert model_a in result["INSTRUMENTAL"]
        assert model_b in result["ACCUSATIVE"]
        assert model_c in result["LOCATIVE"]

    def test_ergative_absolutive_intransitive(self, model_a, model_c):
        """Ergative-absolutive: intransitive subject → NOMINATIVE."""
        result = apply_morphosyntactic_alignment(
            [model_a, model_c],
            alignment_type="ergative_absolutive",
            subject=model_a,
            object=None,
        )
        assert model_a in result["NOMINATIVE"]
        assert model_c in result["LOCATIVE"]

    def test_tripartite_transitive(self, model_a, model_b, model_c):
        result = apply_morphosyntactic_alignment(
            [model_a, model_b, model_c],
            alignment_type="tripartite",
            subject=model_a,
            object=model_b,
        )
        assert model_a in result["NOMINATIVE"]
        assert model_b in result["ACCUSATIVE"]
        assert model_c in result["DATIVE"]

    def test_tripartite_intransitive(self, model_a, model_c):
        result = apply_morphosyntactic_alignment(
            [model_a, model_c],
            alignment_type="tripartite",
            subject=model_a,
        )
        assert model_a in result["ABLATIVE"]
        assert model_c in result["DATIVE"]

    def test_unknown_alignment(self, model_a, model_b):
        result = apply_morphosyntactic_alignment(
            [model_a, model_b],
            alignment_type="unknown_type",
        )
        assert model_a in result["NOMINATIVE"]
        assert model_b in result["NOMINATIVE"]


class TestConvertMessageBetweenCases:
    def test_nom_to_acc_with_predictions(self):
        msg = {"predictions": [1, 2, 3], "extra": "data"}
        converted = convert_message_between_cases(
            msg, Case.NOMINATIVE, Case.ACCUSATIVE
        )
        assert "target_values" in converted
        assert converted["target_values"] == [1, 2, 3]
        assert "predictions" not in converted

    def test_gen_to_dat_with_products(self):
        msg = {"products": {"a": 1}}
        converted = convert_message_between_cases(
            msg, Case.GENITIVE, Case.DATIVE
        )
        assert "received_data" in converted
        assert converted["received_data"] == {"a": 1}
        assert "products" not in converted

    def test_ins_to_nom_with_method_predict(self):
        msg = {"method": "predict", "predictions": [10, 20]}
        converted = convert_message_between_cases(
            msg, Case.INSTRUMENTAL, Case.NOMINATIVE
        )
        assert converted["predictions"] == [10, 20]
        assert "method" not in converted

    def test_loc_to_nom_with_context(self):
        msg = {"context": {"env": "test"}}
        converted = convert_message_between_cases(
            msg, Case.LOCATIVE, Case.NOMINATIVE
        )
        assert "parameters" in converted
        assert converted["parameters"] == {"env": "test"}
        assert "context" not in converted

    def test_no_special_conversion(self):
        msg = {"data": 42}
        converted = convert_message_between_cases(
            msg, Case.ABLATIVE, Case.VOCATIVE
        )
        assert converted["case"] == "VOCATIVE"
        assert converted["converted_from"] == "ABLATIVE"
        assert converted["data"] == 42
