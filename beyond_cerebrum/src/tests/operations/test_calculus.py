"""
Tests for the core calculus operations defined in 
`beyond_cerebrum.src.operations.calculus`.
"""

import pytest
from typing import Dict, List, Any, Optional

# Adjust the import path based on your project structure 
from beyond_cerebrum.src.operations.calculus import unify, project, compose, apply
# Import FeatureStructure for testing
from beyond_cerebrum.src.formalisms.structures import FeatureStructure

# --- Fixtures --- 

@pytest.fixture
def dict1() -> Dict[str, Any]:
    return {'category': 'NP', 'number': 'sg', 'person': 3}

@pytest.fixture
def dict2() -> Dict[str, Any]:
    return {'number': 'sg', 'gender': 'masc'}

@pytest.fixture
def dict3() -> Dict[str, Any]:
    return {'category': 'VP', 'tense': 'present'}

@pytest.fixture
def dict_conflict() -> Dict[str, Any]:
    return {'number': 'pl'}

@pytest.fixture
def nested_dict1() -> Dict[str, Any]:
    return {'head': {'lemma': 'dog', 'pos': 'N'}, 'spec': {'det': 'the'}}

@pytest.fixture
def nested_dict2() -> Dict[str, Any]:
    return {'head': {'pos': 'N', 'case': 'nom'}}

@pytest.fixture
def nested_conflict() -> Dict[str, Any]:
    return {'head': {'pos': 'V'}}

class DummyObject:
    def __init__(self):
        self.attr1 = 10
        self.nested = DummyNested()

class DummyNested:
    def __init__(self):
        self.attr2 = 'hello'

@pytest.fixture
def dummy_obj() -> DummyObject:
    return DummyObject()

# Add FeatureStructure fixtures
@pytest.fixture
def fs1() -> FeatureStructure[str, Any]:
    return FeatureStructure({'category': 'N', 'number': 'sg', 'agr': FeatureStructure({'person': 3})})

@pytest.fixture
def fs2() -> FeatureStructure[str, Any]:
    return FeatureStructure({'number': 'sg', 'case': 'nom', 'agr': FeatureStructure({'gender': 'm'})})

@pytest.fixture
def fs_conflict() -> FeatureStructure[str, Any]:
    return FeatureStructure({'number': 'pl'})

@pytest.fixture
def fs_nested_conflict() -> FeatureStructure[str, Any]:
    return FeatureStructure({'agr': FeatureStructure({'person': 1})})

# --- unify Tests --- 

def test_unify_dict_simple_success(dict1, dict2):
    expected = {'category': 'NP', 'number': 'sg', 'person': 3, 'gender': 'masc'}
    result = unify(dict1, dict2)
    assert result == expected
    # Check commutativity
    result_comm = unify(dict2, dict1)
    assert result_comm == expected

def test_unify_dict_simple_conflict(dict1, dict3):
    """Tests unification with conflicting values at the top level."""
    # dict1: {'category': 'NP', ...}
    # dict3: {'category': 'VP', ...}
    # Conflict at 'category', should fail
    assert unify(dict1, dict3) is None

def test_unify_dict_conflict_different_key(dict1, dict_conflict):
    """Tests unification with conflicting values at a shared key (but not category)."""
    # dict1: {'number': 'sg', ...}
    # dict_conflict: {'number': 'pl'}
    # Conflict at 'number', should fail
    assert unify(dict1, dict_conflict) is None

def test_unify_dict_identical(dict1):
    assert unify(dict1, dict1) == dict1

def test_unify_dict_empty():
    d = {'a': 1}
    assert unify(d, {}) == d
    assert unify({}, d) == d
    assert unify({}, {}) == {}

def test_unify_dict_nested_success(nested_dict1, nested_dict2):
    expected = {
        'head': {'lemma': 'dog', 'pos': 'N', 'case': 'nom'},
        'spec': {'det': 'the'}
    }
    assert unify(nested_dict1, nested_dict2) == expected
    assert unify(nested_dict2, nested_dict1) == expected

def test_unify_dict_nested_conflict(nested_dict1, nested_conflict):
    assert unify(nested_dict1, nested_conflict) is None

def test_unify_fs_simple_success(fs1):
    fs_other = FeatureStructure({'number': 'sg', 'new_feat': True})
    expected = FeatureStructure({'category': 'N', 'number': 'sg', 'new_feat': True, 'agr': FeatureStructure({'person': 3})})
    result = unify(fs1, fs_other)
    assert result == expected
    assert isinstance(result, FeatureStructure)

def test_unify_fs_nested_success(fs1, fs2):
    expected = FeatureStructure({
        'category': 'N', 
        'number': 'sg', 
        'case': 'nom', 
        'agr': FeatureStructure({'person': 3, 'gender': 'm'})
    })
    result = unify(fs1, fs2)
    assert result == expected
    assert isinstance(result, FeatureStructure)
    assert isinstance(result['agr'], FeatureStructure)

def test_unify_fs_simple_conflict(fs1, fs_conflict):
    assert unify(fs1, fs_conflict) is None

def test_unify_fs_nested_conflict(fs1, fs_nested_conflict):
    assert unify(fs1, fs_nested_conflict) is None

def test_unify_fs_with_dict_conflict(fs1):
    # calculus.unify should return None if types mismatch directly
    # unless they are both dicts (which FeatureStructure is not, despite inheritance)
    plain_dict = {'number': 'sg'} 
    assert unify(fs1, plain_dict) is None
    assert unify(plain_dict, fs1) is None
    
    # Test FS with dict containing FS
    fs_cont_dict = FeatureStructure({'data': {}})
    fs_cont_fs = FeatureStructure({'data': FeatureStructure({})}) 
    assert unify(fs_cont_dict, fs_cont_fs) is None

def test_unify_non_fs_non_dict_success():
    assert unify(1, 1) == 1
    assert unify("abc", "abc") == "abc"
    assert unify([1, 2], [1, 2]) == [1, 2]

def test_unify_non_fs_non_dict_conflict():
    assert unify(1, 2) is None
    assert unify("abc", "def") is None
    assert unify([1, 2], [1]) is None
    # Test mixing dict/FS with primitives
    assert unify({}, 1) is None
    assert unify(1, {}) is None
    assert unify(FeatureStructure({'a':1}), 1) is None
    assert unify(1, FeatureStructure({'a':1})) is None

# --- project Tests --- 

def test_project_dict_simple_key(dict1):
    assert project(dict1, 'category') == 'NP'
    assert project(dict1, 'person') == 3

def test_project_dict_missing_key(dict1):
    with pytest.raises(LookupError):
        project(dict1, 'gender')

def test_project_nested_dict_path_list(nested_dict1):
    assert project(nested_dict1, ['head', 'lemma']) == 'dog'
    assert project(nested_dict1, ['spec', 'det']) == 'the'

def test_project_nested_dict_path_list_missing(nested_dict1):
    with pytest.raises(LookupError):
        project(nested_dict1, ['head', 'case'])
    with pytest.raises(LookupError):
        project(nested_dict1, ['mod', 'adj'])

def test_project_object_attribute(dummy_obj):
    assert project(dummy_obj, 'attr1') == 10

def test_project_object_nested_attribute_list(dummy_obj):
     assert project(dummy_obj, ['nested', 'attr2']) == 'hello'

def test_project_object_nested_attribute_dot_string(dummy_obj):
     assert project(dummy_obj, 'nested.attr2') == 'hello' 

def test_project_object_missing_attribute(dummy_obj):
    with pytest.raises(LookupError):
        project(dummy_obj, 'attr3')
    with pytest.raises(LookupError):
        project(dummy_obj, ['nested', 'attr3'])
    with pytest.raises(LookupError):
        project(dummy_obj, 'nested.attr3')

def test_project_mixed_dict_object_path():
    struct = {'obj': DummyObject(), 'key': 5}
    assert project(struct, ['obj', 'attr1']) == 10
    assert project(struct, ['obj', 'nested', 'attr2']) == 'hello'
    with pytest.raises(LookupError):
        project(struct, ['obj', 'key'])

# --- compose Tests (Basic Functionality) --- 

def test_compose():
    def add_one(x): return x + 1
    def times_two(x): return x * 2

    # compose(f, g)(x) = f(g(x))
    h = compose(add_one, times_two) 
    assert h(5) == add_one(times_two(5)) # 1 + (5 * 2) = 11
    assert h.__name__ == "add_one_o_times_two"

# --- apply Tests (Basic Functionality) --- 

def test_apply():
    def add_one(x): return x + 1
    assert apply(add_one, 5) == 6 