# GNN model specifications

This directory holds the Generalized Notation Notation (GNN) specifications for
the generative models declared by this repository. The GNN files are the
**single source of truth for model structure**: hidden-state factors and their
cardinalities, observation modalities, the `A` (likelihood) and `B`
(transition) factor-dependency structure, control cardinalities, the runtime
axis orders, and the case-conditioned precision contract.

GNN is the Active Inference Institute specification language and implementation
core; the pinned upstream revision for this repository is recorded in
[`.github/gnn-pin.json`](../.github/gnn-pin.json).

## Files

| File | Model | Scope |
| --- | --- | --- |
| `case_conditioned_active_inference.gnn.md` | `cerebrum_active_inference_model` | The discrete surface of `src/core/active_inference.py` and its default instantiation in `src/models/insect/base.py`: one factor and one identity-likelihood modality (five states/observations by default), three controls, an identity transition per control, a uniform prior, and the eight-case precision-focus contract. |

The discrete model is dimension-generic at runtime; the specification declares
the default five-state instantiation together with the storage-order and
case-precision contracts that every instantiation keeps.

## Loader and drift guard

`src/core/gnn.py` parses the specification without depending on the upstream
`gnn` package and exposes `load_spec`, `declared_matrix`,
`structural_issues`, `parameter_issues`, `case_focus_issues`, and
`conformance_issues`. `tests/core/test_gnn_spec.py` asserts that:

- the declared factor, modality, control, and connection structure matches the
  expected dependency graph, and the declared matrices are the identity
  likelihood, the identity transition per control, and a uniform prior;
- the declared `A` and `B` values equal the parameters `InsectModel` passes to
  `ActiveInferenceModel`, after the documented axis permutations
  (`A[state, observation]`, `B[previous_state, action, next_state]`);
- the stored observation and transition orientations behave as declared
  (checked with asymmetric matrices, so a wrong axis order fails);
- each case transformation rescales exactly the matrices the specification
  declares for that case's precision focus;
- the pinned upstream parser accepts the specification under `--strict`; and
- the imported runtime comes from the clean pinned export and reports the
  recorded version.

Editing the specification, the model parameters, or the case configurations
without the other side fails the test suite. Until a release ships the shared
reader, `scripts/gnn_checkout.py` owns the local pin/pointer drift check and
the runtime-test fixture uses the same reader. Parser-acceptance and
runtime-identity tests keep their independent clean-export coverage. See
[the GNN prerequisite](../.github/GNN.md#migration-blocker) for the full cutover.

## Upstream pin

- Upstream repository:
  `https://github.com/ActiveInferenceInstitute/Generalized_Notation_Notation.git`
- Pinned tag: `v5.0.3`
- Pinned revision: `8f293d9ba810e6eda5c09842d9c7ed9ffa5f92c1` (the commit the
  `v5.0.3` tag dereferences to)
- Upstream version: `5.0.3`
- Pinned on: `2026-11-01`

Re-validation uses the shared reader through the repository's gate:

```bash
./run.sh gnn
```

The arm checks pin/pointer drift, provisions the pinned runtime, then runs
CEREBRUM's model drift, strict parser-acceptance and exported-runtime tests.
It follows the interim local-reader path, without importing `gnn.importers`.
The syntax authority is `docs/gnn/reference/gnn_syntax.md` in that checkout.
For new integrations, start from the upstream [framework integration
guide](https://github.com/ActiveInferenceInstitute/Generalized_Notation_Notation/blob/8f293d9ba810e6eda5c09842d9c7ed9ffa5f92c1/docs/gnn/integration/framework_integration_guide.md)
and the [downstream adoption
guide](https://github.com/ActiveInferenceInstitute/Generalized_Notation_Notation/blob/1f3abb56e25886dfa6ddc49c76af7b723941485a/docs/gnn/integration/downstream_adoption.md).

## Claim boundary

The specification records the implemented structure of the framework's
discrete model and its case-conditioned precision contract. It is not evidence
of behavioral or biological validation, and the case-to-role mapping remains a
framework convention.
