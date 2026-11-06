# GNN conformance

`gnn conformance (pinned)` is defined in `.github/workflows/ci.yml` and reads
`.github/gnn-pin.json`, which pins
`ActiveInferenceInstitute/Generalized_Notation_Notation` at `v5.0.3`
(`8f293d9ba810e6eda5c09842d9c7ed9ffa5f92c1`).

The job verifies the pin and installed parser, then runs
`tests/core/test_gnn_spec.py`. That test validates the specification selected by
`.github/gnn-pin.json`, `models/case_conditioned_active_inference.gnn.md`, with
the pinned upstream `gnn.cli validate --strict`, and checks the declared model
against the implementation. It does not scan every repository `*.gnn.md`; this
is a pin and model conformance baseline for the selected specification, not a
claim that every sketch is covered.
