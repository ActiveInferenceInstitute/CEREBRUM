#!/usr/bin/env python3
"""
Base Model and Case Definitions for CEREBRUM
Defines fundamental structures for linguistic case modeling

The single source of truth for the `Model` base class lives in
`src.core.model`. This module re-exports the shared `Case` enum and provides a
thin, back-compatible `Model` subclass so that consumers (e.g.
`LinearRegressionModel`) that historically constructed the simplified model with
a `case=` keyword gain the FULL case machinery of the core model — including
`connect()`, `_prior_case`, `get_precision()`, and the `_apply_case_transformation`
hook — rather than a broken, disconnected parallel class.
"""

import logging

# Re-export Case from src.core.model for unified identity
# This ensures all imports of Case refer to the same enum class
from src.core.model import Case
from src.core.model import Model as _CoreModel

logger = logging.getLogger(__name__)


class Model(_CoreModel):
    """Back-compatible Model subclass backed by the full core model.

    Preserves the historical shallow-Model constructor signature
    ``Model(name, case=...)`` for callers such as ``LinearRegressionModel``
    while inheriting the complete case-management machinery from
    ``src.core.model.Model``.
    """

    def __init__(self, name: str = None, case: Case = Case.NOMINATIVE, **kwargs):
        # The core model accepts (name, parameters=None, ...); delegate any
        # extra keyword arguments so future core options remain usable.
        parameters = kwargs.pop("parameters", None)
        super().__init__(name=name, parameters=parameters)
        if isinstance(case, Case):
            self._case = case
        self.data_buffer = {"X": None, "y": None}
        if kwargs:
            for k, v in kwargs.items():
                setattr(self, k, v)
