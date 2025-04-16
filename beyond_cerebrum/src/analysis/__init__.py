"""
FORMICA Analysis Package.

Contains modules for model analysis, interpretability, and monitoring:
- Lexical Forensics: Tracking semantic usage, grounding, and drift.
- Interpretability Hooks: Connecting FORMICA structures to backend model states.
"""

from .lexical_forensics import (
    UsageRecord,
    LexicalForensicsEngine
)
from .interpretability_hooks import (
    register_hook,
    trigger_hook,
    traceable_operation,
    map_structure_to_backend,
    explain_operation_via_backend
)

__all__ = [
    # Lexical Forensics
    'UsageRecord',
    'LexicalForensicsEngine',
    # Interpretability
    'register_hook',
    'trigger_hook',
    'traceable_operation',
    'map_structure_to_backend',
    'explain_operation_via_backend',
] 