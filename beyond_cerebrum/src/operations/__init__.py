"""
FORMICA Operations Package.

Contains modules defining the core computational operations:
- Calculus: Primitive operations (unify, project, compose, apply).
- Transformations: Structural changes (parsing, generation, enrichment).
- Inference: Probabilistic reasoning (Bayesian updates, meaning inference).
"""

from .calculus import (
    unify,
    project,
    compose,
    apply
)
from .transformations import (
    parse_syntax_to_semantics,
    generate_syntax_from_semantics,
    pragmatic_enrichment
)
from .inference import (
    ProbDistribution, 
    update_belief,
    infer_meaning,
    resolve_reference_bayesian
)

__all__ = [
    # Calculus
    'unify',
    'project',
    'compose',
    'apply',
    # Transformations
    'parse_syntax_to_semantics',
    'generate_syntax_from_semantics',
    'pragmatic_enrichment',
    # Inference
    'ProbDistribution',
    'update_belief',
    'infer_meaning',
    'resolve_reference_bayesian',
] 