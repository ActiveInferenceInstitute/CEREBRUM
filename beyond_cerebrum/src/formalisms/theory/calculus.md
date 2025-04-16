# FORMICA Linguistic Calculus Specification

*(Placeholder for detailed specification)*

## 1. Introduction

This document defines the set of operations (the "calculus") used to manipulate and reason over the typed linguistic representations defined in `type_system.md`.

## 2. Design Goals

*   Soundness: Operations should respect the type system.
*   Expressiveness: Support a wide range of linguistic transformations and inferences.
*   Composability: Allow complex processes to be built from simpler operations.
*   Efficiency: Operations should be computationally feasible.

## 3. Primitive Operations

These are the fundamental building blocks.

*   **Unification:** `unify(struct1: T, struct2: T) -> T | Failure` (e.g., for FeatureStructures).
*   **Projection:** `project(struct: T, attribute: K) -> V` (e.g., getting a feature value).
*   **Composition:** `compose(f: Func[B, C], g: Func[A, B]) -> Func[A, C]` (standard function composition).
*   **Application:** `apply(f: Func[A, B], arg: A) -> B` (standard function application).
*   **Construction:** Operations to build complex types (e.g., `make_tree(label, children)`, `add_edge(graph, n1, n2, edge_label)`).
*   **Decomposition:** Operations to break down structures (e.g., `get_children(tree)`, `get_neighbors(graph, node)`).

## 4. Structural Transformation Operations

Operations that map between different types or levels of representation.

*   `parse(input: String) -> ProbDist[Tree[SynLabel]]`
*   `syntax_to_semantics(tree: Tree[SynLabel], lexicon: Lexicon) -> ProbDist[Graph[SemConcept, SemRole]]`
*   `morph_analyze(word: String) -> List[Morpheme]`
*   `pragmatic_enrich(sem_graph: Graph[...], context: PragContext) -> Graph[...]`
*   `linearize(sem_graph: Graph[...]) -> ProbDist[String]`

## 5. Inference Operations

Operations involving reasoning, probability, or belief updates.

*   `bayesian_update(prior: ProbDist[T], evidence: E) -> ProbDist[T]`
*   `resolve_reference(ref_expr: RefExpr, discourse: DiscourseModel) -> ProbDist[Entity]`
*   `infer_speaker_intent(utterance: U, context: C) -> ProbDist[Intent]`
*   `query_knowledge_base(query: Q, kb: KB) -> Answer`

## 6. Composition Rules

*   How operations are chained together.
*   Type checking during composition.
*   Potential use of monadic structures for sequencing probabilistic operations or handling context.

## 7. Algebraic Properties (Optional)

*   Investigate potential algebraic structures (semigroups, monoids, etc.) formed by sets of operations under composition. 