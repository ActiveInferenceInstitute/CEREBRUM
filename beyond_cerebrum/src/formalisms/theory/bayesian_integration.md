# FORMICA Bayesian Integration Specification

*(Placeholder for detailed specification)*

## 1. Introduction

This document describes how Bayesian probability and inference are integrated into the FORMICA framework's typed structures and calculus to handle uncertainty, learning, and context-dependent interpretation.

## 2. Design Goals

*   Principled handling of ambiguity and uncertainty.
*   Integration with formal linguistic structures (types, calculus).
*   Support for online learning and adaptation.
*   Cognitive plausibility (where applicable).

## 3. Probabilistic Representations

*   **Probabilistic Types:** Define `ProbDist[T]` as a fundamental type constructor, representing a probability distribution over structures of type `T`.
    *   How are distributions represented? (e.g., explicit probabilities for discrete types, parameters for continuous types, samples).
*   **Probabilistic Grammars/Lexicons:** Representing probabilities associated with grammar rules, lexical entries, or feature structures.
*   **Probabilistic World/Context Models:** Representing beliefs about the world state or discourse context as probability distributions.

## 4. Bayesian Inference as Calculus Operations

Key linguistic tasks framed as Bayesian inference:

*   **Parsing:** `P(ParseTree | InputString, Grammar, Lexicon)`
    *   Implemented via operations like `infer_MAP(ProbDist[ParseTree])` or sampling operations.
*   **Interpretation:** `P(Meaning | Structure, Context, KnowledgeBase)`
    *   May involve composing multiple Bayesian updates.
*   **Reference Resolution:** `P(Referent | ReferringExpression, DiscourseModel)`
*   **Learning:** `P(Grammar | Data)`, `P(Lexicon | Data)`
    *   Requires operations for parameter updates (e.g., MCMC, variational inference adapted to linguistic structures).

## 5. Integration Mechanisms

*   **Operations on `ProbDist[T]`:** Define how calculus operations (unify, compose, apply) work when applied to or returning probability distributions.
    *   *Example:* Unifying two `ProbDist[FeatureStructure]` might involve combining evidence or finding the most likely joint structure.
*   **Sampling:** Operations to draw samples from `ProbDist[T]`.
*   **Marginalization/Conditioning:** Operations corresponding to standard probability manipulations.

## 6. Backend Integration

*   How are Bayesian operations mapped to computational backends?
    *   Libraries like `NumPyro`, `PyMC`, `Stan` via adapters.
    *   Custom inference algorithms implemented directly.
    *   Integration with Bayesian Neural Networks.

## 7. Challenges

*   Computational cost of inference, especially with complex structures.
*   Defining appropriate priors for linguistic knowledge.
*   Integrating symbolic structure with continuous probability distributions.
*   Structure learning in probabilistic spaces. 