# FORMICA Type System Specification

*(Placeholder for detailed specification)*

## 1. Introduction

This document details the formal type system used within FORMICA to represent linguistic entities and ensure operational consistency.

## 2. Design Goals

*   Expressiveness: Capture relevant distinctions in linguistic structures.
*   Rigor: Provide a sound mathematical foundation.
*   Computational Tractability: Allow for efficient type checking and manipulation.
*   Composability: Support systematic construction of complex types.

## 3. Base Types

*   `Phoneme`
*   `Morpheme`
*   `LexicalItem` (potentially with features, e.g., `POS`, `Lemma`)
*   `SyntacticLabel` (e.g., `NP`, `VP`, `DET`)
*   `SemanticConcept`
*   `SemanticRole` (e.g., `AGENT`, `PATIENT`)
*   `PragmaticMarker`
*   `ContextParameter`
*   ... (to be expanded)

## 4. Type Constructors

*   `List[T]`
*   `Tuple[T1, T2, ...]`
*   `Set[T]`
*   `Dict[K, V]`
*   `Tree[NodeType]` (representing constituency/phrase structure)
*   `Graph[NodeType, EdgeType]` (representing dependency, semantic networks)
*   `FeatureStructure[...]` (representing attribute-value matrices)
*   `Function[ArgType, ReturnType]` (representing mappings/operations)
*   `ProbDist[T]` (representing probability distributions over types)
*   ... (to be expanded)

## 5. Dependent Types (Examples)

*   `Verb[Requires: Tuple[NP[Role=Agent], NP[Role=Patient]]]`
*   `Sentence[Tense=Past]`
*   Need to define the mechanism for dependency (e.g., using constraints, feature structures within types).

## 6. Subtyping and Polymorphism

*   Define subtype relationships (e.g., `Noun` is a subtype of `LexicalItem`).
*   Specify how polymorphism is handled (e.g., operations working on `Tree[Any]`).

## 7. Integration with Python Typing

*   Leverage `typing` module where possible.
*   Define custom classes/metaclasses for complex types.
*   Consider libraries like `Pydantic` for validation. 