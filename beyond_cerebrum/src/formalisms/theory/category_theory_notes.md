# FORMICA Category Theory Notes

*(Placeholder for exploratory notes)*

## 1. Motivation

Explore how concepts from category theory might provide a unifying framework for FORMICA, particularly regarding compositionality, structure preservation, and mappings between linguistic levels (syntax, semantics, pragmatics).

Refs: Lambek, Preller, Coecke, Sadrzadeh, Clark, ...

## 2. Potential Applications

*   **Compositionality:** Model semantic composition using functors from a category of syntactic structures (e.g., based on pregroup grammar or dependency structures) to a category of semantic representations (e.g., vector spaces, logical forms, relational structures).
*   **Context Dependency:** Model context using monads or indexed categories.
*   **Transformations as Morphisms/Functors:** Represent parsing, generation, or translation as structure-preserving maps.
*   **Unification of Levels:** Define a single overarching category or interconnected categories encompassing different linguistic representations.
*   **Probabilistic Categories:** Explore integration with probabilistic models (e.g., Markov categories).

## 3. Key Concepts to Explore

*   **Categories:** Syntax (Syn), Semantics (Sem), Pragmatics (Prag), ...
*   **Objects:** Specific types/structures within each category.
*   **Morphisms:** Grammatical derivations, semantic compositions, pragmatic inferences.
*   **Functors:** Syntax-Semantics mapping, Semantics-Pragmatics mapping.
*   **Monads:** Handling context, ambiguity, or probabilistic effects.
*   **Adjunctions:** Relating different levels of representation (e.g., parsing/linearization).
*   **String Diagrams:** Visualizing compositional processes.

## 4. Challenges

*   Complexity of defining appropriate categories and morphisms for rich linguistic phenomena.
*   Bridging the gap between abstract category theory and concrete implementation.
*   Scalability and computational cost.

## 5. Initial Ideas

*   Represent sentence meaning composition using tensor products in a compact closed category (DisCoCat model).
*   Model dependency grammar using categories.
*   Use functors to map between different levels of analysis (e.g., phonology to morphology). 