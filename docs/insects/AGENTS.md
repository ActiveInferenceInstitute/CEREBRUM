# Insects Documentation — Agent Context

## Purpose

This directory contains the entomological integration documentation for the CEREBRUM framework. Insect cognition serves as a biologically-inspired testbed for CEREBRUM's case-based cognitive modeling, mapping arthropod neural structures, behaviors, and collective intelligence onto the Active Inference case grammar.

## Directory Inventory

| File | Lines | Purpose |
|------|-------|---------|
| `README.md` | 200+ | Navigation hub with architecture diagram and cross-references |
| `AGENTS.md` | This file | Agent context and contribution guidelines |
| `entomological-integration.md` | 438+ | Primary integration: neural structures → CEREBRUM cases |
| `insect-instantiations.md` | 90+ | Case declension theory for arthropod models |
| `insect-cognitive-architectures.md` | 237+ | 10 comprehensive tables: orders, learning, communication, evolution |
| `insect-brain-structures.md` | 159+ | Neuroanatomical mapping with species-specific case assignments |
| `insect-brain-mapping.md` | 117+ | Circuit implementations, pathways, neurochemical modulation |
| `insect-brain-diagrams.md` | 388+ | 5 detailed anatomical mermaid circuit diagrams |
| `insect-diagrams.md` | 558+ | 8 system-level mermaid diagrams (cognition, pheromones, metamorphosis) |
| `insect-case-studies.md` | 150+ | 8 behavioral case studies with neural circuit analysis |
| `insect-cognitive-terms.md` | 212+ | Glossary: acronyms, neural terms, behavioral patterns |
| `assessment-summary.md` | 176 | Current state analysis and gap identification |
| `implementation-completion-summary.md` | 243 | Implementation overview with code cross-references |
| `implementation-roadmap.md` | 210 | Development timeline and milestones |
| `insect-improvement-plan.md` | 528 | Phased improvement plan (4 phases, 16 weeks) |

**Total**: 15 files, ~3,600+ lines of documentation

## Source Code Cross-References

### Implementation Files (`src/models/insect/`)

| File | Lines | Key Classes |
|------|-------|-------------|
| `base.py` | 727 | `InsectModel`, `InsectActiveInferenceModel`, `BehavioralState`, `SensoryInput`, `Action` |
| `neural_structures.py` | 1,107 | `MushroomBody`, `CentralComplex`, `AntennalLobe`, `OpticLobe`, `SubesophagealGanglion`, `VentralNerveCord`, `KenyonCellLayer` |
| `behaviors.py` | — | Foraging, Navigation, Communication behavioral modules |
| `species.py` | — | Species-specific model specializations |
| `cases/pheromonal.py` | — | `PheromonalCase`, `PheromoneType` (12 types), `ChemicalSignal` |
| `cases/swarm.py` | — | `SwarmCase`, `SwarmBehavior` (10 types), `SwarmMember` |
| `cases/metamorphic.py` | — | `MetamorphicCase`, `DevelopmentalStage` (5 stages) |
| `cases/caste.py` | — | `CasteCase`, `CasteType` (10 castes), `CasteProfile` |
| `cases/substrate.py` | — | `SubstrateCase`, `SubstrateType` (12 types) |
| `cases/stigmergic.py` | — | `StigmergicCase`, `StigmergicSignal` (8 types) |

### Test Files (`tests/`)

| File | Coverage Area |
|------|---------------|
| `models/test_insect_models.py` | Core model + integration tests |
| `models/test_insect_cases.py` | All 6 insect-specific case implementations |
| `models/test_insect_cases_extended.py` | Extended case interaction tests |
| `models/test_insect_behaviors.py` | Behavioral module tests |
| `models/test_insect_species.py` | Species-specific model tests |
| `models/test_insect_base_extended.py` | Extended base model tests |
| `visualization/test_insect_visualizer.py` | Visualization module tests |

## Usage Guidelines

- Review brain structure mappings in `insect-brain-structures.md` when implementing new insect models
- Follow case assignments in `entomological-integration.md` for correct CEREBRUM case usage
- Cross-reference `insect-cognitive-architectures.md` for species-specific cognitive specializations
- Use `insect-case-studies.md` as templates for new behavioral analyses
- Consult `insect-cognitive-terms.md` for consistent terminology

## Key Conventions

- **Case notation**: Always use square brackets — `[NOM]`, `[PHE]`, `[SWARM]`
- **Species names**: Italicized binomials — `*Apis mellifera*`, `*Drosophila melanogaster*`
- **Neural structures**: Capitalize proper names — Mushroom Bodies, Central Complex, Antennal Lobes
- **Diagrams**: Use mermaid syntax with consistent color coding per CEREBRUM case
- **Tables**: Include species examples and CEREBRUM case assignments in every mapping table
