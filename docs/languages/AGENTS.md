# Languages Documentation Agent Context

## Purpose

This directory contains **comprehensive linguistic analyses** of CEREBRUM's case system across **100+ natural, constructed, and programming languages**. These documents provide the theoretical and practical foundation for understanding how grammatical case systems from diverse linguistic traditions map to CEREBRUM's computational framework.

## Directory Structure

```
docs/languages/
├── AGENTS.md              # This file - agent context documentation
├── README.md              # Complete language index with links
├── computer/              # 35+ programming language analyses
│   ├── README.md          # Programming language index
│   ├── assembly.md        # Low-level systems languages
│   ├── haskell.md         # Functional programming paradigm
│   ├── lisp.md            # S-expression and macro systems
│   ├── python.md          # Dynamic typing and protocols
│   ├── rust.md            # Ownership and borrowing
│   ├── ...                # Additional languages
│   └── zig.md             # Modern systems programming
├── finnish.md             # Comprehensive 15-case system (flagship)
├── sanskrit.md            # 8-case classical system
├── hungarian.md           # 18+ case rich agglutinative system
├── japanese.md            # Particle-based case marking
├── latin.md               # Classical case paradigms
├── ...                    # 60+ additional natural languages
└── [endangered]/          # Critically endangered language files
    ├── kawahiva.md        # Brazilian isolate
    ├── mani.md            # Nigerian endangered
    ├── xoo.md             # Khoisan click language
    └── warlpiri.md        # Australian Aboriginal
```

## Primary Reference Files

### Flagship Natural Language Analyses

| File | Language Family | Case Count | Key Features |
|------|----------------|------------|--------------|
| [finnish.md](finnish.md) | Uralic | 15 cases | Most comprehensive; includes figures, implementation code |
| [sanskrit.md](sanskrit.md) | Indo-European | 8 cases | Classical reference; category theory connections |
| [hungarian.md](hungarian.md) | Uralic | 18+ cases | Rich agglutination; Markov analysis available |
| [japanese.md](japanese.md) | Japonic | Particle system | Topic/subject distinction; Active Inference integration |
| [hindi.md](hindi.md) | Indo-Aryan | Split ergativity | Differential object marking |
| [basque.md](basque.md) | Isolate | Ergative-absolutive | Unique European ergative system |
| [georgian.md](georgian.md) | Kartvelian | 7 cases | Split ergativity; complex verb agreement |

### Programming Language Categories

| Category | Languages | Key Patterns |
|----------|-----------|--------------|
| **Functional** | Haskell, OCaml, F#, Clojure, Elixir | Type-level case constraints, ADTs |
| **Systems** | Rust, C++, Zig, Nim, Odin | Ownership as case, memory safety |
| **ML/AI** | Python, Julia, R | Dynamic case transformations |
| **Logic** | Prolog, Lean | Unification-based case reasoning |
| **OOP** | Java, C#, Kotlin, Swift | Interface-based case abstractions |
| **Scripting** | JavaScript, TypeScript, Ruby, Perl | Prototype-based case systems |

## Design Principles

### 1. Mapping Conventions

All language files follow a consistent mapping to CEREBRUM's eight core cases:

| CEREBRUM Case | Universal Semantic Role | Primary Linguistic Correspondent |
|---------------|------------------------|----------------------------------|
| **[NOM]** Nominative | Active agent | Subject of intransitive verbs |
| **[ACC]** Accusative | Patient/target | Direct object of transitive verbs |
| **[DAT]** Dative | Recipient/beneficiary | Indirect object; goal |
| **[GEN]** Genitive | Source/possessor | Possession; partitive; source of output |
| **[INS]** Instrumental | Tool/means | Instrument; method; mechanism |
| **[ABL]** Ablative | Origin/separation | Motion from; source of input |
| **[LOC]** Locative | Location/context | Spatial/temporal context; environment |
| **[VOC]** Vocative | Direct address | Invocation; direct calling |

### 2. Document Structure Standards

Each language document should contain:

1. **Overview** - Language family, typological features, speaker demographics
2. **Case Inventory** - Complete case listing with forms and functions
3. **CEREBRUM Mapping Table** - Direct correspondence strengths
4. **Implementation Examples** - Code demonstrating case transformations
5. **Unique Features** - Language-specific insights for CEREBRUM
6. **Active Inference Connections** - Precision, belief updating parallels
7. **References** - Academic sources and documentation

### 3. Quality Tiers

| Tier | Line Count | Features | Examples |
|------|------------|----------|----------|
| **Flagship** | 800+ lines | Full implementation code, diagrams, Active Inference | Finnish, Sanskrit |
| **Comprehensive** | 400-800 lines | Complete mapping, code examples | Hindi, Japanese, Hungarian |
| **Standard** | 200-400 lines | Core mapping, basic examples | Most languages |
| **Stub** | <200 lines | Minimal mapping | New/rare languages |

## Usage Guidelines

### For Case Transformation Design

When implementing new case transformations:

1. **Reference similar alignment systems**:
   - Nominative-accusative → Most European languages
   - Ergative-absolutive → Basque, Georgian, Warlpiri
   - Active-stative → Some Native American languages

2. **Consider morphological type**:
   - Agglutinative (Finnish, Hungarian) → Clean case stacking
   - Fusional (Latin, Russian) → Case-number-gender fusion
   - Isolating (Chinese, Vietnamese) → Particle-based marking

### For Programming Language Integration

When mapping programming constructs to cases:

1. **Ownership models** (Rust, Nim) → ABL/ACC transitions
2. **Type class constraints** (Haskell, Scala) → INS mechanisms
3. **Monadic contexts** (Haskell, F#) → LOC environments
4. **Pattern matching** → VOC dispatch
5. **Return values** → GEN derivation

### For Endangered Language Documentation

These files serve a dual purpose:

1. **CEREBRUM research** - Novel case system patterns
2. **Language preservation** - Computational documentation of endangered systems

## Key Insights by Language Family

### Uralic Languages (Finnish, Hungarian, Estonian)

- **Rich case inventories** (15-18+ cases)
- **Clean agglutination** - One suffix per grammatical function
- **Vowel harmony** - Morphophonological constraints on case marking
- **Locative case families** - Interior/exterior/surface distinctions

### Indo-European Languages

- **Fusional morphology** - Case often fused with number/gender
- **Case syncretism** - Multiple cases sharing forms
- **Historical case loss** - Many modern languages retain only remnants

### Polysynthetic Languages (Navajo, Inuktitut)

- **Verb-internal case marking** - Arguments incorporated into verb
- **Complex agreement systems** - Multiple cross-references
- **Noun incorporation** - Objects merged with verbs

### Isolating Languages (Chinese, Vietnamese)

- **No morphological case** - Word order and particles
- **Topic-prominence** - Information structure over case
- **Serial verb constructions** - Case-like role from verb position

## Integration with CEREBRUM Core

These language analyses inform:

1. **`src/core/transformations.py`** - Case transformation logic
2. **`src/models/case_entity.py`** - Entity case representation
3. **`src/active_inference/`** - Precision and belief update mechanisms
4. **`tests/`** - Validation against linguistic expectations

## Contributing

To add or improve a language file:

1. Follow the document structure standards above
2. Include academic references (ISO 639-3 codes for endangered languages)
3. Provide implementation examples in Python
4. Map to all 8 CEREBRUM cases with correspondence strengths
5. Note unique features that could inform CEREBRUM extensions

## References

1. Croft, W. (2003). *Typology and Universals*. Cambridge University Press.
2. Blake, B. J. (2001). *Case*. Cambridge University Press.
3. Comrie, B. (1989). *Language Universals and Linguistic Typology*. University of Chicago Press.
4. Friston, K. (2010). The free-energy principle. *Nature Reviews Neuroscience*.
5. UNESCO Atlas of the World's Languages in Danger (2024).
