# Mani Case System and CEREBRUM Mapping

## Overview of Mani's Critical Endangerment and Case System

Mani is a critically endangered language spoken by likely only a few dozen elderly individuals in Sierra Leone, primarily in the Samu Chiefdom of Kambia District. As one of the most endangered languages in West Africa, Mani represents a unique opportunity for CEREBRUM to model case systems from languages on the absolute brink of extinction. The language's complex tonal system and verb-final word order provide insights into how case relationships can be encoded through prosodic and syntactic means rather than morphological marking.

Mani's case system is particularly notable for its:
- **Tonal case marking** where different tones encode different grammatical relationships
- **Verb-final syntax** that relies heavily on word order for case disambiguation
- **Classificatory verbs** that encode case relationships through verb choice
- **Serial verb constructions** that express complex case relationships
- **Reduplication patterns** that modify case semantics

## Current Vitality Status

| Location | Population | Speakers | Vitality Status |
|----------|------------|----------|-----------------|
| **Moribaya Village** | ~200 | ~20 | Critically Endangered |
| **Keychom Town** | ~500 | ~5 | Critically Endangered |
| **Other Samu Communities** | ~300 | ~10 | Critically Endangered |
| **Total Samu Chiefdom** | ~1000 | ~35 | Critically Endangered |

**Note**: All remaining speakers are elderly (60+ years old), with no children learning the language as a first language.

## Mani Case Inventory and Tonal Marking

Mani employs a sophisticated system of tonal marking that functions similarly to case marking in other languages:

### 1. **Tonal Case System**

| CEREBRUM Case | Mani Tonal Pattern | Example | Gloss |
|---------------|-------------------|---------|-------|
| **[NOM]** Nominative | High tone (H) | kɔ̀rɔ̀tɛ́ | "fish" (subject) |
| **[ACC]** Accusative | Low tone (L) | kɔ̀rɔ̀tɛ̀ | "fish" (object) |
| **[DAT]** Dative | Rising tone (LH) | kɔ̀rɔ̀tɛ̌ | "fish" (recipient) |
| **[GEN]** Genitive | Falling tone (HL) | kɔ̀rɔ̀tɛ̂ | "fish" (possessor) |
| **[INS]** Instrumental | Mid tone (M) | kɔ̀rɔ̀tɛ̄ | "fish" (instrument) |
| **[LOC]** Locative | High-low tone (HL) | kɔ̀rɔ̀tɛ́ɛ̀ | "fish" (location) |
| **[ABL]** Ablative | Low-high tone (LH) | kɔ̀rɔ̀tɛ̀ɛ́ | "fish" (source) |
| **[VOC]** Vocative | Extra high tone (HH) | kɔ̀rɔ̀tɛ́ɛ́ | "fish!" (address) |

### 2. **Classificatory Verb System**

Mani uses different verbs based on the semantic class and case relationship of the object:

| Verb Class | CEREBRUM Case | Example | Meaning |
|------------|---------------|---------|---------|
| **Animate** | [NOM/ACC] | ɔ̀ bɛ̀ kɔ̀rɔ̀tɛ́ | "He sees the fish" |
| **Inanimate** | [ACC] | ɔ̀ sɛ̀ kɔ̀rɔ̀tɛ̀ | "He takes the fish" |
| **Liquid** | [ACC] | ɔ̀ pɛ̀ kɔ̀rɔ̀tɛ̀ | "He drinks the water" |
| **Long/Thin** | [INS] | ɔ̀ tɛ̀ kɔ̀rɔ̀tɛ̄ | "He uses the stick" |

## Mapping to CEREBRUM Cases

### Direct Correspondences

| CEREBRUM Case | Mani Equivalent | Implementation Notes |
|---------------|-----------------|----------------------|
| **[NOM]** Nominative | High tone (H) marking | Models in [NOM] should implement high tonal marking |
| **[ACC]** Accusative | Low tone (L) marking | Models in [ACC] should implement low tonal marking |
| **[DAT]** Dative | Rising tone (LH) marking | Models in [DAT] should implement rising tonal marking |
| **[GEN]** Genitive | Falling tone (HL) marking | Models in [GEN] should implement falling tonal marking |
| **[INS]** Instrumental | Mid tone (M) marking | Models in [INS] should implement mid tonal marking |
| **[LOC]** Locative | High-low tone (HL) marking | Models in [LOC] should implement high-low tonal marking |
| **[ABL]** Ablative | Low-high tone (LH) marking | Models in [ABL] should implement low-high tonal marking |
| **[VOC]** Vocative | Extra high tone (HH) marking | Models in [VOC] should implement extra high tonal marking |

## Unique Features

### 1. **Tonal Case Marking**

Mani uses tone to distinguish case relationships, a rare feature among case-marking languages:

```
kɔ̀rɔ̀tɛ́ (H) "fish" as subject
kɔ̀rɔ̀tɛ̀ (L) "fish" as object
kɔ̀rɔ̀tɛ̌ (LH) "fish" as recipient
```

This provides a model for CEREBRUM to implement prosodic case marking where different intonation patterns encode different model relationships.

### 2. **Verb-Final Syntax with Case Disambiguation**

Mani's verb-final word order relies on tonal marking for case disambiguation:

```
SOV word order with tonal case marking:
ɔ̀ kɔ̀rɔ̀tɛ́ bɛ̀ "He fish sees" (He sees the fish)
ɔ̀ kɔ̀rɔ̀tɛ̀ sɛ̀ "He fish takes" (He takes the fish)
```

This suggests CEREBRUM could implement word-order-based case systems where position combined with prosodic marking determines case relationships.

### 3. **Serial Verb Constructions for Complex Cases**

Mani uses serial verb constructions to express complex case relationships:

```
ɔ̀ kɔ̀rɔ̀tɛ̀ sɛ̀ bɛ̀ "He fish takes sees" (He takes the fish and sees it)
```

This provides a model for CEREBRUM to implement complex case relationships through serial operations.

### 4. **Reduplication for Case Modification**

Mani uses reduplication to modify case semantics:

```
kɔ̀rɔ̀tɛ́ "fish" → kɔ̀rɔ̀tɛ́-kɔ̀rɔ̀tɛ́ "many fish"
```

This suggests CEREBRUM could implement case modification through morphological processes that affect quantity or intensity.

## Example Sentences with CEREBRUM Parallels

### Mani Examples with CEREBRUM Mappings

| Mani Sentence | Translation | Case Usage | CEREBRUM Parallel |
|---------------|-------------|------------|-------------------|
| **ɔ̀ kɔ̀rɔ̀tɛ́ bɛ̀** | "He sees the fish." | kɔ̀rɔ̀tɛ́ = Nominative (H tone) | He_Model[NOM:H] perceives Fish_Model |
| **ɔ̀ kɔ̀rɔ̀tɛ̀ sɛ̀** | "He takes the fish." | kɔ̀rɔ̀tɛ̀ = Accusative (L tone) | He_Model[NOM:H] acts on Fish_Model[ACC:L] |
| **ɔ̀ kɔ̀rɔ̀tɛ̌ pɛ̀** | "He gives to the fish." | kɔ̀rɔ̀tɛ̌ = Dative (LH tone) | He_Model[NOM:H] transfers to Fish_Model[DAT:LH] |
| **ɔ̀ kɔ̀rɔ̀tɛ̂ tɛ̀** | "He uses the fish's tool." | kɔ̀rɔ̀tɛ̂ = Genitive (HL tone) | He_Model[NOM:H] uses Fish_Model[GEN:HL] |
| **ɔ̀ kɔ̀rɔ̀tɛ̄ tɛ̀** | "He uses the fish as tool." | kɔ̀rɔ̀tɛ̄ = Instrumental (M tone) | He_Model[NOM:H] uses Fish_Model[INS:M] |
| **ɔ̀ kɔ̀rɔ̀tɛ́ɛ̀ lɛ̀** | "He is in the fish." | kɔ̀rɔ̀tɛ́ɛ̀ = Locative (HL tone) | He_Model[NOM:H] located in Fish_Model[LOC:HL] |
| **ɔ̀ kɔ̀rɔ̀tɛ̀ɛ́ fɛ̀** | "He comes from the fish." | kɔ̀rɔ̀tɛ̀ɛ́ = Ablative (LH tone) | He_Model[NOM:H] originates from Fish_Model[ABL:LH] |
| **kɔ̀rɔ̀tɛ́ɛ́!** | "Fish!" | kɔ̀rɔ̀tɛ́ɛ́ = Vocative (HH tone) | Direct invocation of Fish_Model[VOC:HH] |

## Extension Opportunities

### 1. **Prosodic Case Architecture**

Inspired by Mani's tonal case marking, CEREBRUM could implement a prosodic case architecture where case relationships are encoded through intonation patterns rather than morphological markers.

### 2. **Classificatory Verb Integration**

Based on Mani's classificatory verb system, CEREBRUM could implement verb choice based on the semantic class and case relationship of the object, creating more nuanced model interactions.

### 3. **Serial Operation Constructions**

Drawing from Mani's serial verb constructions, CEREBRUM could implement serial operation sequences that express complex case relationships through chained operations.

### 4. **Tonal Harmony Constraints**

Inspired by Mani's tonal system, CEREBRUM could implement tonal harmony constraints where certain prosodic properties must be consistent across related models in a case relationship.

### 5. **Endangered Language Preservation Priority**

Based on Mani's critical endangerment, CEREBRUM could prioritize documentation of endangered language case systems as a form of computational preservation.

## Implications for CEREBRUM Design

Mani's critical endangerment and unique case system offer several insights for CEREBRUM implementations:

### 1. **Prosodic Case Encoding**

CEREBRUM could implement case relationships through prosodic marking rather than morphological markers, potentially creating more natural-sounding model interactions.

### 2. **Word-Order-Based Case Systems**

Based on Mani's verb-final syntax, CEREBRUM could implement case systems that rely on word order combined with prosodic marking for case disambiguation.

### 3. **Critical Endangerment Response**

Mani's documentation provides a model for how CEREBRUM could respond to the crisis of language extinction by creating computational representations of critically endangered linguistic systems.

### 4. **Cross-Modal Case Encoding**

Mani's tonal case marking demonstrates how case relationships can be encoded through different modalities (prosody, morphology, syntax), suggesting CEREBRUM should support multiple encoding strategies.

## References

1. Childs, G. Tucker. 2003. "An Introduction to African Languages." John Benjamins Publishing.
2. Kamara, Gibrilla & Joshua Lew McDermott. 2025. "Revitalizing Mani in Sierra Leone." Endangered Languages Project Blog.
3. SILCC (Society for Indigenous Languages, Communities, and Cultures of Sierra Leone). 2023. "Mani Language Documentation Project."
4. UNESCO. 2024. "Atlas of the World's Languages in Danger." UNESCO Publishing. 