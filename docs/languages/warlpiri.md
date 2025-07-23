# Warlpiri Case System and CEREBRUM Mapping

## Overview of Warlpiri's Endangered Status and Case System

Warlpiri is a critically endangered Australian Aboriginal language of the Pama-Nyungan family, spoken by approximately 200-300 elderly speakers in the Northern Territory of Australia. As one of the most endangered languages in Australia, Warlpiri represents a unique opportunity for CEREBRUM to model case systems from languages on the brink of extinction. The language's complex ergative-absolutive alignment, extensive auxiliary verb system, and free word order provide insights into how case relationships can be encoded through auxiliary marking rather than morphological case.

Warlpiri's case system is particularly notable for its:
- **Ergative-absolutive alignment** with auxiliary verb agreement
- **Free word order** with case marking through auxiliary verbs
- **Extensive auxiliary verb system** that encodes case relationships
- **Noun class system** with four classes (masculine, feminine, vegetable, neuter)
- **Reduplication patterns** that modify case semantics
- **Complex kinship terminology** that affects case marking

## Current Vitality Status

| Location | Population | Speakers | Vitality Status |
|----------|------------|----------|-----------------|
| **Yuendumu** | ~800 | ~100-150 | Critically Endangered |
| **Lajamanu** | ~600 | ~50-75 | Critically Endangered |
| **Willowra** | ~400 | ~30-50 | Critically Endangered |
| **Nyirrpi** | ~200 | ~20-25 | Critically Endangered |
| **Total Warlpiri Area** | ~2000 | ~200-300 | Critically Endangered |

**Note**: All remaining speakers are elderly (60+ years old), with very few children learning the language as a first language. The language is being rapidly replaced by English and Aboriginal English.

## Warlpiri Case Inventory and Auxiliary System

Warlpiri employs a sophisticated auxiliary verb system that functions similarly to case marking in other languages:

### 1. **Core Case System**

| CEREBRUM Case | Warlpiri Case | Warlpiri Term | Auxiliary Marking | Example |
|---------------|---------------|---------------|-------------------|---------|
| **[NOM]** Nominative | Absolutive | -Ø | ka (present) | kurdu "child" |
| **[ACC]** Accusative | Absolutive | -Ø | ka (present) | kurdu "child" (object) |
| **[GEN]** Genitive | Genitive | -kurlangu | ka (present) | kurdu-kurlangu "child's" |
| **[DAT]** Dative | Dative | -ku | ka (present) | kurdu-ku "to the child" |
| **[INS]** Instrumental | Instrumental | -ngku | ka (present) | kurdu-ngku "with the child" |
| **[LOC]** Locative | Locative | -ngka | ka (present) | kurdu-ngka "at the child" |
| **[ABL]** Ablative | Ablative | -ngurlu | ka (present) | kurdu-ngurlu "from the child" |
| **[VOC]** Vocative | Vocative | -ya | ka (present) | kurdu-ya "O child!" |

### 2. **Ergative-Absolutive Alignment with Auxiliary Agreement**

Warlpiri distinguishes between ergative (agent) and absolutive (patient/subject) marking through auxiliary verbs:

```
Ergative marking (agent of transitive verb):
kurdu-ngku ka maliki nya-nyi "The child sees the dog" (child-ERG AUX dog see-NPST)

Absolutive marking (patient of transitive verb, subject of intransitive):
kurdu ka wangka-mi "The child speaks" (child-ABS AUX speak-NPST)
maliki ka kurdu nya-nyi "The dog sees the child" (dog-ABS AUX child see-NPST)
```

### 3. **Auxiliary Verb System**

Warlpiri uses auxiliary verbs to encode case relationships and agreement:

| Auxiliary | Function | Case Agreement | Example |
|-----------|----------|----------------|---------|
| **ka** | Present tense | Subject agreement | kurdu ka wangka-mi |
| **kapu** | Future tense | Subject agreement | kurdu kapu wangka-mi |
| **kala** | Past tense | Subject agreement | kurdu kala wangka-mi |
| **kaji** | Conditional | Subject agreement | kurdu kaji wangka-mi |
| **kula** | Negative | Subject agreement | kurdu kula wangka-mi |

## Mapping to CEREBRUM Cases

### Direct Correspondences

| CEREBRUM Case | Warlpiri Equivalent | Implementation Notes |
|---------------|-------------------|----------------------|
| **[NOM]** Nominative | Absolutive case (-Ø) + auxiliary | Models in [NOM] should implement absolutive marking with auxiliary agreement |
| **[ACC]** Accusative | Absolutive case (-Ø) + auxiliary | Models in [ACC] should implement absolutive marking with auxiliary agreement |
| **[GEN]** Genitive | Genitive case (-kurlangu) + auxiliary | Models in [GEN] should implement genitive marking with auxiliary agreement |
| **[DAT]** Dative | Dative case (-ku) + auxiliary | Models in [DAT] should implement dative marking with auxiliary agreement |
| **[INS]** Instrumental | Instrumental case (-ngku) + auxiliary | Models in [INS] should implement instrumental marking with auxiliary agreement |
| **[LOC]** Locative | Locative case (-ngka) + auxiliary | Models in [LOC] should implement locative marking with auxiliary agreement |
| **[ABL]** Ablative | Ablative case (-ngurlu) + auxiliary | Models in [ABL] should implement ablative marking with auxiliary agreement |
| **[VOC]** Vocative | Vocative case (-ya) + auxiliary | Models in [VOC] should implement vocative marking with auxiliary agreement |

### Ergative-Absolutive Extension

| CEREBRUM Extension | Warlpiri Implementation | CEREBRUM Value |
|-------------------|----------------------|----------------|
| **[ERG]** Ergative | Ergative case (-ngku) + auxiliary | Agent of transitive operations |
| **[ABS]** Absolutive | Absolutive case (-Ø) + auxiliary | Patient of transitive operations, subject of intransitive operations |

## Unique Features

### 1. **Auxiliary-Based Case Marking**

Warlpiri's auxiliary verb system provides a model for CEREBRUM to implement case marking through auxiliary agreement rather than morphological marking:

```
Transitive operation with auxiliary agreement:
Agent_Model[ERG] + AUX + Patient_Model[ABS] + Operation

Intransitive operation with auxiliary agreement:
Subject_Model[ABS] + AUX + Operation
```

This suggests CEREBRUM could implement auxiliary-based case marking where case relationships are encoded through auxiliary verbs rather than morphological markers.

### 2. **Free Word Order with Case Stability**

Warlpiri's free word order with stable case marking provides a model for CEREBRUM to implement flexible word order with consistent case relationships:

```
SOV: kurdu-ngku ka maliki nya-nyi "The child sees the dog"
OSV: maliki ka kurdu-ngku nya-nyi "The dog the child sees"
VSO: nya-nyi ka kurdu-ngku maliki "Sees the child the dog"
```

This suggests CEREBRUM could implement case systems that maintain relationships regardless of word order.

### 3. **Noun Class Interaction with Case**

Warlpiri's four noun classes interact with case marking:

```
Masculine: wati-ngku (man-ERG)
Feminine: karnta-ngku (woman-ERG)
Vegetable: yarla-ngku (yam-ERG)
Neuter: kuyu-ngku (meat-ERG)
```

This provides a model for CEREBRUM to implement case marking that varies based on the semantic class of the model.

### 4. **Complex Kinship Terminology**

Warlpiri's complex kinship system affects case marking:

```
Direct kinship: ngaju-ku (I-DAT) "to me"
Oblique kinship: ngaju-kurlangu (I-GEN) "mine"
```

This suggests CEREBRUM could implement specialized case marking for kinship relationships.

### 5. **Reduplication for Case Modification**

Warlpiri uses reduplication to modify case semantics:

```
kurdu "child" → kurdu-kurdu "children"
kurdu-ngku "with the child" → kurdu-kurdu-ngku "with the children"
```

This provides a model for CEREBRUM to implement case modification through morphological processes.

## Example Sentences with CEREBRUM Parallels

### Warlpiri Examples with CEREBRUM Mappings

| Warlpiri Sentence | Translation | Case Usage | CEREBRUM Parallel |
|-------------------|-------------|------------|-------------------|
| **kurdu-ngku ka maliki nya-nyi** | "The child sees the dog." | kurdu-ngku = Ergative (agent) | Child_Model[ERG] + AUX + Dog_Model[ABS] + seeing operation |
| **kurdu ka wangka-mi** | "The child speaks." | kurdu = Absolutive (subject) | Child_Model[ABS] + AUX + speaking operation |
| **maliki ka kurdu nya-nyi** | "The dog sees the child." | maliki = Absolutive (patient) | Dog_Model[ABS] + AUX + Child_Model + seeing operation |
| **kurdu-ku ka maliki yungu** | "The child gives the dog." | kurdu-ku = Dative (recipient) | Child_Model[DAT] + AUX + Dog_Model + giving operation |
| **kurdu-ngku ka maliki-ngku** | "The child uses the dog." | maliki-ngku = Instrumental (tool) | Child_Model[ERG] + AUX + Dog_Model[INS] |
| **kurdu-ngka ka maliki** | "The dog is at the child." | kurdu-ngka = Locative (location) | Child_Model[LOC] + AUX + Dog_Model |
| **kurdu-ngurlu ka maliki** | "The dog comes from the child." | kurdu-ngurlu = Ablative (source) | Child_Model[ABL] + AUX + Dog_Model |
| **kurdu-ya!** | "O child!" | kurdu-ya = Vocative (address) | Direct invocation of Child_Model[VOC] |

### Auxiliary Variation Examples

| Warlpiri Sentence | Translation | Auxiliary | CEREBRUM Parallel |
|-------------------|-------------|-----------|-------------------|
| **kurdu ka wangka-mi** | "The child speaks." | ka (present) | Child_Model[ABS] + PRES_AUX + speaking |
| **kurdu kapu wangka-mi** | "The child will speak." | kapu (future) | Child_Model[ABS] + FUT_AUX + speaking |
| **kurdu kala wangka-mi** | "The child spoke." | kala (past) | Child_Model[ABS] + PAST_AUX + speaking |
| **kurdu kaji wangka-mi** | "If the child speaks." | kaji (conditional) | Child_Model[ABS] + COND_AUX + speaking |
| **kurdu kula wangka-mi** | "The child doesn't speak." | kula (negative) | Child_Model[ABS] + NEG_AUX + speaking |

## Extension Opportunities

### 1. **Auxiliary-Based Case Architecture**

Inspired by Warlpiri's auxiliary system, CEREBRUM could implement an auxiliary-based case architecture where:
- **Auxiliary verbs**: Encode case relationships and agreement
- **Free word order**: Maintain case relationships regardless of position
- **Tense/aspect marking**: Integrated with case marking through auxiliaries

### 2. **Flexible Word Order with Case Stability**

Based on Warlpiri's free word order, CEREBRUM could implement case systems that maintain relationships regardless of model ordering:
- **SOV ordering**: Standard subject-object-verb
- **OSV ordering**: Object-subject-verb
- **VSO ordering**: Verb-subject-object
- **Case stability**: Relationships maintained across all orderings

### 3. **Noun Class-Case Interaction**

Drawing from Warlpiri's noun class system, CEREBRUM could implement case marking that varies based on the semantic class of the model:
- **Masculine models**: Special case marking patterns
- **Feminine models**: Different case relationships
- **Vegetable models**: Unique case semantics
- **Neuter models**: Standard case marking

### 4. **Kinship Case Specialization**

Inspired by Warlpiri's kinship system, CEREBRUM could implement specialized case marking for relationship types:
- **Direct relationships**: Special case marking
- **Oblique relationships**: Different case patterns
- **Kinship hierarchies**: Complex case relationships

### 5. **Auxiliary Tense-Aspect Integration**

Based on Warlpiri's auxiliary system, CEREBRUM could implement case marking that integrates with tense and aspect:
- **Present case marking**: Current relationships
- **Future case marking**: Anticipated relationships
- **Past case marking**: Historical relationships
- **Conditional case marking**: Hypothetical relationships

## Implications for CEREBRUM Design

Warlpiri's endangered status and unique case system offer several insights for CEREBRUM implementations:

### 1. **Auxiliary-Based Case System**

CEREBRUM could implement an auxiliary-based case system that uses auxiliary verbs to encode case relationships rather than morphological markers, providing more flexible and expressive case marking.

### 2. **Word Order Flexibility**

Based on Warlpiri's free word order, CEREBRUM could implement case systems that maintain relationships regardless of model ordering, creating more flexible computational architectures.

### 3. **Critical Endangerment Response**

Warlpiri's documentation provides a model for how CEREBRUM could respond to the crisis of language extinction by creating computational representations of critically endangered linguistic systems.

### 4. **Cross-Linguistic Case Variation**

Warlpiri's auxiliary-based system demonstrates the diversity of case systems across languages, suggesting that CEREBRUM should be flexible enough to accommodate various case encoding strategies.

## References

1. Hale, Kenneth L. 1983. "Warlpiri and the Grammar of Non-Configurational Languages." Natural Language & Linguistic Theory 1(1): 5-47.
2. Laughren, Mary. 1989. "The Configurationality Parameter and Warlpiri." Configurationality: The Typology of Asymmetries, ed. by László Marácz and Pieter Muysken, 319-353. Dordrecht: Foris.
3. Simpson, Jane. 1991. "Warlpiri Morpho-Syntax: A Lexicalist Approach." Dordrecht: Kluwer Academic Publishers.
4. Nash, David. 1986. "Topics in Warlpiri Grammar." New York: Garland Publishing.
5. UNESCO. 2024. "Atlas of the World's Languages in Danger." UNESCO Publishing.
6. Endangered Languages Project. 2024. "Warlpiri Language Documentation." 