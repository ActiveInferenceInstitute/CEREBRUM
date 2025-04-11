# Puyuma Case System and CEREBRUM Mapping

## Overview of Puyuma's Approach to Grammatical Relations

Puyuma, an Austronesian language spoken in southeastern Taiwan, employs a complex voice-marking system and case-marking particles to indicate grammatical relationships. As part of the Formosan language group, Puyuma exemplifies the Philippine-type voice system where verbal morphology indicates which argument is in focus, while nominal case markers show the syntactic and semantic roles of participants. This approach provides a fascinating model for CEREBRUM implementations, illustrating how grammatical relationships can be encoded through both verbal morphology and nominal marking.

In Puyuma, the focused argument (traditionally called the "pivot" or "subject") is marked with specific particles, while non-focused arguments receive different markers based on their semantic roles. The verbal morphology indicates which argument is in focus, creating a system where various semantic roles can be promoted to syntactic prominence. This voice-focus system offers an alternative conceptual model for CEREBRUM where operational prominence can dynamically shift across participating models without changing their semantic roles.

## Functional Equivalents to Cases in Puyuma

Puyuma employs the following markers and structures to express relationships that would be handled by cases in other languages:

1. **Voice/Focus Affixes on Verbs** - Indicate which argument is in focus
   - **M-/Ma-** Actor Voice (AV): Focuses on the agent/actor
   - **-aw** Patient Voice (PV): Focuses on the patient/undergoer
   - **-ay** Locative Voice (LV): Focuses on the location or direction
   - **i-** Circumstantial Voice (CV): Focuses on instruments, beneficiaries, or other obliques

2. **Case Markers** - Function like case markers for noun phrases
   - **i/na** - Nominative case markers for the focused argument (common/personal)
   - **kana/kana** - Genitive case markers for possessors and agents in non-actor voice (common/personal)
   - **dra/kana** - Oblique case markers for non-focused, non-agent arguments (common/personal)

3. **Word Order** - Typically Verb-Subject-Object, but flexible based on discourse

4. **Transitivity Marking** - Verbal morphology also marks transitivity distinctions

## Mapping to CEREBRUM Cases

Puyuma's voice system and case markers can be mapped to CEREBRUM's eight standard cases as follows:

| CEREBRUM Case | Puyuma Equivalent | Implementation Notes |
|---------------|------------------|----------------------|
| **[NOM]** Nominative | Focused argument (i/na) + Actor Voice verb (M-/Ma-) | Models in [NOM] should implement focus marking with actor voice verbal operation |
| **[ACC]** Accusative | Focused argument (i/na) + Patient Voice verb (-aw) | Models in [ACC] should implement focus marking with patient voice verbal operation |
| **[GEN]** Genitive | Non-focused agent in non-actor voice clauses (kana/kana) or possessive relation | Models in [GEN] should implement non-focused agent relation or possessive relation |
| **[DAT]** Dative | Focused argument (i/na) + Circumstantial Voice verb (i-) for beneficiary or oblique marking (dra/kana) | Models in [DAT] should implement focus or oblique marking based on prominence |
| **[INS]** Instrumental | Focused argument (i/na) + Circumstantial Voice verb (i-) for instrument or oblique marking (dra/kana) | Models in [INS] should implement focus or oblique marking based on prominence |
| **[LOC]** Locative | Focused argument (i/na) + Locative Voice verb (-ay) or oblique marking (dra/kana) | Models in [LOC] should implement focus or oblique marking based on prominence |
| **[ABL]** Ablative | Oblique marking (dra/kana) with appropriate directional verbs | Models in [ABL] should implement oblique marker with source semantics |
| **[VOC]** Vocative | Direct address, no specific marker in documented sources | Models in [VOC] should implement direct address patterns |

## Unique Features

Puyuma's grammatical system offers several unique features relevant to CEREBRUM:

1. **Complex Voice System**
   
   In Puyuma, the verbal morphology determines which argument receives syntactic prominence, creating a system where grammatical relations are marked primarily through verb forms in coordination with case markers. This provides a model for CEREBRUM implementations where operations can dynamically reassign prominence to different participating models.

   ```
   M-ekan na walak kana kuraw.
   AV-eat NOM child GEN fish
   "The child eats fish." (Actor in focus)
   
   Ka-ekan-aw kana walak na kuraw.
   KA-eat-PV GEN child NOM fish
   "The child eats the fish." (Patient in focus)
   ```

   Both sentences describe the same event but differ in which participant is syntactically prominent.

2. **Four-Voice System**

   Puyuma employs a four-voice system (Actor, Patient, Locative, Circumstantial) that allows different semantic roles to be brought into focus. This provides a model for CEREBRUM to implement multiple perspective views on operations with different focal points.

   ```
   Ka-ekan-ay kana walak na palridring.
   KA-eat-LV GEN child NOM plate
   "The child eats on the plate." (Location in focus)
   
   I-ekan kana walak na patraka.
   CV-eat GEN child NOM meat.offering
   "The child eats the meat offering (for some purpose)." (Circumstance in focus)
   ```

3. **Common vs. Personal Noun Distinction**

   Puyuma maintains a systematic distinction between common nouns and personal names in its case-marking system. This provides a model for CEREBRUM to implement entity-type distinctions in its relationship marking.

4. **Realis/Irrealis Distinction**

   Puyuma verbs encode a realis/irrealis distinction that indicates whether an action has occurred or is hypothetical/future. This provides a model for CEREBRUM to implement modal distinctions in its operations.

   ```
   M-ekan na walak.
   AV-eat NOM child
   "The child eats." (realis/actual)
   
   A-ekan na walak.
   IRR-eat NOM child
   "The child will eat." (irrealis/potential)
   ```

## Extension Opportunities

Puyuma's voice and case system suggests several extension opportunities for CEREBRUM:

1. **Multi-Perspective Operation Architecture**
   
   Inspired by Puyuma's four-voice system, CEREBRUM could implement a multi-perspective operation architecture where the same basic operation can be viewed from different participant perspectives without altering the underlying relationship.

2. **Modality-Aware Operations**
   
   Based on Puyuma's realis/irrealis distinction, CEREBRUM could implement modality-aware operations that explicitly represent the actualization status of operations (actual vs. potential/hypothetical).

3. **Entity Type-Sensitive Marking**
   
   Drawing from Puyuma's distinction between common and personal nouns, CEREBRUM could implement entity type-sensitive marking where relationship indicators adapt based on the nature of the related entities.

4. **Circumstantial Voice Extension**
   
   Inspired by Puyuma's circumstantial voice, CEREBRUM could implement a specialized relationship marker for miscellaneous circumstantial roles that don't fit neatly into more specific categories.

5. **Transitivity-Sensitive Operations**
   
   Based on Puyuma's transitivity marking, CEREBRUM could implement operations that explicitly encode transitivity distinctions in their interface specifications.

## Example Sentences

Below are example sentences in Puyuma with their CEREBRUM parallels:

1. **Actor Voice [NOM]**

   **Puyuma:** M-ekan na walak kana kuraw.
   AV-eat NOM child GEN fish
   "The child eats fish." (Actor in focus)
   
   **CEREBRUM:** Child_Model[NOM:AV] performs eating operation on Fish_Model[GEN].

2. **Patient Voice [ACC]**

   **Puyuma:** Ka-ekan-aw kana walak na kuraw.
   KA-eat-PV GEN child NOM fish
   "The child eats the fish." (Patient in focus)
   
   **CEREBRUM:** Fish_Model[ACC:PV] undergoes eating operation by Child_Model[GEN].

3. **Locative Voice [LOC]**

   **Puyuma:** Ka-ekan-ay kana walak na palridring.
   KA-eat-LV GEN child NOM plate
   "The child eats on the plate." (Location in focus)
   
   **CEREBRUM:** Plate_Model[LOC:LV] hosts eating operation with Child_Model[GEN] as agent.

4. **Circumstantial Voice [INS/DAT]**

   **Puyuma:** I-ekan kana walak na patraka.
   CV-eat GEN child NOM meat.offering
   "The child eats the meat offering (for some purpose)." (Circumstance in focus)
   
   **CEREBRUM:** MeatOffering_Model[DAT/INS:CV] serves as circumstantial entity in eating operation performed by Child_Model[GEN].

5. **Possession [GEN]**

   **Puyuma:** Na ruma' kana bangsaran
   NOM house GEN chief
   "The chief's house"
   
   **CEREBRUM:** House_Model is possessed by Chief_Model[GEN].

6. **Oblique Object**

   **Puyuma:** M-ekan na walak dra kuraw.
   AV-eat NOM child OBL fish
   "The child eats (some) fish."
   
   **CEREBRUM:** Child_Model[NOM:AV] performs eating operation on Fish_Model[OBL] with indefinite relationship.

7. **Realis vs. Irrealis**

   **Puyuma:** M-uka na walak.
   AV-go NOM child
   "The child goes." (realis)
   
   **Puyuma:** A-uka na walak.
   IRR-go NOM child
   "The child will go." (irrealis)
   
   **CEREBRUM:** Child_Model[NOM:AV] performs actual going operation vs. Child_Model[NOM:IRR] performs potential going operation.

## Implications for CEREBRUM Design

Puyuma's voice system offers valuable insights for CEREBRUM implementations:

1. **Coordinated Operation-Argument Marking**
   
   CEREBRUM could implement a coordinated marking system where operations and their arguments are marked in a complementary fashion, with verbal morphology and nominal case markers working together to establish relationship types.

2. **Modal Distinction Integration**
   
   Inspired by Puyuma's realis/irrealis distinction, CEREBRUM could integrate modal distinctions directly into its operational framework, allowing clear differentiation between actual and potential/hypothetical operations.

3. **Multi-Perspective Operational View**
   
   Based on Puyuma's four-voice system, CEREBRUM could implement multiple operational views of the same underlying process, allowing users to reframe operations based on which semantic role is most relevant to current needs.

These Puyuma-inspired approaches would be particularly valuable for CEREBRUM implementations requiring flexible perspective assignment and modal awareness across model ecosystems. 