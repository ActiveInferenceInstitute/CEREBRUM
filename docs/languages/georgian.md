# Georgian Case System and CEREBRUM Mapping

Georgian (ქართული, Kartuli), a Kartvelian language spoken primarily in Georgia, possesses a complex case system with notable features like split ergativity and intricate morphophonological changes. This document examines the relationship between Georgian's case system and CEREBRUM's computational framework, focusing on how its specific features can inform CEREBRUM's implementation.

## 1. Overview of Georgian Case System

Georgian employs a system of 7 grammatical cases, marked primarily by suffixes. Key characteristics include:

- **Split Ergativity**: Case marking patterns (ergative-absolutive vs. nominative-accusative) vary depending on verb tense/aspect (specifically, the screeve or tense-aspect-mood category).
- **Agglutinative Tendencies**: Case suffixes are generally added to stems, often triggering stem changes.
- **Harmonic Clusters**: Consonant clusters within words often follow specific phonotactic rules.
- **Postpositions**: Frequently used in conjunction with case-marked nouns.

Georgian's split ergativity offers CEREBRUM a unique model for context-dependent role assignment based on temporal or processal state, while its agglutinative nature aligns with compositional transformation approaches.

## 2. Georgian Case Inventory

| Georgian Case | Suffix (Singular) | Primary Function | Typical Role |
|---------------|-------------------|------------------|-----------------|
| **Nominative** (Nominativi) | -i / -∅ | Subject of intransitive verbs (Series I/II); Subject of transitive verbs (Series I); Object of transitive verbs (Series II) | Absolutive/Nominative |
| **Ergative** (Ergativi/Motxrobiti) | -ma | Subject of transitive verbs (Series II) | Ergative |
| **Dative** (Datvi) | -s | Object of transitive verbs (Series I); Subject of certain verbs (inversion); Recipient | Accusative/Dative |
| **Genitive** (Natesaobiti) | -is | Possession; relation | Genitive |
| **Instrumental** (Instrumentali) | -it | Instrument; means | Instrumental |
| **Adverbial** (Adverbiali/Viṭarebiṭi) | -ad | Manner; transformation into | Adverbial/Translative |
| **Vocative** (Tsodebiṭi) | -o | Direct address | Vocative |

*(Note: Suffix forms can vary based on stem endings and number.)*

## 3. Split Ergativity in Georgian

The core feature is split ergativity, tied to verb conjugation series (screeves):

- **Series I (Present/Future Subseries)**: Uses Nominative-Accusative alignment.
  - *Transitive Verb*: Subject = Nominative, Object = Dative
  - *Intransitive Verb*: Subject = Nominative
- **Series II (Aorist/Perfect Subseries)**: Uses Ergative-Absolutive alignment.
  - *Transitive Verb*: Subject = Ergative, Object = Nominative
  - *Intransitive Verb*: Subject = Nominative

**Example:**

- **Series I (Present):**
  - *k'ats-i tseril-s tsers* (man-NOM letter-DAT writes)
  - "The man writes the letter."
- **Series II (Aorist):**
  - *k'ats-ma tseril-i datsera* (man-ERG letter-NOM wrote)
  - "The man wrote the letter."

The same noun ("man") is Nominative in the present tense context but Ergative in the past (Aorist) context when acting as the agent of a transitive verb. The patient ("letter") is Dative in Series I but Nominative in Series II.

## 4. Mapping CEREBRUM Cases to Georgian Cases

Mapping CEREBRUM's cases requires accounting for the split ergativity based on process state (approximated by tense/aspect series).

| CEREBRUM Case | Georgian Case(s) | Correspondence Strength | Contextual Notes |
|---------------|------------------|-------------------------|------------------|
| **Nominative [NOM]** | Nominative (-i) / Ergative (-ma) | Moderate | Maps to Nominative (intransitive agent, Series I transitive agent, Series II patient); Maps to Ergative (Series II transitive agent) |
| **Accusative [ACC]** | Dative (-s) / Nominative (-i) | Moderate | Maps to Dative (Series I patient); Maps to Nominative (Series II patient) |
| **Dative [DAT]** | Dative (-s) | Strong | Consistent mapping for recipients/indirect objects |
| **Instrumental [INS]** | Instrumental (-it) | Strong | Direct alignment for means/tools |
| **Ablative [ABL]** | Postpositions + Genitive/Instrumental | Moderate | Georgian uses postpositions (e.g., -dan 'from') often with Genitive |
| **Locative [LOC]** | Postpositions + Dative/Genitive | Moderate | Georgian uses postpositions (e.g., -shi 'in', -ze 'on') often with Dative |
| **Genitive [GEN]** | Genitive (-is) | Strong | Direct alignment for possession/relation |
| **Vocative [VOC]** | Vocative (-o) | Strong | Direct alignment for address |

### Conceptual Mapping Based on Split Ergativity

| CEREBRUM Function | Georgian Realization (Case) | Process Context (Series) | CEREBRUM Implementation Parallel |
|-------------------|-----------------------------|-------------------------|----------------------------------|
| **Agent (Transitive)** | Nominative (-i) | Series I (Present/Future) | Model[NOM, {"process_state": "ongoing"}] |
| **Agent (Transitive)** | Ergative (-ma) | Series II (Aorist/Perfect) | Model[NOM, {"process_state": "completed"}] |
| **Agent (Intransitive)** | Nominative (-i) | Series I / II | Model[NOM, {"transitivity": "intransitive"}] |
| **Patient (Transitive)** | Dative (-s) | Series I (Present/Future) | Model[ACC, {"process_state": "ongoing"}] |
| **Patient (Transitive)** | Nominative (-i) | Series II (Aorist/Perfect) | Model[ACC, {"process_state": "completed"}] |

## 5. Technical Implementation: Context-Dependent Role Assignment

Georgian's split ergativity suggests a CEREBRUM implementation where case assignment depends on the process state:

```python
class ProcessStateContext:
    ONGOING = "ongoing"  # Corresponds to Georgian Series I (Present/Future)
    COMPLETED = "completed" # Corresponds to Georgian Series II (Aorist/Perfect)

class GeorgianSplitErgativityManager:
    """
    Manages role assignment based on process state, inspired by Georgian split ergativity.
    """
    
    def assign_roles(self, process_state: ProcessStateContext, agent_model, patient_model=None):
        """
        Assign CEREBRUM case roles based on process state and transitivity.
        
        Args:
            process_state: ONGOING or COMPLETED
            agent_model: The model performing the action.
            patient_model: The model receiving the action (if transitive).
            
        Returns:
            Dict mapping role names ("agent", "patient") to case-bearing model interfaces.
        """
        roles = {}
        is_transitive = patient_model is not None
        
        if is_transitive:
            if process_state == ProcessStateContext.ONGOING:
                # Series I: Nominative-Accusative alignment (Agent=NOM, Patient=DAT -> ACC)
                roles["agent"] = agent_model.transform_to_case(Case.NOM, 
                                                             {"context": "Series I"})
                roles["patient"] = patient_model.transform_to_case(Case.ACC, 
                                                               {"context": "Series I"})
            elif process_state == ProcessStateContext.COMPLETED:
                # Series II: Ergative-Absolutive alignment (Agent=ERG -> NOM, Patient=NOM -> ACC)
                roles["agent"] = agent_model.transform_to_case(Case.NOM, 
                                                             {"context": "Series II", "role": "ergative"})
                roles["patient"] = patient_model.transform_to_case(Case.ACC, 
                                                               {"context": "Series II", "role": "absolutive"})
            else:
                raise ValueError(f"Invalid process state: {process_state}")
        else: # Intransitive
            # Agent/Subject is always Nominative in Georgian intransitives
            roles["agent"] = agent_model.transform_to_case(Case.NOM, 
                                                         {"context": f"Series I/II Intransitive"})
                                                         
        return roles

# Example Usage
processor = ProcessorModel("data_analyzer")
data = DataModel("input_stream")
manager = GeorgianSplitErgativityManager()

# Simulate ongoing transitive process (like Georgian Series I)
ongoing_roles = manager.assign_roles(ProcessStateContext.ONGOING, processor, data)
agent_nom_s1 = ongoing_roles["agent"]
patient_acc_s1 = ongoing_roles["patient"]
result_ongoing = agent_nom_s1.process(patient_acc_s1)

# Simulate completed transitive process (like Georgian Series II)
completed_roles = manager.assign_roles(ProcessStateContext.COMPLETED, processor, data)
agent_nom_s2_erg = completed_roles["agent"] # NOM case, but conceptually Ergative
patient_acc_s2_abs = completed_roles["patient"] # ACC case, but conceptually Absolutive (from NOM)
result_completed = agent_nom_s2_erg.finalize_processing(patient_acc_s2_abs)

# Simulate intransitive process
intransitive_roles = manager.assign_roles(ProcessStateContext.ONGOING, processor)
agent_nom_intrans = intransitive_roles["agent"]
agent_nom_intrans.run_independent_process()
```

## 6. Georgian Postpositions and CEREBRUM Relational Modifiers

Georgian extensively uses postpositions (which follow the noun) in conjunction with case-marked nouns to specify relationships like location, direction, and source. This inspires a CEREBRUM system where relational modifiers refine case functions:

```python
class RelationalModifier:
    """
    Represents a modifier refining a case function, inspired by Georgian postpositions.
    """
    def __init__(self, name, required_case, modifies_case_to):
        self.name = name
        self.required_case = required_case # e.g., Case.DAT for -shi 'in'
        self.modifies_case_to = modifies_case_to # e.g., Case.LOC
        
    def apply(self, model):
        """
        Apply the modifier to a model, ensuring correct base case.
        
        Returns a representation of the modified relationship.
        """
        # Ensure the model is in the required base case
        # (In Georgian, postpositions govern specific cases)
        if model.case != self.required_case:
            # Attempt transformation or raise error
            try:
                model = model.transform_to_case(self.required_case)
            except TransformationError:
                raise ValueError(
                    f"Modifier '{self.name}' requires case {self.required_case}, "
                    f"but model is in case {model.case}"
                )
                
        # Return a modified interface or representation
        # This conceptually shifts the function towards the target case
        return ModifiedInterface(model, self, target_case=self.modifies_case_to)

class ModifiedInterface:
    """Represents a model with a relationally modified case function."""
    def __init__(self, base_model, modifier: RelationalModifier, target_case):
        self.base_model = base_model
        self.modifier = modifier
        self.effective_case = target_case # The case function after modification
        
    def __repr__(self):
        return f"{self.base_model}[{self.base_model.case} + {self.modifier.name} -> {self.effective_case}]"

# Define some Georgian-inspired modifiers
MODIFIER_SHI = RelationalModifier(name="-shi", required_case=Case.DAT, modifies_case_to=Case.LOC) # 'in'
MODIFIER_ZE = RelationalModifier(name="-ze", required_case=Case.DAT, modifies_case_to=Case.LOC) # 'on'
MODIFIER_DAN = RelationalModifier(name="-dan", required_case=Case.GEN, modifies_case_to=Case.ABL) # 'from'
MODIFIER_TAN = RelationalModifier(name="-tan", required_case=Case.GEN, modifies_case_to=Case.LOC) # 'at'/'with'

# Example Usage
storage_model = StorageModel("database")

# Model needs to be in Dative case for -shi modifier
storage_dat = storage_model.transform_to_case(Case.DAT)

# Apply the modifier to get a Locative function
loc_interface_in = MODIFIER_SHI.apply(storage_dat)
print(loc_interface_in) # StorageModel[DATIVE + -shi -> LOC]

# Model needs Genitive case for -dan
storage_gen = storage_model.transform_to_case(Case.GEN)

# Apply -dan modifier to get Ablative function
abl_interface_from = MODIFIER_DAN.apply(storage_gen)
print(abl_interface_from) # StorageModel[GENITIVE + -dan -> ABL]
```

## 7. Georgian Adverbial Case and CEREBRUM State Transformation

The Georgian Adverbial case (-ad) can indicate transformation into a state or manner. This maps well to CEREBRUM's concept of state transformation or setting operational modes:

```python
# Simulate Georgian Adverbial case (-ad) for transformation
# e.g., *m̐ṭʻral-ad gadaakʻtsia* (drunk-ADV he-turned-him)
# "He turned him into a drunkard" / "He made him drunk"

data_raw = RawData(" unprocessed_info")

# Transform raw data into a processed state (like Adverbial -ad)
transformed_data = data_raw.transform_to_state("processed", mode=Case.ADV)

# Simulate Adverbial case for manner
# e.g., *kartul-ad laparakobs* (Georgian-ADV he-speaks)
# "He speaks in Georgian"

translator_model = TranslationModel("multilingual_engine")

# Set processing mode using Adverbial-like case
result = translator_model[Case.ADV, {"manner": "literal"}].translate(text)
```

## 8. Deeper Integration with CEREBRUM Concepts

Georgian grammar, especially its split ergativity and case usage, provides sophisticated models for CEREBRUM:

**a. Split Ergativity, Morphosyntactic Alignment, and Active Inference:**
Georgian's **split ergativity** is a prime example of context-dependent **morphosyntactic alignment** (Figures 9, 10, `CEREBRUM.md`). The choice between Nominative-Accusative (Series I) and Ergative-Absolutive (Series II) alignment depends entirely on the verb's tense-aspect-mood (screeve), representing the process state (ongoing vs. completed).
- **CEREBRUM Modeling:** This necessitates a CEREBRUM implementation where case assignment ([NOM], [ACC]) is dynamically determined by the predicted or actual state of the process/interaction. The `GeorgianSplitErgativityManager` (Section 5) provides a blueprint. A CEREBRUM model must predict the process state to correctly assign/interpret case roles, aligning with **Active Inference** principles – minimizing prediction error about the interaction dynamics.
- **Precision:** The shift in alignment could also signal a shift in **precision** weighting. For instance, the agent ([NOM]) might be assigned higher precision in the completed state (Series II, Ergative) where its causal role is finalized, compared to the ongoing state (Series I, Nominative).

**b. Declinability, Agglutination, and Postpositions:**
- **Case Marking:** The 7 distinct case endings demonstrate **declinability**, altering the noun's function (Section 2).
- **Postpositions as Modifiers:** Georgian's heavy use of postpositions governing specific cases (e.g., Genitive + `-dan` for Ablative function) mirrors the idea of compositional transformations. A base case transformation ([GEN]) is further modified by a postpositional morphism (`-dan`) to yield a more specific functional role ([ABL]). This fits the category-theoretic view of composing morphisms (Section 6).

**c. Polypersonal Agreement and Precision:**
Georgian verbs often agree with multiple arguments (subject, direct object, indirect object) simultaneously through prefixes and suffixes. This **polypersonalism** highlights the grammatical importance of tracking multiple core participants. In CEREBRUM, this translates to needing robust mechanisms (like the event handler in `basque.md`, Section 6) to manage interactions involving multiple case-bearing models ([NOM], [ACC], [DAT]) simultaneously, potentially assigning high **precision** to the state tracking of these core roles involved in the agreement.

**d. Category Theory and Morphisms:**
- **Cases as Morphisms:** Each Georgian case suffix (-i, -ma, -s, -is, -it, -ad, -o) represents a distinct **morphism** transforming a base model stem (object) into a specific functional state within the CEREBRUM category (Figures 7, 8, `CEREBRUM.md`).
- **Postpositions + Case:** The combination `Case + Postposition` acts as a **composite morphism**, refining the functional role (e.g., `DativeMorphism` followed by `LocativeShiMorphism`).

**e. Adverbial Case and Speculative Cases (`cerebrum_beyond_cases.md`):**
- **Adverbial (-ad):** The use of the Adverbial case for transformation ("becoming X") strongly parallels the concept of a **Translative [TRANS]** case (like in Finnish). Its use for manner ("in X way") suggests a specialized **Instrumental [INS-MANNER]** function. This demonstrates how a single morphological case can encompass functions that might emerge as distinct cases in a CEREBRUM ecosystem.
- **Context-Dependent Cases:** Split ergativity itself is a fascinating example. The *same* conceptual role (transitive agent) is realized by *different* cases ([NOM] vs. [ERG]) depending on context (process state). This suggests CEREBRUM might not just evolve new static cases, but potentially **context-sensitive case assignments** where the mapping from function to case dynamically adapts based on higher-level state variables, reflecting a sophisticated form of FEP minimization across the ecosystem.

Georgian's dynamic alignment system, use of postpositions, and polypersonalism offer advanced patterns for context-aware role assignment, compositional transformations, and multi-participant tracking within CEREBRUM.

## 9. Conclusion (Renumbered from 8)

Georgian provides unique and valuable insights for the CEREBRUM framework, primarily through:

1.  **Split Ergativity**: Offers a compelling model for context-dependent morphosyntactic alignment in CEREBRUM, where case assignment depends on process state (e.g., ongoing vs. completed).
2.  **Postpositional System**: Demonstrates how base case functions can be systematically refined by modifiers (postpositions), suggesting a compositional approach to CEREBRUM transformations.
3.  **Adverbial Case**: Provides parallels for state transformation ([TRANS]) and manner ([INS-MANNER]) functions.
4.  **Polypersonalism (Implied)**: Highlights the importance of simultaneously tracking multiple core participant roles ([NOM], [ACC], [DAT]).

Incorporating these features allows CEREBRUM to model more dynamic and context-sensitive interactions, where the grammatical roles and transformations adapt based on the state of the ongoing processes, mirroring the sophistication found in languages like Georgian.

## 10. References (Renumbered from 9)

1.  Aronson, Howard I. Georgian: A Reading Grammar. Slavica Publishers, 1990.
2.  Hewitt, B. G. Georgian: A Structural Reference Grammar. John Benjamins Publishing, 1995.
3.  Harris, Alice C. Diachronic Syntax: The Kartvelian Case. Academic Press, 1985.
4.  Holisky, Dee Ann. "The Case of the Intransitive Subject in Tsova-Tush (Batsbi)." Lingua 71 (1987): 103-132. (Discusses related Kartvelian ergativity)
5.  Dixon, R. M. W. Ergativity. Cambridge University Press, 1994. 