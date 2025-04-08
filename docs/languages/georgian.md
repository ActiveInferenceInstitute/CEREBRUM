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

## 8. Example Sentences with Case Mappings

### Georgian Examples with CEREBRUM Parallels

| Georgian Sentence | Translation | Case Usage | CEREBRUM Parallel (Context) |
|-------------------|-------------|------------|-----------------------------|
| **Modeli** mushaobs. | "The model works." | Modeli = Nominative | model[NOM] (Intransitive) |
| Sistema **models** akhalēbs. | "The system updates the model." | Sistema = Nominative, Models = Dative | system[NOM] updates model[ACC] (Series I / Ongoing) |
| Sistemam **modeli** gaakhalē. | "The system updated the model." | Sistemam = Ergative, Modeli = Nominative | system[NOM, role=erg] updated model[ACC, role=abs] (Series II / Completed) |
| Momkhmarebeli **modelit** angarishobs. | "The user calculates with the model." | Modelit = Instrumental | user[NOM] calculates with model[INS] |
| Sistema **models** ugzavnis monacemebs. | "The system sends data to the model." | Models = Dative | system[NOM] sends data to model[DAT] |
| Monacemebi **modelisgan** modis. | "Data comes from the model." | Modelisgan = Genitive + -gan (Postp.) | Data originates from model[ABL] (via modifier) |
| **Modelis** shedegebi zustia. | "The model's results are accurate." | Modelis = Genitive | model[GEN].results are accurate |
| Informatsia **modelshi** aris. | "Information is in the model." | Modelshi = Dative + -shi (Postp.) | Information located in model[LOC] (via modifier) |
| **Modelad** gadak'etses. | "They turned it into a model." | Modelad = Adverbial | data.transform_to_state("model", mode=Case.ADV) |

## 9. Extension Opportunities Inspired by Georgian

Georgian's complex system suggests several extensions for CEREBRUM:

1.  **Process-State Dependent Roles**: Implement role assignments (agent/patient cases) that dynamically change based on the execution state (ongoing vs. completed) of a process, mirroring Georgian split ergativity.
2.  **Relational Modifier System**: Develop a framework of modifiers that refine base case functions, inspired by Georgian postpositions governing specific cases.
3.  **Adverbial Case for Transformations**: Utilize an Adverbial-like case marker to explicitly signify state transformations or setting operational manner.
4.  **Screeve-Based Contexts**: Model computational contexts based on temporal/aspectual categories similar to Georgian screeves, influencing case behavior.

## 10. Conclusion

Georgian offers CEREBRUM a fascinating model of how grammatical roles can be dynamically assigned based on the temporal or aspectual context of an action (split ergativity). This provides a sophisticated mechanism for representing processes where the relationship between participants changes depending on whether the process is ongoing or completed.

Furthermore, Georgian's extensive use of postpositions modifying case-marked nouns inspires a system of relational modifiers in CEREBRUM, allowing for more nuanced specification of spatial, temporal, and causal relationships beyond the core case functions.

By incorporating insights from Georgian, CEREBRUM can develop more dynamic and context-sensitive ways to model transformations and relationships, particularly reflecting the interplay between process state and participant roles.

## 11. References

1.  Aronson, Howard I. Georgian: A Reading Grammar. Slavica Publishers, 1990.
2.  Hewitt, B. G. Georgian: A Structural Reference Grammar. John Benjamins Publishing, 1995.
3.  Harris, Alice C. Diachronic Syntax: The Kartvelian Case. Academic Press, 1985.
4.  Holisky, Dee Ann. "The Case of the Intransitive Subject in Tsova-Tush (Batsbi)." Lingua 71 (1987): 103-132. (Discusses related Kartvelian ergativity)
5.  Dixon, R. M. W. Ergativity. Cambridge University Press, 1994. 