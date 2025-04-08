# Basque Case System and CEREBRUM Mapping

Basque (Euskara), a language isolate spoken in the Basque Country, features a distinct ergative-absolutive case system. This document explores the relationship between Basque's unique grammatical structure and CEREBRUM's computational case framework, highlighting insights relevant to modeling agent-patient relationships and transitivity.

## 1. Overview of Basque Case System

Basque is known for its ergative-absolutive alignment, which differs significantly from the nominative-accusative alignment found in most European languages. Key characteristics include:

- **Ergative-Absolutive Alignment**: The subject of an intransitive verb and the object of a transitive verb share the same case (Absolutive), while the subject of a transitive verb takes a distinct case (Ergative).
- **Agglutinative Morphology**: Case endings are suffixes added to noun phrases.
- **Multiple Cases**: Basque has around 12-15 cases, including core grammatical cases and numerous local cases.
- **No Grammatical Gender**: Similar to Finno-Ugric languages.

This unique alignment offers CEREBRUM a different perspective on modeling agency and action, focusing on the relationship between the actor and the transitivity of the process.

## 2. Basque Case Inventory (Core Cases)

| Basque Case | Typical Ending | Primary Function | Alignment |
|-------------|----------------|------------------|-----------|
| **Absolutive** (Nor) | -a / -ak (sg/pl) | Subject of intransitive verbs; Object of transitive verbs | Absolutive |
| **Ergative** (Nork) | -ak / -ek (sg/pl) | Subject of transitive verbs | Ergative |
| **Dative** (Nori) | -ari / -ei (sg/pl) | Indirect object; Recipient | Dative |
| **Genitive** (Noren) | -aren / -en (sg/pl) | Possession | Genitive |

### Local Cases (Selection)

Basque also has a rich set of local cases, similar to Finnish or Hungarian:

| Case Group | Static Location | Movement Towards | Movement From |
|------------|-----------------|-----------------|---------------|
| **Inessive** (Non) | -n / -etan | -ra / -etara | -tik / -etatik |
| **Adessive** (Non) | -n / -etan | -ra / -etara | -tik / -etatik |
| **Ablative** (Nondik) | - | - | -tik / -etatik |
| **Allative** (Nora) | - | -ra / -etara | - |

*(Note: Some local cases share forms, context distinguishes usage)*

### Other Cases

| Basque Case | Typical Ending | Primary Function |
|-------------|----------------|------------------|
| **Instrumental** (Nortaz/Zerzaz) | -z / -az | Means, instrument |
| **Comitative** (Norekin) | -ekin | Accompaniment ("with") |
| **Benefactive** (Norentzat) | -entzat | Beneficiary ("for") |
| **Causal** (Zergatik) | -(n)gatik | Reason ("because of") |

## 3. Ergative-Absolutive Alignment Explained

Consider these examples:

1.  **Gizona etorri da.** (gizona = Absolutive)
    *"The man (Abs.) came."* (Intransitive verb)
2.  **Gizonak txakurra ikusi du.** (gizonak = Ergative, txakurra = Absolutive)
    *"The man (Erg.) saw the dog (Abs.)."* (Transitive verb)

In (1), the sole participant "gizona" takes the Absolutive case.
In (2), the agent "gizonak" takes the Ergative case, while the patient "txakurra" takes the Absolutive case.

The Absolutive case marks the core participant directly involved in the event state or undergoing the change, regardless of whether it's the agent (in intransitive) or patient (in transitive).

## 4. Mapping CEREBRUM Cases to Basque Cases

Mapping CEREBRUM's nominative-accusative based system to Basque's ergative-absolutive system requires careful consideration of agentivity and transitivity.

### Core Case Mapping

| CEREBRUM Case | Basque Case(s) | Correspondence Strength | Notes |
|---------------|----------------|-------------------------|-------|
| **Nominative [NOM]** | Ergative (Nork) / Absolutive (Nor) | Moderate | Maps to Ergative for transitive agents, Absolutive for intransitive agents |
| **Accusative [ACC]** | Absolutive (Nor) | Moderate | Maps to Absolutive for patients of transitive actions |
| **Dative [DAT]** | Dative (Nori) | Strong | Direct alignment for recipients |
| **Instrumental [INS]** | Instrumental (Nortaz) | Strong | Direct alignment for means/tools |
| **Ablative [ABL]** | Ablative (Nondik) / Local cases with -tik | Strong | Maps to source/origin cases |
| **Locative [LOC]** | Inessive/Adessive (-n) / Other local cases | Strong | Maps to location-marking cases |
| **Genitive [GEN]** | Genitive (Noren) | Strong | Direct alignment for possession/relation |
| **Vocative [VOC]** | (Limited marking) | Weak | Basque typically uses intonation or specific constructions |

### Conceptual Mapping Based on Ergativity

| CEREBRUM Function | Basque Realization | CEREBRUM Implementation Parallel |
|-------------------|--------------------|----------------------------------|
| **Agent of Transitive Process** | Ergative Case (Nork) | Model[NOM, {"transitivity": "transitive"}] |
| **Agent of Intransitive Process** | Absolutive Case (Nor) | Model[NOM, {"transitivity": "intransitive"}] |
| **Patient of Transitive Process** | Absolutive Case (Nor) | Model[ACC] |
| **Core Participant (State/Change)** | Absolutive Case (Nor) | Model directly representing the core state variable (potentially ACC or NOM intransitive) |

## 5. Technical Implementation: Ergativity-Aware Model Roles

Basque's system suggests modeling roles based on the transitivity context:

```python
class TransitivityContext:
    TRANSITIVE = "transitive"
    INTRANSITIVE = "intransitive"

class ErgativityAwareModel:
    def __init__(self, base_model):
        self.base_model = base_model
        
    def assume_role(self, transitivity: TransitivityContext, role: str):
        """
        Assign a role based on the transitivity context, inspired by Basque.
        
        Args:
            transitivity: TRANSITIVE or INTRANSITIVE
            role: "agent" or "patient"
            
        Returns:
            Case-bearing model interface representing the assigned role.
        """
        
        if transitivity == TransitivityContext.TRANSITIVE:
            if role == "agent":
                # Agent of transitive action -> Ergative -> CEREBRUM NOM
                return self.base_model.transform_to_case(Case.NOM, 
                                                        {"role": "ergative_agent"})
            elif role == "patient":
                # Patient of transitive action -> Absolutive -> CEREBRUM ACC
                return self.base_model.transform_to_case(Case.ACC, 
                                                        {"role": "absolutive_patient"})
            else:
                raise ValueError(f"Invalid role for transitive context: {role}")
                
        elif transitivity == TransitivityContext.INTRANSITIVE:
            if role == "agent":
                # Agent/Subject of intransitive action -> Absolutive -> CEREBRUM NOM
                return self.base_model.transform_to_case(Case.NOM, 
                                                        {"role": "absolutive_agent"})
            elif role == "patient":
                # Intransitive processes typically don't have a distinct patient
                raise ValueError("Intransitive context usually lacks a separate patient role")
            else:
                raise ValueError(f"Invalid role for intransitive context: {role}")
        else:
            raise ValueError(f"Invalid transitivity context: {transitivity}")

# Example Usage
predictor_model = PredictionModel("climate_predictor")
data_model = DataModel("sensor_data")
process_model = ProcessModel("simulation_runner")

# Wrap models for ergativity awareness
erg_predictor = ErgativityAwareModel(predictor_model)
erg_data = ErgativityAwareModel(data_model)
erg_process = ErgativityAwareModel(process_model)

# Simulate a TRANSITIVE process (predictor processes data)
# predictor = agent (Ergative -> NOM)
# data = patient (Absolutive -> ACC)
transitive_agent = erg_predictor.assume_role(TransitivityContext.TRANSITIVE, "agent")
transitive_patient = erg_data.assume_role(TransitivityContext.TRANSITIVE, "patient")
result = transitive_agent.process(transitive_patient)

# Simulate an INTRANSITIVE process (process runs)
# process = agent (Absolutive -> NOM)
intransitive_agent = erg_process.assume_role(TransitivityContext.INTRANSITIVE, "agent")
intransitive_agent.run()
```

## 6. Basque Auxiliary Verbs and CEREBRUM Event Handling

Basque often uses auxiliary verbs (izan 'to be', *edun 'to have', egon 'to be located') that agree with multiple arguments (Absolutive, Ergative, Dative). This multi-argument agreement provides a model for CEREBRUM event handlers that manage interactions between multiple case-bearing models:

```python
class MultiArgumentEventHandler:
    """
    Event handler inspired by Basque auxiliary verb agreement.
    Handles events involving multiple participants marked by different roles.
    """
    
    def __init__(self):
        self.event_registry = {}
        
    def register_event(self, event_name, handler_fn, 
                         required_participants=None):
        """
        Register an event handler function.
        
        Args:
            event_name: Name of the event
            handler_fn: Function to execute for the event
            required_participants: Dict mapping roles (Absolutive, Ergative, Dative) 
                                  to expected model types or interfaces.
                                  e.g., {"Absolutive": DataModel, "Ergative": ProcessorModel}
        """
        self.event_registry[event_name] = {
            "handler": handler_fn,
            "participants": required_participants or {}
        }
        
    def trigger_event(self, event_name, participants):
        """
        Trigger an event with specified participants.
        
        Args:
            event_name: Name of the event to trigger.
            participants: Dict of participants keyed by their role 
                          (Absolutive, Ergative, Dative).
                          e.g., {"Absolutive": data_model_acc, 
                                 "Ergative": processor_model_nom}
        Returns:
            Result of the event handler.
        """
        if event_name not in self.event_registry:
            raise ValueError(f"Unknown event: {event_name}")
            
        registration = self.event_registry[event_name]
        handler_fn = registration["handler"]
        required = registration["participants"]
        
        # Validate participants against requirements (like Basque verb agreement)
        validated_args = {}
        for role, req_type in required.items():
            if role not in participants:
                raise ValueError(f"Missing required participant role: {role}")
            
            participant_model = participants[role]
            # Basic type check (could be more sophisticated interface check)
            if not isinstance(participant_model, req_type):
                raise TypeError(f"Participant for role {role} has wrong type")
            
            validated_args[role.lower()] = participant_model
            
        # Execute handler with validated participants
        return handler_fn(**validated_args)

# Example Registration and Triggering
event_handler = MultiArgumentEventHandler()

def handle_data_processing(absolutive, ergative, dative=None):
    # Process data (absolutive) using processor (ergative)
    # Optionally notify recipient (dative)
    result = ergative.process(absolutive)
    if dative:
        dative.notify(result)
    return result

event_handler.register_event(
    "PROCESS_DATA", 
    handle_data_processing,
    required_participants={
        "Absolutive": DataModel[Case.ACC],
        "Ergative": ProcessorModel[Case.NOM]
    }
)

# Triggering the event
processor_nom = processor_model.transform_to_case(Case.NOM)
data_acc = data_model.transform_to_case(Case.ACC)

result = event_handler.trigger_event(
    "PROCESS_DATA", 
    participants={
        "Absolutive": data_acc, # Model in Absolutive/ACC role
        "Ergative": processor_nom # Model in Ergative/NOM role
    }
)
```

## 7. Deeper Integration with CEREBRUM Concepts

Basque grammar, particularly its ergativity and rich case system, offers profound insights for CEREBRUM:

**a. Morphosyntactic Alignment and CEREBRUM (Figures 9, 10, `CEREBRUM.md`):**
Basque is the archetypal example of **Ergative-Absolutive alignment**. This directly challenges the default Nominative-Accusative bias often implicit in computational models (and English-based descriptions).
- **CEREBRUM Modeling:** CEREBRUM must be flexible enough to represent *both* alignment systems. The Basque example demonstrates the need for case assignments ([NOM], [ACC]) to be sensitive to the transitivity of the interaction (Section 5). CEREBRUM's [NOM] can represent either the Ergative (transitive agent) or the Absolutive (intransitive agent), while [ACC] typically aligns with the Absolutive (transitive patient). This necessitates a more nuanced internal representation or explicit parameters (e.g., `model[Case.NOM, {"alignment_role": "ergative"}]`) within the CEREBRUM framework to capture these different alignment patterns accurately.

**b. Declinability and Agglutination:**
Basque's agglutinative nature, where multiple suffixes (case, number, definiteness) stack onto a stem, is a powerful analogy for **declinability**. A single CEREBRUM model (the stem) can be systematically modified by applying transformations (the suffixes) to adapt its function and relational properties. The `ErgativityAwareModel` (Section 5) and `MultiArgumentEventHandler` (Section 6) hint at how such layered properties (case, role, number, definiteness) could be managed computationally.

**c. Active Inference and Transitivity:**
- **Predicting Alignment:** From an Active Inference perspective, a CEREBRUM ecosystem operating with Basque-like alignment would need its generative model to predict the transitivity of an upcoming interaction to assign the correct case roles (Ergative vs. Absolutive for the agent). The system minimizes free energy by correctly parsing and predicting these relational structures.
- **Precision and Argument Marking:** The complex polypersonal agreement on Basque auxiliary verbs (agreeing with Ergative, Absolutive, and Dative arguments simultaneously) suggests a high degree of **precision** associated with these core participant roles. In CEREBRUM, this could translate to allocating higher computational resources or confidence to interactions involving models in these core case roles, as marked by the event handler.

**d. Category Theory and Complex Morphisms:**
- **Local Cases as Morphisms:** Basque's rich inventory of local cases (Inessive, Allative, Ablative, etc.) can be modeled as specific **morphisms** within the CEREBRUM category (Figures 7, 8, `CEREBRUM.md`). These morphisms transform a model's state by situating it spatially or directionally relative to another model or context ([LOC], [ABL]-like transformations).
- **Agglutination as Composition:** The stacking of suffixes can be viewed as the **composition of morphisms**. Applying a plural suffix, then a case suffix, then a postposition is analogous to applying a sequence of transformations to a CEREBRUM model: `Model -> Model[Plural] -> Model[Plural, Case] -> Model[Plural, Case, Relation]`.

**e. Speculative Cases and Granularity (`cerebrum_beyond_cases.md`):**
- **Rich Case Systems:** Basque's numerous cases (grammatical, local, instrumental, etc.) demonstrate a high degree of **granularity** in specifying relationships. This contrasts with languages like Arabic or English. It suggests that a complex CEREBRUM ecosystem might naturally **evolve** a richer set of specialized cases (beyond the core 8) if doing so provides a more efficient or precise way to model recurring interaction patterns (FEP minimization). The Basque system provides concrete examples of what such evolved cases (e.g., specific spatial relations, Benefactive, Causal) might look like.
- **Synthetic Cases:** Could a CEREBRUM system dynamically **synthesize** a new case marker (like Basque's agglutinative suffixes) to represent a newly stabilized interaction pattern, potentially combining existing case features (e.g., a Locative-Instrumental case for "using something at a location")?

Basque grammar, with its ergativity and agglutination, pushes the CEREBRUM framework to handle diverse alignment strategies and consider how complex, multi-suffix case markings can represent layered functional roles and transformations, offering a rich source of inspiration for modeling advanced model interactions and potential case system evolution.

## 8. Conclusion (Renumbered from 7)

Basque offers critical insights for CEREBRUM, primarily through its ergative-absolutive alignment and rich agglutinative case system. Key takeaways include:

1.  **Alignment Flexibility**: CEREBRUM must accommodate different morphosyntactic alignments (Nominative-Accusative vs. Ergative-Absolutive) by making case assignments sensitive to transitivity.
2.  **Granular Relationships**: The extensive set of Basque cases (especially local ones) provides a model for representing fine-grained relational roles (location, direction, instrument, beneficiary) beyond the core CEREBRUM set.
3.  **Multi-Argument Agreement**: Basque auxiliary verbs highlight the need for event handling mechanisms in CEREBRUM that can manage interactions involving multiple, distinctly-roled participants.
4.  **Agglutination as Declinability**: The suffix-stacking nature of Basque provides a linguistic parallel for how CEREBRUM models can be systematically modified through sequential transformations (adding case, number, definiteness features).

Studying Basque encourages a CEREBRUM design that is adaptable to different grammatical strategies for encoding relationships and agent/patient roles.

## 9. References (Renumbered from 8)

1.  King, Alan R. The Basque Language: A Practical Introduction. University of Nevada Press, 1994.
2.  Hualde, José Ignacio, and Jon Ortiz de Urbina, eds. A Grammar of Basque. Mouton de Gruyter, 2003.
3.  Trask, R. L. The History of Basque. Routledge, 1997.
4.  Dixon, R. M. W. Ergativity. Cambridge University Press, 1994.
5.  Laka, Itziar. "Unergatives that assign ergative, unaccusatives that assign accusative." MIT Working Papers in Linguistics 10 (1988): 149-172. 