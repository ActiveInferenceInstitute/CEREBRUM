# Navajo Verb System and CEREBRUM Mapping

Navajo (Diné bizaad), an Athabaskan language of the Na-Dené family, features a complex, templatic verb system rather than a traditional case system. This document explores how Navajo's intricate verb structure offers unique perspectives for CEREBRUM's computational framework, particularly regarding temporal-spatial relationships, processal state tracking, and event handling.

## 1. Overview of Navajo's Linguistic Structure

Unlike Indo-European languages with nominal case systems, Navajo encodes grammatical relationships primarily through its verb complex, which features:

- **Templatic structure**: 11 ordered position classes for verb prefixes
- **Polysynthetic morphology**: multiple morphemes combine into complex verb forms
- **Classifier system**: verbal stems classified by transitivity and semantic features
- **Viewpoint aspect**: perfective, imperfective, progressive, and iterative aspects
- **Subject/object agreement**: person and number marked on the verb
- **Directional/spatial prefixes**: detailed encoding of motion, direction, and location

This verb-centered approach offers CEREBRUM an alternative model for understanding relationships between models, emphasizing process states, transitions, and transformations over static case assignments.

## 2. The Navajo Verb Template

Navajo verbs follow a strict template with 11 ordered prefix positions:

| Position | Name | Function | Example Prefix |
|----------|------|----------|----------------|
| 0 | **Stem** | Core meaning; encodes aspect | -łtsos "to see" |
| 1 | **Classifier** | Valence, voice | -ł- (causative) |
| 2 | **Mode/Aspect** | Perfective, imperfective, etc. | si- (perfective) |
| 3 | **Subject** | 3rd person subject | yi- |
| 4 | **Deictic Subject** | Indefinite subject | 'a- |
| 5 | **Direct Object** | Object markers | bi- |
| 6 | **Deictic Object** | Indefinite object | 'a- |
| 7 | **Adverbial** | Manner, quality | ni- ("horizontally") |
| 8 | **Iterative** | Repetition | ná- |
| 9 | **Distributive** | Distribution over multiple instances | da- |
| 10 | **Disjunct Prefixes** | Incorporated nouns, direction | tsi- ("head") |
| 11 | **Conjunct Prefixes** | Direction, path | ná- ("around") |

This structured approach to verb formation provides a model for composing complex process flows in CEREBRUM, where different aspects of transformation can be systematically arranged in relation to one another.

## 3. Navajo Aspect System

Navajo has a sophisticated aspect system that encodes viewpoints on actions:

| Aspect | Navajo Prefix | Function | Example |
|--------|---------------|----------|---------|
| **Imperfective** | yi- | Ongoing, habitual | Naałniih "He/she is playing" |
| **Perfective** | yí- | Completed | Naalnii' "He/she played" |
| **Progressive** | yíí- | Ongoing with continuous focus | Naałnííh "He/she is in the process of playing" |
| **Iterative** | náá- | Repeated action | Nánaałniih "He/she plays repeatedly" |
| **Future** | doo- + -da | Action will occur | Doo naałniih da "He/she will play" |
| **Optative** | o- | Desired action | O'naałniih "May he/she play" |

This aspect system offers CEREBRUM a model for handling time-based model states and transformations, particularly for processes that evolve over time or have different temporal phases.

## 4. Subject-Object Relationships in Navajo

Rather than using nominal cases, Navajo marks subject-object relationships through verb prefixes:

| Person | Subject Prefix | Object Prefix | Example Verb Form |
|--------|---------------|---------------|-------------------|
| 1SG | sh- | shi- | Shiłtsá "He/she sees me" |
| 2SG | ni- | ni- | Niłtsá "He/she sees you" |
| 3SG | yi- | bi- | Yiłtsá "He/she sees him/her" |
| 1PL | dii- | nihii- | Nihiiłtsá "He/she sees us" |
| 2PL | oh- | nihii- | Nihiiłtsá "He/she sees you all" |
| 3PL | da- + yi- | da- + bi- | Dayiłtsá "He/she sees them" |

This integrated approach to marking participants within the verb provides CEREBRUM with a model for tracking relationships between entities within a single transformation process, rather than treating them as separately case-marked entities.

## 5. Directional Systems in Navajo

Navajo features an extensive system of directional prefixes that encode precise spatial information:

| Directional Prefix | Meaning | Example |
|--------------------|---------|---------|
| **ni-** | Horizontal movement | Niyá "He/she arrived (horizontally)" |
| **yi-** | Downward movement | Yiyá "He/she descended" |
| **ha-** | Upward movement | Hayá "He/she ascended" |
| **ná-** | Circular movement | Náyá "He/she moved around" |
| **'a-** | Unspecified movement | 'Ayá "He/she went (in general)" |
| **di-** | Starting point | Diyá "He/she started off" |
| **'ii-** | Prolonged movement | 'Iiyá "He/she traveled a long distance" |

This directional system offers CEREBRUM a model for representing transformational pathways and spatial relationships between models or states in a computational framework.

## 6. Mapping Navajo Verb Features to CEREBRUM Concepts

### Direct Conceptual Correspondences

| Navajo Feature | CEREBRUM Concept | Correspondence Strength | Notes |
|----------------|-----------------|-------------------------|-------|
| **Verb Template** | Transformation Pipeline | Strong | Both represent ordered processing stages |
| **Classifier System** | Model Type Categorization | Strong | Both classify entities by operational properties |
| **Aspect Marking** | Process State Tracking | Strong | Both track execution state over time |
| **Subject/Object Prefixes** | Agent/Patient Relations | Strong | Both mark actor/acted-upon relationships |
| **Directional Prefixes** | Transformation Pathways | Strong | Both encode paths between states |
| **Iterative Marking** | Recurring Process Handling | Strong | Both handle repetition of operations |
| **Incorporated Nouns** | Model Composition | Moderate | Both combine entities into complex units |

### CEREBRUM Case to Navajo Feature Mapping

| CEREBRUM Case | Navajo Equivalent | Relationship |
|---------------|-------------------|--------------|
| **Nominative [NOM]** | Subject prefix (yi-, ni-, shi-) | Both mark the agent/doer |
| **Accusative [ACC]** | Object prefix (bi-, ni-, shi-) | Both mark the patient/receiver |
| **Dative [DAT]** | Benefactive prefix (bá-, há-) | Both mark recipient/beneficiary |
| **Instrumental [INS]** | Instrumental postposition (bee) | Both mark instrument/means |
| **Ablative [ABL]** | Source prefix (bits'ą́ą́dóó) | Both mark source/origin |
| **Locative [LOC]** | Locative postposition (-di, -gi) | Both mark location |
| **Genitive [GEN]** | Possessive prefix (bi-, ni-, shi-) | Both mark possession |
| **Vocative [VOC]** | Direct address (no direct equivalent) | Limited correspondence |

## 7. Technical Implementation

Navajo's templatic verb structure inspires a sequential, composable transformation architecture for CEREBRUM:

```python
class NavajoInspiredTemplateProcessor:
    """
    CEREBRUM transformation processor inspired by Navajo verb template.
    
    This approach implements transformations as a series of ordered prefix operations,
    similar to how Navajo builds complex verbs through ordered prefix positions.
    """
    
    def __init__(self):
        # Initialize the template positions (like Navajo's 11 prefix positions)
        self.template_positions = {
            "stem": None,               # Core transformation (position 0)
            "classifier": None,         # Transformation type classifier (position 1)
            "aspect": None,             # Process state/aspect (position 2)
            "subject": None,            # Agent/actor (position 3-4)
            "object": None,             # Patient/target (position 5-6)
            "adverbial": None,          # Transformation manner (position 7)
            "iterative": None,          # Repetition handler (position 8)
            "distributive": None,       # Distribution over instances (position 9)
            "disjunct": None,           # Incorporated models (position 10)
            "conjunct": None            # Direction/path (position 11)
        }
        self.prefix_registry = {}
        
    def register_prefix(self, position, name, function):
        """Register a prefix function for a specific template position"""
        if position not in self.template_positions:
            raise ValueError(f"Invalid template position: {position}")
            
        if position not in self.prefix_registry:
            self.prefix_registry[position] = {}
            
        self.prefix_registry[position][name] = function
        
    def set_prefix(self, position, prefix_name, *args, **kwargs):
        """Set a specific prefix for a template position"""
        if position not in self.template_positions:
            raise ValueError(f"Invalid template position: {position}")
            
        if position not in self.prefix_registry or prefix_name not in self.prefix_registry[position]:
            raise ValueError(f"Unknown prefix '{prefix_name}' for position '{position}'")
            
        # Store prefix with its arguments
        self.template_positions[position] = (prefix_name, args, kwargs)
        
        return self
        
    def set_stem(self, stem_function):
        """Set the stem function (core transformation)"""
        self.template_positions["stem"] = stem_function
        return self
        
    def execute(self, input_model):
        """
        Execute the transformation using the configured template
        (like executing a Navajo verb with all its prefixes)
        """
        # Check if stem is set
        if self.template_positions["stem"] is None:
            raise ValueError("Cannot execute template without a stem")
            
        # Initialize context for the transformation
        context = {"input": input_model, "current": input_model}
        
        # Execute prefixes in template order (like Navajo verb formation)
        for position in ["conjunct", "disjunct", "distributive", "iterative", 
                        "adverbial", "object", "subject", "aspect", "classifier"]:
            prefix_data = self.template_positions[position]
            
            if prefix_data is not None:
                prefix_name, args, kwargs = prefix_data
                prefix_fn = self.prefix_registry[position][prefix_name]
                
                # Execute prefix function with context and args
                context = prefix_fn(context, *args, **kwargs)
                
        # Finally, execute the stem (core transformation)
        stem_fn = self.template_positions["stem"]
        result = stem_fn(context)
        
        return result
```

## 8. Aspect-Inspired State Tracking

Navajo's aspect system provides a model for CEREBRUM to track process states:

```python
class AspectAwareProcessor:
    """
    Process state tracker inspired by Navajo aspect system.
    
    Tracks the state of model transformations using concepts similar
    to Navajo's imperfective, perfective, progressive, and iterative aspects.
    """
    
    # Aspect states
    IMPERFECTIVE = "imperfective"  # Ongoing/habitual process
    PERFECTIVE = "perfective"      # Completed process
    PROGRESSIVE = "progressive"    # Currently in progress
    ITERATIVE = "iterative"        # Repeatedly occurring
    FUTURE = "future"              # Scheduled to occur
    OPTATIVE = "optative"          # Desired but not yet executed
    
    def __init__(self):
        self.processes = {}
        
    def register_process(self, process_id, initial_aspect=None):
        """Register a process with an initial aspect"""
        self.processes[process_id] = {
            "aspect": initial_aspect or self.FUTURE,
            "start_time": None,
            "end_time": None,
            "iterations": 0,
            "history": []
        }
        
    def set_aspect(self, process_id, aspect, metadata=None):
        """Update the aspect of a process"""
        if process_id not in self.processes:
            raise ValueError(f"Unknown process: {process_id}")
            
        # Record previous aspect in history
        previous = self.processes[process_id]["aspect"]
        timestamp = time.time()
        
        self.processes[process_id]["history"].append({
            "from": previous,
            "to": aspect,
            "timestamp": timestamp,
            "metadata": metadata
        })
        
        # Update current aspect
        self.processes[process_id]["aspect"] = aspect
        
        # Track timing based on aspect
        if aspect == self.PROGRESSIVE and previous != self.PROGRESSIVE:
            # Process is starting/resuming
            self.processes[process_id]["start_time"] = timestamp
            
        elif previous == self.PROGRESSIVE and aspect != self.PROGRESSIVE:
            # Process is pausing/completing
            self.processes[process_id]["end_time"] = timestamp
            
        # Track iterations for iterative processes
        if aspect == self.ITERATIVE:
            self.processes[process_id]["iterations"] += 1
            
        return self
        
    def get_aspect(self, process_id):
        """Get the current aspect of a process"""
        if process_id not in self.processes:
            raise ValueError(f"Unknown process: {process_id}")
            
        return self.processes[process_id]["aspect"]
        
    def get_process_info(self, process_id):
        """Get complete information about a process"""
        if process_id not in self.processes:
            raise ValueError(f"Unknown process: {process_id}")
            
        return self.processes[process_id]
        
    def execute_with_aspect(self, process_id, function, inputs, desired_aspect=PERFECTIVE):
        """
        Execute a function with aspect tracking
        (similar to using different aspect forms of a Navajo verb)
        """
        if process_id not in self.processes:
            self.register_process(process_id)
            
        # Set to progressive as we begin
        self.set_aspect(process_id, self.PROGRESSIVE)
        
        try:
            # Execute the function
            result = function(inputs)
            
            # Set to desired final aspect (normally perfective)
            self.set_aspect(process_id, desired_aspect, {"result": "success"})
            
            return result
            
        except Exception as e:
            # Set to imperfective if something went wrong
            self.set_aspect(process_id, self.IMPERFECTIVE, {"result": "error", "error": str(e)})
            raise
```

## 9. Directional-Inspired Transformation Pathways

Navajo's directional prefixes inspire a system for modeling transformation paths:

```python
class DirectionalTransformationRouter:
    """
    Transformation router inspired by Navajo directional prefixes.
    
    Routes transformations along specific pathways based on directional concepts
    similar to Navajo's spatial prefixes.
    """
    
    # Directional constants (inspired by Navajo directionals)
    HORIZONTAL = "horizontal"  # Level/lateral transformation (ni-)
    DOWNWARD = "downward"      # Reduction/simplification (yi-)
    UPWARD = "upward"          # Expansion/elaboration (ha-)
    CIRCULAR = "circular"      # Iterative/recursive transformation (ná-)
    GENERAL = "general"        # Generic/unspecified path ('a-)
    STARTING = "starting"      # Initialization transformation (di-)
    PROLONGED = "prolonged"    # Extended/multi-stage transformation ('ii-)
    
    def __init__(self):
        self.path_registry = {}
        
    def register_path(self, from_state, to_state, direction, transformer):
        """
        Register a transformation path between states with a direction
        
        Args:
            from_state: Starting state type
            to_state: Ending state type
            direction: Directional type (HORIZONTAL, UPWARD, etc.)
            transformer: Function to transform from start to end state
        """
        key = (from_state, to_state, direction)
        self.path_registry[key] = transformer
        
    def transform(self, model, target_state, direction=GENERAL):
        """
        Transform a model to a target state along a specific directional path
        
        Args:
            model: The model to transform
            target_state: The desired end state
            direction: The directional path to follow
            
        Returns:
            Transformed model
        """
        from_state = model.get_state_type()
        
        # Look for exact path match
        key = (from_state, target_state, direction)
        if key in self.path_registry:
            return self.path_registry[key](model)
            
        # Fall back to general direction
        general_key = (from_state, target_state, self.GENERAL)
        if general_key in self.path_registry:
            return self.path_registry[general_key](model)
            
        # No path found
        raise ValueError(
            f"No transformation path found from {from_state} to {target_state} "
            f"with direction {direction}"
        )
        
    def find_path(self, from_state, to_state, preferred_directions=None):
        """
        Find available paths between states, with optional direction preferences
        
        Args:
            from_state: Starting state type
            to_state: Ending state type
            preferred_directions: List of directions in order of preference
            
        Returns:
            List of available paths sorted by preference
        """
        # Default to a standard preference order if none provided
        if preferred_directions is None:
            preferred_directions = [
                self.GENERAL, 
                self.HORIZONTAL, 
                self.UPWARD, 
                self.DOWNWARD,
                self.CIRCULAR, 
                self.STARTING, 
                self.PROLONGED
            ]
            
        # Find all available paths
        available_paths = []
        for key, transformer in self.path_registry.items():
            path_from, path_to, path_direction = key
            
            if path_from == from_state and path_to == to_state:
                available_paths.append((path_direction, transformer))
                
        # Sort by preference order
        def preference_key(item):
            direction = item[0]
            try:
                return preferred_directions.index(direction)
            except ValueError:
                return len(preferred_directions)  # Put unknown directions last
                
        return sorted(available_paths, key=preference_key)
```

## 10. Example Implementation with Navajo-Inspired Concepts

Here's how a CEREBRUM transformation inspired by Navajo verb structure might look:

```python
# Create template processor
template = NavajoInspiredTemplateProcessor()

# Register prefix functions for different template positions
template.register_prefix("aspect", "PERFECTIVE", 
    lambda context, **kwargs: {**context, "aspect": "completed"})
    
template.register_prefix("aspect", "IMPERFECTIVE", 
    lambda context, **kwargs: {**context, "aspect": "ongoing"})
    
template.register_prefix("subject", "FIRST_PERSON", 
    lambda context, **kwargs: {**context, "agent": "self"})
    
template.register_prefix("subject", "THIRD_PERSON", 
    lambda context, **kwargs: {**context, "agent": "external"})
    
template.register_prefix("object", "DEFINITE", 
    lambda context, identifier: {**context, "patient": {"id": identifier, "type": "specific"}})
    
template.register_prefix("object", "INDEFINITE", 
    lambda context, type_id: {**context, "patient": {"type": type_id, "specific": False}})
    
template.register_prefix("iterative", "REPEATED", 
    lambda context, count: {**context, "repeat": count})
    
template.register_prefix("disjunct", "INCORPORATE", 
    lambda context, component: {**context, "components": context.get("components", []) + [component]})
    
template.register_prefix("conjunct", "UPWARD", 
    lambda context: {**context, "direction": "upward"})
    
template.register_prefix("conjunct", "CIRCULAR", 
    lambda context: {**context, "direction": "circular"})

# Define stem function (core transformation)
def temperature_prediction_stem(context):
    """Core temperature prediction transformation (like a Navajo verb stem)"""
    # Extract context elements (set by prefixes)
    model = context["current"]
    direction = context.get("direction", "general")
    agent = context.get("agent", "system")
    patient = context.get("patient", {"type": "default"})
    aspect = context.get("aspect", "ongoing")
    repeat = context.get("repeat", 1)
    components = context.get("components", [])
    
    # Create result based on context
    results = []
    for _ in range(repeat):
        # Run prediction with the given context
        prediction = model.predict(
            direction=direction,
            agent=agent,
            target=patient,
            components=components
        )
        
        # Apply aspect-specific processing
        if aspect == "completed":
            # Finalize prediction (like perfective aspect)
            prediction.finalize()
        else:
            # Mark as preliminary (like imperfective aspect)
            prediction.mark_preliminary()
            
        results.append(prediction)
        
    return results[0] if repeat == 1 else results

# Configure template for a specific "verb" (transformation)
template.set_stem(temperature_prediction_stem)
template.set_prefix("aspect", "PERFECTIVE")
template.set_prefix("subject", "THIRD_PERSON")
template.set_prefix("object", "DEFINITE", "temp_sensor_1")
template.set_prefix("conjunct", "UPWARD")
template.set_prefix("iterative", "REPEATED", 3)
template.set_prefix("disjunct", "INCORPORATE", "humidity_model")

# Execute the transformation
input_model = ClimateModel()
results = template.execute(input_model)
```

## 11. Event Structure in Navajo and CEREBRUM

Navajo's handling of event structure offers insights for CEREBRUM's event processing:

```python
class NavajoInspiredEventProcessor:
    """
    Event processor inspired by Navajo event structure encoding.
    
    Handles events with an understanding of their internal structure,
    temporal relationships, and participant roles.
    """
    
    # Event type constants
    MOMENTARY = "momentary"    # Point-like event (perfective)
    DURATIVE = "durative"      # Extended event (imperfective)
    ITERATIVE = "iterative"    # Repeated event
    INCEPTIVE = "inceptive"    # Beginning of event
    TERMINATIVE = "terminative" # End of event
    CAUSATIVE = "causative"    # Caused event
    
    def __init__(self):
        self.event_handlers = {}
        self.active_events = {}
        
    def register_handler(self, event_type, participant_signature, handler_fn):
        """
        Register an event handler for a specific event type and participants
        
        Args:
            event_type: Type of event (MOMENTARY, DURATIVE, etc.)
            participant_signature: Dict mapping roles to required types
            handler_fn: Function to handle matching events
        """
        key = (event_type, frozenset(participant_signature.items()))
        self.event_handlers[key] = handler_fn
        
    def process_event(self, event_data):
        """
        Process an event based on its structure and participants
        
        Args:
            event_data: Dict with event type and participants
            
        Returns:
            Processing result
        """
        event_type = event_data.get("type", self.MOMENTARY)
        participants = event_data.get("participants", {})
        
        # Create signature for lookup
        participant_types = {
            role: participant.get("type") 
            for role, participant in participants.items()
        }
        signature = frozenset(participant_types.items())
        
        # Look for matching handler
        key = (event_type, signature)
        if key not in self.event_handlers:
            raise ValueError(f"No handler for event type {event_type} with participants {participant_types}")
            
        handler = self.event_handlers[key]
        
        # For durative events, track state
        if event_type == self.DURATIVE:
            event_id = event_data.get("id")
            if event_id:
                self.active_events[event_id] = {
                    "start_time": time.time(),
                    "data": event_data
                }
                
        # For terminative events, retrieve context from active event
        elif event_type == self.TERMINATIVE:
            event_id = event_data.get("id")
            if event_id and event_id in self.active_events:
                # Add duration information
                start_time = self.active_events[event_id]["start_time"]
                event_data["duration"] = time.time() - start_time
                
                # Clean up
                del self.active_events[event_id]
        
        # Process the event
        return handler(event_data)
```

## 12. Navajo-Inspired Extension Opportunities for CEREBRUM

Navajo's linguistic features suggest several innovative extensions for CEREBRUM:

1. **Templatic Transformation Pipeline**: Implement a strictly ordered, multi-position transformation pipeline inspired by Navajo's verb template.

2. **Aspect-Based Process Tracking**: Develop a system for tracking process states using concepts similar to Navajo's rich aspect system.

3. **Directional Path Modeling**: Create transformation routing based on spatial concepts like those encoded in Navajo directional prefixes.

4. **Participant Role Integration**: Integrate participant roles directly into transformation definitions, similar to how Navajo encodes subjects and objects in the verb.

5. **Incorporated Component Modeling**: Implement a system for incorporating subsidiary models into primary transformations, similar to Navajo's incorporation of nouns.

## 13. Technical Advantages of the Navajo Approach

The verb-centered approach of Navajo offers several technical advantages for CEREBRUM implementation:

1. **Processal Focus**: Centers on transformations and processes rather than static relationships.

2. **Temporal Awareness**: Built-in handling of temporal states and process evolution.

3. **Integrated Role Marking**: Participant roles are integrated into the process definition itself.

4. **Directional Precision**: Fine-grained control over transformation pathways and directions.

5. **Compositional Structure**: Clear rules for how complex transformations are composed from simpler elements.

## 14. Conclusion

Navajo's verb-centered linguistic system offers CEREBRUM a radical alternative to case-based models for understanding relationships between entities. While CEREBRUM's core design draws from case-marking languages, the Navajo approach suggests complementary mechanisms that emphasize process, transformation, and temporal-spatial relationships.

By incorporating concepts like templatic transformation ordering, aspect-based state tracking, and directional transformation pathways, CEREBRUM can develop more sophisticated capabilities for tracking complex, evolving processes that involve multiple participants and stages.

These Navajo-inspired extensions could be particularly valuable in scenarios requiring detailed process monitoring, pathway-specific transformations, and integrated tracking of entity roles within transformation processes.

## 15. References

1. Young, Robert W. and William Morgan. The Navajo Language: A Grammar and Colloquial Dictionary. University of New Mexico Press, 1987.
2. Faltz, Leonard M. The Navajo Verb: A Grammar for Students and Scholars. University of New Mexico Press, 1998.
3. Mithun, Marianne. The Languages of Native North America. Cambridge University Press, 1999.
4. Witherspoon, Gary. Language and Art in the Navajo Universe. University of Michigan Press, 1977.
5. Speas, Margaret. "Navajo Verb Prefixes and Clause Structure." Manuscript, University of Massachusetts, Amherst, 1990.
6. Webster, Anthony K. "Navajo Poetry, Linguistic Ideology, and Identity: The Case of the Poetry of Rex Lee Jim." Journal of Linguistic Anthropology, 19(1), 2009.
7. McDonough, Joyce. "The Navajo Sound System." Kluwer Academic Publishers, 2003.
8. Hardy, Heather K. "The Status of Stem Forms in Navajo." International Journal of American Linguistics, 84(2), 2018. 