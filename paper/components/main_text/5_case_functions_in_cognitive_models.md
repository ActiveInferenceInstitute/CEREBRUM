## Case Functions in Cognitive Model Systems

**Table 1: Case Functions in Cognitive Model Systems**

| Abbr | Case | Function in CEREBRUM | Example Usage |
|------|------|----------------------|---------------|
| **[NOM]** | **Nominative** | Model as active agent; produces predictions and exerts causal influence on other models | Model X generates predictions about data distributions; controls downstream processing |
| **[ACC]** | **Accusative** | Model as object of process; receives transformations and updates from other processes | Process optimizes Model X's parameters; quality assessment evaluates Model X |
| **[GEN]** | **Genitive** | Model as source/possessor; generates outputs, products, and derived models | System produces output from Model X; intelligence products derive from Model X |
| **[DAT]** | **Dative** | Model as recipient; receives and processes incoming data flows | System feeds data into Model X; Model X processes information from external sources |
| **[INS]** | **Instrumental** | Model as method/tool; implements analytical operations and procedures | System performs analysis via Model X; Model X executes analytical procedures |
| **[LOC]** | **Locative** | Model as context; establishes environmental constraints and parameters | System operates within Model X's parameters; environment exists as modeled by X |
| **[ABL]** | **Ablative** | Model as origin/cause; defines historical conditions and causal precursors | Insights derive from Model X; causal attributions trace to Model X |
| **[VOC]** | **Vocative** | Model as addressable entity; maintains callable interface with name activation | System activates Model X directly; documentation references Model X explicitly |

In intelligence production systems, these case relationships fulfill distinct functional roles: nominative models drive analytical processes; accusative models receive quality assessments; genitive models generate documentation; dative models process intelligence data; instrumental models provide methodological frameworks; locative models establish situational boundaries; ablative models represent analytical origins; and vocative models serve as directly addressable interfaces. Together, these case relationships create a structured framework for intelligence workflows, as illustrated in Figure 4.

The concept of 'case' extends beyond purely linguistic analysis. Notably, the structured approach inherent in linguistic case systems finds a compelling parallel in the domain of intelligence production, specifically within 'Case Management Systems'. These systems, designed to organize, track, and process complex information flows for decision-making, rely on assigning roles and relationships to data points—much like grammatical cases assign roles to sentence constituents. Examining how information is categorized, prioritized, and transformed within these operational frameworks offers valuable insights into the functional demands placed on cognitive systems that must similarly manage and act upon diverse, often ambiguous, streams of input. This convergence highlights a shared underlying principle: the need for structured frameworks to manage complexity, whether in language comprehension or strategic analysis.

![Generative Model Integration in Intelligence Case Management.](../figures/Figure_4.png){#fig:fig4}

## A Prototype Case-Bearing Model: Homeostatic Thermostat

Consider a cognitive model of a homeostatic thermostat that perceives room temperature and regulates it through connected heating and cooling systems:

* **Nominative [NOM]**: The thermostat actively generates temperature predictions and dispatches control signals, functioning as the primary agent in temperature regulation.
* **Accusative [ACC]**: The model becomes the object of optimization, with parameters updated based on prediction errors between expected and actual temperature readings.
* **Dative [DAT]**: The thermostat receives environmental temperature data streams and occupant comfort preferences as inputs.
* **Genitive [GEN]**: The model transforms to generate temperature regulation reports and system performance analytics.
* **Instrumental [INS]**: The thermostat functions as a computational tool implementing control algorithms for other systems requiring temperature management.
* **Locative [LOC]**: The model reconfigures to represent the contextual environment, modeling building thermal properties.
* **Ablative [ABL]**: The thermostat functions as the origin of historical temperature data and control decisions, providing causal explanations for current thermal conditions.

This single cognitive model thus assumes different functional roles while maintaining its core identity as a thermostat. Supplement 3 provides additional examples of case-bearing models in various application domains.

## Declinability of Active Inference Generative Models

At the core of CEREBRUM lies the concept of **declinability**—the capacity for generative models to assume different morphological and functional roles through case transformations, mirroring the declension patterns of nouns in morphologically rich languages. Unlike traditional approaches where models maintain fixed roles, CEREBRUM treats cognitive models as flexible entities capable of morphological adaptation to different operational contexts, with formal mathematical definitions provided in Supplement 11.

## Morphological Transformation of Generative Models

When an active inference generative model undergoes case transformation, it experiences systematic changes including:

1. **Functional Interfaces**: Input/output specifications adapt to match case role requirements
2. **Parameter Access Patterns**: Parameter exposure or constraint patterns shift based on case
3. **Prior Distributions**: Different cases employ different prior constraints on parameter values
4. **Update Dynamics**: State update mechanisms vary by case role
5. **Computational Resources**: Different cases receive different precision-weighted computational allocations

These transformational properties are summarized in Table 2:

**Table 2: Transformational Properties of Active Inference Generative Models Under Case Declensions**

| Case | Parametric Changes | Interface Transformations | Precision Weighting |
|------|-------------------|--------------------------|-------------------|
| **[NOM]** | Parameters configured for prediction generation | Outputs predictions; exposes forward inference pathways | Highest precision on likelihood mapping |
| **[ACC]** | Parameters configured for learning and adaptation | Receives transformations; exposes update interfaces | Highest precision on parameter updates |
| **[DAT]** | Parameters configured for input processing | Receives data flows; exposes input processing interfaces | Highest precision on incoming data |
| **[GEN]** | Parameters configured for output generation | Generates products; prioritizes output interfaces | Highest precision on generated outputs |
| **[INS]** | Parameters configured for methodological operations | Implements processes; exposes computational interfaces | Highest precision on procedural execution |
| **[LOC]** | Parameters configured for contextual representation | Provides environmental constraints; exposes contextual interfaces | Highest precision on contextual representation |
| **[ABL]** | Parameters configured for causal attribution | Functions as information source; exposes historical data | Highest precision on historical data |
| **[VOC]** | Parameters configured for identification and response | Maintains addressable interfaces; exposes command channels | Highest precision on identification cues |

## Active Inference Model Declension

Consider a perception-oriented generative model M with parameters θ, internal states s, and observational distribution p(o|s,θ). When declined across cases, this single model transforms as follows:

* **M[NOM]**: Actively generates predictions by sampling from p(o|s,θ), with all parameters fully accessible
* **M[ACC]**: Becomes the target of updates, with parameter gradients calculated from prediction errors
* **M[DAT]**: Configured to receive data flows, with specific input interfaces activated
* **M[GEN]**: Optimized to generate outputs, with output interfaces prioritized
* **M[INS]**: Functions as a computational method, exposing algorithmic interfaces
* **M[LOC]**: Provides contextual constraints for other models, with environmental parameters exposed
* **M[ABL]**: Serves as an information source, with historical data accessible
* **M[VOC]**: Functions as an addressable entity responding to direct invocation, with naming parameters activated

The Vocative case [VOC] optimizes models for name-based recognition and command reception, particularly relevant in synthetic intelligence environments where models must be selectively activated. Vocative models maintain specialized interfaces for handling direct commands, documentation references, and initialization requests—similar to how "Hey Siri" or "OK Google" activation phrases function for digital assistants. This creates a natural bridge between human language interaction and model orchestration.

This systematic pattern of transformations constitutes a complete "declension paradigm" for cognitive models, using precision-modulation to fulfill diverse functional roles while maintaining core identity. Figure 5 illustrates the workflow of case transformations.

![Model Workflows as Case Transformations - Sequence Diagram.](../figures/Figure_5.png){#fig:fig5}

The implications of this declension paradigm extend beyond individual models to entire model ecosystems, as further explored in Supplements 2 and 7.
