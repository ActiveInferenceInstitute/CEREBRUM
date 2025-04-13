# Supplement 11: Formal Definitions of Core Linguistic Cases

This supplement provides rigorous mathematical and operational definitions for the traditional linguistic cases as implemented within the CEREBRUM framework.

## 11.1 Introduction

The CEREBRUM framework adopts and adapts traditional linguistic case systems as a foundational organizing principle for model roles and transformations. This supplement formalizes each core case, detailing its semantic origin, computational interpretation, mathematical formulation, and operational characteristics within the CEREBRUM context.

The formal definitions presented here complement the novel cases described in Supplement 2, providing a complete picture of the case system underpinning CEREBRUM. Each case definition includes precise mathematical formulations of free energy components, specifying how precision weighting and message passing operate in models assigned to that case.

## 11.2 Nominative Case [NOM]

### 11.2.1 Semantic Role
Agent, performer, experiencer, or primary subject of an action or state.

### 11.2.2 Computational Role
Prediction generation, model source, primary generative process.

### 11.2.3 Formal Definition
A model $M$ in nominative case [NOM] is characterized by the following free energy formulation:

$$F_{NOM}[q] = D_{KL}[q(s) || p(s)] - \alpha_{NOM} \cdot E_q[\log p(o|s)]$$

With precision weighting that emphasizes generative accuracy:

$$\alpha_{NOM} > 1$$

The message passing operations prioritize forward prediction over belief updates:

$$\nabla_q F_{NOM} \approx -\alpha_{NOM} \cdot \nabla_q E_q[\log p(o|s)] + \nabla_q D_{KL}[q(s) || p(s)]$$

### 11.2.4 Expected Input/Output Signature
- **Input:** Context information, cues for prediction generation
- **Output:** Predictions, generated observations, forward model outcomes

### 11.2.5 Operational Definition
A model operates in NOM case when:
- It functions as a primary source of predictions or state representations
- It actively generates outputs based on its current belief state
- Its interface emphasizes outward information flow
- It maintains relatively stable internal states rather than rapidly updating to external inputs

### 11.2.6 Implementation Requirements
- Forward-pass dominant architecture with strong generative components
- Relatively higher computational allocation to prediction generation than belief updating
- Stable prior distributions that resist rapid modification
- Parameter configurations that favor output precision over input sensitivity

## 11.3 Accusative Case [ACC]

### 11.3.1 Semantic Role
Patient, direct object, or entity directly affected by an action.

### 11.3.2 Computational Role
Receiving updates, target of belief revision, state modification recipient.

### 11.3.3 Formal Definition
A model $M$ in accusative case [ACC] is characterized by the following free energy formulation:

$$F_{ACC}[q] = \beta_{ACC} \cdot D_{KL}[q(s) || p(s)] - E_q[\log p(o|s)]$$

With precision weighting that emphasizes efficient belief updates:

$$\beta_{ACC} < 1$$

The message passing operations prioritize rapid belief updating based on new information:

$$\nabla_q F_{ACC} \approx -\nabla_q E_q[\log p(o|s)] + \beta_{ACC} \cdot \nabla_q D_{KL}[q(s) || p(s)]$$

### 11.3.4 Expected Input/Output Signature
- **Input:** Direct updates, modification signals, belief revision information
- **Output:** Updated state representations, change confirmations

### 11.3.5 Operational Definition
A model operates in ACC case when:
- It serves as the direct recipient of updates or modifications
- Its primary function is to efficiently revise its internal state
- It prioritizes integrating new information over maintaining prior beliefs
- It exhibits parameter changes directly coupled to input signals

### 11.3.6 Implementation Requirements
- Backward-pass dominant architecture with efficient update mechanisms
- Parameter configurations that favor rapid learning over stability
- Higher learning rates or adaptation coefficients
- Mechanisms for rapidly incorporating new evidence into belief distributions

## 11.4 Genitive Case [GEN]

### 11.4.1 Semantic Role
Possessor, source, or entity indicating relationship or association.

### 11.4.2 Computational Role
Relationship representation, context provision, parameter source, hierarchical link.

### 11.4.3 Formal Definition
A model $M$ in genitive case [GEN] is characterized by the following free energy formulation:

$$F_{GEN}[q] = D_{KL}[q(s, r) || p(s, r)] - E_q[\log p(o|s, r)]$$

Where $r$ represents explicit relationship parameters linking the model to other entities. The GEN-specific message passing emphasizes relationship maintenance:

$$\nabla_r F_{GEN} = \nabla_r D_{KL}[q(s, r) || p(s, r)] - \nabla_r E_q[\log p(o|s, r)]$$

With high precision assigned to relationship parameters:

$$\pi_{GEN}(r) > \pi_{GEN}(s)$$

### 11.4.4 Expected Input/Output Signature
- **Input:** Relationship queries, context requests, parameter requests
- **Output:** Associated parameters, contextual information, relationship specifications

### 11.4.5 Operational Definition
A model operates in GEN case when:
- It functions as a source of parameters or context for other models
- It maintains and provides information about relationships between entities
- It responds to queries about associated elements or hierarchical structures
- Its primary value comes from the relationships it represents rather than direct actions

### 11.4.6 Implementation Requirements
- Relational database-like structures for storing associative information
- Graph-based representations with explicit edge parameterization
- Parameter-sharing mechanisms for linked model components
- Optimization objectives that prioritize relational consistency

## 11.5 Dative Case [DAT]

### 11.5.1 Semantic Role
Indirect object, recipient, beneficiary, or goal of an action.

### 11.5.2 Computational Role
Information destination, indirect update recipient, interaction goal, routing endpoint.

### 11.5.3 Formal Definition
A model $M$ in dative case [DAT] is characterized by the following free energy formulation:

$$F_{DAT}[q] = D_{KL}[q(s, g) || p(s, g)] - E_q[\log p(o|s, g)]$$

Where $g$ represents goal parameters that shape how information is received and processed. The precision weighting balances state updates against goal alignment:

$$\pi_{DAT}(g) \approx \pi_{DAT}(s)$$

The update equations specifically incorporate goal-directed processing:

$$\nabla_q F_{DAT} = \nabla_q D_{KL}[q(s, g) || p(s, g)] - \nabla_q E_q[\log p(o|s, g)]$$

### 11.5.4 Expected Input/Output Signature
- **Input:** Indirect updates, goal-relevant information, mediated signals
- **Output:** Goal state information, transformation results, reception confirmations

### 11.5.5 Operational Definition
A model operates in DAT case when:
- It serves as an indirect recipient of actions or information
- It represents goals or intended outcomes of processes
- It mediates or routes information rather than being a final endpoint
- It maintains goal representations that influence how inputs are processed

### 11.5.6 Implementation Requirements
- Goal-conditioned processing pipelines
- Architectures with explicit goal parameter representations
- Information routing mechanisms based on goal states
- Optimization criteria that balance immediate updates with goal-directed outcomes

## 11.6 Instrumental Case [INS]

### 11.6.1 Semantic Role
Instrument, means, or tool used to accomplish an action.

### 11.6.2 Computational Role
Transformation application, function execution, process implementation, policy enactment.

### 11.6.3 Formal Definition
A model $M$ in instrumental case [INS] is characterized by the following free energy formulation:

$$F_{INS}[q] = D_{KL}[q(s, a) || p(s, a)] - E_q[\log p(o|s, a)]$$

Where $a$ represents action or transformation parameters. The precision weighting emphasizes effective action execution:

$$\pi_{INS}(a) > \pi_{INS}(s)$$

The gradient includes specific action-optimization terms:

$$\nabla_a F_{INS} = \nabla_a D_{KL}[q(s, a) || p(s, a)] - \nabla_a E_q[\log p(o|s, a)]$$

### 11.6.4 Expected Input/Output Signature
- **Input:** Function arguments, transformation specifications, process inputs
- **Output:** Transformed results, process outputs, action outcomes

### 11.6.5 Operational Definition
A model operates in INS case when:
- It serves as a means to accomplish transformations or processes
- It implements well-defined functions that map inputs to outputs
- Its primary value comes from the transformations it performs
- It maintains specialized parameters optimized for particular operations

### 11.6.6 Implementation Requirements
- Function composition architectures
- Specialized transformation layers or components
- Parameter optimization focused on transformation fidelity
- Input-output mapping with minimal state maintenance

## 11.7 Locative Case [LOC]

### 11.7.1 Semantic Role
Location, place, or position where an action occurs or state exists.

### 11.7.2 Computational Role
Context provision, environment representation, state container, reference frame.

### 11.7.3 Formal Definition
A model $M$ in locative case [LOC] is characterized by the following free energy formulation:

$$F_{LOC}[q] = D_{KL}[q(s, e) || p(s, e)] - E_q[\log p(o|s, e)]$$

Where $e$ represents environmental or contextual parameters. The precision weighting emphasizes contextual stability:

$$\pi_{LOC}(e) > \pi_{LOC}(s)$$

The update equations prioritize context maintenance:

$$\nabla_e F_{LOC} = \nabla_e D_{KL}[q(s, e) || p(s, e)] - \nabla_e E_q[\log p(o|s, e)]$$

### 11.7.4 Expected Input/Output Signature
- **Input:** Context queries, environmental parameter requests, situational information requests
- **Output:** Environmental parameters, contextual information, situational representations

### 11.7.5 Operational Definition
A model operates in LOC case when:
- It serves as a representation of the environment or context
- It provides stable background parameters for other processes
- It maintains information about spatial, temporal, or conceptual frameworks
- It responds to queries about the current operational context

### 11.7.6 Implementation Requirements
- Context-encoding architectures with high-dimensional state spaces
- Slow-changing parameter regimes for environmental stability
- Efficient query mechanisms for contextual information retrieval
- State representations that condition other models' operations

## 11.8 Ablative Case [ABL]

### 11.8.1 Semantic Role
Source, origin, cause, or starting point of movement or action.

### 11.8.2 Computational Role
Information origin, causal source, initialization provider, trigger point.

### 11.8.3 Formal Definition
A model $M$ in ablative case [ABL] is characterized by the following free energy formulation:

$$F_{ABL}[q] = D_{KL}[q(s, o) || p(s, o)] - E_q[\log p(o'|s, o)]$$

Where $o$ represents origin parameters and $o'$ represents downstream observations. The precision weighting emphasizes source accuracy:

$$\pi_{ABL}(o) > \pi_{ABL}(s)$$

The source-specific gradient focuses on causal origin representation:

$$\nabla_o F_{ABL} = \nabla_o D_{KL}[q(s, o) || p(s, o)] - \nabla_o E_q[\log p(o'|s, o)]$$

### 11.8.4 Expected Input/Output Signature
- **Input:** Triggering signals, initialization requests, source queries
- **Output:** Initial values, originating information, causal indicators

### 11.8.5 Operational Definition
A model operates in ABL case when:
- It functions as a source of causal processes or information flows
- It provides initialization values for other processes
- It represents the origin point of transformations or sequences
- It maintains and provides information about where processes began

### 11.8.6 Implementation Requirements
- Initialization parameter storage with high precision
- Causal tracking mechanisms for process origins
- Architectural components for transformative triggering
- Event-origin association mechanisms

## 11.9 Vocative Case [VOC]

### 11.9.1 Semantic Role
Addressee, entity being directly called or addressed.

### 11.9.2 Computational Role
Interface activation, attention target, communication endpoint, selective listener.

### 11.9.3 Formal Definition
A model $M$ in vocative case [VOC] is characterized by the following free energy formulation:

$$F_{VOC}[q] = D_{KL}[q(s, c) || p(s, c)] - E_q[\log p(o|s, c)]$$

Where $c$ represents communication or attention parameters. The precision weighting emphasizes selective attention:

$$\pi_{VOC}(c) > \pi_{VOC}(s)$$

The attention-specific update equations highlight selective information processing:

$$\nabla_c F_{VOC} = \nabla_c D_{KL}[q(s, c) || p(s, c)] - \nabla_c E_q[\log p(o|s, c)]$$

### 11.9.4 Expected Input/Output Signature
- **Input:** Attention signals, activation cues, addressing patterns
- **Output:** Availability indicators, attention confirmation, readiness signals

### 11.9.5 Operational Definition
A model operates in VOC case when:
- It selectively attends to communication directed specifically to it
- It serves as an explicit target for interaction
- It acts as a communication endpoint rather than a mediator
- It implements activation gates that control when it processes input

### 11.9.6 Implementation Requirements
- Attention mechanisms with selective filtering
- Activation gate architectures with identity-based triggers
- Signal detection components for targeted addressing
- Computational efficiency through selective processing

## 11.10 Case Relationships and Transformations

The core cases defined above form a structured system of functional roles that models can assume within the CEREBRUM framework. These cases are not independent but rather form a coherent system with well-defined transformation paths between them.

### 11.10.1 Common Case Transformations

| Source Case | Target Case | Transformation Description | Primary Parameter Shifts |
|-------------|-------------|----------------------------|--------------------------|
| NOM → ACC | Prediction to Update | Model shifts from generating predictions to receiving updates | $\alpha_{NOM} \to \beta_{ACC}$ (precision shift from accuracy to complexity) |
| ACC → NOM | Update to Prediction | Model shifts from receiving updates to generating predictions | $\beta_{ACC} \to \alpha_{NOM}$ (precision shift from complexity to accuracy) |
| GEN → DAT | Relation to Goal | Model shifts from representing relationships to representing goals | $\pi_{GEN}(r) \to \pi_{DAT}(g)$ (precision shift from relationships to goals) |
| INS → LOC | Process to Context | Model shifts from implementing transformations to providing context | $\pi_{INS}(a) \to \pi_{LOC}(e)$ (precision shift from actions to environment) |
| ABL → INS | Source to Process | Model shifts from being an origin to implementing a process | $\pi_{ABL}(o) \to \pi_{INS}(a)$ (precision shift from origin to action) |
| VOC → NOM | Attention to Generation | Model shifts from receiving attention to generating content | $\pi_{VOC}(c) \to \alpha_{NOM}$ (precision shift from attention to accuracy) |
| LOC → GEN | Context to Relation | Model shifts from representing environment to representing relationships | $\pi_{LOC}(e) \to \pi_{GEN}(r)$ (precision shift from environment to relationships) |

### 11.10.2 Case Properties Summary Table

| Case | Primary Focus | Precision Emphasis | Interface Direction | Update Priority | Typical Role |
|------|--------------|-------------------|---------------------|----------------|-------------|
| NOM | Prediction generation | Accuracy ($\alpha_{NOM} > 1$) | Output-dominant | Stable beliefs | Generative model |
| ACC | Belief updating | Complexity ($\beta_{ACC} < 1$) | Input-dominant | Rapid updates | Update target |
| GEN | Relationship representation | Relationship parameters ($\pi_{GEN}(r)$) | Query-response | Relational consistency | Parameter source |
| DAT | Goal representation | Balanced ($\pi_{DAT}(g) \approx \pi_{DAT}(s)$) | Mediational | Goal-directed updates | Indirect recipient |
| INS | Action execution | Action parameters ($\pi_{INS}(a)$) | Transformational | Process optimization | Function implementation |
| LOC | Context provision | Environmental parameters ($\pi_{LOC}(e)$) | Contextual | Contextual stability | Environment representation |
| ABL | Source representation | Origin parameters ($\pi_{ABL}(o)$) | Initiating | Origin accuracy | Causal source |
| VOC | Selective attention | Communication parameters ($\pi_{VOC}(c)$) | Communicational | Attention gating | Interaction target |

### 11.10.3 Computational Implications of Case Assignment

The assignment of a model to a specific case has significant implications for its computational behavior:

1. **Parameter Update Frequency**: NOM and LOC cases typically have slower parameter updates, while ACC cases update rapidly in response to inputs.

2. **Computational Resource Allocation**: Different cases require different computational resources - INS cases prioritize transformation efficiency, while GEN cases emphasize relationship storage and retrieval.

3. **Interface Design**: Case assignment shapes interface requirements - VOC cases need selective attention mechanisms, while DAT cases require goal-conditioned processing pipelines.

4. **Composition Rules**: Cases determine how models can be composed - NOM cases typically feed into ACC cases, while GEN cases provide parameters to other models.

5. **Error Handling**: Each case has characteristic error modes - NOM cases may suffer from generative inaccuracy, while ACC cases might exhibit belief instability.

This comprehensive case system provides a principled foundation for the design and implementation of CEREBRUM models, ensuring consistent functional roles across the framework. 