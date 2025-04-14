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

## 11.11 Ergative Case [ERG]

### 11.11.1 Semantic Role
Agent of transitive verbs in ergative-absolutive languages, causer of an action with direct impact.

### 11.11.2 Computational Role
Active transformer, causative agent, high-impact state modifier, direct intervention mechanism.

### 11.11.3 Formal Definition
A model $M$ in ergative case [ERG] is characterized by the following free energy formulation:

$$F_{ERG}[q] = D_{KL}[q(s, i) || p(s, i)] - \gamma_{ERG} \cdot E_q[\log p(o|s, i)]$$

Where $i$ represents intervention parameters that directly cause state changes. The precision weighting emphasizes intervention efficacy:

$$\gamma_{ERG} > 1.5$$

The intervention-specific gradient emphasizes causal impact:

$$\nabla_i F_{ERG} = \nabla_i D_{KL}[q(s, i) || p(s, i)] - \gamma_{ERG} \cdot \nabla_i E_q[\log p(o|s, i)]$$

### 11.11.4 Expected Input/Output Signature
- **Input:** Causal triggers, intervention specifications, impact objectives
- **Output:** High-impact transformations, direct state alterations, causal effects

### 11.11.5 Operational Definition
A model operates in ERG case when:
- It functions as a direct causal agent with high-impact interventions
- It generates substantial changes in target systems with minimal mediation
- Its operations have greater impact than complexity would suggest
- It maintains specialized intervention parameters optimized for causal efficacy

### 11.11.6 Implementation Requirements
- Intervention-focused architectures with amplification mechanisms
- High-precision causal parameters with direct state modification capabilities
- Optimization criteria that emphasize intervention impact magnitude
- Feedback mechanisms that monitor and adjust intervention efficacy

## 11.12 Allative Case [ALL]

### 11.12.1 Semantic Role
Destination, target location, or endpoint of movement or directional action.

### 11.12.2 Computational Role
Goal state representation, target configuration, convergence point, attractor state.

### 11.12.3 Formal Definition
A model $M$ in allative case [ALL] is characterized by the following free energy formulation:

$$F_{ALL}[q] = D_{KL}[q(s, d) || p(s, d)] - E_q[\log p(o|s, d)]$$

Where $d$ represents destination or target state parameters. The precision weighting emphasizes goal stability:

$$\pi_{ALL}(d) > \pi_{ALL}(s)$$

The goal-specific gradient emphasizes attractor dynamics:

$$\nabla_d F_{ALL} = \nabla_d D_{KL}[q(s, d) || p(s, d)] - \nabla_d E_q[\log p(o|s, d)]$$

### 11.12.4 Expected Input/Output Signature
- **Input:** Target queries, goal state parameters, convergence criteria
- **Output:** Attractor descriptions, convergence metrics, distance-to-goal indicators

### 11.12.5 Operational Definition
A model operates in ALL case when:
- It represents destination states or target configurations
- It serves as an attractor in dynamical systems
- It provides reference signals for convergent processes
- It maintains stable representations of desired end states

### 11.12.6 Implementation Requirements
- Attractor dynamics with stable fixed points
- Goal representation architectures with distance metrics
- Parameter regimes optimized for representational stability
- Mechanisms for evaluating proximity to target states

## 11.13 Comitative Case [COM]

### 11.13.1 Semantic Role
Accompaniment, partnership, or entity acting in conjunction with another.

### 11.13.2 Computational Role
Co-processing, collaborative computation, parallel operation, mutual constraint.

### 11.13.3 Formal Definition
A model $M$ in comitative case [COM] is characterized by the following free energy formulation:

$$F_{COM}[q] = D_{KL}[q(s, p) || p(s, p)] - E_q[\log p(o|s, p)]$$

Where $p$ represents partnership or collaboration parameters. The precision weighting emphasizes coordination:

$$\pi_{COM}(p) \approx \pi_{COM}(s)$$

The collaboration-specific gradient emphasizes mutual adjustment:

$$\nabla_p F_{COM} = \nabla_p D_{KL}[q(s, p) || p(s, p)] - \nabla_p E_q[\log p(o|s, p)]$$

### 11.13.4 Expected Input/Output Signature
- **Input:** Coordination signals, collaboration requests, synchronization cues
- **Output:** Joint representations, coordinated states, mutual constraints

### 11.13.5 Operational Definition
A model operates in COM case when:
- It functions in coordination with other models rather than independently
- It maintains representations that synchronize with partner models
- It adjusts its operations based on collaborative constraints
- Its effectiveness depends on successful coordination with partners

### 11.13.6 Implementation Requirements
- Synchronization mechanisms with partner models
- Parameter sharing architectures for collaborative computation
- Multi-model optimization objectives that reward coordination
- Interface designs that support bidirectional constraint satisfaction

## 11.14 Extended Case Relationships and Transformations

The extended case system introduces additional functional roles and transformation pathways within the CEREBRUM framework, enhancing its expressivity and computational flexibility.

### 11.14.1 Additional Case Transformations

| Source Case | Target Case | Transformation Description | Primary Parameter Shifts |
|-------------|-------------|----------------------------|--------------------------|
| NOM → ERG | Generation to Intervention | Model shifts from generating predictions to causing direct impacts | $\alpha_{NOM} \to \gamma_{ERG}$ (precision shift from accuracy to impact) |
| ERG → INS | Intervention to Process | Model shifts from direct causation to implementing processes | $\gamma_{ERG} \to \pi_{INS}(a)$ (precision shift from impact to action execution) |
| ABL → ALL | Source to Destination | Model shifts from representing origins to representing destinations | $\pi_{ABL}(o) \to \pi_{ALL}(d)$ (precision shift from origin to destination) |
| LOC → ALL | Context to Goal | Model shifts from representing environment to representing target states | $\pi_{LOC}(e) \to \pi_{ALL}(d)$ (precision shift from environment to goal) |
| ACC → COM | Update to Collaboration | Model shifts from receiving updates to collaborative processing | $\beta_{ACC} \to \pi_{COM}(p)$ (precision shift from complexity to coordination) |
| COM → DAT | Collaboration to Goal | Model shifts from collaborative processing to goal representation | $\pi_{COM}(p) \to \pi_{DAT}(g)$ (precision shift from coordination to goals) |

### 11.14.2 Extended Case Properties Summary

| Case | Primary Focus | Precision Emphasis | Interface Direction | Update Priority | Typical Role |
|------|--------------|-------------------|---------------------|----------------|-------------|
| ERG | Causal intervention | Impact ($\gamma_{ERG} > 1.5$) | Interventional | Causal efficacy | Direct modifier |
| ALL | Goal representation | Destination parameters ($\pi_{ALL}(d)$) | Attractional | Goal stability | Target state |
| COM | Collaboration | Balanced ($\pi_{COM}(p) \approx \pi_{COM}(s)$) | Coordinational | Mutual consistency | Co-processor |

### 11.14.3 Computational Implications of Extended Case System

The extended case system enhances the CEREBRUM framework with additional functional specializations:

1. **Enhanced Causal Modalities**: The ERG case provides explicit representation for high-impact causal interventions distinct from general transformations.

2. **Improved Goal-Directed Processing**: The ALL case complements the ABL case, providing balanced representation of process endpoints alongside origins.

3. **Collaborative Computation**: The COM case enables explicit modeling of coordinated processing, supporting multi-model constraint satisfaction.

4. **Richer Transformation Pathways**: The extended transformation network provides more nuanced ways to shift functional roles within complex computational systems.

5. **Expanded Compositional Grammar**: Additional cases enable more expressive model composition patterns, supporting more sophisticated computational architectures.

This extended case system further enhances CEREBRUM's capacity to represent and implement diverse computational relationships while maintaining a principled linguistic foundation. 

## 11.15 Case Composition and Hierarchical Structures

The CEREBRUM framework supports sophisticated compositional patterns where models can participate in multiple case relationships simultaneously or hierarchically. This section details the principles and mechanisms by which case assignments can be composed to create complex computational structures.

### 11.15.1 Case Composition Principles

Case compositions in CEREBRUM follow several key principles:

1. **Dominance Hierarchies**: When a model participates in multiple case relationships, case dominance determines which free energy formulation takes precedence:
   
   $$F_{combined}[q] = \omega_1 F_{case1}[q] + \omega_2 F_{case2}[q]$$
   
   Where $\omega_1$ and $\omega_2$ are dominance weights satisfying $\omega_1 + \omega_2 = 1$.

2. **Context-Sensitive Case Assignments**: Models may shift their case assignments based on computational context:
   
   $$P(Case_i | Context_j) = \frac{exp(\psi_{i,j})}{\sum_k exp(\psi_{k,j})}$$
   
   Where $\psi_{i,j}$ represents the compatibility between Case $i$ and Context $j$.

3. **Recursive Case Structures**: Case assignments can be nested, with a model-in-case containing sub-models with their own case assignments:
   
   $$F_{outer:inner}[q] = F_{outer}[q_{outer}] + \lambda \cdot F_{inner}[q_{inner}|q_{outer}]$$
   
   Where $\lambda$ represents the coupling strength between hierarchical levels.

4. **Case Inheritance**: Sub-models may inherit case properties from parent models, creating consistent computational patterns:
   
   $$P(Case_{child} = c | Case_{parent} = p) = I(c,p)$$
   
   Where $I(c,p)$ is the inheritance matrix specifying case transition probabilities.

### 11.15.2 Common Case Composition Patterns

Several case composition patterns are particularly important in CEREBRUM architectures:

#### 11.15.2.1 NOM-GEN Composition

The NOM-GEN composition creates generative models that maintain explicit relationship representations:

$$F_{NOM-GEN}[q] = D_{KL}[q(s,r) || p(s,r)] - \alpha_{NOM} \cdot E_q[\log p(o|s,r)]$$

This pattern is essential for generative models operating within structured relationship networks.

#### 11.15.2.2 ACC-COM Composition

The ACC-COM composition creates collaborative update structures that coordinate belief revisions across multiple models:

$$F_{ACC-COM}[q] = \beta_{ACC} \cdot D_{KL}[q(s,p) || p(s,p)] - E_q[\log p(o|s,p)]$$

This pattern enables coordinated learning across model ensembles.

#### 11.15.2.3 DAT-ALL Composition

The DAT-ALL composition creates goal-directed recipient structures that balance current reception with target state representation:

$$F_{DAT-ALL}[q] = D_{KL}[q(s,g,d) || p(s,g,d)] - E_q[\log p(o|s,g,d)]$$

This pattern is crucial for models that serve as waypoints in goal-directed information flows.

#### 11.15.2.4 INS-ERG Composition

The INS-ERG composition creates high-impact transformation agents that combine process implementation with direct causal intervention:

$$F_{INS-ERG}[q] = D_{KL}[q(s,a,i) || p(s,a,i)] - \gamma_{ERG} \cdot E_q[\log p(o|s,a,i)]$$

This pattern enables precise, high-efficacy transformations in complex processing pipelines.

### 11.15.3 Case Composition Implementation

Implementing case compositions requires specialized architectural components:

1. **Interface Adaptors**: Components that reconcile potentially conflicting interface requirements from different case assignments.

2. **Multi-Case Optimizers**: Gradient-based optimization processes that balance multiple free energy objectives:
   
   $$\nabla_q F_{combined} = \omega_1 \nabla_q F_{case1} + \omega_2 \nabla_q F_{case2}$$

3. **Case Transition Controllers**: Mechanisms that manage smooth transitions between dominant case assignments:
   
   $$\omega_i(t+1) = \omega_i(t) + \eta \cdot \nabla_{\omega} F_{combined}$$

4. **Hierarchical Message Passing**: Schemes that coordinate information flow across multiple levels of nested case structures:
   
   $$m_{upper \to lower} = f_{down}(q_{upper})$$
   $$m_{lower \to upper} = f_{up}(q_{lower})$$

### 11.15.4 Computational Benefits of Case Composition

Case composition provides several critical advantages in CEREBRUM implementations:

1. **Functional Polymorphism**: Models can serve multiple functional roles simultaneously, increasing computational efficiency.

2. **Graceful Degradation**: If a particular case-specific functionality is compromised, composed models can fall back to alternative functional modes.

3. **Computational Factorization**: Complex computational tasks can be factorized into simpler case-specific sub-computations.

4. **Emergent Capabilities**: Novel computational capabilities emerge from the interaction of composed case assignments that are not present in any individual case.

5. **Learning Transfer**: Case composition facilitates transfer learning by maintaining certain case-specific parameters while adapting others.

Case composition represents a core innovation of the CEREBRUM framework, enabling a vast space of sophisticated computational architectures while maintaining the clarity and rigor of the linguistic case metaphor. By composing cases at multiple levels, CEREBRUM models can implement computational structures of arbitrary complexity without sacrificing the conceptual clarity provided by the case system.

## 11.16 Cross-Linguistic Case Parallels

The CEREBRUM case system draws inspiration from linguistic case systems across diverse human languages, leveraging cross-linguistic patterns to create a comprehensive and universal computational framework. This section explores how CEREBRUM's formal case definitions align with case systems from various language families.

### 11.16.1 Indo-European Case Parallels

CEREBRUM's core cases show strong parallels with Indo-European case systems:

| CEREBRUM Case | Sanskrit | Latin | Russian | Greek | Computational Significance |
|---------------|----------|-------|---------|-------|----------------------------|
| NOM | Nominative | Nominative | Именительный | Ονομαστική | Agent-driven computation patterns consistent across IE languages |
| ACC | Accusative | Accusative | Винительный | Αιτιατική | Direct object computation consistently marks information recipients |
| GEN | Genitive | Genitive | Родительный | Γενική | Possessive/relational computation reflects IE genitive functions |
| DAT | Dative | Dative | Дательный | Δοτική | Indirect object/goal patterns align with cross-IE semantic roles |
| INS | Instrumental | Ablative (partial) | Творительный | Instrumental (ancient) | Tools/means computation reflects cross-IE patterns |
| LOC | Locative | Locative (archaic) | Предложный | Locative (ancient) | Location/context encoding aligns with IE locative cases |
| ABL | Ablative | Ablative | - | Ablative (ancient) | Source/origin semantics preserved across available IE cases |
| VOC | Vocative | Vocative | Звательный (old) | Κλητική | Addressee/attention relationships consistent with IE vocatives |

Notable computationally-relevant patterns include the widespread nominative-accusative alignment in IE languages, mirroring CEREBRUM's agent-patient computational asymmetry.

### 11.16.2 Non-Indo-European Case Systems

CEREBRUM's extended case system incorporates insights from non-IE language families:

#### 11.16.2.1 Ergative-Absolutive Languages

The ERG case in CEREBRUM draws inspiration from ergative-absolutive languages like Basque, Georgian, and many Australian aboriginal languages:

$$F_{ERG}[q] = D_{KL}[q(s, i) || p(s, i)] - \gamma_{ERG} \cdot E_q[\log p(o|s, i)]$$

Computational parallels include:
- High-impact causal intervention similar to ergative case marking transitive agents
- Distinct treatment of high-agency computational processes vs. passive state holders
- Precision weighting ($\gamma_{ERG} > 1.5$) reflecting the marked nature of ergative case

#### 11.16.2.2 Agglutinative Case Systems

CEREBRUM's compositional case approach draws from agglutinative languages with extensive case systems:

| Language | Case Count | CEREBRUM Parallel |
|----------|------------|-------------------|
| Finnish | 15 cases | Compositional case patterns with specialized spatial relationships |
| Hungarian | 18 cases | Fine-grained goal/location distinctions mirrored in ALL-LOC distinctions |
| Turkish | 6 core + derived | Case stacking parallels case composition rules |
| Japanese | Case particles | Interface-focused case marking similar to VOC and COM implementations |

The CEREBRUM approach to case composition (Section 11.15) particularly reflects the compositional nature of case marking in these languages, where multiple case markers can combine to create complex semantic relationships.

#### 11.16.2.3 Polysynthetic Languages

Polysynthetic languages with incorporated case functions provide insights for CEREBRUM's hierarchical case structures:

```
Inuktitut example: tusaa-vunga (I hear)
CEREBRUM parallel: F_{NOM:ACC}[q] = F_{NOM}[q_{outer}] + λ·F_{ACC}[q_{inner}|q_{outer}]
```

The recursive case formulation in Section 11.15.1 mirrors how polysynthetic languages embed case relationships within complex word-sentences.

### 11.16.3 Universal Case Tendencies

Cross-linguistic research reveals case patterns that appear to be computational universals, strongly reflected in CEREBRUM's design:

1. **Markedness Asymmetry**: Across languages, subject/agent roles (NOM/ERG) are often unmarked or minimally marked, while object/patient roles receive more explicit marking. CEREBRUM's precision weighting follows this pattern:

   $$\alpha_{NOM} > 1 \text{ (enhancing prediction)} \quad \text{vs.} \quad \beta_{ACC} < 1 \text{ (reducing complexity cost)}$$

2. **Animacy Hierarchies**: Languages often treat cases differently based on animacy. CEREBRUM's context-sensitive case assignment parallels this:

   $$P(Case_i | Context_j) = \frac{exp(\psi_{i,j})}{\sum_k exp(\psi_{k,j})}$$

3. **Semantic Role Universality**: Despite surface differences, core semantic roles (agent, patient, instrument, location, etc.) appear across all language families, validating CEREBRUM's case-based computational abstractions.

4. **Compositionality Constraints**: Languages constrain how cases can combine in principled ways. CEREBRUM's case composition rules formalize similar constraints:
   
   ```
   Valid: NOM-GEN composition
   Invalid: *VOC-ABL composition
   ```

### 11.16.4 Computational Implementations of Linguistic Insights

CEREBRUM's implementation leverages these cross-linguistic insights in several key ways:

1. **Differential Precision**: Case-specific precision parameters ($\alpha_{NOM}$, $\beta_{ACC}$, etc.) implement markedness patterns seen across languages.

2. **Interface Asymmetries**: The input/output signatures for each case reflect linguistic argument structure patterns.

3. **Hierarchical Composition**: The case composition system draws from how cases stack and interact in agglutinative and polysynthetic languages.

4. **Case Transitions**: Transformation rules between cases (Section 11.10.1) parallel how languages grammaticalize case shifts for different functions.

By grounding its computational framework in linguistic universals about case systems, CEREBRUM achieves a balance between domain-general computing principles and human-interpretable functional roles. This cross-linguistic foundation enhances both the theoretical coherence and practical implementability of the framework. 