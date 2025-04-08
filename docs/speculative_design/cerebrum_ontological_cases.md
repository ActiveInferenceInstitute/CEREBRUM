# Ontological Negotiation and Affordance Alignment via CEREBRUM Cases

## Introduction: Cases as Ontological Stances

The CEREBRUM framework (`CEREBRUM.md`) provides a grammar for model interaction based on functional roles (cases). Separately, work on joint affordances and ontological negotiation (e.g., `joint_ontological_underwriting_affordances.md`, `joint_distributions_ontological_affordances.md`) explores how different agents or models might perceive different possibilities for action (affordances) based on potentially differing underlying assumptions about the nature of reality (ontologies). Can these two streams be integrated? Can CEREBRUM cases do more than just define functional roles – could they also signal or constrain a model's **ontological commitments** or its **perception of affordances**?

This document speculates on enriching the CEREBRUM framework by associating cases with specific ontological stances or affordance landscapes. We explore how case assignments might mediate interactions between models with potentially incompatible world-models (generative models) and propose novel cases specifically designed for ontological negotiation and affordance alignment, drawing connections to Active Inference.

## Cases Shaping Ontology and Affordances

The core idea is that a model's case assignment influences the aspects of its internal generative model and its perceived action possibilities (policy space) that are currently active or assigned high precision.

### Ontological and Affordance Mappings for Standard CEREBRUM Cases

**Table 1: Traditional CEREBRUM Cases Mapped to Ontological Focus and Affordance Types**

| Case | Primary Function | Ontological Focus | Affordance Types | Example |
|------|-----------------|-------------------|-----------------|---------|
| [NOM] | Actor/Agent | Entities, actions, causality, goals | Direct intervention, transformation, control | A model in [NOM] perceives the world primarily in terms of entities it can act upon and causal relationships it can engage with |
| [ACC] | Object/Update Target | Self-structure, parameter space, transformations | Being modified, learning, adaptation | A model in [ACC] foregrounds its own modifiable structure in its ontology and attends to affordances for updating itself |
| [GEN] | Source/Producer | Generative patterns, outputs, transmission | Production, distribution, influence | A model in [GEN] prioritizes understanding generative relationships and opportunities to produce outputs for others |
| [DAT] | Recipient | Input channels, compatibility, integration | Receiving, filtering, incorporating | A model in [DAT] focuses on input structures and perceives the world in terms of what information it can receive |
| [INS] | Tool/Method | Procedural knowledge, functions, mechanisms | Procedural execution, methodological application | A model in [INS] highlights functional relationships and attends primarily to opportunities for applying its specialized processes |
| [LOC] | Context/Environment | Spatial/temporal structures, situations, conditions | Constraining, framing, contextualizing | A model in [LOC] emphasizes environmental structures and conditions that frame other activities |
| [ABL] | Origin/History | Historical relationships, provenance, causes | Explaining, attributing, reconstructing | A model in [ABL] prioritizes causal/historical connections and perceives opportunities for explanation or source identification |
| [VOC] | Addressable Interface | Interface structures, activation patterns | Being called, activation, response | A model in [VOC] foregrounds its interface mechanisms and perceives signals that can activate specific responses |

**Ontological Focus (Modulating the Generative Model):** Different cases might implicitly activate or prioritize different facets of a model's world-model (its ontology, encoded in $p(o, s | \theta, m)$).

* `M[NOM]` (Actor): Emphasizes entities, actions, causal relationships needed for agency.
* `M[LOC]` (Context): Emphasizes spatial, temporal, or abstract contextual structures that frame observations.
* `M[INS]` (Tool): Emphasizes processes, mechanisms, functional properties relevant to its instrumental role.
* `M[ABL]` (Origin): Emphasizes history, causality, provenance relevant to explaining current states.

**Affordance Landscapes (Modulating Policy Priors):** A model's case could shape its perceived affordances by modulating prior beliefs over policies $p(\pi)$. The case filters or weights the action sequences the model considers viable or relevant.

* `M[NOM]`: High prior probability on policies involving direct action and influence on others or the environment.
* `M[ACC]`: High prior probability on policies involving internal state/parameter changes receptive to external influence.
* `M[DAT]`: High prior probability on policies focused on information gathering from specific sources.
* `M[INS]`: High prior probability on policies executing its core procedural function.

**Active Inference Interpretation:**
The case assignment $k$ acts as a high-level context or constraint $m_k$ modulating both the generative model $p(o, s | \theta, m_k)$ and the priors over policies $p(\pi | m_k)$. Transitioning $k \to k'$ means activating a different modulation $m_{k'}$, effectively shifting the model's active ontology and perceived/preferred affordances to suit the new functional role. This provides a mechanism for context-dependent cognition and action within the ecosystem.

### Visualizing Affordance Landscapes Across Cases

Consider a hypothetical visualization where the x-axis represents different possible actions, and the y-axis represents the prior probability assigned to each action under different case assignments. Each case would generate a distinct "affordance landscape" - a probability distribution over actions:

- [NOM] cases would show high peaks over actions that directly transform the environment
- [ACC] cases would show high peaks over actions that facilitate parameter updates
- [DAT] cases would show high peaks over information-seeking actions
- [INS] cases would show high peaks over specialized functional procedures

Case transitions would appear as shifts in this affordance landscape, with some peaks growing and others diminishing as the model's role changes.

## Mediating Ontological Differences

What happens when two models, $M_1$ and $M_2$, need to interact but possess different underlying generative models (ontologies) or perceive different affordances in a shared situation? This represents a breakdown in establishing a shared **Markov blanket** or **partition** for the interaction.

**Example Clash:** $M_1[\text{NOM}]$ attempts to perform an action $a$ on $M_2[\text{ACC}]$. However, $M_1$'s generative model $p_1$ assumes $M_2$ has a state $s_x$ that affords modification via $a$ (i.e., $p_1(s'_2 | s_2, a)$ has significant dependence on $a$ via $s_x$). $M_2$'s own model $p_2$ lacks $s_x$ or represents the transition $p_2(s'_2 | s_2, a)$ differently, leading to high surprisal for $M_1$ upon observing the outcome and potential failure of $M_2$ to update as expected. The interaction fails due to ontological/affordance mismatch.

This necessitates mechanisms for detecting and resolving such discrepancies, potentially via specialized CEREBRUM cases.

### Types of Ontological Mismatches

Several types of mismatches can occur between interacting models:

1. **State Space Mismatch**: One model includes states that another does not recognize or track
2. **Causal Relationship Mismatch**: Models disagree about how states influence each other
3. **Affordance Mismatch**: Models perceive different action possibilities in the same situation
4. **Prior Belief Mismatch**: Models have different expectations about likely states or transitions
5. **Precision Mismatch**: Models assign different certainty/importance to the same aspects of a situation

Each type of mismatch might require different negotiation and alignment strategies.

## Speculative Ontological / Alignment Cases

Novel cases could emerge specifically to manage ontological friction and align affordance perceptions, enabling robust interaction despite heterogeneity.

1. **[NEG] Negotiative Case:** Models in [NEG] specialize in interactions where ontological assumptions or affordance landscapes potentially differ. Their primary function is meta-interaction aimed at probing, comparing, and reconciling generative models, rather than direct task execution.

   * **Function:** Engage in structured dialogue: exchange predictions under shared hypothetical inputs, query parameter sensitivities, compare expected free energy gradients for potential joint policies.
   
   * **Active Inference:** Minimize 'negotiation free energy', perhaps defined as the mutual surprisal about each other's model structures or predicted responses. Actions involve generating informative queries or proposing simplified shared ontological frameworks (local `partitions`).
   
   * **Precision:** High precision on beliefs about the *other* model's generative model parameters and structure (meta-beliefs), and on the communication channel used for negotiation.

2. **[ALIGN] Alignment Case:** Models in [ALIGN] actively work to modify their own or another model's generative model/parameters/policies to reduce ontological divergence or align perceived affordances, typically following a [NEG] phase.

   * **Function:** Implement targeted changes (parameter updates, structural adjustments via e.g., Bayesian model reduction/averaging, prior modifications, policy pruning/sharing) aimed at increasing mutual predictability or enabling successful task execution under a temporarily shared (local) ontology.
   
   * **Active Inference:** Minimize expected free energy associated with *future task-specific interactions* with the aligned model(s), conditioned on the alignment actions. Policy selection involves choosing modifications expected to yield the most congruent future interactions.
   
   * **Precision:** High precision on the specific parameters/policies being targeted for alignment and on the prediction errors generated during subsequent joint tasks to confirm successful alignment.

3. **[ONT] Ontological Case (Hypothetical/Core):** Could represent the relatively stable core ontological commitments or foundational priors of a model. Transformations into/out of this hypothetical case might represent fundamental, slow belief revisions (akin to paradigm shifts), potentially triggered by persistent, high surprisal across many contexts. Interactions requiring modification of [ONT] aspects would likely have very high expected free energy cost.

4. **[AFF] Affordance Signaling Case:** Models specialized in identifying and communicating potential affordances within a specific context ([LOC]) to other models ([NOM]), effectively guiding action selection.

### Formal Characterization of Negotiation Free Energy

For models in [NEG], we might formalize the 'negotiation free energy' (NFE) as:

$$\text{NFE} = D_{KL}[q_{1}(\theta_2) \parallel p_2(\theta_2)] + D_{KL}[q_{2}(\theta_1) \parallel p_1(\theta_1)]$$

Where $q_{1}(\theta_2)$ represents model 1's beliefs about model 2's parameters, and $p_2(\theta_2)$ represents model 2's actual parameter distribution (and vice versa). NFE is minimized when both models have accurate beliefs about each other's generative models.

### Interaction Example Flow

1. $M_1[\text{NOM}]$ interacts with $M_2[\text{ACC}]$; high surprisal / interaction failure occurs.
2. Failure triggers transition (potentially mediated by a governing [REF] model) to $M_1[\text{NEG}]$ and $M_2[\text{NEG}]$.
3. $M_1$ & $M_2$ exchange queries/predictions (e.g., "What is your expected state if I apply action $a$?"). Divergence in predictions reveals ontological mismatch regarding state $s_x$.
4. Transition to [ALIGN]. Models negotiate a modification: e.g., $M_1$ agrees to use action $b$ targeting state $s_y$ which both models agree upon, or $M_2$ temporarily incorporates a simplified proxy for $s_x$ into its model for this context.
5. Transition back to $M_1[\text{NOM}]$ / $M_2[\text{ACC}]$. Interaction proceeds using the aligned ontology/policies, resulting in lower surprisal.

## Relationship to Joint Ontological Underwriting and Partitions

This framework provides a dynamic mechanism for achieving **joint ontological underwriting**. Cases like [NEG] and [ALIGN] allow models to actively construct a mutually intelligible, localized, and temporary shared understanding sufficient for a specific joint activity or interaction. They dynamically establish a sufficient shared **partition** of the state space (relevant distinctions) and alignment of perceived affordances for the task at hand, rather than requiring complete ontological identity.

The outcome is not necessarily a globally consistent ontology but rather a context-dependent, task-specific **joint distribution** over states, parameters, and policies that minimizes expected free energy *for that specific interaction*, enabling collaboration despite underlying heterogeneity.

### Alignment Strategies

Several strategies might emerge for aligning divergent ontologies:

1. **Minimal Ontological Commitment**: Find the smallest shared ontology sufficient for the specific task
2. **Proxy State Modeling**: Create temporary proxy states that approximate missing concepts in partner models
3. **Affordance-Only Alignment**: Focus on aligning perceived affordances while leaving underlying ontological differences unresolved
4. **Interface Development**: Develop specialized interface structures that translate between ontologies
5. **Hierarchical Alignment**: Align at higher levels of abstraction where differences may be less pronounced

## Challenges and Future Directions

* **Complexity & Scalability:** Modeling ontological negotiation is computationally intensive. How can these mechanisms scale to large, diverse ecosystems without combinatorial explosion?

* **Meta-Reasoning:** Requires models capable of representing and reasoning about their own and others' generative models (requiring aspects of the [REF] case).

* **Infinite Regress:** How to avoid needing meta-negotiation cases if the initial negotiation fails?

* **Formalization:** Rigorously defining the state spaces, action spaces (including communicative acts), precision dynamics, and free energy formulations for [NEG] and [ALIGN] cases is a major challenge.

* **Implementation:** Requires flexible architectures supporting reflection, dynamic model modification, and meta-belief representation.

* **Grounding:** How is the success of alignment ultimately evaluated and grounded in task outcomes?

### Research Directions

1. **Empirical Studies**: Investigate how humans negotiate shared ontologies and affordances in collaborative tasks
2. **Computational Models**: Develop concrete implementations of [NEG] and [ALIGN] cases using existing Active Inference frameworks
3. **Scaling Laws**: Explore how ontological negotiation complexity scales with ecosystem size and diversity
4. **Emergence Studies**: Investigate whether [NEG] and [ALIGN] cases might emerge spontaneously in sufficiently complex CEREBRUM ecosystems

## Conclusion: Towards Ontologically Fluent Ecosystems

Integrating ontological awareness and affordance alignment into the CEREBRUM framework via specialized cases like [NEG] and [ALIGN] represents a significant speculative leap towards more sophisticated AI ecosystems. It moves beyond purely functional roles to endow cases with semantic depth related to a model's assumptions about reality and action possibilities. 

This could enable the design of more robust, flexible, and truly collaborative AI systems capable of dynamically negotiating shared understandings and resolving conflicts arising from differing world-models or incompatible affordance perceptions. By grounding these meta-interactions in Active Inference principles, we can explore how systems might minimize surprisal not just about external data, but about the ontological commitments and perceived possibilities of their interaction partners, paving the way for more synergistic, adaptive, and potentially even ethical synthetic intelligence. 