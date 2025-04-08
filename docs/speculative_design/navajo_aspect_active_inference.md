# Speculative Design: Navajo Aspectual Viewpoints as a Framework for Active Inference

## 1. Introduction: Converging Frameworks of Perspective

This document explores a speculative framework that connects the sophisticated aspect system of the Navajo language (Diné bizaad) with the mathematical and theoretical structures of Active Inference and the Free Energy Principle—specifically Expected Free Energy, policy selection, and planning-as-inference. The core proposal is that Navajo's intricate grammatical encoding of viewpoints on actions provides a natural linguistic analogue to the perspective-taking operations inherent in active inference processes, particularly in how agents evaluate potential policies through the lens of Expected Free Energy.

Both systems—Navajo aspect and Active Inference—fundamentally concern themselves with representing multiple viewpoints on actions, their impacts, and their unfolding across time. This parallel offers potential insights for:
1. Conceptualizing Active Inference in more nuanced, phenomenologically grounded ways
2. Exploring how cultural-linguistic frameworks might inform computational approaches to agency and inference
3. Developing richer vocabularies for discussing the subjective dimensions of inference, planning, and policy selection

## 2. The Navajo Aspectual System: Encoding Viewpoints on Actions

The Navajo language employs a remarkably sophisticated system for encoding various perspectives on actions through verb forms that specify not just when an action occurs (tense) but how it unfolds and is experienced (aspect). Key features include:

### 2.1 Core Aspectual Distinctions in Navajo

*   **Momentaneous vs. Continuative:** Distinguishes between point-like, instantaneous actions (momentaneous) and extended, ongoing processes (continuative).
    * *Example:* The distinction between a single act of seeing something (`yí'į́`) versus continuously watching something (`yineł'į́`).

*   **Imperfective vs. Perfective:** Contrasts incomplete, ongoing, or habitual actions (imperfective) with completed actions viewed in their entirety (perfective).
    * *Example:* The difference between "I am running" (`nishááh` - imperfective) and "I ran" (`níyá` - perfective).

*   **Iterative & Repetitive Aspects:** Specific markings denote actions performed repeatedly or habitually.
    * *Example:* Using the distributive plural (`da-`) or iterative infix (`-d-`) to indicate repeated actions.

*   **Seriative & Progressive:** Marks the sequence, progression, or stage-by-stage unfolding of actions.
    * *Example:* Using the progressive mode to indicate an action in progress (`yishááł` - "I am walking along").

*   **Optative & Future Viewpoints:** Specific conjugations exist for desired actions, intended actions, and future possibilities.
    * *Example:* The optative mode for expressing wishes or desires (`ná'ashné` - "may I do it" or "I wish to do it").

### 2.2 The Role of "Start-Points" and "Classifier Stems"

A particularly relevant aspect of Navajo verb structure is the system of "classifier stems" (sometimes called verb themes), which encode different viewpoints on action properties:

*   **Ø-Classifier:** Often represents stative situations or simple actions.
*   **Ł-Classifier:** Often indicates causative or transitive actions (making something happen).
*   **D-Classifier:** Frequently marks reflexive, reciprocal, or passive perspectives.
*   **L-Classifier:** Often indicates actions involving handling specific types of objects or particular manners of manipulation.

The choice of classifier shifts the perspective on how an action is conceived, who/what is prominently involved, and how agency flows within the action—paralleling the way active inference represents different causal flows in generative models.

## 3. Core Concepts in Active Inference and the Free Energy Principle

Before drawing parallels, let us review the key concepts from Active Inference theory:

### 3.1 Expected Free Energy (EFE)

Expected Free Energy is a criterion that guides policy selection in active inference. For a policy π and future time τ, it quantifies:
1. **Pragmatic Value:** How well the policy helps fulfill the agent's preferred outcomes/goals
2. **Epistemic Value:** How much uncertainty or ambiguity the policy resolves

Formally, EFE can be expressed as:
```
G(π) = ∑_τ G(π,τ)
```
where G(π,τ) balances:
- **Pragmatic value** (achieving preferred outcomes)
- **Epistemic value** (information gain)

An agent selects the policy with the lowest Expected Free Energy, which optimally balances these considerations.

### 3.2 Policy Selection and Planning-as-Inference

In Active Inference, plans are sequences of actions (policies) selected through an inferential process:
1. The agent maintains a generative model of how actions lead to future states
2. The agent treats goal states as observations it expects to make
3. The agent infers which policies would most likely generate those "observed" goal states
4. This inference process balances achieving goals with gaining information

The probability of selecting a policy π is proportional to:
```
P(π) ∝ exp(-G(π))
```
where policies with lower Expected Free Energy are more probable.

### 3.3 Temporal Depth and Horizon

Active Inference agents operate with varying temporal horizons for policy evaluation:
1. **Deep policies:** Evaluate consequences over many time steps
2. **Shallow policies:** Consider only immediate or near-term effects
3. **Nested timescales:** Hierarchical policies operating at different temporal resolutions

The agent's perspective on time critically shapes how it evaluates policies and plans.

## 4. Navajo Aspect as a Framework for Active Inference

This section outlines how Navajo's aspect system offers a natural linguistic framework for understanding key processes in Active Inference:

### 4.1 Momentaneous vs. Continuative: Temporal Granularity in Policy Evaluation

The Navajo distinction between momentaneous (point-like) and continuative (extended) actions provides a linguistic analogue to how Active Inference agents evaluate policies with different temporal granularities:

*   **Momentaneous Aspect → Discrete Policy Evaluation:** Just as momentaneous aspect focuses on point-like actions, Active Inference can evaluate policies as discrete state transitions (s→s'). This perspective suits abrupt, decisive actions.
*   **Continuative Aspect → Trajectory-Based Policy Evaluation:** Just as continuative aspect frames actions as extended processes, Active Inference can evaluate policies as continuous trajectories through state space. This perspective suits gradually unfolding processes.

These different aspectual viewpoints could inform models that flexibly switch between discrete and continuous evaluation frames, potentially addressing the "chunking" problem in hierarchical policy optimization.

### 4.2 Imperfective vs. Perfective: Process vs. Outcome Focus

The imperfective/perfective distinction in Navajo parallels a key duality in how Expected Free Energy evaluates policies:

*   **Imperfective Aspect → Epistemic Value:** Like the imperfective aspect, which focuses on actions' ongoing, unfolding nature, the epistemic component of EFE focuses on the information-gathering process. It values the journey through state space for what it reveals.
*   **Perfective Aspect → Pragmatic Value:** Like the perfective aspect, which views actions as completed wholes, the pragmatic component of EFE focuses on terminal outcomes. It values the destination in state space for its alignment with preferences.

This parallel suggests a potential extension to standard EFE formulations that might more explicitly weight process-focused versus outcome-focused evaluation based on context.

### 4.3 Classifier Stems: Causal Flow Perspectives

Navajo's classifier stems, which encode different perspectives on action causality, offer a compelling analogue to the causal perspectives in Active Inference's generative models:

*   **Ø-Classifier → "Is" Perspective:** Focuses on states themselves, similar to Active Inference's emphasis on hidden states.
*   **Ł-Classifier → "Makes Happen" Perspective:** Emphasizes causal agency, similar to Active Inference's model of how actions cause state transitions.
*   **D-Classifier → "Happens To" Perspective:** Represents passive/recipient viewpoints, similar to Active Inference's model of how environment dynamics affect the agent.
*   **L-Classifier → "Handling" Perspective:** Details interaction mechanics, similar to Active Inference's fine-grained sensorimotor models.

These distinctions suggest a natural way to categorize the multiple perspectives inherent in policy evaluation, potentially offering a more intuitive framework for explaining the multifaceted nature of inference.

### 4.4 Optative & Future: Counterfactual Simulation

Navajo's optative mode (expressing desired states) and future perspectives align with how Active Inference evaluates potential futures:

*   **Optative Mode → Preference-Driven Simulation:** Like the optative expressing desires, Active Inference generates preferred futures as "observations" to work backward from.
*   **Future Marking → Policy Evaluation:** Like future markers indicating what might happen, Active Inference simulates consequences of potential policies.

This parallel highlights the fundamentally counterfactual nature of planning-as-inference, potentially suggesting more nuanced ways to represent "desire" in computational models.

### 4.5 Iterative & Seriative Aspects: Sequential Policy Evaluation

Navajo's aspects for repetition and sequencing offer insights for understanding hierarchical policy evaluation:

*   **Iterative Aspect → Repeated Policy Assessment:** Like iterative aspect marking repeated actions, Active Inference may repeatedly evaluate similar policies across contexts.
*   **Seriative Aspect → Policy Sequencing:** Like seriative aspect marking step-by-step progression, Active Inference tracks the sequential unfolding of nested policies across timescales.

These parallels could inform more sophisticated models of hierarchical and sequential planning, especially for complex tasks requiring coordination across timescales.

## 5. Multiple Angles on the Perspective Metaphor

This section explores different interpretative angles on the Navajo-Active Inference parallel:

### 5.1 The Epistemic Angle: Multiple Ways of Knowing

From an epistemological perspective, both Navajo aspect and Active Inference acknowledge multiple modes of apprehending action:

* **Navajo:** Through grammatical structure, Navajo encodes knowledge about events from multiple simultaneous perspectives, reflecting a deeply relational epistemology.
* **Active Inference:** Through its Bayesian framework, Active Inference integrates multiple sources of evidence and perspectives (prior knowledge, immediate evidence, goal-driven predictions).

This parallel suggests that Active Inference might benefit from more explicitly modeling how different "ways of knowing" interact in the evaluation of policies—distinguishing, for example, between knowledge derived from personal experience, social learning, and abstract reasoning.

### 5.2 The Phenomenological Angle: Lived Experience of Planning

From a phenomenological perspective, both systems capture something about the subjective experience of planning:

* **Navajo:** The aspect system reflects how actions are experienced subjectively—as unfolding processes, completed wholes, repeated patterns, etc.
* **Active Inference:** The balance of epistemic and pragmatic value captures how planning feels—as a tension between curiosity (information-seeking) and goal-pursuit.

This parallel suggests that Active Inference might benefit from more explicitly modeling the phenomenological characteristics of planning, including the sense of agency, temporal perspective, and affective valence associated with different policy options.

### 5.3 The Computational-Cognitive Angle: Efficiency in Representation

From a computational perspective, both systems provide efficient compression of complex information:

* **Navajo:** The aspect system compactly encodes rich information about action properties within minimal grammatical markers.
* **Active Inference:** The Expected Free Energy formulation compactly expresses complex evaluation criteria for policies.

This parallel suggests that the natural efficiencies found in language might inform more computationally tractable approaches to policy evaluation in artificial agents, particularly for resource-constrained systems.

### 5.4 The Cultural-Evolutionary Angle: Adapted Solutions

From an evolutionary perspective, both systems represent adapted solutions to the problem of coordinating action in complex environments:

* **Navajo:** The aspect system evolved culturally to facilitate communication about complex temporal and causal relationships relevant to survival.
* **Active Inference:** The mathematical framework evolved academically to formalize the principles by which biological systems maintain their integrity.

This parallel suggests that studying diverse cultural-linguistic solutions to the problem of "planning" might reveal alternative optimization strategies not currently captured in standard active inference formulations.

## 6. Practical Implications and Potential Applications

This speculative framework offers several practical implications:

### 6.1 Enhanced Model Formulations

* Developing active inference models that explicitly incorporate the multiple perspectives on action encoded in systems like Navajo aspect.
* Creating hybrid models that can flexibly shift between momentaneous/continuative, perfective/imperfective frames based on context.
* Incorporating classifier-like perspectives (cause, receive, state, handle) as explicit components in policy evaluation.

### 6.2 Explanatory Frameworks

* Using aspectual concepts to explain active inference to non-specialists.
* Developing visual representations of policy evaluation that leverage the intuitive distinctions found in aspectual systems.
* Creating pedagogical materials that use linguistic analogies to teach technical concepts.

### 6.3 Cross-Cultural AI Development

* Incorporating diverse linguistic frameworks for representing action and causality into AI systems.
* Developing active inference implementations that are more explicitly aligned with non-Western conceptualizations of time, causality, and agency.
* Building systems that can "code-switch" between different aspectual frames in different contexts.

## 7. Challenges and Limitations

Several challenges arise in this speculative mapping:

* **Oversimplification Risk:** Both Navajo grammar and Active Inference are complex systems; drawing parallels risks oversimplification.
* **Cultural Appropriation Concerns:** Care must be taken to respect the cultural context of Navajo linguistic structures.
* **Empirical Validation Difficulty:** Testing whether these parallels lead to improved computational models presents methodological challenges.
* **Translation Gaps:** Mathematical formalism and linguistic categories represent fundamentally different types of systems.

## 8. Conclusion: Toward a Linguistically-Informed Active Inference

The parallels between Navajo's aspectual system and Active Inference's approach to policy evaluation suggest fertile ground for cross-disciplinary exploration. By viewing Expected Free Energy through the lens of linguistic systems specialized for representing viewpoints on actions, we gain new perspectives on how to conceptualize, formalize, and explain the inferential processes underlying planning and policy selection.

This framework invites us to consider how the rich diversity of human languages, each encoding unique perspectives on action, causality, and time, might inform more sophisticated and culturally-nuanced approaches to computational agency. Just as Navajo speakers can fluidly shift between different aspectual viewpoints to capture the multifaceted nature of actions, future active inference systems might benefit from similar flexibility in perspective-taking—evaluating policies not through a single monolithic criterion, but through an aspectually-rich engagement with the multiple ways actions can be conceived, experienced, and valued. 