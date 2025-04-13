# Supplement 8: Active Inference Formulation Details

This supplement provides detailed mathematical derivations and formulations connecting the Active Inference framework to the CEREBRUM case system.

## 8.1 Generative Model Specification

The generative model underlying CEREBRUM's active inference framework can be specified as a tuple $(S, A, T, \Omega, O, F, \pi)$ where:

- $S$ represents the space of hidden states, which includes both environmental states and the internal states of models in various cases.
- $A$ represents the action space, which crucially includes case transformation operations alongside traditional actions.
- $T: S \times A \rightarrow P(S)$ specifies state transition dynamics, mapping current state and action to a probability distribution over next states.
- $\Omega$ is the space of possible observations available to models.
- $O: S \rightarrow P(\Omega)$ is the likelihood mapping from states to observations.
- $F$ is the free energy functional that models minimize.
- $\pi$ represents precision parameters modulating the influence of various probabilistic terms.

For a model $M$ with case $C$, the generative model is instantiated with case-specific parameters:

$$p(o_{1:T}, s_{1:T}, \pi | C) = p(s_1 | C) \prod_{t=1}^{T} p(o_t | s_t, C) p(s_t | s_{t-1}, a_{t-1}, C) p(\pi | C)$$

This factorization represents how future observations and states depend on the current case assignment, with case-specific priors, likelihood mappings, and transition dynamics.

## 8.2 Free Energy Principle in CEREBRUM

The Variational Free Energy (VFE) functional for a model with case $C$ is defined as:

$$F[q, C] = D_{KL}[q(s, \pi) || p(s, \pi | o, C)] - \log p(o | C)$$

This can be reformulated as:

$$F[q, C] = E_q[\log q(s, \pi) - \log p(s, \pi, o | C)]$$

Which decomposes into:

$$F[q, C] = \underbrace{D_{KL}[q(s, \pi) || p(s, \pi | C)]}_{\text{Complexity}} - \underbrace{E_q[\log p(o | s, C)]}_{\text{Accuracy}}$$

Further expanding the complexity term:

$$D_{KL}[q(s, \pi) || p(s, \pi | C)] = \int q(s, \pi) \log \frac{q(s, \pi)}{p(s, \pi | C)} ds d\pi$$

And expanding the accuracy term:

$$E_q[\log p(o | s, C)] = \int q(s, \pi) \log p(o | s, C) ds d\pi$$

These terms have specific interpretations in CEREBRUM:

1. **Complexity** measures the divergence between the approximate posterior and the prior, which represents the computational cost of updating beliefs when adopting a case.

2. **Accuracy** measures how well the model with a given case explains observations, which represents the explanatory power of a case assignment.

For a time series of observations $o_{1:T}$, the path integral of free energy is:

$$\mathcal{F}[q, C, o_{1:T}] = \sum_{t=1}^{T} F[q_t, C]$$

Where $q_t$ represents the evolving belief distribution at time $t$. The principle of least action dictates that natural systems minimize this path integral.

### 8.2.1 Relationship Between Free Energy and Case Transformations

Case transformations in CEREBRUM can be characterized by changes in the free energy landscape:

$$\Delta F[C_1 \rightarrow C_2] = F[q, C_2] - F[q, C_1]$$

Successful case transformations decrease the total free energy of the system. For a composite system with multiple models in different cases, the total free energy is:

$$F_{total} = \sum_i F[q_i, C_i] + \sum_{i,j} I[q_i, q_j | C_i, C_j]$$

Where $I[q_i, q_j | C_i, C_j]$ represents the mutual information between models in different cases.

### 8.2.2 Mapping Between Active Inference and CEREBRUM

| Active Inference Concept | CEREBRUM Implementation | Mathematical Formulation |
|--------------------------|------------------------|--------------------------|
| Generative Model | Case-Parametrized Model | $p(o, s, \pi | C)$ |
| Variational Density | Case-Specific Beliefs | $q(s, \pi | C)$ |
| Free Energy | Case-Specific Free Energy | $F[q, C]$ |
| Policy Evaluation | Case Transformation Selection | $G(\pi_{C_1 \rightarrow C_2})$ |
| Precision Parameters | Case-Modulated Parameters | $\pi_C$ |
| Belief Updating | Case-Specific Message Passing | $q(s_i) \propto \exp(E_{q(\backslash s_i)}[\log p(o, s, \pi | C)])$ |
| Expected Free Energy | Case Transformation Planning | $G(\pi) = E_{q(o, s | \pi)}[\log q(s | \pi) - \log p(o, s | \pi)]$ |
| Markov Blankets | Case Boundaries | $\text{Case}(M) \subseteq \text{MB}(M)$ |
| Hierarchical Models | Case Transformation Sequences | $C_1 \rightarrow C_2 \rightarrow C_3$ |
| Action Selection | Case-Based Policy | $\pi^*(C) = \arg\min_\pi G(\pi | C)$ |
| Prediction Errors | Case-Specific Prediction Errors | $\varepsilon_C = o - g(s, C)$ |
| Active Inference | Active Case Management | Minimizing $F[q, C]$ through case selection |

### 8.2.3 Case-Specific Free Energy Applications

Different cases in CEREBRUM implement specialized forms of free energy minimization, aligning with their functional roles in the system:

$$F_{NOM}[q] = D_{KL}[q(s) || p(s | C_{NOM})] - \alpha_{NOM} \cdot E_q[\log p(o | s, C_{NOM})]$$

$$F_{ACC}[q] = \beta_{ACC} \cdot D_{KL}[q(s) || p(s | C_{ACC})] - E_q[\log p(o | s, C_{ACC})]$$

$$F_{DAT}[q] = D_{KL}[q(s) || p(s_1, s_2 | C_{DAT})] - E_q[\log p(o | s, C_{DAT})]$$

$$F_{INS}[q] = D_{KL}[q(s, a) || p(s, a | C_{INS})] - E_q[\log p(o | s, a, C_{INS})]$$

$$F_{GEN}[q] = D_{KL}[q(s) || p(s | C_{GEN})] - E_q[\log p(o | s, C_{GEN})] + \lambda_{GEN} \cdot E_q[\log p(r | s, C_{GEN})]$$

Where:
- $\alpha_{NOM}$ represents increased precision on accuracy for Nominative case
- $\beta_{ACC}$ represents increased precision on complexity for Accusative case
- $p(s_1, s_2 | C_{DAT})$ captures the mediating role of Dative case connecting two state spaces
- $p(s, a | C_{INS})$ jointly represents states and actions in Instrumental case
- $\lambda_{GEN} \cdot E_q[\log p(r | s, C_{GEN})]$ adds a relational term for Genitive case

These specialized forms enable principled adaptation of free energy minimization to different functional contexts.

#### Information Flow Patterns Across Case Transformations

The following table characterizes how information flows change during case transformations:

| Source Case | Target Case | Information Flow Pattern | Free Energy Change |
|-------------|-------------|--------------------------|-------------------|
| NOM → ACC | $F_{NOM} \to F_{ACC}$ | Top-down to bottom-up | $\Delta F \propto -D_{KL}[q(o) \| p(o\|C_{ACC})]$ |
| ACC → NOM | $F_{ACC} \to F_{NOM}$ | Bottom-up to top-down | $\Delta F \propto -D_{KL}[p(s\|C_{NOM}) \| q(s)]$ |
| NOM → DAT | $F_{NOM} \to F_{DAT}$ | Prediction to mediation | $\Delta F \propto -I[s_1; s_2\|C_{DAT}]$ |
| ACC → DAT | $F_{ACC} \to F_{DAT}$ | Error correction to mediation | $\Delta F \propto -E_q[\log p(s_2\|s_1,C_{DAT})]$ |
| DAT → INS | $F_{DAT} \to F_{INS}$ | Mediation to action-oriented | $\Delta F \propto -E_q[\log p(o\|s,a,C_{INS})]$ |
| NOM → GEN | $F_{NOM} \to F_{GEN}$ | Prediction to relation | $\Delta F \propto -\lambda_{GEN} \cdot E_q[\log p(r\|s,C_{GEN})]$ |
| ACC → INS | $F_{ACC} \to F_{INS}$ | Error-correction to action | $\Delta F \propto -E_q[\log p(a\|s,C_{INS})]$ |

Each transformation induces characteristic changes in the free energy landscape, with information flow redirected according to the functional role of the target case. The proportionality relations ($\Delta F \propto$) capture the dominant terms determining whether a transformation increases or decreases free energy.

#### Multi-Scale Free Energy Minimization

CEREBRUM implements free energy minimization across multiple scales:

1. **Within-Case Scale**: Each model minimizes free energy according to its current case assignment
   $$F_i[q_i, C_i] \to \min$$

2. **Transformation Scale**: Case transformations are selected to minimize expected free energy
   $$G(\pi_{C_1 \rightarrow C_2}) \to \min$$

3. **System Scale**: The configuration of cases across all models minimizes total system free energy
   $$F_{total} = \sum_i F[q_i, C_i] + \sum_{i,j} I[q_i, q_j | C_i, C_j] \to \min$$

This multi-scale optimization aligns with the nested Markov blanket formulation in the Free Energy Principle, where each scale offers a distinct perspective on the same underlying dynamics.

Each case modifies this formulation by emphasizing different components:

- **Nominative Case [NOM]**: Emphasizes accuracy of predictions, with higher precision on the likelihood term.
- **Accusative Case [ACC]**: Emphasizes complexity reduction through effective belief updates.
- **Dative Case [DAT]**: Balances complexity and accuracy for information mediation.
- **Instrumental Case [INS]**: Emphasizes action-dependent state transitions.

For case transformations, we define a transformation-specific free energy:

$$F_{\text{trans}}[q, C_{\text{source}} \rightarrow C_{\text{target}}] = F[q, C_{\text{target}}] + D_{KL}[q_{\text{target}}(s, \pi) || q_{\text{source}}(s, \pi)]$$

The additional KL divergence term represents the transformation cost between source and target case parametrizations.

## 8.3 Message Passing Schemes

For a model $M$ with case $C$, belief updates follow a variational message passing scheme. Assuming factorized approximate posteriors:

$$q(s, \pi) = \prod_i q(s_i) \prod_j q(\pi_j)$$

The update equations for each factor take the form:

$$q(s_i) \propto \exp(E_{q(\backslash s_i)}[\log p(o, s, \pi | C)])$$

$$q(\pi_j) \propto \exp(E_{q(\backslash \pi_j)}[\log p(o, s, \pi | C)])$$

where $q(\backslash x)$ denotes the approximate posterior for all variables except $x$.

These updates are implemented as message passing operations, with the form of messages determined by the model's case. For example:

- **Nominative [NOM]** messages emphasize prediction generation:
  $$m_{\text{NOM}}(s_i \rightarrow o) = E_{q(s_{\backslash i})}[\log p(o | s, C=\text{NOM})]$$

- **Accusative [ACC]** messages emphasize belief updates:
  $$m_{\text{ACC}}(o \rightarrow s_i) = E_{q(s_{\backslash i})}[\log p(o | s, C=\text{ACC})]$$

- **Dative [DAT]** messages emphasize mediation:
  $$m_{\text{DAT}}(s_i \rightarrow s_j) = E_{q(s_{\backslash \{i,j\}})}[\log p(s_j | s_i, C=\text{DAT})]$$

## 8.4 Expected Free Energy (EFE) for Case Selection

Case selection in CEREBRUM follows an active inference approach, where the Expected Free Energy (EFE) of different case transformation policies is evaluated. For a policy $\pi$ that includes transforming to case $C_{\text{target}}$, the EFE is:

$$G(\pi) = E_{q(o, s | \pi)}[\log q(s | \pi) - \log p(o, s | \pi)]$$

This can be decomposed into:

$$G(\pi) = \underbrace{E_{q(s | \pi)}[D_{KL}[q(o | s, \pi) || p(o | s)]]}_{\text{Epistemic Value (Exploration)}} + \underbrace{E_{q(o, s | \pi)}[\log q(o | s, \pi) - \log p(o)]}_{\text{Pragmatic Value (Exploitation)}}$$

For case transformation policies, this becomes:

$$G(\pi_{C_{\text{source}} \rightarrow C_{\text{target}}}) = \underbrace{E_{q(s | C_{\text{target}})}[D_{KL}[q(o | s, C_{\text{target}}) || p(o | s)]]}_{\text{Information Gain from New Case}} + \underbrace{E_{q(o, s | C_{\text{target}})}[\log q(o | s, C_{\text{target}}) - \log p(o | C_{\text{preferred}})]}_{\text{Goal Alignment of New Case}}$$

The optimal case transformation policy minimizes this expected free energy:

$$\pi^* = \arg\min_\pi G(\pi)$$

In practice, a softmax function converts these EFE values into a probability distribution over policies:

$$p(\pi | o) \propto \exp(-\gamma \cdot G(\pi))$$

where $\gamma$ is an inverse temperature parameter controlling the randomness of policy selection.

## 8.5 Precision Dynamics

Precision parameters in CEREBRUM modulate the influence of different probabilistic terms, affecting both inference within a case and case selection dynamics. For a model with case $C$, precision parameters $\pi$ are updated according to:

$$q(\pi_i) \propto p(\pi_i | C) \cdot \exp(-\frac{1}{2}\pi_i \cdot \varepsilon_i^T \varepsilon_i)$$

where $\varepsilon_i$ represents prediction errors associated with the $i$-th component of the generative model.

Case-specific precision defaults establish the characteristic behavior of each case:

- **Nominative [NOM]**: Higher precision on generative parameters ($\pi_{\text{gen}} > \pi_{\text{prior}}$)
- **Accusative [ACC]**: Higher precision on updating parameters ($\pi_{\text{update}} > \pi_{\text{gen}}$)
- **Genitive [GEN]**: Higher precision on relational parameters ($\pi_{\text{rel}} > \pi_{\text{other}}$)

During case transformations, precision parameters undergo structured remapping:

$$\pi_{\text{target}} = f_{C_{\text{source}} \rightarrow C_{\text{target}}}(\pi_{\text{source}})$$

This remapping function $f$ implements case-specific precision dynamics that control how attention and computational resources are allocated after transformation.

## 8.6 Connections to POMDPs

The CEREBRUM Active Inference formulation extends the traditional POMDP framework in several key ways:

1. **State Space Expansion**: States include not just environmental variables but also case assignments, interface configurations, and precision parameters.

2. **Action Space Enrichment**: Actions include case transformations alongside traditional actions, enabling models to modify their functional roles.

3. **Policy Evaluation**: Unlike standard POMDPs that maximize expected reward, CEREBRUM minimizes expected free energy, balancing exploration (information gain) and exploitation (goal achievement).

4. **Belief Dynamics**: While POMDPs update beliefs using Bayes' rule, CEREBRUM implements variational belief updates that can vary based on case assignment.

The mapping between POMDP and CEREBRUM components can be formalized as:

| POMDP Component | CEREBRUM Extension |
|-----------------|-------------------|
| States $s$ | States $s$ + Case assignment $C$ |
| Actions $a$ | Actions $a$ + Case transformations |
| Transition $T(s'|s,a)$ | Case-dependent transitions $T(s'|s,a,C)$ |
| Observations $o$ | Observations with case-specific attention $o_C$ |
| Observation model $O(o|s)$ | Case-dependent observation model $O(o|s,C)$ |
| Reward function $R(s,a)$ | Free energy minimization $F[q,C]$ |
| Value function $V(b)$ | Expected free energy $G(\pi)$ |

This mapping shows how CEREBRUM specializes the POMDP framework through its case-based structure and free energy optimization approach.

## 8.7 Neurobiological Connections and Computational Complexity

### 8.7.1 Neurobiological Plausibility of Case-Based Active Inference

CEREBRUM's case-based active inference formulation connects to several neurobiological mechanisms:

| CEREBRUM Component | Neurobiological Correlate | Functional Role |
|-------------------|---------------------------|-----------------|
| Case Assignment | Neuromodulation | Context-dependent processing modes |
| Precision Parameters | Dopaminergic/Cholinergic Modulation | Attentional allocation and learning rate control |
| Message Passing | Canonical Microcircuits | Implementation of predictive coding |
| Case Transformation | Neural Gain Control | Dynamic reconfiguration of functional connectivity |
| Free Energy Minimization | Hierarchical Predictive Processing | Prediction error minimization across cortical hierarchies |
| Expected Free Energy | Prefrontal Planning | Counterfactual reasoning about future states |
| Markov Blankets | Functional Segregation | Maintaining conditional independence between neural subsystems |

The mapping demonstrates how CEREBRUM's formal apparatus aligns with empirical findings in neuroscience, particularly regarding:

1. **Multiple Simultaneous Objectives**: The brain optimizes multiple objectives simultaneously (accuracy, complexity, exploration), which maps to CEREBRUM's case-specific free energy formulations.

2. **Context-Sensitivity**: Neural circuits reconfigure based on contextual demands, similar to case transformations in CEREBRUM.

3. **Hierarchical Processing**: The brain implements hierarchical predictive processing, with distinct information flows matching CEREBRUM's case-specific message passing schemes.

### 8.7.2 Computational Complexity Implications

The computational complexity of active inference in CEREBRUM varies by case:

| Case | Time Complexity | Space Complexity | Dominant Operation |
|------|----------------|------------------|-------------------|
| NOM | $O(n)$ | $O(n)$ | Forward prediction |
| ACC | $O(n \log n)$ | $O(n)$ | Belief update |
| DAT | $O(nm)$ | $O(n+m)$ | Information mediation |
| GEN | $O(n^2)$ | $O(n^2)$ | Relational modeling |
| INS | $O(na)$ | $O(n+a)$ | Action selection |
| VOC | $O(\log n)$ | $O(1)$ | Attention allocation |

Where:
- $n$ is the dimensionality of state space
- $m$ is the dimensionality of connected model's state space
- $a$ is the dimensionality of action space

This complexity analysis reveals an important tradeoff: case assignments effectively manage computational resources by directing attention to specific aspects of inference. The system can strategically transform between cases to optimize computational efficiency based on current demands.

For a system with $k$ models in potentially different cases, the total computational complexity is bounded by:

$$O\left(\sum_{i=1}^{k} C(C_i) + \sum_{i,j} T(C_i, C_j)\right)$$

Where $C(C_i)$ is the complexity of inference in case $C_i$ and $T(C_i, C_j)$ is the transformation cost between cases.

This formulation demonstrates how CEREBRUM achieves scalable active inference through distributed processing and strategic case management, enabling computationally efficient implementation of the Free Energy Principle in complex systems.

## 8.8 Comparison with Other Active Inference Frameworks

CEREBRUM's case-based formulation of active inference extends traditional frameworks in several key directions. This section provides a mathematical comparison with other prominent active inference formalisms.

### 8.8.1 Comparative Free Energy Formulations

| Framework | Free Energy Formulation | Key Distinguishing Features |
|-----------|------------------------|------------------------------|
| **Standard Active Inference** | $F[q] = D_{KL}[q(s) \| p(s)] - E_q[\log p(o\|s)]$ | Single-model inference with fixed functional role |
| **Hierarchical Active Inference** | $F[q] = \sum_i D_{KL}[q(s_i) \| p(s_i\|s_{i+1})] - E_q[\log p(o\|s_1)]$ | Fixed hierarchical message passing structure |
| **Deep Active Inference** | $F[q] = D_{KL}[q(s, \theta) \| p(s, \theta)] - E_q[\log p(o\|s, \theta)]$ | Parameterizes generative model with neural networks |
| **CEREBRUM** | $F[q, C] = D_{KL}[q(s, \pi\|C) \| p(s, \pi\|C)] - E_q[\log p(o\|s, C)]$ | Case-parameterized inference with dynamic functional roles |

The key mathematical distinction of CEREBRUM is the explicit parameterization of both the generative model and approximate posterior by the case assignment $C$, which enables dynamic reconfiguration of functional roles through case transformations.

### 8.8.2 Relationship to Variational Message Passing

Standard variational message passing updates take the form:

$$\log q(s_i) = E_{q(s_{\backslash i})}[\log p(o, s)] + \text{const}$$

CEREBRUM generalizes this to case-dependent message passing:

$$\log q(s_i | C) = E_{q(s_{\backslash i}|C)}[\log p(o, s | C)] + \text{const}$$

The case parameter $C$ modifies both the form of the joint distribution $p(o, s | C)$ and the factorization of the approximate posterior $q(s | C)$.

### 8.8.3 Extensions to Expected Free Energy

Standard expected free energy is formulated as:

$$G(\pi) = E_{q(o, s | \pi)}[\log q(s | \pi) - \log p(o, s | \pi)]$$

CEREBRUM extends this to include case transformations in the policy space:

$$G(\pi_{C_1 \rightarrow C_2}) = E_{q(o, s | C_2)}[\log q(s | C_2) - \log p(o, s | C_2)] + \gamma \cdot D_{KL}[q(s | C_2) \| q(s | C_1)]$$

Where the additional term $\gamma \cdot D_{KL}[q(s | C_2) \| q(s | C_1)]$ represents the transformation cost weighted by precision parameter $\gamma$.

### 8.8.4 Mathematical Advances Over Prior Work

CEREBRUM makes several mathematical contributions to active inference theory:

1. **Case-Parameterized Generative Models**: The explicit conditioning of the generative model on case assignment $p(o, s | C)$ provides a formal mechanism for dynamic reconfiguration of functional roles.

2. **Transformation-Specific Free Energy**: The introduction of transformation costs in the free energy functional $F_{\text{trans}}$ enables principled evaluation of case transformations.

3. **Multi-Scale Free Energy Minimization**: The hierarchical organization of free energy minimization (within-case, transformation, system) provides a formal account of nested optimization processes.

4. **Precision-Weighted Case Selection**: The softmax formulation of case selection with precision weighting $\beta(c, m)$ formalizes how systems adaptively allocate resources across competing functional roles.

These advances extend active inference beyond its traditional formulation as a theory of brain function to a more general computational framework for adaptive systems with dynamic functional reconfiguration. 