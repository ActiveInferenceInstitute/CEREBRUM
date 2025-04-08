# Appendix 1: Mathematical Formalization

\appendix

## Mathematical Appendix

This appendix contains all mathematical formalizations referenced throughout the paper, organized by equation number.

### Variational Free Energy and Case Transformations

**Equation 1: Variational Free Energy for Case Transformation**

$$
F = D_{KL}[q(s|T(m))||p(s|m)] - \mathbb{E}_{p}[\log p(o|s,T(m))]  \tag{1}
$$

where T(m) represents the transformed model, s are internal states, and o are observations.

**Equation 2: Markov Blanket and Case Relationship**

$$\text{Case}(M) \subseteq \text{MB}(M)  \tag{2}$$

where MB(M) denotes the Markov blanket of model M.

**Equation 3: Precision Weighting for Case Selection**

$$\beta(c,m) = \frac{\exp(-F(c,m))}{\sum_{i}\exp(-F(c_i,m))}  \tag{3}$$

where β(c,m) is the precision weight for case c and model m.

**Equation 4: Case-Specific Gradient Descent on Free Energy**

$$\frac{\partial m}{\partial t} = -\kappa_c \cdot \frac{\partial F}{\partial m}  \tag{4}$$

where $\kappa_c$ is the case-specific learning rate.

**Equation 5: Expected Free Energy Reduction in Case Transitions**

$$
\mathbb{E}[\Delta F] = \sum_{s,a}T(s'|s,a)\pi[a|s](F(s,c)-F(s',c'))  \tag{5}
$$

where c and c' represent the initial and target cases respectively.

**Equation 6: Bayes Factor for Case Selection**

$$BF = \frac{p(o|m,c_1)}{p(o|m,c_2)}  \tag{6}$$

**Equation 7: Free Energy Minimization in Case Transitions**

$$
F = D_{KL}[q(s|c,m) || p(s|m)] - \mathbb{E}_{q(s|c,m)}[\log p(o|s,c,m)]  \tag{7}
$$

### Message Passing Rules for Different Cases

These equations illustrate how case assignments modulate standard hierarchical message passing (e.g., in predictive coding) where beliefs/predictions ($\mu$) and prediction errors ($\varepsilon$) flow between adjacent levels (denoted by superscripts 0 and 1). The case-specific weights ($\kappa_c$) determine the influence of each message type based on the model's current functional role.

**Equations 8-12: Case-Specific Message Passing Rules**

$$\text{Nominative [NOM]}: \mu^0 = \mu^0 + \kappa_{NOM} \cdot (\mu^1 - \mu^0)  \tag{8}$$
*(Lower-level prediction $\mu^0$ updated by top-down prediction $\mu^1$, weighted by $\kappa_{NOM}$)*

$$\text{Accusative [ACC]}: \varepsilon^1 = \varepsilon^1 + \kappa_{ACC} \cdot (\varepsilon^0 - \varepsilon^1)  \tag{9}$$
*(Higher-level error $\varepsilon^1$ updated by bottom-up error $\varepsilon^0$, weighted by $\kappa_{ACC}$)*

$$\text{Dative [DAT]}: \mu^0 = \mu^0 + \kappa_{DAT} \cdot (data - \mu^0)  \tag{10}$$
*(Lower-level prediction $\mu^0$ updated directly by incoming 'data', weighted by $\kappa_{DAT}$)*

$$\text{Genitive [GEN]}: output = \mu^0 + \kappa_{GEN} \cdot \eta  \tag{11}$$
*(Output generated based on lower-level prediction $\mu^0$, weighted by $\kappa_{GEN}$, potentially with noise $\eta$)*

$$\text{Instrumental [INS]}: process = f(\mu^1, \varepsilon^0) \cdot \kappa_{INS} \tag{12}$$

*(A process output determined by some function $f$ of top-down prediction $\mu^1$ and bottom-up error $\varepsilon^0$, weighted by $\kappa_{INS}$)*

$$\text{Vocative [VOC]}: activation = \sigma(\kappa_{VOC} \cdot sim(id, address)) \tag{12a}$$

*(Activation state determined by similarity between model identity $id$ and incoming address, weighted by $\kappa_{VOC}$ and passed through activation function $\sigma$)*

where $\kappa_c$ represents case-specific learning rates or precision weights, $\eta$ is a noise term, $\mu^0, \mu^1$ represent beliefs/predictions, and $\varepsilon^0, \varepsilon^1$ represent prediction errors at adjacent hierarchical levels.

### Precision Allocation and Resource Optimization

**Equation 13: Precision Weight Allocation with Temperature**

$$\beta(c,m) = \frac{\exp(-\gamma \cdot F(c,m))}{\sum_i \exp(-\gamma \cdot F(c_i,m))}  \tag{13}$$

where γ is the inverse temperature parameter controlling allocation sharpness.

**Equation 14: Resource-Weighted Free Energy**

$$F_{\beta}(m) = \sum_c \beta(c,m) \cdot F(c,m) \cdot R(c)  \tag{14}$$

where R(c) represents the computational resources allocated to case c.

### Novel Case Formalizations

**Equation 15: Conjunctive Case Free Energy**

$$
F_{CNJ} = D_{KL}[q(s|CNJ,m) || p(s|m)] - \mathbb{E}_{q(s|CNJ,m)}[\log p(o|s,\{m_i\})]  \tag{15}
$$

where {m_i} represents the assembly of connected models.

**Equation 16: Conjunctive Case Message Passing**

$$\mu^{CNJ} = \sum_i w_i \cdot \mu_i + \kappa_{CNJ} \cdot (\prod_i \mu_i - \sum_i w_i \cdot \mu_i)  \tag{16}$$

where w_i are model-specific weighting factors.

**Equation 17: Recursive Case Precision Dynamics**

$$\beta(REC,m) = \frac{\exp(-\gamma \cdot F(REC,m))}{\sum_i \exp(-\gamma \cdot F(c_i,m)) + \exp(-\gamma \cdot F(REC,m))}  \tag{17}$$

#### Glossary of Variables

- $a$: Action (in MDP context, often selecting a case transition)
- $\alpha$: Learning rate (in Neural Process Models context)
- $BF$: Bayes Factor (for comparing model evidence between cases)
- $c, c_i, c', c_1, c_2$: Linguistic case assignment (e.g., NOM, ACC, specific case instances)
- $\text{Case}(M)$: Case assignment of model $M$
- **Case Transformation**: An operation that changes the functional role (case) of a model within the system
- **CEREBRUM**: Case-Enabled Reasoning Engine with Bayesian Representations for Unified Modeling
- $D_{KL}$: Kullback-Leibler divergence
- $\text{data}$: Input data (in Dative case message passing; Eq 10)
- **Declinability**: The capacity of a generative model within CEREBRUM to assume different morphological and functional roles (cases) through transformations
- $E_p[\cdot]$: Expectation with respect to distribution $p$ (Information Geometry)
- $\mathbb{E}[\cdot]$: Expectation operator
- $F$: Variational Free Energy
- $F_{\beta}(m)$: Resource-weighted free energy for model $m$
- $F_{CNJ}$: Free energy for the speculative Conjunctive case
- $f(...)$: Function (used generally; e.g., in Instrumental message passing; Eq 12)
- $g_{ij}$: Fisher information metric tensor component (Information Geometry)
- $i, j$: Indices for summation or tensor components
- $L(M)$: Lyapunov function for model $M$ (Dynamical Systems section)
- $m, M$: Cognitive model
- $\{m_i\}$: Assembly or set of connected models
- $\text{MB}(M)$: Markov blanket of model $M$
- **Morphological Marker (Computational Analogue)**: Specific computational properties (e.g., active interfaces; parameter access patterns; update dynamics) that signal a model's current case assignment within CEREBRUM
- $n$: Model parameter count (Complexity section)
- $O(...)$: Big O notation for computational complexity
- $o$: Observations or sensory data
- $\text{output}$: Output generated by a model (in Genitive case; Eq 11)
- $p(s|...)$: Prior distribution over internal states $s$
- $p(o|...)$: Likelihood distribution of observations $o$
- $p(x|theta)$: Probability distribution of data $x$ given parameters $theta$ (Information Geometry)
- $\text{process}$: Result of a process executed by a model (in Instrumental case; Eq 12)
- $q(s|...)$: Approximate posterior distribution over internal states $s$
- $R(c)$: Computational resources allocated to case $c$
- $REC$: Speculative Recursive case assignment
- $s$: Internal states of a model
- $s'$: Next state (in MDP context; target case assignment)
- $t$: Time variable (in gradient descent context; Eq 4)
- $T$: Transformation function (e.g., $T(m)$ is a transformed model in Eq 1; also MDP transition function)
- $T(s'|s,a)$: State transition function in MDP (probability of transitioning to state $s'$ from state $s$ given action $a$)
- $w_i$: Model-specific weighting factors (in Conjunctive case; Eq 16)
- $\Delta F$: Change in Free Energy
- $\Delta w_{ij}$: Change in synaptic weight between neuron $i$ and $j$ (Neural Process Models section)
- $\beta(c,m)$: Precision weight (allocation) assigned to model $m$ in case $c$
- $\gamma$: Inverse temperature parameter (controlling precision allocation sharpness)
- $\epsilon_i$: Error signal of neuron $i$ (Neural Process Models section)
- $\varepsilon^0, \varepsilon^1$: Error signals used in message passing (representing prediction errors at adjacent hierarchical levels; Eq 9, 12)
- $\eta$: Noise term (Eq 11)
- $\kappa_c$: Case-specific learning rate or precision weight (modulating message updates; Eqs 4, 8-12)
- $\mu^0, \mu^1$: Mean values used in message passing (representing predictions or beliefs at adjacent hierarchical levels)
- $\mu^{CNJ}$: Mean value resulting from Conjunctive case message passing
- $\pi(a|s)$: Policy in MDP (probability of taking action $a$ in state $s$)
- $\sigma'(a_j)$: Derivative of activation function of neuron $j$ (Neural Process Models section)
- $theta, theta_i, theta_j$: Model parameters 