# Supplement 1: Mathematical Formalization

This supplement contains all mathematical formalizations referenced throughout the paper, organized by equation number.

## 1.1 Variational Free Energy Framework

CEREBRUM builds on the foundational concept of variational free energy in active inference, establishing a principled approach to model transformations. The variational free energy $F$ for a model $m$ with internal states $s$ and observations $o$ is:

$$F = D_{KL}[q(s|T(m))||p(s|m)] - \mathbb{E}_{p}[\log p(o|s,T(m))]$$

This formulation, where $T(m)$ represents a case transformation applied to model $m$, captures two essential quantities: (1) the KL divergence between the recognition density and prior, measuring complexity; and (2) the expected log-likelihood, measuring accuracy. Together, they form the evidence lower bound that models seek to minimize.

## 1.2 Markov Blanket Formulation

A key insight in CEREBRUM is the interpretation of case transformations as operations on Markov blankets. The Markov blanket of a model $M$ contains all variables that shield it from the rest of the network, comprising parents, children, and co-parents. We define:

$$\text{Case}(M) \subseteq \text{MB}(M)$$

This indicates that a case assignment acts on a subset of the Markov blanket, modifying how information flows between the model and its environment.

## 1.3 Precision-Weighted Case Selection

The probability of a model assuming a particular case $c$ is determined by its ability to minimize free energy. The softmax function provides a natural mechanism for case selection:

$$\beta(c,m) = \frac{\exp(-F(c,m))}{\sum_{i}\exp(-F(c_i,m))}$$

Where $\beta(c,m)$ represents the probability of model $m$ adopting case $c$. This allows for dynamic case assignment based on contextual factors and observations.

## 1.4 Dynamical Implementation

The dynamics of case transformations can be implemented through gradient flows on free energy:

$$\frac{\partial m}{\partial t} = -\kappa_c \cdot \frac{\partial F}{\partial m}$$

Here, $\kappa_c$ is a case-specific learning rate that determines how quickly the model adapts when in a particular case. This provides a continuous-time formulation of case-based learning.

## 1.5 Expected Free Energy Minimization

For planning and policy selection, CEREBRUM extends to expected free energy minimization over sequences of case transformations:

$$\mathbb{E}[\Delta F] = \sum_{s,a}T(s'|s,a)\pi[a|s](F(s,c)-F(s',c'))$$

Where $T(s'|s,a)$ is the transition probability from state $s$ to $s'$ given action $a$, and $\pi[a|s]$ is the policy. This allows for optimal sequencing of case transformations to achieve goals.

## 1.6 Bayesian Model Comparison Between Cases

To evaluate competing case assignments, we employ Bayesian model comparison using the Bayes Factor:

$$BF = \frac{p(o|m,c_1)}{p(o|m,c_2)}$$

This quantifies the relative evidence for model $m$ being in case $c_1$ versus case $c_2$ given observations $o$.

## 1.7 Case-Specific Free Energy

The case-specific free energy explicitly includes the case $c$ in the formulation:

$$F = D_{KL}[q(s|c,m) || p(s|m)] - \mathbb{E}_{q(s|c,m)}[\log p(o|s,c,m)]$$

This allows different cases to maintain distinct recognition densities and likelihood functions while operating on the same underlying model.

## 1.8 Core Linguistic Case Equations

CEREBRUM defines mathematical operations for each core linguistic case, establishing precise transformations:

$$\text{Nominative [NOM]}: \mu^0 = \mu^0 + \kappa_{NOM} \cdot (\mu^1 - \mu^0)$$
*(Lower-level prediction $\mu^0$ updated by top-down prediction $\mu^1$, weighted by $\kappa_{NOM}$)*

$$\text{Accusative [ACC]}: \varepsilon^1 = \varepsilon^1 + \kappa_{ACC} \cdot (\varepsilon^0 - \varepsilon^1)$$
*(Higher-level error $\varepsilon^1$ updated by bottom-up error $\varepsilon^0$, weighted by $\kappa_{ACC}$)*

$$\text{Dative [DAT]}: \mu^0 = \mu^0 + \kappa_{DAT} \cdot (data - \mu^0)$$
*(Lower-level prediction $\mu^0$ updated directly by incoming 'data', weighted by $\kappa_{DAT}$)*

$$\text{Genitive [GEN]}: output = \mu^0 + \kappa_{GEN} \cdot \eta$$
*(Output generated based on lower-level prediction $\mu^0$, weighted by $\kappa_{GEN}$ and noise $\eta$)*

$$\text{Instrumental [INS]}: process = f(\mu^1, \varepsilon^0) \cdot \kappa_{INS}$$
*(Process defined as a function of top-down prediction and bottom-up error, scaled by $\kappa_{INS}$)*

$$\text{Vocative [VOC]}: activation = \sigma(\kappa_{VOC} \cdot sim(id, address))$$
*(Activation determined by similarity between model's identity and the addressing signal, weighted by $\kappa_{VOC}$)*

## 1.9 Precision-Weighted Mixture of Cases

Models can simultaneously exist in multiple cases with varying probabilities. The case probability is precision-weighted:

$$\beta(c,m) = \frac{\exp(-\gamma \cdot F(c,m))}{\sum_i \exp(-\gamma \cdot F(c_i,m))}$$

Where $\gamma$ represents the precision or confidence in case assignments. This enables nuanced, multimodal model behavior.

## 1.10 Composite Free Energy

The effective free energy for a model in a mixture of cases is:

$$F_{\beta}(m) = \sum_c \beta(c,m) \cdot F(c,m) \cdot R(c)$$

Where $R(c)$ represents the relevance or priority of case $c$. This formulation allows for weighted importance across different case assignments.

## 1.11 Conjunction of Models

For composite systems combining multiple models, the Conjunctive case [CNJ] has a specialized free energy:

$$F_{CNJ} = D_{KL}[q(s|CNJ,m) || p(s|m)] - \mathbb{E}_{q(s|CNJ,m)}[\log p(o|s,\{m_i\})]$$

This formulation accounts for emergent properties arising from model interactions, where $\{m_i\}$ represents the set of constituent models.

## 1.12 Conjunctive Mean Distribution

The mean prediction in the Conjunctive case is:

$$\mu^{CNJ} = \sum_i w_i \cdot \mu_i + \kappa_{CNJ} \cdot (\prod_i \mu_i - \sum_i w_i \cdot \mu_i)$$

This combines weighted averages with multiplicative interactions, controlled by $\kappa_{CNJ}$, representing nonlinear emergent effects.

## 1.13 Recursive Case Formulation

The Recursive case [REC] allows for self-reference, with probability:

$$\beta(REC,m) = \frac{\exp(-\gamma \cdot F(REC,m))}{\sum_i \exp(-\gamma \cdot F(c_i,m)) + \exp(-\gamma \cdot F(REC,m))}$$

This creates a distinct pathway for models to operate on themselves, enabling self-modification capabilities.

## 1.14 Glossary of Variables

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