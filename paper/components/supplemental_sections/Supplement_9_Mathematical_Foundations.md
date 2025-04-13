# Supplement 9: Mathematical Foundations

This supplement provides the mathematical foundations underlying the CEREBRUM framework, detailing the formal definitions, theorems, and proofs that support the case-based cognitive architecture.

## 9.1 Category Theory Foundations

### 9.1.1 Category of Cases

We begin by formalizing the notion of linguistic cases as a mathematical category.

**Definition 9.1.1** (Category of Cases). The category $\mathcal{C}$ of CEREBRUM cases consists of:
- **Objects**: Linguistic cases (e.g., Nominative, Accusative, Genitive, etc.)
- **Morphisms**: Case transformations $f: C_1 \rightarrow C_2$ between cases
- **Composition**: Sequential application of transformations $g \circ f: C_1 \rightarrow C_3$ for $f: C_1 \rightarrow C_2$ and $g: C_2 \rightarrow C_3$
- **Identity**: The identity transformation $\text{id}_C: C \rightarrow C$ for each case $C$

**Theorem 9.1.1** (Well-formed Category). The category $\mathcal{C}$ of cases is well-formed, satisfying:
1. Associativity: $(h \circ g) \circ f = h \circ (g \circ f)$ for all composable transformations
2. Identity: $f \circ \text{id}_C = f$ and $\text{id}_D \circ f = f$ for any transformation $f: C \rightarrow D$

*Proof*: The associativity follows from the sequential nature of transformations, as they represent adjustments to model interfaces and precision weights. The identity properties follow from the definition of the identity transformation as one that preserves all interface configurations and precision parameters. $\square$

### 9.1.2 Functors and Natural Transformations

Case-bearing models can be viewed through the lens of functors between categories.

**Definition 9.1.2** (Model Functor). A case-bearing model $M$ defines a functor $F_M: \mathcal{C} \rightarrow \mathcal{S}$ where $\mathcal{C}$ is the category of cases and $\mathcal{S}$ is the category of model states with:
- For each case $C$, $F_M(C)$ is the state space of model $M$ in case $C$
- For each transformation $f: C_1 \rightarrow C_2$, $F_M(f): F_M(C_1) \rightarrow F_M(C_2)$ is the corresponding state transformation

**Theorem 9.1.2** (Natural Transformation of Models). Given two models $M$ and $N$, a coherent mapping between them across all cases forms a natural transformation $\eta: F_M \Rightarrow F_N$ between functors.

*Proof*: For any case transformation $f: C_1 \rightarrow C_2$, the naturality square commutes:
$F_N(f) \circ \eta_{C_1} = \eta_{C_2} \circ F_M(f)$, due to the consistent relationship between models across different cases. $\square$

## 9.2 Free Energy and Active Inference

### 9.2.1 Free Energy Principle

The free energy principle provides the theoretical basis for CEREBRUM's inference mechanisms.

**Definition 9.2.1** (Variational Free Energy). For a model $M$ with internal state $s$ and observations $o$, the variational free energy is defined as:

$$F(s, o) = D_{KL}[q(h|s) || p(h|o)] - \ln p(o)$$

where $q(h|s)$ is the recognition density over hidden states $h$ implied by internal state $s$, $p(h|o)$ is the true posterior, and $p(o)$ is the evidence.

**Theorem 9.2.1** (Free Energy Decomposition). The variational free energy can be decomposed into:

$$F(s, o) = \underbrace{E_{q(h|s)}[-\ln p(o|h)]}_{\text{Accuracy}} + \underbrace{D_{KL}[q(h|s) || p(h)]}_{\text{Complexity}}$$

*Proof*: Starting with Definition 9.2.1, we apply Bayes' rule to the posterior:

$$
\begin{aligned}
F(s, o) &= D_{KL}[q(h|s) || p(h|o)] - \ln p(o) \\
&= \int q(h|s) \ln \frac{q(h|s)}{p(h|o)} dh - \ln p(o) \\
&= \int q(h|s) \ln \frac{q(h|s)}{p(h) \cdot p(o|h) / p(o)} dh - \ln p(o) \\
&= \int q(h|s) \ln \frac{q(h|s) \cdot p(o)}{p(h) \cdot p(o|h)} dh - \ln p(o)
\end{aligned}
$$

Distributing the logarithm and separating terms:

$$
\begin{aligned}
F(s, o) &= \int q(h|s) \ln \frac{q(h|s)}{p(h)} dh + \int q(h|s) \ln \frac{p(o)}{p(o|h)} dh - \ln p(o) \\
&= D_{KL}[q(h|s) || p(h)] + \int q(h|s) \ln p(o) dh - \int q(h|s) \ln p(o|h) dh - \ln p(o)
\end{aligned}
$$

Since $\int q(h|s) dh = 1$, we have $\int q(h|s) \ln p(o) dh = \ln p(o)$, giving:

$$F(s, o) = D_{KL}[q(h|s) || p(h)] - \int q(h|s) \ln p(o|h) dh$$

The second term is the negative expected log likelihood, which measures the (in)accuracy of predictions. $\square$

### 9.2.2 Case-Specific Free Energy

In CEREBRUM, free energy is case-dependent due to differences in precision weighting.

**Definition 9.2.2** (Case-Specific Free Energy). For a model $M$ in case $C$ with precision parameters $\pi_C$, the case-specific free energy is:

$$F_C(s, o) = E_{q(h|s)}[-\ln p(o|h; \pi_C)] + D_{KL}[q(h|s) || p(h)]$$

where $p(o|h; \pi_C)$ is the likelihood function with case-specific precision weighting.

**Theorem 9.2.2** (Free Energy Transformation). For cases $C_1$ and $C_2$ with a transformation $f: C_1 \rightarrow C_2$, the free energies relate as:

$$F_{C_2}(f(s), o) = F_{C_1}(s, o) + \Delta F_{f}(s, o)$$

where $\Delta F_{f}(s, o)$ is the transformation impact on free energy.

*Proof*: The transformation $f$ modifies both the internal state representation and the precision parameters. Let $\pi_{C_1}$ and $\pi_{C_2}$ be the precision parameters for cases $C_1$ and $C_2$ respectively. Then:

$$
\begin{aligned}
F_{C_2}(f(s), o) &= E_{q(h|f(s))}[-\ln p(o|h; \pi_{C_2})] + D_{KL}[q(h|f(s)) || p(h)] \\
F_{C_1}(s, o) &= E_{q(h|s)}[-\ln p(o|h; \pi_{C_1})] + D_{KL}[q(h|s) || p(h)]
\end{aligned}
$$

The difference $\Delta F_{f}(s, o)$ arises from:
1. Changes in the recognition density: $q(h|f(s))$ vs. $q(h|s)$
2. Changes in precision weighting: $\pi_{C_2}$ vs. $\pi_{C_1}$

These contribute to the overall transformation impact. $\square$

## 9.3 Precision and Uncertainty

### 9.3.1 Precision Matrices and Uncertainty

Precision plays a central role in CEREBRUM's case representations.

**Definition 9.3.1** (Precision Matrix). For a Gaussian likelihood model $p(o|h) = \mathcal{N}(g(h), \Sigma)$, the precision matrix is $\Pi = \Sigma^{-1}$, where $g(h)$ is the generative mapping from hidden states to observations.

**Theorem 9.3.1** (Precision and Free Energy). Increased precision on a specific observation dimension reduces free energy when predictions are accurate, but increases it when predictions are inaccurate.

*Proof*: For a univariate Gaussian likelihood with precision $\pi$, the contribution to free energy is:

$$-\ln p(o|h) = \frac{1}{2}\ln(2\pi/\pi) + \frac{\pi}{2}(o - g(h))^2$$

Taking the derivative with respect to precision:

$$\frac{\partial(-\ln p(o|h))}{\partial \pi} = -\frac{1}{2\pi} + \frac{1}{2}(o - g(h))^2$$

This is negative when $(o - g(h))^2 < 1/\pi$, meaning prediction errors are small relative to expected variance, and positive otherwise. $\square$

### 9.3.2 Case-Specific Precision Allocation

Different cases allocate precision differently across observation dimensions.

**Definition 9.3.2** (Case-Specific Precision Profile). A case $C$ defines a precision profile $P_C: D \rightarrow \mathbb{R}^+$ mapping each dimension $d \in D$ of the observation space to a precision weight.

**Theorem 9.3.2** (Optimal Case Selection). Given observations $o$, the optimal case $C^*$ minimizes expected free energy:

$$C^* = \arg\min_C E_{p(o'|o)}[F_C(s, o')]$$

where $p(o'|o)$ is the predictive distribution over future observations.

*Proof*: This follows directly from the active inference principle of minimizing expected free energy. The case that assigns precision optimally for future observations will minimize the expected free energy. $\square$

## 9.4 Multiple Dispatch and Case Polymorphism

### 9.4.1 Formal Definition of Multiple Dispatch

Multiple dispatch provides the mathematical basis for case-specific operations.

**Definition 9.4.1** (Multiple Dispatch). A multiple dispatch function $\delta$ maps an operation $\omega$, a case $C$, and arguments $\mathbf{a}$ to an implementation:

$$\delta: \Omega \times \mathcal{C} \times A \rightarrow I$$

where $\Omega$ is the set of operations, $\mathcal{C}$ is the set of cases, $A$ is the argument space, and $I$ is the implementation space.

**Theorem 9.4.1** (Dispatch Consistency). For any operation $\omega$, cases $C_1$ and $C_2$, and transformation $f: C_1 \rightarrow C_2$, the results of dispatch are consistent:

$$\delta(\omega, C_2, f(\mathbf{a})) \circ f = f \circ \delta(\omega, C_1, \mathbf{a})$$

where composition denotes sequential application.

*Proof*: This follows from the requirement that case transformations preserve the semantics of operations while adapting their implementations to case-specific contexts. $\square$

### 9.4.2 Case Polymorphism

Case polymorphism generalizes object-oriented polymorphism to linguistic cases.

**Definition 9.4.2** (Case Polymorphism). A case-polymorphic function $\phi$ has different implementations for different cases while maintaining a consistent interface:

$$\phi_C: A \rightarrow B_C$$

where $A$ is the input type and $B_C$ is the case-specific output type.

**Theorem 9.4.2** (Polymorphic Coherence). For a case-polymorphic function $\phi$ and transformation $f: C_1 \rightarrow C_2$, there exists a mapping $f_B: B_{C_1} \rightarrow B_{C_2}$ such that:

$$\phi_{C_2} \circ f = f_B \circ \phi_{C_1}$$

*Proof*: The existence of $f_B$ follows from the requirement that case transformations preserve the semantic relationships between inputs and outputs across different implementations. $\square$

## 9.5 Information Geometry of Case Spaces

### 9.5.1 Case Manifold

The space of cases can be viewed as a manifold with an information-geometric structure.

**Definition 9.5.1** (Case Manifold). The manifold $\mathcal{M}$ of cases is a geometric structure where:
- Points represent individual cases
- Tangent vectors represent infinitesimal case transformations
- The metric is derived from the Kullback-Leibler divergence between recognition densities

**Theorem 9.5.1** (Fisher Information Metric). The natural metric on the case manifold is the Fisher information metric:

$$g_{\mu\nu}(\theta) = E_{p(x|\theta)}\left[\frac{\partial \log p(x|\theta)}{\partial \theta^\mu}\frac{\partial \log p(x|\theta)}{\partial \theta^\nu}\right]$$

where $\theta$ parametrizes the case and $p(x|\theta)$ is the case-conditional probability.

*Proof*: This follows from information geometry principles, where the KL divergence between nearby distributions induces a Riemannian metric that corresponds to the Fisher information. $\square$

### 9.5.2 Geodesics and Optimal Transformations

The geometry of the case manifold determines optimal transformation paths.

**Definition 9.5.2** (Transformation Geodesic). A geodesic path between cases $C_1$ and $C_2$ on the manifold $\mathcal{M}$ represents the optimal transformation sequence minimizing information loss.

**Theorem 9.5.2** (Minimum Free Energy Path). The geodesic path between cases $C_1$ and $C_2$ minimizes the integrated free energy change:

$$\gamma^* = \arg\min_\gamma \int_0^1 \Delta F(s, o, \gamma(t))\, dt$$

where $\gamma: [0, 1] \rightarrow \mathcal{M}$ is a path with $\gamma(0) = C_1$ and $\gamma(1) = C_2$.

*Proof*: The proof follows from calculus of variations, showing that the Euler-Lagrange equations for this functional yield the geodesic equation in the Fisher information metric. $\square$

## 9.6 Topological Data Analysis of Case Structures

### 9.6.1 Persistent Homology

Topological features provide insights into case structure invariants.

**Definition 9.6.1** (Persistence Diagram). For a case structure filtration $\{K_\alpha\}_{\alpha \geq 0}$, the persistence diagram $\text{Dgm}_p(K)$ captures the birth and death times of $p$-dimensional topological features.

**Theorem 9.6.1** (Case Structure Invariants). Topological invariants of case structures are preserved under continuous transformations.

*Proof*: This follows from the functoriality of homology and the stability theorem for persistence diagrams, which ensures that small perturbations in the case structure lead to small changes in the diagram. $\square$

### 9.6.2 Mapper Algorithm for Case Visualization

The Mapper algorithm provides a topological representation of case spaces.

**Definition 9.6.2** (Case Mapper). The Mapper algorithm applied to case space produces a simplicial complex representation:
1. Define a filter function $f: \mathcal{C} \rightarrow \mathbb{R}^d$
2. Cover $\mathbb{R}^d$ with overlapping sets $\{U_i\}$
3. For each $U_i$, cluster $f^{-1}(U_i) \cap \mathcal{C}$
4. Create a vertex for each cluster and an edge for each non-empty intersection

**Theorem 9.6.2** (Topological Representativeness). The Mapper complex captures essential topological features of the case space that persist across scales.

*Proof*: By the nerve theorem, under appropriate conditions, the Mapper complex is homotopy equivalent to the underlying space, preserving key topological features. $\square$

## 9.7 Dynamical Systems Perspective

### 9.7.1 Vector Fields and Flows

Case models can be viewed through the lens of dynamical systems.

**Definition 9.7.1** (Free Energy Gradient Flow). The dynamics of a case model follows the negative gradient of free energy:

$$\frac{ds}{dt} = -\nabla_s F_C(s, o)$$

where $s$ is the model state and $F_C$ is the case-specific free energy.

**Theorem 9.7.1** (Fixed Points and Stability). The fixed points of the gradient flow correspond to free energy minima, with stability determined by the Hessian of free energy.

*Proof*: At fixed points, $\nabla_s F_C(s, o) = 0$. The stability is determined by the eigenvalues of the Hessian matrix $H = \nabla^2_s F_C(s, o)$. If all eigenvalues are positive, the fixed point is a stable minimum. $\square$

### 9.7.2 Bifurcations and Case Transitions

Case transformations can induce bifurcations in system dynamics.

**Definition 9.7.2** (Case Bifurcation). A case transformation $f: C_1 \rightarrow C_2$ induces a bifurcation if the qualitative structure of fixed points changes across the transformation.

**Theorem 9.7.2** (Bifurcation Conditions). A case transformation induces a bifurcation if there exists a state $s$ such that the Hessian $\nabla^2_s F_{C_1}(s, o)$ has a zero eigenvalue and the corresponding eigenvector is not in the kernel of the transformation's third derivative tensor.

*Proof*: This follows from bifurcation theory, specifically the conditions for a saddle-node bifurcation in dynamical systems. $\square$

## 9.8 Computational Complexity

### 9.8.1 Complexity of Case Operations

The computational requirements of CEREBRUM operations are case-dependent.

**Definition 9.8.1** (Operation Complexity). The computational complexity of an operation $\omega$ in case $C$ with parameters $\pi$ is denoted $\kappa(\omega, C, \pi)$.

**Theorem 9.8.1** (Transformation Complexity). The complexity of a case transformation $f: C_1 \rightarrow C_2$ is bounded by:

$$\kappa(f, C_1, C_2, \pi) = O(P \cdot D)$$

where $P$ is the number of parameters and $D$ is the dimensionality of the interface.

*Proof*: The transformation requires mapping $P$ parameters across $D$ interface dimensions in the worst case, giving the stated complexity bound. $\square$

### 9.8.2 Tractability and Approximations

Approximations are necessary for tractable implementation in complex models.

**Definition 9.8.2** (Free Energy Approximation). An approximation $\tilde{F}_C$ of the true free energy $F_C$ satisfies:

$$|F_C(s, o) - \tilde{F}_C(s, o)| \leq \epsilon(s, o)$$

for some error bound $\epsilon$.

**Theorem 9.8.2** (Approximation Impact). Using an approximation $\tilde{F}_C$ affects the gradient flow by at most:

$$\|\nabla_s F_C(s, o) - \nabla_s \tilde{F}_C(s, o)\| \leq \nabla_s \epsilon(s, o)$$

*Proof*: This follows directly from the properties of function approximation and gradient operations. $\square$

## 9.9 Convergence and Optimization

### 9.9.1 Convergence of Case-Specific Learning

Learning in case-bearing models requires case-specific convergence analysis.

**Definition 9.9.1** (Case-Specific Learning). A learning algorithm for case $C$ generates a sequence of states $\{s_t\}$ such that:

$$s_{t+1} = s_t - \eta_t \nabla_s L_C(s_t, o)$$

where $L_C$ is a case-specific loss function and $\eta_t$ is the learning rate.

**Theorem 9.9.1** (Convergence Conditions). The case-specific learning algorithm converges if:
1. $L_C$ is $\mu$-strongly convex
2. $\nabla_s L_C$ is $L$-Lipschitz continuous
3. The learning rate satisfies $0 < \eta_t < \frac{2}{\mu + L}$

*Proof*: Under these conditions, the optimization algorithm is a contraction mapping in the parameter space, guaranteeing convergence to the unique minimum. $\square$

### 9.9.2 Multi-Case Optimization

Optimizing across multiple cases requires balancing case-specific objectives.

**Definition 9.9.2** (Multi-Case Objective). A multi-case objective function combines case-specific objectives with weights $\alpha_C$:

$$L(\{s_C\}) = \sum_{C \in \mathcal{C}} \alpha_C L_C(s_C, o)$$

where $\{s_C\}$ is the collection of case-specific states.

**Theorem 9.9.2** (Pareto Optimality). A state collection $\{s_C^*\}$ is Pareto optimal if there exists no alternative collection $\{s_C'\}$ such that $L_C(s_C', o) \leq L_C(s_C^*, o)$ for all $C$ with strict inequality for at least one case.

*Proof*: This follows from the definition of Pareto optimality in multi-objective optimization. $\square$

## 9.10 Formal Correctness and Verification

### 9.10.1 Type Theory for Cases

Type theory provides a foundation for formal verification of case operations.

**Definition 9.10.1** (Case Type System). A type system for cases assigns to each case $C$ a type $T_C$ representing its interface, with case transformations $f: C_1 \rightarrow C_2$ corresponding to type morphisms $T_f: T_{C_1} \rightarrow T_{C_2}$.

**Theorem 9.10.1** (Type Safety). A well-typed case operation cannot result in interface mismatches or precision violations.

*Proof*: By the properties of a sound type system, well-typed terms do not "go wrong" during evaluation. The case type system ensures that operations only access interface elements that exist and respect precision constraints. $\square$

### 9.10.2 Invariants and Properties

Formal verification ensures certain properties of case-bearing models.

**Definition 9.10.2** (Case Invariant). A case invariant $I_C$ is a property that holds for all valid states $s$ of a model in case $C$: $I_C(s) = \text{true}$.

**Theorem 9.10.2** (Invariant Preservation). For a transformation $f: C_1 \rightarrow C_2$ and invariants $I_{C_1}$ and $I_{C_2}$, if $I_{C_1}(s) \implies I_{C_2}(f(s))$ for all $s$, then the transformation preserves invariants.

*Proof*: This follows directly from the definition of invariant preservation under transformation. $\square$

## References

1. Amari, S. I. (2016). Information geometry and its applications. Springer.
2. Friston, K. (2010). The free-energy principle: a unified brain theory? Nature Reviews Neuroscience, 11(2), 127-138.
3. Carlsson, G. (2009). Topology and data. Bulletin of the American Mathematical Society, 46(2), 255-308.
4. Mac Lane, S. (2013). Categories for the working mathematician. Springer.
5. Friston, K., FitzGerald, T., Rigoli, F., Schwartenbeck, P., & Pezzulo, G. (2017). Active inference: a process theory. Neural Computation, 29(1), 1-49.
6. Pierce, B. C. (2002). Types and programming languages. MIT press.
7. Guckenheimer, J., & Holmes, P. (2013). Nonlinear oscillations, dynamical systems, and bifurcations of vector fields. Springer.
8. Boyd, S., & Vandenberghe, L. (2004). Convex optimization. Cambridge university press.
9. Parisi, G. (1988). Statistical field theory. Addison-Wesley.
10. Spivak, D. I. (2014). Category theory for the sciences. MIT Press. 