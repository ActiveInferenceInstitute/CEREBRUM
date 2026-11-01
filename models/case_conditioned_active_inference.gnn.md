# GNN spec: CEREBRUM case-conditioned active inference model

## GNNSection
ActInfCerebrumCaseConditioned

## GNNVersionAndFlags
GNN v1.1

## ModelName
CEREBRUM Case-Conditioned Active Inference Model

## ModelAnnotation
Generative-model specification for the discrete surface of
`src/core/active_inference.py` (`ActiveInferenceModel`) and its canonical
concrete instantiation in `src/models/insect/base.py` (`InsectModel`).

One hidden-state factor and one observation modality, with an identity
likelihood by default: the model observes the discretized hidden state
directly. The default instantiation declares five states, five observations,
and three discrete controls; the factor and modality cardinalities are
dimension-generic and are read from `parameters` at construction time.

Runtime storage orders (declared in `## ModelParameters` and checked by
`src/core/gnn.py`):

- the observation matrix is stored as `observation_matrix[state, observation]`,
  the transpose of the canonical GNN `A[observation, state]` order;
- the transition tensor is stored as
  `transition_matrix[previous_state, action, next_state]`, an axis permutation
  of the canonical GNN `B[next_state, previous_state, action]` order.

Case conditioning is the CEREBRUM-specific layer on top of the discrete model.
Each of the eight grammatical cases declares one `precision_focus`; entering a
case rescales a base precision matrix by the case precision
(`Model.get_precision`). The declared focus-to-matrix contract is:

| Precision focus | Rescaled precision |
| --- | --- |
| `likelihood` | likelihood precision |
| `parameters` | prior precision |
| `outputs` | posterior precision |
| `inputs` | likelihood precision |
| `operations` | likelihood and prior precision |
| `contexts` | posterior precision |
| `historical` | prior precision |
| `identity` | prior precision |

The Gaussian belief path is declared in `## Equations`: variational free energy
is the precision-weighted prediction error (accuracy) plus the
KL divergence between posterior and prior (complexity). Entering a case
rescales only the declared target matrices; matrices rescaled by an earlier
transition keep their previous value until a case that targets them is entered.
The case layer is a framework convention for role-conditioned precision, not an
empirical claim about cognition. `## ModelParameters` declares both the focus
and the rescaled matrix for every case (`case_precision_focus_*`,
`case_precision_target_*`); `src/core/gnn.py` and
`tests/core/test_gnn_spec.py` verify the declared mapping against
`_apply_case_transformation`.

Claim boundary: this specification records the implemented structure of the
framework's discrete model and its case-conditioned precision contract. It is
not evidence of behavioral or biological validation.

## StateSpaceBlock
# Hidden state factor (one factor): discretized hidden state.
s_state[5,1,type=int]                # hidden factor 0 (five default states)

# Observation modality (one): the discretized hidden state.
o_observation[5,1,type=int]          # modality 0 (five default observations)

# Likelihood: A[observation, state] (runtime stores the transpose).
A_observation[5,5,type=float]

# Transition tensor: B[next_state, previous_state, action].
B_state[5,5,3,type=float]

# Initial-state prior over hidden states.
D_state[5,type=float]

# Controls for the single controllable factor.
u_control[3,1,type=int]              # discrete control index (3 controls)
π_control[3,type=float]              # policy posterior over the 3 controls

# Free energy and time.
F[1,type=float]
G[1,type=float]
t[1,type=int]

## Connections
# Prior initializes the hidden factor.
D_state>s_state

# Identity likelihood: the modality depends on the state alone.
s_state>A_observation
A_observation>o_observation

# Transitions depend on the state and its control.
s_state>B_state
u_control>B_state
B_state>s_state

# Policy selection and free energy.
G>π_control
π_control>u_control
F-s_state

## InitialParameterization
# A: identity likelihood (observation equals the hidden state),
# declared in canonical GNN order A[observation, state].
A_observation={
  (1.0,0.0,0.0,0.0,0.0),
  (0.0,1.0,0.0,0.0,0.0),
  (0.0,0.0,1.0,0.0,0.0),
  (0.0,0.0,0.0,1.0,0.0),
  (0.0,0.0,0.0,0.0,1.0)
}

# B: transition tensor in canonical (next_state, previous_state, action)
# order. The default insect instantiation is an identity transition for every
# control: each next-state slice is a (previous_state, action) matrix whose
# diagonal entry is one for all three actions. Runtime storage is
# transition_matrix[previous_state, action, next_state].
B_state={
  ( (1.0,1.0,1.0),
    (0.0,0.0,0.0),
    (0.0,0.0,0.0),
    (0.0,0.0,0.0),
    (0.0,0.0,0.0) ),   # next_state 0
  ( (0.0,0.0,0.0),
    (1.0,1.0,1.0),
    (0.0,0.0,0.0),
    (0.0,0.0,0.0),
    (0.0,0.0,0.0) ),   # next_state 1
  ( (0.0,0.0,0.0),
    (0.0,0.0,0.0),
    (1.0,1.0,1.0),
    (0.0,0.0,0.0),
    (0.0,0.0,0.0) ),   # next_state 2
  ( (0.0,0.0,0.0),
    (0.0,0.0,0.0),
    (0.0,0.0,0.0),
    (1.0,1.0,1.0),
    (0.0,0.0,0.0) ),   # next_state 3
  ( (0.0,0.0,0.0),
    (0.0,0.0,0.0),
    (0.0,0.0,0.0),
    (0.0,0.0,0.0),
    (1.0,1.0,1.0) )    # next_state 4
}

# D: uniform prior over hidden states.
D_state={0.2,0.2,0.2,0.2,0.2}

## Equations
# Discrete belief update (Bayes rule with the stored likelihood):
#   L(s) = A_stored[s, :] . o
#   Q(s) = normalize(L * Q_prior)
# Predicted next state distribution:
#   Q(s') = sum_s Q(s) * transition_matrix[s, a, s']
# Predicted observation distribution:
#   Q(o) = sum_s Q(s) * observation_matrix[s, o]
# Greedy action selection (no expected-free-energy term):
#   a* = argmax_s Q(s) mod n_actions
# Variational free energy (Gaussian path):
#   F = 0.5 (o - g(mu))^T P_likelihood (o - g(mu)) + KL[Q || P]
# Case-conditioned precision:
#   P_focus(case) = P_base(focus) * precision(case)
# Column-stochastic normalization (canonical orders):
#   sum_o A[o, s] = 1 for every state s
#   sum_{s'} B[s', s, a] = 1 for every state s and control a

## Time
Dynamic
DiscreteTime=t
ModelTimeHorizon=Unbounded

## ActInfOntologyAnnotation
s_state=HiddenStateFactor0
o_observation=ObservationModality0
A_observation=LikelihoodMatrixModality0
B_state=TransitionMatrixFactor0
D_state=PriorOverHiddenStatesFactor0
u_control=Action
π_control=PolicyVector
F=VariationalFreeEnergy
G=ExpectedFreeEnergy
t=Time

## ModelParameters
model_id: cerebrum_active_inference_model
num_factors: 1
num_hidden_states: 5
num_modalities: 1
num_obs: 5
num_actions: 3
num_timesteps: Unbounded
conditional_independence: true
observation_axis_order: state,observation
transition_axis_order: previous_state,action,next_state
case_precision_focus_nominative: likelihood
case_precision_focus_accusative: parameters
case_precision_focus_genitive: outputs
case_precision_focus_dative: inputs
case_precision_focus_instrumental: operations
case_precision_focus_locative: contexts
case_precision_focus_ablative: historical
case_precision_focus_vocative: identity
case_precision_target_likelihood: likelihood_precision
case_precision_target_parameters: prior_precision
case_precision_target_outputs: posterior_precision
case_precision_target_inputs: likelihood_precision
case_precision_target_operations: likelihood_precision+prior_precision
case_precision_target_contexts: posterior_precision
case_precision_target_historical: prior_precision
case_precision_target_identity: prior_precision

## Footer
CEREBRUM Case-Conditioned Active Inference Model — GNN v1.1 structural spec.
Default five-state insect instantiation plus the eight-case
precision-focus contract; not behavioral or biological validation.

## Signature
pending
