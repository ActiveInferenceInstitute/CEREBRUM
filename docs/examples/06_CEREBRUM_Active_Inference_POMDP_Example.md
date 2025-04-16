# CEREBRUM Example: Active Inference with POMDPs

This document provides an example of applying the CEREBRUM framework to understand Active Inference agents modeled as Partially Observable Markov Decision Processes (POMDPs), as demonstrated in `src/tests/test_active_inference_pomdp.py`.

## Introduction to Active Inference and POMDPs

**Active Inference** is a theoretical framework originating from computational neuroscience, suggesting that biological agents (like the brain) minimize prediction errors about their sensory inputs. Agents achieve this by actively sampling their environment and updating their internal models (beliefs) about the world's hidden states.

**POMDPs** provide a mathematical framework for modeling decision-making in situations where the agent cannot directly observe the true state of the world (partially observable). A POMDP involves:
- **States (S):** The possible true states of the environment.
- **Actions (A):** The actions the agent can take.
- **Observations (O):** The sensory information the agent receives.
- **Transition Probabilities (T):** The likelihood of moving between states given an action \(P(s' | s, a)\).
- **Observation Probabilities (Z):** The likelihood of receiving an observation given the true state \(P(o | s')\).
- **Rewards (R):** (Optional) Rewards associated with states or transitions.

Active Inference often utilizes POMDPs. The agent maintains a **belief state** (a probability distribution over hidden states) and selects actions to minimize expected **free energy**, a quantity that balances exploring the environment (reducing uncertainty) and exploiting known rewards (reaching preferred states).

## CEREBRUM Cases for Active Inference POMDPs

The CEREBRUM framework uses linguistic cases to dissect the different functional roles and perspectives within the Active Inference POMDP model. This allows for a more nuanced analysis of the agent's behavior. The test suite (`test_active_inference_pomdp.py`) explores these facets:

### 1. NOMINATIVE Case: The Agent as Actor

- **Linguistic Meaning:** Subject/Doer of action.
- **Active Inference Context:** Focuses on the agent *actively* selecting and executing actions based on its current beliefs and policies to minimize expected free energy. It's about the agent *causing* a change or interaction.
- **Test Focus:** Simulating the agent's action selection process (`get_optimal_action`), executing transitions in the environment (`transition`), and observing the immediate outcome. Visualizations might show the chosen action sequence or the resulting state trajectory.
- **Why it Matters:** Understanding how the agent's beliefs translate into purposeful behavior.

### 2. ACCUSATIVE Case: The Agent/Beliefs as Object of Update

- **Linguistic Meaning:** Direct object/Receiver of action.
- **Active Inference Context:** Focuses on the agent's internal belief state *being updated* as a result of receiving new observations. The belief state is the *object* being modified by the inference process.
- **Test Focus:** Examining the belief update mechanism (`update_belief`) after an action-observation cycle. Visualizations often include plots showing the evolution of the belief state distribution over time (e.g., bar charts, heatmaps) or metrics derived from it, like entropy (uncertainty).
- **Why it Matters:** Understanding how the agent learns and adapts its internal model based on sensory evidence.

### 3. DATIVE Case: The Agent as Recipient of Observation

- **Linguistic Meaning:** Indirect object/Recipient.
- **Active Inference Context:** Focuses on the agent *receiving* sensory input (observations) from the environment. The observation function (`observe`) is central here.
- **Test Focus:** Analyzing the relationship between hidden states and the observations they generate. How informative are observations? How do they constrain the agent's beliefs? Visualizations might compare true states to generated observations or analyze the observation likelihood matrix (Z).
- **Why it Matters:** Understanding the nature and quality of the sensory information the agent has access to.

### 4. GENITIVE Case: The Belief State's Properties

- **Linguistic Meaning:** Possessive/Source.
- **Active Inference Context:** Focuses on the properties *of* the agent's belief state, such as its uncertainty (entropy) or its divergence from the true state. The belief state is the *source* of these properties.
- **Test Focus:** Calculating and tracking metrics derived *from* the belief state, like Shannon entropy or Kullback-Leibler divergence. Visualizations often plot these metrics over time to show how belief quality evolves.
- **Why it Matters:** Quantifying the agent's certainty and accuracy in its world model.

### 5. INSTRUMENTAL Case: The POMDP Model as a Tool

- **Linguistic Meaning:** By means of/Using.
- **Active Inference Context:** Views the entire POMDP structure (transition matrix T, observation matrix Z) as the *instrument* or *method* by which the agent models the world and plans its actions.
- **Test Focus:** Analyzing the structure of the T and Z matrices themselves. Visualizations often include heatmaps of these matrices or network diagrams representing the POMDP's connectivity.
- **Why it Matters:** Understanding the underlying generative model the agent assumes about its environment.

### 6. LOCATIVE Case: The Agent's Position in Belief Space

- **Linguistic Meaning:** In/At/Within (Location).
- **Active Inference Context:** Focuses on the agent's belief state as a point *within* the space of all possible probability distributions (the probability simplex). How does the belief move *within* this space?
- **Test Focus:** Visualizing the trajectory of the belief state vector within the belief simplex (often projected to 2D or 3D for visualization). Analyzing convergence points or attractors in belief space.
- **Why it Matters:** Understanding the dynamics of belief updating as movement within a structured space.

### 7. ABLATIVE Case: The Environment/Observation as Source

- **Linguistic Meaning:** From/Out of/Because of.
- **Active Inference Context:** Focuses on the environment or the true state as the *source* from which observations originate, influencing the agent's beliefs. It emphasizes the causal link from the world *to* the agent's perception.
- **Test Focus:** Analyzing how changes in the true environmental state *cause* specific observations and subsequent belief updates. May involve simulating interventions or changes in the environment.
- **Why it Matters:** Understanding how the external world drives the agent's internal inference process.

### 8. VOCATIVE Case: Addressing/Interacting with the Agent

- **Linguistic Meaning:** Direct address/Invocation.
- **Active Inference Context:** Views the agent as an entity that can be queried or interacted with. How does the agent respond *when given* a specific input or scenario?
- **Test Focus:** Simulating specific scenarios or querying the agent's belief/action under particular conditions (e.g., "What would you do *if* you received observation X?").
- **Why it Matters:** Evaluating the agent's behavior in specific, targeted situations or for interactive control.

## Demonstration (`test_active_inference_pomdp.py`)

The test suite implements these cases by:
1.  **Defining a POMDP:** Sets up transition (T) and observation (Z) matrices for a simple environment.
2.  **Defining Cases:** Creates specific linguistic interpretations and associated data/parameters for each CEREBRUM case (e.g., assigning specific goals or initial beliefs relevant to the case).
3.  **Instantiating the Model:** Creates `POMDPModel` instances, potentially tailored for each case.
4.  **Running Simulations:** Executes action-perception cycles, updating beliefs.
5.  **Generating Visualizations:** Creates plots and animations specific to each case, stored in `src/tests/output/active_inference_pomdp/`. These visualizations directly illustrate the perspective offered by that case:
    - Belief state evolution (bar charts, often animated) - ACCUSATIVE, LOCATIVE
    - State/Observation trajectories - NOMINATIVE, DATIVE
    - Entropy/KL Divergence plots - GENITIVE
    - POMDP structure heatmaps - INSTRUMENTAL
    - Parameter sensitivity plots - ABLATIVE/VOCATIVE (depending on what's varied)

## Conclusion

Applying CEREBRUM cases to Active Inference POMDPs provides a structured way to analyze different facets of agent behavior. Instead of viewing the agent monolithically, we can adopt different perspectives (agent as actor, learner, perceiver, etc.) corresponding to the linguistic cases. The visualizations generated in the test suite serve as concrete examples of how each case highlights specific aspects of the agent's interaction with its partially observable world, its internal belief updating, and its action selection process aimed at minimizing free energy. This multi-faceted approach aids in deeper understanding, debugging, and interpretation of complex agent models. 