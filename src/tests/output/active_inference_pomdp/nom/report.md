# POMDP Model in NOM Case

## Subject/Doer of action

Statistical role: Active decision-maker

### Context

The POMDP as an agent actively choosing actions to maximize reward

### Mathematical Representation

```
π(a|s) = argmax_a Q(s,a)
```

### Primary Methods

get_optimal_action(), transition()

### Example

The POMDP DECIDES which action to take next

### Simulation Results

* Simulation steps: 20
* Model parameters:
  - States: 2
  - Observations: 2
  - Actions: ['left', 'right']
* Performance metrics:
  - Decision accuracy: 0.7000
  - Average belief confidence: 0.7691
  - Average belief entropy: 0.7429 bits

### Decision Process Analysis

In the NOMINATIVE case, the POMDP acts as an active decision-maker, choosing actions based on its current beliefs about the world state. The visualizations show the evolution of beliefs over time and the actions chosen by the model.

### Visualizations

1. [POMDP Structure](pomdp_structure.png)
2. [Initial Belief State](initial_belief.png)
3. [Belief Evolution](belief_evolution.png)
4. [Policy Visualization](policy_visualization.png)
5. [Decision Process Analysis](decision_process.png)
6. [Belief Animation](belief_animation.gif)
