# Supplement 10: Algorithmic Details & Pseudocode

This supplement provides detailed, language-agnostic pseudocode for key algorithms within the CEREBRUM framework to aid understanding, implementation, and reproducibility.

## 10.1 Core `CaseModel` Representation

The fundamental data structure in CEREBRUM is the `CaseModel`, which encapsulates the state, parameters, interfaces, and case-specific behaviors of a model.

```
class CaseModel:
    // Core properties
    State s                  // Current belief state
    Parameters θ             // Model parameters
    Case currentCase         // Current case assignment (NOM, ACC, DAT, etc.)
    PrecisionParameters π    // Precision weights for different model components
    InterfaceMap interfaces  // Available input/output interfaces
    
    // Core methods
    function transform(targetCase)              // Transform to a different case
    function calculateFreeEnergy()              // Compute current free energy
    function updateBeliefs(observation)         // Update beliefs based on observation
    function generatePrediction()               // Generate predictions from current state
    function selectOptimalCase(context)         // Select optimal case for current context
```

The `CaseModel` structure includes both common methods applicable to all cases and specialized methods that become active based on the current case assignment.

## 10.2 Case Transformation Algorithm

The `transform` method changes a model from its current case to a target case, adapting its interfaces, precision weights, and operational characteristics.

```
function transform(model, targetCase):
    // Input: Current model, target case
    // Output: Transformed model
    
    // 1. Verify transformation validity
    if not isValidTransformation(model.currentCase, targetCase):
        throw InvalidTransformationError
    
    // 2. Create new parameter mapping based on transformation type
    newParameters = mapParameters(model.θ, model.currentCase, targetCase)
    
    // 3. Adjust precision weights according to target case requirements
    newPrecision = mapPrecision(model.π, model.currentCase, targetCase)
    
    // 4. Reconfigure input/output interfaces
    newInterfaces = configureInterfaces(targetCase)
    
    // 5. Update state representation if necessary
    newState = adaptState(model.s, model.currentCase, targetCase)
    
    // 6. Create transformed model with new configuration
    transformedModel = new CaseModel(
        state: newState,
        parameters: newParameters,
        currentCase: targetCase,
        precision: newPrecision,
        interfaces: newInterfaces
    )
    
    // 7. Verify coherence of the transformed model
    if not verifyCoherence(transformedModel):
        throw IncoherentTransformationError
    
    return transformedModel
```

### 10.2.1 Parameter Mapping Function

The parameter mapping function translates parameters between cases, preserving relevant information while adapting to the new case's requirements.

```
function mapParameters(parameters, sourceCase, targetCase):
    // Create parameter mapping based on case pair
    mapping = getParameterMapping(sourceCase, targetCase)
    
    // Initialize new parameters with defaults for target case
    newParameters = initializeDefaultParameters(targetCase)
    
    // Apply the mapping to transfer applicable parameters
    for each (sourceParam, targetParam) in mapping:
        newParameters[targetParam] = transformParameter(
            parameters[sourceParam], 
            sourceCase, 
            targetCase
        )
    
    return newParameters
```

### 10.2.2 Precision Remapping Function

The precision remapping function adjusts precision weights to match the emphasis appropriate for the target case.

```
function mapPrecision(precision, sourceCase, targetCase):
    // Create new precision structure for target case
    newPrecision = initializeDefaultPrecision(targetCase)
    
    // Apply case-specific transformations
    switch targetCase:
        case NOM:
            // Emphasize prediction accuracy
            newPrecision.likelihood = HIGH_PRECISION
            newPrecision.prior = MEDIUM_PRECISION
        case ACC:
            // Emphasize belief updating
            newPrecision.likelihood = MEDIUM_PRECISION
            newPrecision.prior = LOW_PRECISION
        case GEN:
            // Emphasize relationship parameters
            newPrecision.relationship = HIGH_PRECISION
            newPrecision.state = MEDIUM_PRECISION
        // ... other cases
    
    return newPrecision
```

### 10.2.3 Interface Configuration Function

The interface configuration function activates the appropriate input/output interfaces for the target case.

```
function configureInterfaces(targetCase):
    // Initialize empty interface map
    interfaces = new InterfaceMap()
    
    // Configure case-specific interfaces
    switch targetCase:
        case NOM:
            // Nominative case emphasizes outputs
            interfaces.addInterface("predictions", INTERFACE_OUTPUT)
            interfaces.addInterface("context", INTERFACE_INPUT, LOW_PRIORITY)
        case ACC:
            // Accusative case emphasizes inputs
            interfaces.addInterface("updates", INTERFACE_INPUT, HIGH_PRIORITY)
            interfaces.addInterface("state", INTERFACE_OUTPUT)
        case DAT:
            // Dative case acts as mediator
            interfaces.addInterface("indirectInput", INTERFACE_INPUT)
            interfaces.addInterface("goal", INTERFACE_OUTPUT)
        // ... other cases
    
    return interfaces
```

## 10.3 Free Energy Calculation Algorithm

The free energy calculation is central to CEREBRUM's operation. While the specific form varies by case (as detailed in Supplement 9), the general algorithm follows this structure:

```
function calculateFreeEnergy(model):
    // Dispatch to case-specific implementation
    switch model.currentCase:
        case NOM:
            return calculateNominativeFreeEnergy(model)
        case ACC:
            return calculateAccusativeFreeEnergy(model)
        // ... other cases
    
    throw UnsupportedCaseError
```

### 10.3.1 Nominative Case Free Energy

```
function calculateNominativeFreeEnergy(model):
    // Emphasizes prediction accuracy
    
    // Calculate complexity term (KL divergence from prior)
    complexity = calculateKLDivergence(model.s, model.prior)
    
    // Calculate accuracy term (expected log-likelihood)
    // Note the enhanced precision α > 1
    accuracy = model.π.likelihood * calculateExpectedLogLikelihood(model)
    
    // Free energy is complexity minus accuracy
    return complexity - accuracy
```

### 10.3.2 Accusative Case Free Energy

```
function calculateAccusativeFreeEnergy(model):
    // Emphasizes efficient belief updates
    
    // Calculate complexity term with reduced weight β < 1
    complexity = model.π.prior * calculateKLDivergence(model.s, model.prior)
    
    // Calculate accuracy term (expected log-likelihood)
    accuracy = calculateExpectedLogLikelihood(model)
    
    // Free energy is complexity minus accuracy
    return complexity - accuracy
```

## 10.4 Case Selection Algorithm

The case selection algorithm uses active inference principles to determine the optimal case for a model given the current context.

```
function selectOptimalCase(model, context):
    // 1. Define possible target cases to consider
    possibleCases = getValidTransformationTargets(model.currentCase)
    
    // 2. Calculate expected free energy for each possible case
    efe = {}
    for each targetCase in possibleCases:
        // Create hypothetical transformed model
        hypotheticalModel = simulateTransform(model, targetCase)
        
        // Calculate information gain (epistemic value)
        infoGain = calculateInformationGain(hypotheticalModel, context)
        
        // Calculate goal alignment (pragmatic value)
        goalAlignment = calculateGoalAlignment(hypotheticalModel, context)
        
        // Combine into expected free energy
        efe[targetCase] = infoGain + goalAlignment
    
    // 3. Apply softmax to get case selection probabilities
    probabilities = softmax(-efe, temperature)
    
    // 4. Select case (sampling or maximum)
    if explorationEnabled:
        selectedCase = sampleFromDistribution(probabilities)
    else:
        selectedCase = argmax(probabilities)
    
    return selectedCase
```

### 10.4.1 Information Gain Calculation

```
function calculateInformationGain(model, context):
    // Calculate mutual information between model state and future observations
    // I(future observations; model state | case)
    
    // Estimate expected posterior after observing context
    expectedPosterior = estimatePosterior(model, context)
    
    // Calculate KL from current posterior to expected posterior
    return calculateKLDivergence(expectedPosterior, model.s)
```

### 10.4.2 Goal Alignment Calculation

```
function calculateGoalAlignment(model, context):
    // Calculate how well the model in this case would achieve goals
    
    // Get goal-oriented preferences (could be from context or model)
    preferences = extractPreferences(context)
    
    // Calculate expected log probability of preferred outcomes
    expectedLogProb = 0
    for each outcome in possibleOutcomes:
        outcomeProbability = predictOutcome(model, outcome)
        expectedLogProb += outcomeProbability * log(preferences[outcome])
    
    return expectedLogProb
```

## 10.5 Multiple Dispatch Mechanism

CEREBRUM uses case-based multiple dispatch to route operations to appropriate implementation based on the model's current case.

```
// Case-based dispatch system

// Registry for case-specific handlers
dispatchRegistry = {}

// Registration function
function registerHandler(case, operation, handler):
    if not dispatchRegistry[operation]:
        dispatchRegistry[operation] = {}
    dispatchRegistry[operation][case] = handler

// Dispatch function
function dispatch(model, operation, ...args):
    if not dispatchRegistry[operation] or not dispatchRegistry[operation][model.currentCase]:
        throw UnsupportedOperationError("Operation not supported for this case")
    
    // Get the case-specific handler
    handler = dispatchRegistry[operation][model.currentCase]
    
    // Execute with the model and additional arguments
    return handler(model, ...args)

// Example handler registrations
registerHandler(NOM, "process", processAsNominative)
registerHandler(ACC, "process", processAsAccusative)
registerHandler(DAT, "process", processAsDative)
// ... etc.

// Usage
function process(model, data):
    return dispatch(model, "process", data)
```

## 10.6 Novel Case Algorithms

The novel cases introduced in Supplement 2 require specialized algorithms to implement their unique behaviors.

### 10.6.1 Conjunctive Case [CNJ] Algorithm

```
function synthesizeJointPrediction(models):
    // Input: List of models to integrate
    // Output: Joint prediction
    
    // 1. Extract individual predictions
    predictions = []
    weights = []
    for each model in models:
        predictions.append(model.generatePrediction())
        // Weight based on precision and reliability
        weights.append(calculateModelWeight(model))
    
    // 2. Normalize weights
    weights = normalize(weights)
    
    // 3. Determine integration method based on prediction types
    if predictionsAreDistributions(predictions):
        // For probabilistic predictions, use product of experts
        jointPrediction = productOfExperts(predictions, weights)
    else:
        // For point predictions, use weighted combination
        jointPrediction = weightedCombination(predictions, weights)
    
    // 4. Check consistency of joint prediction
    consistencyScore = evaluateConsistency(jointPrediction, predictions)
    if consistencyScore < CONSISTENCY_THRESHOLD:
        // Apply consistency optimization
        jointPrediction = optimizeForConsistency(jointPrediction, predictions, weights)
    
    return jointPrediction
```

### 10.6.2 Recursive Case [REC] Algorithm

```
function applySelfTransformation(model, maxDepth=5):
    // Input: Model to recursively transform, maximum recursion depth
    // Output: Result of recursive self-application
    
    // 1. Base case: stop at maximum depth or convergence
    if maxDepth <= 0:
        return model
    
    // 2. Create a copy to transform
    workingModel = copyModel(model)
    
    // 3. Apply model to itself
    transformedState = model.applyTo(workingModel.s)
    workingModel.s = transformedState
    
    // 4. Check for convergence
    if isConverged(workingModel.s, model.s):
        return workingModel
    
    // 5. Recursive application with decreased depth
    return applySelfTransformation(workingModel, maxDepth - 1)
```

### 10.6.3 Metaphorical Case [MET] Algorithm

```
function mapStructure(sourceModel, targetDomain):
    // Input: Source model, target domain description
    // Output: Mapped model in target domain
    
    // 1. Extract structural elements from source model
    sourceStructure = extractStructuralElements(sourceModel)
    
    // 2. Identify potential mappings to target domain
    candidateMappings = identifyPotentialMappings(sourceStructure, targetDomain)
    
    // 3. Score mappings based on structural preservation
    for each mapping in candidateMappings:
        mapping.score = evaluateStructuralPreservation(mapping, sourceStructure, targetDomain)
    
    // 4. Select best mapping
    bestMapping = selectBestMapping(candidateMappings)
    
    // 5. Apply mapping to create new model in target domain
    mappedModel = applyMapping(sourceModel, bestMapping, targetDomain)
    
    // 6. Verify that key invariants are preserved
    if not verifyInvariants(sourceModel, mappedModel, bestMapping):
        // Adjust mapping to preserve critical invariants
        mappedModel = adjustMapping(mappedModel, sourceModel, bestMapping)
    
    return mappedModel
```

### 10.6.4 Explicative Case [EXP] Algorithm

```
function generateExplanation(model, abstractionLevel):
    // Input: Model to explain, desired abstraction level
    // Output: Human-interpretable explanation
    
    // 1. Extract relevant model components based on abstraction level
    relevantComponents = extractRelevantComponents(model, abstractionLevel)
    
    // 2. Compute feature attribution/importance scores
    attributions = computeAttributions(model, relevantComponents)
    
    // 3. Filter to most important features based on attribution
    significantFeatures = filterSignificantFeatures(attributions, SIGNIFICANCE_THRESHOLD)
    
    // 4. Map technical features to domain concepts
    conceptualMapping = mapFeaturesToConcepts(significantFeatures)
    
    // 5. Generate explanation text using conceptual mapping
    explanationStructure = structureExplanation(conceptualMapping, abstractionLevel)
    
    // 6. Validate explanation against model behavior
    if not validateExplanation(explanationStructure, model):
        // Refine explanation to resolve inconsistencies
        explanationStructure = refineExplanation(explanationStructure, model)
    
    // 7. Format explanation according to abstractionLevel
    explanation = formatExplanation(explanationStructure, abstractionLevel)
    
    return explanation
```

### 10.6.5 Diagnostic Case [DIA] Algorithm

```
function diagnoseModel(targetModel, testCases=null):
    // Input: Model to diagnose, optional specific test cases
    // Output: Diagnostic report
    
    // 1. If no test cases provided, generate appropriate test cases
    if not testCases:
        testCases = generateTestCases(targetModel)
    
    // 2. Run tests and collect results
    results = {}
    anomalies = []
    for each test in testCases:
        // Run test and compare to expected behavior
        actual = executeTest(targetModel, test.input)
        expected = test.expectedOutput
        results[test.id] = compareResults(actual, expected)
        
        // Track anomalies
        if isAnomalous(results[test.id]):
            anomalies.append({
                test: test,
                actual: actual,
                expected: expected,
                divergence: calculateDivergence(actual, expected)
            })
    
    // 3. Localize anomalies to specific model components
    if anomalies:
        componentAnalysis = localizeAnomalies(targetModel, anomalies)
        
        // 4. Generate diagnostic hypothesis
        diagnosticHypotheses = generateHypotheses(componentAnalysis)
        
        // 5. Rank hypotheses by likelihood
        rankedHypotheses = rankHypotheses(diagnosticHypotheses, anomalies)
    else:
        rankedHypotheses = []
    
    // 6. Compile diagnostic report
    report = {
        testResults: results,
        anomalies: anomalies,
        hypotheses: rankedHypotheses,
        modelHealth: calculateModelHealth(results)
    }
    
    return report
```

### 10.6.6 Orchestrative Case [ORC] Algorithm

```
function orchestrateModels(task, availableModels, resources):
    // Input: Task to accomplish, available models, resource constraints
    // Output: Orchestration plan
    
    // 1. Decompose task into subtasks
    subtasks = decomposeTask(task)
    
    // 2. Analyze model capabilities and match to subtasks
    modelCapabilities = analyzeCapabilities(availableModels)
    assignments = matchModelsToSubtasks(subtasks, modelCapabilities)
    
    // 3. Create dependency graph for subtasks
    dependencyGraph = createDependencyGraph(subtasks)
    
    // 4. Allocate resources based on priority and constraints
    resourceAllocation = allocateResources(assignments, resources)
    
    // 5. Create execution schedule
    schedule = scheduleExecution(assignments, dependencyGraph, resourceAllocation)
    
    // 6. Define coordination protocol
    protocol = defineCoordinationProtocol(availableModels, assignments)
    
    // 7. Create monitoring and error handling strategy
    errorHandling = defineErrorHandlingStrategy(assignments, dependencyGraph)
    
    // 8. Compile orchestration plan
    plan = {
        assignments: assignments,
        schedule: schedule,
        resourceAllocation: resourceAllocation,
        protocol: protocol,
        errorHandling: errorHandling
    }
    
    return plan
```

### 10.6.7 Generative Case [GEN] Algorithm

```
function generateInstance(latentVector, constraints):
    // Input: Point in latent space, constraints on generated instance
    // Output: Generated instance satisfying constraints
    
    // 1. Apply initial decoding from latent vector
    candidate = decodeFromLatentSpace(latentVector)
    
    // 2. Evaluate constraint satisfaction
    constraintSatisfaction = evaluateConstraints(candidate, constraints)
    
    // 3. If constraints not satisfied, perform constrained optimization
    if not allConstraintsSatisfied(constraintSatisfaction):
        // Define constraint satisfaction objective
        objective = createConstraintObjective(constraints)
        
        // Perform gradient-based optimization in latent space
        optimizedLatent = optimizeLatentVector(latentVector, objective)
        
        // Decode optimized latent vector
        candidate = decodeFromLatentSpace(optimizedLatent)
    
    // 4. Apply post-processing to enhance quality
    candidate = postProcessInstance(candidate)
    
    // 5. Verify novelty (avoid duplicating training data)
    noveltyScore = assessNovelty(candidate)
    if noveltyScore < NOVELTY_THRESHOLD:
        // Adjust to increase novelty while maintaining coherence
        candidate = adjustForNovelty(candidate, noveltyScore)
    
    // 6. Final quality check
    qualityScore = assessQuality(candidate)
    if qualityScore < QUALITY_THRESHOLD:
        // Enhance quality through targeted refinement
        candidate = enhanceQuality(candidate)
    
    return candidate
```

## 10.7 Database Operations for Case-Bearing Models

The following pseudocode outlines common database operations for storing and retrieving case-bearing models.

### 10.7.1 Storing a Model

```
function storeModel(model, database):
    // 1. Serialize model core components
    serializedModel = {
        id: generateUniqueId(),
        case: model.currentCase,
        state: serializeState(model.s),
        parameters: serializeParameters(model.θ),
        precision: serializePrecision(model.π),
        metadata: {
            created: currentTimestamp(),
            version: MODEL_VERSION,
            description: model.metadata.description
        }
    }
    
    // 2. Store in database with appropriate indexing
    database.models.insert(serializedModel)
    
    // 3. Update case-specific indexes
    database.caseIndex.insert({
        case: model.currentCase,
        modelId: serializedModel.id,
        timestamp: currentTimestamp()
    })
    
    return serializedModel.id
```

### 10.7.2 Retrieving Models by Case

```
function getModelsByCase(case, database, limit=10):
    // 1. Query case index
    caseEntries = database.caseIndex.find({case: case})
                                   .sort({timestamp: -1})
                                   .limit(limit)
    
    // 2. Retrieve full models
    modelIds = caseEntries.map(entry => entry.modelId)
    models = database.models.find({id: {$in: modelIds}})
    
    // 3. Deserialize models
    deserializedModels = []
    for each modelData in models:
        deserializedModels.append(deserializeModel(modelData))
    
    return deserializedModels
```

### 10.7.3 Recording a Case Transformation

```
function recordTransformation(sourceModelId, targetModelId, transformationType, database):
    // 1. Create transformation record
    transformation = {
        id: generateUniqueId(),
        sourceModelId: sourceModelId,
        targetModelId: targetModelId,
        transformationType: transformationType,
        timestamp: currentTimestamp(),
        metadata: {}
    }
    
    // 2. Store transformation record
    database.transformations.insert(transformation)
    
    // 3. Update model links
    database.models.update(
        {id: sourceModelId},
        {$push: {transformations: transformation.id}}
    )
    
    database.models.update(
        {id: targetModelId},
        {$set: {sourceTransformation: transformation.id}}
    )
    
    return transformation.id
```

## 10.8 Complexity Notes

The algorithms presented in this supplement have varying computational complexity, which is analyzed in detail in Supplement 7. Key complexity considerations include:

1. **Case Transformation**: O(P) where P is the number of parameters in the model
2. **Free Energy Calculation**: O(S) where S is the size of the state space
3. **Case Selection**: O(C·S) where C is the number of candidate cases and S is the state space size
4. **Conjunctive Integration**: O(M²) where M is the number of models being integrated
5. **Metaphorical Mapping**: O(N³) in the general case, where N is the number of structural elements being mapped
6. **Diagnostic Testing**: O(T·P) where T is the number of test cases and P is the number of model parameters

For practical implementations, approximation techniques described in Supplement 7 can be used to reduce these complexity bounds for large-scale models. 