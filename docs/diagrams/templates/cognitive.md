# Cognitive Architecture Template

## Usage
This template is designed for neural network architectures, cognitive systems, and brain-inspired computing models with CEREBRUM case mappings and modern styling.

## Template Code

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

graph TD
    %% Style definitions for cognitive components
    classDef sensory fill:#74B9FF,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef memory fill:#A29BFE,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef processing fill:#00B894,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef motor fill:#FD79A8,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef integration fill:#FDCB6E,stroke:#2C3E50,stroke-width:2px,color:#2C3E50,font-weight:600
    classDef modulation fill:#FF7675,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    classDef case fill:#4ECDC4,stroke:#2C3E50,stroke-width:3px,color:#FFFFFF,font-weight:bold
    
    %% Main cognitive system
    CognitiveSystem["🧠 Cognitive System<br/><small>Integrated neural architecture</small>"]:::case
    
    %% Sensory processing [DAT]
    subgraph "Sensory Processing [DAT]"
        direction TB
        Visual["👁️ Visual Cortex<br/><small>Visual feature extraction</small>"]:::sensory
        Auditory["👂 Auditory Cortex<br/><small>Auditory processing</small>"]:::sensory
        Somatosensory["🖐️ Somatosensory Cortex<br/><small>Tactile processing</small>"]:::sensory
        Olfactory["👃 Olfactory Bulb<br/><small>Olfactory processing</small>"]:::sensory
        
        Visual --> SensoryIntegration["🔗 Sensory Integration<br/><small>Multi-modal fusion</small>"]:::integration
        Auditory --> SensoryIntegration
        Somatosensory --> SensoryIntegration
        Olfactory --> SensoryIntegration
    end
    
    %% Memory systems [ACC]
    subgraph "Memory Systems [ACC]"
        direction LR
        WorkingMemory["💭 Working Memory<br/><small>Short-term storage</small>"]:::memory
        EpisodicMemory["📚 Episodic Memory<br/><small>Event sequences</small>"]:::memory
        SemanticMemory["📖 Semantic Memory<br/><small>Conceptual knowledge</small>"]:::memory
        ProceduralMemory["🎯 Procedural Memory<br/><small>Skill execution</small>"]:::memory
        
        WorkingMemory --> MemoryIntegration["🔗 Memory Integration<br/><small>Cross-modal association</small>"]:::integration
        EpisodicMemory --> MemoryIntegration
        SemanticMemory --> MemoryIntegration
        ProceduralMemory --> MemoryIntegration
    end
    
    %% Processing centers [NOM]
    subgraph "Processing Centers [NOM]"
        direction TB
        PrefrontalCortex["🧠 Prefrontal Cortex<br/><small>Executive control</small>"]:::processing
        ParietalCortex["🧭 Parietal Cortex<br/><small>Spatial processing</small>"]:::processing
        TemporalCortex["⏰ Temporal Cortex<br/><small>Temporal processing</small>"]:::processing
        OccipitalCortex["👁️ Occipital Cortex<br/><small>Visual processing</small>"]:::processing
        
        PrefrontalCortex --> ProcessingIntegration["🔗 Processing Integration<br/><small>Cross-regional coordination</small>"]:::integration
        ParietalCortex --> ProcessingIntegration
        TemporalCortex --> ProcessingIntegration
        OccipitalCortex --> ProcessingIntegration
    end
    
    %% Motor systems [GEN]
    subgraph "Motor Systems [GEN]"
        direction LR
        MotorCortex["🏃 Motor Cortex<br/><small>Movement planning</small>"]:::motor
        Cerebellum["⚖️ Cerebellum<br/><small>Motor coordination</small>"]:::motor
        BasalGanglia["🎯 Basal Ganglia<br/><small>Action selection</small>"]:::motor
        SpinalCord["🦴 Spinal Cord<br/><small>Motor execution</small>"]:::motor
        
        MotorCortex --> MotorIntegration["🔗 Motor Integration<br/><small>Coordinated movement</small>"]:::integration
        Cerebellum --> MotorIntegration
        BasalGanglia --> MotorIntegration
        SpinalCord --> MotorIntegration
    end
    
    %% Modulatory systems [INS]
    subgraph "Modulatory Systems [INS]"
        direction TB
        Dopaminergic["💊 Dopaminergic<br/><small>Reward processing</small>"]:::modulation
        Serotonergic["🌅 Serotonergic<br/><small>Mood regulation</small>"]:::modulation
        Noradrenergic["⚡ Noradrenergic<br/><small>Arousal control</small>"]:::modulation
        Cholinergic["🧠 Cholinergic<br/><small>Attention modulation</small>"]:::modulation
        
        Dopaminergic --> ModulatoryIntegration["🔗 Modulatory Integration<br/><small>State regulation</small>"]:::integration
        Serotonergic --> ModulatoryIntegration
        Noradrenergic --> ModulatoryIntegration
        Cholinergic --> ModulatoryIntegration
    end
    
    %% Case-specific processing
    subgraph "Case Processing Centers"
        direction TB
        NominativeCenter["👤 Nominative Center [NOM]<br/><small>Agent/subject processing</small>"]:::case
        AccusativeCenter["🎯 Accusative Center [ACC]<br/><small>Object/patient processing</small>"]:::case
        GenitiveCenter["📦 Genitive Center [GEN]<br/><small>Source/possession processing</small>"]:::case
        DativeCenter["📤 Dative Center [DAT]<br/><small>Recipient/beneficiary processing</small>"]:::case
        InstrumentalCenter["🛠️ Instrumental Center [INS]<br/><small>Means/method processing</small>"]:::case
        LocativeCenter["📍 Locative Center [LOC]<br/><small>Location/context processing</small>"]:::case
        AblativeCenter["🌊 Ablative Center [ABL]<br/><small>Origin/cause processing</small>"]:::case
        
        NominativeCenter --> CaseIntegration["🔗 Case Integration<br/><small>Cross-case coordination</small>"]:::integration
        AccusativeCenter --> CaseIntegration
        GenitiveCenter --> CaseIntegration
        DativeCenter --> CaseIntegration
        InstrumentalCenter --> CaseIntegration
        LocativeCenter --> CaseIntegration
        AblativeCenter --> CaseIntegration
    end
    
    %% Main system connections
    CognitiveSystem --> SensoryProcessing["Sensory Processing [DAT]"]
    CognitiveSystem --> MemorySystems["Memory Systems [ACC]"]
    CognitiveSystem --> ProcessingCenters["Processing Centers [NOM]"]
    CognitiveSystem --> MotorSystems["Motor Systems [GEN]"]
    CognitiveSystem --> ModulatorySystems["Modulatory Systems [INS]"]
    CognitiveSystem --> CaseProcessing["Case Processing Centers"]
    
    %% Internal connections
    SensoryProcessing --> Visual
    MemorySystems --> WorkingMemory
    ProcessingCenters --> PrefrontalCortex
    MotorSystems --> MotorCortex
    ModulatorySystems --> Dopaminergic
    CaseProcessing --> NominativeCenter
    
    %% Cross-system connections
    SensoryIntegration --> WorkingMemory
    MemoryIntegration --> PrefrontalCortex
    ProcessingIntegration --> MotorCortex
    MotorIntegration --> BasalGanglia
    ModulatoryIntegration --> PrefrontalCortex
    CaseIntegration --> ProcessingIntegration
    
    %% Feedback loops
    MotorCortex -.->|"Feedback"| SensoryIntegration
    PrefrontalCortex -.->|"Top-down"| MemoryIntegration
    BasalGanglia -.->|"Action feedback"| ProcessingIntegration
    
    %% Interactive elements
    click CognitiveSystem "docs/cognitive-architecture/" "View cognitive architecture documentation"
    click Visual "src/neural/visual.py" "View visual processing implementation"
    click WorkingMemory "src/memory/working.py" "View working memory implementation"
    click PrefrontalCortex "src/processing/executive.py" "View executive control implementation"
    click MotorCortex "src/motor/planning.py" "View motor planning implementation"
    click NominativeCenter "src/cases/nominative.py" "View nominative case processing"
    click Dopaminergic "src/modulation/dopamine.py" "View dopaminergic modulation"
    
    %% Enhanced styling
    linkStyle default stroke:#7F8C8D,stroke-width:2px
    linkStyle 0 stroke:#4ECDC4,stroke-width:3px
    linkStyle 1 stroke:#74B9FF,stroke-width:2px
    linkStyle 2 stroke:#A29BFE,stroke-width:2px
    linkStyle 3 stroke:#00B894,stroke-width:2px
    linkStyle 4 stroke:#FD79A8,stroke-width:2px
    linkStyle 5 stroke:#FDCB6E,stroke-width:2px
```

## Alternative: Neural Network Architecture

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

graph TD
    %% Neural network components
    classDef input fill:#74B9FF,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef hidden fill:#A29BFE,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef output fill:#00B894,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef attention fill:#FDCB6E,stroke:#2C3E50,stroke-width:2px,color:#2C3E50,font-weight:600
    classDef memory fill:#FD79A8,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    
    %% Input layer [DAT]
    subgraph "Input Layer [DAT]"
        direction LR
        Input1["Input 1<br/><small>Feature 1</small>"]:::input
        Input2["Input 2<br/><small>Feature 2</small>"]:::input
        Input3["Input 3<br/><small>Feature 3</small>"]:::input
    end
    
    %% Hidden layers [ACC]
    subgraph "Hidden Layers [ACC]"
        direction TB
        Hidden1["Hidden Layer 1<br/><small>Feature extraction</small>"]:::hidden
        Hidden2["Hidden Layer 2<br/><small>Pattern recognition</small>"]:::hidden
        Hidden3["Hidden Layer 3<br/><small>Abstract representation</small>"]:::hidden
    end
    
    %% Attention mechanism [INS]
    Attention["Attention Mechanism<br/><small>Focus selection</small>"]:::attention
    
    %% Memory components [GEN]
    Memory["Memory Module<br/><small>State persistence</small>"]:::memory
    
    %% Output layer [NOM]
    subgraph "Output Layer [NOM]"
        direction LR
        Output1["Output 1<br/><small>Action 1</small>"]:::output
        Output2["Output 2<br/><small>Action 2</small>"]:::output
        Output3["Output 3<br/><small>Action 3</small>"]:::output
    end
    
    %% Connections
    Input1 --> Hidden1
    Input2 --> Hidden1
    Input3 --> Hidden1
    
    Hidden1 --> Hidden2
    Hidden2 --> Hidden3
    
    Hidden3 --> Attention
    Attention --> Memory
    Memory --> Output1
    Memory --> Output2
    Memory --> Output3
    
    %% Feedback connections
    Output1 -.->|"Feedback"| Hidden3
    Output2 -.->|"Feedback"| Hidden3
    Output3 -.->|"Feedback"| Hidden3
```

## Customization Options

### 1. Component Types
- **Sensory**: Input processing components (`:::sensory`)
- **Memory**: Storage and retrieval components (`:::memory`)
- **Processing**: Computational components (`:::processing`)
- **Motor**: Output and action components (`:::motor`)
- **Integration**: Coordination components (`:::integration`)
- **Modulation**: Regulatory components (`:::modulation`)
- **Case**: Case-specific processing (`:::case`)

### 2. Case Mappings
Map cognitive functions to CEREBRUM cases:
- `[NOM]` - Nominative: Agent/subject processing (executive control)
- `[ACC]` - Accusative: Object/patient processing (memory systems)
- `[GEN]` - Genitive: Source/possession (motor systems)
- `[DAT]` - Dative: Recipient/beneficiary (sensory systems)
- `[INS]` - Instrumental: Means/method (modulatory systems)
- `[LOC]` - Locative: Location/context (spatial processing)
- `[ABL]` - Ablative: Origin/cause (causal processing)

### 3. Neural Network Variants
- **Feedforward Networks**: Linear processing chains
- **Recurrent Networks**: Feedback loops and state
- **Attention Networks**: Focus mechanisms
- **Memory Networks**: Persistent state
- **Transformer Networks**: Self-attention mechanisms

### 4. Interactive Features
Add clickable elements for:
- Neural implementation details
- Cognitive model documentation
- Case processing algorithms
- Performance metrics
- Related research papers

## Best Practices

1. **Hierarchy**: Establish clear processing hierarchy
2. **Modularity**: Group related functions in subgraphs
3. **Case Mapping**: Clearly label case-specific functions
4. **Feedback**: Show feedback and recurrent connections
5. **Modulation**: Include regulatory and modulatory systems
6. **Documentation**: Include brief descriptions in component labels
7. **Consistency**: Use consistent styling and naming conventions
8. **Accessibility**: Ensure high contrast and clear labels

## Common Patterns

### 1. Sensory-Motor Loop
```
Sensory Input → Processing → Decision → Motor Output → Feedback
```

### 2. Memory Consolidation
```
Working Memory → Processing → Long-term Memory → Retrieval
```

### 3. Attention Mechanism
```
Input → Attention Selection → Focused Processing → Output
```

### 4. Case Processing
```
Input → Case Assignment → Case-specific Processing → Integration
``` 