# Figure 3: Cognitive Model Case Framework

```mermaid
graph TD
    %% Core Model
    Core["Cognitive ModelBase Entity<br>(Core Generative Model)<br><small>Centralized intelligence with adaptive interfaces</small>"]
    
    %% Primary Cases (Top)
    NOM["Nominative [NOM]<br>Subject/Agent<br><i>Predictive model; Primary analytical model; Active controller</i><br><small>Drives processing autonomously</small>"]
    ACC["Accusative [ACC]<br>Direct Object<br><i>Model being evaluated; Target of transformation; Parameter updates</i><br><small>Receives modifications</small>"]
    DAT["Dative [DAT]<br>Recipient/Goal<br><i>Target model; Receives inputs; Destination system</i><br><small>Endpoint for information flows</small>"]
    
    %% Contextual Cases (Left)
    LOC["Locative [LOC]<br>Context/Environment<br><i>Parameter context; Execution environment; Model location</i><br><small>Situational boundaries</small>"]
    INS["Instrumental [INS]<br>Tool/Method<br><i>Analysis technique; Transformation method; Processing tool</i><br><small>Implementation mechanism</small>"]
    VOC["Vocative [VOC]<br>Addressable Entity<br><i>Direct invocation; Named reference; Command interface</i><br><small>Activation point</small>"]
    
    %% Source Cases (Right)
    GEN["Genitive [GEN]<br>Source/Possessor<br><i>Output generator; Intelligence source; Product origin</i><br><small>Creates derived artifacts</small>"]
    ABL["Ablative [ABL]<br>Origin/Cause<br><i>Data source; Initial state; Historical reference</i><br><small>Provides causal history</small>"]
    
    %% Connections with descriptions
    Core -->|"Transforms to"| NOM
    Core -->|"Transforms to"| ACC
    Core -->|"Transforms to"| DAT
    Core -->|"Transforms to"| LOC
    Core -->|"Transforms to"| INS
    Core -->|"Transforms to"| VOC
    Core -->|"Transforms to"| GEN
    Core -->|"Transforms to"| ABL
    
    %% Layout hints with functional relationships
    NOM -->|"Affects"| ACC
    ACC -->|"Directs to"| DAT
    LOC -->|"Defines"| INS
    INS -->|"Activates"| VOC
    GEN -->|"References"| ABL
    
    %% Styling with improved accessibility
    classDef core fill:#e6e6ff,stroke:#6666ff,stroke-width:3px,color:#000066,font-weight:bold
    classDef primary fill:#e6f3ff,stroke:#4682b4,stroke-width:2px,color:#00008B
    classDef contextual fill:#f0fff0,stroke:#99cc99,stroke-width:2px,color:#006600
    classDef source fill:#ffe6e6,stroke:#ff9999,stroke-width:2px,color:#8B0000
    
    class Core core
    class NOM,ACC,DAT primary
    class LOC,INS,VOC contextual
    class GEN,ABL source
    
    %% Subgraph styling with clearer titles
    subgraph Primary["Primary Cases: Agent-Object-Recipient Relationships"]
        NOM
        ACC
        DAT
    end
    
    subgraph Contextual["Contextual Cases: Environmental & Methodological"]
        LOC
        INS
        VOC
    end
    
    subgraph Source["Source/Origin Cases: Production & History"]
        GEN
        ABL
    end
    
    classDef group fill:none,stroke:#999,stroke-dasharray: 5 5,color:#333333,font-weight:bold
    class Primary,Contextual,Source group
```

Figure 3. Cognitive Model Case Framework. This diagram illustrates how a single core generative model can assume different functional roles through case assignments. At the center lies the core cognitive model entity, which can be transformed into eight distinct case forms, each serving a specific function in the model ecosystem. The cases are organized into three functional groups: Primary Cases (Nominative, Accusative, Dative) handle the main agent-object-recipient relationships in model processing; Contextual Cases (Locative, Instrumental, Vocative) provide environmental, methodological, and interface functions; and Source Cases (Genitive, Ablative) manage output generation and historical attribution. Each case modifies the model's behavior, parameter access patterns, and computational interfaces while maintaining its fundamental identity. For example, when in Nominative case, the model functions as an active agent generating predictions; in Accusative case, it becomes the object of transformations; and in Genitive case, it serves as a source of outputs. This framework enables flexible, context-appropriate model behavior while preserving a coherent identity across transformations.