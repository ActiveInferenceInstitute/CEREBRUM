# Architecture Diagram Template

## Usage
This template is designed for system and component architecture diagrams with modern styling and interactive features.

## Template Code

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

graph TD
    %% Style definitions
    classDef framework fill:#4ECDC4,stroke:#2C3E50,stroke-width:3px,color:#FFFFFF,font-weight:bold
    classDef engine fill:#45B7D1,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef library fill:#96CEB4,stroke:#2C3E50,stroke-width:2px,color:#2C3E50,font-weight:500
    classDef transformation fill:#FFE66D,stroke:#2C3E50,stroke-width:2px,color:#2C3E50,font-weight:500
    classDef visualization fill:#FF6B6B,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    classDef integration fill:#A29BFE,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    classDef data fill:#FD79A8,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    classDef process fill:#00B894,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    
    %% Main system framework
    System["🏛️ System Name<br/><small>High-level description</small>"]:::framework
    
    %% Core engine components
    subgraph "Core Engine"
        direction TB
        Engine1["Engine Component 1<br/><small>[NOM] - Primary function</small>"]:::engine
        Engine2["Engine Component 2<br/><small>[ACC] - Secondary function</small>"]:::engine
        Engine3["Engine Component 3<br/><small>[GEN] - Supporting function</small>"]:::engine
        
        Engine1 --> Engine2
        Engine2 --> Engine3
    end
    
    %% Model library components
    subgraph "Model Library"
        direction LR
        Model1["Predefined Model 1"]:::library
        Model2["Custom Template 1"]:::library
        Model3["Model Instance 1"]:::library
        
        Model1 --> Model3
        Model2 --> Model3
    end
    
    %% Transformation engine
    subgraph "Transformation Engine"
        direction TB
        Transform1["Model Adaptation<br/><small>Adaptive processes</small>"]:::transformation
        Transform2["Case Transformations<br/><small>Case system mappings</small>"]:::transformation
        Transform3["Learning Mechanisms<br/><small>Update processes</small>"]:::transformation
        
        Transform1 --> Transform2
        Transform2 --> Transform3
    end
    
    %% Visualization tools
    subgraph "Visualization & Interaction"
        direction LR
        Viz1["Interactive Visualizations<br/><small>User interface</small>"]:::visualization
        Viz2["Model Inspection<br/><small>Debugging tools</small>"]:::visualization
        Viz3["Diagnostic Reports<br/><small>Analytics output</small>"]:::visualization
        
        Viz1 --> Viz2
        Viz2 --> Viz3
    end
    
    %% Integration points
    subgraph "Integration Layer"
        direction TB
        API["REST API<br/><small>External interface</small>"]:::integration
        SDK["Language SDKs<br/><small>Multi-language support</small>"]:::integration
        CLI["Command Line Tools<br/><small>Automation interface</small>"]:::integration
        
        API --> SDK
        SDK --> CLI
    end
    
    %% Data stores
    subgraph "Data Layer"
        direction LR
        Data1[("Primary Database<br/><small>Main data store</small>")]:::data
        Data2[("Cache Layer<br/><small>Performance optimization</small>")]:::data
        Data3[("File Storage<br/><small>Persistent storage</small>")]:::data
        
        Data1 --> Data2
        Data2 --> Data3
    end
    
    %% Main system connections
    System --> CoreEngine["Core Engine"]
    System --> ModelLib["Model Library"]
    System --> TransEng["Transformation Engine"]
    System --> VizTools["Visualization Tools"]
    
    %% Internal connections
    CoreEngine --> Engine1
    ModelLib --> Model1
    TransEng --> Transform1
    VizTools --> Viz1
    
    %% Data connections
    Engine1 --> Data1
    Model1 --> Data2
    Transform1 --> Data3
    Viz1 --> Data1
    
    %% Integration connections
    Engine1 --> API
    Model1 --> SDK
    Transform1 --> CLI
    Viz1 --> API
    
    %% Interactive elements
    click System "https://github.com/ActiveInferenceInstitute/CEREBRUM" "View project repository"
    click Engine1 "src/core/engine.py" "View engine source code"
    click Model1 "src/models/" "View model implementations"
    click Transform1 "src/transformations/" "View transformation logic"
    click Viz1 "src/visualization/" "View visualization tools"
    click API "docs/api/" "View API documentation"
    click Data1 "docs/data-model/" "View data model documentation"
    
    %% Enhanced styling
    linkStyle default stroke:#7F8C8D,stroke-width:2px
    linkStyle 0 stroke:#4ECDC4,stroke-width:3px
    linkStyle 1 stroke:#45B7D1,stroke-width:3px
    linkStyle 2 stroke:#96CEB4,stroke-width:3px
    linkStyle 3 stroke:#FFE66D,stroke-width:3px
```

## Customization Options

### 1. Component Types
- **Framework**: Main system components (`:::framework`)
- **Engine**: Core processing components (`:::engine`)
- **Library**: Model and data components (`:::library`)
- **Transformation**: Processing and adaptation components (`:::transformation`)
- **Visualization**: UI and display components (`:::visualization`)
- **Integration**: API and interface components (`:::integration`)
- **Data**: Storage and data components (`:::data`)

### 2. Case Annotations
Include relevant CEREBRUM case annotations:
- `[NOM]` - Nominative (subject/agent)
- `[ACC]` - Accusative (object/patient)
- `[GEN]` - Genitive (source/possession)
- `[DAT]` - Dative (recipient/beneficiary)
- `[INS]` - Instrumental (means/method)
- `[LOC]` - Locative (location/context)
- `[ABL]` - Ablative (origin/cause)

### 3. Interactive Features
Add clickable elements for:
- Source code links
- Documentation references
- External resources
- Related diagrams

### 4. Subgraph Organization
Use subgraphs to group related components:
- Logical separation of concerns
- Clear visual boundaries
- Improved readability
- Better layout control

## Best Practices

1. **Naming**: Use descriptive, action-oriented names
2. **Hierarchy**: Establish clear visual hierarchy with styling
3. **Connections**: Show meaningful relationships between components
4. **Documentation**: Include brief descriptions in component labels
5. **Interactivity**: Add relevant links for navigation
6. **Consistency**: Use consistent styling across similar components
7. **Accessibility**: Ensure high contrast and clear labels 