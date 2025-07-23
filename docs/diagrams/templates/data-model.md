# Data Model Template

## Usage
This template is designed for entity-relationship diagrams, data flow visualizations, and knowledge graph representations with CEREBRUM case annotations and modern styling.

## Template Code

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

erDiagram
    %% Style definitions
    classDef entity fill:#4ECDC4,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:bold
    classDef relationship fill:#45B7D1,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef attribute fill:#96CEB4,stroke:#2C3E50,stroke-width:1px,color:#2C3E50,font-weight:500
    classDef data fill:#FD79A8,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    classDef process fill:#00B894,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    
    %% Main entities
    CASE_SYSTEM {
        string case_id PK "Unique identifier"
        string case_type "NOM|ACC|GEN|DAT|INS|LOC|ABL"
        string case_name "Descriptive name"
        text case_description "Detailed description"
        json case_attributes "Flexible attributes"
        timestamp created_at "Creation timestamp"
        timestamp updated_at "Last update"
    }:::entity
    
    BAYESIAN_MODEL {
        string model_id PK "Model identifier"
        string model_type "Model classification"
        json model_parameters "Model configuration"
        json prior_distributions "Prior beliefs"
        json likelihood_functions "Likelihood models"
        timestamp trained_at "Training timestamp"
        float model_performance "Performance metrics"
    }:::entity
    
    TRANSFORMATION_RULE {
        string rule_id PK "Rule identifier"
        string source_case "Source case type"
        string target_case "Target case type"
        json transformation_logic "Transformation algorithm"
        float confidence_score "Rule confidence"
        boolean is_active "Active status"
        timestamp created_at "Creation time"
    }:::entity
    
    DATA_SOURCE {
        string source_id PK "Source identifier"
        string source_type "Data source type"
        string source_name "Source name"
        json source_config "Configuration"
        string data_format "Input format"
        boolean is_active "Active status"
        timestamp last_updated "Last update"
    }:::entity
    
    PROCESSING_PIPELINE {
        string pipeline_id PK "Pipeline identifier"
        string pipeline_name "Pipeline name"
        json pipeline_steps "Processing steps"
        string status "Current status"
        timestamp started_at "Start time"
        timestamp completed_at "Completion time"
        json performance_metrics "Pipeline metrics"
    }:::entity
    
    %% Relationships
    CASE_SYSTEM ||--o{ TRANSFORMATION_RULE : "defines"
    CASE_SYSTEM ||--o{ BAYESIAN_MODEL : "informs"
    BAYESIAN_MODEL ||--o{ TRANSFORMATION_RULE : "supports"
    DATA_SOURCE ||--o{ PROCESSING_PIPELINE : "feeds"
    PROCESSING_PIPELINE ||--o{ CASE_SYSTEM : "generates"
    TRANSFORMATION_RULE ||--o{ PROCESSING_PIPELINE : "applies"
```

## Alternative: Data Flow Diagram

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

flowchart TD
    %% Style definitions
    classDef data fill:#FD79A8,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    classDef process fill:#00B894,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    classDef storage fill:#A29BFE,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    classDef output fill:#FF6B6B,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    
    %% Data sources [DAT]
    subgraph "Data Sources [DAT]"
        direction LR
        RawData["📄 Raw Data<br/><small>Input data sources</small>"]:::data
        ExternalAPI["🌐 External APIs<br/><small>Third-party data</small>"]:::data
        UserInput["👤 User Input<br/><small>Manual data entry</small>"]:::data
        SensorData["📡 Sensor Data<br/><small>Real-time feeds</small>"]:::data
    end
    
    %% Data processing [ACC]
    subgraph "Data Processing [ACC]"
        direction TB
        Validation["✅ Data Validation<br/><small>Quality checks</small>"]:::process
        Transformation["🔄 Data Transformation<br/><small>Format conversion</small>"]:::process
        Enrichment["📊 Data Enrichment<br/><small>Feature addition</small>"]:::process
        Normalization["⚖️ Data Normalization<br/><small>Standardization</small>"]:::process
    end
    
    %% Data storage [GEN]
    subgraph "Data Storage [GEN]"
        direction LR
        Cache["⚡ Cache Layer<br/><small>Temporary storage</small>"]:::storage
        Database["🗄️ Primary Database<br/><small>Persistent storage</small>"]:::storage
        FileSystem["📁 File System<br/><small>Document storage</small>"]:::storage
        Backup["💾 Backup Storage<br/><small>Data protection</small>"]:::storage
    end
    
    %% Data analysis [NOM]
    subgraph "Data Analysis [NOM]"
        direction TB
        PatternRecognition["🔍 Pattern Recognition<br/><small>Feature extraction</small>"]:::process
        StatisticalAnalysis["📈 Statistical Analysis<br/><small>Quantitative analysis</small>"]:::process
        MachineLearning["🤖 Machine Learning<br/><small>Predictive modeling</small>"]:::process
        CaseAnalysis["🗂️ Case Analysis<br/><small>Case-based reasoning</small>"]:::process
    end
    
    %% Data output [INS]
    subgraph "Data Output [INS]"
        direction LR
        Reports["📋 Reports<br/><small>Analytical reports</small>"]:::output
        Visualizations["📊 Visualizations<br/><small>Data visualizations</small>"]:::output
        APIs["🔌 APIs<br/><small>Data interfaces</small>"]:::output
        Alerts["🚨 Alerts<br/><small>Notification system</small>"]:::output
    end
    
    %% Data flow connections
    RawData --> Validation
    ExternalAPI --> Validation
    UserInput --> Validation
    SensorData --> Validation
    
    Validation --> Transformation
    Transformation --> Enrichment
    Enrichment --> Normalization
    
    Normalization --> Cache
    Cache --> Database
    Database --> FileSystem
    FileSystem --> Backup
    
    Database --> PatternRecognition
    PatternRecognition --> StatisticalAnalysis
    StatisticalAnalysis --> MachineLearning
    MachineLearning --> CaseAnalysis
    
    CaseAnalysis --> Reports
    CaseAnalysis --> Visualizations
    CaseAnalysis --> APIs
    CaseAnalysis --> Alerts
    
    %% Interactive elements
    click RawData "docs/data-sources/" "View data source documentation"
    click Validation "src/data/validation.py" "View validation logic"
    click Database "docs/database/" "View database schema"
    click CaseAnalysis "src/cases/analysis.py" "View case analysis"
    click Reports "docs/reports/" "View report templates"
```

## Alternative: Knowledge Graph Template

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

graph TD
    %% Style definitions
    classDef concept fill:#4ECDC4,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:bold
    classDef relation fill:#45B7D1,stroke:#2C3E50,stroke-width:1px,color:#FFFFFF,font-weight:600
    classDef instance fill:#96CEB4,stroke:#2C3E50,stroke-width:1px,color:#2C3E50,font-weight:500
    classDef property fill:#FD79A8,stroke:#2C3E50,stroke-width:1px,color:#FFFFFF,font-weight:500
    
    %% Core concepts
    CognitiveSystem["🧠 Cognitive System"]:::concept
    BayesianModel["📊 Bayesian Model"]:::concept
    CaseSystem["🗂️ Case System"]:::concept
    ActiveInference["🎯 Active Inference"]:::concept
    
    %% Case types
    NominativeCase["👤 Nominative [NOM]"]:::instance
    AccusativeCase["🎯 Accusative [ACC]"]:::instance
    GenitiveCase["📦 Genitive [GEN]"]:::instance
    DativeCase["📤 Dative [DAT]"]:::instance
    InstrumentalCase["🛠️ Instrumental [INS]"]:::instance
    LocativeCase["📍 Locative [LOC]"]:::instance
    AblativeCase["🌊 Ablative [ABL]"]:::instance
    
    %% Properties and attributes
    Probability["📈 Probability"]:::property
    Uncertainty["❓ Uncertainty"]:::property
    Evidence["🔍 Evidence"]:::property
    Belief["💭 Belief"]:::property
    Action["⚡ Action"]:::property
    Observation["👁️ Observation"]:::property
    
    %% Relationships
    CognitiveSystem -->|"implements"| BayesianModel
    CognitiveSystem -->|"uses"| CaseSystem
    BayesianModel -->|"informs"| ActiveInference
    CaseSystem -->|"structures"| ActiveInference
    
    %% Case relationships
    CaseSystem --> NominativeCase
    CaseSystem --> AccusativeCase
    CaseSystem --> GenitiveCase
    CaseSystem --> DativeCase
    CaseSystem --> InstrumentalCase
    CaseSystem --> LocativeCase
    CaseSystem --> AblativeCase
    
    %% Property relationships
    BayesianModel --> Probability
    BayesianModel --> Uncertainty
    ActiveInference --> Evidence
    ActiveInference --> Belief
    ActiveInference --> Action
    ActiveInference --> Observation
    
    %% Interactive elements
    click CognitiveSystem "docs/cognitive-architecture/" "View cognitive architecture"
    click BayesianModel "src/models/bayesian.py" "View Bayesian implementation"
    click CaseSystem "src/cases/" "View case system"
    click ActiveInference "src/core/active_inference.py" "View active inference"
```

## Customization Options

### 1. Entity Types
- **Entity**: Main data objects (`:::entity`)
- **Relationship**: Connections between entities (`:::relationship`)
- **Attribute**: Properties of entities (`:::attribute`)
- **Data**: Data sources and stores (`:::data`)
- **Process**: Data processing operations (`:::process`)

### 2. Case Annotations
Include relevant CEREBRUM case annotations:
- `[NOM]` - Nominative (subject/agent data)
- `[ACC]` - Accusative (object/patient data)
- `[GEN]` - Genitive (source/possession data)
- `[DAT]` - Dative (recipient/beneficiary data)
- `[INS]` - Instrumental (means/method data)
- `[LOC]` - Locative (location/context data)
- `[ABL]` - Ablative (origin/cause data)

### 3. Data Flow Patterns
- **Linear Flow**: Sequential data processing
- **Parallel Flow**: Concurrent data processing
- **Feedback Loop**: Iterative data refinement
- **Branching Flow**: Conditional data routing
- **Aggregation Flow**: Data combination and synthesis

### 4. Interactive Features
Add clickable elements for:
- Database schema documentation
- Data processing algorithms
- API specifications
- Data validation rules
- Related data models

## Best Practices

1. **Normalization**: Follow database normalization principles
2. **Naming**: Use clear, descriptive entity and attribute names
3. **Relationships**: Clearly define relationship cardinalities
4. **Documentation**: Include brief descriptions in entity labels
5. **Consistency**: Use consistent naming and styling conventions
6. **Accessibility**: Ensure high contrast and clear labels
7. **Performance**: Consider data access patterns and indexing
8. **Scalability**: Design for future growth and changes

## Common Patterns

### 1. Master-Detail Relationship
```
Master Entity ||--o{ Detail Entity : "contains"
```

### 2. Many-to-Many Relationship
```
Entity A }o--o{ Entity B : "associates"
```

### 3. Inheritance Hierarchy
```
Base Entity ||--o{ Specialized Entity : "extends"
```

### 4. Audit Trail
```
Main Entity ||--o{ Audit Record : "tracks"
``` 