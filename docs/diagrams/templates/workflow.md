# Workflow Diagram Template

## Usage
This template is designed for process and workflow diagrams with modern styling, decision points, and interactive features.

## Template Code

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

flowchart TD
    %% Style definitions
    classDef start fill:#00B894,stroke:#2C3E50,stroke-width:3px,color:#FFFFFF,font-weight:bold
    classDef process fill:#45B7D1,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef decision fill:#FDCB6E,stroke:#2C3E50,stroke-width:2px,color:#2C3E50,font-weight:600
    classDef data fill:#FD79A8,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    classDef end fill:#FF6B6B,stroke:#2C3E50,stroke-width:3px,color:#FFFFFF,font-weight:bold
    classDef error fill:#E74C3C,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    classDef success fill:#27AE60,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    
    %% Workflow start
    Start([🏁 Start Process<br/><small>Initial state or trigger</small>]):::start
    
    %% Initial processing
    Process1["📥 Input Processing<br/><small>[DAT] - Data ingestion and validation</small>"]:::process
    Process2["🔍 Analysis Phase<br/><small>[ACC] - Pattern recognition and analysis</small>"]:::process
    
    %% Decision points
    Decision1{"❓ Quality Check<br/><small>Data validation decision</small>"}:::decision
    Decision2{"🔄 Retry Logic<br/><small>Error handling decision</small>"}:::decision
    Decision3{"📊 Output Selection<br/><small>Result routing decision</small>"}:::decision
    
    %% Processing steps
    Process3["⚙️ Core Processing<br/><small>[NOM] - Main transformation logic</small>"]:::process
    Process4["🛠️ Model Adaptation<br/><small>[GEN] - Model updates and learning</small>"]:::process
    Process5["📈 Result Generation<br/><small>[INS] - Output creation</small>"]:::process
    
    %% Data stores
    Data1[("💾 Input Cache<br/><small>Temporary data storage</small>")]:::data
    Data2[("🗄️ Model Store<br/><small>Persistent model data</small>")]:::data
    Data3[("📁 Output Storage<br/><small>Result persistence</small>")]:::data
    
    %% Error handling
    Error1["⚠️ Error Handler<br/><small>Exception processing</small>"]:::error
    Retry["🔄 Retry Process<br/><small>Recovery mechanism</small>"]:::process
    
    %% Success paths
    Success1["✅ Success Path<br/><small>Normal completion</small>"]:::success
    Success2["🎯 Final Output<br/><small>Process completion</small>"]:::success
    
    %% End states
    End1([🏁 Process Complete<br/><small>Successful completion</small>]):::end
    End2([❌ Process Failed<br/><small>Error termination</small>]):::end
    
    %% Main workflow
    Start --> Process1
    Process1 --> Data1
    Data1 --> Process2
    Process2 --> Decision1
    
    %% Decision paths
    Decision1 -->|Valid| Process3
    Decision1 -->|Invalid| Error1
    
    %% Core processing
    Process3 --> Data2
    Data2 --> Process4
    Process4 --> Decision2
    
    %% Error handling
    Decision2 -->|Retry| Retry
    Decision2 -->|Fail| End2
    Retry --> Process3
    
    %% Output generation
    Process4 --> Process5
    Process5 --> Data3
    Data3 --> Decision3
    
    %% Output routing
    Decision3 -->|Success| Success1
    Decision3 -->|Error| Error1
    
    %% Final paths
    Success1 --> Success2
    Success2 --> End1
    Error1 --> End2
    
    %% Interactive elements
    click Start "docs/workflows/start-process.md" "View start process documentation"
    click Process1 "src/processing/input.py" "View input processing code"
    click Process3 "src/core/engine.py" "View core processing logic"
    click Data2 "src/models/" "View model implementations"
    click Error1 "docs/troubleshooting/error-handling.md" "View error handling guide"
    click Success2 "docs/outputs/" "View output documentation"
    
    %% Enhanced styling
    linkStyle default stroke:#7F8C8D,stroke-width:2px
    linkStyle 0 stroke:#00B894,stroke-width:3px
    linkStyle 1 stroke:#45B7D1,stroke-width:2px
    linkStyle 2 stroke:#FDCB6E,stroke-width:2px
    linkStyle 3 stroke:#FF6B6B,stroke-width:2px
    linkStyle 4 stroke:#27AE60,stroke-width:2px
```

## Alternative: State Diagram Template

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

stateDiagram-v2
    [*] --> Idle
    
    state Idle {
        [*] --> Ready
        Ready --> Processing: Start Process
        Processing --> Ready: Complete
        Processing --> Error: Fail
        Error --> Ready: Retry
    }
    
    Idle --> Active: Activate System
    Idle --> [*]: Shutdown
    
    state Active {
        [*] --> Running
        Running --> Paused: Pause
        Paused --> Running: Resume
        Running --> Error: Fail
        Error --> Running: Retry
        Running --> Complete: Success
    }
    
    Active --> Idle: Deactivate
    Active --> [*]: Shutdown
    
    Complete --> [*]: End Process
    Error --> [*]: Error Exit
```

## Customization Options

### 1. Process Types
- **Start/End**: Process boundaries (`:::start`, `:::end`)
- **Process**: Main processing steps (`:::process`)
- **Decision**: Conditional logic (`:::decision`)
- **Data**: Data storage and retrieval (`:::data`)
- **Error**: Error handling (`:::error`)
- **Success**: Successful outcomes (`:::success`)

### 2. Case Annotations
Include relevant CEREBRUM case annotations:
- `[NOM]` - Nominative (subject/agent performing action)
- `[ACC]` - Accusative (object/patient being processed)
- `[GEN]` - Genitive (source/possession of data)
- `[DAT]` - Dative (recipient/beneficiary of output)
- `[INS]` - Instrumental (means/method of processing)
- `[LOC]` - Locative (location/context of operation)
- `[ABL]` - Ablative (origin/cause of process)

### 3. Flow Control
- **Linear Flow**: Sequential processing steps
- **Conditional Flow**: Decision-based branching
- **Parallel Flow**: Concurrent processing paths
- **Loop Flow**: Iterative processing cycles
- **Error Flow**: Exception handling paths

### 4. Interactive Features
Add clickable elements for:
- Process documentation
- Source code links
- Error handling guides
- Output specifications
- Related workflows

## Best Practices

1. **Clarity**: Use clear, action-oriented process names
2. **Flow Direction**: Maintain consistent flow direction (top-to-bottom or left-to-right)
3. **Decision Points**: Clearly label decision conditions
4. **Error Handling**: Include comprehensive error paths
5. **Data Flow**: Show data movement between processes
6. **Documentation**: Include brief descriptions in process labels
7. **Consistency**: Use consistent styling and naming conventions
8. **Accessibility**: Ensure high contrast and clear labels

## Common Patterns

### 1. Input-Process-Output (IPO)
```
Input → Validation → Processing → Output → Storage
```

### 2. Error Handling
```
Process → Decision → Success Path
         ↓
      Error Path → Recovery → Retry
```

### 3. Parallel Processing
```
Input → Split → Process A → Merge → Output
         ↓
      Process B ↗
```

### 4. Iterative Processing
```
Start → Process → Decision → Continue
                ↓
             Complete
``` 