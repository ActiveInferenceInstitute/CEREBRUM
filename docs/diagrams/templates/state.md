# State Diagram Template

## Usage
This template is designed for state diagrams, model lifecycle visualizations, and system state transitions with CEREBRUM case annotations and modern styling.

## Template Code

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

stateDiagram-v2
    [*] --> Initialized
    
    state Initialized {
        [*] --> Ready
        Ready --> Configuring: Configure Model [DAT]
        Configuring --> Ready: Configuration Complete
        Configuring --> Error: Configuration Failed
        Error --> Ready: Retry Configuration
    }
    
    Initialized --> Active: Activate System [ACC]
    Initialized --> [*]: Shutdown
    
    state Active {
        [*] --> Running
        Running --> Processing: Start Processing [NOM]
        Processing --> Learning: Update Beliefs [GEN]
        Learning --> Adapting: Apply Transformations [INS]
        Adapting --> Running: Adaptation Complete
        Processing --> Error: Processing Failed
        Learning --> Error: Learning Failed
        Adapting --> Error: Adaptation Failed
        Error --> Running: Retry Operation
        Running --> Paused: Pause System
        Paused --> Running: Resume System
        Running --> Complete: Processing Complete
    }
    
    Active --> Inactive: Deactivate System
    Active --> [*]: Shutdown
    
    state Inactive {
        [*] --> Idle
        Idle --> Maintenance: Perform Maintenance [LOC]
        Maintenance --> Idle: Maintenance Complete
        Maintenance --> Error: Maintenance Failed
        Error --> Idle: Retry Maintenance
    }
    
    Inactive --> Active: Reactivate System
    Inactive --> [*]: Shutdown
    
    Complete --> [*]: End Process
    Error --> [*]: Error Exit
```

## Alternative: Model Lifecycle State Diagram

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

stateDiagram-v2
    [*] --> ModelCreated
    
    state ModelCreated {
        [*] --> Draft
        Draft --> Validated: Validate Configuration [DAT]
        Validated --> Draft: Validation Failed
        Validated --> Ready: Validation Passed
    }
    
    ModelCreated --> ModelTraining: Start Training [ACC]
    ModelCreated --> [*]: Delete Model
    
    state ModelTraining {
        [*] --> Initializing
        Initializing --> Training: Begin Training [NOM]
        Training --> Evaluating: Training Complete
        Evaluating --> Training: Evaluation Failed
        Evaluating --> Validated: Evaluation Passed
        Training --> Failed: Training Failed
        Failed --> Training: Retry Training
    }
    
    ModelTraining --> ModelDeployed: Deploy Model [GEN]
    ModelTraining --> ModelArchived: Archive Model
    
    state ModelDeployed {
        [*] --> Active
        Active --> Monitoring: Monitor Performance [INS]
        Monitoring --> Active: Performance OK
        Monitoring --> Degraded: Performance Degraded
        Degraded --> Retraining: Retrain Model
        Degraded --> Active: Performance Recovered
        Retraining --> Active: Retraining Complete
        Retraining --> Degraded: Retraining Failed
    }
    
    ModelDeployed --> ModelRetired: Retire Model [LOC]
    ModelDeployed --> ModelArchived: Archive Model
    
    state ModelRetired {
        [*] --> Inactive
        Inactive --> Archived: Archive Model
        Inactive --> Reactivated: Reactivate Model
        Reactivated --> ModelDeployed: Redeploy Model
    }
    
    ModelRetired --> ModelArchived: Archive Model
    ModelArchived --> [*]: Permanent Deletion
```

## Alternative: System State Machine

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

stateDiagram-v2
    [*] --> SystemOff
    
    SystemOff --> SystemBooting: Power On [DAT]
    SystemOff --> [*]: Permanent Shutdown
    
    state SystemBooting {
        [*] --> HardwareCheck
        HardwareCheck --> SoftwareLoad: Hardware OK [ACC]
        HardwareCheck --> SystemOff: Hardware Failed
        SoftwareLoad --> ServiceStart: Software Loaded [NOM]
        ServiceStart --> SystemReady: Services Started
        ServiceStart --> SystemOff: Service Failed
    }
    
    SystemBooting --> SystemReady: Boot Complete [GEN]
    SystemBooting --> SystemOff: Boot Failed
    
    state SystemReady {
        [*] --> Idle
        Idle --> Processing: Start Processing [INS]
        Processing --> Idle: Processing Complete
        Processing --> Error: Processing Failed
        Error --> Idle: Error Recovered
        Error --> Maintenance: Error Unrecoverable
        Maintenance --> Idle: Maintenance Complete
    }
    
    SystemReady --> SystemMaintenance: Enter Maintenance [LOC]
    SystemReady --> SystemOff: Shutdown
    
    state SystemMaintenance {
        [*] --> Diagnosing
        Diagnosing --> Repairing: Issue Identified
        Diagnosing --> SystemOff: Issue Unfixable
        Repairing --> Testing: Repair Complete
        Testing --> Diagnosing: Test Failed
        Testing --> SystemReady: Test Passed
    }
    
    SystemMaintenance --> SystemReady: Maintenance Complete
    SystemMaintenance --> SystemOff: Maintenance Failed
```

## Alternative: Process State Diagram

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

stateDiagram-v2
    [*] --> ProcessCreated
    
    state ProcessCreated {
        [*] --> Queued
        Queued --> Scheduled: Schedule Process [DAT]
        Scheduled --> Queued: Reschedule
        Scheduled --> Ready: Ready to Execute
    }
    
    ProcessCreated --> ProcessRunning: Start Process [ACC]
    ProcessCreated --> ProcessCancelled: Cancel Process
    
    state ProcessRunning {
        [*] --> Executing
        Executing --> Waiting: Wait for Resource [NOM]
        Waiting --> Executing: Resource Available
        Executing --> Blocked: Block on I/O
        Blocked --> Executing: I/O Complete
        Executing --> Suspended: Suspend Process
        Suspended --> Executing: Resume Process
        Executing --> Terminating: Terminate Process
    }
    
    ProcessRunning --> ProcessCompleted: Process Complete [GEN]
    ProcessRunning --> ProcessFailed: Process Failed
    ProcessRunning --> ProcessCancelled: Cancel Process
    
    state ProcessCompleted {
        [*] --> Success
        Success --> ProcessArchived: Archive Results [INS]
        Success --> ProcessRestarted: Restart Process
    }
    
    state ProcessFailed {
        [*] --> Error
        Error --> ProcessRetry: Retry Process
        Error --> ProcessArchived: Archive Error
        ProcessRetry --> ProcessRunning: Retry Started
    }
    
    ProcessCompleted --> ProcessArchived: Archive Process [LOC]
    ProcessFailed --> ProcessArchived: Archive Process
    ProcessCancelled --> ProcessArchived: Archive Process
    ProcessArchived --> [*]: Cleanup
```

## Alternative: Data Flow State Diagram

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

stateDiagram-v2
    [*] --> DataReceived
    
    state DataReceived {
        [*] --> Raw
        Raw --> Validated: Validate Data [DAT]
        Validated --> Raw: Validation Failed
        Validated --> Clean: Validation Passed
    }
    
    DataReceived --> DataProcessing: Process Data [ACC]
    DataReceived --> DataRejected: Reject Data
    
    state DataProcessing {
        [*] --> Transforming
        Transforming --> Enriching: Transform Complete [NOM]
        Enriching --> Analyzing: Enrichment Complete
        Analyzing --> Transforming: Analysis Failed
        Analyzing --> Processed: Analysis Complete
    }
    
    DataProcessing --> DataStored: Store Data [GEN]
    DataProcessing --> DataFailed: Processing Failed
    
    state DataStored {
        [*] --> Cached
        Cached --> Persistent: Cache to Persistent [INS]
        Persistent --> Archived: Archive Data
        Archived --> Cached: Retrieve Data
    }
    
    DataStored --> DataAccessed: Access Data [LOC]
    DataStored --> DataDeleted: Delete Data
    
    state DataAccessed {
        [*] --> Available
        Available --> InUse: Data in Use
        InUse --> Available: Use Complete
        InUse --> Locked: Data Locked
        Locked --> Available: Lock Released
    }
    
    DataAccessed --> DataStored: Return Data
    DataAccessed --> DataDeleted: Delete Data
    DataDeleted --> [*]: Cleanup
```

## Customization Options

### 1. State Types
- **Initial States**: Starting points of processes
- **Processing States**: Active computation states
- **Waiting States**: Resource or event waiting
- **Error States**: Failure and recovery states
- **Final States**: Completion or termination states

### 2. Case Annotations
Include relevant CEREBRUM case annotations:
- `[NOM]` - Nominative (subject/agent actions)
- `[ACC]` - Accusative (object/patient processing)
- `[GEN]` - Genitive (source/possession storage)
- `[DAT]` - Dative (recipient/beneficiary input)
- `[INS]` - Instrumental (means/method tools)
- `[LOC]` - Locative (location/context settings)
- `[ABL]` - Ablative (origin/cause triggers)

### 3. Transition Types
- **Automatic Transitions**: Time-based or condition-based
- **Manual Transitions**: User-initiated actions
- **Event-Driven Transitions**: External event triggers
- **Error Transitions**: Exception handling paths
- **Recovery Transitions**: Error recovery mechanisms

### 4. Interactive Features
Add clickable elements for:
- State documentation
- Transition logic
- Error handling procedures
- Recovery mechanisms
- Related state machines

## Best Practices

1. **Clear States**: Use descriptive, unambiguous state names
2. **Complete Transitions**: Cover all possible state changes
3. **Error Handling**: Include comprehensive error states and recovery
4. **Consistency**: Use consistent naming and styling conventions
5. **Documentation**: Include brief descriptions in state labels
6. **Validation**: Ensure all states are reachable and complete
7. **Simplicity**: Avoid overly complex state machines
8. **Testing**: Design for testable state transitions

## Common Patterns

### 1. Linear State Machine
```
State A → State B → State C → Final State
```

### 2. Cyclic State Machine
```
State A → State B → State C → State A
```

### 3. Hierarchical State Machine
```
Parent State {
    Child State A → Child State B
}
```

### 4. Parallel State Machine
```
State A1 → State A2
State B1 → State B2
```

### 5. Error Recovery Pattern
```
Normal State → Error State → Recovery State → Normal State
```

## State Machine Examples

### 1. Model Training Lifecycle
```
Created → Validated → Training → Evaluating → Deployed → Retired
```

### 2. System Boot Process
```
Off → Booting → Ready → Active → Maintenance → Ready
```

### 3. Data Processing Pipeline
```
Received → Validated → Processing → Stored → Accessed → Archived
```

### 4. User Session Management
```
Logged Out → Logging In → Authenticated → Active → Idle → Logged Out
``` 