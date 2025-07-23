# User Journey Template

## Usage
This template is designed for user experience flows, interaction patterns, and workflow visualizations with CEREBRUM case annotations and modern styling.

## Template Code

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

journey
    title CEREBRUM User Journey: Model Development Workflow
    
    section Initial Setup [DAT]
        Define Problem: 5: User, System
        Configure Environment: 4: User, System
        Import Data: 3: User, System
        Validate Input: 4: System
    
    section Model Development [ACC]
        Select Case Structure: 5: User
        Configure Bayesian Model: 4: User, System
        Define Transformations: 4: User, System
        Set Parameters: 3: User
    
    section Training & Validation [NOM]
        Start Training: 5: System
        Monitor Progress: 4: User, System
        Validate Results: 4: User, System
        Adjust Parameters: 3: User
    
    section Deployment [GEN]
        Export Model: 4: User, System
        Deploy to Production: 5: System
        Monitor Performance: 4: System
        Generate Reports: 3: System
    
    section Maintenance [INS]
        Update Model: 4: User, System
        Retrain with New Data: 3: System
        Optimize Performance: 4: System
        Archive Old Versions: 2: System
```

## Alternative: User Flow Diagram

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
    classDef user fill:#74B9FF,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef system fill:#45B7D1,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef decision fill:#FDCB6E,stroke:#2C3E50,stroke-width:2px,color:#2C3E50,font-weight:600
    classDef data fill:#FD79A8,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    classDef end fill:#FF6B6B,stroke:#2C3E50,stroke-width:3px,color:#FFFFFF,font-weight:bold
    
    %% User journey start
    Start([🚀 Start Journey<br/><small>User begins interaction</small>]):::start
    
    %% User actions [DAT]
    subgraph "User Actions [DAT]"
        direction TB
        Login["👤 User Login<br/><small>Authentication process</small>"]:::user
        Navigate["🧭 Navigation<br/><small>Browse interface</small>"]:::user
        Input["📝 Data Input<br/><small>Enter information</small>"]:::user
        Configure["⚙️ Configuration<br/><small>Set preferences</small>"]:::user
    end
    
    %% System responses [ACC]
    subgraph "System Responses [ACC]"
        direction TB
        Validate["✅ Validation<br/><small>Check input quality</small>"]:::system
        Process["🔄 Processing<br/><small>Execute operations</small>"]:::system
        Analyze["📊 Analysis<br/><small>Generate insights</small>"]:::system
        Present["📋 Presentation<br/><small>Display results</small>"]:::system
    end
    
    %% Decision points [NOM]
    subgraph "Decision Points [NOM]"
        direction LR
        QualityCheck{"❓ Quality Check<br/><small>Meet standards?</small>"}:::decision
        UserSatisfaction{"😊 User Satisfied?<br/><small>Happy with results?</small>"}:::decision
        ContinueProcess{"🔄 Continue?<br/><small>Proceed further?</small>"}:::decision
    end
    
    %% Data interactions [GEN]
    subgraph "Data Interactions [GEN]"
        direction LR
        SaveData[("💾 Save Data<br/><small>Persist information</small>")]:::data
        LoadData[("📂 Load Data<br/><small>Retrieve information</small>")]:::data
        ExportData[("📤 Export Results<br/><small>Share outcomes</small>")]:::data
        BackupData[("💾 Backup<br/><small>Data protection</small>")]:::data
    end
    
    %% Process flows [INS]
    subgraph "Process Flows [INS]"
        direction TB
        Iterate["🔄 Iterate<br/><small>Refine and improve</small>"]:::system
        Optimize["⚡ Optimize<br/><small>Performance tuning</small>"]:::system
        Scale["📈 Scale<br/><small>Expand capacity</small>"]:::system
        Maintain["🔧 Maintain<br/><small>Ongoing support</small>"]:::system
    end
    
    %% End states
    Success([✅ Journey Complete<br/><small>Successful completion</small>]):::end
    Failure([❌ Journey Failed<br/><small>Unsuccessful outcome</small>]):::end
    
    %% Main flow
    Start --> Login
    Login --> Navigate
    Navigate --> Input
    
    Validate --> QualityCheck
    QualityCheck -->|Pass| Process
    QualityCheck -->|Fail| Input
    
    Process --> Analyze
    Analyze --> Present
    Present --> UserSatisfaction
    
    UserSatisfaction -->|Yes| SaveData
    UserSatisfaction -->|No| Iterate
    
    SaveData --> ContinueProcess
    ContinueProcess -->|Yes| Scale
    ContinueProcess -->|No| Success
    
    Iterate --> Optimize
    Optimize --> Maintain
    Maintain --> Failure
    
    %% Data connections
    Input --> LoadData
    Process --> SaveData
    Present --> ExportData
    Scale --> BackupData
    
    %% Interactive elements
    click Start "docs/getting-started/" "View getting started guide"
    click Login "src/auth/login.py" "View authentication logic"
    click Validate "src/validation/" "View validation rules"
    click Process "src/processing/" "View processing logic"
    click SaveData "docs/data-management/" "View data management"
    click Success "docs/success-stories/" "View success examples"
```

## Alternative: Customer Journey Map

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

graph TD
    %% Style definitions
    classDef awareness fill:#74B9FF,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef consideration fill:#A29BFE,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef decision fill:#00B894,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef purchase fill:#FFE66D,stroke:#2C3E50,stroke-width:2px,color:#2C3E50,font-weight:600
    classDef retention fill:#FF6B6B,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    
    %% Customer journey stages
    subgraph "Awareness Stage [DAT]"
        direction LR
        Discovery["🔍 Problem Discovery<br/><small>Identify need</small>"]:::awareness
        Research["📚 Market Research<br/><small>Explore solutions</small>"]:::awareness
        Comparison["⚖️ Solution Comparison<br/><small>Evaluate options</small>"]:::awareness
    end
    
    subgraph "Consideration Stage [ACC]"
        direction TB
        Evaluation["📊 Technical Evaluation<br/><small>Assess capabilities</small>"]:::consideration
        Testing["🧪 Trial Testing<br/><small>Proof of concept</small>"]:::consideration
        Validation["✅ Requirements Validation<br/><small>Check fit</small>"]:::consideration
    end
    
    subgraph "Decision Stage [NOM]"
        direction LR
        Approval["👥 Stakeholder Approval<br/><small>Get buy-in</small>"]:::decision
        Budget["💰 Budget Allocation<br/><small>Secure funding</small>"]:::decision
        Procurement["📋 Procurement Process<br/><small>Legal review</small>"]:::decision
    end
    
    subgraph "Purchase Stage [GEN]"
        direction TB
        Acquisition["🛒 Product Acquisition<br/><small>Purchase decision</small>"]:::purchase
        Implementation["🚀 Implementation<br/><small>Deploy solution</small>"]:::purchase
        Training["🎓 User Training<br/><small>Skill development</small>"]:::purchase
    end
    
    subgraph "Retention Stage [INS]"
        direction LR
        Usage["📈 Active Usage<br/><small>Regular use</small>"]:::retention
        Support["🆘 Ongoing Support<br/><small>Help and maintenance</small>"]:::retention
        Expansion["📊 Solution Expansion<br/><small>Scale usage</small>"]:::retention
    end
    
    %% Journey flow
    Discovery --> Research
    Research --> Comparison
    Comparison --> Evaluation
    
    Evaluation --> Testing
    Testing --> Validation
    Validation --> Approval
    
    Approval --> Budget
    Budget --> Procurement
    Procurement --> Acquisition
    
    Acquisition --> Implementation
    Implementation --> Training
    Training --> Usage
    
    Usage --> Support
    Support --> Expansion
    Expansion --> Usage
    
    %% Interactive elements
    click Discovery "docs/use-cases/" "View use case examples"
    click Evaluation "docs/technical-specs/" "View technical specifications"
    click Implementation "docs/deployment/" "View deployment guide"
    click Support "docs/support/" "View support documentation"
```

## Alternative: Service Blueprint

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

graph TD
    %% Style definitions
    classDef customer fill:#74B9FF,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef frontstage fill:#A29BFE,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef backstage fill:#00B894,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef support fill:#FFE66D,stroke:#2C3E50,stroke-width:2px,color:#2C3E50,font-weight:600
    classDef process fill:#FF6B6B,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    
    %% Customer actions
    subgraph "Customer Actions [DAT]"
        direction LR
        Contact["📞 Contact Support<br/><small>Initiate request</small>"]:::customer
        Provide["📝 Provide Information<br/><small>Share details</small>"]:::customer
        Review["👀 Review Solution<br/><small>Evaluate response</small>"]:::customer
        Accept["✅ Accept Solution<br/><small>Confirm resolution</small>"]:::customer
    end
    
    %% Frontstage actions
    subgraph "Frontstage Actions [ACC]"
        direction TB
        Receive["📥 Receive Request<br/><small>Initial contact</small>"]:::frontstage
        Clarify["❓ Clarify Requirements<br/><small>Gather details</small>"]:::frontstage
        Present["📋 Present Solution<br/><small>Show options</small>"]:::frontstage
        Confirm["✅ Confirm Resolution<br/><small>Verify satisfaction</small>"]:::frontstage
    end
    
    %% Backstage actions
    subgraph "Backstage Actions [NOM]"
        direction TB
        Analyze["🔍 Analyze Problem<br/><small>Technical investigation</small>"]:::backstage
        Develop["🛠️ Develop Solution<br/><small>Create fix</small>"]:::backstage
        Test["🧪 Test Solution<br/><small>Validate fix</small>"]:::backstage
        Document["📚 Document Resolution<br/><small>Record solution</small>"]:::backstage
    end
    
    %% Support processes
    subgraph "Support Processes [GEN]"
        direction LR
        Escalation["⬆️ Escalation<br/><small>Advanced support</small>"]:::support
        Collaboration["🤝 Collaboration<br/><small>Team coordination</small>"]:::support
        Knowledge["📖 Knowledge Base<br/><small>Solution repository</small>"]:::support
        Training["🎓 Training<br/><small>Skill development</small>"]:::support
    end
    
    %% Physical evidence
    subgraph "Physical Evidence [INS]"
        direction TB
        Interface["🖥️ User Interface<br/><small>Visual elements</small>"]:::process
        Documentation["📄 Documentation<br/><small>Written materials</small>"]:::process
        Reports["📊 Reports<br/><small>Analytics output</small>"]:::process
        Feedback["💬 Feedback System<br/><small>User input</small>"]:::process
    end
    
    %% Service flow
    Contact --> Receive
    Receive --> Analyze
    Analyze --> Clarify
    Clarify --> Provide
    
    Provide --> Develop
    Develop --> Test
    Test --> Present
    Present --> Review
    
    Review --> Accept
    Accept --> Confirm
    Confirm --> Document
    
    %% Support connections
    Analyze --> Escalation
    Develop --> Collaboration
    Document --> Knowledge
    Confirm --> Training
    
    %% Evidence connections
    Receive --> Interface
    Present --> Documentation
    Document --> Reports
    Confirm --> Feedback
    
    %% Interactive elements
    click Contact "docs/support/contact.md" "View contact information"
    click Analyze "src/support/analysis.py" "View analysis tools"
    click Develop "src/support/development.py" "View solution development"
    click Knowledge "docs/knowledge-base/" "View knowledge base"
```

## Customization Options

### 1. Journey Types
- **User Journey**: Individual user experience flows
- **Customer Journey**: Business-to-customer interactions
- **Service Blueprint**: Service delivery processes
- **User Flow**: Interface interaction patterns
- **Process Flow**: Operational workflows

### 2. Case Annotations
Include relevant CEREBRUM case annotations:
- `[NOM]` - Nominative (subject/agent actions)
- `[ACC]` - Accusative (object/patient interactions)
- `[GEN]` - Genitive (source/possession processes)
- `[DAT]` - Dative (recipient/beneficiary flows)
- `[INS]` - Instrumental (means/method tools)
- `[LOC]` - Locative (location/context settings)
- `[ABL]` - Ablative (origin/cause triggers)

### 3. Journey Stages
- **Awareness**: Problem discovery and research
- **Consideration**: Evaluation and validation
- **Decision**: Approval and procurement
- **Purchase**: Acquisition and implementation
- **Retention**: Usage and expansion

### 4. Interactive Features
Add clickable elements for:
- User interface mockups
- Process documentation
- Training materials
- Support resources
- Related journeys

## Best Practices

1. **User-Centered**: Focus on user needs and goals
2. **Realistic**: Base on actual user behavior and data
3. **Comprehensive**: Cover all touchpoints and interactions
4. **Actionable**: Provide clear next steps and improvements
5. **Measurable**: Include metrics and success criteria
6. **Iterative**: Plan for continuous improvement
7. **Accessible**: Ensure inclusive design principles
8. **Documented**: Maintain clear documentation

## Common Patterns

### 1. Linear Journey
```
Awareness → Consideration → Decision → Purchase → Retention
```

### 2. Circular Journey
```
Usage → Feedback → Improvement → Enhanced Usage
```

### 3. Branching Journey
```
Start → Decision Point → Path A or Path B → Different Outcomes
```

### 4. Parallel Journey
```
Multiple users → Different paths → Converging outcomes
``` 