# Integration Template

## Usage
This template is designed for API and service integration diagrams, system communication patterns, and interface specifications with CEREBRUM case annotations and modern styling.

## Template Code

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

sequenceDiagram
    participant U as 👤 User
    participant F as 🏛️ Frontend
    participant A as 🔌 API Gateway
    participant C as 🧠 CEREBRUM Core
    participant M as 📊 Model Service
    participant D as 🗄️ Database
    participant E as 🌐 External API
    
    %% User initiates request [DAT]
    U->>F: Submit Model Request [DAT]
    F->>A: Forward Request
    A->>C: Authenticate & Route
    
    %% Core processing [ACC]
    C->>M: Load Model Configuration [ACC]
    M->>D: Retrieve Model Data
    D-->>M: Return Model State
    M-->>C: Model Configuration
    
    %% Model execution [NOM]
    C->>C: Execute Active Inference [NOM]
    C->>C: Apply Case Transformations
    C->>C: Update Bayesian Beliefs
    
    %% Data persistence [GEN]
    C->>D: Save Results [GEN]
    D-->>C: Confirmation
    
    %% External integration [INS]
    C->>E: Fetch External Data [INS]
    E-->>C: External Response
    
    %% Response delivery [LOC]
    C-->>A: Processed Results [LOC]
    A-->>F: Formatted Response
    F-->>U: Display Results
```

## Alternative: API Architecture Diagram

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

graph TD
    %% Style definitions
    classDef client fill:#74B9FF,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef gateway fill:#A29BFE,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef service fill:#00B894,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef core fill:#4ECDC4,stroke:#2C3E50,stroke-width:3px,color:#FFFFFF,font-weight:bold
    classDef database fill:#FD79A8,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    classDef external fill:#FFE66D,stroke:#2C3E50,stroke-width:2px,color:#2C3E50,font-weight:600
    
    %% Client layer [DAT]
    subgraph "Client Layer [DAT]"
        direction LR
        WebApp["🌐 Web Application<br/><small>User interface</small>"]:::client
        MobileApp["📱 Mobile Application<br/><small>Mobile interface</small>"]:::client
        CLITool["💻 CLI Tool<br/><small>Command line interface</small>"]:::client
        SDK["🔧 SDK Library<br/><small>Developer tools</small>"]:::client
    end
    
    %% Gateway layer [ACC]
    subgraph "API Gateway [ACC]"
        direction TB
        LoadBalancer["⚖️ Load Balancer<br/><small>Request distribution</small>"]:::gateway
        AuthService["🔐 Authentication<br/><small>Identity verification</small>"]:::gateway
        RateLimiter["🚦 Rate Limiting<br/><small>Traffic control</small>"]:::gateway
        Router["🛣️ Request Router<br/><small>Path routing</small>"]:::gateway
    end
    
    %% Service layer [NOM]
    subgraph "Service Layer [NOM]"
        direction TB
        ModelService["🧠 Model Service<br/><small>Model management</small>"]:::service
        CaseService["🗂️ Case Service<br/><small>Case processing</small>"]:::service
        InferenceService["🎯 Inference Service<br/><small>Active inference</small>"]:::service
        TransformService["🔄 Transform Service<br/><small>Data transformation</small>"]:::service
    end
    
    %% Core engine [GEN]
    subgraph "CEREBRUM Core [GEN]"
        direction TB
        ActiveInference["🧠 Active Inference Engine<br/><small>Core reasoning</small>"]:::core
        BayesianEngine["📊 Bayesian Engine<br/><small>Probabilistic modeling</small>"]:::core
        CaseEngine["🗂️ Case Engine<br/><small>Case management</small>"]:::core
        TransformEngine["🔄 Transform Engine<br/><small>Data transformation</small>"]:::core
    end
    
    %% Data layer [INS]
    subgraph "Data Layer [INS]"
        direction LR
        PrimaryDB["🗄️ Primary Database<br/><small>Main data store</small>"]:::database
        CacheDB["⚡ Cache Database<br/><small>Performance cache</small>"]:::database
        FileStorage["📁 File Storage<br/><small>Document storage</small>"]:::database
        BackupDB["💾 Backup Database<br/><small>Data protection</small>"]:::database
    end
    
    %% External services [LOC]
    subgraph "External Services [LOC]"
        direction LR
        MLPlatform["🤖 ML Platform<br/><small>External ML services</small>"]:::external
        DataProvider["📊 Data Provider<br/><small>External data sources</small>"]:::external
        Monitoring["📈 Monitoring Service<br/><small>Performance tracking</small>"]:::external
        Notification["🔔 Notification Service<br/><small>Alert system</small>"]:::external
    end
    
    %% Client connections
    WebApp --> LoadBalancer
    MobileApp --> LoadBalancer
    CLITool --> LoadBalancer
    SDK --> LoadBalancer
    
    %% Gateway connections
    LoadBalancer --> AuthService
    AuthService --> RateLimiter
    RateLimiter --> Router
    
    %% Service connections
    Router --> ModelService
    Router --> CaseService
    Router --> InferenceService
    Router --> TransformService
    
    %% Core connections
    ModelService --> ActiveInference
    CaseService --> CaseEngine
    InferenceService --> BayesianEngine
    TransformService --> TransformEngine
    
    %% Data connections
    ActiveInference --> PrimaryDB
    BayesianEngine --> CacheDB
    CaseEngine --> FileStorage
    TransformEngine --> BackupDB
    
    %% External connections
    ActiveInference --> MLPlatform
    BayesianEngine --> DataProvider
    CaseEngine --> Monitoring
    TransformEngine --> Notification
    
    %% Interactive elements
    click WebApp "docs/clients/web-app.md" "View web app documentation"
    click LoadBalancer "src/gateway/load_balancer.py" "View load balancer"
    click ModelService "src/services/model_service.py" "View model service"
    click ActiveInference "src/core/active_inference.py" "View active inference"
    click PrimaryDB "docs/database/schema.md" "View database schema"
    click MLPlatform "docs/integrations/ml-platform.md" "View ML platform integration"
```

## Alternative: Microservices Architecture

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

graph TD
    %% Style definitions
    classDef api fill:#4ECDC4,stroke:#2C3E50,stroke-width:3px,color:#FFFFFF,font-weight:bold
    classDef service fill:#45B7D1,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef database fill:#96CEB4,stroke:#2C3E50,stroke-width:2px,color:#2C3E50,font-weight:500
    classDef message fill:#FFE66D,stroke:#2C3E50,stroke-width:2px,color:#2C3E50,font-weight:500
    classDef external fill:#FF6B6B,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    
    %% API Services [DAT]
    subgraph "API Services [DAT]"
        direction LR
        RESTAPI["🌐 REST API<br/><small>HTTP interface</small>"]:::api
        GraphQLAPI["📊 GraphQL API<br/><small>Query interface</small>"]:::api
        WebSocketAPI["🔌 WebSocket API<br/><small>Real-time interface</small>"]:::api
        gRPCAPI["⚡ gRPC API<br/><small>High-performance interface</small>"]:::api
    end
    
    %% Core Services [ACC]
    subgraph "Core Services [ACC]"
        direction TB
        ModelService["🧠 Model Service<br/><small>Model lifecycle management</small>"]:::service
        CaseService["🗂️ Case Service<br/><small>Case processing and storage</small>"]:::service
        InferenceService["🎯 Inference Service<br/><small>Active inference execution</small>"]:::service
        TransformService["🔄 Transform Service<br/><small>Data transformation</small>"]:::service
    end
    
    %% Support Services [NOM]
    subgraph "Support Services [NOM]"
        direction TB
        AuthService["🔐 Auth Service<br/><small>Authentication & authorization</small>"]:::service
        ConfigService["⚙️ Config Service<br/><small>Configuration management</small>"]:::service
        LoggingService["📝 Logging Service<br/><small>Centralized logging</small>"]:::service
        MonitoringService["📈 Monitoring Service<br/><small>Health monitoring</small>"]:::service
    end
    
    %% Data Services [GEN]
    subgraph "Data Services [GEN]"
        direction LR
        ModelDB["🗄️ Model Database<br/><small>Model storage</small>"]:::database
        CaseDB["🗂️ Case Database<br/><small>Case storage</small>"]:::database
        ConfigDB["⚙️ Config Database<br/><small>Configuration storage</small>"]:::database
        LogDB["📝 Log Database<br/><small>Log storage</small>"]:::database
    end
    
    %% Message Queue [INS]
    subgraph "Message Queue [INS]"
        direction TB
        EventBus["🚌 Event Bus<br/><small>Event distribution</small>"]:::message
        TaskQueue["📋 Task Queue<br/><small>Background processing</small>"]:::message
        NotificationQueue["🔔 Notification Queue<br/><small>Alert distribution</small>"]:::message
        DataPipeline["🔄 Data Pipeline<br/><small>Data flow management</small>"]:::message
    end
    
    %% External Integrations [LOC]
    subgraph "External Integrations [LOC]"
        direction LR
        MLPlatform["🤖 ML Platform<br/><small>External ML services</small>"]:::external
        DataProvider["📊 Data Provider<br/><small>External data sources</small>"]:::external
        MonitoringTool["📈 Monitoring Tool<br/><small>External monitoring</small>"]:::external
        NotificationService["🔔 Notification Service<br/><small>External notifications</small>"]:::external
    end
    
    %% API connections
    RESTAPI --> ModelService
    GraphQLAPI --> CaseService
    WebSocketAPI --> InferenceService
    gRPCAPI --> TransformService
    
    %% Service connections
    ModelService --> AuthService
    CaseService --> ConfigService
    InferenceService --> LoggingService
    TransformService --> MonitoringService
    
    %% Database connections
    ModelService --> ModelDB
    CaseService --> CaseDB
    ConfigService --> ConfigDB
    LoggingService --> LogDB
    
    %% Message queue connections
    ModelService --> EventBus
    CaseService --> TaskQueue
    InferenceService --> NotificationQueue
    TransformService --> DataPipeline
    
    %% External connections
    ModelService --> MLPlatform
    CaseService --> DataProvider
    MonitoringService --> MonitoringTool
    NotificationQueue --> NotificationService
    
    %% Interactive elements
    click RESTAPI "docs/api/rest.md" "View REST API documentation"
    click ModelService "src/services/model_service.py" "View model service"
    click AuthService "src/services/auth_service.py" "View auth service"
    click ModelDB "docs/database/model-schema.md" "View model database schema"
    click EventBus "src/messaging/event_bus.py" "View event bus implementation"
    click MLPlatform "docs/integrations/ml-platform.md" "View ML platform integration"
```

## Alternative: Service Mesh Architecture

```markdown
---
config:
  look: neo
  theme: cerebrum
  layout: elk
---

graph TD
    %% Style definitions
    classDef proxy fill:#4ECDC4,stroke:#2C3E50,stroke-width:3px,color:#FFFFFF,font-weight:bold
    classDef service fill:#45B7D1,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:600
    classDef control fill:#96CEB4,stroke:#2C3E50,stroke-width:2px,color:#2C3E50,font-weight:500
    classDef data fill:#FD79A8,stroke:#2C3E50,stroke-width:2px,color:#FFFFFF,font-weight:500
    
    %% Service proxies [DAT]
    subgraph "Service Proxies [DAT]"
        direction LR
        Proxy1["🔀 Proxy 1<br/><small>Service mesh proxy</small>"]:::proxy
        Proxy2["🔀 Proxy 2<br/><small>Service mesh proxy</small>"]:::proxy
        Proxy3["🔀 Proxy 3<br/><small>Service mesh proxy</small>"]:::proxy
        Proxy4["🔀 Proxy 4<br/><small>Service mesh proxy</small>"]:::proxy
    end
    
    %% Application services [ACC]
    subgraph "Application Services [ACC]"
        direction TB
        ServiceA["🧠 Service A<br/><small>Model service</small>"]:::service
        ServiceB["🗂️ Service B<br/><small>Case service</small>"]:::service
        ServiceC["🎯 Service C<br/><small>Inference service</small>"]:::service
        ServiceD["🔄 Service D<br/><small>Transform service</small>"]:::service
    end
    
    %% Control plane [NOM]
    subgraph "Control Plane [NOM]"
        direction TB
        Discovery["🔍 Service Discovery<br/><small>Service registration</small>"]:::control
        Config["⚙️ Configuration<br/><small>Policy management</small>"]:::control
        Security["🔐 Security<br/><small>Authentication & authorization</small>"]:::control
        Observability["📊 Observability<br/><small>Monitoring & tracing</small>"]:::control
    end
    
    %% Data plane [GEN]
    subgraph "Data Plane [GEN]"
        direction LR
        Traffic["🚦 Traffic Management<br/><small>Load balancing</small>"]:::data
        CircuitBreaker["⚡ Circuit Breaker<br/><small>Fault tolerance</small>"]:::data
        Retry["🔄 Retry Logic<br/><small>Error handling</small>"]:::data
        Timeout["⏰ Timeout Management<br/><small>Request timing</small>"]:::data
    end
    
    %% Proxy-service connections
    Proxy1 --> ServiceA
    Proxy2 --> ServiceB
    Proxy3 --> ServiceC
    Proxy4 --> ServiceD
    
    %% Service-service connections
    ServiceA --> Proxy2
    ServiceB --> Proxy3
    ServiceC --> Proxy4
    ServiceD --> Proxy1
    
    %% Control plane connections
    Discovery --> Proxy1
    Config --> Proxy2
    Security --> Proxy3
    Observability --> Proxy4
    
    %% Data plane connections
    Traffic --> Proxy1
    CircuitBreaker --> Proxy2
    Retry --> Proxy3
    Timeout --> Proxy4
    
    %% Interactive elements
    click Proxy1 "docs/service-mesh/proxy.md" "View proxy configuration"
    click ServiceA "src/services/model_service.py" "View model service"
    click Discovery "src/service-mesh/discovery.py" "View service discovery"
    click Traffic "src/service-mesh/traffic.py" "View traffic management"
```

## Customization Options

### 1. Integration Types
- **API Integration**: REST, GraphQL, gRPC interfaces
- **Service Integration**: Microservices communication
- **Data Integration**: Database and storage connections
- **Event Integration**: Message queues and event streams
- **External Integration**: Third-party service connections

### 2. Case Annotations
Include relevant CEREBRUM case annotations:
- `[NOM]` - Nominative (subject/agent services)
- `[ACC]` - Accusative (object/patient data)
- `[GEN]` - Genitive (source/possession storage)
- `[DAT]` - Dative (recipient/beneficiary clients)
- `[INS]` - Instrumental (means/method tools)
- `[LOC]` - Locative (location/context external)
- `[ABL]` - Ablative (origin/cause events)

### 3. Communication Patterns
- **Synchronous**: Request-response patterns
- **Asynchronous**: Event-driven patterns
- **Streaming**: Real-time data flows
- **Batch**: Bulk data processing
- **Hybrid**: Mixed communication modes

### 4. Interactive Features
Add clickable elements for:
- API documentation
- Service specifications
- Database schemas
- Configuration files
- Monitoring dashboards

## Best Practices

1. **Loose Coupling**: Minimize dependencies between services
2. **High Cohesion**: Group related functionality together
3. **Fault Tolerance**: Implement circuit breakers and retry logic
4. **Observability**: Include comprehensive monitoring and logging
5. **Security**: Implement proper authentication and authorization
6. **Performance**: Optimize for latency and throughput
7. **Scalability**: Design for horizontal scaling
8. **Documentation**: Maintain clear API and service documentation

## Common Patterns

### 1. API Gateway Pattern
```
Client → API Gateway → Multiple Services
```

### 2. Service Mesh Pattern
```
Service → Proxy → Service Mesh → Proxy → Service
```

### 3. Event-Driven Pattern
```
Service → Event Bus → Multiple Subscribers
```

### 4. CQRS Pattern
```
Command → Command Handler → Write Model
Query → Query Handler → Read Model
``` 