# CEREBRUM Insect Cognitive Diagrams

This document contains technical diagrams illustrating the integration of insect cognitive systems with the CEREBRUM framework.

## 1. Insect Neural Architecture with CEREBRUM Case Mapping

```mermaid
flowchart TD
    subgraph Brain["Insect Brain with CEREBRUM Case Assignments"]
        subgraph MB["Mushroom Bodies"]
            direction TB
            KC["Kenyon Cells<br>[ACC]<br><small>Learning & memory</small>"]
            CA["Calyx<br>[DAT]<br><small>Sensory input</small>"]
            VL["Vertical Lobe<br>[GEN]<br><small>Output generation</small>"]
            ML["Medial Lobe<br>[NOM]<br><small>Action selection</small>"]
            
            CA --> KC
            KC --> VL
            KC --> ML
        end
        
        subgraph CX["Central Complex"]
            direction TB
            FB["Fan-shaped Body<br>[NOM]<br><small>Motor control</small>"]
            EB["Ellipsoid Body<br>[LOC]<br><small>Spatial context</small>"]
            PB["Protocerebral Bridge<br>[INS]<br><small>Orientation method</small>"]
            NO["Noduli<br>[ABL]<br><small>Movement origin</small>"]
            
            PB --> FB
            EB --> FB
            NO --> FB
        end
        
        subgraph AL["Antennal Lobes"]
            direction TB
            GML["Glomeruli<br>[DAT]/[PHE]<br><small>Olfactory input</small>"]
            PN["Projection Neurons<br>[GEN]<br><small>Signal processing</small>"]
            LN["Local Neurons<br>[INS]<br><small>Lateral inhibition</small>"]
            
            GML --> PN
            GML --> LN
            LN --> GML
            PN --> MB
        end
        
        subgraph OL["Optic Lobes"]
            direction TB
            LA["Lamina<br>[DAT]<br><small>Primary visual input</small>"]
            ME["Medulla<br>[INS]<br><small>Feature processing</small>"]
            LO["Lobula<br>[GEN]<br><small>Motion detection</small>"]
            
            LA --> ME --> LO
            LO --> MB
        end
        
        subgraph SEG["Subesophageal Ganglion"]
            direction TB
            MN["Motor Neurons<br>[GEN]<br><small>Feeding output</small>"]
            SN["Sensory Neurons<br>[DAT]<br><small>Gustatory input</small>"]
            CPG["Central Pattern Generators<br>[INS]<br><small>Rhythmic patterns</small>"]
            
            SN --> CPG --> MN
        end
    end
    
    subgraph Integration["Cross-Structure Integration"]
        direction TB
        CX -->|"Navigation<br>commands"| MB
        MB -->|"Contextual<br>memory"| CX
        
        AL -->|"Olfactory<br>information"| MB
        OL -->|"Visual<br>information"| MB
        
        MB -->|"Behavioral<br>decisions"| SEG
        CX -->|"Motor<br>coordination"| SEG
    end
    
    classDef mb fill:#e6f3ff,stroke:#4682b4,stroke-width:2px,color:#00008B
    classDef cx fill:#ffe6e6,stroke:#ff9999,stroke-width:2px,color:#8B0000
    classDef al fill:#e6ffe6,stroke:#99cc99,stroke-width:2px,color:#006600
    classDef ol fill:#fff5e6,stroke:#ffb347,stroke-width:2px,color:#663300
    classDef seg fill:#f0f0ff,stroke:#b19cd9,stroke-width:2px,color:#4b0082
    
    class MB,KC,CA,VL,ML mb
    class CX,FB,EB,PB,NO cx
    class AL,GML,PN,LN al
    class OL,LA,ME,LO ol
    class SEG,MN,SN,CPG seg
```

## 2. Pheromonal Communication System with CEREBRUM Cases

```mermaid
sequenceDiagram
    participant Env as Environment
    participant Emitter as Emitter Ant [GEN]/[PHE]
    participant Trail as Pheromone Trail [STIG]
    participant Receiver as Receiver Ant [DAT]/[PHE]
    participant Brain as Neural Processing [ACC]/[NOM]
    participant Behavior as Behavioral Response [NOM]
    
    Note over Emitter,Behavior: Pheromonal Communication Cycle with CEREBRUM Cases
    
    Emitter->>Emitter: Food discovery [NOM]
    Emitter->>Emitter: Decision to communicate [ACC]
    Emitter->>Trail: Pheromone deposition [GEN]/[PHE]
    Note right of Emitter: Precision focused on signal fidelity
    
    Trail->>Env: Environmental persistence [STIG]
    Note right of Trail: Modulated by temperature, humidity
    
    Receiver->>Trail: Antennae contact [DAT]
    Trail->>Receiver: Chemical detection [PHE]
    Note right of Receiver: Precision focused on detection sensitivity
    
    Receiver->>Brain: Signal transduction
    Brain->>Brain: Signal interpretation [ACC]
    Note right of Brain: Parameter updates in antennal lobe circuits
    
    Brain->>Behavior: Behavioral decision [NOM]
    Note right of Brain: Shift to motor pattern generation
    
    Behavior->>Receiver: Follow trail/recruit [NOM]
    Receiver->>Trail: Additional pheromone [GEN]
    Note right of Receiver: Positive feedback amplification
    
    Trail->>Env: Signal decay over time
    
    Behavior->>Emitter: Colony-level coordination [SWARM]
    Note over Emitter,Behavior: Emergent colony intelligence
```

## 3. Metamorphic Neural Reorganization with CEREBRUM [MET] Case

```mermaid
graph TB
    subgraph LarvalStage["Larval Stage Neural Organization"]
        L_Feeding["Feeding Circuits<br>[NOM]<br><small>Primary behavior</small>"]
        L_Sensory["Larval Sensory System<br>[DAT]<br><small>Limited scope</small>"]
        L_Learning["Simple Learning<br>[ACC]<br><small>Basic associations</small>"]
        L_Motor["Crawling Motor Patterns<br>[GEN]<br><small>Locomotion output</small>"]
    end
    
    subgraph Metamorphosis["Metamorphosis Period [MET]"]
        direction TB
        Hormones["Hormonal Triggers<br><small>Ecdysone cascade</small>"]
        Apoptosis["Neural Apoptosis<br><small>Selective pruning</small>"]
        Neurogenesis["Neurogenesis<br><small>Adult circuit formation</small>"]
        Rewiring["Circuit Rewiring<br><small>Functional reorganization</small>"]
        
        Hormones --> Apoptosis
        Hormones --> Neurogenesis
        Apoptosis --> Rewiring
        Neurogenesis --> Rewiring
    end
    
    subgraph AdultStage["Adult Stage Neural Organization"]
        A_Flight["Flight Control System<br>[NOM]<br><small>Complex navigation</small>"]
        A_Sensory["Adult Sensory System<br>[DAT]<br><small>Enhanced capacity</small>"]
        A_Learning["Complex Learning<br>[ACC]<br><small>Advanced associations</small>"]
        A_Motor["Flight Motor Patterns<br>[GEN]<br><small>3D movement output</small>"]
        A_Mating["Reproductive Behavior<br>[NOM]/[GEN]<br><small>New capability</small>"]
    end
    
    LarvalStage -->|"Triggers"| Metamorphosis
    Metamorphosis -->|"Produces"| AdultStage
    
    L_Feeding -.->|"Partially preserved"| A_Sensory
    L_Sensory -.->|"Drastically modified"| A_Sensory
    L_Learning -.->|"Enhanced"| A_Learning
    L_Motor -.->|"Completely reorganized"| A_Motor
    
    classDef larval fill:#e6f3ff,stroke:#4682b4,stroke-width:2px,color:#00008B
    classDef meta fill:#ffe6e6,stroke:#ff9999,stroke-width:2px,color:#8B0000
    classDef adult fill:#e6ffe6,stroke:#99cc99,stroke-width:2px,color:#006600
    
    class L_Feeding,L_Sensory,L_Learning,L_Motor,LarvalStage larval
    class Hormones,Apoptosis,Neurogenesis,Rewiring,Metamorphosis meta
    class A_Flight,A_Sensory,A_Learning,A_Motor,A_Mating,AdultStage adult
```

## 4. Precision Dynamics in Foraging Decision-Making

```mermaid
graph TB
    subgraph PrecisionModulation["Precision Modulation in Foraging Decision"]
        direction TB
        
        subgraph States["Behavioral States"]
            Explore["Exploration<br>[NOM]<br><small>Search behavior</small>"]
            Evaluate["Evaluation<br>[DAT]/[ACC]<br><small>Resource assessment</small>"]
            Exploit["Exploitation<br>[NOM]/[GEN]<br><small>Resource collection</small>"]
            Recruit["Recruitment<br>[GEN]/[PHE]<br><small>Communication</small>"]
        end
        
        subgraph Precision["Precision Parameters"]
            direction TB
            
            P_Motor["Motor Precision<br>(γ motor)<br><small>Action certainty</small>"]
            P_Sensory["Sensory Precision<br>(γ sensory)<br><small>Input reliability</small>"]
            P_Memory["Memory Precision<br>(γ memory)<br><small>Prior reliability</small>"]
            P_Social["Social Precision<br>(γ social)<br><small>Communication weight</small>"]
            
            AmineLevel["Octopamine Level<br><small>Arousal state</small>"]
            SatietyLevel["Satiety Level<br><small>Motivational state</small>"]
            UncertaintyLevel["Uncertainty Level<br><small>Environmental variability</small>"]
            SocialContext["Social Context<br><small>Colony needs</small>"]
        end
    end
    
    %% State transitions
    Explore -->|"Resource detected"| Evaluate
    Evaluate -->|"High quality"| Exploit
    Evaluate -->|"Low quality"| Explore
    Exploit -->|"Successful collection"| Recruit
    Recruit -->|"Communication complete"| Exploit
    
    %% Precision modulation
    AmineLevel -->|"Modulates"| P_Motor
    AmineLevel -->|"Modulates"| P_Sensory
    SatietyLevel -->|"Modulates"| P_Memory
    UncertaintyLevel -->|"Modulates"| P_Sensory
    SocialContext -->|"Modulates"| P_Social
    
    %% Precision effects on states
    P_Motor -->|"Controls"| Explore
    P_Sensory -->|"Controls"| Evaluate
    P_Memory -->|"Controls"| Exploit
    P_Social -->|"Controls"| Recruit
    
    %% Dynamic relationships (feedback)
    Exploit -.->|"Increases"| SatietyLevel
    Explore -.->|"Increases"| AmineLevel
    Evaluate -.->|"Decreases"| UncertaintyLevel
    Recruit -.->|"Affects"| SocialContext
    
    classDef state fill:#e6f3ff,stroke:#4682b4,stroke-width:2px,color:#00008B
    classDef precision fill:#ffe6e6,stroke:#ff9999,stroke-width:2px,color:#8B0000
    classDef modulator fill:#e6ffe6,stroke:#99cc99,stroke-width:2px,color:#006600
    
    class Explore,Evaluate,Exploit,Recruit,States state
    class P_Motor,P_Sensory,P_Memory,P_Social,Precision precision
    class AmineLevel,SatietyLevel,UncertaintyLevel,SocialContext modulator
```

## 5. Stigmergic Construction System

```mermaid
flowchart TB
    subgraph StigmergicSystem["Termite Mound Construction System [STIG]"]
        direction TB
        
        subgraph Environment["Environmental Elements"]
            Substrate["Building Material<br>[SUB]<br><small>Soil, saliva, excretions</small>"]
            Structure["Existing Structure<br>[LOC]<br><small>Current mound state</small>"]
            Gradients["Environmental Gradients<br>[LOC]<br><small>Temperature, CO2, humidity</small>"]
        end
        
        subgraph Construction["Construction Process"]
            Sensing["Material/Structure Sensing<br>[DAT]<br><small>Tactile/chemical assessment</small>"]
            Decision["Building Decision<br>[NOM]<br><small>Rule-based response</small>"]
            Action["Building Action<br>[GEN]<br><small>Material manipulation</small>"]
            Template["Building Template<br>[INS]<br><small>Construction method</small>"]
        end
        
        subgraph Emergence["Emergent Properties"]
            Ventilation["Ventilation System<br>[SWARM]<br><small>Collective outcome</small>"]
            Regulation["Climate Regulation<br>[SWARM]<br><small>Collective outcome</small>"]
            Defense["Defensive Structure<br>[SWARM]<br><small>Collective outcome</small>"]
        end
    end
    
    %% Process flows
    Substrate -->|"Provides material"| Action
    Structure -->|"Influences"| Sensing
    Gradients -->|"Affects"| Decision
    
    Sensing -->|"Informs"| Decision
    Decision -->|"Guides"| Action
    Template -->|"Shapes"| Decision
    Action -->|"Modifies"| Structure
    
    Structure -->|"Emerges as"| Ventilation
    Structure -->|"Emerges as"| Regulation
    Structure -->|"Emerges as"| Defense
    
    %% Feedback loops
    Ventilation -.->|"Alters"| Gradients
    Regulation -.->|"Stabilizes"| Gradients
    Defense -.->|"Protects"| Structure
    
    classDef environment fill:#e6f3ff,stroke:#4682b4,stroke-width:2px,color:#00008B
    classDef construction fill:#ffe6e6,stroke:#ff9999,stroke-width:2px,color:#8B0000
    classDef emergence fill:#e6ffe6,stroke:#99cc99,stroke-width:2px,color:#006600
    
    class Substrate,Structure,Gradients,Environment environment
    class Sensing,Decision,Action,Template,Construction construction
    class Ventilation,Regulation,Defense,Emergence emergence
```

## 6. Caste Differentiation System

```mermaid
graph TB
    subgraph CasteDifferentiation["Honeybee Caste Differentiation System [CAST]"]
        direction TB
        
        subgraph Genetics["Genetic Foundation"]
            Genome["Genome<br><small>Shared genetic material</small>"]
            Epigenetics["Epigenetics<br><small>Gene expression regulation</small>"]
        end
        
        subgraph Development["Developmental Trajectories"]
            QueenLarva["Queen Larva<br>[MET]<br><small>Royal jelly fed</small>"]
            WorkerLarva["Worker Larva<br>[MET]<br><small>Limited royal jelly</small>"]
            DroneLarva["Drone Larva<br>[MET]<br><small>Unfertilized egg</small>"]
        end
        
        subgraph AdultCastes["Adult Caste Functions"]
            Queen["Queen<br>[CAST]/[GEN]<br><small>Reproduction specialist</small>"]
            
            subgraph Workers["Worker Age Polyethism"]
                NurseWorker["Nurse Worker<br>[CAST]/[INS]<br><small>Brood care specialist</small>"]
                HouseWorker["House Worker<br>[CAST]/[SUB]<br><small>Nest maintenance specialist</small>"]
                ForagerWorker["Forager Worker<br>[CAST]/[NOM]<br><small>Resource collection specialist</small>"]
            end
            
            Drone["Drone<br>[CAST]/[GEN]<br><small>Reproduction specialist</small>"]
        end
        
        subgraph Regulation["Regulatory System"]
            QueenPheromone["Queen Mandibular Pheromone<br>[PHE]<br><small>Colony cohesion signal</small>"]
            BroodPheromone["Brood Pheromone<br>[PHE]<br><small>Care elicitation signal</small>"]
            WorkerFeedback["Worker Interaction<br>[STIG]<br><small>Task allocation feedback</small>"]
        end
    end
    
    %% Development pathways
    Genome -->|"Determines"| QueenLarva
    Genome -->|"Determines"| WorkerLarva
    Genome -->|"Determines"| DroneLarva
    
    Epigenetics -->|"Modifies"| QueenLarva
    Epigenetics -->|"Modifies"| WorkerLarva
    
    QueenLarva -->|"Develops into"| Queen
    WorkerLarva -->|"Develops into"| NurseWorker
    DroneLarva -->|"Develops into"| Drone
    
    %% Worker age polyethism
    NurseWorker -->|"Ages into"| HouseWorker
    HouseWorker -->|"Ages into"| ForagerWorker
    
    %% Regulatory influences
    Queen -->|"Produces"| QueenPheromone
    Queen -->|"Produces"| BroodPheromone
    
    QueenPheromone -->|"Inhibits"| Queen
    QueenPheromone -->|"Regulates"| NurseWorker
    QueenPheromone -->|"Regulates"| HouseWorker
    QueenPheromone -->|"Regulates"| ForagerWorker
    
    BroodPheromone -->|"Stimulates"| NurseWorker
    BroodPheromone -->|"Influences"| ForagerWorker
    
    NurseWorker -->|"Contributes to"| WorkerFeedback
    HouseWorker -->|"Contributes to"| WorkerFeedback
    ForagerWorker -->|"Contributes to"| WorkerFeedback
    
    WorkerFeedback -->|"Balances"| NurseWorker
    WorkerFeedback -->|"Balances"| HouseWorker
    WorkerFeedback -->|"Balances"| ForagerWorker
    
    %% Resource flows
    ForagerWorker -->|"Provisions"| HouseWorker
    HouseWorker -->|"Feeds"| NurseWorker
    NurseWorker -->|"Nourishes"| Queen
    NurseWorker -->|"Cares for"| QueenLarva
    NurseWorker -->|"Cares for"| WorkerLarva
    NurseWorker -->|"Cares for"| DroneLarva
    
    classDef genetic fill:#e6f3ff,stroke:#4682b4,stroke-width:2px,color:#00008B
    classDef develop fill:#ffe6e6,stroke:#ff9999,stroke-width:2px,color:#8B0000
    classDef adult fill:#e6ffe6,stroke:#99cc99,stroke-width:2px,color:#006600
    classDef regulate fill:#fff5e6,stroke:#ffb347,stroke-width:2px,color:#663300
    
    class Genome,Epigenetics,Genetics genetic
    class QueenLarva,WorkerLarva,DroneLarva,Development develop
    class Queen,Workers,NurseWorker,HouseWorker,ForagerWorker,Drone,AdultCastes adult
    class QueenPheromone,BroodPheromone,WorkerFeedback,Regulation regulate
```

## 7. Swarm Decision-Making Process

```mermaid
stateDiagram-v2
    [*] --> QuiescentState
    
    state "Quiescent Colony [LOC]" as QuiescentState
    state "Queen Assessment [NOM]/[CAST]" as QueenAssessment
    state "Scout Activation [VOC]/[NOM]" as ScoutActivation
    
    state ScoutForaging {
        [*] --> IndividualSearch
        IndividualSearch: Scout bees [NOM]
        IndividualSearch --> SiteAssessment
        SiteAssessment: Quality evaluation [DAT]/[ACC]
        SiteAssessment --> DanceRecruitment
        DanceRecruitment: Dance communication [GEN]/[PHE]
        DanceRecruitment --> [*]
    }
    
    state ConsensusBuilding {
        [*] --> MultipleOptions
        MultipleOptions: Competing sites [SWARM]
        MultipleOptions --> StrengthAssessment
        StrengthAssessment: Dance vigor comparison [DAT]
        StrengthAssessment --> CrossInhibition
        CrossInhibition: Stop signal production [GEN]/[PHE]
        CrossInhibition --> Convergence
        Convergence: Majority formation [SWARM]
        Convergence --> [*]
    }
    
    state SwarmPreparation {
        [*] --> FoodCollection
        FoodCollection: Resource gathering [NOM]
        FoodCollection --> ClusterFormation
        ClusterFormation: Physical aggregation [SWARM]
        ClusterFormation --> ThermalRegulation
        ThermalRegulation: Temperature control [SWARM]
        ThermalRegulation --> [*]
    }
    
    state SwarmMovement {
        [*] --> LiftOff
        LiftOff: Coordinated departure [SWARM]
        LiftOff --> DirectionalGuidance
        DirectionalGuidance: Streaker bee guidance [INS]/[LOC]
        DirectionalGuidance --> ClusterTravelFormation
        ClusterTravelFormation: Travel formation [SWARM]
        ClusterTravelFormation --> [*]
    }
    
    state NewNestEstablishment {
        [*] --> ArrivalAssessment
        ArrivalAssessment: Final site verification [DAT]/[ACC]
        ArrivalAssessment --> EntranceClearing
        EntranceClearing: Site preparation [SUB]
        EntranceClearing --> CombConstruction
        CombConstruction: Wax production [SUB]/[STIG]
        CombConstruction --> QueenEggLaying
        QueenEggLaying: Colony reproduction [GEN]/[CAST]
        QueenEggLaying --> [*]
    }
    
    QuiescentState --> QueenAssessment: Overcrowding trigger
    QueenAssessment --> ScoutActivation: Queen preparation
    ScoutActivation --> ScoutForaging: Initiate search
    ScoutForaging --> ConsensusBuilding: Multiple sites found
    ConsensusBuilding --> SwarmPreparation: Decision made
    SwarmPreparation --> SwarmMovement: Preparation complete
    SwarmMovement --> NewNestEstablishment: Arrival at chosen site
    NewNestEstablishment --> QuiescentState: Colony established
    
    note right of QueenAssessment
        Queen reduces egg-laying
        and loses weight for flight
    end note
    
    note right of ConsensusBuilding
        Democratic decision process
        with cross-inhibition and
        quorum sensing
    end note
    
    note right of SwarmMovement
        Collective navigation with
        distributed intelligence
    end note
```

## 8. Free Energy Principle in Insect Collective Intelligence

```mermaid
flowchart TD
    subgraph FreeEnergy["Free Energy Principle in Colony Intelligence"]
        direction TB
        
        subgraph Individual["Individual Level Processing"]
            Sensory["Sensory Input<br>[DAT]<br><small>Environmental sampling</small>"]
            Model["Individual Model<br>[NOM]/[ACC]<br><small>Local predictions</small>"]
            Action["Individual Action<br>[GEN]<br><small>Environment manipulation</small>"]
            Error["Prediction Error<br>[ACC]<br><small>Surprise minimization</small>"]
            
            Sensory -->|"Updates"| Model
            Model -->|"Generates"| Action
            Action -->|"Affects"| Sensory
            Sensory -->|"Produces"| Error
            Error -->|"Updates"| Model
        end
        
        subgraph Local["Local Coordination"]
            DirectComm["Direct Communication<br>[PHE]<br><small>Antennation, trophallaxis</small>"]
            LocalRules["Local Interaction Rules<br>[INS]<br><small>Response thresholds</small>"]
            SpatialPatterns["Spatial Self-Organization<br>[LOC]<br><small>Emergence without central control</small>"]
            
            DirectComm -->|"Establishes"| LocalRules
            LocalRules -->|"Creates"| SpatialPatterns
            SpatialPatterns -->|"Constrains"| DirectComm
        end
        
        subgraph Colony["Colony-Level Emergence"]
            ColonyModel["Colony Generative Model<br>[SWARM]<br><small>Global predictive system</small>"]
            FreeEnergyMin["Collective Free Energy Minimization<br>[SWARM]<br><small>Surprise reduction at colony scale</small>"]
            Homeostasis["Colony Homeostasis<br>[SWARM]<br><small>Stabilized collective state</small>"]
            AdaptiveBehavior["Adaptive Colony Behavior<br>[SWARM]<br><small>Coordinated response</small>"]
            
            ColonyModel -->|"Shapes"| FreeEnergyMin
            FreeEnergyMin -->|"Produces"| Homeostasis
            Homeostasis -->|"Enables"| AdaptiveBehavior
            AdaptiveBehavior -->|"Updates"| ColonyModel
        end
        
        subgraph Environment["Environmental Coupling"]
            EnvState["Environmental State<br>[LOC]<br><small>Resources, threats, conditions</small>"]
            EnvFeedback["Environmental Feedback<br>[DAT]<br><small>Response to colony actions</small>"]
            ExtendedPhenotype["Extended Phenotype<br>[SUB]/[STIG]<br><small>Nest as embodied model</small>"]
            
            EnvState -->|"Provides"| EnvFeedback
            EnvFeedback -->|"Shapes"| ExtendedPhenotype
            ExtendedPhenotype -->|"Modifies"| EnvState
        end
    end
    
    %% Cross-level connections
    Model -.->|"Contributes to"| ColonyModel
    Action -.->|"Generates"| DirectComm
    DirectComm -.->|"Modulates"| Error
    
    SpatialPatterns -.->|"Emerges as"| FreeEnergyMin
    LocalRules -.->|"Constraints on"| AdaptiveBehavior
    
    Action -->|"Collective effect"| EnvFeedback
    EnvState -->|"Perceived by"| Sensory
    ExtendedPhenotype -.->|"Implements"| ColonyModel
    Homeostasis -.->|"Maintains"| ExtendedPhenotype
    
    classDef individual fill:#e6f3ff,stroke:#4682b4,stroke-width:2px,color:#00008B
    classDef local fill:#ffe6e6,stroke:#ff9999,stroke-width:2px,color:#8B0000
    classDef colony fill:#e6ffe6,stroke:#99cc99,stroke-width:2px,color:#006600
    classDef environment fill:#fff5e6,stroke:#ffb347,stroke-width:2px,color:#663300
    
    class Sensory,Model,Action,Error,Individual individual
    class DirectComm,LocalRules,SpatialPatterns,Local local
    class ColonyModel,FreeEnergyMin,Homeostasis,AdaptiveBehavior,Colony colony
    class EnvState,EnvFeedback,ExtendedPhenotype,Environment environment
``` 