# Figure 14: Case-Specific Message Passing in Active Inference

```mermaid
flowchart TB
    %% Title with description
    title["Message Passing Mechanisms under Different Case Assignments"]
    
    subgraph HierarchicalProcessing["Hierarchical Message Passing Framework"]
        direction TB
        
        subgraph Level1["Higher Hierarchical Level"]
            L1_Pred["Predictions μ¹<br><small>High-level expected states</small>"]
            L1_Error["Errors ε¹<br><small>High-level surprise</small>"]
        end
        
        subgraph Level0["Lower Hierarchical Level"]
            L0_Pred["Predictions μ⁰<br><small>Low-level expected observations</small>"]
            L0_Error["Errors ε⁰<br><small>Low-level surprise</small>"]
        end
        
        subgraph CaseRules["Case-Specific Update Rules"]
            direction TB
            NOMRule["[NOM] Nominative Case Rule<br>μ¹ → μ⁰<br><small>• Active prediction generation</small><br><small>• Top-down information flow</small><br><small>• Prior distribution propagation</small>"]
            ACCRule["[ACC] Accusative Case Rule<br>ε⁰ → ε¹<br><small>• Error propagation mechanism</small><br><small>• Bottom-up information flow</small><br><small>• Parameter update target</small>"]
            DATRule["[DAT] Dative Case Rule<br>data → μ⁰<br><small>• External data reception</small><br><small>• Input processing pathways</small><br><small>• Sensory information handling</small>"]
            GENRule["[GEN] Genitive Case Rule<br>μ⁰ → output<br><small>• Model output generation</small><br><small>• Product creation</small><br><small>• Response formulation</small>"]
            INSRule["[INS] Instrumental Case Rule<br>process(μ¹,ε⁰)<br><small>• Computational methods</small><br><small>• Transformation functions</small><br><small>• Processing techniques</small>"]
            VOCRule["[VOC] Vocative Case Rule<br>address → activation<br><small>• Direct model invocation</small><br><small>• Named reference activation</small><br><small>• Command interface</small>"]
        end

        %% External data flows
        EXT_DATA["External Data<br><small>Environmental information</small>"]
        OUTPUT["System Output<br><small>Actions & responses</small>"]
        
        %% Connect external elements
        EXT_DATA -->|"Input"| DATRule
        GENRule -->|"Output"| OUTPUT
    end
    
    %% Message passing connections with labels
    L1_Pred -->|"Top-down predictions<br>Prior expectations"| L0_Pred
    L0_Error -->|"Bottom-up errors<br>Surprise signals"| L1_Error
    
    %% Case modulation connections with explicit descriptions
    NOMRule -.->|"Drives<br>predictions"| L1_Pred
    ACCRule -.->|"Processes<br>errors"| L0_Error
    DATRule -.->|"Receives<br>inputs"| L0_Pred
    GENRule -.->|"Generates<br>outputs"| L0_Pred
    INSRule -.->|"Implements<br>methods"| L1_Error
    VOCRule -.->|"Activates<br>models"| L0_Pred
    
    %% Enhanced styling with better accessibility
    classDef title fill:none,stroke:none,color:#000066,font-size:18px,font-weight:bold
    classDef level fill:#E6F3FF,stroke:#87CEFA,stroke-width:2px,color:#000066,font-weight:bold
    classDef node fill:#F8F8F8,stroke:#DDDDDD,stroke-width:1px,color:#333333
    classDef rule fill:#FFF0F0,stroke:#FFCCCC,stroke-width:1px,color:#8B0000
    classDef external fill:#F0FFF0,stroke:#99cc99,stroke-width:2px,color:#006600
    classDef framework fill:#f9f9ff,stroke:#9999ff,stroke-width:3px,color:#000066,font-weight:bold
    
    class title title
    class Level0,Level1 level
    class L0_Pred,L0_Error,L1_Pred,L1_Error node
    class NOMRule,ACCRule,DATRule,GENRule,INSRule,VOCRule rule
    class EXT_DATA,OUTPUT external
    class HierarchicalProcessing framework
```

Figure 14. Case-Specific Message Passing in Active Inference. This diagram details the specific message passing mechanisms that implement case transformations within CEREBRUM's active inference framework. The central component shows the hierarchical bidirectional message passing that characterizes active inference, with top-down predictions (μ¹) flowing from higher to lower levels and bottom-up prediction errors (ε⁰) propagating from lower to higher levels. The Case Rules section specifies how each case modulates these message flows: Nominative case [NOM] governs top-down prediction generation (μ¹ → μ⁰); Accusative case [ACC] handles bottom-up error propagation (ε⁰ → ε¹); Dative case [DAT] manages incoming data reception (data → μ⁰); Genitive case [GEN] controls output generation (μ⁰ → output); Instrumental case [INS] implements processing functions that operate on both predictions and errors (process(μ¹,ε⁰)); and Vocative case [VOC] manages direct activation through addressing (address → activation). These case-specific update rules create a functional specialization while maintaining the core active inference principles of prediction error minimization. By formalizing message passing in terms of case-specific operations, CEREBRUM provides a precise mathematical framework for implementing case transformations as precision-weighted Bayesian updates within hierarchical generative models. This approach connects linguistic case semantics directly to computational message-passing algorithms, creating a principled foundation for model interactions in complex cognitive systems.

