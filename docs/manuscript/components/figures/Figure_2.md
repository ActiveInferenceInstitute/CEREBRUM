# Figure 2: Case Relationships - Model and Linguistic Parallels

```mermaid
graph TB
    %% Linguistic Examples on top
    LNOM["The cat chases (Subject)<br><small>Actor performing action</small>"]
    LACC["Chases the cat (Direct Object)<br><small>Recipient of action</small>"]
    LDAT["Gives to the cat (Indirect Object)<br><small>Secondary recipient</small>"]
    LGEN["The cat's toy (Possessor)<br><small>Owner or source</small>"]
    LINS["With the cat (Tool/Means)<br><small>Instrument for action</small>"]
    LLOC["On the cat (Location)<br><small>Positional context</small>"]
    LABL["From the cat (Source)<br><small>Origin point</small>"]
    LVOC["Hey cat! (Direct Address)<br><small>Entity being called</small>"]
    
    %% Cases in center
    NOM["Nominative [NOM]<br><small>Subject/Agent</small>"]
    ACC["Accusative [ACC]<br><small>Direct Object</small>"]
    DAT["Dative [DAT]<br><small>Recipient/Goal</small>"]
    GEN["Genitive [GEN]<br><small>Source/Possessor</small>"]
    INS["Instrumental [INS]<br><small>Tool/Method</small>"]
    LOC["Locative [LOC]<br><small>Context/Environment</small>"]
    ABL["Ablative [ABL]<br><small>Origin/Cause</small>"]
    VOC["Vocative [VOC]<br><small>Addressable Entity</small>"]
    
    %% Model Examples on bottom
    MNOM["Makes predictions (Active Agent)<br><small>Generates outputs autonomously</small>"]
    MACC["Receives updates (Object)<br><small>Target of transformations</small>"]
    MDAT["Receives data (Recipient)<br><small>Destination for inputs</small>"]
    MGEN["Produces outputs (Source)<br><small>Origin of intelligence products</small>"]
    MINS["Implements methods (Tool)<br><small>Execution mechanism</small>"]
    MLOC["Provides context (Environment)<br><small>Situational parameters</small>"]
    MABL["Tracks history (Origin)<br><small>Source of causal information</small>"]
    MVOC["Responds to name<br>(Addressable)<br><small>Direct command interface</small>"]
    
    %% Connect examples to their respective cases with labeled edges
    LNOM -->|"Maps to"| NOM -->|"Implemented as"| MNOM
    LACC -->|"Maps to"| ACC -->|"Implemented as"| MACC
    LDAT -->|"Maps to"| DAT -->|"Implemented as"| MDAT
    LGEN -->|"Maps to"| GEN -->|"Implemented as"| MGEN
    LINS -->|"Maps to"| INS -->|"Implemented as"| MINS
    LLOC -->|"Maps to"| LOC -->|"Implemented as"| MLOC
    LABL -->|"Maps to"| ABL -->|"Implemented as"| MABL
    LVOC -->|"Maps to"| VOC -->|"Implemented as"| MVOC
    
    %% Create the horizontal connection only between cases with relationship types
    NOM ---|"Subject-Object"| ACC
    ACC ---|"Object-Recipient"| DAT
    DAT ---|"Recipient-Source"| GEN
    GEN ---|"Source-Means"| INS
    INS ---|"Means-Location"| LOC
    LOC ---|"Location-Origin"| ABL
    ABL ---|"Origin-Address"| VOC
    
    %% Subgraph labels
    Lang["Linguistic Examples<br><small>(Natural Language)</small>"]
    CS["Case System<br><small>(Grammatical Framework)</small>"]
    Mod["Model Examples<br><small>(Computational Implementation)</small>"]
    
    %% Position the labels (invisible connections to group them)
    Lang -.- LNOM
    Lang -.- LVOC
    CS -.- NOM
    CS -.- VOC
    Mod -.- MNOM
    Mod -.- MVOC
    
    %% Styling with distinct colors and improved contrast
    classDef case fill:#e6f2ff,stroke:#0066cc,stroke-width:2px,color:#000066,font-weight:bold
    classDef lang fill:#ffe6e6,stroke:#cc0000,stroke-width:2px,color:#660000
    classDef model fill:#e6ffe6,stroke:#009900,stroke-width:2px,color:#006600
    classDef label fill:none,stroke:none,color:#333333,font-weight:bold
    
    class NOM,ACC,DAT,GEN,INS,LOC,ABL,VOC case
    class LNOM,LACC,LDAT,LGEN,LINS,LLOC,LABL,LVOC lang
    class MNOM,MACC,MDAT,MGEN,MINS,MLOC,MABL,MVOC model
    class Lang,CS,Mod label
```

Figure 2. Case Relationships - Model and Linguistic Parallels. This figure illustrates the direct mapping between linguistic case systems and cognitive model relationships in CEREBRUM. The top row presents everyday linguistic examples of each case in English (though English largely expresses cases through word order and prepositions rather than morphological markers). The middle row shows the eight fundamental cases with their standard abbreviations. The bottom row demonstrates how these same case relationships apply to cognitive models within the CEREBRUM framework. For example, just as "the cat" functions as the subject (Nominative case) in language, a model in Nominative case functions as an active agent making predictions. This systematic parallel between linguistic structure and model relationships provides a principled foundation for understanding how models can assume different functional roles while maintaining their core identity, similar to how a noun retains its meaning while its form changes according to its grammatical function.
