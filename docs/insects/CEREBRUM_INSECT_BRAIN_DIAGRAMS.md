# CEREBRUM Insect Brain Anatomical Diagrams

This document provides detailed anatomical diagrams of insect brain structures mapped to the CEREBRUM case framework, illustrating neural circuit organization and functional relationships.

## Mushroom Bodies [ACC]

The Mushroom Bodies primarily implement the [ACC] (Accusative) case, specializing in associative learning and memory formation.

```mermaid
flowchart TD
    subgraph "Mushroom Body [ACC]"
        subgraph "Input Region: Calyx"
            PN[Projection Neurons] --> KC1[Kenyon Cells - Class I]
            PN --> KC2[Kenyon Cells - Class II]
            MBIN1[Modulatory Input - Octopaminergic] -.-> Calyx[Calyx]
            MBIN2[Modulatory Input - Dopaminergic] -.-> Calyx
            Calyx --> KC1
            Calyx --> KC2
        end
        
        subgraph "Intrinsic Neurons: Kenyon Cells"
            KC1 --> alpha[α Lobe Axons]
            KC1 --> beta[β Lobe Axons]
            KC2 --> gamma[γ Lobe Axons]
        end
        
        subgraph "Output Region: Vertical and Medial Lobes"
            alpha --> MBON1[MBON - Feedforward]
            beta --> MBON2[MBON - Inhibitory]
            gamma --> MBON3[MBON - Recurrent]
            
            MBIN3[Dopaminergic Neurons - α1] -.-> alpha
            MBIN4[Dopaminergic Neurons - β1] -.-> beta
            MBIN5[Dopaminergic Neurons - γ1] -.-> gamma
        end
    end
    
    Sensory[Sensory Input\n[DAT]] --> PN
    MBON1 --> Motor[Motor Output\n[NOM]/[GEN]]
    MBON2 --> Inhibition[Response Inhibition\n[NOM]]
    MBON3 --> Feedback[Feedback Loop\n[ACC]]
    
    classDef primary fill:#f9f,stroke:#333,stroke-width:2px
    classDef secondary fill:#bbf,stroke:#333,stroke-width:1px
    classDef tertiary fill:#fbb,stroke:#333,stroke-width:1px
    classDef calyx fill:#fff,stroke:#333,stroke-width:1px,stroke-dasharray: 5 5
    
    class KC1,KC2,alpha,beta,gamma primary
    class PN,MBON1,MBON2,MBON3 secondary
    class MBIN1,MBIN2,MBIN3,MBIN4,MBIN5 tertiary
    class Calyx calyx
```

## Central Complex [NOM]

The Central Complex primarily implements the [NOM] (Nominative) case, responsible for action selection, navigation, and motor pattern generation.

```mermaid
flowchart TD
    subgraph "Central Complex [NOM]"
        subgraph "Fan-shaped Body (FB)"
            FB1[FB Layer 1] --- FB2[FB Layer 2]
            FB2 --- FB3[FB Layer 3]
            FB3 --- FB4[FB Layer 4]
            FB4 --- FB5[FB Layer 5]
            FB5 --- FB6[FB Layer 6]
            FB6 --- FB7[FB Layer 7]
            FB7 --- FB8[FB Layer 8]
            
            PFN[PB-FB-Noduli Neurons] --> FB2
            CL1[CL1 Neurons] --> FB5
            ExFB[ExFB Neurons] --> FB1
        end
        
        subgraph "Ellipsoid Body (EB) [LOC]"
            EBr1[EB Region 1] --- EBr2[EB Region 2]
            EBr2 --- EBr3[EB Region 3]
            EBr3 --- EBr4[EB Region 4]
            
            EPG[E-PG Neurons] --> EBr1
            EIP[EIP Neurons] --> EBr2
            EB_NO[EB-NO Neurons] -.-> EBr3
        end
        
        subgraph "Protocerebral Bridge (PB) [INS]"
            PBL[PB Left Glomeruli] --- PBR[PB Right Glomeruli]
            
            PEN[P-EN Neurons] --> PBL
            PEG[P-EG Neurons] --> PBR
        end
        
        subgraph "Noduli (NO)"
            NO1[Noduli Layer 1]
            NO2[Noduli Layer 2]
            NO3[Noduli Layer 3]
            
            PFN --> NO1
            EB_NO --> NO2
        end
        
        EPG --> PEG
        PEN -.-> FB3
        NO2 -.-> FB4
    end
    
    Visual[Visual Input\n[DAT]] --> EPG
    Visual --> PEN
    EPG --> EBr1
    PEN --> PBL
    FB5 --> Motor[Motor Command\n[GEN]]
    FB8 --> HigherOrder[Higher Order Processing\n[ACC]]
    
    classDef nom fill:#f99,stroke:#333,stroke-width:2px
    classDef loc fill:#9f9,stroke:#333,stroke-width:2px
    classDef ins fill:#99f,stroke:#333,stroke-width:2px
    classDef gen fill:#ff9,stroke:#333,stroke-width:1px
    classDef dat fill:#9ff,stroke:#333,stroke-width:2px
    classDef acc fill:#f9f,stroke:#333,stroke-width:2px
    
    class FB1,FB2,FB3,FB4,FB5,FB6,FB7,FB8,ExFB,CL1 nom
    class EBr1,EBr2,EBr3,EBr4,EPG,EIP,EB_NO loc
    class PBL,PBR,PEN,PEG,PFN ins
    class NO1,NO2,NO3 gen
    class Visual dat
    class HigherOrder acc
```

## Antennal Lobe [DAT]

The Antennal Lobe primarily implements the [DAT] (Dative) case, processing olfactory information from the antennae.

```mermaid
flowchart TD
    subgraph "Antennal Lobe [DAT]"
        subgraph "Sensory Input"
            ORN1[ORN Type 1] --> GL1[Glomerulus 1]
            ORN2[ORN Type 2] --> GL2[Glomerulus 2]
            ORN3[ORN Type 3] --> GL3[Glomerulus 3]
            ORN4[ORN Type 4] --> GL4[Glomerulus 4]
            ORNn[ORN Type n] --> GLn[Glomerulus n]
        end
        
        subgraph "Glomerular Processing"
            GL1 --- GL2
            GL2 --- GL3
            GL3 --- GL4
            GL4 -..- GLn
        end
        
        subgraph "Local Processing"
            LN1[Excitatory LN] -.-> GL1
            LN1 -.-> GL2
            LN2[Inhibitory LN] -.-> GL3
            LN2 -.-> GL4
            LN3[Multiglomerular LN] -.-> GLn
        end
        
        subgraph "Output Pathways"
            GL1 --> uPN1[uniglomerular PN]
            GL2 --> uPN2[uniglomerular PN]
            GL3 --> mPN1[multiglomerular PN]
            GL4 --> mPN2[multiglomerular PN]
            GLn --> mPN3[multiglomerular PN]
        end
        
        subgraph "Modulatory Input"
            SER[Serotonergic] -.-> LN1
            OCT[Octopaminergic] -.-> LN2
            DA[Dopaminergic] -.-> LN3
        end
    end
    
    Odor[Odor Molecules] --> ORN1
    Odor --> ORN2
    Odor --> ORN3
    Odor --> ORN4
    Odor --> ORNn
    
    uPN1 --> MB_Calyx[Mushroom Body Calyx\n[ACC]]
    uPN2 --> MB_Calyx
    uPN1 --> LH[Lateral Horn\n[NOM]]
    uPN2 --> LH
    mPN1 --> LH
    mPN2 --> VLPR[Ventrolateral Protocerebrum\n[PHE]]
    mPN3 --> SLP[Superior Lateral Protocerebrum\n[INS]]
    
    classDef dat fill:#9ff,stroke:#333,stroke-width:2px
    classDef local fill:#ff9,stroke:#333,stroke-width:1px
    classDef output fill:#f9f,stroke:#333,stroke-width:1px
    classDef mod fill:#bbf,stroke:#333,stroke-width:1px
    classDef phe fill:#f6c,stroke:#333,stroke-width:1px
    classDef acc fill:#f9f,stroke:#333,stroke-width:2px
    classDef nom fill:#f99,stroke:#333,stroke-width:2px
    classDef ins fill:#99f,stroke:#333,stroke-width:2px
    
    class GL1,GL2,GL3,GL4,GLn dat
    class LN1,LN2,LN3,ORN1,ORN2,ORN3,ORN4,ORNn local
    class uPN1,uPN2,mPN1,mPN2,mPN3 output
    class SER,OCT,DA mod
    class MB_Calyx acc
    class LH nom
    class VLPR phe
    class SLP ins
```

## Optic Lobe [DAT]

The Optic Lobe primarily implements the [DAT] (Dative) case for visual information processing.

```mermaid
flowchart TD
    subgraph "Optic Lobe [DAT]"
        subgraph "Retina"
            R1[R1-6 Photoreceptors] --> LA[Lamina]
            R7[R7 Photoreceptors] --> ME[Medulla]
            R8[R8 Photoreceptors] --> ME
        end
        
        subgraph "Lamina"
            LA --> L1[L1 Monopolar Cell]
            LA --> L2[L2 Monopolar Cell]
            LA --> L3[L3 Monopolar Cell]
            LA --> L4[L4 Monopolar Cell]
            LA --> L5[L5 Monopolar Cell]
        end
        
        subgraph "Medulla"
            L1 --> Mi1[Mi1 Neuron]
            L1 --> Tm3[Tm3 Neuron]
            L2 --> Tm1[Tm1 Neuron]
            L2 --> Tm2[Tm2 Neuron]
            L3 --> Tm9[Tm9 Neuron]
            L4 --> Tm4[Tm4 Neuron]
            L5 --> Tm5[Tm5 Neuron]
            
            Mi1 --> T4[T4 Neuron]
            Tm1 --> T5[T5 Neuron]
            Tm2 --> T5
            Tm3 --> T4
        end
        
        subgraph "Lobula"
            T4 --> LPa[Lobula Plate - Direction Selective]
            T5 --> LPb[Lobula Plate - Direction Selective]
            
            LCa[Lobula Columnar Type a] --> VLPR[Ventral Lateral Protocerebrum]
            LCb[Lobula Columnar Type b] --> AOTU[Anterior Optic Tubercle]
        end
        
        subgraph "Lobula Plate"
            LPa --> HSN[Horizontal System North]
            LPa --> HSE[Horizontal System Equatorial]
            LPa --> HSS[Horizontal System South]
            
            LPb --> VS1[Vertical System 1]
            LPb --> VS2[Vertical System 2]
            LPb --> VS3[Vertical System 3]
        end
    end
    
    Vision[Visual Input] --> R1
    Vision --> R7
    Vision --> R8
    
    HSN --> PVLP[Posterior Ventrolateral Protocerebrum\n[LOC]]
    HSE --> PVLP
    HSS --> PVLP
    
    VS1 --> CX[Central Complex\n[NOM]/[LOC]]
    VS2 --> CX
    VS3 --> CX
    
    VLPR --> MB[Mushroom Bodies\n[ACC]]
    AOTU --> BU[Bulb\n[LOC]]
    BU --> EB[Ellipsoid Body\n[LOC]]
    
    classDef retina fill:#fbb,stroke:#333,stroke-width:1px
    classDef lamina fill:#fbf,stroke:#333,stroke-width:1px
    classDef medulla fill:#bff,stroke:#333,stroke-width:2px
    classDef lobula fill:#bbf,stroke:#333,stroke-width:1px
    classDef plate fill:#9f9,stroke:#333,stroke-width:1px
    classDef dat fill:#9ff,stroke:#333,stroke-width:2px
    classDef loc fill:#9f9,stroke:#333,stroke-width:2px
    classDef nom fill:#f99,stroke:#333,stroke-width:2px
    classDef acc fill:#f9f,stroke:#333,stroke-width:2px
    
    class R1,R7,R8 retina
    class LA,L1,L2,L3,L4,L5 lamina
    class ME,Mi1,Tm1,Tm2,Tm3,Tm4,Tm5,Tm9,T4,T5 medulla
    class LCa,LCb,VLPR lobula
    class LPa,LPb,HSN,HSE,HSS,VS1,VS2,VS3 plate
    class Vision dat
    class PVLP,BU,EB,AOTU loc
    class CX nom
    class MB acc
```

## Brain-Wide Integration and CEREBRUM Case Implementation

This diagram demonstrates cross-structure integration and information flow across CEREBRUM cases in the insect brain.

```mermaid
flowchart TD
    subgraph "Sensory Input [DAT]"
        OL[Optic Lobe] --> VisualFeatures[Visual Feature Extraction]
        AL[Antennal Lobe] --> OlfactoryFeatures[Olfactory Feature Extraction]
        GNG[Gnathal Ganglia] --> GustatoryFeatures[Gustatory Processing]
        MC[Mechanosensory Centers] --> MechanicalFeatures[Mechanical Feature Extraction]
    end
    
    subgraph "Memory Systems [ACC]"
        VisualFeatures --> MB_Visual[MB Visual Input Region]
        OlfactoryFeatures --> MB_Olfactory[MB Olfactory Input Region]
        
        MB_Visual --> KCs[Kenyon Cells]
        MB_Olfactory --> KCs
        
        KCs --> V_Lobe[Vertical Lobe Output]
        KCs --> M_Lobe[Medial Lobe Output]
        
        DA[Dopaminergic Input] -.-> KCs
        OA[Octopaminergic Input] -.-> KCs
    end
    
    subgraph "Navigation [LOC]"
        VisualFeatures --> AOTU[Anterior Optic Tubercle]
        AOTU --> BU[Bulb]
        BU --> EB[Ellipsoid Body]
        
        EB --> PB[Protocerebral Bridge]
        PB --> EB
        
        HeadDir[Head Direction Cells] --> EB
        MechanicalFeatures -.-> HeadDir
    end
    
    subgraph "Action Selection [NOM]"
        V_Lobe --> SMP[Superior Medial Protocerebrum]
        M_Lobe --> SMP
        EB --> FB[Fan-shaped Body]
        
        FB --> SMP
        SMP --> DescNeurons[Descending Neurons]
        
        LAL[Lateral Accessory Lobe] --> DescNeurons
        LAL --> FB
        FB --> LAL
        
        SER[Serotonergic Input] -.-> FB
        GustatoryFeatures -.-> SMP
    end
    
    subgraph "Motor Control [GEN]"
        DescNeurons --> VNC[Ventral Nerve Cord]
        VNC --> Muscles[Muscles]
        
        SEG[Subesophageal Ganglion] --> VNC
    end
    
    subgraph "Modulation [MET]"
        PI[Pars Intercerebralis] -.-> DA
        PI -.-> OA
        PI -.-> SER
        
        CA[Corpora Allata] -.-> Hormones[Hormonal Signaling]
        Hormones -.-> V_Lobe
        Hormones -.-> FB
        Hormones -.-> VNC
    end
    
    SMP -.-> PI
    
    classDef dat fill:#9ff,stroke:#333,stroke-width:2px,color:#000
    classDef acc fill:#f9f,stroke:#333,stroke-width:2px,color:#000
    classDef loc fill:#9f9,stroke:#333,stroke-width:2px,color:#000
    classDef nom fill:#f99,stroke:#333,stroke-width:2px,color:#000
    classDef gen fill:#ff9,stroke:#333,stroke-width:2px,color:#000
    classDef met fill:#bbf,stroke:#333,stroke-width:2px,color:#000
    
    class OL,AL,GNG,MC,VisualFeatures,OlfactoryFeatures,GustatoryFeatures,MechanicalFeatures dat
    class MB_Visual,MB_Olfactory,KCs,V_Lobe,M_Lobe acc
    class AOTU,BU,EB,PB,HeadDir loc 
    class SMP,FB,LAL,DescNeurons nom
    class VNC,SEG,Muscles gen
    class PI,CA,DA,OA,SER,Hormones met
```

These diagrams illustrate the complex structural organization of major insect brain regions within the CEREBRUM case framework, highlighting neural circuit implementations that enable cognitive processing. The diagrams emphasize both anatomical organization and functional connectivity between regions, showing how information flows through different CEREBRUM cases to produce coordinated behavior. 