# Hindi Case System and CEREBRUM Mapping

## Overview of Hindi's Case System

Hindi is an Indo-Aryan language with a relatively simplified case system compared to Sanskrit, its classical ancestor. While Sanskrit had eight distinct morphological cases, modern Hindi has evolved to use a combination of direct and oblique noun forms plus postpositions to express grammatical relationships. This two-case system (direct and oblique) supplemented by postpositions creates a rich functional equivalent to the more morphologically complex case systems of other languages.

Hindi's approach represents an interesting middle ground between highly inflected languages like Sanskrit or Russian and analytic languages like English. This makes it particularly valuable for CEREBRUM implementations in environments where a balance between morphological simplicity and functional expressiveness is desired.

## Case Inventory in Hindi

Hindi utilizes the following case structure:

1. **Direct Case (कर्ता कारक)** - Used for subjects of intransitive verbs and objects of certain verbs
   - No explicit marking (zero morpheme) in singular
   - Marked with "-े" (-e) or "-ए" (-ye) in masculine plurals

2. **Oblique Case (विभक्ति आधार)** - Base form used before postpositions
   - Marked with "-े" (-e) in masculine singular
   - Marked with "-ों" (-oṃ) in plurals

3. **Postpositional Case Marking:**
   - **ने (ne)** - Ergative marker for subjects of transitive verbs in perfective aspect
   - **को (ko)** - Dative/Accusative marker for recipients and specific direct objects
   - **का/के/की (kā/ke/kī)** - Genitive marker (agrees with possessed noun)
   - **में (meṃ)** - Locative marker indicating "in, inside"
   - **पर (par)** - Locative marker indicating "on, upon"
   - **से (se)** - Instrumental/Ablative marker for instruments, sources, and paths
   - **तक (tak)** - Terminative marker indicating "until, up to"

4. **Vocative Case** - Used for direct address
   - Often marked with special intonation rather than morphology
   - Sometimes marked with "हे" (he) or "अरे" (are) before the noun

## Mapping to CEREBRUM Cases

Hindi's case system maps to CEREBRUM's eight standard cases as follows:

| CEREBRUM Case | Hindi Equivalent | Implementation Notes |
|---------------|------------------|----------------------|
| **[NOM]** Nominative | Direct case (unmarked) or Ergative (ने/ne) | Differentiated by verb transitivity and aspect; models in [NOM] should implement ergative-absolutive logic for perfective actions |
| **[ACC]** Accusative | Direct case (unmarked) or Dative-Accusative (को/ko) | Differentiation based on specificity; models in [ACC] should implement animacy and specificity detection |
| **[GEN]** Genitive | Genitive (का/के/की kā/ke/kī) | Models in [GEN] must implement agreement with the possessed noun; three-way agreement system |
| **[DAT]** Dative | Dative-Accusative (को/ko) | Models in [DAT] should differentiate from [ACC] through semantic role detection |
| **[INS]** Instrumental | Instrumental-Ablative (से/se) | Models in [INS] should implement contextual disambiguation from ablative functions |
| **[LOC]** Locative | Locative (में/meṃ, पर/par) | Models in [LOC] should implement containment vs. surface distinction |
| **[ABL]** Ablative | Instrumental-Ablative (से/se) | Models in [ABL] should implement source/origin semantics distinct from instrumental |
| **[VOC]** Vocative | Vocative (हे/he, अरे/are) | Models in [VOC] should implement direct address recognition patterns |

## Unique Features

Hindi's case system offers several unique features relevant to CEREBRUM:

1. **Ergative-Absolutive Alignment in Perfective Aspect**

   Hindi displays split ergativity, where the alignment system changes based on aspect. In perfective aspect with transitive verbs, the subject takes ergative marking (ने/ne), while in imperfective aspect, nominative-accusative alignment is used. This aspect-driven case marking provides a model for CEREBRUM implementations where functional roles might shift based on temporal factors or completion status.

   ```
   राम ने किताब पढ़ी। (Rām ne kitāb paṛhī)
   Ram ERG book read.PFV.F
   "Ram read the book."
   
   राम किताब पढ़ता है। (Rām kitāb paṛhtā hai)
   Ram.NOM book read.IMPFV.M be.PRES
   "Ram reads books."
   ```

2. **Differential Object Marking**

   Hindi marks direct objects with को (ko) based on specificity and animacy, leaving less specific or inanimate objects unmarked. This provides a model for CEREBRUM implementations where accusative models have variable marking based on contextual significance.

3. **Genitive Agreement System**

   Hindi's genitive postposition agrees with the possessed noun (का/kā for masculine singular, के/ke for masculine plural/oblique, की/kī for feminine), creating an agreement chain that preserves gender and number information across the possessive relationship. This offers a model for CEREBRUM's [GEN] case implementations where multiple agreement features must be preserved across relationships.

4. **Postpositional Compound Expressions**

   Hindi uses compound postpositions to express complex spatial and temporal relations (के ऊपर/ke ūpar - "on top of", के बाद/ke bād - "after"). This provides patterns for extending CEREBRUM's case system with compound case expressions.

## Extension Opportunities

Hindi's case system suggests several extension opportunities for CEREBRUM:

1. **Aspect-Sensitive Case Assignment**

   Inspired by Hindi's split ergativity, CEREBRUM could implement aspect-sensitive case assignments where models change their case marking based on completion status, temporality, or other aspectual features.

2. **Animacy Hierarchy in Model Significance**

   Following Hindi's differential object marking, CEREBRUM could implement significance-based model marking where more central models receive explicit case marking while peripheral models use default marking.

3. **Agreement Propagation Chains**

   Based on Hindi's genitive agreement system, CEREBRUM could implement feature-propagation chains where relevant features of one model propagate through relationship markers to related models.

4. **Compound Case Expressions**

   Hindi's compound postpositions suggest extending CEREBRUM with combinatorial case expressions where primary cases combine with secondary specifiers to create more precise relationship descriptors.

5. **Classifier Integration**

   Though not part of the case system per se, Hindi's classifier system for counting objects could inspire integration of classifier logic in CEREBRUM's model handling, particularly for collection or array-type models.

## Example Sentences

Below are example sentences in Hindi with their CEREBRUM parallels:

1. **Nominative/Ergative [NOM]**

   **Hindi:** राम सोता है। (Rām sotā hai) - "Ram sleeps."
   **CEREBRUM:** Model_Ram[NOM] generates sleep state predictions.

   **Hindi:** राम ने खाना खाया। (Rām ne khānā khāyā) - "Ram ate food."
   **CEREBRUM:** Model_Ram[NOM:ergative] processes input data actively.

2. **Accusative [ACC]**

   **Hindi:** मैं फल खाता हूँ। (Maiṃ phal khātā hūṃ) - "I eat fruit."
   **CEREBRUM:** I[NOM] process Fruit_Model[ACC:unmarked].

   **Hindi:** मैं राम को देखता हूँ। (Maiṃ Rām ko dekhtā hūṃ) - "I see Ram."
   **CEREBRUM:** I[NOM] process Ram_Model[ACC:marked] with specificity weighting.

3. **Genitive [GEN]**

   **Hindi:** राम का घर (Rām kā ghar) - "Ram's house"
   **CEREBRUM:** House_Model derived from Ram_Model[GEN:masculine].

   **Hindi:** सीता की किताब (Sītā kī kitāb) - "Sita's book"
   **CEREBRUM:** Book_Model derived from Sita_Model[GEN:feminine].

4. **Dative [DAT]**

   **Hindi:** राम ने सीता को फल दिया। (Rām ne Sītā ko phal diyā) - "Ram gave fruit to Sita."
   **CEREBRUM:** Ram_Model[NOM] transfers data to Sita_Model[DAT].

5. **Instrumental [INS]**

   **Hindi:** राम ने चाबी से दरवाज़ा खोला। (Rām ne cābī se darvāzā kholā) - "Ram opened the door with a key."
   **CEREBRUM:** Ram_Model[NOM] utilizes Key_Model[INS] to transform Door_Model[ACC].

6. **Locative [LOC]**

   **Hindi:** किताब मेज़ पर है। (Kitāb mez par hai) - "The book is on the table."
   **CEREBRUM:** Book_Model exists within Table_Model[LOC:surface].

   **Hindi:** राम घर में है। (Rām ghar meṃ hai) - "Ram is in the house."
   **CEREBRUM:** Ram_Model exists within House_Model[LOC:container].

7. **Ablative [ABL]**

   **Hindi:** राम दिल्ली से आया। (Rām Dillī se āyā) - "Ram came from Delhi."
   **CEREBRUM:** Ram_Model originates from Delhi_Model[ABL].

8. **Vocative [VOC]**

   **Hindi:** हे राम! (He Rām!) - "O Ram!"
   **CEREBRUM:** Direct invocation of Ram_Model[VOC] for immediate attention.

These examples demonstrate how Hindi's case system can be systematically mapped to CEREBRUM's case framework, providing intuitive parallels for model relationships and transformations.

## Deeper Integration with CEREBRUM Concepts

Hindi grammar provides rich parallels and extensions for the CEREBRUM framework:

### a. Declinability and Split Ergativity

Hindi's split ergative system exemplifies **conditional declinability** where case marking changes based on aspectual features. A model (the noun) takes different case markers depending on whether the action is complete (perfective) or ongoing (imperfective). This directly mirrors how CEREBRUM models could assume different functional roles based on process state:

```python
from enum import Enum, auto
from dataclasses import dataclass
from typing import Optional, Dict, Any

class Aspect(Enum):
    """Aspectual distinctions inspired by Hindi verb system"""
    PERFECTIVE = auto()    # Completed action (triggers ergative)
    IMPERFECTIVE = auto()  # Ongoing action (nominative-accusative alignment)
    HABITUAL = auto()      # Habitual action
    PROGRESSIVE = auto()   # Currently in progress

class HindiInspiredCase(Enum):
    """Case system with Hindi-inspired ergative split"""
    NOM = auto()           # Nominative (unmarked subject)
    ERG = auto()           # Ergative (ने/ne marked subject)
    ACC = auto()           # Accusative (unmarked object)
    ACC_MARKED = auto()    # Accusative marked (को/ko for specific/animate)
    DAT = auto()           # Dative (को/ko for recipient)
    GEN_MASC = auto()      # Genitive masculine (का/kā)
    GEN_FEM = auto()       # Genitive feminine (की/kī)
    GEN_OBL = auto()       # Genitive oblique (के/ke)
    INS = auto()           # Instrumental (से/se as tool)
    ABL = auto()           # Ablative (से/se as source)
    LOC_IN = auto()        # Locative interior (में/meṃ)
    LOC_ON = auto()        # Locative surface (पर/par)
    VOC = auto()           # Vocative (हे/he, अरे/are)

@dataclass
class HindiCaseModifiers:
    """Modifiers inspired by Hindi postpositional features"""
    aspect: Aspect = Aspect.IMPERFECTIVE
    animacy: bool = False
    specificity: float = 0.5  # 0.0 = generic, 1.0 = highly specific
    gender: str = "masculine"  # masculine, feminine, neuter
    number: str = "singular"   # singular, plural

def get_subject_case(modifiers: HindiCaseModifiers, is_transitive: bool) -> HindiInspiredCase:
    """
    Determine subject case based on Hindi split ergativity rules.
    
    In Hindi:
    - Perfective + transitive → ergative (ने/ne)
    - Imperfective or intransitive → nominative (unmarked)
    """
    if modifiers.aspect == Aspect.PERFECTIVE and is_transitive:
        return HindiInspiredCase.ERG
    return HindiInspiredCase.NOM

def get_object_case(modifiers: HindiCaseModifiers) -> HindiInspiredCase:
    """
    Determine object case based on Hindi differential object marking.
    
    In Hindi:
    - Animate or specific objects → को/ko marked
    - Inanimate or generic objects → unmarked
    """
    if modifiers.animacy or modifiers.specificity > 0.7:
        return HindiInspiredCase.ACC_MARKED
    return HindiInspiredCase.ACC
```

### b. Active Inference and Differential Object Marking

Hindi's differential object marking (DOM) reflects a prediction about the **significance** of the object in the discourse. In CEREBRUM, this maps to precision weighting:

```python
class DifferentialObjectMarking:
    """
    Hindi-inspired differential marking based on animacy and specificity.
    
    Models the observation that Hindi marks objects with को (ko) when:
    1. The object is animate (higher animacy = higher precision)
    2. The object is specific/definite (higher specificity = higher precision)
    """
    
    ANIMACY_HIERARCHY = {
        "human": 1.0,
        "animal": 0.8,
        "animate": 0.6,
        "inanimate": 0.3,
        "abstract": 0.1
    }
    
    def __init__(self):
        self.precision_threshold = 0.6
    
    def calculate_precision(self, animacy_type: str, specificity: float) -> float:
        """Calculate precision weight for an object model"""
        animacy_weight = self.ANIMACY_HIERARCHY.get(animacy_type, 0.3)
        return (animacy_weight + specificity) / 2
    
    def should_mark_object(self, animacy_type: str, specificity: float) -> bool:
        """Determine if object should receive explicit case marking (को/ko)"""
        precision = self.calculate_precision(animacy_type, specificity)
        return precision > self.precision_threshold
    
    def get_case_with_precision(self, model, animacy_type: str, specificity: float):
        """
        Return appropriate case with precision weighting.
        
        High precision objects (marked with को):
        - Receive more attention in processing
        - Have higher influence on predictions
        - Are tracked more carefully in working memory
        """
        precision = self.calculate_precision(animacy_type, specificity)
        if self.should_mark_object(animacy_type, specificity):
            return {
                "case": HindiInspiredCase.ACC_MARKED,
                "precision": precision,
                "marked": True,
                "postposition": "को"
            }
        return {
            "case": HindiInspiredCase.ACC,
            "precision": precision,
            "marked": False,
            "postposition": None
        }

# Example usage
dom = DifferentialObjectMarking()

# "मैं राम को देखता हूँ" - I see Ram (specific, human)
ram_case = dom.get_case_with_precision("human", 0.9)
# Returns: {"case": ACC_MARKED, "precision": 0.95, "marked": True, "postposition": "को"}

# "मैं फल खाता हूँ" - I eat fruit (generic, inanimate)
fruit_case = dom.get_case_with_precision("inanimate", 0.3)
# Returns: {"case": ACC, "precision": 0.30, "marked": False, "postposition": None}
```

### c. Category Theory and Genitive Agreement

Hindi's genitive postposition demonstrates **morphism composition** with agreement features. The genitive marker (का/के/की) agrees with the possessed noun, creating a chain of feature propagation:

```python
class GenitiveAgreementChain:
    """
    Models Hindi genitive agreement where the postposition agrees
    with the possessed noun's gender and number.
    
    का (kā) - masculine singular
    के (ke) - masculine plural/oblique
    की (kī) - feminine (all numbers)
    """
    
    GENITIVE_FORMS = {
        ("masculine", "singular", "direct"): "का",
        ("masculine", "singular", "oblique"): "के",
        ("masculine", "plural", "direct"): "के",
        ("masculine", "plural", "oblique"): "के",
        ("feminine", "singular", "direct"): "की",
        ("feminine", "singular", "oblique"): "की",
        ("feminine", "plural", "direct"): "की",
        ("feminine", "plural", "oblique"): "की",
    }
    
    def __init__(self, possessor_model, possessed_model):
        self.possessor = possessor_model
        self.possessed = possessed_model
    
    def get_genitive_marker(self, possessed_gender: str, 
                            possessed_number: str, 
                            syntactic_position: str = "direct") -> str:
        """Get the appropriate genitive postposition form"""
        key = (possessed_gender, possessed_number, syntactic_position)
        return self.GENITIVE_FORMS.get(key, "का")
    
    def create_possession_relation(self, possessed_gender: str,
                                   possessed_number: str) -> Dict[str, Any]:
        """
        Create a genitive relationship with proper agreement.
        
        This models the morphism: Possessor[GEN] → Possessed
        where the genitive marker carries agreement features from Possessed
        """
        marker = self.get_genitive_marker(possessed_gender, possessed_number)
        return {
            "possessor": self.possessor,
            "possessed": self.possessed,
            "relation": "genitive",
            "marker": marker,
            "agreement_features": {
                "gender": possessed_gender,
                "number": possessed_number,
                "source": "possessed_noun"
            }
        }

# Example: राम का घर (Rām kā ghar) - Ram's house
# घर (ghar/house) is masculine singular, so "का" is used
relation1 = GenitiveAgreementChain("Ram_Model", "House_Model")
result1 = relation1.create_possession_relation("masculine", "singular")
# marker = "का"

# Example: सीता की किताब (Sītā kī kitāb) - Sita's book  
# किताब (kitāb/book) is feminine, so "की" is used
relation2 = GenitiveAgreementChain("Sita_Model", "Book_Model")
result2 = relation2.create_possession_relation("feminine", "singular")
# marker = "की"
```

### d. Morphosyntactic Alignment and CEREBRUM

Hindi's split ergativity provides a model for **dynamic alignment** in CEREBRUM:

| Alignment Type | Hindi Context | CEREBRUM Parallel |
|---------------|---------------|-------------------|
| **Nominative-Accusative** | Imperfective aspect | Standard model interactions |
| **Ergative-Absolutive** | Perfective aspect with transitive | Completed transformations |

```python
class AlignmentManager:
    """
    Manages morphosyntactic alignment inspired by Hindi split ergativity.
    
    The alignment changes based on aspect:
    - Imperfective: Nominative-Accusative (agent marked as subject)
    - Perfective + Transitive: Ergative-Absolutive (agent marked specially)
    """
    
    def __init__(self):
        self.current_alignment = "nominative-accusative"
    
    def determine_alignment(self, aspect: Aspect, is_transitive: bool) -> str:
        """Determine alignment based on aspectual and transitivity features"""
        if aspect == Aspect.PERFECTIVE and is_transitive:
            return "ergative-absolutive"
        return "nominative-accusative"
    
    def assign_roles(self, agent_model, patient_model, 
                     aspect: Aspect, is_transitive: bool) -> Dict[str, Any]:
        """
        Assign case roles based on alignment.
        
        Nominative-Accusative:
            Agent → [NOM], Patient → [ACC]
        
        Ergative-Absolutive:
            Agent → [ERG], Patient → [ABS] (unmarked like intransitive subject)
        """
        alignment = self.determine_alignment(aspect, is_transitive)
        
        if alignment == "ergative-absolutive":
            return {
                "alignment": alignment,
                "agent": {"model": agent_model, "case": HindiInspiredCase.ERG, "marker": "ने"},
                "patient": {"model": patient_model, "case": HindiInspiredCase.ACC, "marker": None},
                "verb_agreement": "patient"  # Verb agrees with patient in ergative
            }
        else:
            return {
                "alignment": alignment,
                "agent": {"model": agent_model, "case": HindiInspiredCase.NOM, "marker": None},
                "patient": {"model": patient_model, "case": HindiInspiredCase.ACC, "marker": None},
                "verb_agreement": "agent"  # Verb agrees with agent in nominative
            }

# Example: Perfective transitive
# राम ने किताब पढ़ी (Ram ne kitab padhi) - Ram read the book
manager = AlignmentManager()
perfective_roles = manager.assign_roles("Ram_Model", "Book_Model", 
                                         Aspect.PERFECTIVE, is_transitive=True)
# Agent gets ERG (ने), verb agrees with patient

# Example: Imperfective transitive  
# राम किताब पढ़ता है (Ram kitab padhta hai) - Ram reads book
imperfective_roles = manager.assign_roles("Ram_Model", "Book_Model",
                                           Aspect.IMPERFECTIVE, is_transitive=True)
# Agent gets NOM (unmarked), verb agrees with agent
```

### e. Locative Distinctions: में vs पर

Hindi distinguishes between interior location (में/meṃ) and surface location (पर/par), similar to Finnish's inessive/adessive distinction:

```python
class LocativeDistinction:
    """
    Hindi locative case distinctions for CEREBRUM spatial modeling.
    
    में (meṃ) - interior location, containment
    पर (par) - surface location, contact
    """
    
    def __init__(self, model):
        self.model = model
    
    def in_context(self, container_model, location_type: str = "interior") -> Dict[str, Any]:
        """
        Place model in locative context.
        
        Args:
            container_model: The containing/supporting model
            location_type: "interior" (में) or "surface" (पर)
        """
        if location_type == "interior":
            return {
                "model": self.model,
                "container": container_model,
                "case": HindiInspiredCase.LOC_IN,
                "postposition": "में",
                "spatial_relation": "contained_in",
                "access_pattern": "internal"
            }
        else:  # surface
            return {
                "model": self.model,
                "container": container_model,
                "case": HindiInspiredCase.LOC_ON,
                "postposition": "पर",
                "spatial_relation": "on_surface_of",
                "access_pattern": "external"
            }

# Example: राम घर में है (Ram ghar meṃ hai) - Ram is in the house
ram_location = LocativeDistinction("Ram_Model")
in_house = ram_location.in_context("House_Model", "interior")

# Example: किताब मेज़ पर है (Kitab mez par hai) - Book is on the table
book_location = LocativeDistinction("Book_Model")
on_table = book_location.in_context("Table_Model", "surface")
```

### f. Instrumental vs Ablative: से Disambiguation

Hindi uses the same postposition (से/se) for both instrumental and ablative functions, requiring contextual disambiguation:

```python
class SePostpositionDisambiguator:
    """
    Disambiguates Hindi से (se) between instrumental and ablative meanings.
    
    से as Instrumental: "with/by means of" - tool or method
    से as Ablative: "from" - source or origin
    """
    
    VERB_PATTERNS = {
        "motion_verbs": ["आना", "जाना", "निकलना"],  # come, go, exit
        "action_verbs": ["खोलना", "काटना", "लिखना"],  # open, cut, write
        "communication": ["बोलना", "कहना"],  # speak, say
    }
    
    def disambiguate(self, verb_type: str, semantic_role: str) -> Dict[str, Any]:
        """
        Determine whether से indicates instrumental or ablative.
        
        Heuristics:
        - Motion verbs + origin → Ablative
        - Action verbs + tool → Instrumental
        - Communication verbs + medium → Instrumental
        """
        if verb_type == "motion" and semantic_role == "origin":
            return {
                "case": HindiInspiredCase.ABL,
                "function": "source",
                "english_gloss": "from",
                "cerebrum_role": "data_source"
            }
        elif verb_type in ["action", "communication"]:
            return {
                "case": HindiInspiredCase.INS,
                "function": "instrument",
                "english_gloss": "with/by",
                "cerebrum_role": "processing_tool"
            }
        else:
            # Default to instrumental
            return {
                "case": HindiInspiredCase.INS,
                "function": "means",
                "english_gloss": "by means of",
                "cerebrum_role": "method"
            }

# Example: राम दिल्ली से आया (Ram Delhi se aaya) - Ram came from Delhi
disambiguator = SePostpositionDisambiguator()
ablative_use = disambiguator.disambiguate("motion", "origin")
# Returns ABL case

# Example: राम ने चाबी से दरवाज़ा खोला - Ram opened the door with a key
instrumental_use = disambiguator.disambiguate("action", "tool")
# Returns INS case
```

## Complete Implementation Example

The following demonstrates a complete Hindi-inspired CEREBRUM processing pipeline:

```python
from dataclasses import dataclass, field
from typing import List, Optional, Callable
from enum import Enum, auto

@dataclass
class HindiInspiredModel:
    """
    A CEREBRUM model with Hindi-inspired case and agreement features.
    """
    name: str
    case: HindiInspiredCase = HindiInspiredCase.NOM
    gender: str = "masculine"
    number: str = "singular"
    animacy: str = "inanimate"
    specificity: float = 0.5
    aspect: Aspect = Aspect.IMPERFECTIVE
    
    # Model data and state
    data: Dict[str, Any] = field(default_factory=dict)
    precision: float = 1.0
    
    def transform_to_case(self, target_case: HindiInspiredCase, 
                          **modifiers) -> 'HindiInspiredModel':
        """Transform model to target case with optional modifiers"""
        new_model = HindiInspiredModel(
            name=self.name,
            case=target_case,
            gender=modifiers.get('gender', self.gender),
            number=modifiers.get('number', self.number),
            animacy=modifiers.get('animacy', self.animacy),
            specificity=modifiers.get('specificity', self.specificity),
            aspect=modifiers.get('aspect', self.aspect),
            data=self.data.copy(),
            precision=modifiers.get('precision', self.precision)
        )
        return new_model
    
    def get_postposition(self) -> Optional[str]:
        """Get Hindi postposition for current case"""
        postpositions = {
            HindiInspiredCase.ERG: "ने",
            HindiInspiredCase.ACC_MARKED: "को",
            HindiInspiredCase.DAT: "को",
            HindiInspiredCase.GEN_MASC: "का",
            HindiInspiredCase.GEN_FEM: "की",
            HindiInspiredCase.GEN_OBL: "के",
            HindiInspiredCase.INS: "से",
            HindiInspiredCase.ABL: "से",
            HindiInspiredCase.LOC_IN: "में",
            HindiInspiredCase.LOC_ON: "पर",
            HindiInspiredCase.VOC: "हे",
        }
        return postpositions.get(self.case)
    
    def __repr__(self):
        pp = self.get_postposition()
        pp_str = f" ({pp})" if pp else ""
        return f"{self.name}[{self.case.name}]{pp_str}"


class HindiInspiredProcessor:
    """
    Complete processing pipeline with Hindi-inspired case semantics.
    """
    
    def __init__(self):
        self.alignment_manager = AlignmentManager()
        self.dom = DifferentialObjectMarking()
    
    def process_transitive(self, agent: HindiInspiredModel, 
                           patient: HindiInspiredModel,
                           action: Callable,
                           aspect: Aspect = Aspect.IMPERFECTIVE) -> Dict[str, Any]:
        """
        Process a transitive action with proper case assignment.
        
        Hindi: राम ने किताब पढ़ी (perfective)
        Hindi: राम किताब पढ़ता है (imperfective)
        """
        # Determine alignment and assign cases
        roles = self.alignment_manager.assign_roles(
            agent, patient, aspect, is_transitive=True
        )
        
        # Apply case transformations
        agent_cased = agent.transform_to_case(
            roles["agent"]["case"], 
            aspect=aspect
        )
        
        # Apply differential object marking
        patient_marking = self.dom.get_case_with_precision(
            patient.animacy, patient.specificity
        )
        patient_cased = patient.transform_to_case(
            patient_marking["case"],
            precision=patient_marking["precision"]
        )
        
        # Execute action
        result = action(agent_cased, patient_cased)
        
        return {
            "agent": agent_cased,
            "patient": patient_cased,
            "alignment": roles["alignment"],
            "verb_agreement": roles["verb_agreement"],
            "result": result
        }
    
    def process_ditransitive(self, agent: HindiInspiredModel,
                              theme: HindiInspiredModel,
                              recipient: HindiInspiredModel,
                              action: Callable,
                              aspect: Aspect = Aspect.PERFECTIVE) -> Dict[str, Any]:
        """
        Process a ditransitive action (give, send, etc.)
        
        Hindi: राम ने सीता को फल दिया
        (Ram ne Sita ko phal diya - Ram gave fruit to Sita)
        """
        # Agent gets ergative in perfective
        if aspect == Aspect.PERFECTIVE:
            agent_case = HindiInspiredCase.ERG
        else:
            agent_case = HindiInspiredCase.NOM
        
        agent_cased = agent.transform_to_case(agent_case, aspect=aspect)
        
        # Recipient always gets को (dative)
        recipient_cased = recipient.transform_to_case(HindiInspiredCase.DAT)
        
        # Theme (thing being transferred) - may or may not be marked
        theme_marking = self.dom.get_case_with_precision(
            theme.animacy, theme.specificity
        )
        theme_cased = theme.transform_to_case(theme_marking["case"])
        
        # Execute action
        result = action(agent_cased, theme_cased, recipient_cased)
        
        return {
            "agent": agent_cased,
            "theme": theme_cased,
            "recipient": recipient_cased,
            "result": result
        }
    
    def process_with_instrument(self, agent: HindiInspiredModel,
                                 patient: HindiInspiredModel,
                                 instrument: HindiInspiredModel,
                                 action: Callable) -> Dict[str, Any]:
        """
        Process action with instrumental case.
        
        Hindi: राम ने चाबी से दरवाज़ा खोला
        (Ram ne chabi se darvaza khola - Ram opened the door with a key)
        """
        agent_cased = agent.transform_to_case(HindiInspiredCase.ERG)
        patient_cased = patient.transform_to_case(HindiInspiredCase.ACC)
        instrument_cased = instrument.transform_to_case(HindiInspiredCase.INS)
        
        result = action(agent_cased, patient_cased, instrument_cased)
        
        return {
            "agent": agent_cased,
            "patient": patient_cased,
            "instrument": instrument_cased,
            "result": result
        }


# Complete usage example
def example_pipeline():
    """Demonstrates the complete Hindi-inspired CEREBRUM pipeline"""
    
    # Create models
    ram = HindiInspiredModel(
        name="Ram", gender="masculine", 
        animacy="human", specificity=1.0
    )
    sita = HindiInspiredModel(
        name="Sita", gender="feminine",
        animacy="human", specificity=1.0
    )
    book = HindiInspiredModel(
        name="Book", gender="feminine",
        animacy="inanimate", specificity=0.3
    )
    key = HindiInspiredModel(
        name="Key", gender="feminine",
        animacy="inanimate", specificity=0.8
    )
    door = HindiInspiredModel(
        name="Door", gender="masculine",
        animacy="inanimate", specificity=0.9
    )
    
    processor = HindiInspiredProcessor()
    
    # Example 1: राम ने किताब पढ़ी (Ram ne kitab padhi - Ram read the book)
    def read_action(agent, patient):
        return f"{agent} reads {patient}"
    
    result1 = processor.process_transitive(
        ram, book, read_action, aspect=Aspect.PERFECTIVE
    )
    print(f"Transitive (perfective): {result1['result']}")
    print(f"  Agent: {result1['agent']}")
    print(f"  Patient: {result1['patient']}")
    print(f"  Alignment: {result1['alignment']}")
    
    # Example 2: राम ने सीता को किताब दी 
    # (Ram ne Sita ko kitab di - Ram gave Sita the book)
    def give_action(agent, theme, recipient):
        return f"{agent} gives {theme} to {recipient}"
    
    result2 = processor.process_ditransitive(
        ram, book, sita, give_action, aspect=Aspect.PERFECTIVE
    )
    print(f"\nDitransitive: {result2['result']}")
    print(f"  Agent: {result2['agent']}")
    print(f"  Theme: {result2['theme']}")
    print(f"  Recipient: {result2['recipient']}")
    
    # Example 3: राम ने चाबी से दरवाज़ा खोला
    # (Ram ne chabi se darvaza khola - Ram opened the door with a key)
    def open_action(agent, patient, instrument):
        return f"{agent} opens {patient} using {instrument}"
    
    result3 = processor.process_with_instrument(
        ram, door, key, open_action
    )
    print(f"\nInstrumental: {result3['result']}")
    print(f"  Agent: {result3['agent']}")
    print(f"  Patient: {result3['patient']}")
    print(f"  Instrument: {result3['instrument']}")

# Run example
example_pipeline()
```

## Extended Case Mapping Table

| CEREBRUM Case | Hindi Marker | Function | Precision Weight | Implementation Pattern |
|--------------|--------------|----------|-----------------|----------------------|
| **[NOM]** | ∅ (unmarked) | Subject (imperfective) | 1.0 | Active agent, prediction generation |
| **[ERG]** | ने (ne) | Subject (perfective transitive) | 1.0 | Completed action agent |
| **[ACC]** | ∅ (unmarked) | Object (generic) | 0.3-0.5 | Low-salience patient |
| **[ACC:ko]** | को (ko) | Object (specific/animate) | 0.7-1.0 | High-salience patient |
| **[DAT]** | को (ko) | Recipient | 0.8 | Data/control recipient |
| **[GEN:m]** | का (kā) | Possessor (masc sg) | 0.7 | Source of masculine output |
| **[GEN:f]** | की (kī) | Possessor (fem) | 0.7 | Source of feminine output |
| **[GEN:obl]** | के (ke) | Possessor (oblique) | 0.7 | Source in oblique context |
| **[INS]** | से (se) | Instrument/means | 0.8 | Processing tool |
| **[ABL]** | से (se) | Source/origin | 0.6 | Data origin |
| **[LOC:in]** | में (meṃ) | Interior location | 0.5 | Internal context |
| **[LOC:on]** | पर (par) | Surface location | 0.5 | External context |
| **[VOC]** | हे (he) | Direct address | 0.9 | Model invocation |

## Conclusion

Hindi's case system, with its split ergativity, differential object marking, and rich agreement system, provides valuable insights for CEREBRUM implementation. Key contributions include:

1. **Split Ergativity**: The aspect-driven case alternation models how CEREBRUM case assignments can depend on process completion status.

2. **Differential Object Marking**: The animacy/specificity-based object marking provides a framework for precision-weighted case assignment.

3. **Genitive Agreement**: The three-way genitive agreement system models feature propagation across relationships.

4. **Locative Distinctions**: The में/पर distinction enriches spatial modeling with interior/surface semantics.

5. **Syncretic Forms**: The से postposition's dual instrumental/ablative function demonstrates contextual case disambiguation.

6. **Postpositional System**: Hindi's analytic approach (oblique + postposition) provides a compositional model for case transformation.

By incorporating these Hindi-inspired concepts, CEREBRUM can develop more nuanced case assignment logic that responds dynamically to aspectual, semantic, and pragmatic features of model interactions.

## References

1. Kachru, Yamuna. Hindi. John Benjamins Publishing, 2006.
2. McGregor, R.S. Outline of Hindi Grammar. Oxford University Press, 1995.
3. Mohanan, Tara. Argument Structure in Hindi. CSLI Publications, 1994.
4. Butt, Miriam and Tracy Holloway King. "Case Systems: Beyond Structural Distinctions." Oxford Handbook of Case, 2009.
5. Comrie, Bernard. "Ergativity." Encyclopedia of Language and Linguistics, 2006.
6. Masica, Colin P. The Indo-Aryan Languages. Cambridge University Press, 1991.
7. Snell, Rupert and Simon Weightman. Teach Yourself Hindi. McGraw-Hill, 2003.
8. Shapiro, Michael C. A Primer of Modern Standard Hindi. Motilal Banarsidass, 1989.
