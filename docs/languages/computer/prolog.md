# Prolog Language Paradigms and CEREBRUM Mapping

Prolog (Programming in Logic) is a logic programming language associated with artificial intelligence and computational linguistics. It's based on first-order logic and works by defining facts and rules, then querying the knowledge base. This document explores how Prolog's concepts map to CEREBRUM's case system.

## 1. Overview of Prolog Paradigms

Prolog is characterized by:

- **Logic Programming**: Based on Horn clauses (facts and rules).
- **Declarative Nature**: Describe *what* is true, let Prolog figure out *how* to prove it.
- **Unification**: Mechanism for matching terms and binding variables.
- **Backtracking**: Built-in search mechanism to find solutions.
- **Knowledge Base**: Programs consist of facts and rules defining relationships.

Relationships in Prolog are explicitly defined as predicates (facts and rules).

## 2. Mapping CEREBRUM Cases to Prolog Concepts

Mapping cases to Prolog requires interpreting the roles of terms within predicates and the query process.

| CEREBRUM Case | Prolog Equivalent/Analogy | Correspondence Strength | Notes |
|---------------|--------------------------|-------------------------|-------|
| **Nominative [NOM]** | Subject in a fact (`parent(john, mary).` - john); Result variable in a query (`?- parent(X, mary).` - X) | Strong | Represents the entity being defined or found. |
| **Accusative [ACC]** | Object in a fact/rule (`parent(john, mary).` - mary); Term being processed by a rule | Strong | Entity undergoing relation or being acted upon. |
| **Dative [DAT]** | Indirect object/recipient term in a multi-argument predicate | Moderate | Recipient role (often context-dependent). |
| **Genitive [GEN]** | Source in a relationship (`parent(john, mary).` implies mary is GEN source of parenthood for john); Attribute value | Strong | Represents source, possession, or attribute. |
| **Instrumental [INS]** | Predicate/Rule used for inference; Built-in predicate (`write/1`) | Strong | Means or mechanism for proving or computing. |
| **Ablative [ABL]** | Knowledge base providing facts/rules; Recursive call source | Strong | Origin of information or recursive step. |
| **Locative [LOC]** | Clause scope; Module system; Database/knowledge base itself | Strong | Context containing facts and rules. |
| **Vocative [VOC]** | Query (`?- predicate(...)`); Predicate call within a rule | Strong | Direct invocation of reasoning process. |

## 3. Key Prolog Features and Case Relationships

### Facts and Rules

Facts define basic relationships, rules define logical implications:

```prolog
% Facts (Define relationships)
% parent(Parent, Child)
% john is NOM (subject), mary is ACC (object of relation)
parent(john, mary).
parent(john, peter).
parent(susan, mary).
parent(susan, peter).

% mary is NOM, computer is ACC
owns(mary, computer).
% peter is NOM, book is ACC
owns(peter, book).

% Rule (Defines grandparent relationship - INS mechanism)
% Grandparent is NOM, Grandchild is ACC
grandparent(Grandparent, Grandchild) :-
    % parent(Grandparent, Parent) is VOC (predicate call)
    % Grandparent is NOM, Parent is ACC temporarily
    parent(Grandparent, Parent), 
    % parent(Parent, Grandchild) is VOC
    % Parent is NOM, Grandchild is ACC
    parent(Parent, Grandchild).
```

### Queries and Unification

Queries invoke the reasoning engine (VOC) to find matching NOM entities:

```prolog
% Query (VOC - invoking reasoning)
% Find NOM entity X such that parent(john, X) is true.
?- parent(john, X).
% Prolog finds matches by unification:
% X = mary ;
% X = peter.

% Query (VOC)
% Find NOM entity G such that grandparent(G, peter) is true.
?- grandparent(G, peter).
% G = john ;
% G = susan.

% Query with conjunction (VOC)
% Find NOM entity Person who owns ACC entity Item
% and where Person is ACC child of NOM john
?- owns(Person, Item), parent(john, Person).
% Person = mary,
% Item = computer ;
% Person = peter,
% Item = book.
```

### Recursion

Recursion is common for traversing structures or defining properties:

```prolog
% Define ancestor relationship recursively (INS mechanism)

% Base case: Parent is an ancestor (VOC)
% Ancestor is NOM, Descendant is ACC
ancestor(Ancestor, Descendant) :-
    parent(Ancestor, Descendant).

% Recursive step (VOC)
% Ancestor is NOM, Descendant is ACC
ancestor(Ancestor, Descendant) :-
    % parent(Ancestor, Child) is VOC call
    % Child becomes NOM in recursive call
    parent(Ancestor, Child),
    % ancestor(Child, Descendant) is recursive VOC call
    % Knowledge base acts as ABL source for parent facts
    ancestor(Child, Descendant).

% Query (VOC)
?- ancestor(john, mary).
% true.

?- ancestor(X, peter).
% X = john ;
% X = susan ;
% false. % Backtracking completes search
```

### Lists

Prolog handles lists using `[Head|Tail]` notation:

```prolog
% Predicate to find the length of a list (INS tool)
% list_length(List, Length)
% List is ACC, Length is NOM/GEN (result)

% Base case: Empty list has length 0
list_length([], 0).

% Recursive step: Length is 1 + length of tail
list_length([_Head|Tail], N) :-
    % list_length(Tail, N1) is VOC call
    % Tail is ACC in recursive call
    list_length(Tail, N1),
    % N is NOM/GEN (derived value)
    % is/2 predicate is INS (arithmetic evaluation)
    N is N1 + 1.

% Query (VOC)
?- list_length([a, b, c], Length).
% Length = 3.

% Predicate to append lists (INS tool)
% append(List1, List2, ResultList)
% List1, List2 are ACC, ResultList is NOM/GEN

% Base case
append([], L, L).

% Recursive step
append([H|T1], L2, [H|T3]) :-
    append(T1, L2, T3).

% Query (VOC)
?- append([1, 2], [3, 4], Result).
% Result = [1, 2, 3, 4].
```

## 4. Implementation Approach

Prolog inherently models relationships. Explicitly modeling CEREBRUM cases might involve structuring predicates to reflect roles:

```prolog
% Define predicates reflecting case roles
% action(Agent:nom, Patient:acc, Instrument:ins, Recipient:dat, Source:abl, Location:loc)

% Example: Sending a message
action(send, Sender, Message, network, Recipient, Sender, network_node) :-
    person(Sender), 
    message(Message),
    recipient(Recipient),
    format('~w [NOM] sends ~w [ACC] to ~w [DAT] via ~w [INS] from ~w [ABL] at ~w [LOC]~n', 
           [Sender, Message, Recipient, network, Sender, network_node]).

% Define supporting facts (basic types/entities)
person(alice).
person(bob).
message(hello).
recipient(bob).

% Query (VOC)
?- action(send, alice, hello, network, bob, alice, node1).
% Output: alice [NOM] sends hello [ACC] to bob [DAT] via network [INS] from alice [ABL] at node1 [LOC]
% true.

% Using variables to query roles
% Who (NOM) sent what (ACC) to bob (DAT)?
?- action(send, Sender, MsgContent, _, bob, _, _).
% Sender = alice,
% MsgContent = hello ;
% false.

% Representing case-bearing entities explicitly
% entity(BaseObject, CaseRole)

% Define action using explicit case entities
process_action(Agent, Patient, Tool) :-
    Agent = entity(_, nom),    % Unify with NOM case
    Patient = entity(_, acc),  % Unify with ACC case
    Tool = entity(_, ins),    % Unify with INS case
    
    Agent = entity(AgentBase, _),
    Patient = entity(PatientBase, _),
    Tool = entity(ToolBase, _),
    
    format('Processing with ~w [NOM], ~w [ACC], using ~w [INS]~n', 
           [AgentBase, PatientBase, ToolBase]).

% Query with explicit entities
?- Agent = entity(processor, nom),
   Patient = entity(data, acc),
   Tool = entity(algorithm, ins),
   process_action(Agent, Patient, Tool).
% Output: Processing with processor [NOM], data [ACC], using algorithm [INS]
% Agent = entity(processor, nom),
% Patient = entity(data, acc),
% Tool = entity(algorithm, ins).
```

## 5. Conclusion

Prolog's logic programming paradigm provides a natural fit for expressing relationships analogous to CEREBRUM cases:

- Facts and rules directly model relationships between entities (**NOM**, **ACC**, **GEN**).
- The query mechanism acts as **VOC**, invoking the reasoning process.
- Predicates and built-ins serve as **INS** mechanisms.
- Unification binds variables, effectively finding **NOM** entities satisfying relational constraints.
- The knowledge base itself serves as the **ABL** source and **LOC** context.

While Prolog doesn't have explicit grammatical cases, its relational structure and query-driven nature align well with the core concepts of agents, patients, instruments, sources, and locations central to CEREBRUM.

## 6. References

1. Sterling, L., & Shapiro, E. (1994). The Art of Prolog (2nd ed.). MIT Press.
2. Clocksin, W. F., & Mellish, C. S. (2003). Programming in Prolog (5th ed.). Springer.
3. Bratko, I. (2011). Prolog Programming for Artificial Intelligence (4th ed.). Addison-Wesley. 