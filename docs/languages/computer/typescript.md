# TypeScript Language Paradigms and CEREBRUM Mapping

TypeScript is a strongly typed superset of JavaScript developed by Microsoft. It adds static types to JavaScript, enabling better tooling, code maintainability, and catching errors at compile time. TypeScript compiles to plain JavaScript. This document explores how TypeScript features map to CEREBRUM cases.

## 1. Overview of TypeScript Paradigms

TypeScript builds upon JavaScript paradigms and adds:

- **Static Typing**: Optional static types for variables, parameters, and return values.
- **Type Inference**: Types can often be inferred by the compiler.
- **Interfaces**: Define contracts for object shapes or class implementations.
- **Classes**: Supports ES6 class syntax with additional type features.
- **Generics**: Create reusable components that work with various types.
- **Enums**: Define sets of named constants.
- **Decorators**: Experimental feature for modifying classes and members (metadata).

It supports OOP, functional, event-driven, and asynchronous programming, enhanced by its type system.

## 2. Mapping CEREBRUM Cases to TypeScript Concepts

Mapping is similar to JavaScript, but strengthened by the type system.

| CEREBRUM Case | TypeScript Equivalent/Analogy | Correspondence Strength | Notes |
|---------------|-----------------------------|-------------------------|-------|
| **Nominative [NOM]** | Object (`obj.method()`); `this`; Variable declared (`let x: Type`) | Strong | Acting entity or subject being defined. Type annotation clarifies role. |
| **Accusative [ACC]** | Function parameter; Object being modified; Target of assignment | Strong | Entity receiving action. Type annotation specifies expected type. |
| **Dative [DAT]** | Callback function; Event listener; Promise `.then()` handler; Output parameter type | Strong | Recipient of data, result, or event. Type clarifies expected data. |
| **Genitive [GEN]** | Property access (`obj.prop`); Return value type; Enum member; Interface/Class source | Strong | Source, possession, or derived value. Type specifies the derived type. |
| **Instrumental [INS]** | Higher-order function; Interface constraint; Utility function type; Decorator | Strong | Tool, mechanism, or constraint. Type definitions clarify the tool's signature. |
| **Ablative [ABL]** | Data source (array in `map`); Event emitter; Iterator source; Async source | Strong | Origin of data or process. Type clarifies source element type. |
| **Locative [LOC]** | Scope (function, class, module); Namespace; Object/Interface definition | Strong | Context, container, or location. Type definitions create structural contexts. |
| **Vocative [VOC]** | Function/method call; Constructor call (`new Type()`); Event trigger | Strong | Direct invocation. Type checking ensures correct arguments. |

## 3. Key TypeScript Features and Case Relationships

### Static Typing and Interfaces

Types and interfaces define structures and contracts:

```typescript
// Interface definition (INS contract and LOC structure)
interface Printable {
  print(): void; // Method signature requirement
}

// Class implementing interface (INS)
class Report implements Printable {
  // Property (GEN source)
  title: string;

  // Constructor (VOC invocation, creates ACC entity)
  constructor(title: string) {
    // this is ACC receiving initialization
    this.title = title; // title is GEN source
  }

  // Method implementation (conforms to INS)
  print(): void {
    // this is NOM acting
    console.log(`Printing report: ${this.title}`);
  }
}

// Function accepting conforming type (INS constraint)
function processPrintable(item: Printable): void {
  // item is ACC conforming to INS
  item.print(); // VOC call via interface
}

// Type alias (LOC structure)
type UserID = string | number;

function displayUser(id: UserID): void {
    // id is ACC
    console.log(`Displaying user with ID: ${id}`);
}

// Usage
const myReport = new Report("Q3 Sales"); // myReport is ACC created
processPrintable(myReport); // Pass ACC conforming to INS

let userId1: UserID = 123; // userId1 is NOM/GEN
displayUser(userId1); // VOC call

let userId2: UserID = "abc-456"; // userId2 is NOM/GEN
displayUser(userId2); // VOC call
```

### Classes and Inheritance

OOP features with type safety:

```typescript
// Base class (GEN source for inheritance)
abstract class Animal {
  // Property (GEN)
  abstract readonly species: string;

  // Constructor (VOC)
  constructor(public name: string) { // name is GEN source, public makes it property
    // this is ACC receiving init
  }

  // Abstract method (INS requirement for subclasses)
  abstract makeSound(): void;

  // Concrete method (this is NOM)
  move(distance: number): void {
    // distance is ACC
    console.log(`${this.name} moved ${distance}m.`);
  }
}

// Derived class (GEN derived from Animal)
class Dog extends Animal {
  // Override property (GEN)
  readonly species = "Canine";

  // Constructor (VOC)
  constructor(name: string, public breed: string) {
    // breed is GEN source, public makes it property
    super(name); // VOC call to parent constructor
  }

  // Implement abstract method (INS conformance)
  makeSound(): void {
    // this is NOM
    console.log("Woof!");
  }
}

// Usage
const buddy = new Dog("Buddy", "Golden Retriever"); // buddy is ACC created
buddy.makeSound(); // VOC call, buddy is NOM
buddy.move(10); // VOC call, buddy is NOM
console.log(`${buddy.name} is a ${buddy.species} (${buddy.breed})`); // GEN access
```

### Generics

Reusable components working with different types:

```typescript
// Generic function (INS tool with type flexibility)
// T is type variable
function wrapInArray<T>(item: T): T[] { 
  // item is ACC
  // T[] specifies GEN return type
  return [item]; 
}

// Generic interface (INS contract)
interface Container<T> {
  value: T; // GEN source
  getValue(): T;
}

// Generic class implementing interface (INS)
class Box<T> implements Container<T> {
  value: T; // GEN source

  // Constructor (VOC)
  constructor(value: T) { // value is GEN source
    // this is ACC receiving init
    this.value = value;
  }

  // Method (this is NOM)
  getValue(): T { // Return type is GEN
    return this.value;
  }
}

// Usage
const numberArray = wrapInArray(123); // NOM/GEN result: number[] inferred
const stringArray = wrapInArray("hello"); // NOM/GEN result: string[] inferred

const numberBox = new Box<number>(42); // ACC created
const stringBox: Container<string> = new Box("world"); // ACC created

console.log(numberBox.getValue()); // VOC call, numberBox is NOM
console.log(stringBox.value); // GEN access
```

### Enums

Named constants (LOC containing GEN members):

```typescript
// Numeric enum (LOC)
enum Direction {
  Up,    // GEN member, value 0
  Down,  // GEN member, value 1
  Left,  // GEN member, value 2
  Right, // GEN member, value 3
}

// String enum (LOC)
enum LogLevel {
  Info = "INFO",  // GEN member
  Warn = "WARN",  // GEN member
  Error = "ERROR", // GEN member
}

function move(dir: Direction) {
  // dir is ACC
  switch (dir) {
    case Direction.Up: console.log("Moving Up"); break;
    // ... other cases
    default: console.log("Moving...")
  }
}

function log(level: LogLevel, message: string) {
    // level, message are ACC
    console.log(`[${level}] ${message}`);
}

// Usage
let moveDirection = Direction.Up; // NOM/GEN
move(moveDirection); // VOC call

log(LogLevel.Warn, "Potential issue detected."); // VOC call
```

## 4. Implementation Approach

TypeScript's features allow for well-typed case implementations:

```typescript
// Case enum
enum CaseRole { NOM, ACC, DAT, GEN, INS, ABL, LOC, VOC }

// Generic wrapper interface (INS contract)
interface ICaseBearer<T> {
  readonly baseObject: T;
  readonly caseRole: CaseRole;
  readonly properties: Readonly<Record<string, unknown>>;

  // Method to conceptually change case
  as(newRole: CaseRole): ICaseBearer<T>;
}

// Generic wrapper class
class CaseWrapper<T> implements ICaseBearer<T> {
  public readonly baseObject: T;
  public readonly caseRole: CaseRole;
  private _properties: Record<string, unknown>;
  public get properties(): Readonly<Record<string, unknown>> {
      return this._properties;
  }

  constructor(base: T, role: CaseRole, props: Record<string, unknown> = {}) {
    this.baseObject = base;
    this.caseRole = role;
    this._properties = { ...props }; // Shallow copy
  }

  as(newRole: CaseRole): ICaseBearer<T> {
    console.log(`Transforming ${this.baseObject} from ${this.caseRole} to ${newRole}`);
    // Create new wrapper, copy properties
    return new CaseWrapper<T>(this.baseObject, newRole, this._properties);
  }

  public setProperty(key: string, value: unknown): void {
      // Note: This mutates the wrapper, consider returning new wrapper for immutability
      this._properties[key] = value;
  }

  toString(): string {
    return `[${CaseRole[this.caseRole]}] ${this.baseObject}`;
  }
}

// Example Domain
interface Request {
  url: string;
  method: 'GET' | 'POST';
}

interface Response {
  statusCode: number;
  body: string;
}

interface HttpClient {
  send(request: Request): Promise<Response>;
}

// Function enforcing roles
async function performRequest(
  requester: ICaseBearer<string>,      // Expected NOM
  requestData: ICaseBearer<Request>,   // Expected ACC
  client: ICaseBearer<HttpClient>     // Expected INS
): Promise<ICaseBearer<Response>> {   // Returns GEN
  
  if (requester.caseRole !== CaseRole.NOM) throw new Error("Requester must be NOM");
  if (requestData.caseRole !== CaseRole.ACC) throw new Error("Request data must be ACC");
  if (client.caseRole !== CaseRole.INS) throw new Error("Client must be INS");

  console.log(`${requester.baseObject} [NOM] sends request [ACC] using client [INS]`);
  const response = await client.baseObject.send(requestData.baseObject);
  
  // Wrap result as Genitive
  return new CaseWrapper(response, CaseRole.GEN);
}

// Demo
class MockHttpClient implements HttpClient {
    async send(request: Request): Promise<Response> {
        console.log(`Mock sending ${request.method} to ${request.url}`);
        await new Promise(resolve => setTimeout(resolve, 100)); // Simulate delay
        return { statusCode: 200, body: "Mock Response" };
    }
}

async function runDemo() {
    const user = "User123";
    const request: Request = { url: "/api/data", method: "GET" };
    const httpClient = new MockHttpClient();

    // Create wrappers
    const requesterW = new CaseWrapper(user, CaseRole.NOM);
    const requestW = new CaseWrapper(request, CaseRole.ACC);
    const clientW = new CaseWrapper(httpClient, CaseRole.INS);

    console.log(`Initial: ${requesterW}, ${requestW}, ${clientW}`);

    try {
        const responseW = await performRequest(requesterW, requestW, clientW);
        console.log(`Received Response: ${responseW}`); // GEN result
        console.log(`Status Code: ${(responseW.baseObject as Response).statusCode}`); // GEN access
    } catch (error: any) {
        console.error("Error:", error.message);
    }
}

// runDemo();
```

## 5. Conclusion

TypeScript enhances JavaScript's CEREBRUM case mapping capabilities with static typing:

- Type annotations clarify the intended roles (**NOM**, **ACC**, **DAT**, **GEN**) of variables and parameters.
- Interfaces and type aliases define structural contracts (**LOC**) and constraints (**INS**).
- Generics allow creating reusable functions and classes (**INS**) that operate on various types while maintaining type safety for case interactions.
- The compiler can catch misuse of entities based on their declared types, indirectly enforcing case-like relationships.

TypeScript provides a robust foundation for building systems where case relationships are explicitly typed and checked during development, improving code correctness and maintainability compared to plain JavaScript.

## 6. References

1. TypeScript Documentation. (https://www.typescriptlang.org/docs/)
2. TypeScript Handbook. (https://www.typescriptlang.org/docs/handbook/intro.html)
3. Effective TypeScript. (By Dan Vanderkam)
4. JavaScript/TypeScript Features Comparison. (Various online resources) 