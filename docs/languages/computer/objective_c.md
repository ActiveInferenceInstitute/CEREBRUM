# Objective-C Language Paradigms and CEREBRUM Mapping

Objective-C is a general-purpose, object-oriented programming language that adds Smalltalk-style messaging to the C programming language. It was the main programming language used by Apple for macOS, iOS, watchOS, and tvOS applications prior to the introduction of Swift. It remains relevant for legacy systems and interoperability.

## 1. Overview of Objective-C Paradigms

- **Object-Oriented Programming**: Class-based, single inheritance with protocols (similar to interfaces) for polymorphism. Dynamic messaging system is central.
- **C Superset**: Any valid C code is typically valid Objective-C code.
- **Dynamic Typing & Runtime**: Message dispatch is resolved at runtime, allowing for dynamism (e.g., method swizzling, dynamic subclassing).
- **Messaging Syntax**: Distinctive `[receiver message]` syntax for method calls.
- **Reference Counting**: Primarily uses Automatic Reference Counting (ARC) for memory management, though manual retain/release (MRC) exists in older code.
- **Protocols**: Define sets of methods that classes can adopt.
- **Blocks**: Closure-like constructs for encapsulating code.

Relationships are defined via message sends, C function calls, property access (dot syntax or getter/setter messages), protocol conformance, and block execution.

## 2. Mapping CEREBRUM Cases to Objective-C Concepts

| CEREBRUM Case | Objective-C Equivalent/Analogy | Correspondence Strength | Notes |
|---------------|--------------------------------|-------------------------|-------|
| **Nominative [NOM]** | Object instance (`[[Class alloc] init]`); Result of message send/function; `self` pointer within methods | Strong | The acting entity, result, or context object. |
| **Accusative [ACC]** | Parameter in message send/function call; Object receiving message (`[receiver ...]`); Property being set | Strong | Entity receiving action or message. |
| **Dative [DAT]** | Variable receiving assignment result; Parameter passed by pointer for output (`NSError **error`); Target of block callback | Strong | Recipient of data, result, or error information. |
| **Genitive [GEN]** | Parameter in message send (source); Message/function return value; Property access (`object.property`); Block capturing variable | Strong | Source of data, value, attribute. |
| **Instrumental [INS]** | Method/Function definition; Protocol definition (`@protocol`); Class definition (`@interface`/`@implementation`); Block literal (`^{...}`) | Strong | The tool, method, function, protocol, or block used. |
| **Ablative [ABL]** | Input parameter (`NSString *input`); Collection being iterated; Source object in copy | Strong | Origin of data. |
| **Locative [LOC]** | Class scope (`@implementation`); Method/Function scope; Block scope; Header file (`.h`); Implementation file (`.m`) | Strong | Context, container, or scope where definitions exist. |
| **Vocative [VOC]** | Message send (`[receiver message:arg]`); Function call; Block execution (`block()`) | Strong | Direct invocation or message dispatch. |

## 3. Key Objective-C Features and Case Relationships

### Classes and Objects

Class definitions (`.h`, `.m`) and object instantiation/messaging.

```objectivec
// --- MyClass.h --- (LOC: Header File)
#import <Foundation/Foundation.h>

// Class interface definition (LOC blueprint)
@interface MyClass : NSObject

// Property declaration (defines GEN getter, ACC setter implicitly with ARC)
@property (nonatomic, strong) NSString *name;
@property (nonatomic, assign) NSInteger value;

// Method declaration (INS tool signature)
- (instancetype)initWithName:(NSString *)name value:(NSInteger)value; // Constructor-like
- (void)display; // Instance method
+ (NSString *)classDescription; // Class method

@end

// --- MyClass.m --- (LOC: Implementation File)
#import "MyClass.h"

// Class implementation (LOC)
@implementation MyClass

// Method implementation (INS tool)
- (instancetype)initWithName:(NSString *)aName value:(NSInteger)aValue {
    self = [super init]; // VOC message to superclass, self is NOM/ACC being init'd
    if (self) {
        // Property assignment (ACC setter implicitly called)
        // aName, aValue are GEN sources
        _name = [aName copy]; // Use backing ivar directly sometimes (GEN copy result)
        _value = aValue;
    }
    return self; // Returns NOM initialized instance
}

// Method implementation (INS tool)
- (void)display {
    // `self` is implicit NOM/ACC receiver
    // Access properties (GEN getter implicitly called)
    NSLog(@"MyClass Instance: Name=%@, Value=%ld", self.name, (long)self.value);
}

// Class method implementation (INS tool)
+ (NSString *)classDescription {
    return @"This is MyClass"; // Returns GEN string literal
}

@end

// --- main.m --- (Usage Example)
#import <Foundation/Foundation.h>
#import "MyClass.h"

int main(int argc, const char * argv[]) {
    @autoreleasepool { // LOC scope for memory management
        
        // Object allocation & initialization (VOC messages)
        // alloc returns NOM uninitialized obj, init returns NOM initialized obj
        // "Example" and 100 are GEN sources
        MyClass *obj1 = [[MyClass alloc] initWithName:@"Example" value:100]; // obj1 is NOM/DAT

        // Message send (VOC)
        // obj1 is NOM/ACC receiver
        [obj1 display];

        // Property access (ACC setter via dot syntax)
        obj1.value = 150; // 150 is GEN source

        // Property access (GEN getter via dot syntax)
        NSInteger currentValue = obj1.value; // currentValue is NOM/DAT
        NSLog(@"Current value: %ld", (long)currentValue);
        
        // Class method call (VOC)
        NSString *desc = [MyClass classDescription]; // desc is NOM/DAT
        NSLog(@"Class description: %@", desc);
    }
    return 0;
}
```

### Protocols

Define contracts (INS) that classes can adopt.

```objectivec
// --- Logger.h ---
#import <Foundation/Foundation.h>

// Protocol definition (INS contract)
@protocol LoggerDelegate <NSObject> // Inherits from NSObject protocol
- (void)logMessage:(NSString *)message; // Required method (INS tool signature)
@optional
- (BOOL)shouldLogTimestamp; // Optional method (INS tool signature)
@end

// --- Service.h ---
#import <Foundation/Foundation.h>
#import "Logger.h"

@interface Service : NSObject
// Delegate property (weak reference common)
// Conforms to LoggerDelegate protocol (GEN/DAT)
@property (nonatomic, weak) id<LoggerDelegate> delegate;
- (void)performTask;
@end

// --- Service.m ---
#import "Service.h"

@implementation Service
- (void)performTask {
    NSString *taskResult = @"Task completed successfully"; // NOM/GEN
    
    // Check if delegate implements the method (VOC respondsToSelector)
    if ([self.delegate respondsToSelector:@selector(logMessage:)]) {
        BOOL addTimestamp = NO; // NOM/DAT
        // Check optional method (VOC respondsToSelector)
        if ([self.delegate respondsToSelector:@selector(shouldLogTimestamp)]) {
           addTimestamp = [self.delegate shouldLogTimestamp]; // VOC message send, GEN result
        }
        
        NSString* finalMessage = taskResult; // NOM/DAT
        if (addTimestamp) {
           finalMessage = [NSString stringWithFormat:@"%@ - %@", [NSDate date], taskResult]; // VOC, GEN sources
        }
        
        // Send message to delegate (VOC)
        // self.delegate is NOM/ACC receiver
        // finalMessage is ACC/GEN argument
        [self.delegate logMessage:finalMessage];
    }
}
@end

// --- LoggerImpl.h / LoggerImpl.m ---
#import <Foundation/Foundation.h>
#import "Logger.h"

// Class conforming to protocol (INS adoption)
@interface LoggerImpl : NSObject <LoggerDelegate>
@end

@implementation LoggerImpl
- (void)logMessage:(NSString *)message { // ACC/GEN argument
    NSLog(@"[LoggerImpl]: %@", message);
}
- (BOOL)shouldLogTimestamp { return YES; } // GEN result
@end

// --- main.m ---
#import <Foundation/Foundation.h>
#import "Service.h"
#import "LoggerImpl.h"

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Service *service = [[Service alloc] init]; // NOM/DAT
        LoggerImpl *logger = [[LoggerImpl alloc] init]; // NOM/DAT
        
        service.delegate = logger; // Assign delegate (DAT target, GEN source)
        
        [service performTask]; // VOC message send
    }
    return 0;
}
```

*Mermaid Diagram: Delegate Pattern Flow*
```mermaid
graph TD
    Main -->|VOC| CreateService[Service alloc/init];
    Main -->|VOC| CreateLogger[LoggerImpl alloc/init];
    CreateService --> ServiceObj[NOM/DAT: service];
    CreateLogger --> LoggerObj[NOM/DAT: logger];
    Main --> SetDelegate[service.delegate = logger (DAT=GEN)];
    Main --> PerformTask[VOC: [service performTask]];
    PerformTask --> ServiceImpl[INS: Service performTask method];
    ServiceImpl --> CheckDelegateResponds{VOC: respondsToSelector?};    
    CheckDelegateResponds -- YES --> SendLogMsg[VOC: [delegate logMessage:msg]];
    SendLogMsg --> LoggerImpl[INS: LoggerImpl logMessage method];
    LoggerImpl --> Output[NSLog Output];
    CheckDelegateResponds -- NO --> EndTask(End Task);
```

### Blocks

Closures used for callbacks and inline code.

```objectivec
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Block type definition
        typedef void (^CompletionBlock)(NSString *result); // INS Type

        // Variable holding a block (NOM/DAT target)
        // Block literal `^{...}` is INS tool
        CompletionBlock myBlock = ^(NSString *result) { // result is ACC/GEN
            NSLog(@"Block executed with result: %@", result);
        };

        // Execute the block (VOC)
        myBlock(@"Success!"); // "Success!" is ACC/GEN argument

        // Function taking a block as argument
        void (^processData)(NSArray *, void (^)(id)) = 
            ^(NSArray *data, void (^callback)(id item)) { // data is ABL, callback is INS
                for (id item in data) { // item is NOM iterator var
                    callback(item); // VOC callback, item is ACC/GEN
                }
        };
        
        NSArray *items = @[@"One", @"Two", @"Three"]; // NOM/LOC array
        
        // Call function with inline block (VOC)
        processData(items, ^(id item) { // Inline block is INS tool
            NSLog(@"Processing item: %@", item);
        });
        
        // Blocks capturing variables from scope (GEN capture)
        __block NSInteger counter = 0; // __block allows modification
        void (^incrementBlock)(void) = ^{ // INS block
            counter++; // Access captured variable (ACC/DAT)
            NSLog(@"Counter: %ld", (long)counter);
        };
        
        incrementBlock(); // VOC
        incrementBlock(); // VOC
    }
    return 0;
}
```

## 4. Implementation Approach

Case roles are generally clear from Objective-C syntax and conventions:

1.  **Messaging Syntax (`[]`)**: Clearly marks the receiver (NOM/ACC) and the message/arguments (INS tool, ACC/GEN arguments).
2.  **Property Syntax (`.`)**: Dot syntax for properties implies VOC messages to ACC/DAT setters or GEN getters.
3.  **Pointer Usage**: Pointers passed for output (e.g., `NSError **`) signify DAT recipients.
4.  **Protocols**: Define INS contracts adopted by classes.
5.  **Blocks**: Act as self-contained INS tools, often used as callbacks (DAT targets for results).
6.  **ARC**: Manages object lifecycles, implicitly linking NOM objects to their scope (LOC).

Explicit CEREBRUM modeling is not idiomatic. Roles are understood through the messaging paradigm and standard Cocoa/Foundation patterns (like delegates).

## 5. Conclusion

Objective-C's unique dynamic messaging system provides a distinct way to map CEREBRUM cases:

- Objects are **NOM** actors, receiving messages (**VOC**) directed at them (**ACC**).
- Methods are **INS** tools invoked via messages.
- Properties encapsulate state (**GEN** access, **ACC** modification).
- Protocols define **INS** contracts for behavior.
- Blocks act as first-class **INS** tools, often capturing **GEN** context from their defining **LOC**.
- The C foundation provides basic procedural mappings for function calls and assignments (**VOC**, **DAT**, **GEN**).

The runtime dynamism allows relationships between objects and methods to be altered, but the fundamental roles in a message send (receiver, selector, arguments) remain clear mappings for CEREBRUM cases.

## 6. References

1.  Kochan, S. G. (2014). *Programming in Objective-C* (6th ed.). Addison-Wesley Professional.
2.  Conway, A., & Hillegass, A. (2011). *Objective-C Programming: The Big Nerd Ranch Guide*. Big Nerd Ranch Guides.
3.  Apple Developer Documentation - Objective-C. (https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/ProgrammingWithObjectiveC/)
4.  Ryba, M. (Ed.). (2013). *Pro Objective-C*. Apress.
``` 