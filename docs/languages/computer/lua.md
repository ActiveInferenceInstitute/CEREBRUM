# Lua Language Paradigms and CEREBRUM Mapping

Lua is a lightweight, high-level, multi-paradigm programming language designed primarily for embedded use in applications. It is known for its simplicity, efficiency, portability, and ease of integration with C.

## 1. Overview of Lua Paradigms

- **Procedural Programming**: Core paradigm with functions, control structures (`if`, `while`, `for`), and assignment.
- **Functional Programming**: Functions are first-class values; supports closures and proper tail calls.
- **Object-Oriented Programming**: Supported via tables and metatables, providing flexibility in implementing prototype-based or class-based styles.
- **Data Description**: Often used for configuration files due to its simple table syntax.
- **Dynamic Typing**: Variables do not have fixed types; types are associated with values.
- **Tables**: The single unifying data structure (associative arrays) used to implement arrays, records, objects, and namespaces.

Relationships are defined through function calls, table manipulation (indexing, assignment), and metatable interactions (operator overloading, inheritance simulation).

## 2. Mapping CEREBRUM Cases to Lua Concepts

| CEREBRUM Case | Lua Equivalent/Analogy | Correspondence Strength | Notes |
|---------------|------------------------|-------------------------|-------|
| **Nominative [NOM]** | Result of function/expression; Variable defined (`local var = ...`); Table being created (`{}`); Function itself | Strong | Entity resulting from computation or the active procedure/object. |
| **Accusative [ACC]** | Function argument; Table being modified (`tbl[key] = val`); Object receiving method call (`obj:method()`) | Strong | Entity receiving action, being processed, or modified. |
| **Dative [DAT]** | Variable receiving assignment result (`target = ...`); Table key being assigned a value | Strong | Recipient of data or result. |
| **Genitive [GEN]** | Function argument (source); Value on RHS of assignment; Table value access (`tbl[key]`, `tbl.key`); Return value | Strong | Source of data, value, attribute, or component. |
| **Instrumental [INS]** | Function definition; Operator (`+`, `..`, `#`); Metatable (`setmetatable`); `require` function | Strong | The tool, function, metatable, or operator used. |
| **Ablative [ABL]** | Input argument; Source table in iteration (`pairs`/`ipairs`); Source variable in copy | Strong | Origin of data or iteration stream. |
| **Locative [LOC]** | Table container; Function scope (`local`); Module scope (table returned by `require`); Global environment (`_G`) | Strong | Context, container, or environment where variables/functions exist. |
| **Vocative [VOC]** | Function call (`func(arg)`); Method call (`tbl:method()` or `tbl.method(tbl)`); `require("module")` | Strong | Direct invocation, application, or module loading. |

## 3. Key Lua Features and Case Relationships

### Tables

Tables are the cornerstone of Lua data structures.

```lua
-- Table creation (NOM/DAT assignment, LOC container)
local person = { 
  name = "Lua", -- Key-value pairs are GEN
  founded = 1993,
  tags = {"scripting", "embedded"} -- Nested table (LOC)
}

-- Accessing values (GEN access using INS `.`/`[]` operators)
-- person is LOC container
print("Name: " .. person.name) -- `..` is INS concatenation operator
print("First Tag: " .. person.tags[1])

-- Adding/Modifying fields (ACC/DAT target field)
person.latest_version = 5.4 -- 5.4 is GEN source
person["founded"] = 1994 -- String key access, 1994 is GEN source

-- Print modified table (VOC print, ACC/GEN person)
-- Requires a helper function to print tables nicely
local function print_table(t, indent)
  indent = indent or ""
  for k, v in pairs(t) do -- pairs is INS iterator, t is ABL source
    if type(v) == "table" then
      print(indent .. tostring(k) .. ":")
      print_table(v, indent .. "  ")
    else
      print(indent .. tostring(k) .. ": " .. tostring(v))
    end
  end
end

print("-- Person Table --")
print_table(person)

-- Numeric indices (Array-like table)
local numbers = {10, 20, 30} -- NOM/LOC
print("Second number: " .. numbers[2]) -- GEN access
```

### Functions

Functions are first-class citizens (INS tools).

```lua
-- Function definition (INS tool)
local function add(a, b) -- a, b are ACC/GEN arguments
  return a + b -- Returns GEN result
end

-- Function call (VOC)
local sum_result = add(5, 3) -- sum_result is NOM/DAT
print("Sum: " .. sum_result)

-- Functions assigned to variables (NOM/DAT target)
local multiply = function(a, b)
  return a * b
end

local product = multiply(5, 3) -- VOC call via variable
print("Product: " .. product)

-- Functions in tables (Methods - see OOP section)
local utils = {} -- NOM/LOC table
utils.double = function(n) return n * 2 end -- Assign function (INS) to table key (DAT)

print("Double 7: " .. utils.double(7)) -- VOC call (GEN access to function first)
```

### Object-Oriented Programming (using Tables and Metatables)

OOP is simulated using tables (LOC/NOM instances) and metatables (INS behavior definitions).

```lua
-- Define a "class" table (LOC blueprint)
local Vector = {}
Vector.__index = Vector -- INS: Makes instances look up methods in Vector table

-- Constructor function (INS tool)
function Vector.new(x, y)
  local self = setmetatable({}, Vector) -- Create NOM instance, link to INS metatable
  self.x = x -- ACC/DAT fields being set
  self.y = y
  return self -- Return NOM instance
end

-- Method definition (INS tool)
function Vector:magnitude() -- Colon syntax includes implicit `self` (NOM/ACC)
  return math.sqrt(self.x^2 + self.y^2) -- GEN access to fields
end

-- Method definition (INS tool)
function Vector:add(other) -- `self` implicit (NOM/ACC), `other` is ACC/GEN
  return Vector.new(self.x + other.x, self.y + other.y) -- Returns new NOM Vector
end

-- Create instances (VOC call to constructor)
local v1 = Vector.new(3, 4) -- v1 is NOM/DAT
local v2 = Vector.new(1, 2) -- v2 is NOM/DAT

-- Call methods (VOC using colon syntax)
-- v1, v2 act as NOM/ACC instances
print("Magnitude v1: " .. v1:magnitude()) 

local v3 = v1:add(v2) -- v3 is NOM/DAT
print("v3 (v1 + v2): x=" .. v3.x .. ", y=" .. v3.y)

-- Example of metatable for operators (INS tool)
local mt = { 
  __add = function (vec1, vec2) -- Overloads `+` operator (INS)
    return Vector.new(vec1.x + vec2.x, vec1.y + vec2.y) 
  end
}
setmetatable(Vector, mt) -- Apply operator metatable to the "class" itself

-- Now we can use `+` directly (VOC operator call)
local v4 = v1 + v2 -- Uses __add metamethod
print("v4 (v1 + v2 via +): x=" .. v4.x .. ", y=" .. v4.y)
```

*Mermaid Diagram: Method Call Flow (`v1:add(v2)`)*
```mermaid
graph TD
    Call[VOC: v1:add(v2)] --> LookupAdd{Lookup 'add' in v1's metatable (__index -> Vector)};
    LookupAdd --> AddFunc[INS: Vector.add function];
    V1[NOM/ACC: v1 (self)] --> AddFunc;
    V2[ACC/GEN: v2 (other)] --> AddFunc;
    AddFunc --> CreateNew{Call Vector.new (INS)};
    CreateNew --> V3[NOM/DAT: new Vector instance];
    AddFunc --> ReturnV3[Return V3];
    ReturnV3 --> Assignment[Assign to v3 (NOM/DAT)];
```

### Modules (`require`)

Modules encapsulate code in tables (LOC) and are loaded using `require` (VOC/INS).

```lua
-- Assume file 'mymodule.lua' exists:
-- ```lua
-- local M = {}
-- M.greeting = "Hello from Module!"
-- function M.greet(name)
--   print(M.greeting .. " to " .. name)
-- end
-- return M -- Returns LOC table
-- ```

-- Load module (VOC `require`, INS tool)
-- 'mymodule' is GEN identifier
-- mymod is NOM/DAT reference to the module table (LOC)
local mymod = require("mymodule") 

-- Access module content (GEN access via module table LOC)
print(mymod.greeting)

-- Call module function (VOC call)
-- mymod acts as LOC/INS container for the function
mymod.greet("Lua User")
```

## 4. Implementation Approach

Case roles in Lua are inferred primarily from:

1.  **Table Usage**: Tables act as LOC containers. Access (`.`, `[]`) is GEN, assignment to keys makes the table ACC/DAT.
2.  **Function Calls**: Standard function calls (`func(args)`) are VOC on INS functions with ACC/GEN args.
3.  **Method Calls**: Colon syntax (`obj:method(args)`) implies a VOC call where `obj` is the NOM/ACC instance (implicit first argument `self`).
4.  **Metatables**: `setmetatable` applies INS behaviors (__index, __add, etc.) to LOC tables.
5.  **`local` Keyword**: Defines variables within the current LOC scope.
6.  **Iteration**: `pairs`/`ipairs` act as INS iterators over ABL source tables.

Explicit CEREBRUM modeling isn't idiomatic. Comments are the main way to clarify intent.

## 5. Conclusion

Lua's simplicity, centered around tables and functions, provides clear mappings to CEREBRUM cases:

- Tables serve as versatile **LOC** containers, **NOM** objects, and namespaces.
- Table indexing provides **GEN** access, while assignment makes tables **ACC/DAT** targets.
- Functions are first-class **INS** tools invoked via **VOC** calls.
- Metatables provide a powerful mechanism for defining **INS** behaviors (like operator overloading or inheritance) associated with tables (**LOC**).
- The single assignment operator (`=`) marks **DAT** recipients and **GEN** sources.
- `require` acts as a **VOC**/**INS** mechanism for loading code into a **LOC** (module table).

The prototype-based nature derived from metatables allows flexible modeling where an object's behavior (**INS**) can be dynamically associated or changed.

## 6. References

1.  Ierusalimschy, R. (2016). *Programming in Lua* (4th ed.). Lua.org.
2.  Lua 5.4 Reference Manual. (https://www.lua.org/manual/5.4/)
3.  Lua-Users Wiki. (http://lua-users.org/wiki/)
4.  Roberto Ierusalimschy's Papers/Talks on Lua Design. (Accessible via Lua.org or his homepage) 