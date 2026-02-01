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

1. **Table Usage**: Tables act as LOC containers. Access (`.`, `[]`) is GEN, assignment to keys makes the table ACC/DAT.
2. **Function Calls**: Standard function calls (`func(args)`) are VOC on INS functions with ACC/GEN args.
3. **Method Calls**: Colon syntax (`obj:method(args)`) implies a VOC call where `obj` is the NOM/ACC instance (implicit first argument `self`).
4. **Metatables**: `setmetatable` applies INS behaviors (__index,__add, etc.) to LOC tables.
5. **`local` Keyword**: Defines variables within the current LOC scope.
6. **Iteration**: `pairs`/`ipairs` act as INS iterators over ABL source tables.

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

## 6. Advanced CEREBRUM Implementation

### Case-Enforcing Metatables

```lua
-- CEREBRUM Case System Implementation in Lua
local CaseSystem = {}
CaseSystem.__index = CaseSystem

-- Case enumeration
local Case = {
    NOM = "NOM",  -- Nominative: Active agent
    ACC = "ACC",  -- Accusative: Patient/target
    DAT = "DAT",  -- Dative: Recipient
    GEN = "GEN",  -- Genitive: Source/possessor
    INS = "INS",  -- Instrumental: Tool/means
    ABL = "ABL",  -- Ablative: Origin
    LOC = "LOC",  -- Locative: Location/context
    VOC = "VOC"   -- Vocative: Direct address
}

-- Precision modifiers per case (Active Inference)
local CasePrecision = {
    [Case.NOM] = 1.5,
    [Case.ACC] = 1.2,
    [Case.DAT] = 1.3,
    [Case.GEN] = 1.0,
    [Case.INS] = 0.8,
    [Case.ABL] = 1.1,
    [Case.LOC] = 0.9,
    [Case.VOC] = 2.0
}

-- Create a case-bearing entity
function CaseSystem.new(base, case, precision)
    local self = setmetatable({}, CaseSystem)
    self._base = base
    self._case = case or Case.NOM
    self._precision = precision or 1.0
    self._history = {}
    return self
end

-- Case transformation with validation
local ValidTransitions = {
    [Case.NOM] = {Case.ACC, Case.GEN},
    [Case.ACC] = {Case.GEN, Case.DAT},
    [Case.ABL] = {Case.NOM},
    [Case.LOC] = {Case.ABL}
}

function CaseSystem:transform_to(target_case)
    local valid = ValidTransitions[self._case]
    if valid then
        for _, v in ipairs(valid) do
            if v == target_case then
                table.insert(self._history, {
                    from = self._case,
                    to = target_case,
                    time = os.time()
                })
                self._case = target_case
                return self
            end
        end
    end
    error(string.format("Invalid transition: %s -> %s", self._case, target_case))
end

-- Get current case
function CaseSystem:get_case()
    return self._case
end

-- Get base value
function CaseSystem:get_base()
    return self._base
end

-- Get effective precision (case-adjusted)
function CaseSystem:effective_precision()
    return self._precision * (CasePrecision[self._case] or 1.0)
end

-- Metatable for pretty printing
function CaseSystem:__tostring()
    return string.format("<%s>[%s](p=%.2f)", 
        tostring(self._base), 
        self._case, 
        self:effective_precision())
end

-- Usage example
local agent = CaseSystem.new("Processor", Case.NOM, 1.0)
local patient = CaseSystem.new("Data", Case.ACC, 0.9)
print("Agent:", agent)
print("Patient:", patient)
```

### Coroutines as Case Transitions

```lua
-- Case transformation as coroutine-based state machine
local function case_machine(initial_case)
    local current = initial_case
    local history = {}
    
    while true do
        -- Yield current state, receive action
        local action, target = coroutine.yield(current)
        
        if action == "transition" then
            table.insert(history, {from = current, to = target})
            current = target
        elseif action == "history" then
            coroutine.yield(history)
        elseif action == "stop" then
            return current
        end
    end
end

-- Create and use case state machine
local function create_case_entity(name, initial_case)
    local co = coroutine.create(case_machine)
    local _, state = coroutine.resume(co, initial_case)
    
    return {
        name = name,
        _co = co,
        
        -- Get current case
        current = function(self)
            return state
        end,
        
        -- Transform to new case
        transform = function(self, new_case)
            local success, result = coroutine.resume(self._co, "transition", new_case)
            if success then
                state = result
                return self
            else
                error("Transformation failed: " .. tostring(result))
            end
        end,
        
        -- Get transformation history
        history = function(self)
            local _, h = coroutine.resume(self._co, "history")
            return h
        end
    }
end

-- Example workflow
local entity = create_case_entity("Model", Case.NOM)
print("Initial:", entity:current())  -- NOM

entity:transform(Case.ACC)
print("After transform:", entity:current())  -- ACC

entity:transform(Case.GEN)
print("After second transform:", entity:current())  -- GEN

for i, h in ipairs(entity:history()) do
    print(string.format("Transition %d: %s -> %s", i, h.from, h.to))
end
```

### Active Inference Implementation

```lua
-- Active Inference Model in Lua
local ActiveInference = {}
ActiveInference.__index = ActiveInference

function ActiveInference.new(initial_belief, precision, case)
    local self = setmetatable({}, ActiveInference)
    self.belief = {
        mean = initial_belief,
        precision = precision
    }
    self.case = case or Case.NOM
    return self
end

-- Bayesian belief update
function ActiveInference:update(observation, obs_precision)
    local case_mod = CasePrecision[self.case] or 1.0
    local adjusted_precision = obs_precision * case_mod
    
    local total_precision = self.belief.precision + adjusted_precision
    local posterior_mean = 
        (self.belief.precision * self.belief.mean + 
         adjusted_precision * observation) / total_precision
    
    self.belief.mean = posterior_mean
    self.belief.precision = total_precision
    
    return self
end

-- Calculate variational free energy
function ActiveInference:free_energy(observation)
    local case_mod = CasePrecision[self.case] or 1.0
    local eff_precision = self.belief.precision * case_mod
    
    local pred_error = (observation - self.belief.mean) ^ 2
    return pred_error * eff_precision / 2
end

-- Predict next observation
function ActiveInference:predict()
    return self.belief.mean
end

-- Select action to minimize expected free energy
function ActiveInference:select_action(possible_observations)
    local best_obs, min_fe = nil, math.huge
    
    for _, obs in ipairs(possible_observations) do
        local fe = self:free_energy(obs)
        if fe < min_fe then
            min_fe = fe
            best_obs = obs
        end
    end
    
    return best_obs, min_fe
end

-- Example usage
local model = ActiveInference.new(5.0, 1.0, Case.NOM)
print(string.format("Initial belief: mean=%.2f, precision=%.2f", 
    model.belief.mean, model.belief.precision))

model:update(6.0, 0.5)
print(string.format("After update: mean=%.2f, precision=%.2f", 
    model.belief.mean, model.belief.precision))

local fe = model:free_energy(5.5)
print(string.format("Free energy at 5.5: %.4f", fe))

local best, min_fe = model:select_action({4.0, 5.0, 6.0, 7.0})
print(string.format("Best action: observe %.1f (FE=%.4f)", best, min_fe))
```

### Message Passing with Case Roles

```lua
-- Actor-like message passing with explicit case roles
local Actor = {}
Actor.__index = Actor

function Actor.new(name)
    local self = setmetatable({}, Actor)
    self.name = name
    self.inbox = {}
    self.handlers = {}
    return self
end

function Actor:register_handler(message_type, handler)
    self.handlers[message_type] = handler
end

function Actor:send(target, message_type, payload, case_role)
    -- Message with explicit case marking
    local msg = {
        from = self,
        from_case = Case.NOM,  -- Sender is always NOM
        to_case = case_role or Case.DAT,  -- Recipient role
        type = message_type,
        payload = payload,
        timestamp = os.time()
    }
    table.insert(target.inbox, msg)
    return msg
end

function Actor:process()
    for _, msg in ipairs(self.inbox) do
        local handler = self.handlers[msg.type]
        if handler then
            -- Execute with case context
            handler(self, msg)
        end
    end
    self.inbox = {}
end

-- Example multi-actor system
local processor = Actor.new("Processor")
local database = Actor.new("Database")
local client = Actor.new("Client")

processor:register_handler("process", function(self, msg)
    print(string.format("[%s][%s] received from [%s][%s]: %s",
        self.name, msg.to_case, msg.from.name, msg.from_case, 
        msg.payload))
    
    -- Forward result to client as GEN (derived output)
    self:send(msg.from, "result", "processed_" .. msg.payload, Case.GEN)
end)

client:register_handler("result", function(self, msg)
    print(string.format("[%s] received result [%s]: %s",
        self.name, msg.to_case, msg.payload))
end)

-- Workflow
client:send(processor, "process", "raw_data", Case.ACC)  -- Data as ACC
processor:process()
client:process()
```

## 7. Functional Composition with Case Awareness

```lua
-- Composable case-aware functions
local function compose(...)
    local fns = {...}
    return function(x)
        local result = x
        for _, fn in ipairs(fns) do
            result = fn(result)
        end
        return result
    end
end

-- Case-preserving map operation
local function case_map(fn)
    return function(entity)
        local new_base = fn(entity:get_base())
        return CaseSystem.new(new_base, entity:get_case(), entity._precision)
    end
end

-- Case-transforming operation
local function case_transform(target_case)
    return function(entity)
        local clone = CaseSystem.new(
            entity:get_base(), 
            entity:get_case(), 
            entity._precision
        )
        return clone:transform_to(target_case)
    end
end

-- Pipeline example
local pipeline = compose(
    case_map(function(x) return x * 2 end),
    case_transform(Case.ACC),
    case_map(function(x) return x + 5 end),
    case_transform(Case.GEN)
)

local input = CaseSystem.new(10, Case.NOM)
local output = pipeline(input)
print("Pipeline result:", output)
```

## 8. References

1. Ierusalimschy, R. (2016). *Programming in Lua* (4th ed.). Lua.org.
2. Lua 5.4 Reference Manual. (<https://www.lua.org/manual/5.4/>)
3. Lua-Users Wiki. (<http://lua-users.org/wiki/>)
4. Roberto Ierusalimschy's Papers/Talks on Lua Design. (Accessible via Lua.org)
5. Friston, K. (2010). The free-energy principle. Nature Reviews Neuroscience.
6. Hewitt, C., et al. (1973). A Universal Modular ACTOR Formalism. IJCAI.
