# SQL Language Paradigms and CEREBRUM Mapping

SQL (Structured Query Language), the standard language for managing and manipulating databases, uses a declarative approach based on relational algebra. This document explores how SQL's core concepts (tables, columns, queries, joins) relate to CEREBRUM's computational case framework.

## 1. Overview of SQL Paradigm

SQL operates on sets of data organized into tables (relations) with rows (tuples) and columns (attributes). Key characteristics include:

- **Declarative Nature**: Specify *what* data you want, not *how* to retrieve it.
- **Relational Algebra Foundation**: Operations like SELECT, PROJECT, JOIN, UNION, etc.
- **Set-Based Operations**: Queries operate on and return sets of rows.
- **Schema Definition**: Explicit definition of table structures and data types (DDL).
- **Data Manipulation**: Inserting, updating, deleting data (DML).
- **Querying**: Retrieving data based on conditions (SELECT statements).

Relationships in SQL are defined structurally (table schema), through explicit joins between tables, and functionally through the roles elements play in a query (e.g., selection criteria, projection target, join key).

## 2. Mapping CEREBRUM Cases to SQL Concepts

Mapping CEREBRUM cases involves identifying analogous roles and operations within SQL queries and database structure.

| CEREBRUM Case | SQL Equivalent/Analogy | Correspondence Strength | Notes |
|---------------|------------------------|-------------------------|-------|
| **Nominative [NOM]** | Table being queried (`FROM table`); Row being processed; Result set | Strong | The primary entity or set being operated upon or produced. |
| **Accusative [ACC]** | Table/Rows being modified (`UPDATE table`, `DELETE FROM table`); Target columns in `INSERT INTO table (...)` | Strong | The entity undergoing change or receiving data. |
| **Dative [DAT]** | Target table in `INSERT INTO table`; Parameter in stored procedure/function | Moderate | Represents the recipient of data. |
| **Genitive [GEN]** | Selected columns (`SELECT col1, col2`); Result of aggregation (`COUNT(*)`, `SUM(col)`); Foreign Key (source of relation) | Strong | Represents derived/extracted values or source attributes. |
| **Instrumental [INS]** | `WHERE` clause conditions; `JOIN` condition; Function used in query (`LOWER(col)`) | Moderate | Represents the means/criteria for filtering or processing. |
| **Ablative [ABL]** | Source table in `SELECT ... FROM source`; Subquery providing data | Strong | Represents the origin/source of the data being processed. |
| **Locative [LOC]** | Database/Schema; Table containing rows; `GROUP BY` clause (context for aggregation) | Strong | Represents the container or context for data. |
| **Vocative [VOC]** | Stored procedure/function call; Direct query execution | Strong | Represents direct invocation of a data operation. |

## 3. SQL Queries and Case Relationships

A standard SQL `SELECT` query demonstrates multiple case analogies:

```sql
SELECT -- Genitive (Derived/Extracted Values)
    c.customer_id, 
    c.name, 
    COUNT(o.order_id) AS order_count
FROM -- Ablative (Data Source)
    Customers c 
JOIN -- Instrumental (Joining Mechanism/Condition)
    Orders o ON c.customer_id = o.customer_id
WHERE -- Instrumental (Filtering Condition)
    c.city = 'London'
GROUP BY -- Locative (Context for Aggregation)
    c.customer_id, c.name
HAVING -- Instrumental (Filtering on Aggregated Result)
    COUNT(o.order_id) > 1
ORDER BY -- Adverbial (Manner of Presentation)
    order_count DESC;
```

- **`Customers c` / `Orders o` (FROM/JOIN)**: Act as **Ablative** sources providing the raw data.
- **`c`, `o` within the query**: Represent **Nominative** entities (rows) being processed.
- **`SELECT c.customer_id, c.name, COUNT(...)`**: These projections are **Genitive**, representing values derived or extracted from the Nominative rows.
- **`ON c.customer_id = o.customer_id`**: The join condition acts as an **Instrumental** constraint linking the sources.
- **`WHERE c.city = 'London'`**: The filter condition is **Instrumental**, defining the criteria for selection.
- **`GROUP BY c.customer_id, c.name`**: Establishes a **Locative** context for the `COUNT` aggregation.
- **`HAVING COUNT(...) > 1`**: Another **Instrumental** filter applied to the aggregated (**Genitive**) results.
- **The entire result set**: Can be viewed as a new **Nominative** entity (a derived table/relation).

## 4. Data Manipulation Language (DML) and Cases

- **`INSERT INTO target_table (...) VALUES (...)`**: `target_table` is **Accusative/Dative** (receiving data). The `VALUES` provide the **Genitive** source data.
- **`UPDATE target_table SET col = value WHERE ...`**: `target_table` is **Accusative** (being modified). `col` is the specific attribute being changed (**Accusative** sub-part). `value` is the source (**Genitive**). `WHERE` is **Instrumental**.
- **`DELETE FROM target_table WHERE ...`**: `target_table` is **Accusative** (having rows removed). `WHERE` is **Instrumental**.

## 5. Implementing CEREBRUM Cases with SQL

While SQL itself doesn't use cases, structuring interactions with a database can reflect case logic:

```python
import sqlite3

# Assume Case enum and CaseBearingModel wrapper exist (from Python example)

class SQLInterface:
    def __init__(self, db_file):
        self.conn = sqlite3.connect(db_file)
        self.cursor = self.conn.cursor()
        # Simplified: map table names to conceptual case roles
        self.table_roles = {
            "Customers": Case.ABL, # Usually a source
            "Orders": Case.ABL,
            "Products": Case.ABL,
            "QueryResults": Case.NOM # Often represents result entity
        }

    def represent_table(self, table_name):
        # Represent a table as a CaseBearingModel (defaults to ABL source)
        role = self.table_roles.get(table_name, Case.ABL)
        return CaseBearingModel(table_name, role)

    def execute_query(self, query_model: CaseBearingModel, conditions=None):
        """ Executes a query, conceptually treating query as VOC """
        if query_model.case != Case.VOC:
             print("Warning: Query should ideally be in VOC case for execution")
             query_model = query_model.transform_to_case(Case.VOC)

        query_string = query_model._base # Assume base object holds query string
        params = []
        
        # Apply conditions (INS)
        if conditions:
            # Simplified condition handling
            where_clauses = []
            for cond_model in conditions:
                if cond_model.case != Case.INS:
                    cond_model = cond_model.transform_to_case(Case.INS)
                # Assume cond_model._base is tuple like ('city=?', ['London'])
                clause, value = cond_model._base 
                where_clauses.append(clause)
                params.extend(value)
            query_string += " WHERE " + " AND ".join(where_clauses)

        print(f"Executing Query [VOC]: {query_string} with params {params}")
        self.cursor.execute(query_string, params)
        results = self.cursor.fetchall()
        
        # Represent results as a new NOM entity
        return CaseBearingModel(results, Case.NOM)

    def insert_data(self, target_table: CaseBearingModel, data: CaseBearingModel):
        """ Inserts data, target=ACC/DAT, data=GEN """
        if target_table.case not in [Case.ACC, Case.DAT]:
            target_table = target_table.transform_to_case(Case.ACC)
        if data.case != Case.GEN:
            data = data.transform_to_case(Case.GEN)
            
        table_name = target_table._base
        # Assume data._base is a dict
        columns = ', '.join(data._base.keys())
        placeholders = ', '.join(['?' for _ in data._base])
        values = list(data._base.values())
        
        sql = f"INSERT INTO {table_name} ({columns}) VALUES ({placeholders})"
        print(f"Inserting [GEN] {values} into {table_name} [ACC/DAT]")
        self.cursor.execute(sql, values)
        self.conn.commit()
        return self.cursor.lastrowid

# Example Usage
sql_interface = SQLInterface(':memory:') # Use in-memory DB
# Setup DB (simplified)
sql_interface.cursor.execute("CREATE TABLE Customers (id INTEGER PRIMARY KEY, name TEXT, city TEXT)")
sql_interface.cursor.execute("CREATE TABLE Orders (id INTEGER PRIMARY KEY, customer_id INTEGER, amount REAL)")

# Represent tables and data with cases
customers_table = sql_interface.represent_table("Customers") # ABL source
new_customer_data = CaseBearingModel({"name": "Alice", "city": "London"}, Case.GEN) # Data to insert

# Insert data
new_id = sql_interface.insert_data(customers_table, new_customer_data)
print(f"Inserted new customer with ID: {new_id}")

# Prepare a query (VOC)
query_str = "SELECT name, city FROM Customers"
query_model = CaseBearingModel(query_str, Case.VOC)

# Prepare conditions (INS)
condition_data = ('city=?', ['London'])
condition_model = CaseBearingModel(condition_data, Case.INS)

# Execute query
results_nom = sql_interface.execute_query(query_model, conditions=[condition_model])

print(f"Query Results [NOM]: {results_nom._base}")

sql_interface.conn.close()
```

## 6. Conclusion

SQL's declarative, relational model provides clear analogies for CEREBRUM cases, although expressed through query structure rather than inflection.

- **Tables/Rows** often map to **Nominative** (being processed) or **Ablative** (source).
- **Selected Columns/Results** map strongly to **Genitive** (derived/extracted).
- **Modification Targets** map to **Accusative** (or Dative for INSERT).
- **Query Clauses (`WHERE`, `JOIN ON`, `GROUP BY`)** often represent **Instrumental** (criteria, mechanism) or **Locative** (context) roles.
- **Query Execution** itself is analogous to **Vocative** (invocation).

Understanding these parallels allows for structuring database interactions within a CEREBRUM framework in a way that reflects the underlying relational logic using case concepts. This can help clarify the role of different data elements and operations within a larger computational process managed by CEREBRUM.

## 7. Advanced CEREBRUM Implementation

### Case-Aware Query Builder

```python
from enum import Enum
from typing import Any, Dict, List, Optional, Tuple
from dataclasses import dataclass, field
import math

class CaseRole(Enum):
    NOM = "Nominative"  # Result, subject
    ACC = "Accusative"  # Target of modification
    DAT = "Dative"      # Recipient
    GEN = "Genitive"    # Source, selected columns
    INS = "Instrumental"  # WHERE, JOIN conditions
    ABL = "Ablative"    # FROM source
    LOC = "Locative"    # Schema, context
    VOC = "Vocative"    # Query execution

# Case precision modifiers
CASE_PRECISION = {
    CaseRole.NOM: 1.5,
    CaseRole.ACC: 1.2,
    CaseRole.DAT: 1.3,
    CaseRole.GEN: 1.0,
    CaseRole.INS: 0.8,
    CaseRole.ABL: 1.1,
    CaseRole.LOC: 0.9,
    CaseRole.VOC: 2.0,
}

@dataclass
class CaseAnnotatedColumn:
    """Column with case role annotation"""
    name: str
    table: str
    case_role: CaseRole
    precision: float = 1.0
    
    def effective_precision(self) -> float:
        return self.precision * CASE_PRECISION[self.case_role]
    
    def qualified_name(self) -> str:
        return f"{self.table}.{self.name}"


@dataclass
class CaseQuery:
    """Query builder with case semantics"""
    select_columns: List[CaseAnnotatedColumn] = field(default_factory=list)  # GEN
    from_tables: List[Tuple[str, CaseRole]] = field(default_factory=list)    # ABL
    join_conditions: List[Tuple[str, CaseRole]] = field(default_factory=list)  # INS
    where_conditions: List[Tuple[str, CaseRole]] = field(default_factory=list)  # INS
    group_by: List[str] = field(default_factory=list)  # LOC
    order_by: List[str] = field(default_factory=list)
    
    def select(self, column: str, table: str, precision: float = 1.0) -> 'CaseQuery':
        """Add GEN column to selection"""
        self.select_columns.append(
            CaseAnnotatedColumn(column, table, CaseRole.GEN, precision)
        )
        return self
    
    def from_table(self, table: str) -> 'CaseQuery':
        """Add ABL source table"""
        self.from_tables.append((table, CaseRole.ABL))
        return self
    
    def join(self, table: str, condition: str) -> 'CaseQuery':
        """Add JOIN with INS condition"""
        self.from_tables.append((table, CaseRole.ABL))
        self.join_conditions.append((condition, CaseRole.INS))
        return self
    
    def where(self, condition: str) -> 'CaseQuery':
        """Add INS filter condition"""
        self.where_conditions.append((condition, CaseRole.INS))
        return self
    
    def group(self, *columns: str) -> 'CaseQuery':
        """Set LOC grouping context"""
        self.group_by.extend(columns)
        return self
    
    def build(self) -> str:
        """Build SQL query string"""
        parts = []
        
        # SELECT (GEN)
        cols = [c.qualified_name() for c in self.select_columns]
        parts.append(f"SELECT {', '.join(cols)}")
        
        # FROM (ABL)
        if self.from_tables:
            primary = self.from_tables[0][0]
            parts.append(f"FROM {primary}")
            
            # JOINs (ABL + INS)
            for i, (table, role) in enumerate(self.from_tables[1:]):
                if i < len(self.join_conditions):
                    condition, _ = self.join_conditions[i]
                    parts.append(f"JOIN {table} ON {condition}")
        
        # WHERE (INS)
        if self.where_conditions:
            conditions = [c for c, _ in self.where_conditions]
            parts.append(f"WHERE {' AND '.join(conditions)}")
        
        # GROUP BY (LOC)
        if self.group_by:
            parts.append(f"GROUP BY {', '.join(self.group_by)}")
        
        return '\n'.join(parts)
    
    def total_query_precision(self) -> float:
        """Calculate combined precision of query elements"""
        total = 0.0
        for col in self.select_columns:
            total += col.effective_precision()
        for _, role in self.where_conditions:
            total += CASE_PRECISION[role]
        return total


# Usage example
query = (CaseQuery()
    .select("customer_id", "c")
    .select("name", "c")
    .from_table("Customers c")
    .join("Orders o", "c.customer_id = o.customer_id")
    .where("c.city = 'London'")
    .group("c.customer_id", "c.name"))

print(query.build())
print(f"Query precision: {query.total_query_precision():.2f}")
```

### Active Inference Query Optimizer

```python
from dataclasses import dataclass
from typing import List, Dict
import math

@dataclass
class QueryBelief:
    """Belief state about query result characteristics"""
    expected_rows: float
    precision: float  # Confidence in estimate
    
    def update(self, actual_rows: float, obs_precision: float) -> 'QueryBelief':
        """Bayesian update with actual query result"""
        total = self.precision + obs_precision
        new_mean = (self.precision * self.expected_rows + obs_precision * actual_rows) / total
        return QueryBelief(new_mean, total)


class ActiveInferenceQueryOptimizer:
    """Uses Active Inference to optimize query execution"""
    
    def __init__(self):
        self.table_beliefs: Dict[str, QueryBelief] = {}
        self.join_selectivity: Dict[str, QueryBelief] = {}
        
    def initialize_table(self, table: str, est_rows: float):
        """Initialize belief about table cardinality"""
        self.table_beliefs[table] = QueryBelief(est_rows, 0.1)
    
    def update_table_belief(self, table: str, actual_rows: float):
        """Update belief after observing actual row count"""
        if table in self.table_beliefs:
            belief = self.table_beliefs[table]
            # Case-adjusted observation precision
            obs_precision = 1.0 * CASE_PRECISION[CaseRole.ABL]
            self.table_beliefs[table] = belief.update(actual_rows, obs_precision)
    
    def predict_result_size(self, query: CaseQuery) -> float:
        """Predict result set size using beliefs"""
        if not query.from_tables:
            return 0.0
        
        # Start with first table's belief
        primary_table = query.from_tables[0][0].split()[0]
        if primary_table in self.table_beliefs:
            estimate = self.table_beliefs[primary_table].expected_rows
        else:
            estimate = 1000.0  # Default
        
        # Apply selectivity for WHERE conditions
        for condition, role in query.where_conditions:
            # INS precision affects selectivity estimate
            selectivity = 0.3 * CASE_PRECISION[role]
            estimate *= selectivity
        
        return estimate
    
    def free_energy(self, query: CaseQuery, actual_rows: float) -> float:
        """Calculate free energy (prediction error cost)"""
        predicted = self.predict_result_size(query)
        error = actual_rows - predicted
        precision = query.total_query_precision()
        return (error ** 2 * precision) / 2.0
    
    def suggest_optimization(self, query: CaseQuery) -> List[str]:
        """Suggest query optimizations based on beliefs"""
        suggestions = []
        
        # Check if indexes would help WHERE conditions
        for condition, role in query.where_conditions:
            if "=" in condition:
                col = condition.split("=")[0].strip()
                suggestions.append(f"Consider index on {col} (INS filter)")
        
        # Check table sizes for join ordering
        if len(query.from_tables) > 1:
            sizes = []
            for table, role in query.from_tables:
                table_name = table.split()[0]
                if table_name in self.table_beliefs:
                    sizes.append((table_name, self.table_beliefs[table_name].expected_rows))
            
            if sizes:
                sizes.sort(key=lambda x: x[1])
                suggestions.append(f"Optimal join order: {' -> '.join([t for t, _ in sizes])}")
        
        return suggestions


# Usage
optimizer = ActiveInferenceQueryOptimizer()
optimizer.initialize_table("Customers", 10000)
optimizer.initialize_table("Orders", 50000)

# After observing actual data
optimizer.update_table_belief("Customers", 12500)

# Build and analyze query
query = (CaseQuery()
    .select("customer_id", "c")
    .from_table("Customers c")
    .join("Orders o", "c.customer_id = o.customer_id")
    .where("c.city = 'London'"))

print(f"Predicted result size: {optimizer.predict_result_size(query):.0f}")
print("Optimization suggestions:")
for s in optimizer.suggest_optimization(query):
    print(f"  - {s}")
```

### Relational Case Mapping

```sql
-- Schema with explicit case role annotations
-- Each table/column has a conceptual case role comment

-- Customers table: Primary NOM entities
CREATE TABLE Customers (
    customer_id INTEGER PRIMARY KEY,  -- GEN: identifier
    name VARCHAR(100),                 -- GEN: attribute
    email VARCHAR(100),                -- GEN: attribute  
    city VARCHAR(50),                  -- LOC: location context
    created_at TIMESTAMP DEFAULT NOW() -- ABL: origin timestamp
);

-- Orders table: ACC entities (targets of operations)
CREATE TABLE Orders (
    order_id INTEGER PRIMARY KEY,      -- GEN: identifier
    customer_id INTEGER REFERENCES Customers(customer_id), -- DAT: FK recipient
    total_amount DECIMAL(10,2),        -- GEN: derived value
    status VARCHAR(20),                -- NOM: current state
    order_date TIMESTAMP               -- ABL: origin timestamp
);

-- Case-aware view combining roles
CREATE VIEW CustomerOrderSummary AS
SELECT 
    -- GEN: Selected/derived values
    c.customer_id,
    c.name,
    COUNT(o.order_id) AS order_count,
    SUM(o.total_amount) AS total_spent
FROM 
    Customers c  -- ABL: Source
JOIN 
    Orders o ON c.customer_id = o.customer_id  -- INS: Join condition
GROUP BY 
    c.customer_id, c.name;  -- LOC: Aggregation context

-- Stored procedure with case-annotated parameters
CREATE PROCEDURE UpdateOrderStatus(
    IN p_order_id INTEGER,        -- ACC: Target being modified
    IN p_new_status VARCHAR(20),  -- GEN: Source of new value
    OUT p_success BOOLEAN         -- DAT: Receives result
)
BEGIN
    -- VOC: Execute update
    UPDATE Orders              -- ACC: Target table
    SET status = p_new_status  -- GEN -> ACC assignment
    WHERE order_id = p_order_id; -- INS: Filter condition
    
    SET p_success = (ROW_COUNT() > 0);  -- DAT: Result assignment
END;
```

## 8. Mermaid Diagram: SQL Case Flow

```mermaid
graph TD
    subgraph "Query Structure"
        SELECT["SELECT cols\n[GEN: Project]"]
        FROM["FROM table\n[ABL: Source]"]
        JOIN["JOIN ON cond\n[INS: Link]"]
        WHERE["WHERE cond\n[INS: Filter]"]
        GROUP["GROUP BY\n[LOC: Context]"]
    end
    
    FROM --> JOIN --> WHERE --> GROUP --> SELECT
    
    subgraph "DML Operations"
        INSERT["INSERT INTO\n[ACC/DAT: Target]"]
        UPDATE["UPDATE SET\n[ACC: Modify]"]
        DELETE["DELETE FROM\n[ACC: Remove]"]
    end
    
    subgraph "Result"
        Result["Result Set\n[NOM: Entity]"]
    end
    
    SELECT --> Result
```

## 9. References

1. Date, C. J. An Introduction to Database Systems. 8th ed., Addison-Wesley, 2003.
2. Silberschatz, Abraham, Henry F. Korth, and S. Sudarshan. Database System Concepts. 7th ed., McGraw-Hill, 2019.
3. SQL Standard (e.g., ISO/IEC 9075).
4. Friston, K. (2010). The free-energy principle. Nature Reviews Neuroscience.
5. Ramakrishnan, R., & Gehrke, J. (2003). Database Management Systems. McGraw-Hill.
