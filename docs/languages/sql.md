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

## 7. References

1.  Date, C. J. An Introduction to Database Systems. 8th ed., Addison-Wesley, 2003.
2.  Silberschatz, Abraham, Henry F. Korth, and S. Sudarshan. Database System Concepts. 7th ed., McGraw-Hill, 2019.
3.  SQL Standard (e.g., ISO/IEC 9075). 