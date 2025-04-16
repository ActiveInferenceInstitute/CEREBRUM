# Fortran Language Paradigms and CEREBRUM Mapping

Fortran (Formula Translation) is one of the earliest high-level programming languages, primarily designed for numerical and scientific computing. Modern Fortran includes procedural, array-oriented, and object-oriented features. Mapping CEREBRUM cases involves interpreting its data structures (arrays, derived types) and procedures (subroutines, functions).

## 1. Overview of Fortran Paradigms

Modern Fortran supports:

- **Procedural Programming**: Organized into programs, subroutines, and functions.
- **Array Programming**: Powerful intrinsic functions and syntax for whole-array operations.
- **Modular Programming**: Modules encapsulate data and procedures.
- **Object-Oriented Features**: Derived types (structs), type-bound procedures (methods), inheritance (limited).
- **Static Typing**: Explicit type declarations are generally required.
- **Performance Focus**: Compiles to efficient machine code, especially for numerical tasks.

Relationships are defined by procedure calls, array operations, module usage, and derived type structures.

## 2. Mapping CEREBRUM Cases to Fortran Concepts

| CEREBRUM Case | Fortran Equivalent/Analogy | Correspondence Strength | Notes |
|---------------|----------------------------|-------------------------|-------|
| **Nominative [NOM]** | Result of function/expression; Variable defined (`TYPE :: var`); Subroutine/Function itself | Strong | Entity resulting from computation or the active procedure. |
| **Accusative [ACC]** | Argument with `INTENT(IN)` or `INTENT(INOUT)`; Array being operated upon; Target of assignment | Strong | Entity receiving action or being processed. |
| **Dative [DAT]** | Argument with `INTENT(OUT)` or `INTENT(INOUT)`; Target of assignment | Strong | Recipient of data or result. |
| **Genitive [GEN]** | Argument with `INTENT(IN)`; Function return value; Component access (`obj%component`); Module variable source | Strong | Source of data or component part. |
| **Instrumental [INS]** | Subroutine/Function performing action; Module providing procedures/data; Operator overload | Strong | Procedure, tool, or mechanism used. |
| **Ablative [ABL]** | Input argument (`INTENT(IN)`); Source array in operation; Input file unit | Strong | Origin of data. |
| **Locative [LOC]** | Module scope; Derived type definition; Array/Common block; Procedure scope | Strong | Context or container for data or procedures. |
| **Vocative [VOC]** | `CALL subroutine`; Function reference; `READ`/`WRITE` statement | Strong | Direct invocation of procedure or I/O operation. |

## 3. Key Fortran Features and Case Relationships

### Procedures (Subroutines and Functions)

Procedures are fundamental building blocks:

```fortran
PROGRAM MAIN_PROC_DEMO
  IMPLICIT NONE
  REAL :: x, y, result

  x = 5.0
  y = 3.0

  ! Function call (VOC)
  ! add_numbers is INS tool
  ! x, y are ACC arguments (implicitly INTENT(IN))
  ! result is NOM/DAT (receiving assignment)
  result = add_numbers(x, y)
  PRINT *, 'Function Result:', result

  ! Subroutine call (VOC)
  ! update_value is INS tool
  ! result has INTENT(INOUT) - acts as ACC/DAT
  CALL update_value(result, 2.0)
  PRINT *, 'Updated Result:', result

CONTAINS

  ! Function definition (INS tool)
  FUNCTION add_numbers(a, b) RESULT(sum)
    IMPLICIT NONE
    REAL, INTENT(IN) :: a, b ! a, b are ACC/GEN sources
    REAL :: sum            ! sum is NOM/GEN return value
    sum = a + b
  END FUNCTION add_numbers

  ! Subroutine definition (INS tool)
  SUBROUTINE update_value(val, factor)
    IMPLICIT NONE
    REAL, INTENT(INOUT) :: val ! val is ACC/DAT
    REAL, INTENT(IN)    :: factor ! factor is ACC/GEN source
    val = val * factor
  END SUBROUTINE update_value

END PROGRAM MAIN_PROC_DEMO
```

### Arrays

Fortran excels at array manipulation:

```fortran
PROGRAM ARRAY_DEMO
  IMPLICIT NONE
  REAL, DIMENSION(5) :: source_array = [1.0, 2.0, 3.0, 4.0, 5.0] ! NOM/GEN definition
  REAL, DIMENSION(5) :: result_array                          ! NOM/DAT definition
  REAL :: array_sum

  ! Whole array operation (INS implicit operation)
  ! source_array is ABL/GEN source
  ! result_array is ACC/DAT target
  result_array = source_array * 2.0 
  PRINT *, 'Result Array:', result_array

  ! Array intrinsic function (INS tool)
  ! source_array is ABL/GEN source
  ! array_sum is NOM/DAT result
  array_sum = SUM(source_array)
  PRINT *, 'Array Sum:', array_sum

  ! WHERE statement (INS conditional operation)
  WHERE (result_array > 5.0) ! result_array acts as ABL/GEN source for condition
     result_array = result_array / 2.0 ! result_array is ACC/DAT target
  END WHERE
  PRINT *, 'Array after WHERE:', result_array

END PROGRAM ARRAY_DEMO
```

*Mermaid Diagram: Array Operation Flow*
```mermaid
graph TD
    SA[ABL/GEN: source_array] -->|VOC: * 2.0| MulOp{Multiply Elements};
    MulOp -->|DAT Assignment| RA1[ACC/DAT: result_array (initial)];
    SA -->|VOC: SUM()| SumOp{Sum Elements};
    SumOp -->|DAT Assignment| AS[NOM/DAT: array_sum];
    RA1 -->|ABL for Condition| WhereCond{WHERE (array > 5.0)};
    WhereCond -- TRUE --> DivOp{Divide Elements by 2.0};
    DivOp -->|DAT Assignment| RA2[ACC/DAT: result_array (updated)];
    WhereCond -- FALSE --> SkipOp(Skip Element);
    SkipOp --> RA2;
```

### Modules

Modules provide encapsulation (LOC) for data (GEN) and procedures (INS):

```fortran
! Module definition (LOC container)
MODULE PHYSICS_CONSTANTS
  IMPLICIT NONE
  PRIVATE ! Default visibility
  PUBLIC :: GRAVITY, SPEED_OF_LIGHT ! Public entities

  ! Module variables (GEN sources)
  REAL, PARAMETER :: GRAVITY = 9.81
  REAL, PARAMETER :: SPEED_OF_LIGHT = 299792458.0

END MODULE PHYSICS_CONSTANTS

PROGRAM MODULE_USER
  ! Use module (makes INS/GEN entities available)
  USE PHYSICS_CONSTANTS
  IMPLICIT NONE
  REAL :: time_elapsed, distance

  time_elapsed = 10.0
  
  ! Access module variable (GEN access from LOC module)
  distance = 0.5 * GRAVITY * time_elapsed**2
  
  PRINT *, 'Using Gravity:', GRAVITY
  PRINT *, 'Distance fallen in', time_elapsed, 'sec:', distance

END PROGRAM MODULE_USER
```

### Derived Types (Object-Oriented Features)

Modern Fortran allows user-defined types with associated procedures:

```fortran
! Module defining the type and its methods (LOC)
MODULE GEOMETRY_TYPES
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: Point, create_point, calculate_distance

  ! Derived type definition (LOC blueprint)
  TYPE :: Point
     REAL :: x = 0.0 ! Component (GEN)
     REAL :: y = 0.0 ! Component (GEN)
   CONTAINS
     ! Type-bound procedure (INS method)
     PROCEDURE :: display => display_point
  END TYPE Point

CONTAINS

  ! Constructor-like function (INS tool)
  FUNCTION create_point(x_coord, y_coord) RESULT(p)
    IMPLICIT NONE
    REAL, INTENT(IN) :: x_coord, y_coord ! ACC/GEN sources
    TYPE(Point) :: p                   ! NOM/GEN result
    p%x = x_coord
    p%y = y_coord
  END FUNCTION create_point

  ! Subroutine bound to type (INS tool)
  SUBROUTINE display_point(self)
    IMPLICIT NONE
    CLASS(Point), INTENT(IN) :: self ! self is NOM/ACC receiver
    PRINT *, 'Point(X=', self%x, ', Y=', self%y, ')'
  END SUBROUTINE display_point

  ! Module procedure (INS tool)
  FUNCTION calculate_distance(p1, p2) RESULT(dist)
    IMPLICIT NONE
    CLASS(Point), INTENT(IN) :: p1, p2 ! ACC/GEN sources
    REAL :: dist                      ! NOM/GEN result
    dist = SQRT((p1%x - p2%x)**2 + (p1%y - p2%y)**2)
  END FUNCTION calculate_distance

END MODULE GEOMETRY_TYPES

PROGRAM OO_DEMO
  USE GEOMETRY_TYPES
  IMPLICIT NONE
  TYPE(Point) :: point_a, point_b ! NOM definitions
  REAL :: d

  ! Create instances (VOC call to INS constructor)
  point_a = create_point(1.0, 2.0) ! point_a is ACC/DAT target
  point_b = create_point(4.0, 6.0) ! point_b is ACC/DAT target

  ! Call type-bound procedure (VOC call)
  ! point_a is NOM receiver
  CALL point_a%display()
  CALL point_b%display()

  ! Call module procedure (VOC call)
  ! point_a, point_b are ACC/GEN arguments
  ! d is NOM/DAT target
  d = calculate_distance(point_a, point_b)
  PRINT *, 'Distance:', d

END PROGRAM OO_DEMO
```

## 4. Implementation Approach

Explicit case modeling is not idiomatic Fortran. Roles are typically understood through:

1.  **`INTENT` Attributes**: `INTENT(IN)` implies ABL/GEN source, `INTENT(OUT)` implies DAT target, `INTENT(INOUT)` implies ACC/DAT.
2.  **Function vs. Subroutine**: Functions return a single NOM/GEN result, while subroutines modify ACC/DAT arguments or module variables.
3.  **Variable Usage**: How variables are used in assignments and expressions determines their momentary role (GEN source, DAT target, ACC operand).
4.  **Comments**: Documenting the intended purpose and role of arguments and variables.

```fortran
SUBROUTINE SOLVE_EQUATION(COEFFS, N_COEFFS, ROOT, STATUS)
  ! Args:
  !   COEFFS   (Input - REAL, Array): Coefficients (ABL/GEN source)
  !   N_COEFFS (Input - INTEGER): Number of coefficients (ACC/GEN)
  !   ROOT     (Output - REAL): Calculated root (DAT target)
  !   STATUS   (Output - INTEGER): Status code (DAT target)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N_COEFFS
  REAL, DIMENSION(N_COEFFS), INTENT(IN) :: COEFFS 
  REAL, INTENT(OUT) :: ROOT 
  INTEGER, INTENT(OUT) :: STATUS
  
  ! Local variables (NOM)
  INTEGER :: i
  REAL :: calculation_result

  PRINT *, 'Solving equation...' ! VOC
  ! ... calculation logic using COEFFS (ABL/GEN) ...
  calculation_result = COEFFS(1) + COEFFS(2) ! Example
  
  ! Assign results to output parameters (DAT)
  ROOT = calculation_result
  STATUS = 0 ! Indicate success
  
END SUBROUTINE SOLVE_EQUATION 
```

## 5. Conclusion

Fortran, especially modern Fortran, provides clear structural elements that map to CEREBRUM cases:

- Procedures (functions/subroutines) act as **INS** tools, invoked via **VOC** calls.
- `INTENT` attributes explicitly define the role of arguments as sources (**ABL/GEN**), targets (**DAT**), or both (**ACC/DAT**).
- Arrays facilitate **ABL** (source) and **ACC/DAT** (target) operations efficiently.
- Modules define **LOC** contexts containing **GEN** data and **INS** procedures.
- Derived types define **LOC** blueprints for objects whose components are **GEN** sources.

While lacking specific case syntax, Fortran's explicit argument intents and strong procedural structure allow for a reasonably clear understanding of how data flows and which entities perform actions or receive results, aligning well with the CEREBRUM concept of distinct participant roles.

## 6. References

1. Metcalf, M., Reid, J., & Cohen, M. (2018). Modern Fortran Explained: Incorporating Fortran 2018. Oxford University Press.
2. Fortran Standard Documents (e.g., Fortran 2018 - ISO/IEC 1539-1:2018).
3. Fortran-Lang Official Website & Tutorials. (https://fortran-lang.org/)
4. Oliphant, T. E. (2006). Guide to NumPy (While primarily Python, explains array concepts influential in scientific computing). 