ExnSTLC — STLC with Named Exceptions
Overview

This project implements ExnSTLC, a small programming language based on the Simply Typed Lambda Calculus (STLC) extended with named exceptions.

The language supports:

Numbers
Arithmetic operations (+, /)
Variables
Lambda functions
Function application
Named exceptions (raise)
Exception handling (try/catch)
Static type checking

The goal of this project is to demonstrate how exceptions can be integrated into a statically typed functional language, while preserving type safety and predictable runtime behavior.

Language Features
Core Expressions
numE — numeric literals
addE — addition
divE — division (raises DivZero if divisor is 0)
idE — variables
lamE — lambda functions
appE — function application
Exceptions
raiseE code type — raises a named exception
tryE body code handler — catches a specific exception

Example:

(tryE
  (raiseE 'DivZero (numT))
  'DivZero
  (numE 42))
Type System

The language uses a static type system to ensure programs are well-formed before execution.

Types
numT — numbers
funT — function types
Key Type Rules
Arithmetic requires numbers
Functions must be applied to arguments of the correct type
raiseE is valid only if the exception is declared
tryE requires the body and handler to have the same type

Example rule:

Γ ⊢ body : T    Γ ⊢ handler : T
--------------------------------
Γ ⊢ try body catch code => handler : T

This ensures exception handling does not break type consistency.

Interpreter Design

The interpreter evaluates expressions and returns a Result:

(okR value)      ; normal execution
(raiseR code)    ; exception raised
Key Design Decisions
Exceptions are explicit values, not runtime crashes
Exceptions propagate outward until caught
Only matching tryE blocks handle exceptions
Uncaught exceptions remain as raiseR

Example:

(run ex-uncaught)
→ (raiseR 'BadInput)
Exception Handling
Exceptions are identified by symbols (e.g., 'DivZero, 'Fail)
Only declared exceptions are allowed
Division by zero automatically raises 'DivZero

Example:

(divE (numE 10) (numE 0))
→ (raiseR 'DivZero)
Running Programs

Use:

(run expr)

This performs:

Type checking
Interpretation
Example Programs
Normal Execution
(run (addE (numE 1) (numE 2)))
→ (okR (numV 3))
Exception Handling
(run
 (tryE
  (raiseE 'DivZero (numT))
  'DivZero
  (numE 42)))
→ (okR (numV 42))
Uncaught Exception
(run
 (tryE
  (raiseE 'BadInput (numT))
  'DivZero
  (numE 42)))
→ (raiseR 'BadInput)
Testing

The project includes:

Unit tests for type checking
Interpreter tests for normal execution
Exception handling tests
Negative tests for type errors

Examples:

(test (run ex-add) (okR (numV 3)))
(test/exn (typecheck bad-apply-number) "application expects a function")
Design Summary

