# Tiny Maude

A tiny Maude interpreter written in F# inspired by [Sub-Maude](https://github.com/ncaq/sub-maude/) and [Maude](http://maude.cs.illinois.edu/). This project is for educational purposes only to demonstrate the concepts of the Maude language through a simple implementation.

## What is Maude?

Maude is a high-performance reflective language and system supporting both equational and rewriting logic specification and programming for a wide range of applications. This tiny implementation demonstrates some of the core concepts in a simplified manner.

## Core Concepts of Maude

### Term Rewriting

At the heart of Maude is term rewriting - the process of transforming a term step by step according to a set of rules until no more rules can be applied. This paradigm is particularly useful for:

- Formal specification
- Symbolic computation
- Logical systems
- Operational semantics of programming languages

### Functional Modules

Functional modules (fmod) in Maude define a set of sorts (types), operators, and equations that specify the behavior of the operators through rewriting rules.

```
fmod MODULE-NAME is
    sort Sort1 ... SortN .
    var X1 ... XN : Sort .
    op operator : Sort1 ... SortN -> ResultSort .
    eq term1 = term2 .
endfm
```

### Terms

Terms are the basic data structure in Maude, formed from operators and variables:
- Constants are operators with no arguments
- Variables can be bound during matching
- Complex terms are built by applying operators to other terms

### Equations

Equations define how terms can be transformed during rewriting. When the left-hand side of an equation matches a term, it's replaced with the equation's right-hand side.

## Exploring Maude with Tiny Maude

This project demonstrates how a Maude interpreter works by implementing core functionality that can be embedded in F# code. While not intended for production use, it shows how term rewriting systems operate.

### Example: Natural Numbers

Here's how the Peano natural numbers can be represented and manipulated:

```
fmod NAT is
    sort Nat .
    var N : Nat .
    var M : Nat .

    op z : -> Nat .          // Zero
    op s_ : Nat -> Nat .     // Successor
    op p : Nat Nat -> Nat .  // Addition
    op m : Nat Nat -> Nat .  // Multiplication

    eq p(N, z) = N .
    eq p(N, s M) = s p(N, M) .

    eq m(N, z) = z .
    eq m(N, s z) = N .
    eq m(N, s M) = p(N, m(N, M)) .
endfm
```

With these definitions:
- `z` represents 0
- `s z` represents 1, `s s z` represents 2, etc.
- `p(s s z, s s s z)` reduces to `s s s s s z` (2 + 3 = 5)
- `m(s s z, s s s z)` reduces to `s s s s s s z` (2 * 3 = 6)

### How Term Reduction Works

When evaluating `p(s s z, s s z)` (2 + 2):

1. Apply rule `eq p(N, s M) = s p(N, M)` with N=`s s z` and M=`z`
   - Result: `s p(s s z, z)`
2. Apply rule `eq p(N, z) = N` with N=`s s z`
   - Result: `s s s z`
3. No more rules applicable, final result: `s s s z` (3)

## F# Implementation Demonstration

Here's how the embedded interpreter demonstrates these concepts:

```fsharp
// Define a simple Maude module as a string
let natModuleText = """
fmod NAT is
    sort Nat .
    var N : Nat .
    var M : Nat .

    op z : -> Nat .
    op s_ : Nat -> Nat .
    op p : Nat Nat -> Nat .  // Addition

    eq p(N, z) = N .
    eq p(N, s M) = s p(N, M) .
endfm
"""

// Parse a module and a term, then reduce the term
let evaluateExample() =
    // Parse the module definition
    let natModule = readModule natModuleText |> Result.get
    
    // Create a term representing 2 + 3
    let term = run pTerm "p(s s z, s s s z)" |> Result.get
    
    // Reduce the term and get the result
    let (result, state) = reduce natModule term
    
    // View the result and reduction trace
    printfn "Result: %s" (result |> Term.toString)
    printfn "Reduction steps:"
    state.trace |> List.map Term.toString |> List.iter (printfn "  %s")
```

## Further Learning

To learn more about Maude and rewriting logic:
- [Maude System](http://maude.cs.illinois.edu/)
- [Rewriting Logic](https://en.wikipedia.org/wiki/Rewriting_logic)
- [Term Rewriting](https://en.wikipedia.org/wiki/Rewriting)

