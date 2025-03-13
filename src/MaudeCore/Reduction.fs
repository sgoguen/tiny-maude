namespace TinyMaude

module ReductionEngine =

    open System
    open FSharp.Core
    open AST

    // -----------------------------------------------------------------------------
    // Reduction Engine
    // -----------------------------------------------------------------------------

    /// Represents the state of a reduction process
    type ReductionState =
        {
            /// The module containing equations for rewriting
            activeModule: Module

            /// Log of terms produced during reduction (newest first)
            trace: Term list
        }

        /// Add a term to the reduction trace
        member this.AddTrace(t: Term) = { this with trace = t :: this.trace }

        /// Create an initial reduction state
        static member Create(m: Module) = { activeModule = m; trace = [] }

    /// The core rewriting function that applies equations to reduce a term
    let rewrite (state: ReductionState) (t: Term) : Term * ReductionState =

        let rec rewriteLoop state t =

            let m = state.activeModule

            match m.matchingEquals t with
            | [] ->
                // No rule applied - recursively process each subterm
                let newArgs, newState = processSubTerms state t

                let newTerm = { t with args = newArgs }

                if newTerm = t then
                    // No changes, we're done with this term
                    newTerm, newState
                else
                    // Term changed, track it and continue reduction
                    let updatedState = newState.AddTrace newTerm
                    rewriteLoop updatedState newTerm
            | { left = left; right = right } :: _ ->
                // Rule matched, apply substitution and continue
                let bm = m.variables.MakeMapBind(t, left)
                let substituted = bindVariable bm right
                let updatedState = state.AddTrace substituted
                rewriteLoop updatedState substituted

        and processSubTerms (state: ReductionState) (t: Term) : list<Term> * ReductionState =

            let rec loop (args: list<Term>) (state: ReductionState) (terms: list<Term>) =
                match terms with
                | [] -> args, state
                | term :: rest ->
                    let newArg, newState = rewriteLoop state term
                    let newArgs = args @ [ newArg ]
                    loop newArgs newState rest

            loop [] state t.args

        /// Applies variable bindings to a term, replacing variables with their bound values
        and bindVariable (bm: Map<Term, Term>) (r: Term) : Term =
            match Map.tryFind r bm with
            | Some t -> t
            | None ->
                { r with
                    args = r.args |> List.map (bindVariable bm) }

        rewriteLoop state t


    /// Reduce a term using the equations in a module
    let reduce (m: Module) (t: Term) : Term * ReductionState =
        let initialState = ReductionState.Create(m)
        rewrite initialState t