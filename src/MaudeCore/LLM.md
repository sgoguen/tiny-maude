namespace TinyMaude

//  Inspired by https://github.com/ncaq/sub-maude/

module AST =

    open System

    type SortId = string
    type Sort = Sort

    [<StructuredFormatDisplay("{sort}")>]
    type Variable = { sort: SortId }

    let Variable sort = { sort = sort }


    type OpId = string

    [<StructuredFormatDisplay("{op}({args})")>]
    type Term = { op: OpId; args: Term list }

    module Term =
        let rec toString (t: Term) =
            match t.args with
            | [] -> t.op
            | [ a ] -> t.op + " " + toString a
            | args -> sprintf "%s(%s)" t.op (String.concat ", " (List.map toString args))

        /// Pretty-print a term
        let rec showTerm (t: Term) : string =
            match t.args with
            | [] -> t.op
            | [ a ] -> t.op + " " + showTerm a
            | args -> t.op + " (" + String.Join(", ", args |> List.map showTerm) + ")"


    [<StructuredFormatDisplay("eq {left} = {right}")>]
    type Equation = { left: Term; right: Term }

    [<StructuredFormatDisplay("{argSorts} -> {result}")>]
    type Operator =
        { argSorts: SortId list
          result: SortId
          eqs: Equation list }

    type Sorts = Sorts of Map<string, Sort>

    type Variables =
        | Vars of Map<string, Variable>

        member this.IsVar(t: Term) =
            let (Vars variables) = this
            Map.containsKey t.op variables

        member this.IsMatch(t: Term, u: Term) : bool =
            // If the pattern is a variable, it can match anything
            if this.IsVar u then
                true
            // If the pattern isn't a variable but the subject term is, don't match
            elif this.IsVar t then
                t.op = u.op // Only match if they are the exact same variable
            // Otherwise, both are non-variables, match if operators match and all args match
            else
                t.op = u.op
                && List.length t.args = List.length u.args
                && List.forall2 (fun a b -> this.IsMatch(a, b)) t.args u.args

        member this.MakeMapBind(t: Term, l: Term) =
            let rec makeBindMap (variables: Variables) (t: Term) (l: Term) : Map<Term, Term> =
                if variables.IsVar(l) then
                    Map.empty |> Map.add l t
                else
                    List.zip t.args l.args
                    |> List.fold
                        (fun acc (ta, la) ->
                            let subMap = makeBindMap variables ta la
                            Map.fold (fun m key value -> Map.add key value m) acc subMap)
                        Map.empty

            makeBindMap this t l

    type Operators =
        | Ops of Map<string, Operator>

        member this.GetEquals(t: Term) =
            let (Ops ops) = this

            match Map.tryFind t.op ops with
            | Some op -> op.eqs
            | None -> []

    type Module =
        { name: string
          sort: Sorts
          variables: Variables
          operators: Operators }

        member this.matchingEquals(t: Term) =
            let eqs = this.operators.GetEquals t
            let variables = this.variables

            eqs |> List.filter (fun eq -> variables.IsMatch(t, eq.left))

    type Statement =
        | StatementSort of string * Sort
        | StatementVariable of string * Variable
        | StatementOperator of string * Operator
        | StatementEqual of string * Equation

module Parser =

    open AST
    open System
    open FParsec

    open FSharp.Core

    let isAlphaNum c = isAsciiLetter c || isDigit c || c = '_'

    let ws = spaces

    // a simple identifier: one or more alphanumeric characters
    let identifier: Parser<string, unit> = many1Satisfy isAlphaNum .>> ws

    // parse a sort statement: "sort <id> ."
    let pSort: Parser<Statement, unit> =
        pstring "sort" >>. ws >>. identifier .>> ws .>> pchar '.' .>> ws
        |>> fun n -> StatementSort(n, Sort)

    // parse a variable statement: "var <id> : <id> ."
    let pVariable: Parser<Statement, unit> =
        pipe2
            (pstring "var" >>. ws >>. identifier)
            (ws >>. pchar ':' >>. ws >>. identifier .>> ws .>> pchar '.' .>> ws)
            (fun n s -> StatementVariable(n, { sort = s }))

    // parse an operator statement, e.g.:
    //    op f : a b -> c .
    let opName: Parser<string, unit> =
        // operator name: a sequence of alphanumerics (optionally with underscores that we skip)
        many1Satisfy isAlphaNum .>>. many (pchar '_') |>> fst .>> ws

    let pOperator: Parser<Statement, unit> =
        pipe3
            (pstring "op" >>. ws >>. opName)
            (pchar ':'
             >>. ws
             >>. manyCharsTill anyChar (attempt (ws >>. pstring "->" >>. ws))
             |>> (fun s -> s.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList))
            (many1Satisfy isAlphaNum .>> ws .>> pchar '.' .>> ws)
            (fun name args result ->
                StatementOperator(
                    name,
                    { argSorts = args
                      result = result
                      eqs = [] }
                ))

    // forward-declare pTerm so that pEqual can use it
    //  ERROR:  Only simple variable patterns can be bound in 'let rec' constructs
    let (pTerm, pTermRef) = createParserForwardedToRef<Term, unit> ()

    // parse an equality (rewrite rule) statement, e.g.:
    //    eq f(x) = g(x) .
    let pEqual: Parser<Statement, unit> =
        pipe2
            (pstring "eq" >>. ws >>. pTerm)
            (ws >>. pchar '=' >>. ws >>. pTerm .>> ws .>> pchar '.' .>> ws)
            (fun l r ->
                // use the operator of the left term as the key
                StatementEqual(l.op, { left = l; right = r }))

    // parse a term. This parser supports both parenthesized argument lists
    // (e.g. "f(a, b)") and simple function application (e.g. "f a").
    let pTermPrefix =
        pipe2
            identifier
            (choice
                [
                  // if there is a parenthesized argument list: "f ( a, b, c )"
                  attempt (between (pchar '(' >>. ws) (ws >>. pchar ')') (sepBy pTerm (pchar ',' >>. ws)))
                  // otherwise, if there is another term immediately after, treat it as a single argument
                  attempt (ws >>. pTerm |>> (fun t -> [ t ]))
                  // or no argument at all
                  preturn [] ])
            (fun op args -> { op = op; args = args })

    do pTermRef.Value <- pTermPrefix

    // a statement can be any of the four kinds
    let pStatement: Parser<Statement, unit> =
        choice [ attempt pSort; attempt pVariable; attempt pOperator; attempt pEqual ]
        .>> ws

    // many statements until the "endfm" keyword
    let pStatements: Parser<Statement list, unit> = many pStatement

    let pickout fn list = list |> List.choose fn |> Map.ofList

    // parse a module:
    //    fmod <moduleName> is <statements> endfm
    let pModule: Parser<Module, unit> =
        pipe2
            (ws >>. pstring "fmod" >>. ws >>. identifier .>> ws .>> pstring "is" .>> ws)
            (pStatements .>> pstring "endfm")
            (fun name statements ->
                let mSort =
                    statements
                    |> pickout (function
                        | StatementSort(n, v) -> Some(n, v)
                        | _ -> None)

                let mVar =
                    statements
                    |> pickout (function
                        | StatementVariable(n, v) -> Some(n, v)
                        | _ -> None)

                let equals =
                    statements
                    |> List.choose (function
                        | StatementEqual(n, eq) -> Some(n, eq)
                        | _ -> None)

                let mOp =
                    statements
                    |> List.choose (function
                        | StatementOperator(n, op) -> Some(n, op)
                        | _ -> None)
                    |> List.map (fun (n, op) ->
                        // attach any equalities with matching operator name
                        let opEquals = equals |> List.filter (fun (n2, _) -> n = n2) |> List.map snd
                        (n, { op with eqs = opEquals }))
                    |> Map.ofList

                { name = name
                  sort = Sorts(mSort)
                  variables = Vars(mVar)
                  operators = Ops(mOp) })

    let toResult (r: ParserResult<_, _>) =
        match r with
        | Success(result, _, _) -> Ok result
        | e -> Error(sprintf "%A" e)

    // Helper: run a parser and return a result
    let runParser (p) (input: string) : Result<_, _> =
        let result = run p input

        match result with
        | Success(result, _, _) -> Ok result
        | e -> Error(sprintf "%A" e)

    // Read a module from a string
    let readModule (input: string) : Result<Module, string> = runParser pModule input

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

            /// Log of terms produced during reduction (in chronological order)
            trace: Term list
        }

        /// Add a term to the reduction trace
        member this.AddTrace(t: Term) = { this with trace = this.trace @ [t] }

        /// Create an initial reduction state
        static member Create(m: Module) = { activeModule = m; trace = [] }

    /// The core rewriting function that applies equations to reduce a term
    let rewrite (state: ReductionState) (t: Term) : Term * ReductionState =
        // Add the initial term to trace
        let state = state.AddTrace t
        
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