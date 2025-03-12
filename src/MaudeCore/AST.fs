namespace TinyMaude

//  Inspired by https://github.com/ncaq/sub-maude/

module AST =

    type SortId = string
    type OpId = string
    type Sort = Sort

    [<StructuredFormatDisplay("{sort}")>]
    type Variable = { sort: SortId }

    let Variable sort = { sort = sort }

    [<StructuredFormatDisplay("{op}({args})")>]
    type Term = { op: OpId; args: Term list }

    [<StructuredFormatDisplay("eq {left} = {right}")>]
    type Equal = { left: Term; right: Term }

    [<StructuredFormatDisplay("{argSorts} -> {result}")>]
    type Operator =
        { argSorts: SortId list
          result: SortId
          eqs: Equal list }




    let rec termToString (t: Term) =
        match t.args with
        | [] -> t.op
        | args -> sprintf "%s(%s)" t.op (String.concat ", " (List.map termToString args))

    let rec equalToString (e: Equal) =
        sprintf "%s = %s" (termToString e.left) (termToString e.right)

    let Operator (args, result, equals) =
        { argSorts = args
          result = result
          eqs = equals }

    let Equal (left, right) = { left = left; right = right }
    let Term (op, args) = { op = op; args = args }

    type Module =
        { name: string
          sort: Map<string, Sort>
          variables: Map<string, Variable>
          operators: Map<string, Operator> }

    type Statement =
        | StatementSort of string * Sort
        | StatementVariable of string * Variable
        | StatementOperator of string * Operator
        | StatementEqual of string * Equal

// -----------------------------------------------------------------------------
// Parsing with FParsec
// -----------------------------------------------------------------------------

// module Parser =

//     open AST
//     open System
//     open FParsec

//     open FSharp.Core

//     let isAlphaNum c = isAsciiLetter c || isDigit c || c = '_'

//     let ws = spaces

//     // a simple identifier: one or more alphanumeric characters
//     let identifier: Parser<string, unit> = many1Satisfy isAlphaNum .>> ws

//     // parse a sort statement: "sort <id> ."
//     let pSort: Parser<Statement, unit> =
//         pstring "sort" >>. ws >>. identifier .>> ws .>> pchar '.' .>> ws
//         |>> fun n -> StatementSort(n, Sort)

//     // parse a variable statement: "var <id> : <id> ."
//     let pVariable: Parser<Statement, unit> =
//         pipe2
//             (pstring "var" >>. ws >>. identifier)
//             (ws >>. pchar ':' >>. ws >>. identifier .>> ws .>> pchar '.' .>> ws)
//             (fun n s -> StatementVariable(n, { sort = s }))

//     // parse an operator statement, e.g.:
//     //    op f : a b -> c .
//     let opName: Parser<string, unit> =
//         // operator name: a sequence of alphanumerics (optionally with underscores that we skip)
//         many1Satisfy isAlphaNum .>>. many (pchar '_') |>> fst .>> ws

//     let pOperator: Parser<Statement, unit> =
//         pipe3
//             (pstring "op" >>. ws >>. opName)
//             (pchar ':'
//              >>. ws
//              >>. manyCharsTill anyChar (attempt (ws >>. pstring "->" >>. ws))
//              |>> (fun s -> s.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList))
//             (many1Satisfy isAlphaNum .>> ws .>> pchar '.' .>> ws)
//             (fun name args result ->
//                 StatementOperator(
//                     name,
//                     { argSorts = args
//                       result = result
//                       eqs = [] }
//                 ))

//     // forward-declare pTerm so that pEqual can use it
//     //  ERROR:  Only simple variable patterns can be bound in 'let rec' constructs
//     let (pTerm, pTermRef) = createParserForwardedToRef<Term, unit> ()

//     // parse an equality (rewrite rule) statement, e.g.:
//     //    eq f(x) = g(x) .
//     let pEqual: Parser<Statement, unit> =
//         pipe2
//             (pstring "eq" >>. ws >>. pTerm)
//             (ws >>. pchar '=' >>. ws >>. pTerm .>> ws .>> pchar '.' .>> ws)
//             (fun l r ->
//                 // use the operator of the left term as the key
//                 StatementEqual(l.op, { left = l; right = r }))

//     // parse a term. This parser supports both parenthesized argument lists
//     // (e.g. "f(a, b)") and simple function application (e.g. "f a").
//     let pTermPrefix =
//         pipe2
//             identifier
//             (choice
//                 [
//                   // if there is a parenthesized argument list: "f ( a, b, c )"
//                   attempt (between (pchar '(' >>. ws) (ws >>. pchar ')') (sepBy pTerm (pchar ',' >>. ws)))
//                   // otherwise, if there is another term immediately after, treat it as a single argument
//                   attempt (ws >>. pTerm |>> (fun t -> [ t ]))
//                   // or no argument at all
//                   preturn [] ])
//             (fun op args -> { op = op; args = args })

//     do pTermRef.Value <- pTermPrefix

//     // a statement can be any of the four kinds
//     let pStatement: Parser<Statement, unit> =
//         choice [ attempt pSort; attempt pVariable; attempt pOperator; attempt pEqual ]
//         .>> ws

//     // many statements until the "endfm" keyword
//     let pStatements: Parser<Statement list, unit> = many pStatement

//     let pickout fn list = list |> List.choose fn |> Map.ofList

//     // parse a module:
//     //    fmod <moduleName> is <statements> endfm
//     let pModule: Parser<Module, unit> =
//         pipe2
//             (ws >>. pstring "fmod" >>. ws >>. identifier .>> ws .>> pstring "is" .>> ws)
//             (pStatements .>> pstring "endfm")
//             (fun name statements ->
//                 let mSort =
//                     statements
//                     |> pickout (function
//                         | StatementSort(n, v) -> Some(n, v)
//                         | _ -> None)

//                 let mVar =
//                     statements
//                     |> pickout (function
//                         | StatementVariable(n, v) -> Some(n, v)
//                         | _ -> None)

//                 let equals =
//                     statements
//                     |> List.choose (function
//                         | StatementEqual(n, eq) -> Some(n, eq)
//                         | _ -> None)

//                 let mOp =
//                     statements
//                     |> List.choose (function
//                         | StatementOperator(n, op) -> Some(n, op)
//                         | _ -> None)
//                     |> List.map (fun (n, op) ->
//                         // attach any equalities with matching operator name
//                         let opEquals = equals |> List.filter (fun (n2, _) -> n = n2) |> List.map snd
//                         (n, { op with eqs = opEquals }))
//                     |> Map.ofList

//                 { name = name
//                   sort = mSort
//                   variables = mVar
//                   operators = mOp })

//     let toResult (r: ParserResult<_, _>) =
//         match r with
//         | Success(result, _, _) -> Ok result
//         | e -> Error(sprintf "%A" e)

//     // Helper: run a parser and return a result
//     let runParser (p) (input: string) : Result<_, _> =
//         let result = run p input

//         match result with
//         | Success(result, _, _) -> Ok result
//         | e -> Error(sprintf "%A" e)

//     // Read a module from a string
//     let readModule (input: string) : Result<Module, string> = runParser pModule input


// module ReductionEngine =

//     open System
//     open FSharp.Core
//     open AST
//     open Parser

//     // -----------------------------------------------------------------------------
//     // Command and Reduction Engine
//     // -----------------------------------------------------------------------------
//     type Command =
//         { forModule: Module
//           termLog: Term list }

//     // isMatch returns true if either t or u represents a variable (looked up in the module)
//     // or if they are compound terms with the same number of arguments and all children match.
//     let rec isMatch (cmd: Command) (t: Term) (u: Term) : bool =
//         let varOr =
//             (Map.containsKey t.op cmd.forModule.variables)
//             || (Map.containsKey u.op cmd.forModule.variables)

//         let ta = t.args
//         let ua = u.args

//         let childMatch =
//             (List.length ta = List.length ua)
//             && (List.forall2 (fun a b -> isMatch cmd a b) ta ua)

//         varOr || childMatch

//     // makeBindMap builds a binding from a pattern term (l) to a term (t)
//     // If l is a variable (i.e. found in the module’s variable map), then return a singleton map;
//     // otherwise, recursively build a binding map from corresponding subterms.
//     let rec makeBindMap (cmd: Command) (t: Term) (l: Term) : Map<Term, Term> =
//         let isVariable term =
//             Map.containsKey term.op cmd.forModule.variables

//         if isVariable l then
//             Map.empty |> Map.add l t
//         else
//             List.zip t.args l.args
//             |> List.fold
//                 (fun acc (ta, la) ->
//                     let subMap = makeBindMap cmd ta la
//                     Map.fold (fun m key value -> Map.add key value m) acc subMap)
//                 Map.empty

//     // bindVariable applies the binding map to a term, replacing any subterm found in the map.
//     let rec bindVariable (bm: Map<Term, Term>) (r: Term) : Term =
//         match Map.tryFind r bm with
//         | Some t -> t
//         | None ->
//             { r with
//                 args = r.args |> List.map (bindVariable bm) }

//     // termEqual retrieves the list of equality rules associated with the operator of term t.
//     let termEqual (cmd: Command) (t: Term) : Equal list =
//         match Map.tryFind t.op cmd.forModule.operators with
//         | Some op -> op.eqs
//         | None -> []

//     // termVariable checks if the term’s operator is defined as a variable.
//     let termVariable (cmd: Command) (t: Term) : Variable option =
//         Map.tryFind t.op cmd.forModule.variables

//     // The rewriting function (similar to Haskell’s replace)
//     // It tries to apply an equality (rewrite rule) to the term t.
//     // If a rule matches, it builds the binding, substitutes into the rule’s right‐hand side,
//     // and then recursively continues until no further rewrite applies.
//     let rec replace (cmd: Command) (t: Term) : Term * Command =
//         let eqs = termEqual cmd t
//         let matchingEquals = eqs |> List.filter (fun eq -> isMatch cmd t eq.left)

//         match matchingEquals with
//         | [] ->
//             // No rule applied – recursively process each subterm
//             let newArgs, newCmd =
//                 t.args
//                 |> List.fold
//                     (fun (acc, state) arg ->
//                         let newArg, state' = replace state arg
//                         (acc @ [ newArg ], state'))
//                     ([], cmd)

//             let newTerm = { t with args = newArgs }

//             if newTerm = t then
//                 (newTerm, newCmd)
//             else
//                 let updatedCmd =
//                     { newCmd with
//                         termLog = newTerm :: newCmd.termLog }

//                 replace updatedCmd newTerm
//         | eq :: _ ->
//             let bm = makeBindMap cmd t eq.left
//             let substituted = bindVariable bm eq.right
//             replace cmd substituted

//     // reduce takes a module and a term, initializes the command state, and performs rewriting.
//     let reduce (m: Module) (t: Term) : Term * Command =
//         let initCmd = { forModule = m; termLog = [] }
//         replace initCmd t

//     // Pretty-print a term
//     let rec showTerm (t: Term) : string =
//         match t.args with
//         | [] -> t.op
//         | [ a ] -> t.op + " " + showTerm a
//         | args -> t.op + " (" + (String.Join(", ", args |> List.map showTerm)) + ")"


// #load "Result.fsx"

// module Example =
//     open FParsec
//     open Parser
//     open ReductionEngine
//     open Result


//     let natModuleInput =
//         """
//     fmod NAT is
//         sort Nat .
//         var N : Nat .
//         var M : Nat .

//         op z : -> Nat .
//         op s_ : Nat -> Nat .

//         op p : Nat Nat -> Nat .
//         op m : Nat Nat -> Nat .
//         op w : Nat Nat -> Nat .
//         op sq : Nat -> Nat .

//         eq p (N, z) = N .
//         eq p (N, s M) = s (p (N, M)) .

//         eq m (N, z) = z .
//         eq m (N, s z) = N .
//         eq m (N, s M) = p (N, m (N, M)) .

//         eq w (N, z) = s z .
//         eq w (N, s M) = m (N, w (N, M)) .

//         eq sq (M) = m (M, M) .
//     endfm
//     """

//     let (t) =
//         result {
//             let! natMod = readModule natModuleInput
//             let! term1 = run pTerm "sq (m (s (s (s z)), s (s (s z))))" |> toResult
//             let (r, c) = reduce natMod term1
//             return r |> AST.termToString
//         }

//     printfn "%A" t
