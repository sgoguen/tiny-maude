namespace TinyMaude

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
                  sort = mSort
                  variables = mVar
                  operators = mOp })

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
