namespace TinyMaude

module ReductionEngine =

    open System
    open FSharp.Core
    open AST
    open Parser

    // -----------------------------------------------------------------------------
    // Command and Reduction Engine
    // -----------------------------------------------------------------------------
    type Command =
        { forModule: Module
          termLog: Term list }

    // isMatch returns true if either t or u represents a variable (looked up in the module)
    // or if they are compound terms with the same number of arguments and all children match.
    let rec isMatch (cmd: Command) (t: Term) (u: Term) : bool =
        // Check if u (the pattern) contains a variable
        let patternIsVar = Map.containsKey u.op cmd.forModule.variables
        
        // If the pattern is a variable, it can match anything
        if patternIsVar then
            true
        // If the pattern isn't a variable but the subject term is, don't match
        elif Map.containsKey t.op cmd.forModule.variables then
            t.op = u.op // Only match if they are the exact same variable
        // Otherwise, both are non-variables, match if operators match and all args match
        else
            t.op = u.op &&
            List.length t.args = List.length u.args &&
            List.forall2 (fun a b -> isMatch cmd a b) t.args u.args

    // makeBindMap builds a binding from a pattern term (l) to a term (t)
    // If l is a variable (i.e. found in the module’s variable map), then return a singleton map;
    // otherwise, recursively build a binding map from corresponding subterms.
    let rec makeBindMap (cmd: Command) (t: Term) (l: Term) : Map<Term, Term> =
        let isVariable term =
            Map.containsKey term.op cmd.forModule.variables

        if isVariable l then
            Map.empty |> Map.add l t
        else
            List.zip t.args l.args
            |> List.fold
                (fun acc (ta, la) ->
                    let subMap = makeBindMap cmd ta la
                    Map.fold (fun m key value -> Map.add key value m) acc subMap)
                Map.empty

    // bindVariable applies the binding map to a term, replacing any subterm found in the map.
    let rec bindVariable (bm: Map<Term, Term>) (r: Term) : Term =
        match Map.tryFind r bm with
        | Some t -> t
        | None ->
            { r with
                args = r.args |> List.map (bindVariable bm) }

    // termEqual retrieves the list of equality rules associated with the operator of term t.
    let termEqual (cmd: Command) (t: Term) : Equal list =
        match Map.tryFind t.op cmd.forModule.operators with
        | Some op -> op.eqs
        | None -> []

    // termVariable checks if the term’s operator is defined as a variable.
    let termVariable (cmd: Command) (t: Term) : Variable option =
        Map.tryFind t.op cmd.forModule.variables

    // The rewriting function (similar to Haskell’s replace)
    // It tries to apply an equality (rewrite rule) to the term t.
    // If a rule matches, it builds the binding, substitutes into the rule’s right‐hand side,
    // and then recursively continues until no further rewrite applies.
    let rec replace (cmd: Command) (t: Term) : Term * Command =
        let eqs = termEqual cmd t
        let matchingEquals = eqs |> List.filter (fun eq -> isMatch cmd t eq.left)

        match matchingEquals with
        | [] ->
            // No rule applied – recursively process each subterm
            let newArgs, newCmd =
                t.args
                |> List.fold
                    (fun (acc, state) arg ->
                        let newArg, state' = replace state arg
                        (acc @ [ newArg ], state'))
                    ([], cmd)

            let newTerm = { t with args = newArgs }

            if newTerm = t then
                (newTerm, newCmd)
            else
                let updatedCmd =
                    { newCmd with
                        termLog = newTerm :: newCmd.termLog }

                replace updatedCmd newTerm
        | eq :: _ ->
            let bm = makeBindMap cmd t eq.left
            let substituted = bindVariable bm eq.right
            replace cmd substituted

    // reduce takes a module and a term, initializes the command state, and performs rewriting.
    let reduce (m: Module) (t: Term) : Term * Command =
        let initCmd = { forModule = m; termLog = [] }
        replace initCmd t

    // Pretty-print a term
    let rec showTerm (t: Term) : string =
        match t.args with
        | [] -> t.op
        | [ a ] -> t.op + " " + showTerm a
        | args -> t.op + " (" + (String.Join(", ", args |> List.map showTerm)) + ")"