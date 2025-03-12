namespace TinyMaude

module ReductionEngine =

    open System
    open FSharp.Core
    open AST

    // -----------------------------------------------------------------------------
    // Command and Reduction Engine
    // -----------------------------------------------------------------------------
    type Command =
        { forModule: Module
          termLog: Term list }

        member this.AddTerm(t: Term) =
            { this with
                termLog = t :: this.termLog }

    // makeBindMap builds a binding from a pattern term (l) to a term (t)
    // If l is a variable (i.e. found in the module’s variable map), then return a singleton map;
    // otherwise, recursively build a binding map from corresponding subterms.
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

    // bindVariable applies the binding map to a term, replacing any subterm found in the map.
    let rec bindVariable (bm: Map<Term, Term>) (r: Term) : Term =
        match Map.tryFind r bm with
        | Some t -> t
        | None ->
            { r with
                args = r.args |> List.map (bindVariable bm) }

    // The rewriting function (similar to Haskell’s replace)
    // It tries to apply an equality (rewrite rule) to the term t.
    // If a rule matches, it builds the binding, substitutes into the rule’s right‐hand side,
    // and then recursively continues until no further rewrite applies.
    let rec replace (cmd: Command) (t: Term) : Term * Command =
        let m = cmd.forModule

        match m.matchingEquals (t) with
        | [] ->
            // No rule applied – recursively process each subterm
            let newArgs, newCmd =
                t.args
                |> List.fold
                    (fun (acc, state) arg ->
                        let newArg, state' = replace state arg
                        acc @ [ newArg ], state')
                    ([], cmd)

            let newTerm = { t with args = newArgs }

            if newTerm = t then
                newTerm, newCmd
            else
                let updatedCmd = newCmd.AddTerm newTerm
                replace updatedCmd newTerm
        | eq :: _ ->
            let bm = makeBindMap m.variables t eq.left
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
        | args -> t.op + " (" + String.Join(", ", args |> List.map showTerm) + ")"
