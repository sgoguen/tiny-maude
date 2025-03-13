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
