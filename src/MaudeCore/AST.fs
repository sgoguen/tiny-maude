namespace TinyMaude

//  Inspired by https://github.com/ncaq/sub-maude/

module AST =

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
            | args -> sprintf "%s(%s)" t.op (String.concat ", " (List.map toString args))

    [<StructuredFormatDisplay("eq {left} = {right}")>]
    type Equal = { left: Term; right: Term }

    [<StructuredFormatDisplay("{argSorts} -> {result}")>]
    type Operator =
        { argSorts: SortId list
          result: SortId
          eqs: Equal list }

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
        | StatementEqual of string * Equal
