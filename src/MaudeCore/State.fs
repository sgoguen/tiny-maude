module TinyMaude.Stateful

/// A stateful computation.
type Stateful<'state, 'result> = State of ('state -> 'result * 'state)

module State =

    /// 'state -> Stateful<'state, 'result> -> ('result * 'state)
    let run state (State f) = f state

    /// 'result -> Stateful<'state, 'result>
    let ret result = State(fun state -> (result, state))

    /// ('a -> Stateful<'state, 'b>) -> Stateful<'state, 'a> -> Stateful<'state, 'b>
    let bind binder stateful =
        State(fun state ->
            let result, state' = stateful |> run state
            binder result |> run state')

module Builder =

    type StatefulBuilder() =
        let (>>=) stateful binder = State.bind binder stateful
        member __.Return result = State.ret result
        member __.ReturnFrom stateful = stateful
        member __.Bind(stateful, binder) = stateful >>= binder
        member __.Zero() = State.ret ()
        member __.Combine(statefulA, statefulB) = statefulA >>= fun _ -> statefulB
        member __.Delay f = f ()

    let state = StatefulBuilder()
