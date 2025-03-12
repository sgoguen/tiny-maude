module Tests

open System
open Xunit

module Example =
    open FParsec
    open TinyMaude
    open TinyMaude.Parser
    open TinyMaude.ReductionEngine
    open Result
    open FSharp.Core
    open TinyMaude.AST


    let natModuleInput =
        """
    fmod NAT is
        sort Nat .
        var N : Nat .
        var M : Nat .

        op z : -> Nat .
        op s_ : Nat -> Nat .

        op p : Nat Nat -> Nat .
        op m : Nat Nat -> Nat .
        op w : Nat Nat -> Nat .
        op sq : Nat -> Nat .

        eq p (N, z) = N .
        eq p (N, s M) = s (p (N, M)) .

        eq m (N, z) = z .
        eq m (N, s z) = N .
        eq m (N, s M) = p (N, m (N, M)) .

        eq w (N, z) = s z .
        eq w (N, s M) = m (N, w (N, M)) .

        eq sq (M) = m (M, M) .
    endfm
    """

    let evalNatTerm (term: string) =
        result {
            let! natMod = readModule natModuleInput
            let! term1 = run pTerm term |> toResult
            let (r, c) = reduce natMod term1
            return r |> Term.toString
        } |> Result.defaultWith (fun _ -> "")

    [<Fact>]
    let ``M + M = M + M`` () =
        let result = evalNatTerm "p(M, M)"
        // This is failing, the resulting is coming back M
        Assert.Equal("p(M, M)", result)
    
    [<Fact>]
    let ``sq(M) = M * M`` () =
        let result = evalNatTerm "sq(M)"
        // This is failing, the resulting is coming back M
        Assert.Equal("m(M, M)", result)

    [<Fact>]
    let ``3 + 3 = 6`` () =
        let result = evalNatTerm "p ( s (s (s z)), s (s (s z)))"
        Assert.Equal("s(s(s(s(s(s(z))))))", result)

    [<Fact>]
    let ``3 * 3 = 9`` () =
        let result = evalNatTerm "m ( s (s (s z)), s (s (s z)))"
        Assert.Equal("s(s(s(s(s(s(s(s(s(z)))))))))", result)

    [<Fact>]
    let ``(3 * 3) ^ 2 = 81`` () =
        let result = evalNatTerm "sq (m (s (s (s z)), s (s (s z))))"
        let count = result |> Seq.filter (fun c -> c = 's') |> Seq.length
        Assert.Equal(81, count)

