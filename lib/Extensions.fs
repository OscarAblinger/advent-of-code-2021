module Extensions

module Seq =
    /// Splits a sequence into subsequences at all elements that the given predicate returns true for
    /// The triggering element is included in the following subsequence
    let splitWith f input =
        let i = ref 0
        input
        |> Seq.groupBy (fun s -> (if f s then incr i) ; !i)
        |> Seq.map snd

    /// Splits a sequence into subsequence at all elements that the given predicate returns true for
    /// The triggering element is not included in any subsequence
    let splitWhen f input =
        let i = ref 0
        input
        |> Seq.choose (fun s ->
                        if f s then
                            incr i
                            None
                        else
                            Some (!i, s))
        |> Seq.groupBy fst
        |> Seq.map (snd >> Seq.map snd)

    let triplemap<'t, 'r> (f: 't -> 't -> 't -> 'r) (input: 't seq): 'r seq = seq {
        let mutable v1: 't option = None
        let mutable v2: 't option = None
        let mutable v3: 't option = None

        for el in input do
            v1 <- v2
            v2 <- v3
            v3 <- Some el

            match (v1, v2, v3) with
            | Some arg1, Some arg2, Some arg3 -> yield f arg1 arg2 arg3
            | _ -> ()
    }
