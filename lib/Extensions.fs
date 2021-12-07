﻿module Extensions

let (|LargerThan|Equal|LessThan|) (a, b) =
    match compare a b with
    | 0 -> Equal
    | x when x > 0 -> LargerThan
    | _ -> LessThan

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

module Map =
    let mapValues f (map: Map<_, _>) = Map.map (fun _ values -> f values) map
