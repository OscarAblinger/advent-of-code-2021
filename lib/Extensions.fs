module Extensions

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

    let merge (convert: 'b -> 'b -> 'b) (m1: Map<'a, 'b>) (m2: Map<'a, 'b>): Map<'a, 'b> =
        Map.fold (fun acc key value ->
            Map.change key (fun el ->
                el
                |> Option.map (convert value)
                |> Option.orElse (Some value)
            ) acc
        ) m2 m1

module MultiMap =
    let union (m1: Map<'a, Set<'b>>) (m2: Map<'a, Set<'b>>) =
        Map.fold (fun acc key value ->
            Map.change key (fun el ->
                    el
                    |> Option.map (Set.union value)
                    |> Option.orElse (Some value)
                ) acc
            ) m1 m2

    let intersection (m1: Map<'a, Set<'b>>) (m2: Map<'a, Set<'b>>) =
        Map.fold (fun acc key value ->
            Map.change key (fun el ->
                    el
                    |> Option.map (Set.intersect value)
                    |> Option.bind (fun s -> if s.IsEmpty then None else Some s)
                ) acc
            ) m1 m2
