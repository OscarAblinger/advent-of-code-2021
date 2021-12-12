open Extensions
open FSharp.Text.RegexProvider
open InputUtils

type LineRegex = Regex< @"(?<From>[a-zA-Z]+)-(?<To>[a-zA-Z]+)">

type Cave =
    | Start
    | End
    | Big of string
    | Small of string

    override this.ToString () =
        match this with
        | Start -> "start"
        | End -> "end"
        | Big str -> str
        | Small str -> str

let parseCave str =
    match str with
    | "start" -> Start
    | "end" -> End
    | lower when lower = lower.ToLower () -> Small lower
    | upper when upper = upper.ToUpper () -> Big upper
    | _ -> failwithf "unknown cave found: %s" str

let toUndirectedGraph (caves: (Cave*Cave) list) =
    let directedForwardGraph =
        caves
        |> List.groupBy fst
        |> Map.ofList
        |> Map.mapValues (List.map snd)

    let directedBackwardGraph =
        caves
        |> List.groupBy snd
        |> Map.ofList
        |> Map.mapValues (List.map fst)

    Map.merge (fun l1 l2 -> l1 @ l2) directedForwardGraph directedBackwardGraph

let allSequences (caves: Map<Cave, Cave list>) =
    let rec recAllSequences (finishedSequences: Cave list list) (stack: Cave list list) =
        if stack.IsEmpty then
            finishedSequences
        else
            let current::rest = stack
            let currentCave::_ = current

            let nextCaves =
                caves
                |> Map.tryFind currentCave

            if Option.isNone nextCaves then
                recAllSequences finishedSequences rest
            else
                let (finished, unfinished) =
                    Option.get nextCaves
                    |> List.filter (fun cave ->
                        match cave with
                        | Start -> false // never go back to start
                        | Small _ -> if List.contains cave current then false else true
                        | Big _ -> true
                        | End -> true
                        )
                    |> List.map (fun l -> l::current)
                    |> List.partition (fun l -> l.[0] = End) // always stop when at end

                recAllSequences (finished @ finishedSequences) (unfinished @ rest)

    caves.[Start]
    |> List.map (fun cave -> [cave; Start])
    |> recAllSequences List.empty

let part1 (caves: list<Cave*Cave>) =
    caves
    |> toUndirectedGraph
    |> allSequences
    |> List.length

let allSequencesWithMaxOneSmallCaveTwice (caves: Map<Cave, Cave list>) =
    let rec recAllSequencesWithMaxOneSmallCaveTwice (finishedSequences: Cave list list) (stack: (bool * Cave list) list) =
        if stack.IsEmpty then
            finishedSequences
        else
            let (hadDoubleVisit, current)::rest = stack
            let currentCave::_ = current

            let nextCaves =
                caves
                |> Map.tryFind currentCave

            if Option.isNone nextCaves then
                recAllSequencesWithMaxOneSmallCaveTwice finishedSequences rest
            else
                let (finished, unfinished) =
                    Option.get nextCaves
                    |> List.choose (fun cave ->
                        match cave with
                        | Start -> None // never go back to start
                        | Big _ -> Some (hadDoubleVisit, cave)
                        | End -> Some (hadDoubleVisit, cave)
                        | Small _ ->
                            let isDoubleVisit = List.contains cave current
                            if hadDoubleVisit && isDoubleVisit then
                                None 
                            else
                                Some (hadDoubleVisit || isDoubleVisit, cave)
                        )
                    |> List.map (fun (hdv, cave) -> hdv, cave::current)
                    |> List.partition (fun (_, l) -> l.[0] = End) // always stop when at end

                let finishedList =
                    finished
                    |> List.map snd

                recAllSequencesWithMaxOneSmallCaveTwice (finishedList @ finishedSequences) (unfinished @ rest)

    caves.[Start]
    |> List.map (fun cave -> false, [cave; Start])
    |> recAllSequencesWithMaxOneSmallCaveTwice List.empty

let part2 (caves: list<Cave*Cave>) =
    caves
    |> toUndirectedGraph
    |> allSequencesWithMaxOneSmallCaveTwice
    |> List.length

[<EntryPoint>]
let main argv =
    let regex = LineRegex()

    let input =
        System.IO.File.ReadLines (getFileName argv)
        |> Seq.map regex.TypedMatch
        |> Seq.map (fun m -> parseCave m.From.Value, parseCave m.To.Value)
        |> List.ofSeq

    part1 input
    |> printfn "%d"

    part2 input
    |> printfn "%d"

    0

