open Extensions
open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions
open InputUtils

type PairInsertionLineRegex = Regex< @"(?<Left>[A-Z])(?<Right>[A-Z]) -> (?<Insert>[A-Z])">

type InsertionMap = Map<char*char, char>
type ResolvedMap = Map<char*char, (char*char) list>

let oneStep (polymerTemplate: char list) (rules: InsertionMap) =
    polymerTemplate
    |> List.pairwise
    |> List.collect (fun pair ->
        match rules.TryFind pair with
        | Some insert -> [insert; snd pair]
        | None -> List.singleton (snd pair)
    )
    |> (fun tail -> (List.head polymerTemplate)::tail)

let nSteps n polymerTemplate rules =
    let mutable template = polymerTemplate
    for _ = n downto 1 do
        template <- oneStep template rules

    template

let part1 (polymerTemplate: char list) (rules: InsertionMap) =
    let letterCounts =
        nSteps 10 polymerTemplate rules
        |> List.countBy id

    let min =
        List.minBy snd letterCounts
        |> snd

    let max =
        List.maxBy snd letterCounts
        |> snd

    max - min

let nStepsWithPairs (n: int) (polymerTemplate: char list) (rules: ResolvedMap) =
    let oneStepWithPairs (countMap: list<(char*char) * int64>) =
        countMap
        |> List.collect (fun (pair, cnt) ->
            match rules.TryFind pair with
            | Some pairs ->
                pairs
                |> List.map (fun p -> p, cnt)
            | None -> [pair, cnt]
            )
        |> List.mergePairsBy (+)
    
    let mutable countMap =
        polymerTemplate
        |> List.pairwise
        |> List.countInt64
        |> Map.toList

    for i = 1 to n do
        countMap <- oneStepWithPairs countMap

    countMap
  
let resolveInsertionMap (rules: InsertionMap): ResolvedMap =
    rules
    |> Map.map (fun (left, right) insert -> [ left,insert; insert,right ])

let part2 (polymerTemplate: char list) (rules: InsertionMap) =
    let lastChar =
        polymerTemplate
        |> List.last

    let letterCounts =
        rules
        |> resolveInsertionMap
        |> nStepsWithPairs 40 polymerTemplate
        |> List.map (fun ((left, _), cnt) -> left, cnt)
        |> List.mergePairsBy (+)
        |> List.map (fun (ch, cnt) -> if ch = lastChar then ch, cnt+1L else ch, cnt)

    let min =
        List.minBy snd letterCounts
        |> snd

    let max =
        List.maxBy snd letterCounts
        |> snd

    max - min

[<EntryPoint>]
let main argv =
    let pairInsertionLineRegex = PairInsertionLineRegex()

    let input =
        System.IO.File.ReadLines (getFileName argv)
        |> Seq.splitWhen ((=) "")

    let polymerTemplate =
        input
        |> Seq.head
        |> Seq.exactlyOne
        |> List.ofSeq

    let rules =
        input
        |> Seq.tail
        |> Seq.head
        |> Seq.map pairInsertionLineRegex.TypedMatch
        |> Seq.map (fun m -> (m.Left.AsChar, m.Right.AsChar), m.Insert.AsChar)
        |> Map.ofSeq

    part1 polymerTemplate rules
    |> printfn "%d"

    part2 polymerTemplate rules
    |> printfn "%d"

    0

