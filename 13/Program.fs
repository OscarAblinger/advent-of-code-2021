open Extensions
open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions
open InputUtils

type DotLineRegex = Regex< @"(?<X>\d+),(?<Y>\d+)">
type FoldLineRegex = Regex< @"fold along (?<Direction>x|y)=(?<Value>\d+)">

type Dot = int * int
type Fold =
    | AlongX of int
    | AlongY of int

    static member along (direction: char) (value: int) =
        match direction with
        | 'x' -> AlongX value
        | 'y' -> AlongY value
        | _ -> failwithf "Unknown direction: %c" direction

let printDots (dots: Set<Dot>) =
    let dotLines =
        dots
        |> Set.toList
        |> List.groupBy snd
        |> List.map (fun (y, dotsAtY) -> (y, List.map fst dotsAtY))
        |> Map.ofList

    let maxX =
        dots
        |> Seq.maxBy fst
        |> fst

    let maxY =
        dots
        |> Seq.maxBy snd
        |> snd

    for y = 0 to maxY do
        for x = 0 to maxX do
            if dotLines.ContainsKey y && List.contains x dotLines.[y] then
                printf "#"
            else
                printf "."
        printfn ""

let foldADot (fold: Fold) ((x, y): Dot) =
    match fold with
    | AlongX fx -> if x > fx then 2*fx - x, y else x, y
    | AlongY fy -> if y > fy then x, 2*fy - y else x, y

let fold (dots: Set<Dot>) (fold: Fold) =
    dots
    |> Set.map (foldADot fold)

let part1 (dots: Set<Dot>) (folds: Fold list) =
    folds
    |> List.head
    |> fold dots
    |> Set.count

let part2_print (dots: Set<Dot>) (folds: Fold list) =
    folds
    |> List.fold fold dots
    |> printDots


[<EntryPoint>]
let main argv =
    let dotLineRegex = DotLineRegex()
    let foldLineRegex = FoldLineRegex()

    let input =
        System.IO.File.ReadLines (getFileName argv)
        |> Seq.splitWhen ((=) "")

    let dots =
        input
        |> Seq.head
        |> Seq.map (fun line -> dotLineRegex.TypedMatch(line))
        |> Seq.map (fun m -> (m.X.AsInt, m.Y.AsInt))
        |> Set.ofSeq

    let folds =
        input
        |> Seq.tail
        |> Seq.head
        |> Seq.map (fun line -> foldLineRegex.TypedMatch(line))
        |> Seq.map (fun m -> Fold.along m.Direction.AsChar m.Value.AsInt)
        |> List.ofSeq

    part1 dots folds
    |> printfn "%d"

    part2_print dots folds

    0

