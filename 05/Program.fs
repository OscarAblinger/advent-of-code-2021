open Extensions
open FSharp.Text.RegexExtensions
open FSharp.Text.RegexProvider
open InputUtils

type LineRegex = Regex< @"(?<X1>\d+),(?<Y1>\d+) -> (?<X2>\d+),(?<Y2>\d+)">

type LineDefinition = (int * int) * (int * int)
type LineMap = Map<int, (int*int) list>

module LineDefinition =
    let expand (ld: LineDefinition) = 
        let pointsBetween a b =
            if a > b then
                [-a .. -b]
                |> List.map (fun x -> -x)
            else
                [a..b]

        match ld with
        | ((x1, y1), (x2, y2)) when x1 = x2 && y1 = y2 -> [ (x1, x2) ]
        | ((x1, y1), (x2, y2)) when x1 = x2 -> List.map (fun y -> (x1, y)) (pointsBetween y1 y2)
        | ((x1, y1), (x2, y2)) when y1 = y2 -> List.map (fun x -> (x, y1)) (pointsBetween x1 x2)
        | ((x1, y1), (x2, y2)) -> List.zip (pointsBetween x1 x2) (pointsBetween y1 y2)

let part1 (lines: LineDefinition list) =
    lines
    |> List.filter (fun ((x1, y1), (x2, y2)) -> x1 = x2 || y1 = y2)
    |> List.collect LineDefinition.expand
    |> List.countBy id
    |> List.filter (snd >> (<=)2)
    |> List.length

let part2 (lines: LineDefinition list) =
    lines
    |> List.collect LineDefinition.expand
    |> List.countBy id
    |> List.filter (snd >> (<=)2)
    |> List.length


[<EntryPoint>]
let main argv =
    let lineRegex = LineRegex()

    let input =
        System.IO.File.ReadLines (getFileName argv)
        |> Seq.map lineRegex.TypedMatch
        |> Seq.map (fun m -> ((m.X1.AsInt, m.Y1.AsInt), (m.X2.AsInt, m.Y2.AsInt)))
        |> Seq.toList

    part1 input
    |> printfn "%d"

    Efficient.efficientPart1 input
    |> printfn "%d"

    part2 input
    |> printfn "%d"

    0

