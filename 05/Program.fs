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
    let listOfLinesToMapOfStartAndFinish (keySelector: int*int -> int) (valuesSelector: int*int -> int) (lines: LineDefinition list) =
        let sortTuple ((a, b): int*int) =
            if a < b then
                (a, b)
            else
                (b, a)

        lines
        |> List.groupBy (fst >> keySelector)
        |> Map.ofList
        |> Map.mapValues (List.map (fun (start, finish) -> valuesSelector start, valuesSelector finish))
        // now sort the values so that fst < snd and that the fst of each element is <= that of the next
        // and if the fst of two elements are the same sort by snd
        |> Map.mapValues (List.map sortTuple)
        |> Map.mapValues (List.sortWith (fun (x1, y1) (x2, y2) -> if x1 = x2 then y1 - y2 else x1 - x2))


    let (verticalLines, horizontalLines) =
        lines
        |> List.filter (fun ((x1, y1), (x2, y2)) -> x1 = x2 || y1 = y2)
        |> List.partition (fun ((x1, _), (x2, _)) -> x1 = x2)

    let mappedVerticals = listOfLinesToMapOfStartAndFinish fst snd verticalLines
    let mappedHorizontals = listOfLinesToMapOfStartAndFinish snd fst horizontalLines

    let doublesInLines (map: LineMap) toTuple =
        let rec doublesInLine (doubles: int list) (line: (int*int) list): int list =
            match line with
            | [] -> doubles
            | [_] -> doubles
            | (_, y1)::(start, y2)::rest ->
                let finish = min y1 y2
                doublesInLine (doubles @ [start .. finish]) ((start, max y1 y2)::rest)

        map
        |> Map.mapValues (doublesInLine List.empty)
        |> Map.toList
        |> List.collect (fun (key, values) -> values |> List.map (toTuple key))

    let rec pointIsInAnyOfSortedLines v (lines: (int*int) list) =
        lines
        |> List.tryFind (fun (start, finish) -> start <= v && finish >= v)
        |> Option.isSome
        

    let doublesVerticals = doublesInLines mappedVerticals (fun key value -> (key, value))
    let doublesHorizontals = doublesInLines mappedHorizontals (fun key value -> (value, key))
    let intersections =
        mappedHorizontals
        |> Map.toList
        |> List.collect (fun (y, lines) ->
            lines
            |> List.collect (fun (xStart, xFinish) -> [xStart .. xFinish])
            |> List.distinct
            |> List.filter(fun x ->
                        Map.tryFind x mappedVerticals
                        |> Option.map (pointIsInAnyOfSortedLines y)
                        |> Option.defaultValue false
                    )
            |> List.map (fun x -> (x, y))
        )

    let dangerousPoints = 
        [doublesVerticals; doublesHorizontals; intersections]
        |> List.collect id
        |> List.distinct

    dangerousPoints
    |> List.length


// this time not es efficient as the part 1, but way shorter ^^
let part2 (lines: LineDefinition list) =
    lines
    |> List.collect LineDefinition.expand
    |> List.countBy id
    |> List.filter (snd >> (<=)2)
    |> List.map fst
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

    part2 input
    |> printfn "%d"

    0

