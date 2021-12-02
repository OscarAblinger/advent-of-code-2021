open FSharp.Text.RegexExtensions
open FSharp.Text.RegexProvider
open InputUtils

type MovementRegex = Regex< @"(?<Direction>up|down|forward) (?<Amount>\d+)">

let part1 (input: (string * int) seq) =
    let move (length, height) movement =
        match movement with
        | ("up", x) -> (length, height - x)
        | ("down", x) -> (length, height + x)
        | ("forward", x) -> (length + x, height)
        | _ -> failwithf "unexpected input: %A" movement

    input
    |> Seq.fold move (0, 0)
    ||> (*)

let part2 (input: (string * int) seq) =
    let move (length, height, aim) movement =
        match movement with
        | ("up", x) -> (length, height, aim - x)
        | ("down", x) -> (length, height, aim + x)
        | ("forward", x) -> (length + x, height + aim * x, aim)
        | _ -> failwithf "unexpected input: %A" movement

    input
    |> Seq.fold move (0, 0, 0)
    |> (fun (l, h, _) -> l * h)

[<EntryPoint>]
let main argv =
    let movementRegex = MovementRegex()
    let input =
        System.IO.File.ReadLines (getFileName argv)
        |> Seq.map movementRegex.TypedMatch
        |> Seq.map (fun x -> (x.Direction.Value, x.Amount.AsInt))

    part1 input
    |> printfn "%d"

    part2 input
    |> printfn "%d"

    0
