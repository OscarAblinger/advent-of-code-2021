open InputUtils
open Extensions

let part1 (fish: int list) =
    let rec iteration (n: int) (fishCnts: (int*int) list) =
        if n = 0 then
            fishCnts
        else
            fishCnts
            |> List.collect (fun (age, cnt) ->
                match age, cnt with
                | 0, _ -> [(6, cnt); (8, cnt)]
                | _ -> [(age-1, cnt)]
            )
            |> iteration (n-1)

    fish
    |> List.countBy id
    |> iteration 80
    |> List.sumBy snd

let part2 (fish: int list) =
    let rec iteration (n: int) (fishCnts: (int*int64) list) =
        if n = 0 then
            fishCnts
        else
            fishCnts
            |> List.collect (fun (age, cnt) ->
                match age, cnt with
                | 0, _ -> [(6, cnt); (8, cnt)]
                | _ -> [(age-1, cnt)]
            )
            |> List.groupBy fst
            |> List.map (fun (age, ageCntList) -> (age, List.sumBy snd ageCntList))
            |> iteration (n-1)

    fish
    |> List.countBy id
    |> List.map (fun (age, cnt) -> (age, int64 cnt))
    |> iteration 256
    |> List.sumBy snd

[<EntryPoint>]
let main argv =
    let input =
        System.IO.File.ReadLines (getFileName argv)
        |> Seq.exactlyOne
        |> (fun str -> str.Split(','))
        |> Array.toList
        |> List.map System.Int32.Parse

    part1 input
    |> printfn "%d"

    part2 input
    |> printfn "%d"

    0

