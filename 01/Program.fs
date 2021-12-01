open InputUtils
open Extensions

let part1 (numbers: int seq) =
    numbers
    |> Seq.pairwise
    |> Seq.filter (fun (v1, v2) -> v1 < v2)
    |> Seq.length
    |> printfn "%i"

let part2 (numbers: int seq) =
    numbers
    |> Seq.triplemap (fun v1 v2 v3 -> v1 + v2 + v3)
    |> part1

[<EntryPoint>]
let main argv =
    let numbers =
        System.IO.File.ReadLines (getFileName argv)
        |> Seq.map int
        |> List.ofSeq

    part1 numbers
    part2 numbers
    
    waitForUser ()
    0 // return an integer exit code
