open Extensions
open InputUtils
open Matrix
open System.Collections.Generic

// Original solution was modeled after Euler's 81st problem, but as it turns out, the big difference is
// that in euler's version you can only move to the right and down, which is not the case for this problem.
// As such this solution only incidentally works for some inputs.
// The actual solution uses Dijkstra's algorithm (based on the F# implementation that can be found here:
// https://rosettacode.org/wiki/Dijkstra%27s_algorithm#F.23
// I decided to leave the old version in (with their functions prefixed with 'old') since they are nice.

let OFFSETS = [ (1,0); (0,1); (-1,0); (0,-1) ]

let neighbors (map: int Matrix) (x, y) =
    OFFSETS
    |> List.map (fun (dx, dy) -> dx+x, dy+y)
    |> List.zipWithSome (fun k -> Matrix.tryItem k map)

let part1Dijkstra (map: int Matrix) =
    let endPos = Matrix.lastIndex map
    let queue = PriorityQueue<int*int, int>()
    queue.Enqueue ((0,0), 0)
    let mutable costs: Map<int*int, int> = Map.empty.Add ((0,0), 0)

    while queue.Count <> 0 do
        let curr = queue.Dequeue ()

        if curr = endPos then
            ()
        else
            for (next, cost) in neighbors map curr do
                let accCost = costs.[curr] + cost
                if not <| Map.containsKey next costs || accCost < costs.[next] then
                    costs <- costs.Add (next, accCost)
                    queue.Enqueue(next, accCost)

    costs.[endPos]

let fiveTimesMatrix (matrix: int Matrix) = [
        for offsetX = 0 to 4 do
            for list in matrix do
                yield [
                    for offsetY = 0 to 4 do
                        for el in list do
                            yield ((el + offsetX + offsetY - 1) % 9) + 1
                ]
    ]

let oldSolution (input: int Matrix) =
    let matrix =
        Array2D.init input.Length input.Length (fun x y -> input.[y].[x])

    matrix.[0, 0] <- 0 // the starting position is never entered

    let l = matrix.GetLength 0
    for i = l - 2 downto 0 do
        matrix.[l-1, i] <- matrix.[l-1, i] + matrix.[l-1, i+1]
        matrix.[i, l-1] <- matrix.[i, l-1] + matrix.[i+1, l-1]

    for i = l - 2 downto 0 do
        for j = l - 2 downto 0 do
            matrix.[i, j] <- matrix.[i, j] + (min matrix[i+1, j] matrix[i, j+1])

    matrix.[0, 0]

let oldFiveTimesMatrix (input: int list list) =
    let overflowAt9 i = ((i - 1) % 9) + 1
    let length = input.Length
    let matrix5Times =
        Array2D.init (length*5) (length*5) (fun x y -> overflowAt9 (input.[y % length].[x % length] + (x/length) + (y/length)))

    matrix5Times.[0, 0] <- 0 // the starting position is never entered
    matrix5Times

[<EntryPoint>]
let main argv =
    let input =
        System.IO.File.ReadLines (getFileName argv)
        |> Seq.map (Seq.map (fun ch -> System.Int32.Parse (string ch)))
        |> Seq.map Seq.toList
        |> Seq.toList


    part1Dijkstra input
    |> printfn "%d"

    fiveTimesMatrix input
    |> part1Dijkstra
    |> printfn "%d"

    0

