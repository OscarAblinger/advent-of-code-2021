open InputUtils

let OFFSETS = [ -1,0; 1,0; 0,-1; 0,1 ]

let tryItem (matrix: 'a list list) (x: int, y: int) =
    List.tryItem x matrix
    |> Option.bind (List.tryItem y)

let getLowPoints (matrix: int list list) =
    let isLowestAtPoint (x, y) =
        let value = matrix.[x].[y]
        OFFSETS
        |> List.map (fun (dx, dy) -> dx + x, dy + y)
        |> List.choose (tryItem matrix)
        |> List.forall ((<) value)

    seq {
        for x in [0..matrix.Length-1] do
            for y in [0..matrix.[0].Length-1] do
                if isLowestAtPoint (x,y) then
                    yield (x,y)
    }

let part1 (input: int list list) =
    let lowpoints = getLowPoints input |> List.ofSeq
    lowpoints
    |> Seq.sumBy (fun (x, y) -> 1 + input.[x].[y])

let growPoint (input: int list list) startPos =
    let isNotAWall ((x, y) as pos) =
        Matrix.hasPosition pos input && input.[x].[y] <> 9

    let rec growPoints (checkedPoints: Set<(int*int)>) (uncheckedPoints: (int*int) list) =
        match uncheckedPoints with
        | [] -> checkedPoints
        | ((x,y) as pos)::nextUnchecked ->
            if isNotAWall pos then
                let pointsAroundPos =
                    OFFSETS
                    |> List.map (fun (dx, dy) -> dx + x, dy + y)
                    |> List.filter (fun pos -> Matrix.hasPosition pos input)
                    |> List.filter (checkedPoints.Contains >> not)

                growPoints (checkedPoints.Add(pos)) (nextUnchecked @ pointsAroundPos)
            else
                growPoints checkedPoints nextUnchecked

    growPoints Set.empty (List.singleton startPos)

let part2 (input: int list list) =
    getLowPoints input
    |> Seq.map (growPoint input)
    |> List.ofSeq
    |> Seq.map Set.count
    |> Seq.sortDescending
    |> Seq.take 3
    |> List.ofSeq
    |> Seq.reduce (*)

[<EntryPoint>]
let main argv =
    let input =
        System.IO.File.ReadLines (getFileName argv)
        |> Seq.map (Seq.map (fun ch -> System.Int32.Parse(string ch)))
        |> Seq.map List.ofSeq
        |> List.ofSeq
        |> List.transpose // not necessary, but it allows for .[x].[y] instead of .[y].[x]

    part1 input
    |> printfn "%d"

    part2 input
    |> printfn "%d"

    0

