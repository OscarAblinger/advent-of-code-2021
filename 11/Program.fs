open InputUtils
open Matrix

type Board = uint16 Matrix

let SURROUNDING_OFFSET =
    List.allPairs [-1..1] [-1..1]
    |> List.filter ((<>)(0, 0))

let surroundingIndexes (board: Board) (x, y) =
    SURROUNDING_OFFSET
    |> List.map (fun (dx, dy) -> (dx+x, dy+y))
    |> List.filter (fun (nx, ny) -> Matrix.hasPosition (nx, ny) board)


let rec recSimulateFlashes (previouslyFlashedIdx: Set<int*int>) (nowFlashingIdx: Set<int*int>) (board: Board) =
    if nowFlashingIdx.Count = 0 then
        (previouslyFlashedIdx, board)
    else
        let allFlashedIdx = Set.union previouslyFlashedIdx nowFlashingIdx

        let idxOfSurroundingEl =
            nowFlashingIdx
            |> Seq.collect (surroundingIndexes board)

        let boardWithIncrElements =
            idxOfSurroundingEl
            |> Seq.fold (fun b (x, y) -> List.updateAt x (List.updateAt y (b.[x].[y] + 1us) b.[x]) b) board

        let newFlashingIdx =
            idxOfSurroundingEl
            |> Set.ofSeq
            |> Set.filter (fun (x, y) -> boardWithIncrElements.[x].[y] > 9us)
            |> fun nfi -> Set.difference nfi allFlashedIdx

        recSimulateFlashes allFlashedIdx newFlashingIdx boardWithIncrElements

let simulateFlashes (board: Board) =
    let flashingElements =
        board
        |> Matrix.indexed
        |> Matrix.fold (fun acc (x, y, v) -> if v > 9us then (x,y)::acc else acc) List.empty
        |> Set.ofList

    recSimulateFlashes Set.empty flashingElements board

let rec recSimulate totalFlashes n board =
    if n <= 0 then
        totalFlashes, board
    else
        let (flashedOctopuses, boardAfterSim) =
            board
            |> Matrix.map ((+)1us)
            |> simulateFlashes

        let newBoard =
            flashedOctopuses
            |> Set.fold (fun b (x, y) -> List.updateAt x (List.updateAt y 0us b.[x]) b) boardAfterSim

        recSimulate (totalFlashes + flashedOctopuses.Count) (n-1) newBoard

let simulate = recSimulate 0

let part1 input =
    simulate 100 input
    |> fst

let rec simulateUntilSync (step: int) (board: Board) =
    if List.forall (List.forall ((=) 0us)) board then
        step
    else
        let (flashedOctopuses, boardAfterSim) =
            board
            |> Matrix.map ((+)1us)
            |> simulateFlashes

        let newBoard =
            flashedOctopuses
            |> Set.fold (fun b (x, y) -> List.updateAt x (List.updateAt y 0us b.[x]) b) boardAfterSim

        simulateUntilSync (step+1) newBoard

let part2 = simulateUntilSync 0

[<EntryPoint>]
let main argv =
    let input =
        System.IO.File.ReadLines (getFileName argv)
        |> Seq.map (Seq.map (string >> System.UInt16.Parse))
        |> Seq.map List.ofSeq
        |> List.ofSeq
        |> List.transpose // not necessary, but allows for nice x,y

    part1 input
    |> printfn "%d"

    part2 input
    |> printfn "%d"

    0

