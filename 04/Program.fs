open Extensions
open FSharp.Text.RegexExtensions
open FSharp.Text.RegexProvider
open InputUtils
open Matrix

type MutableBingoBoard(values: int Matrix) =
    let rowMatches = [| for _ in 1 .. values.Length -> 0 |]
    let colMatches = [| for _ in 1 .. values.[0].Length -> 0 |]
    let mutable marked = List.empty
    member _.Values = values

    member _.GetUnmarkedValues () = [
            for (x, row) in List.indexed values do
                for (y, el) in List.indexed row do
                    if not (List.contains (x, y) marked) then
                        yield el
        ]
                    

    /// Marks the given number on the board and returns true if a bingo happened or false otherwise.
    /// This operation mutates the instance of the bingo board
    member _.MarkNumberAndCheckBingo nr =
        let idxFound =
            values
            |> Matrix.tryFindIndex ((=) nr)

        match idxFound with
        | Some (row, col) ->
            Array.set rowMatches row (rowMatches.[row] + 1)
            Array.set colMatches col (colMatches.[col] + 1)
            marked <- (row, col)::marked
            rowMatches.[row] >= colMatches.Length || colMatches.[col] >= rowMatches.Length
        | None -> false

    override _.ToString () =
        seq {
            for (x, row) in List.indexed values do
                for (y, el) in List.indexed row do
                    if List.contains (x, y) marked then
                        yield sprintf "(%3d) " el
                    else
                        yield sprintf "%5d " el
                yield "\n"
        }
        |> Seq.fold (+) ""

type BoardLineRegex = Regex< @"(?<number>\d+)( +|$)">

let parseBoard (boardStr: string seq): MutableBingoBoard =
    let boardLineRegex = BoardLineRegex()
    boardStr
    |> Seq.map boardLineRegex.TypedMatches
(*    |> Seq.map (fun x -> 
                    printfn "%A" x
                    x) *)
    |> Seq.map (Seq.map (fun x -> x.number.AsInt))
    |> Seq.map Seq.toList
    |> Seq.toList
    |> MutableBingoBoard

let rec getWinningBoardAndNumber (numbers: int list) (boards: MutableBingoBoard list) =
    match numbers with
    | [] -> failwithf "No winning board found. Boards left are: %A" boards
    | nextNr::futureNrs ->
        match List.tryFind (fun (b: MutableBingoBoard) -> b.MarkNumberAndCheckBingo nextNr) boards with
        | Some board -> (board, nextNr)
        | None -> getWinningBoardAndNumber futureNrs boards

let part1 (numbers: int list) (boards: MutableBingoBoard list) =
    let (winningBoard, winningNumber) = getWinningBoardAndNumber numbers boards
    winningBoard.GetUnmarkedValues ()
    |> List.sum
    |> (*) winningNumber

let rec getLastWinningBoardAndNumber (numbers: int list) (boards: MutableBingoBoard list) =
    match numbers with
    | [] -> failwithf "Multiple winning board found. Boards left are: %A" boards
    | nextNr::futureNrs ->
        let markAndCheckBingo (b: MutableBingoBoard) = b.MarkNumberAndCheckBingo nextNr
        let incompleteBoards =
            List.filter (markAndCheckBingo >> not) boards

        match incompleteBoards with
        | [] ->
            match List.tryExactlyOne boards with
            | Some board -> (board, nextNr)
            | None ->
                failwithf "Seems like all leftover boards were removed at once with the number %d Boards were: %A" nextNr boards
        | _ -> getLastWinningBoardAndNumber futureNrs incompleteBoards

let part2 (numbers: int list) (boards: MutableBingoBoard list) =
    let (winningBoard, winningNumber) = getLastWinningBoardAndNumber numbers boards
    winningBoard.GetUnmarkedValues ()
    |> List.sum
    |> (*) winningNumber

[<EntryPoint>]
let main argv =
    let input =
        System.IO.File.ReadLines (getFileName argv)

    let numbers =
        input
        |> Seq.head
        |> fun s -> s.Split ','
        |> Array.map System.Int32.Parse
        |> Array.toList

    let splitInput =
        input
        |> Seq.tail
        |> Seq.splitWhen ((=) "")

    // These steps have to be re-done since MutableBingoBoard is mutable
    splitInput
    |> Seq.map parseBoard
    |> Seq.toList
    |> part1 numbers
    |> printfn "%d"

    splitInput
    |> Seq.map parseBoard
    |> Seq.toList
    |> part2 numbers
    |> printfn "%d"

    0

