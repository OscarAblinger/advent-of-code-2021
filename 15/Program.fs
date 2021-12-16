open InputUtils

let part1 (matrix: int[,]) =
    let l = matrix.GetLength 0
    for i = l - 2 downto 0 do
        matrix.[l-1, i] <- matrix.[l-1, i] + matrix.[l-1, i+1]
        matrix.[i, l-1] <- matrix.[i, l-1] + matrix.[i+1, l-1]

    for i = l - 2 downto 0 do
        for j = l - 2 downto 0 do
            matrix.[i, j] <- matrix.[i, j] + (min matrix[i+1, j] matrix[i, j+1])

    matrix.[0, 0]

let fiveTimesMatrix (input: int list list) =
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

    let matrix =
        Array2D.init input.Length input.Length (fun x y -> input.[y].[x])

    matrix.[0, 0] <- 0 // the starting position is never entered

    part1 matrix
    |> printfn "%d"

    fiveTimesMatrix input
    |> part1
    |> printfn "%d"

    0

