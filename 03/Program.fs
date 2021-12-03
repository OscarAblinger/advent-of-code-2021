open InputUtils

let binaryToInt str = System.Convert.ToInt64 (str, 2)

let part1 (input: int list list) =
    let halfOfInputs = input.Length / 2

    let countedBits =
        input
        |> List.transpose
        |> List.map List.sum

    let readRate vIf0 vIf1 values =
        values
        |> List.map (fun sum -> if sum > halfOfInputs then vIf1 else vIf0)
        |> System.String.Concat
        |> binaryToInt

    let γ = readRate "0" "1" countedBits
    let ε = readRate "1" "0" countedBits

    γ * ε

let parseBits (line: char seq) = [
    for ch in line do
        match ch with
        | '0' -> yield 0
        | '1' -> yield 1
        | ch -> failwithf "'%c' is not a recognised character" ch
]

let part2 (input: int list list) =
    let rec findIdealNumber preferredNumber currBit (numbers: int list list) =

        match numbers with
        | [] -> failwith "All numbers were eliminated :("
        | [ idealNumber ] -> idealNumber
        | _ ->
            let doubleNumberOf1s =
                numbers
                |> List.transpose
                |> List.item currBit
                |> List.filter ((=) 1)
                |> List.length
                |> (*) 2

            let acceptedBitValue =
                if doubleNumberOf1s < numbers.Length then
                    preferredNumber
                else
                    (preferredNumber + 1) % 2
            
            let newNumbers =
                numbers
                |> List.filter (fun num -> List.item currBit num = acceptedBitValue)

            findIdealNumber preferredNumber (currBit+1) newNumbers

    let findRating preferredNumber =
        findIdealNumber preferredNumber 0 input
        |> List.map string
        |> System.String.Concat
        |> binaryToInt

    let oxygenGeneratorRating = findRating 1
    let co2ScrubberRating = findRating 0

    oxygenGeneratorRating * co2ScrubberRating

[<EntryPoint>]
let main argv =
    let input =
        System.IO.File.ReadLines (getFileName argv)
        |> Seq.map parseBits
        |> List.ofSeq

    part1 input
    |> printfn "%d"

    part2 input
    |> printfn "%d"

    0

