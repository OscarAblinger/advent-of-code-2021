open Extensions
open InputUtils

type DigitLight = char list
[<Struct>]
type NumberDescription = {
    Signal: DigitLight list
    Output: DigitLight list
}

let parseInputLine (line: string) =
    let splitDigitLightList (dll: string): DigitLight list =
        dll.Split(' ')
        |> Array.toList
        |> List.map Seq.toList
        |> List.map List.sort

    match line.Split(" | ") with
    | [| signal; output |] ->
        {
            Signal = splitDigitLightList signal
            Output = splitDigitLightList output
        }
    | _ -> failwithf "Line could not be split as expected: %s" line

let part1 (input: NumberDescription seq) =
    input
    |> Seq.map (fun nd ->
        nd.Output
        |> List.filter (fun str -> str.Length = 2 || str.Length = 4 || str.Length = 3 || str.Length = 7)
        |> List.length)
    |> Seq.sum

let COUNTS_OF_ACTUAL_SECTIONS =
    Map.empty
        .Add(4, Set.singleton 'e')
        .Add(6, Set.singleton 'b')
        .Add(7, Set.empty.Add('d').Add('g')) // differenciate using digit 4
        .Add(8, Set.empty.Add('a').Add('c')) // differenciate using digit 4
        .Add(9, Set.singleton 'f')

let LIGHTS_TO_NUMBER =
    Map.empty
        .Add("abcefg", 0)
        .Add("cf", 1)
        .Add("acdeg", 2)
        .Add("acdfg", 3)
        .Add("bcdf", 4)
        .Add("abdfg", 5)
        .Add("abdefg", 6)
        .Add("acf", 7)
        .Add("abcdefg", 8)
        .Add("abcdfg", 9)

let getCharMap (digitLights: DigitLight list) =
    let getCorrectCharFor (digit4: DigitLight) (ch: char, cnt: int) =
        let possibleChars = COUNTS_OF_ACTUAL_SECTIONS.[cnt]
        if possibleChars.Count = 1 then
            ch, Seq.exactlyOne possibleChars
        else
            match cnt, List.contains ch digit4 with
            | 7, true -> ch, 'd'
            | 7, false -> ch, 'g'
            | 8, true -> ch, 'c'
            | 8, false -> ch, 'a'

    let digit4 =
        digitLights
        |> List.find (fun dg -> dg.Length = 4)
    digitLights
    |> List.concat
    |> List.countBy id
    |> List.map (getCorrectCharFor digit4)
    |> Map.ofList

let part2 (input: NumberDescription seq) =
    let solveLine (nd: NumberDescription) =
        let chMap = getCharMap nd.Signal
        nd.Output
        |> List.map (List.map (fun ch -> chMap.[ch]))
        |> List.map List.sort
        |> List.map (fun chList -> System.String.Join("", chList))
        |> List.map (fun light -> LIGHTS_TO_NUMBER.[light])
        |> List.reduce (fun acc n -> acc*10 + n)

    input
    |> Seq.map solveLine
    |> Seq.sum

[<EntryPoint>]
let main argv =
    let input =
        System.IO.File.ReadLines (getFileName argv)
        |> Seq.map parseInputLine
        |> Seq.cache

    part1 input
    |> printfn "%d"

    analyse input

    part2 input
    |> printfn "%d"

    0

