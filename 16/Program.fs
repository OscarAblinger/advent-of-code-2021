open BITSGrammar
open FParsec.CharParsers
open InputUtils

let hex2binary (str: string) =
    str
    |> String.collect (fun ch ->
        match ch with
        | '0' -> "0000"
        | '1' -> "0001"
        | '2' -> "0010"
        | '3' -> "0011"
        | '4' -> "0100"
        | '5' -> "0101"
        | '6' -> "0110"
        | '7' -> "0111"
        | '8' -> "1000"
        | '9' -> "1001"
        | 'A' -> "1010"
        | 'B' -> "1011"
        | 'C' -> "1100"
        | 'D' -> "1101"
        | 'E' -> "1110"
        | 'F' -> "1111"
        | _ -> failwithf "Char '%c' is not a valid hex character" ch)

let countVersions (syntaxTree: Packet) =
    let rec recCountVersions total packages =
        match packages with
        | [] -> total
        | (LiteralPacket (version, _))::rest -> recCountVersions (total + version) rest
        | (OperatorPacket (version, children))::rest ->
            recCountVersions (total + version) (children @ rest)

    recCountVersions 0 [syntaxTree]

let rec evaluateTree (syntaxTree: Packet) =
    match syntaxTree with
    | LiteralPacket (_, value) -> value
    | SumPacket (_, children) ->
        children
        |> List.map evaluateTree
        |> List.sum
    | ProductPacket (_, children) -> 
        children
        |> List.map evaluateTree
        |> List.reduce (*)
    | MinimumPacket (_, children) -> 
        children
        |> List.map evaluateTree
        |> List.min
    | MaximumPacket (_, children) -> 
        children
        |> List.map evaluateTree
        |> List.max
    | GreaterThanPacket (_, children) -> combineTwoChildrenWith (>) children
    | LessThanPacket (_, children) -> combineTwoChildrenWith (<) children
    | EqualToPacket (_, children) -> combineTwoChildrenWith (=) children
and combineTwoChildrenWith f (children: Packet list) =
        match children with
        | first::second::[] ->
            if f (evaluateTree first) (evaluateTree second) then
                1
            else
                0
        | _ -> failwithf "Expected only two children, but found: %O" children

let part1 (str: string) =
    match parse str with
    | Success (syntaxTree, _, _) ->
        countVersions syntaxTree
    | Failure (errorMsg, errors, pos) ->
        failwith errorMsg

let part2 (str: string) =
    match parse str with
    | Success (syntaxTree, _, _) ->
        evaluateTree syntaxTree
    | Failure (errorMsg, errors, pos) ->
        failwith errorMsg

let foo hex =
    printfn "%s" hex
    hex2binary hex
    |> part1
    |> printfn "%d"

[<EntryPoint>]
let main argv =
    let input =
        System.IO.File.ReadLines (getFileName argv)
        |> Seq.head

    hex2binary input
    |> part1
    |> printfn "%d"

    hex2binary input
    |> part2
    |> printfn "%d"

    0

