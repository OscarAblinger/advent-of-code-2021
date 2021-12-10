open InputUtils

type Bracket =
    | Round
    | Square
    | Curly
    | Angle
    with
        static member ofChar (ch: char): Bracket =
            match ch with
            | '(' | ')' -> Round
            | '[' | ']' -> Square
            | '{' | '}' -> Curly
            | '<' | '>' -> Angle
            | _ -> failwithf "Bracket %c not found" ch

type UnexpectedClosingBracketData = {
    expected: Bracket option
    actual: Bracket
    index: int
}
type SyntaxError =
    | UnexpectedClosingBracket of UnexpectedClosingBracketData
    | UnfinishedLine of Bracket list


let analyze (input: string seq) =
    let rec analyzeLine (stack: Bracket list) (index: int) (line: string): SyntaxError =
        if index >= line.Length then
            if stack.Length > 0 then
                UnfinishedLine stack
            else
                failwithf "Valid line found: %s" line
        else
            match stack, line.[index] with
            | _, (('('|'['|'{'|'<') as ch) -> analyzeLine ((Bracket.ofChar ch)::stack) (index+1) line
            | expected::restStack, ch when expected = Bracket.ofChar ch -> analyzeLine restStack (index+1) line
            | expected::_, ch ->
                UnexpectedClosingBracket ({ expected = Some expected; actual = Bracket.ofChar ch; index = index })
            | [], ch ->
                UnexpectedClosingBracket ({ expected = None; actual = Bracket.ofChar ch; index = index })

    input
    |> Seq.map (analyzeLine List.empty 0)

let part1 (input: string seq) =
    let calcUnexpectedClosingBracketScore (unexpectedClosingBracket: UnexpectedClosingBracketData): int64 =
        match unexpectedClosingBracket.actual with
        | Round -> 3L
        | Square -> 57L
        | Curly -> 1197L
        | Angle -> 25137L

    analyze input
    |> Seq.choose (fun error -> match error with | UnexpectedClosingBracket data -> Some data | _ -> None)
    |> Seq.map calcUnexpectedClosingBracketScore
    |> Seq.sum

let part2 (input: string seq) =
    let calcUnfinishedLineScore =
        let rec recCalcUnfinishedScore (score: int64) (stack: Bracket list) =
            match stack with
            | [] -> score
            | Round::rest -> recCalcUnfinishedScore (score*5L + 1L) rest
            | Square::rest -> recCalcUnfinishedScore (score*5L + 2L) rest
            | Curly::rest -> recCalcUnfinishedScore (score*5L + 3L) rest
            | Angle::rest -> recCalcUnfinishedScore (score*5L + 4L) rest

        recCalcUnfinishedScore 0

    let scores =
        analyze input
        |> Seq.choose (fun error -> match error with | UnfinishedLine stack -> Some stack | _ -> None)
        |> Seq.map calcUnfinishedLineScore
        |> Seq.sort
        |> List.ofSeq
    
    scores.[scores.Length / 2]

[<EntryPoint>]
let main argv =
    let input =
        System.IO.File.ReadLines (getFileName argv)

    part1 input
    |> printfn "%d"

    part2 input
    |> printfn "%d"

    0

