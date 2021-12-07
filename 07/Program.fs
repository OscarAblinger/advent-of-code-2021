open Extensions
open InputUtils

let part1 (positions: int list) =
    let sorted =
        positions
        |> List.sort

    // not 100% technically the median since for an even amount of inputs, you
    // just select one of the middles instead of the arithmetic middle of those two,
    // but the distance to any point in between the two middles will always be the same.
    let median =
        sorted
        |> List.item ((positions.Length / 2)-1)

    sorted
    |> List.fold (fun acc pos -> acc + abs(pos-median)) 0

let part2 (positions: int list) =
    let calcFuelForDistance distance = (distance * distance + distance) / 2
        
    let calcFuelCostToPos (pos: int) (list: int list) =
        list
        |> List.map ((-)pos)
        |> List.map abs
        |> List.map calcFuelForDistance
        |> List.sum

    let rec findBestPosition lastPos lastCost moveFunc sortedPositions =
        let newPos = moveFunc lastPos
        let newCost = calcFuelCostToPos newPos sortedPositions
        
        if newCost > lastCost then
            lastCost
        else
            findBestPosition newPos newCost moveFunc sortedPositions

    // first get the median
    let sorted =
        positions
        |> List.sort

    // again not always the exact median, but we're just using it as starting
    // point for some local optimization so it doesn't matter that much
    let median =
        sorted
        |> List.item ((positions.Length / 2)-1)
    
    // then see which side is more expensive
    let leftCost =
        sorted
        |> List.take (positions.Length / 2)
        |> calcFuelCostToPos median
    let rightCost =
        sorted
        |> List.skip (positions.Length / 2)
        |> calcFuelCostToPos median

    // and move in increments of 1 to the more expensive one
    let moveFunc =
        match leftCost, rightCost with
        | LargerThan -> Some (fun x -> x-1)
        | LessThan -> Some ((+)1)
        | Equal -> None

    if Option.isNone moveFunc then
        median
    else
        findBestPosition median (leftCost+rightCost) (Option.get moveFunc) sorted
    

[<EntryPoint>]
let main argv =
    let input =
        System.IO.File.ReadLines (getFileName argv)
        |> Seq.exactlyOne
        |> (fun str -> str.Split(','))
        |> Array.toList
        |> List.map System.Int32.Parse

    part1 input
    |> printfn "%d"

    part2 input
    |> printfn "%d"

    0
