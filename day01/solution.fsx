open System
open System.IO

let newline = Environment.NewLine
let twoLines = $"{newline}{newline}"

let parseInts (input:string) =
    let trimmed = input.TrimEnd()
    trimmed.Split newline
    |> Array.map int

let parse (filepath:string) =
    File.ReadAllText filepath
    |> (fun s -> s.Split twoLines)
    |> Array.map (parseInts >> Array.sum)

let partOne (calories:array<int>) =
    calories
    |> Array.max

let partTwo (calories:array<int>) =
    calories
    |> Array.sortDescending
    |> Array.take 3
    |> Array.sum

let testInput = parse "day01/test_input.txt"
let input = parse "day01/input.txt"

assert (partOne testInput = 24000)
printfn $"{partOne input}"

assert (partTwo testInput = 45000)
printfn $"{partTwo input}"
