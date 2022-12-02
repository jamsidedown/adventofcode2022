open System.IO

// input
// A: Rock, B: Paper, C: Scissors

// output
// X: Rock, Y: Paper, Z: Scissors

type Choice =
    | Rock
    | Paper
    | Scissors

let beatenBy = Map [ (Rock, Paper); (Paper, Scissors); (Scissors, Rock) ]
let points = Map [ (Rock, 1); (Paper, 2); (Scissors, 3) ]
let win = 6
let draw = 3

let parseChoice (choice:char) =
    match choice with
    | 'A' | 'X' -> Some(Rock)
    | 'B' | 'Y' -> Some(Paper)
    | 'C' | 'Z' -> Some(Scissors)
    | _ -> None

let parseRow (row:string) =
    let theirs = row |> Seq.head |> parseChoice
    let ours = row |> Seq.last |> parseChoice
    match (theirs, ours) with
    | Some(t), Some(o) when beatenBy[t] = o -> points[o] + win
    | Some(t), Some(o) when t = o -> points[o] + draw
    | Some(_), Some(o) -> points[o]
    | _ -> 0

let parse (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map parseRow

let partOne (scores:array<int>) =
    scores |> Array.sum

let testInput = parse "day02/test_input.txt"
let input = parse "day02/input.txt"

assert (partOne testInput = 15)
printfn $"{partOne input}"
