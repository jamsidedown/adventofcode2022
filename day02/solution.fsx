open System.IO

// A: Rock, B: Paper, C: Scissors

// X: Rock, Y: Paper, Z: Scissors
// X: Lose, Y: Draw, Z: Win

type Choice =
    | Rock
    | Paper
    | Scissors

type Result =
    | Win
    | Draw
    | Lose

let choicePoints = Map [ (Rock, 1); (Paper, 2); (Scissors, 3) ]
let resultPoints = Map [ (Win, 6); (Draw, 3); (Lose, 0) ]

let getResult (opponent:Choice) (self:Choice) =
    match (opponent, self) with
    | o, s when o = s -> Draw
    | Rock, Paper | Paper, Scissors | Scissors, Rock -> Win
    | _ -> Lose

let getChoice (opponent:Choice) (result:Result) =
    match (opponent, result) with
    | _, Draw -> opponent
    | Rock, Win | Scissors, Lose -> Paper
    | Paper, Win | Rock, Lose -> Scissors
    | Scissors, Win | Paper, Lose -> Rock

let getScore (choice:Choice) (result:Result) =
    choicePoints[choice] + resultPoints[result]

let parseChoice (choice:char) =
    match choice with
    | 'A' | 'X' -> Some(Rock)
    | 'B' | 'Y' -> Some(Paper)
    | 'C' | 'Z' -> Some(Scissors)
    | _ -> None

let parseOutcome (outcome:char) =
    match outcome with
    | 'X' -> Some(Lose)
    | 'Y' -> Some(Draw)
    | 'Z' -> Some(Win)
    | _ -> None

let parsePartOne (row:string) =
    let theirs = row |> Seq.head |> parseChoice
    let ours = row |> Seq.last |> parseChoice
    match (theirs, ours) with
    | Some(t), Some(o) -> Some(o, getResult t o)
    | _ -> None

let parsePartTwo (row:string) =
    let theirs = row |> Seq.head |> parseChoice
    let outcome = row |> Seq.last |> parseOutcome
    match (theirs, outcome) with
    | Some(t), Some(o) -> Some(getChoice t o, o)
    | _ -> None

let partOne (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map parsePartOne
    |> Array.choose id
    |> Array.sumBy (fun (choice, result) -> getScore choice result)

let partTwo (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map parsePartTwo
    |> Array.choose id
    |> Array.sumBy (fun (choice, result) -> getScore choice result)

let testInputPath = "day02/test_input.txt"
let inputPath = "day02/input.txt"

assert (partOne testInputPath = 15)
printfn $"{partOne inputPath}"

assert (partTwo testInputPath = 12)
printfn $"{partTwo inputPath}"
