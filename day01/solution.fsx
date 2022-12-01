open System.IO

let parse (filepath:string) =
    File.ReadAllText filepath
    |> (fun s -> s.Split "\n\n")
    |> Array.map (fun s -> s.TrimEnd())
    |> Array.map (fun s -> (s.Split "\n") |> Array.map int)
    |> Array.map Array.sum

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
