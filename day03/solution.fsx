open System.IO

let priorities (c:char) =
    match c with
    | c when c >= 'a' && c <= 'z' -> (int c) - (int 'a') + 1
    | c when c >= 'A' && c <= 'Z' -> (int c) - (int 'A') + 27
    | _ -> 0

let parse (filepath:string) =
    File.ReadAllLines(filepath)

let partOne (rucksacks:array<string>) =
    rucksacks
    |> Array.sumBy (
        Seq.toArray
        >> Array.splitInto 2
        >> Array.map Set.ofSeq
        >> Set.intersectMany
        >> Set.toArray
        >> Array.head
        >> priorities)

let partTwo (rucksacks:array<string>) =
    rucksacks
    |> Array.map Set.ofSeq
    |> Array.chunkBySize 3
    |> Array.sumBy (Set.intersectMany >> Set.toArray >> Array.head >> priorities)

let testInput = parse "./day03/test_input.txt"
let input = parse "./day03/input.txt"

assert (partOne testInput = 157)
printfn $"{partOne input}"

assert (partTwo testInput = 70)
printfn $"{partTwo input}"
