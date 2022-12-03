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
    |> Array.map (fun line -> line |> Seq.toArray |> Array.splitAt (line.Length/2))
    |> Array.map (fun (first, second) ->
        Set.intersect (Set.ofSeq first) (Set.ofSeq second))
    |> Array.map (Set.toArray >> Array.head >> priorities)
    |> Array.sum

let partTwo (rucksacks:array<string>) =
    rucksacks
    |> Array.map Set.ofSeq
    |> Array.chunkBySize 3
    |> Array.map (Set.intersectMany >> Set.toArray >> Array.head >> priorities)
    |> Array.sum

let test_input = parse "./day03/test_input.txt"
let input = parse "./day03/input.txt"

assert (partOne test_input = 157)
printfn $"{partOne input}"

assert (partTwo test_input = 70)
printfn $"{partTwo input}"
