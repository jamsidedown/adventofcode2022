open System
open System.IO

type Instr =
    | Noop
    | Add of int

let parseLine (line:string) =
    match line.Split ' ' with
    | [| "noop" |] -> Some Noop
    | [| "addx"; value |] -> int value |> Add |> Some
    | _ ->
        printfn "error %s" line
        None

let parse (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map parseLine
    |> Array.choose id

let run (instructions:array<Instr>) =
    let rec recurse (x:int) (instrs:list<Instr>) =
        match instrs with
        | Noop :: tail -> x :: recurse x tail
        | Add v :: tail -> x :: x + v :: recurse (x + v) tail
        | [] -> []

    let register =
        instructions
        |> Array.toList
        |> recurse 1

    1 :: register

let partOne (instructions:array<Instr>) =
    let register =
        instructions
        |> run
        |> List.toArray

    [| 20; 60; 100; 140; 180; 220 |]
    |> Array.sumBy (fun addr -> register[addr-1] * addr)

let pixel (index:int) (value:int) =
    match index - value with
    | -1 | 0 | 1 -> "#"
    | _ -> "."

let pprint (memory:array<int>) =
    memory
    |> Array.chunkBySize 40
    |> Array.map (Array.mapi pixel >> String.Concat >> printfn "%s")

let partTwo (instructions:array<Instr>) =
    instructions
    |> run
    |> List.toArray
    |> pprint

let testInputOne = parse "day10/test_input_1.txt"
assert (run testInputOne = [ 1; 1; 1; 4; 4; -1 ])

let testInputTwo = parse "day10/test_input_2.txt"
assert (partOne testInputTwo = 13140)

let input = parse "day10/input.txt"
partOne input |> printfn "%A"

// partTwo testInputTwo
partTwo input
