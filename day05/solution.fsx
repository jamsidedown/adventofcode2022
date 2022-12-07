open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let newline = Environment.NewLine
let instructionPattern = Regex @"^move (\d+) from (\d) to (\d)$"

type Instruction = { Count:int; From:int; To:int }

let getCrate (index:int) (line:string) =
    match (line.Length > index) with
    | true ->
        match line[index] with
        | ' ' -> None
        | c -> Some c
    | false -> None

let parseStacks (lines:array<string>) =
    let reversed = lines |> Array.rev
    let (indexLine, crateLines) = (Array.head reversed, Array.tail reversed)

    let indexes =
        indexLine.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int

    let stacks = 
        indexes
        |> Array.map (fun i -> indexLine.IndexOf (string i))
        |> Array.map (fun i ->
            crateLines
            |> Array.map (getCrate i)
            |> Array.choose id)

    let lookup = Dictionary<int,Stack<char>>()

    for (index, stack) in (Array.zip indexes stacks) do
        lookup.Add(index, Stack<char>(stack))

    lookup

let parseInstruction (line:string) =
    instructionPattern.Match line
    |> (fun m ->
        m.Groups
        |> Seq.toArray
        |> Array.map (fun group -> group.Value)
        |> Array.tail
        |> Array.map int)
    |> function
        | [| count; from; until |] -> Some {Count=count; From=from; To=until}
        | _ -> None

let parse (filepath:string) =
    File.ReadAllText filepath
    |> (fun s -> s.Split $"{newline}{newline}")
    |> Array.map (fun s -> s.TrimEnd())
    |> (fun arr ->
        let stacks = (Array.head arr).Split(newline) |> parseStacks
        let instructions = (Array.last arr).Split(newline) |> Array.map parseInstruction |> Array.choose id
        (stacks, instructions))

let partOne (stacks:Dictionary<int,Stack<char>>) (instructions:array<Instruction>) =
    for instruction in instructions do
        let fromStack = stacks[instruction.From]
        let toStack = stacks[instruction.To]
        for _ in 1..instruction.Count do
            let entry = fromStack.Pop()
            toStack.Push(entry)

    stacks.Keys
    |> Seq.toArray
    |> Array.sort
    |> Array.map (fun index -> stacks[index].Peek())
    |> String.Concat

let partTwo (stacks:Dictionary<int,Stack<char>>) (instructions:array<Instruction>) =
    for instruction in instructions do
        let fromStack = stacks[instruction.From]
        let toStack = stacks[instruction.To]
        let tempStack = Stack<char>()
        for _ in 1..instruction.Count do
            let entry = fromStack.Pop()
            tempStack.Push(entry)
        for _ in 1..instruction.Count do
            let entry = tempStack.Pop()
            toStack.Push(entry)
    
    stacks.Keys
    |> Seq.toArray
    |> Array.sort
    |> Array.map (fun index -> stacks[index].Peek())
    |> String.Concat

assert (parse "day05/test_input.txt" ||> partOne = "CMZ")
parse "day05/input.txt" ||> partOne |> printfn "%s"

assert (parse "day05/test_input.txt" ||> partTwo = "MCD")
parse "day05/input.txt" ||> partTwo |> printfn "%s"
