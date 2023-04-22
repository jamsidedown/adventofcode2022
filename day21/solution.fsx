open System.IO
open System
open System.Collections.Generic

type Operator =
    | Add
    | Subtract
    | Multiply
    | Divide

type Shout =
    | Value of int64
    | Equation of string * Operator * string

type Monkey = {
    name: string;
    shout: Shout
}

let parseOperator (opString:string) =
    match opString with
    | "+" -> Add
    | "-" -> Subtract
    | "*" -> Multiply
    | "/" -> Divide
    | _ ->
        raise (ArgumentException "opString")

let parse (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map (fun line ->
        match line.Split " " with
        | [| name; value |] -> Some { name = name.TrimEnd(':'); shout = (Value (int value))}
        | [| name; first; opString; second |] ->
            let operator = parseOperator opString
            let shout = Equation (first, operator, second)
            Some { name = name.TrimEnd(':'); shout = shout }
        | _ -> None)
    |> Array.choose id

let operate (a:int64) (op:Operator) (b:int64) =
    match op with
    | Add -> a + b
    | Subtract -> a - b
    | Multiply -> a * b
    | Divide -> a / b

let partOne (monkeysArr:array<Monkey>) =
    let resolved = Dictionary<string, int64>()

    for monkey in monkeysArr do
        match monkey.shout with
        | Value value -> resolved.Add (monkey.name, value)
        | _ -> ()

    let mutable monkeySet =
        monkeysArr
        |> Array.filter (fun monkey -> resolved.ContainsKey monkey.name |> not)
        |> Set.ofArray

    while not monkeySet.IsEmpty do
        let monkeys = monkeySet
        for monkey in monkeys do
            match monkey.shout with
            | Value value ->
                resolved.Add (monkey.name, value)
                monkeySet <- monkeySet.Remove monkey
            | Equation (first, op, second)
                when resolved.ContainsKey first && resolved.ContainsKey second ->
                    let a = resolved[first]
                    let b = resolved[second]
                    let value = operate a op b
                    resolved.Add (monkey.name, value)
                    monkeySet <- monkeySet.Remove monkey
            | _ -> ()
    
    resolved["root"]

        

let testInput = parse "day21/test_input.txt"
assert (partOne testInput = 152)

let input = parse "day21/input.txt"
partOne input |> printfn "%i"
