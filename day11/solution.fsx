open System
open System.IO
open System.Collections.Generic

let newline = Environment.NewLine

type Operation =
    | Add of int
    | Multiply of int
    | Square

type Monkey = {
    id: int;
    items: Queue<int>;
    operation: Operation;
    divisor: int;
    trueTarget: int;
    falseTarget: int;
}

let parseId (line:string) =
    line[7..].TrimEnd ':' |> int

let parseItems (line:string) =
    line[18..].Split ", " |> Array.map int

let parseOperation (line:string) =
    match line[19..].Split ' ' with
    | [| "old"; "*"; "old"|] -> Square
    | [| "old"; "*"; value|] -> Multiply (int value)
    | [| "old"; "+"; "old"|] -> Multiply 2
    | [| "old"; "+"; value|] -> Add (int value)
    | _ -> Add 0

let parseDivisor (line:string) =
    line[21..] |> int

let parseTrue (line:string) =
    line[29..] |> int

let parseFalse (line:string) =
    line[30..] |> int

let parseMonkey (lines:array<string>) : Option<Monkey> =
    match lines with
    | [| _id; _items; _operation; _divisor; _true; _false |] -> Some {
            id=parseId _id;
            items=Queue(parseItems _items);
            operation=parseOperation _operation;
            divisor=parseDivisor _divisor;
            trueTarget=parseTrue _true;
            falseTarget=parseFalse _false
        }
    | _ -> None

let parse (filepath:string) =
    File.ReadAllText filepath
    |> fun s -> s.TrimEnd()
    |> fun s -> s.Split $"{newline}{newline}"
    |> Array.map (fun s -> s.Split newline)
    |> Array.map parseMonkey
    |> Array.choose id

let runMonkey (monkeys:array<Monkey>) (monkey:Monkey) =
    let count = monkey.items.Count

    while monkey.items |> Seq.isEmpty |> not do
        let item = monkey.items.Dequeue()
        let operated =
            match monkey.operation with
            | Square -> item * item
            | Add n -> item + n
            | Multiply n -> item * n
        let inspected = operated / 3
        match inspected % monkey.divisor with
        | 0 -> monkeys[monkey.trueTarget].items.Enqueue inspected
        | _ -> monkeys[monkey.falseTarget].items.Enqueue inspected

    count

let runRound (monkeys:array<Monkey>) =
    monkeys |> Array.map (runMonkey monkeys)

let partOne (monkeys:array<Monkey>) =
    let mutable counts = [|0..(monkeys.Length-1)|] |> Array.map (fun _ -> 0)

    for _ in [|1..20|] do
        counts <-
            runRound monkeys
            |> Array.zip counts
            |> Array.map (fun (a, b) -> a + b)

    counts
    |> Array.sortDescending
    |> Array.take 2
    |> Array.reduce ( * )

assert (parse "day11/test_input.txt" |> partOne = 10605)
parse "day11/input.txt" |> partOne |> printfn "%A"
