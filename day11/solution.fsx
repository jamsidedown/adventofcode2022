open System
open System.IO
open System.Collections.Generic

let newline = Environment.NewLine

type Operation =
    | Add of int64
    | Multiply of int64
    | Square

type Monkey = {
    id: int;
    items: Queue<int64>;
    operation: Operation;
    divisor: int64;
    trueTarget: int;
    falseTarget: int;
}

let parseId (line:string) =
    line[7..].TrimEnd ':' |> int

let parseItems (line:string) =
    line[18..].Split ", " |> Array.map int64

let parseOperation (line:string) =
    match line[19..].Split ' ' with
    | [| "old"; "*"; "old"|] -> Square
    | [| "old"; "*"; value|] -> Multiply (int value)
    | [| "old"; "+"; "old"|] -> Multiply 2
    | [| "old"; "+"; value|] -> Add (int value)
    | _ -> Add 0

let parseDivisor (line:string) =
    line[21..] |> int64

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

let runMonkey (monkeys:array<Monkey>) (manage:int64 -> int64) (monkey:Monkey) =
    let count = monkey.items.Count |> int64

    while monkey.items |> Seq.isEmpty |> not do
        let item = monkey.items.Dequeue()
        let operated =
            match monkey.operation with
            | Square -> item * item
            | Add n -> item + n
            | Multiply n -> item * n
        let inspected = manage operated
        match inspected % monkey.divisor with
        | 0L -> monkeys[monkey.trueTarget].items.Enqueue inspected
        | _ -> monkeys[monkey.falseTarget].items.Enqueue inspected

    count

let run (monkeys:array<Monkey>) (rounds:int) (manage:int64 -> int64) =
    let mutable counts = [|0..(monkeys.Length-1)|] |> Array.map (fun _ -> 0L)

    for _ in [|1..rounds|] do
        counts <-
            monkeys |> Array.map (runMonkey monkeys manage)
            |> Array.zip counts
            |> Array.map (fun (a, b) -> a + b)

    counts
    |> Array.sortDescending
    |> Array.take 2
    |> Array.reduce ( * )

let partOne (monkeys:array<Monkey>) =
    run monkeys 20 (fun (x:int64) -> x / 3L)

let partTwo (monkeys:array<Monkey>) =
    let divisor =
        monkeys
        |> Array.map (fun m -> m.divisor)
        |> Array.reduce ( * )

    run monkeys 10_000 (fun x -> x % divisor)

assert (parse "day11/test_input.txt" |> partOne = 10605L)
parse "day11/input.txt" |> partOne |> printfn "%A"

assert (parse "day11/test_input.txt" |> partTwo = 2713310158L)
parse "day11/input.txt" |> partTwo |> printfn "%A"
