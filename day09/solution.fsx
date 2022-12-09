open System
open System.IO

type Direction = Up | Down | Left | Right
type Instruction = {dir:Direction; dist:int}
type Coord = {x:int; y:int}

let parseLine (line:string) =
    match line.Split ' ' with
    | [| "U"; distance |] -> Some {dir=Up; dist=int distance}
    | [| "D"; distance |] -> Some {dir=Down; dist=int distance}
    | [| "L"; distance |] -> Some {dir=Left; dist=int distance}
    | [| "R"; distance |] -> Some {dir=Right; dist=int distance}
    | _ -> None

let parse (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map parseLine
    |> Array.choose id

let add (a:Coord) (b:Coord) =
    {x=a.x + b.x; y=a.y + b.y}

let distance (head:Coord) (tail:Coord) =
    let dx = head.x - tail.x |> Math.Abs
    let dy = head.y - tail.y |> Math.Abs
    [| dx; dy |] |> Array.max

let vector (head:Coord) (tail:Coord) =
    let vec (a:int) (b:int) =
        match a - b with
        | c when c < 0 -> -1
        | c when c > 0 -> 1
        | _ -> 0

    let vx = vec head.x tail.x
    let vy = vec head.y tail.y
    {x=vx; y=vy}

let partOne (instructions:array<Instruction>) =
    let mutable head = {x=0; y=0}
    let mutable tail = {x=0; y=0}
    let mutable path = Set<Coord>(seq {{x=0; y=0}})

    for instruction in instructions do
        for _ in [| 1..instruction.dist |] do
            match instruction.dir with
            | Up -> head <- {x=head.x; y=head.y + 1}
            | Down -> head <- {x=head.x; y=head.y - 1}
            | Left -> head <- {x=head.x - 1; y=head.y}
            | Right -> head <- {x=head.x + 1; y=head.y}

            match distance head tail with
            | d when d > 1 ->
                let vec = vector head tail
                tail <- add tail vec
                path <- path.Add tail
            | _ -> ()

    path |> Set.count

let testInput = parse "day09/test_input.txt"
let input = parse "day09/input.txt"

assert (partOne testInput = 13)
printfn $"{partOne input}"
