open System.IO

type Coord = {x:int; y:int}

let rec step (current:Coord) (target:Coord) =
    match current.x with
    | x when x = target.x ->
        match current.y with
        | y when y = target.y -> [current]
        | y when y < target.y -> current :: step {current with y=y+1} target
        | y when y > target.y -> current :: step {current with y=y-1} target
        | _ -> []
    | x when x < target.x -> current :: step {current with x=x+1} target
    | x when x > target.x -> current :: step {current with x=x-1} target
    | _ -> []

let parseCoord (coord:string) =
    match coord.Split ',' with
    | [| first; second |] -> Some {x=(int first); y=(int second)}
    | _ -> None

let parseLine (line:string) =
    line.Split " -> "
    |> Array.toList
    |> List.choose parseCoord
    |> List.pairwise
    |> List.collect (fun (first, second) -> step first second)
    |> Set.ofList

let parse (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map parseLine
    |> Set.unionMany

let partOne (blocks:Set<Coord>) =
    let rec drop (sand:Set<Coord>) =
        let maxY = sand |> Seq.map (fun c -> c.y) |> Seq.max

        let rec lower (current:Coord) =
            match {current with y=current.y+1} with
            | down when down.y > maxY -> None
            | down when sand.Contains down ->
                match {down with x=down.x-1} with
                | left when sand.Contains left ->
                    match {down with x=down.x+1} with
                    | right when sand.Contains right -> Some current
                    | right -> lower right
                | left -> lower left
            | down -> lower down

        match lower {x=500; y=0} with
        | Some coord -> sand |> Set.add coord |> drop
        | None -> sand

    let blocksWithSand = drop blocks
    blocksWithSand.Count - blocks.Count

let partTwo (blocks:Set<Coord>) =
    let maxY = blocks |> Seq.map (fun c -> c.y) |> Seq.max

    let rec drop (sand:Set<Coord>) =
        let rec lower (current:Coord) =
            if current.y > maxY then
                Some current
            else
                match {current with y=current.y+1} with
                | down when sand.Contains down ->
                    match {down with x=down.x-1} with
                    | left when sand.Contains left ->
                        match {down with x=down.x+1} with
                        | right when sand.Contains right -> if current.y = 0 then None else Some current
                        | right -> lower right
                    | left -> lower left
                | down -> lower down

        match lower {x=500; y=0} with
        | Some coord -> sand |> Set.add coord |> drop
        | None -> sand

    let blocksWithSand = drop blocks
    (blocksWithSand.Count + 1) - blocks.Count


let testInput = parse "day14/test_input.txt"
assert (partOne testInput = 24)

let input = parse "day14/input.txt"
printfn $"{partOne input}"

assert (partTwo testInput = 93)
printfn $"{partTwo input}"
