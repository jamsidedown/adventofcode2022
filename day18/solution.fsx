open System.IO
open System.Collections.Generic

type Cube = { x:int; y:int; z:int; }

let parse (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map (fun s ->
        match s.Split ',' with
        | [| x; y; z |] -> Some {x=(int x); y=(int y); z=(int z)}
        | _ -> None)
    |> Array.choose id

let isNeighbour (a:Cube) (b:Cube) =
    let dx = (a.x - b.x) |> abs
    let dy = (a.y - b.y) |> abs
    let dz = (a.z - b.z) |> abs
    dx + dy + dz = 1

let getNeighbours (cubes:array<Cube>) =
    let neighbours = Dictionary<Cube,HashSet<Cube>>()

    for cube in cubes do
        neighbours.Add(cube, HashSet())
    
    let rec recurse (cubes:list<Cube>) =
        match cubes with
        | head :: tail ->
            for cube in tail do
                match isNeighbour head cube with
                | true ->
                    neighbours[head].Add cube |> ignore
                    neighbours[cube].Add head |> ignore
                | false -> ()
            recurse tail
        | [] -> ()
    
    cubes
    |> Array.toList
    |> recurse

    neighbours

let partOne (cubes:array<Cube>) =
    let neighbours = getNeighbours cubes
    let totalNeighbours =
        neighbours
        |> Seq.map (fun pair -> pair.Value.Count)
        |> Seq.sum
    (cubes.Length * 6) - totalNeighbours

let testInput = parse "day18/test_input.txt"
assert (partOne testInput = 64)

let input = parse "day18/input.txt"
partOne input |> printfn "%i"
