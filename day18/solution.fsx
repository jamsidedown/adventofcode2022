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

let partTwo (cubes:array<Cube>) =
    let maxX = cubes |> Array.map (fun cube -> cube.x) |> Array.max |> fun x -> x + 1
    let maxY = cubes |> Array.map (fun cube -> cube.y) |> Array.max |> fun y -> y + 1
    let maxZ = cubes |> Array.map (fun cube -> cube.z) |> Array.max |> fun z -> z + 1

    let cubeSet = cubes |> Set.ofArray
    let mutable airSet = Set.empty<Cube>

    let getAirNeighbours (current:Cube) =
        let neighbours =
            seq {
                { current with x=(current.x - 1)};
                { current with x=(current.x + 1)};
                { current with y=(current.y - 1)};
                { current with y=(current.y + 1)};
                { current with z=(current.z - 1)};
                { current with z=(current.z + 1)};
            }
            |> Seq.filter (fun cube -> cube.x >= -1 && cube.x <= maxX)
            |> Seq.filter (fun cube -> cube.y >= -1 && cube.y <= maxY)
            |> Seq.filter (fun cube -> cube.z >= -1 && cube.z <= maxZ)
            |> Set.ofSeq
        
        let notCubes = Set.difference neighbours cubeSet
        Set.difference notCubes airSet

    let rec recurse (current:Cube) =
        airSet <- airSet.Add current
        for neighbour in getAirNeighbours current do
            recurse neighbour

    recurse {x=(-1);y=(-1);z=(-1)}

    airSet
    |> Seq.map (fun air ->
        cubes
        |> Array.filter (isNeighbour air)
        |> Array.length)
    |> Seq.sum

    // get max extents of x,y,z
    // starting at 0,0,0 recursively check neighbours up to max extents + 1 to find external air
    //
    // either
    // get cube neighbours of external air
    //
    // or
    // subtract cubes and external air from max extents cube to find internal cubes
    // subtract internal air cube neighbours from total from part one

let testInput = parse "day18/test_input.txt"
assert (partOne testInput = 64)
assert (partTwo testInput = 58)

let input = parse "day18/input.txt"
partOne input |> printfn "%i"
partTwo input |> printfn "%i"
