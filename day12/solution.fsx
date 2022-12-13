open System.IO

type Coord = { x : int; y : int; }
type Path = { distance: int; previous: Coord; }

let charToInt (c:char) =
    match c with
    | 'S' -> -1
    | 'E' -> 26
    | _ -> int c - int 'a'

let parse (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map (Seq.toArray >> Array.map charToInt)

let withinRange (first:int) (second:int) =
    match second - first with
    | n when n <= 1 -> true
    | _ -> false

let getNeighbours (grid:array<array<int>>) (x:int) (y:int) =
    [| (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) |]
    |> Array.filter (fun (x, _) -> x >= 0 && x < (grid.Length))
    |> Array.filter (fun (_, y) -> y >= 0 && y < grid[0].Length)
    |> Array.filter (fun (nx, ny) -> withinRange (grid[x][y]) (grid[nx][ny]))

let getAllNeighbours (grid:array<array<int>>) =
    [| 0..(grid.Length-1) |]
    |> Array.collect (fun x ->
        [| 0..(grid[0].Length-1) |]
        |> Array.map (fun y ->
            ((x, y), getNeighbours grid x y)))
    |> dict

let pathGrid (width:int) (height:int) (start:Coord) =
    let mutable grid =
        [| 0..width-1 |]
        |> Array.map (fun x ->
            [| 0..height-1 |]
            |> Array.map (fun y ->
                { distance = 9999; previous = { x=x; y=y } }))
    grid[start.x][start.y] <- { grid[0][0] with distance = 0 }
    grid

let findStart (grid:array<array<int>>) =
    grid
    |> Array.mapi (fun x row ->
        row |> Array.mapi (fun y value -> ({ x=x; y=y }, value)))
    |> Array.collect id
    |> Array.sortBy snd
    |> Array.head
    |> fst

let dijkstra (grid:array<array<int>>) (distancePredicate:int -> int -> int) =
    let startCoord = findStart grid
    let paths = pathGrid grid.Length grid[0].Length startCoord
    let allNeighbours = getAllNeighbours grid
    let mutable active = Set.ofList [ (startCoord.x, startCoord.y) ]

    while not active.IsEmpty do
        let source = active |> Seq.sortBy (fun (x, y) -> (paths[x][y]).distance) |> Seq.head
        let (x, y) = source
        let distance = (paths[x][y]).distance
        let neighbours = allNeighbours[source] |> Seq.filter (active.Contains >> not)
        for neighbour in neighbours do
            let (nx, ny) = neighbour
            let distanceTransform = distancePredicate (grid[nx][ny])
            let neighbourDistance = distanceTransform distance 
            if neighbourDistance < (paths[nx][ny]).distance then
                paths[nx][ny] <- { distance = neighbourDistance; previous = { x=x; y=y } }
                active <- active.Add neighbour
        active <- active.Remove source

    paths

let findFinish (grid:array<array<int>>) =
    grid
    |> Array.mapi (fun x row ->
        row |> Array.mapi (fun y value -> ({x=x; y=y}, value)))
    |> Array.collect id
    |> Array.sortByDescending snd
    |> Array.head
    |> fst

let partOne (grid:array<array<int>>) =
    let distancePredicate (value:int) = (fun d -> d + 1)
    let paths = dijkstra grid distancePredicate
    let finish = findFinish grid
    (paths[finish.x][finish.y]).distance


let partTwo (grid:array<array<int>>) =
    let distancePredicate (value:int) =
        match value with
        | 0 -> fun d -> 0
        | _ -> fun d -> d + 1

    let paths = dijkstra grid distancePredicate
    let finish = findFinish grid
    (paths[finish.x][finish.y]).distance

let testInput = parse "day12/test_input.txt"
let input = parse "day12/input.txt"

assert (partOne testInput = 31)
partOne input |> printfn "%A"

assert (partTwo testInput = 29)
partTwo input |> printfn "%A"
