open System.IO
open System
open System.Collections.Generic

type Direction =
    | Left
    | Right
    | Down

type Coord = {X:int64; Y:int64}

let parseDirections (filepath:string) =
    let parsed = 
        File.ReadAllLines filepath
        |> Array.head
        |> Seq.toArray
        |> Array.choose (fun c ->
            match c with
            | '<' -> Some Left
            | '>' -> Some Right
            | _ -> None)
    
    seq {
        while true do
            for direction in parsed do
                yield direction
    }

let parseBlocks (filepath:string) =
    let newline = Environment.NewLine
    let blocks =
        File.ReadAllText filepath
        |> fun (text) -> text.Trim().Split($"{newline}{newline}")
        |> Array.map (fun block ->
            let lines = block.Split newline
            lines
            |> Array.mapi (fun row line ->
                let y = lines.Length - (row + 1)
                line
                |> Array.ofSeq
                |> Array.mapi (fun col c ->
                    match c with
                    | '#' -> Some {X=col; Y=y}
                    | _ -> None)
                |> Array.choose id)
            |> Array.collect id
            |> Set.ofArray)
    
    seq {
        while true do
            for block in blocks do
                yield block
    }

let getHeight (placed:Set<Coord>) =
    placed
    |> Seq.map (fun coord -> coord.Y)
    |> Seq.max

let getMinimumHeight (placed:Set<Coord>) =
    placed
    |> Seq.map (fun coord -> coord.Y)
    |> Seq.min

let reduce (placed:Set<Coord>) =
    let lowestPeak =
        seq { 0 .. 6 }
        |> Seq.map (fun x ->
            placed
            |> Seq.filter (fun coord -> coord.X = x)
            |> Seq.map (fun coord -> coord.Y)
            |> Seq.max)
        |> Seq.min
    
    let newPlaced = 
        placed
        |> Seq.filter (fun coord -> coord.Y >= (lowestPeak - 10L))
        |> Set.ofSeq

    newPlaced

let place (block:Set<Coord>) (placed:Set<Coord>) (directions:IEnumerator<Direction>) =
    let outOfBounds (current:Set<Coord>) =
        current
        |> Seq.map (fun coord -> coord.X)
        |> Seq.where (fun x -> x < 0L || x > 6L)
        |> Seq.isEmpty
        |> not

    let colliding (current:Set<Coord>) =
        current
        |> Set.intersect placed
        |> Set.count > 0

    let move (direction:Direction) (current:Set<Coord>) =
        let next =
            current
            |> Seq.map (fun coord ->
                match direction with
                | Left -> {X=(coord.X - 1L); Y=coord.Y}
                | Right -> {X=(coord.X + 1L); Y=coord.Y}
                | Down -> {X=coord.X; Y=(coord.Y - 1L)})
            |> Set.ofSeq
        
        match outOfBounds next with
        | true -> current
        | false ->
            match colliding next with
            | true -> current
            | false -> next

    let rec recurse (current:Set<Coord>) =
        directions.MoveNext() |> ignore
        let dir = directions.Current
        let afterSideways = current |> move dir
        let afterDown = afterSideways |> move Down

        match afterDown = afterSideways with
        | true -> afterDown
        | false -> recurse afterDown

    let startHeight = (getHeight placed) + 4L

    block
    |> Seq.map (fun coord -> {X=(coord.X + 2L); Y=(coord.Y + startHeight)})
    |> Set.ofSeq
    |> recurse
    |> Set.union placed
    |> reduce

let pprint (placed:Set<Coord>) =
    let height = getHeight placed
    let minimumHeight = getMinimumHeight placed
    for y in height .. -1L .. minimumHeight do
        seq { 0L .. 6L }
        |> Seq.map (fun x ->
            match Seq.contains {X=x; Y=y} placed with
            | true -> '#'
            | false -> '.')
        |> Seq.toArray
        |> String
        |> printfn "%s"

let partOne (directions:seq<Direction>) =
    let blocks = parseBlocks "day17/blocks.txt"

    let directionsEnumerator = directions.GetEnumerator()

    let mutable placed =
        seq { 0 .. 6 }
        |> Seq.map (fun x -> {X=x; Y=(-1)})
        |> Set.ofSeq
    
    for block in blocks |> Seq.take 2022 do
        placed <- place block placed directionsEnumerator

    (getHeight placed) + 1L

let partTwo (directions:seq<Direction>) (primaryIterations:int64) (period:int64) =
    let blocks = parseBlocks "day17/blocks.txt"
    let blocksEnumerator = blocks.GetEnumerator()
    
    let directionsEnumerator = directions.GetEnumerator()

    let totalIterations = 1_000_000_000_000L
    let secondaryIterations = (totalIterations - primaryIterations) % period

    let mutable placed =
        seq { 0 .. 6 }
        |> Seq.map (fun x -> {X=x; Y=(-1)})
        |> Set.ofSeq

    for _ in 1L .. primaryIterations do
        blocksEnumerator.MoveNext() |> ignore
        let block = blocksEnumerator.Current
        placed <- place block placed directionsEnumerator

    let initialHeight = getHeight placed
    
    for _ in 1L .. period do
        blocksEnumerator.MoveNext() |> ignore
        let block = blocksEnumerator.Current
        placed <- place block placed directionsEnumerator

    let intermediateHeight = getHeight placed
    let diffHeight = intermediateHeight - initialHeight

    for _ in 1L .. secondaryIterations do
        blocksEnumerator.MoveNext() |> ignore
        let block = blocksEnumerator.Current
        placed <- place block placed directionsEnumerator

    let endHeight = getHeight placed
    let extraHeight = endHeight - intermediateHeight

    let multiple = (totalIterations - primaryIterations) / period

    initialHeight + (multiple * diffHeight) + extraHeight + 1L

let testInput = parseDirections "day17/test_input.txt"
assert (partOne testInput = 3068L)
// test data repeats after 27 with period of 35
assert (partTwo testInput 27L 35L = 1514285714288L)

let input = parseDirections "day17/input.txt"
partOne input |> printfn "%A"
// input data repeats after 447 with period of 1700
partTwo input 447L 1700L |> printfn "%A"
