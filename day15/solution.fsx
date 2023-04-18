open System.IO
open System.Text.RegularExpressions

let pattern = Regex @"Sensor at x=(.+), y=(.+): closest beacon is at x=(.+), y=(.+)"

type Coord = { x:int; y:int; }
type Range = { low:int; high:int }

let parseLine (line:string) =
    match pattern.Match line with
    | m when m.Success ->
        let groups =
            m.Groups
            |> Seq.skip 1
            |> Seq.map ((fun g -> g.Value) >> int)
            |> Seq.toList
        match groups with
        | [ a; b; c; d ] -> Some ({x=a; y=b}, {x=c; y=d})
        | _ -> None
    | _ -> None

let parse (filepath:string) =
    File.ReadAllLines filepath
    |> Array.choose parseLine

let inRange (range:Range) (value:int) =
    value >= range.low && value <= range.high

let rangeLength (range:Range) =
    1 + (range.high - range.low)

let overlaps (left:Range) (right:Range) =
    (inRange left right.low) || (inRange left right.high) || (inRange right left.low) || (inRange right left.high)

let neighbours (left:Range) (right:Range) =
    left.high + 1 = right.low || right.high + 1 = left.low

let combine (left:Range) (right:Range) =
    if overlaps left right || neighbours left right
    then Some {low=min left.low right.low; high=max left.high right.high}
    else None

let chooseFallback (option:Option<'t>) (fallback:'t) =
    match option with
    | Some value -> value
    | None -> fallback

let rec combineRanges (ranges:list<Range>) =
    match ranges with
    | head :: tail ->
        let combined = tail |> List.map (combine head)
        match combined |> List.choose id with
        | [] -> head :: combineRanges tail
        | _ ->
            List.zip combined tail
            |> List.map (fun (c, t) -> chooseFallback c t)
            |> combineRanges
    | [] -> []

let manhattenDistance (first:Coord) (second:Coord) =
    let dx = (first.x - second.x) |> abs
    let dy = (first.y - second.y) |> abs
    dx + dy

let getRange (row:int) (sensor:Coord) (beacon:Coord) =
    let distance = manhattenDistance sensor beacon
    let dy = sensor.y - row |> abs
    match distance - dy with
    | range when range > 0 -> Some {low=sensor.x - range; high=sensor.x + range}
    | _ -> None

let partOne (row:int) (coords:array<Coord*Coord>) =
    let beacons =
        coords
        |> Array.map snd
        |> Array.where (fun beacon -> beacon.y = row)
        |> Array.map (fun beacon -> beacon.x)
        |> Set.ofArray

    let impossibleRanges =
        coords
        |> Array.choose (fun (sensor, beacon) -> getRange row sensor beacon)
        |> Array.toList
        |> combineRanges

    let beaconsInRanges =
        beacons
        |> Seq.where (fun col ->
            impossibleRanges
            |> List.where (fun range -> inRange range col)
            |> List.length > 0)
        |> Seq.length

    impossibleRanges
    |> List.sumBy rangeLength
    |> fun count -> count - beaconsInRanges

let getGap (lower:int) (higher:int) (coords:array<Coord*Coord>) =
    [| lower .. higher |]
    |> Array.Parallel.map (fun row ->
        coords
        |> Array.choose (fun (sensor, beacon) -> getRange row sensor beacon)
        |> Array.toList
        |> combineRanges)
    |> Array.mapi (fun index ranges -> (index, ranges))
    |> Array.where (fun (_, ranges) -> ranges.Length > 1)
    |> Array.tryHead

let partTwo (lower:int) (higher:int) (coords:array<Coord*Coord>) =
    match getGap lower higher coords with
    | Some (y, [first; second]) when first.high + 2 = second.low ->
        let x = first.high + 1 |> int64
        let y = y |> int64
        x * 4_000_000L + y
    | Some (y, [first; second]) when second.high + 2 = first.low ->
        let x = second.high + 1 |> int64
        let y = y |> int64
        x * 4_000_000L + y
    | _ -> 0L

let testInput = parse "day15/test_input.txt"
assert (testInput |> partOne 10 = 26)

let input = parse "day15/input.txt"
input |> partOne 2_000_000 |> printfn "%A"

assert (testInput |> partTwo 0 20 = 56000011L)
input |> partTwo 0 4_000_000 |> printfn "%A"
