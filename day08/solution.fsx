open System.IO

let parse (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map (Seq.toArray >> Array.map (string >> int))

let getVisibility (row:array<int>) =
    let rec recurse (height:int) (trees:list<int>) =
        match trees with
        | tree :: tail when tree > height -> true :: recurse tree tail
        | _ :: tail -> false :: recurse height tail
        | [] -> []

    row
    |> Array.toList
    |> recurse -1
    |> List.toArray

let getVisibleCoords (forest:array<array<bool>>) =
    forest
    |> Array.mapi (fun row trees ->
        trees
        |> Array.mapi (fun col visible -> ((row, col), visible))
        |> Array.where (fun (_, visible) -> visible)
        |> Array.map fst)
    |> Array.collect id
    |> Set.ofArray

let partOne (forest:array<array<int>>) =
    let visibleLeft =
        forest
        |> Array.map getVisibility
        |> getVisibleCoords

    let visibleRight = 
        forest
        |> Array.map (Array.rev >> getVisibility >> Array.rev)
        |> getVisibleCoords

    let visibleTop =
        forest
        |> Array.transpose
        |> Array.map getVisibility
        |> Array.transpose
        |> getVisibleCoords

    let visibleBottom =
        forest 
        |> Array.transpose
        |> Array.map (Array.rev >> getVisibility >> Array.rev)
        |> Array.transpose
        |> getVisibleCoords

    [| visibleLeft; visibleRight; visibleTop; visibleBottom |]
    |> Set.unionMany
    |> Set.count

let countVisible (row:array<int>) (index:int) =
    let rec recurse (maxHeight:int) (height:int) (trees:list<int>) =
        match trees with
        | tree :: _ when tree >= maxHeight -> 1
        | _ :: tail -> 1 + recurse maxHeight height tail
        | [] -> 0

    let before = row[..(index-1)] |> Array.rev |> Array.toList |> recurse row[index] -1
    let after = row[(index+1)..] |> Array.toList |> recurse row[index] -1
    before * after

let partTwo (forest:array<array<int>>) =
    let horizontalCounts =
        forest
        |> Array.map (fun row ->
            [| 0..(row.Length-1) |]
            |> Array.map (countVisible row))

    let verticalCounts =
        forest
        |> Array.transpose
        |> Array.map (fun row ->
            [| 0..(row.Length-1) |]
            |> Array.map (countVisible row))
        |> Array.transpose

    (horizontalCounts, verticalCounts)
    ||> Array.zip
    |> Array.collect (fun (h, v) ->
        (h, v)
        ||> Array.zip
        |> Array.map (fun (a, b) -> a * b))
    |> Array.max

let testInput = parse "day08/test_input.txt"
let input = parse "day08/input.txt"

assert (partOne testInput = 21)
printfn $"{partOne input}"

assert (partTwo testInput = 8)
printfn $"{partTwo input}"
