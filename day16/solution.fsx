open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let pattern = Regex @"^Valve (\w\w) has flow rate=(\d+); tunnels? leads? to valves? (.+)$"

type Valve = {key:string; flow:int; tunnels:Dictionary<string,int>}

let parseLine (line:string) =
    let m = pattern.Match line
    match m.Success with
    | true ->
        m.Groups
        |> Seq.skip 1
        |> Seq.map (fun x -> x.Value)
        |> Seq.toArray
        |> function
        | [| valveKey; flow; tunnels |] ->
            let tunnelsDict = Dictionary<string,int>()
            for tunnel in tunnels.Split ", " do
                tunnelsDict[tunnel] <- 1
            Some {
                key=valveKey;
                flow=int flow;
                tunnels= tunnelsDict
            }
        | _ -> None
    | false -> None

let rec reduce (visited:Set<string>) (valves:Map<string,Valve>) =
    let rec recurseTunnels (source:string) (tunnels:list<string*int>) =
        match tunnels with
        | (tunnelKey, tunnelDist) :: tail ->
            let valve = valves[tunnelKey]
            valve.tunnels.Remove source |> ignore
            for (otherKey, otherDist) in tail do
                let distance =
                    match valve.tunnels.ContainsKey otherKey with
                    | true ->
                        let existingDist = valve.tunnels[otherKey]
                        min existingDist (tunnelDist + otherDist)
                    | false -> tunnelDist + otherDist
                valves[tunnelKey].tunnels[otherKey] <- distance
                valves[otherKey].tunnels[tunnelKey] <- distance
            recurseTunnels source tail
        | [] -> ()

    let zero =
        valves.Values
        |> Seq.where (fun valve -> Set.contains valve.key visited |> not)
        |> Seq.where (fun valve -> valve.flow = 0)
        |> Seq.tryHead

    match zero with
    | Some valve ->
        valve.tunnels
        |> Seq.map (fun pair -> (pair.Key, pair.Value))
        |> Seq.toList
        |> recurseTunnels valve.key

        reduce (visited.Add valve.key) (valves |> Map.remove valve.key)
    | None -> valves

let parse (filepath:string) =
    File.ReadAllLines filepath
    |> Array.choose parseLine
    |> Array.map (fun valve -> (valve.key, valve))
    |> Map
    |> (Set ["AA"] |> reduce)

let distance (source:string) (dest:string) (valves:Map<string,Valve>) =
    let distances = Dictionary<string,int>()
    for key in valves.Keys do
        distances[key] <- Int32.MaxValue
    distances[source] <- 0

    let mutable active = Set.ofList [source]
    while not active.IsEmpty do
        let current =
            active
            |> Seq.sortBy (fun key -> distances[key])
            |> Seq.head
        let currentDistance = distances[current]
        for neighbour in valves[current].tunnels do
            let neighbourDistance = currentDistance + neighbour.Value
            if neighbourDistance < distances[neighbour.Key] then
                distances[neighbour.Key] <- neighbourDistance
                active <- active.Add neighbour.Key
        active <- active.Remove current

    distances[dest]

let rec combinations (values:list<string>) =
    match values with
    | head :: tail ->
        tail
        |> List.map (fun value -> (head, value))
        |> fun combined -> combined @ combinations tail
    | [] -> []

let getDistances (valves:Map<string,Valve>) =
    valves.Keys
    |> Seq.toList
    |> combinations
    |> List.collect (fun (first, second) ->
        let d = distance first second valves
        [((first,second), d); ((second,first), d)])
    |> Map

let partOne (valves:Map<string,Valve>) =
    let distances = getDistances valves
    let keys = valves.Keys |> Set.ofSeq

    let score (active:seq<String>) =
        active |> Seq.sumBy (fun key -> valves[key].flow)

    let rec recurse (active:Set<string>) (current:string) (timeRemaining:int) =
        match timeRemaining with
        | x when x < 1 -> 0
        | _ ->
            Set.ofSeq active
            |> Set.difference keys
            |> Seq.where (fun dest -> distances[(current,dest)] + 1 < timeRemaining)
            |> Array.ofSeq
            |> Array.Parallel.map (fun dest ->
                let d = distances[(current,dest)] + 1
                let destScore = (d * score active)
                let recurseScore = recurse (active.Add dest) dest (timeRemaining - d)
                destScore + recurseScore)
            |> function
            | [||] -> timeRemaining * score active
            | ls -> ls |> Array.max

    recurse (Set ["AA"]) "AA" 30

let partTwo (valves:Map<string,Valve>) =
    let distances = getDistances valves
    let keys = valves.Keys |> Set.ofSeq

    let score (active:seq<String>) =
        active |> Seq.sumBy (fun key -> valves[key].flow)

    let rec recurse (active:Set<string>) (current:string) (elephant:string) (currentTime:int) (elephantTime:int) (timeRemaining:int) =
        match timeRemaining with
        | x when x < 1 -> 0
        | _ ->
            match (currentTime,elephantTime) with
            | (0,_) ->
                let newActive = active.Add current
                Set.difference keys (newActive.Add elephant)
                |> Seq.where (fun dest -> distances[(current,dest)] + 1 < timeRemaining)
                |> Array.ofSeq
                |> Array.Parallel.map (fun dest ->
                    let newTime = distances[(current,dest)] + 1
                    recurse newActive dest elephant newTime elephantTime timeRemaining)
                |> function
                | [||] -> recurse newActive current elephant timeRemaining elephantTime timeRemaining
                | ls -> ls |> Array.max
            | (_,0) ->
                let newActive = active.Add elephant
                Set.difference keys (newActive.Add current)
                |> Seq.where (fun dest -> distances[(elephant,dest)] + 1 < timeRemaining)
                |> Array.ofSeq
                |> Array.map (fun dest ->
                    let newTime = distances[(elephant,dest)] + 1
                    recurse newActive current dest currentTime newTime timeRemaining)
                |> function
                | [||] -> recurse newActive current elephant currentTime timeRemaining timeRemaining
                | (ls: int array) -> ls |> Array.max
            | _ -> 
                let dt = seq { currentTime; elephantTime; timeRemaining } |> Seq.min
                let recurseScore = recurse active current elephant (currentTime - dt) (elephantTime - dt) (timeRemaining - dt)
                ((score active) * dt) + recurseScore

    recurse (Set ["AA"]) "AA" "AA" 0 0 26

let testInput = parse "day16/test_input.txt"
assert (partOne testInput = 1651)
assert (partTwo testInput = 1707)

let input = parse "day16/input.txt"
partOne input |> printfn "%i"
partTwo input |> printfn "%i"
