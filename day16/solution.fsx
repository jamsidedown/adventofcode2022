open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let pattern = Regex @"^Valve (\w\w) has flow rate=(\d+); tunnels? leads? to valves? (.+)$"

// type Tunnel = {target:string; distance:int}
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

let parse (filepath:string) =
    File.ReadAllLines filepath
    |> Array.choose parseLine
    |> Array.map (fun valve -> (valve.key, valve))
    |> Map

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

let testInput = parse "day16/test_input.txt"
testInput.Keys |> Seq.toArray |> printfn "%A"
testInput |> (Set ["AA"] |> reduce) |> Seq.toArray |> printfn "%A"
