open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let pattern = Regex @"^Valve (\w\w) has flow rate=(\d+); tunnels lead to valves (.+)$"

type Tunnel = {target:string; distance:int}
type Valve = {key:string; flow:int; tunnels:array<Tunnel>}

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
            Some {
                key=valveKey;
                flow=int flow;
                tunnels=
                    tunnels.Split ", "
                    |> Array.map (fun t -> {target=t; distance=1})
            }
        | _ -> None
    | false -> None

let parse (filepath:string) =
    File.ReadAllLines filepath
    |> Array.choose parseLine
    |> Array.map (fun valve -> (valve.key, valve))
    |> dict

let reduceDistance (from:Tunnel) (dest:Tunnel) =
    {dest with distance=from.distance + dest.distance}

let rec reduceTunnels (valves:IDictionary<string,Valve>) (tunnels:list<Tunnel>) =
    match tunnels with
    | head :: tail -> []
    | [] -> []

let rec reduce (visited:Set<string>) (valves:IDictionary<string,Valve>) =
    let zero =
        valves.Values
        |> Seq.where (fun valve -> Set.contains valve.key visited |> not)
        |> Seq.where (fun valve -> valve.flow = 0)
        |> Seq.tryHead
    match zero with
    | Some valve ->
        let mutable reducedValves = valves
        let tunnels = valve.tunnels |> Set.ofArray
        for tunnel in tunnels do
            let others = tunnels |> Set.remove tunnel

        valves
        // valves
    | None -> valves
    // if an unvisited valve exists with zero flow
    // connect all tunnels together, increasing distances
    // recurse with valve removed and added to visited

    // else return valves

let testInput = parse "day16/test_input.txt"
testInput |> Seq.head |> printfn "%A"
