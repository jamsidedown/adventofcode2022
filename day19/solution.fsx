open System.IO
open System.Text.RegularExpressions

let blueprintPattern = Regex @"^Blueprint \d+: Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.$"

type Mineral =
    | Ore
    | Clay
    | Obsidian
    | Geode

type Minerals = {
    ore:int;
    clay:int;
    obsidian:int;
    geode:int
}

type Blueprint = {
    oreCost:Minerals;
    clayCost:Minerals;
    obsidianCost:Minerals;
    geodeCost:Minerals
}

let defaultMinerals = {ore=0; clay=0; obsidian=0; geode=0}

let parse (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map (fun line ->
        match blueprintPattern.IsMatch line with
        | true ->
            blueprintPattern.Match line
            |> fun m -> m.Groups
            |> Seq.skip 1
            |> Seq.map (fun g -> g.Value)
            |> Seq.map int
            |> Seq.toArray
            |> function
            | [| oreCost; clayCost; obsidianOreCost; obsidianClayCost; geodeOreCost; geodeObsidianCost |] ->
                Some {
                    oreCost={defaultMinerals with ore=oreCost};
                    clayCost={defaultMinerals with ore=clayCost};
                    obsidianCost={defaultMinerals with ore=obsidianOreCost; clay=obsidianClayCost};
                    geodeCost={defaultMinerals with ore=geodeOreCost; obsidian=geodeObsidianCost}}
            | _ -> None
        | _ -> None)
    |> Array.choose id

let produce (robots:Minerals) (materials:Minerals) =
    {
        ore=(materials.ore + robots.ore);
        clay=(materials.clay + robots.clay);
        obsidian=(materials.obsidian + robots.obsidian);
        geode=(materials.geode + robots.geode)
    }

let greaterThan (a:Minerals) (b:Minerals) =
    (a.ore >= b.ore) && (a.clay >= b.clay) && (a.obsidian >= b.obsidian) && (a.geode >= b.geode)

let getOptions (blueprint:Blueprint) (materials:Minerals) =
    seq {
        yield None
        if greaterThan materials blueprint.oreCost then yield Some Ore
        if greaterThan materials blueprint.clayCost then yield Some Clay
        if greaterThan materials blueprint.obsidianCost then yield Some Obsidian
        if greaterThan materials blueprint.geodeCost then yield Some Geode
    } |> Seq.toArray

let subtract (a:Minerals) (b:Minerals) =
    {
        ore=(a.ore - b.ore);
        clay=(a.clay - b.clay);
        obsidian=(a.obsidian - b.obsidian);
        geode=(a.geode - b.geode)
    }

let run (turns:int) (blueprint:Blueprint) =
    let rec recurse (robots:Minerals) (materials:Minerals) (turns:int) =
        match turns with
        | 0 -> materials.geode
        | _ ->
            let nextTurn = turns - 1

            getOptions blueprint materials
            |> Array.map (fun robot ->
                match robot with
                | None -> recurse robots (produce robots materials) nextTurn
                | Some Ore -> recurse {robots with ore=(robots.ore + 1)} (produce robots (subtract materials blueprint.oreCost)) nextTurn
                | Some Clay -> recurse {robots with clay=(robots.clay + 1)} (produce robots (subtract materials blueprint.clayCost)) nextTurn
                | Some Obsidian -> recurse {robots with obsidian=(robots.obsidian + 1)} (produce robots (subtract materials blueprint.obsidianCost)) nextTurn
                | Some Geode -> recurse {robots with geode=(robots.geode + 1)} (produce robots (subtract materials blueprint.geodeCost)) nextTurn)
            |> Array.max

    recurse {defaultMinerals with ore=1} defaultMinerals turns

let partOne (blueprints: array<Blueprint>) =

    blueprints
    // |> Array.map (run 24)
    |> Array.map (fun blueprint ->
        let result = run 24 blueprint
        printfn $"{result}"
        result)
    |> Array.max

let testInput = parse "day19/test_input.txt"
// assert (partOne testInput = 12)
testInput |> partOne |> printfn "%A"

let input = parse "day19/input.txt"
// partOne input |> printfn "%A"
