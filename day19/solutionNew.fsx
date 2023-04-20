open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

let blueprintPattern = Regex @"^Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.$"

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
    id:int;
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
            | [| id; oreCost; clayCost; obsidianOreCost; obsidianClayCost; geodeOreCost; geodeObsidianCost |] ->
                Some {
                    id=id;
                    oreCost={defaultMinerals with ore=oreCost};
                    clayCost={defaultMinerals with ore=clayCost};
                    obsidianCost={defaultMinerals with ore=obsidianOreCost; clay=obsidianClayCost};
                    geodeCost={defaultMinerals with ore=geodeOreCost; obsidian=geodeObsidianCost}}
            | _ -> None
        | _ -> None)
    |> Array.choose id

let produce (robots:Minerals) (materials:Minerals) (turns:int) =
    {
        ore=(materials.ore + (robots.ore * turns));
        clay=(materials.clay + (robots.clay * turns));
        obsidian=(materials.obsidian + robots.obsidian);
        geode=(materials.geode + robots.geode)
    }

let greaterThan (a:Minerals) (b:Minerals) =
    (a.ore >= b.ore) && (a.clay >= b.clay) && (a.obsidian >= b.obsidian) && (a.geode >= b.geode)

let timeToBuild (blueprint:Blueprint) (robots:Minerals) (materials:Minerals) (target:Mineral) =
    match target with
    | Ore -> (blueprint.oreCost.ore - materials.ore) / robots.ore
    | Clay -> (blueprint.clayCost.ore - materials.ore) / robots.ore
    | Obsidian -> [| (blueprint.obsidianCost.ore - materials.ore) / robots.ore; (blueprint.obsidianCost.clay - materials.clay) / robots.clay |] |> Array.max
    | Geode -> [| (blueprint.geodeCost.ore - materials.ore) / robots.ore; (blueprint.geodeCost.obsidian - materials.obsidian) / robots.obsidian |] |> Array.max

let getOptions (blueprint:Blueprint) (robots:Minerals) =
    // let canAffordOre = greaterThan materials blueprint.oreCost
    // let canAffordClay = greaterThan materials blueprint.clayCost
    // let canAffordObsidian = greaterThan materials blueprint.obsidianCost
    // let canAffordGeode = greaterThan materials blueprint.geodeCost

    let maxOreCost = [| blueprint.oreCost.ore; blueprint.clayCost.ore; blueprint.obsidianCost.ore; blueprint.geodeCost.ore |] |> Array.max

    seq {
        // if (not canAffordOre || not canAffordClay || not canAffordObsidian || not canAffordGeode) then yield None
        if robots.ore < maxOreCost then yield Some Ore
        if robots.clay < blueprint.obsidianCost.clay then yield Some Clay
        if robots.clay > 0 && robots.obsidian < blueprint.geodeCost.obsidian then yield Some Obsidian
        if robots.obsidian > 0 then yield Some Geode
    }
    |> Seq.toArray
    |> Array.choose id

let subtract (a:Minerals) (b:Minerals) =
    {
        ore=(a.ore - b.ore);
        clay=(a.clay - b.clay);
        obsidian=(a.obsidian - b.obsidian);
        geode=(a.geode - b.geode)
    }

let run (turns:int) (blueprint:Blueprint) =
    let cache = Dictionary<Minerals*Minerals*int, int>()

    let rec recurse (robots:Minerals) (materials:Minerals) (turns:int) =
        let entry = (robots,materials,turns)
        match cache.ContainsKey entry with
        | true ->
            cache[entry]
        | false ->
            let result = 
                match turns with
                | 0 -> materials.geode
                | _ ->
                    // let nextTurn = turns - 1

                    getOptions blueprint robots
                    |> Array.map (fun robot ->
                        match robot with
                        // | None -> recurse robots (produce robots materials) nextTurn
                        | Ore ->
                            let waitTurns = (timeToBuild blueprint robots materials Ore) + 1
                            let usedMaterials = subtract materials blueprint.oreCost
                            match waitTurns <

                            recurse {robots with ore=(robots.ore + 1)} (produce robots (subtract materials blueprint.oreCost)) (turns - waitTurns)
                        | Clay -> recurse {robots with clay=(robots.clay + 1)} (produce robots (subtract materials blueprint.clayCost)) nextTurn
                        | Obsidian -> recurse {robots with obsidian=(robots.obsidian + 1)} (produce robots (subtract materials blueprint.obsidianCost)) nextTurn
                        | Geode -> recurse {robots with geode=(robots.geode + 1)} (produce robots (subtract materials blueprint.geodeCost)) nextTurn)
                    |> Array.max
            cache.Add(entry, result)
            result

    recurse {defaultMinerals with ore=1} defaultMinerals turns

let partOne (blueprints: array<Blueprint>) =
    blueprints
    |> Array.map (fun blueprint ->
        let result = run 24 blueprint
        printfn $"{blueprint.id}: {result}, score: {result * blueprint.id}"
        result * blueprint.id)
    |> Array.sum

let partTwo (blueprints: array<Blueprint>) =
    blueprints
    |> Array.map (fun blueprint ->
        let result = run 32 blueprint
        printfn $"{blueprint.id}: {result}"
        result)
    |> Array.reduce ( * )

let testInput = parse "day19/test_input.txt"
// assert (partOne testInput = 33)
partTwo testInput |> printfn "%i"

let input = parse "day19/input.txt"
// partOne input |> printfn "%A"
