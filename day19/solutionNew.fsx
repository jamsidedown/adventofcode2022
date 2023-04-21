open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Threading

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
    geode:int;
}

let toString (minerals:Minerals) =
    $"{minerals.ore} ore, {minerals.clay} clay, {minerals.obsidian} obsidian, {minerals.geode} geode"

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
        obsidian=(materials.obsidian + (robots.obsidian * turns));
        geode=(materials.geode + (robots.geode * turns))
    }

let divideAndRoundUp (a:int) (b:int) =
    match a >= b with
    | true ->
        match a % b with
        | 0 -> a / b
        | _ -> (a / b) + 1
    | false -> 0

let calculateTime (cost:int) (materials:int) (robots:int) =
    match materials with
    | m when m >= cost -> 0
    | _ ->
        let toMake = cost - materials
        let turns = toMake / robots
        if toMake % robots = 0 then turns else turns + 1

let timeToBuild (blueprint:Blueprint) (robots:Minerals) (materials:Minerals) (target:Mineral) =
    match target with
    | Ore -> calculateTime blueprint.oreCost.ore materials.ore robots.ore
    | Clay -> calculateTime blueprint.clayCost.ore materials.ore robots.ore
    | Obsidian -> [| calculateTime blueprint.obsidianCost.ore materials.ore robots.ore; calculateTime blueprint.obsidianCost.clay materials.clay robots.clay |] |> Array.max
    | Geode -> [| calculateTime blueprint.geodeCost.ore materials.ore robots.ore; calculateTime blueprint.geodeCost.obsidian materials.obsidian robots.obsidian |] |> Array.max

let getOptions (blueprint:Blueprint) (robots:Minerals) (materials:Minerals) (turns:int) =
    let maxOreCost = [| blueprint.oreCost.ore; blueprint.clayCost.ore; blueprint.obsidianCost.ore; blueprint.geodeCost.ore |] |> Array.max
    let enoughClay = materials.clay + (robots.clay * turns) > (blueprint.obsidianCost.clay * turns)
    let enoughObsidian = materials.obsidian + (robots.obsidian * turns) > (blueprint.geodeCost.obsidian * turns)

    seq {
        if robots.ore < maxOreCost then yield Ore
        if not enoughClay then yield Clay
        if robots.clay > 0 && not enoughObsidian then yield Obsidian
        if robots.obsidian > 0 then yield Geode
    }
    |> Seq.toArray

let subtract (a:Minerals) (b:Minerals) =
    {
        ore=(a.ore - b.ore);
        clay=(a.clay - b.clay);
        obsidian=(a.obsidian - b.obsidian);
        geode=(a.geode - b.geode)
    }

let getCost (blueprint:Blueprint) (robot:Mineral) =
    match robot with
    | Ore -> blueprint.oreCost
    | Clay -> blueprint.clayCost
    | Obsidian -> blueprint.obsidianCost
    | Geode -> blueprint.geodeCost

let buildRobot (robots:Minerals) (robot:Mineral) =
    match robot with
    | Ore -> {robots with ore=(robots.ore + 1)}
    | Clay -> {robots with clay=(robots.clay + 1)}
    | Obsidian -> {robots with obsidian=(robots.obsidian + 1)}
    | Geode -> {robots with geode=(robots.geode + 1)}

let pprint (robots:Minerals) (materials:Minerals) (turns:int) =
    let turn = 24 - turns
    printfn $"robots: {toString robots};  materials: {toString materials};  turn {turn}"

let run (turns:int) (blueprint:Blueprint) =
    let mutable count = 0L
    let rec recurse (robots:Minerals) (materials:Minerals) (turns:int) (log:bool) =
        // printfn "%A, %A, %i" robots materials turns
        // Thread.Sleep 500
        // let logging =
        //     // log || (
        //     // turns = 13 &&
        //     // robots.ore = 1 &&
        //     // robots.clay = 3 &&
        //     // robots.obsidian = 1 &&
        //     // robots.geode = 0 &&
        //     // materials.ore = 2 &&
        //     // materials.clay = 4 &&
        //     // materials.obsidian = 0 &&
        //     // materials.geode = 0)
        //     count < 1000L

        // if logging then
        //     pprint robots materials turns

        count <- count + 1L

        let options = getOptions blueprint robots materials turns
        // if logging then
        //     printfn "options: %A" options
        options
        |> Array.map (fun robot ->
            // if logging then
            //     printfn "option: %A" robot
            let waitTurns = (timeToBuild blueprint robots materials robot) + 1
            // if logging then
            //     printfn "time to build: %A" waitTurns

            match waitTurns < turns with
            | false ->
                let final = produce robots materials turns
                final.geode
            | true ->
                let cost = getCost blueprint robot

                let beforeBuild = produce robots materials waitTurns
                let afterBuild = subtract beforeBuild cost

                let newRobots = buildRobot robots robot

                recurse newRobots afterBuild (turns - waitTurns) log)
        |> Array.max

    let result = recurse {defaultMinerals with ore=1} defaultMinerals turns false
    // printfn $"finished in {count} iterations"
    result

let partOne (blueprints: array<Blueprint>) =
    blueprints
    |> Array.map (fun blueprint ->
        let result = run 24 blueprint
        // printfn $"{blueprint.id}: {result}, score: {result * blueprint.id}"
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
assert (partOne testInput = 33)
assert (partTwo testInput = (56 * 62))

let input = parse "day19/input.txt"
partOne input |> printfn "%i"
input |> Array.take 3 |> partTwo |> printfn "%i"
