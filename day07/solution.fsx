open System.IO

let parent (dir:string) =
    dir[..((dir.LastIndexOf '/') - 1)]

let rec allParents (dir:string) =
    match dir with
    | "/" | "" -> []
    | _ -> dir :: allParents (parent dir)

let parse (filepath:string) =
    let rec recurse (dir:string) (commands:list<string>) =
        match commands with
        | command :: tail ->
            match command.Split ' ' |> Array.toList with
            | ["$"; "cd"; "/"] -> recurse "" tail
            | ["$"; "cd"; ".."] -> recurse (parent dir) tail
            | ["$"; "cd"; newDir] -> recurse $"{dir}/{newDir}" tail
            | ["$"; "ls"] | "dir" :: _ -> recurse dir tail
            | [size; file] -> ($"{dir}/{file}", int size) :: recurse dir tail
            | _ -> recurse dir tail
        | [] -> []
    
    File.ReadAllLines filepath
    |> Array.toList
    |> recurse ""
    |> Map.ofList

let getDirectories (files:Map<string,int>) =
    files
    |> Seq.collect (fun pair ->
        allParents (parent pair.Key)
        |> Seq.map (fun dir -> (dir, pair.Value)))
    |> Seq.groupBy fst
    |> Seq.map (fun (dir, pairs) ->
        let dirSize = pairs |> Seq.sumBy snd
        (dir, dirSize))

let partOne (files:Map<string,int>) =
    getDirectories files
    |> Seq.sortByDescending snd
    |> Seq.where (fun (_, size) -> size <= 100_000)
    |> Seq.sumBy snd

let partTwo (files:Map<string,int>) =
    let totalSize = files |> Seq.sumBy (fun pair -> pair.Value)
    let unused = 70_000_000 - totalSize
    let toFree = 30_000_000 - unused
    assert (toFree >= 0)
    getDirectories files
    |> Seq.where (fun (_, size) -> size >= toFree)
    |> Seq.sortBy snd
    |> Seq.head
    |> snd

let testInput = parse "day07/test_input.txt"
let input = parse "day07/input.txt"

assert (partOne testInput = 95437)
printfn $"{partOne input}"

assert (partTwo testInput = 24933642)
printfn $"{partTwo input}"
