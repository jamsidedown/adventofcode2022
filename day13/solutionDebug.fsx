open System
open System.IO

let newline = Environment.NewLine

let isDigit (c:char) =
    c >= '0' && c <= '9'

let parseInt (chars:list<char>) =
    let rec recurse (current:string) (chars:list<char>) =
        match chars with
        | c :: tail when isDigit c -> recurse (sprintf "%s%c" current c) tail
        | _ -> (int current, chars)
    recurse "" chars

let rec insertBracket (chars:list<char>) =
    match chars with
    | c :: tail when isDigit c -> c :: insertBracket tail
    | _ -> ']' :: chars

let parsePackets (lines:string) =
    match lines.Split newline with
    | [| left; right |] -> Some (left, right)
    | _ -> None

let parse (filepath:string) =
    File.ReadAllText filepath
    |> (fun s -> s.Trim().Split $"{newline}{newline}")
    |> Array.choose parsePackets

let compare (log:bool) (left:string) (right:string) =
    let rec recurse (left:list<char>) (right:list<char>) (depth:int) =
        match left, right with
        | ('[' :: lt, '[' :: rt) ->
            if log then printfn "depth %i" (depth+1)
            recurse lt rt (depth+1)
        | (']' :: lt, ']' :: rt) ->
            if log then printfn "depth %i" (depth-1)
            recurse lt rt (depth-1)
        | (',' :: lt, ',' :: rt) ->
            if log then printfn "commas"
            recurse lt rt depth
        | (lc :: _, rc :: _) when isDigit lc && isDigit rc ->
            if log then printfn "digits %c %c" lc rc
            let (lv, lt) = parseInt left
            let (rv, rt) = parseInt right
            match lv with
            | lv when lv < rv -> true
            | lv when lv > rv -> false
            | _ -> recurse lt rt depth
        | ('[' :: _, rc :: _) when isDigit rc ->
            if log then printfn "left depth, right digit"
            let rt = '[' :: insertBracket right
            recurse left rt depth
        | (lc :: _, '[' :: _) when isDigit lc ->
            if log then printfn "left digit, right depth"
            let lt = '[' :: insertBracket left
            recurse lt right depth
        | (']' :: _, _)  ->
            if log then printfn "left ran out"
            true
        | (_, ']' :: _) ->
            if log then printfn "right ran out"
            false
        | ([], _) -> true
        | _ ->
            printfn "left %A" left
            printfn "right %A" right
            false
    
    recurse (Seq.toList left) (Seq.toList right) 0

let rec quicksort (packets:list<string>) =
    match packets with
    | head :: tail ->
        let comparisons = tail |> List.map (fun packet -> (compare false packet head, packet))
        let lower = comparisons |> List.where (fun pair -> fst pair = true) |> List.map snd
        let higher = comparisons |> List.where (fun pair -> fst pair = false) |> List.map snd
        (quicksort lower) @ head :: (quicksort higher)
    | [] -> []

let partOne (packets:array<string*string>) =
    packets
    |> Array.mapi (fun index packets -> (index+1, packets ||> compare false))
    |> Array.where snd
    |> Array.sumBy fst

let partTwo (packets:array<string*string>) =
    let sorted =
        packets
        |> Array.toList
        |> List.collect (fun pair -> [fst pair; snd pair])
        |> fun ps -> "[[2]]" :: "[[6]]" :: ps
        |> quicksort
    
    sorted
    |> List.mapi (fun index packet ->
        match packet with
        | "[[2]]" | "[[6]]" -> Some(index+1)
        | _ -> None)
    |> List.choose id
    |> List.reduce ( * )
    

assert ((parsePackets $"[1,1,3,1,1]{newline}[1,1,5,1,1]").Value ||> (compare false) = true)
assert ((parsePackets $"[[1],[2,3,4]]{newline}[[1],4]").Value ||> (compare false) = true)
assert ((parsePackets $"[9]{newline}[[8,7,6]]").Value ||> (compare false) = false)
assert ((parsePackets $"[[4,4],4,4]{newline}[[4,4],4,4,4]").Value ||> (compare false) = true)
assert ((parsePackets $"[7,7,7,7]{newline}[7,7,7]").Value ||> (compare false) = false)
assert ((parsePackets $"[]{newline}[3]").Value ||> (compare false) = true)
assert ((parsePackets $"[[[]]]{newline}[[]]").Value ||> (compare false) = false)
assert ((parsePackets $"[1,[2,[3,[4,[5,6,7]]]],8,9]{newline}[1,[2,[3,[4,[5,6,0]]]],8,9]").Value ||> (compare false) = false)

let testInput = parse "day13/test_input.txt"
assert (partOne testInput = 13)

let input = parse "day13/input.txt"
printfn $"{partOne input}"

assert (partTwo testInput = 140)
printfn $"{partTwo input}"
