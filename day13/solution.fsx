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

let compare (left:string) (right:string) =
    let rec recurse (left:list<char>) (right:list<char>) =
        match left, right with
        | ('[' :: lt, '[' :: rt) -> recurse lt rt
        | (']' :: lt, ']' :: rt) -> recurse lt rt
        | (',' :: lt, ',' :: rt) -> recurse lt rt
        | (']' :: _, _)  -> true
        | (_, ']' :: _) -> false
        | ('[' :: _, rc :: _) when isDigit rc -> recurse left ('[' :: insertBracket right)
        | (lc :: _, '[' :: _) when isDigit lc -> recurse ('[' :: insertBracket left) right
        | (lc :: _, rc :: _) when isDigit lc && isDigit rc ->
            let (lv, lt) = parseInt left
            let (rv, rt) = parseInt right
            match lv with
            | lv when lv < rv -> true
            | lv when lv > rv -> false
            | _ -> recurse lt rt
        | ([], _) -> true
        | _ -> false

    recurse (Seq.toList left) (Seq.toList right)

let rec quicksort (packets:list<string>) =
    match packets with
    | head :: tail ->
        let comparisons = tail |> List.map (fun packet -> (compare packet head, packet))
        let lower = comparisons |> List.where (fun pair -> fst pair = true) |> List.map snd
        let higher = comparisons |> List.where (fun pair -> fst pair = false) |> List.map snd
        (quicksort lower) @ head :: (quicksort higher)
    | [] -> []

let partOne (packets:array<string*string>) =
    packets
    |> Array.mapi (fun index packets -> (index+1, packets ||> compare))
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

assert ((parsePackets $"[1,1,3,1,1]{newline}[1,1,5,1,1]").Value ||> compare = true)
assert ((parsePackets $"[[1],[2,3,4]]{newline}[[1],4]").Value ||> compare = true)
assert ((parsePackets $"[9]{newline}[[8,7,6]]").Value ||> compare = false)
assert ((parsePackets $"[[4,4],4,4]{newline}[[4,4],4,4,4]").Value ||> compare = true)
assert ((parsePackets $"[7,7,7,7]{newline}[7,7,7]").Value ||> compare = false)
assert ((parsePackets $"[]{newline}[3]").Value ||> compare = true)
assert ((parsePackets $"[[[]]]{newline}[[]]").Value ||> compare = false)
assert ((parsePackets $"[1,[2,[3,[4,[5,6,7]]]],8,9]{newline}[1,[2,[3,[4,[5,6,0]]]],8,9]").Value ||> compare = false)

let testInput = parse "day13/test_input.txt"
assert (partOne testInput = 13)

let input = parse "day13/input.txt"
printfn $"{partOne input}"

assert (partTwo testInput = 140)
printfn $"{partTwo input}"
