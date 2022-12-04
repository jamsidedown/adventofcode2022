open System.IO

type Elf = {Low:int; High:int}

let parseElf (input:string) =
    input.Split '-'
    |> Array.map int
    |> function
        | [| low; high |] -> Some {Low=low; High=high}
        | _ -> None

let parseLine (line:string) =
    line.Split ','
    |> Array.map parseElf
    |> Array.choose id
    |> function
        | [| first; second; |] -> Some (first, second)
        | _ -> None

let contains (first:Elf) (second:Elf) =
    first.Low <= second.Low && first.High >= second.High

let overlap (first:Elf) (second:Elf) =
    (first.Low >= second.Low && first.Low <= second.High)

let parse (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map parseLine
    |> Array.choose id

let partOne (elves:array<Elf*Elf>) =
    elves
    |> Array.where (fun (first, second) -> contains first second || contains second first)
    |> Array.length

let partTwo (elves:array<Elf*Elf>) =
    elves
    |> Array.where (fun (first, second) -> overlap first second || overlap second first)
    |> Array.length

let testInput = parse "day04/test_input.txt"
let input = parse "day04/input.txt"

assert (partOne testInput = 2)
printfn $"{partOne input}"

assert (partTwo testInput = 4)
printfn $"{partTwo input}"
