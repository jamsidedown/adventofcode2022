open System.IO

let getMarker (data:string) (windowSize:int) =
    data
    |> Seq.windowed windowSize
    |> Seq.where (fun window ->
        let set = Set.ofSeq window
        set.Count = windowSize)
    |> Seq.head
    |> Seq.map string
    |> String.concat ""

let partOne (data:string) =
    let marker = getMarker data 4
    data.IndexOf marker + 4

let partTwo (data:string) =
    let marker = getMarker data 14
    data.IndexOf marker + 14

let input = File.ReadAllText "day06/input.txt"

assert (partOne "bvwbjplbgvbhsrlpgdmjqwftvncz" = 5)
assert (partOne "nppdvjthqldpwncqszvftbrmjlhg" = 6)
assert (partOne "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" = 10)
assert (partOne "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" = 11)
printfn $"{partOne input}"

assert (partTwo "mjqjpqmgbljsphdztnvjfqwrcgsmlb" = 19)
printfn $"{partTwo input}"
