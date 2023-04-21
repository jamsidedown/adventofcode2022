open System.IO

type Node =
    val mutable value: int64
    [<DefaultValue>] val mutable left: Node
    [<DefaultValue>] val mutable right: Node
    new(v:int64) = {value = v}

let parse (filepath:string) =
    let nodes =
        File.ReadAllLines filepath
        |> Array.map int64
        |> Array.map (fun value -> Node(value))

    let getIndex (i:int) =
        (i + nodes.Length) % nodes.Length
    
    let nodesWithIndexes =
        nodes
        |> Array.mapi (fun i n -> (i, n))

    for (i, n) in nodesWithIndexes do
        n.left <- nodes[getIndex (i - 1)]
        n.right <- nodes[getIndex (i + 1)]
    
    nodes

let moveLeft (node:Node) (distance:int) =
    node.left.right <- node.right
    node.right.left <- node.left

    let mutable currentLeft = node.left
    for _ in 1 .. distance do
        currentLeft <- currentLeft.left

    node.left <- currentLeft
    node.right <- currentLeft.right
    currentLeft.right.left <- node
    currentLeft.right <- node

let moveRight (node:Node) (distance:int) =
    node.left.right <- node.right
    node.right.left <- node.left

    let mutable currentRight = node.right
    for _ in 1 .. distance do
        currentRight <- currentRight.right

    node.left <- currentRight.left
    node.right <- currentRight
    currentRight.left.right <- node
    currentRight.left <- node

let normalise (value:int64) (length:int64) =
    match value with
    | v when v > 0L -> value % (length - 1L) |> int
    | _ -> (-value) % (length - 1L) |> int |> fun v -> -v

let move (node:Node) (nodes:array<Node>) =
    let distance = normalise node.value nodes.LongLength
    match distance with
    | d when d > 0 -> moveRight node distance
    | d when d < 0 -> moveLeft node (-distance)
    | _ -> ()

let partOne (filepath:string) =
    let nodes = parse filepath

    for node in nodes do
        move node nodes
    
    let zero =
        nodes
        |> Array.filter (fun n -> n.value = 0)
        |> Array.head

    let mutable current = zero

    seq {
        for _ in 1 .. 3 do
            for _ in 1 .. 1000 do
                current <- current.right
            yield current.value
    } |> Seq.sum

let partTwo (filepath:string) =
    let nodes = parse filepath

    for node in nodes do
        node.value <- node.value * 811589153L

    for _ in 1 .. 10 do
        for node in nodes do
            move node nodes

    let zero =
        nodes
        |> Array.filter (fun n -> n.value = 0L)
        |> Array.head

    let mutable current = zero

    seq {
        for _ in 1 .. 3 do
            for _ in 1 .. 1000 do
                current <- current.right
            yield current.value
    } |> Seq.sum

assert (partOne "day20/test_input.txt" = 3L)
assert (partTwo "day20/test_input.txt" = 1623178306L)

partOne "day20/input.txt" |> printfn "%A"
partTwo "day20/input.txt" |> printfn "%A"
