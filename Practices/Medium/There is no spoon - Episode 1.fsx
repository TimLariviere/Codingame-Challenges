open System

type NodeState = FULL | EMPTY
type Node = { X: int; Y:int; State:NodeState }
type Direction = RIGHT | BOTTOM

let readNLines height = Seq.init height (fun _ -> Console.In.ReadLine())

let convertCharAndIndexToNode gridWidth c i =
    let x = i % gridWidth
    let y = i / gridWidth
    let state = if c = '0' then FULL else EMPTY
    { X = x; Y = y; State = state }

let strListToNodes width =
    Seq.fold (fun acc elem -> acc + elem) ""
    >> Seq.mapi (fun i e -> convertCharAndIndexToNode width e i)
    >> Seq.filter (fun e -> e.State = FULL)
    >> Seq.toList

let findNode nodes node direction =
    let neighborX = if direction = RIGHT then node.X + 1 else node.X
    let neighborY = if direction = RIGHT then node.Y else node.Y + 1
    let optionNode = nodes |> List.tryFind (fun n ->
        if direction = RIGHT then n.Y = node.Y && n.X > node.X
        else n.X = node.X && n.Y > node.Y
    )

    if optionNode.IsSome then optionNode.Value else { X = -1; Y = -1; State = EMPTY }

let getNodeWithNeighbors nodes node =
    let rightNode = findNode nodes node RIGHT
    let bottomNode = findNode nodes node BOTTOM
    (node, rightNode, bottomNode)
    
let printNodeWithNeighbors (firstNode, secondNode, thirdNode) =
    printfn "%i %i %i %i %i %i" firstNode.X firstNode.Y secondNode.X secondNode.Y thirdNode.X thirdNode.Y

let width = int(Console.In.ReadLine()) (* the number of cells on the X axis *)
let height = int(Console.In.ReadLine()) (* the number of cells on the Y axis *)

let nodes = height |> readNLines |> strListToNodes width

nodes
|> List.map (fun n -> getNodeWithNeighbors nodes n)
|> List.iter (fun n -> printNodeWithNeighbors n)

