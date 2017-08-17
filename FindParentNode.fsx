type Node = { Name: string; Children: Node array }
type Result = { Parent: Node option; Index: int }
type Request = { Nodes: Node array; NodeToFind: Node }

let findParent rootNodes node =
    let rec findParentWithState nodeToFind parentNode index remainingChildrenNodes =
        match remainingChildrenNodes with
        | [] -> None
        | n::rest when n = nodeToFind -> Some { Parent = parentNode; Index = index }
        | n::rest ->
            let childNode = n.Children |> Array.toList |> findParentWithState nodeToFind (Some n) 0
            if childNode.IsSome then childNode 
            else findParentWithState nodeToFind parentNode (index+1) rest

    rootNodes |> Array.toList |> findParentWithState node None 0

let request =
    let node111 = { Name = "111"; Children = [||] }
    let node112 = { Name = "112"; Children = [||] }
    let node121 = { Name = "121"; Children = [||] }
    let node122 = { Name = "122"; Children = [||] }
    let node211 = { Name = "211"; Children = [||] }
    let node212 = { Name = "212"; Children = [||] }
    let node221 = { Name = "221"; Children = [||] }
    let node222 = { Name = "222"; Children = [||] }

    let node11 = { Name = "11"; Children = [| node111; node112 |] }
    let node12 = { Name = "12"; Children = [| node121; node122 |] }
    let node21 = { Name = "21"; Children = [| node211; node212 |] }
    let node22 = { Name = "22"; Children = [| node221; node222 |] }

    let node1 = { Name = "1"; Children = [| node11; node12 |] }
    let node2 = { Name = "2"; Children = [| node21; node22 |] }

    { Nodes = [| node1; node2 |]; NodeToFind = node2 }
    
findParent request.Nodes request.NodeToFind