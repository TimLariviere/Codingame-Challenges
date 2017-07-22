open System

let readNLines n = List.init n (fun _ -> Console.ReadLine())
let horses = Console.ReadLine() |> int |> readNLines |> List.map(int)

match horses with
| [] -> printfn "0"
| _ ->
    let sortedHorses = List.sort horses |> List.toArray

    sortedHorses
       |> Array.mapi (fun i x -> if sortedHorses.Length > i+1 then sortedHorses.[i+1] - x else Int32.MaxValue)
       |> Array.min
       |> printfn "%i"