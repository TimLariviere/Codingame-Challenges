(* Auto-generated code below aims at helping you parse *)
(* the standard input according to the problem statement. *)
open System

let n = int(Console.In.ReadLine()) (* the number of temperatures to analyse *)

match n with
    | 0 -> printfn "0"
    | _ -> Console.In.ReadLine().Split [|' '|]
           |> Array.map int
           |> Array.sortBy (fun x -> abs x * 10 + (if x < 0 then 1 else 0))
           |> Array.head
           |> printfn "%i"
