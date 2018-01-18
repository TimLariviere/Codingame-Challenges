type Word = { Value: string; Points: int; Letters: Set<char> }

let readNLines n = List.init n (fun _ -> System.Console.In.ReadLine())

let getLetterPoints =
    function
    | 'e' | 'a' | 'i' | 'o' | 'n' | 'r' | 't' | 'l' | 's' | 'u' -> 1
    | 'd' | 'g' -> 2
    | 'b' | 'c' | 'm' | 'p' -> 3
    | 'f' | 'h' | 'v' | 'w' | 'y' -> 4
    | 'k' -> 5
    | 'j' | 'x' -> 8
    | 'q' | 'z' -> 10
    | _ -> 0
    
let getTotalPoints word =
    word |> List.ofSeq |> List.map getLetterPoints |> List.sum
    
let sortLetters word =
    word |> Seq.sort |> Set.ofSeq
    
let createDictionary words =
    words
    |> Seq.map (fun w -> { Value = w; Points = getTotalPoints w; Letters = sortLetters w })
    |> Seq.distinctBy (fun w -> w.Letters)
    |> Seq.sortByDescending (fun w -> w.Points)
    |> List.ofSeq
    
let findBestWord dict letters =
    let lettersSet = sortLetters letters
    dict 
    |> List.map (fun w -> (w, Set.intersect lettersSet w.Letters)) 
    |> List.tryFind (fun (w, s) -> w.Letters = s && w.Value.Length <= Seq.length letters)
    |> (function
        | Some (w, s) -> w
        | None -> { Value = ""; Points = 0; Letters = [] |> Set.ofList })
        
let dict = System.Console.In.ReadLine() |> int |> readNLines |> createDictionary
System.Console.In.ReadLine() |> findBestWord dict |> (fun w -> printfn "%s" w.Value)
