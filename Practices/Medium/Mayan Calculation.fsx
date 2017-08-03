type Digit = Digit of string array
type Dialect = Dialect of Digit array

let readLines n = Seq.init n (fun _ -> System.Console.In.ReadLine())
let flatLines strs = Seq.fold (fun acc elem -> acc + elem) "" strs

let everyNth chunkSize wantedNth seq = 
    seq |> Seq.mapi (fun i el -> el, i)              
        |> Seq.filter (fun (el, i) -> i % chunkSize = wantedNth) 
        |> Seq.map fst
        
let getDigitsFromLine line =
    [0..19] |> List.map (fun i -> everyNth 20 i line) |> List.map (fun s -> Digit (Seq.toArray s)) |> List.toArray |> Dialect

// Input
let token = (System.Console.In.ReadLine()).Split [|' '|]
let dialectLines = int(token.[0])
let dialectWidth = int(token.[1])

let readDialect = readLines >> flatLines >> Seq.chunkBySize dialectWidth >> Seq.map (fun cs -> System.String.Concat(cs)) >> getDigitsFromLine

readDialect dialectLines