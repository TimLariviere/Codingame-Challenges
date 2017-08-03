let readNLines n = List.init n (fun _ -> System.Console.In.ReadLine())

let asciiLetterWidth = System.Console.In.ReadLine() |> int
let asciiLetterHeight = System.Console.In.ReadLine() |> int
let sentence = System.Console.In.ReadLine()
let templates = asciiLetterHeight |> readNLines

let indices = sentence.ToLower()
              |> Seq.toList
              |> Seq.map (fun c -> 
                              let value = (int c) - 97
                              if value < 0 || value > 25 then 26
                              else value
                         )
              |> Seq.toList

let getStrForIndex (template:string) letterWidth i =
    let chars = template |> Seq.chunkBySize letterWidth |> Seq.item i
    System.String.Concat(chars)

let rec getLine template letterWidth indices str =
    let getLineFixed = getLine template letterWidth
    match indices with
    | i::rest -> 
        let strForIndex = getStrForIndex template letterWidth i
        let newStr = str + strForIndex
        getLineFixed rest newStr
    | [] -> str

templates |> List.map (fun l -> getLine l asciiLetterWidth indices "") |> List.map (fun x -> printfn "%s" x)