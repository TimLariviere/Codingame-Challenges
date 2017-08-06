type Group = { Digit: int; Number: int }
type GroupState = { CurrentDigit: int; Groups: Group list }

let rec groupDigitsWithState (digits: int list) (state: GroupState) =
    match digits with
    | x::rest ->
        let groups = if state.CurrentDigit = x then List.tail state.Groups else state.Groups
        let group = if state.CurrentDigit = x then        
                        let lastModifiedGroup = List.head state.Groups                 
                        { lastModifiedGroup with Number = lastModifiedGroup.Number + 1} 
                    else
                        { Digit = x; Number = 1 }

        let newState = { CurrentDigit = x; Groups = group::groups }
        groupDigitsWithState rest newState
    | [] -> state

let groupDigits digits = groupDigitsWithState digits { CurrentDigit = -1; Groups = [] }
let convertGroupStateToList state =
    state.Groups |> List.rev |> List.collect (fun g -> [ g.Number; g.Digit ])

let cleanString (str: string) =
    str.[1..]

let rec getListForLine digits line =
    match line > 0 with
    | false -> digits
    | true ->
        let newDigits = groupDigits digits |> convertGroupStateToList
        getListForLine newDigits (line - 1)

let number = int(System.Console.In.ReadLine())
let lines = int(System.Console.In.ReadLine())

getListForLine [number] (lines - 1)
    |> List.fold (fun acc elem -> acc + " " + string elem) ""
    |> cleanString
    |> printfn "%s"

