type Digit = NONE | ZERO | ONE
type DigitGroup = { Digit: Digit; Number: int }
type State = { PreviousDigit: Digit; Groups: DigitGroup list }

let appendLeadingZeros (str: string) =
    let missingZerosNumber = 7 - str.Length
    let missingZeros = String.replicate missingZerosNumber "0"
    missingZeros + str

let char_to_binaryString (c: char) =
    let i = int c
    let binaryString = System.Convert.ToString(i, 2)
    let finalBinaryString = appendLeadingZeros binaryString
    finalBinaryString

let appendStr acc element =
    acc + element

let appendStrWithSpace acc element =
    match acc with
    | "" -> element
    | x -> acc + " " + element

let char_to_digit c =
    match c with
    | '0' -> ZERO
    | '1' -> ONE
    | x -> NONE

let updateGroups previousDigit newDigit groups =
    if (previousDigit = newDigit) then
        match groups with
        | g::rest -> { Digit = g.Digit; Number = g.Number + 1 } :: rest
        | g -> groups
    else
        { Digit = newDigit; Number = 1 } :: groups
 
let rec groupByDigits str state =
    match str with
    | x::rest -> 
        let newDigit = char_to_digit x
        let newGroups = updateGroups state.PreviousDigit newDigit state.Groups
        let newState = { PreviousDigit = newDigit; Groups = newGroups }
        groupByDigits rest newState
    | x -> state.Groups

let get_groups_from_binaryStrings str =
    groupByDigits str { PreviousDigit = NONE; Groups = [] }

let getDigitStarter digit =
    match digit with
    | ZERO -> "00"
    | ONE -> "0"
    | NONE -> ""

let digitGroup_to_string digitGroup =
    let starter = getDigitStarter digitGroup.Digit
    let filler = String.replicate digitGroup.Number "0"
    starter + " " + filler

let convert_string_to_binaryString =
    Seq.map char_to_binaryString >> Seq.fold appendStr System.String.Empty

let convert_groups_to_string =
    List.map digitGroup_to_string >> List.fold appendStrWithSpace System.String.Empty

let printAsChuckNorrisSays = 
    convert_string_to_binaryString >> Seq.toList >> get_groups_from_binaryStrings >> List.rev >> convert_groups_to_string >> printfn "%s"

System.Console.In.ReadLine() |> printAsChuckNorrisSays

