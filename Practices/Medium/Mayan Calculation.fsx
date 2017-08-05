type Digit = Digit of string array
type Dialect = Dialect of Digit array
type Operation = ADD | SUBSTRACT | MULTIPLY | DIVIDE | UNKNOWN

let getDigitsFromDialect (Dialect arr) = arr

let readLines n = Array.init n (fun _ -> System.Console.In.ReadLine())
let flatLines strs = Seq.fold (fun acc elem -> acc + elem) "" strs

let everyNth chunkSize wantedNth seq = 
    seq |> Seq.mapi (fun i el -> el, i)              
        |> Seq.filter (fun (el, i) -> i % chunkSize = wantedNth) 
        |> Seq.map fst
        
let getDigitsFromLine line =
    [0..19] |> List.map (fun i -> everyNth 20 i line)
            |> List.map (fun s -> Digit (Seq.toArray s))
            |> List.toArray
            |> Dialect

let findIndexInDialect dialect digit = dialect |> getDigitsFromDialect |> Array.findIndex (fun d -> d = digit) 

let chunkBySize length str = 
    str |> Seq.chunkBySize length
        |> Seq.map (fun (cs: char[]) -> System.String.Concat(cs))

let convertToRealNumberBasedOnIndex i d =
    d * (pown 20 i)

let addAllParts = Seq.fold (fun acc elem -> acc + elem) 0

let readDigit length dialect = chunkBySize length >> Seq.toArray >> Digit >> findIndexInDialect dialect
let readDialect length = flatLines >> chunkBySize length >> getDigitsFromLine
let readNumber length totalDigitLength dialect = flatLines >> Seq.chunkBySize totalDigitLength >> Seq.map (fun s -> readDigit length dialect s) >> Seq.rev >> Seq.mapi convertToRealNumberBasedOnIndex >> addAllParts

let readOperation op =
    match op with
    | "+" -> ADD
    | "-" -> SUBSTRACT
    | "*" -> MULTIPLY
    | "/" -> DIVIDE
    | _ -> UNKNOWN

let computeOp op aInt bInt =
    let a = int64 aInt
    let b = int64 bInt
    match op with
    | ADD -> a + b
    | SUBSTRACT -> a - b
    | MULTIPLY -> a * b
    | DIVIDE -> a / b
    | UNKNOWN -> failwith "Unknown operation"

let rec decomposeWithState number state =
    match number > 0L with
    | false ->
        match state with
        | [] -> [0]
        | _ -> state
    | true ->
        let numberForCurrentPower = number % 20L
        let rest = (number - numberForCurrentPower) / 20L
        let newState = (int numberForCurrentPower)::state
        decomposeWithState rest newState

let decompose number = decomposeWithState number []
let printDigits (Dialect d) = decompose >> List.map (fun n -> d.[n]) >> List.iter (fun (Digit di) -> Array.iter (fun s -> printfn "%s" s) di)

// Input
let dialectDimensions = (System.Console.In.ReadLine()).Split [|' '|]
let dialectWidth = int(dialectDimensions.[0])
let dialectHeight = int(dialectDimensions.[1])
let dialectLines = readLines dialectHeight

let firstNumberHeight = int(System.Console.In.ReadLine())
let firstNumberLines = readLines firstNumberHeight

let secondNumberHeight = int(System.Console.In.ReadLine())
let secondNumberLines = readLines secondNumberHeight

let operationLine = System.Console.In.ReadLine()

// Computation
let digitTotalLength = dialectWidth * dialectHeight
let dialect = readDialect dialectWidth dialectLines

let readNumberFn = readNumber dialectWidth digitTotalLength dialect

let firstNumber = readNumberFn firstNumberLines
let secondNumber = readNumberFn secondNumberLines
let operation = readOperation operationLine
let operationResult = computeOp operation firstNumber secondNumber

printDigits dialect operationResult