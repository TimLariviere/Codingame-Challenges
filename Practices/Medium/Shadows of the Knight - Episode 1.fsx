type SearchZone = { X1 : int; Y1: int; X2 : int; Y2: int }
type Direction = UP | UP_RIGHT | RIGHT | DOWN_RIGHT | DOWN | DOWN_LEFT | LEFT | UP_LEFT

let parseDirection str =
    match str with
    | "U" -> UP
    | "UR" -> UP_RIGHT
    | "R" -> RIGHT
    | "DR" -> DOWN_RIGHT
    | "D" -> DOWN
    | "DL" -> DOWN_LEFT
    | "L" -> LEFT
    | "UL" -> UP_LEFT
    | _ -> failwith "Invalid direction"

let readDirection() = System.Console.ReadLine() |> parseDirection

let computeNewSearchZone searchZone batmanPosition newDirection =
    let newX1 = match newDirection with
                | UP_RIGHT | RIGHT | DOWN_RIGHT -> (fst batmanPosition) + 1
                | _ -> searchZone.X1

    let newY1 = match newDirection with
                | DOWN_RIGHT | DOWN | DOWN_LEFT -> (snd batmanPosition) + 1
                | _ -> searchZone.Y1

    let newX2 = match newDirection with
                | UP_LEFT | LEFT | DOWN_LEFT -> (fst batmanPosition) - 1
                | _ -> searchZone.X2

    let newY2 = match newDirection with
                | UP_RIGHT | UP | UP_LEFT -> (snd batmanPosition) - 1
                | _ -> searchZone.Y2
    {
        X1 = newX1;
        Y1 = newY1;
        X2 = newX2;
        Y2 = newY2;
    }

let getSearchZoneCenterPosition searchZone =
    let x = (searchZone.X2 - searchZone.X1) / 2 + searchZone.X1
    let y = (searchZone.Y2 - searchZone.Y1) / 2 + searchZone.Y1
    (x, y)

// Input
let buildingSize = (System.Console.In.ReadLine()).Split [|' '|]
let buildingWidth = int(buildingSize.[0])
let buildingHeight = int(buildingSize.[1])

let turnsBeforeGameOver = int(System.Console.In.ReadLine())

let batmanPositionStr = (System.Console.In.ReadLine()).Split [|' '|]
let batmanX = int(batmanPositionStr.[0])
let batmanY = int(batmanPositionStr.[1])

// Computation
let mutable searchZone = 
    {
        X1 = 0;
        Y1 = 0;
        X2 = buildingWidth - 1;
        Y2 = buildingHeight - 1;
    }
let mutable batmanPosition = (batmanX, batmanY)

while true do
    let newDirection = readDirection()
    searchZone <- computeNewSearchZone searchZone batmanPosition newDirection
    batmanPosition <- getSearchZoneCenterPosition searchZone
    printfn "%i %i" (fst batmanPosition) (snd batmanPosition)