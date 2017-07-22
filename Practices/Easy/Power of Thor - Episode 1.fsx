(* Auto-generated code below aims at helping you parse *)
(* the standard input according to the problem statement. *)
(* --- *)
(* Hint: You can use the debug stream to print initialTX and initialTY, if Thor seems not follow your orders. *)
open System

(* lightX: the X position of the light of power *)
(* lightY: the Y position of the light of power *)
(* initialTX: Thor's starting X position *)
(* initialTY: Thor's starting Y position *)
let token = (Console.In.ReadLine()).Split [|' '|]
let lightX = int(token.[0])
let lightY = int(token.[1])
let initialTX = int(token.[2])
let initialTY = int(token.[3])

type directionsH = UP | DOWN | NOPE
type directionsW = LEFT | RIGHT | NONE

let mutable thorX = initialTX
let mutable thorY = initialTY

let getDirectionH l t =
    let h = l - t
    if h > 0 then DOWN
    else if h < 0 then UP
    else NOPE
    
let getDirectionW l t =
    let w = l - t
    if w > 0 then RIGHT
    else if w < 0 then LEFT
    else NONE
    
let updateThorPosX w =
    match w with
    | LEFT -> thorX <- thorX - 1
    | RIGHT -> thorX <- thorX + 1
    | NONE -> thorX <- thorX
    
let updateThorPosY h =
    match h with
    | UP -> thorY <- thorY - 1
    | DOWN -> thorY <- thorY + 1
    | NOPE -> thorY <- thorY 

let getDirections h w =
    if h = UP && w = LEFT then "NW"
    else if h = UP && w = RIGHT then "NE"
    else if h = UP && w = NONE then "N"
    else if h = DOWN && w = LEFT then "SW"
    else if h = DOWN && w = RIGHT then "SE"
    else if h = DOWN && w = NONE then "S"
    else if h = NOPE && w = LEFT then "W"
    else if h = NOPE && w = RIGHT then "E"
    else ""
    
let printDirectionsAndUpdateThorPosition h w =
    updateThorPosX w
    updateThorPosY h
    printfn "%s" (getDirections h w)


(* game loop *)
while true do
    let remainingTurns = int(Console.In.ReadLine()) (* The remaining amount of turns Thor can move. Do not remove this line. *)
    let h = getDirectionH lightY thorY
    let w = getDirectionW lightX thorX
    printDirectionsAndUpdateThorPosition h w
    ()
