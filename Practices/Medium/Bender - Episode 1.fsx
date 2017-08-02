// Types declaration
type Direction = NORTH | EAST | SOUTH | WEST 
type CellType = EMPTY | BENDER | SUICIDE_BOOTH | TELEPORTER | UNBREAKABLE_OBSTACLE | OBSTACLE | BEER | INVERTER | DIRECTION_MODIFIER of Direction
type InversionType = NOT_INVERTED | INVERTED
type BenderMode = NORMAL_MODE | BREAKER_MODE
type Position = { X: int; Y: int }
type BenderState = { Position: Position; Direction: Direction; Inversion: InversionType; Mode: BenderMode }

let directionPriorities = [| SOUTH; EAST; NORTH; WEST |]
let directionPrioritiesInverted = [| WEST; NORTH; EAST; SOUTH |]

// Methods declaration
let getPositionFromIndex lineSize i = 
    let x = i % lineSize;
    let y = i / lineSize;
    { X = x; Y = y }

let getIndexFromPosition lineSize position =
    position.X + (position.Y * lineSize)

let readLines n = Seq.init n (fun _ -> System.Console.In.ReadLine())
let flatMapLines strs = Seq.fold (fun acc elem -> acc + elem) "" strs

let parseCell c =
    match c with
    | ' ' -> EMPTY
    | '@' -> BENDER
    | '$' -> SUICIDE_BOOTH
    | 'T' -> TELEPORTER
    | '#' -> UNBREAKABLE_OBSTACLE
    | 'X' -> OBSTACLE
    | 'B' -> BEER
    | 'I' -> INVERTER
    | 'N' -> DIRECTION_MODIFIER NORTH
    | 'E' -> DIRECTION_MODIFIER EAST
    | 'S' -> DIRECTION_MODIFIER SOUTH
    | 'W' -> DIRECTION_MODIFIER WEST
    | c -> failwith "Is this Fry?"

let parseMapFromStr = Seq.mapi (fun i c -> parseCell c) >> Seq.toArray
let readMap = readLines >> flatMapLines >> parseMapFromStr

let getPositionOfCellType lineSize map cellType =
    map
    |> Array.findIndex (fun c -> c = cellType)
    |> getPositionFromIndex lineSize

let getPositionsOfCellType lineSize map cellType =
    map
    |> Array.mapi (fun i c -> (i,c))
    |> Array.filter (fun (i,c) -> c = cellType)
    |> Array.map (fun (i,c) -> getPositionFromIndex lineSize i)

let getCellAtPosition lineSize (map: CellType array) position =
    let index = getIndexFromPosition lineSize position
    map.[index]

let changeCellAtPosition lineSize (map: CellType array) position cellType = 
    let index = getIndexFromPosition lineSize position
    map.[index] <- cellType

let getNewX direction x =
    match direction with
    | NORTH | SOUTH -> x
    | WEST -> x - 1
    | EAST -> x + 1

let getNewY direction y =
    match direction with
    | WEST | EAST -> y
    | NORTH -> y - 1
    | SOUTH -> y + 1

let getNewPositionForDirection direction position =
    let x = getNewX direction position.X
    let y = getNewY direction position.Y
    { X = x; Y = y }

let changeBenderPosition benderState direction =
    let newPosition = getNewPositionForDirection direction benderState.Position
    {benderState with Position = newPosition }

let printDirection direction =
    match direction with
    | NORTH -> printfn "NORTH"
    | EAST -> printfn "EAST"
    | SOUTH -> printfn "SOUTH"
    | WEST -> printfn "WEST"

let isBlockedByCell mode cellType =
    match cellType with
    | EMPTY | SUICIDE_BOOTH | TELEPORTER | BEER | INVERTER -> false
    | DIRECTION_MODIFIER d -> false
    | OBSTACLE when mode = BREAKER_MODE -> false
    | UNBREAKABLE_OBSTACLE | OBSTACLE -> true
    | BENDER -> failwith "Wait. Another Bender?"

let getDirectionPriorities inversionMode =
    match inversionMode with
    | NOT_INVERTED -> directionPriorities
    | INVERTED -> directionPrioritiesInverted

let getNextDirection columnsNumber map inversionMode benderMode currentPosition currentDirection =
    getDirectionPriorities inversionMode
    |> Array.except [| currentDirection |]
    |> Array.find (fun d -> 
        let nextPosition = getNewPositionForDirection d currentPosition
        let nextCellType = getCellAtPosition columnsNumber map nextPosition
        not (isBlockedByCell benderMode nextCellType)
    )

let getNewInversion currentInversion =
    match currentInversion with
    | NOT_INVERTED -> INVERTED
    | INVERTED -> NOT_INVERTED

let getNewMode currentMode =
    match currentMode with
    | NORMAL_MODE -> BREAKER_MODE
    | BREAKER_MODE -> NORMAL_MODE

let hasAlreadyBeenThere states state =
    states |> Seq.tryFind (fun s -> s = state) |> Option.isSome

// Input
let token = (System.Console.In.ReadLine()).Split [|' '|]

let linesNumber = token.[0] |> int
let columnsNumber = token.[1] |> int

// Compute initial data
let map = readMap linesNumber
let benderInitialPosition = getPositionOfCellType columnsNumber map BENDER
let suicideBoothPosition = getPositionOfCellType columnsNumber map SUICIDE_BOOTH
let teleporterPositions = getPositionsOfCellType columnsNumber map TELEPORTER

let mutable benderState = { Position = benderInitialPosition; Direction = SOUTH; Inversion = NOT_INVERTED; Mode = NORMAL_MODE }
let mutable isLooping = false
let states = new ResizeArray<BenderState>()
let statesSinceLastBreak = new ResizeArray<BenderState>()

// Partially apply functions
let updateCellAtPositionFn = changeCellAtPosition columnsNumber map
let getCellAtPositionFn = getCellAtPosition columnsNumber map
let updateBenderDirectionFn direction = benderState <- { benderState with Direction = direction }
let updateBenderPositionFn direction = benderState <- changeBenderPosition benderState direction
let getNextDirectionFn() = getNextDirection columnsNumber map benderState.Inversion benderState.Mode benderState.Position benderState.Direction

let getOtherTeleporterPositionFn position = 
    teleporterPositions |> Array.find (fun p -> p <> position)

let updateBenderAndMapFn cellType = 
    match cellType with
    | DIRECTION_MODIFIER d ->
        benderState <- {benderState with Direction = d}
    | BEER ->
        benderState <- {benderState with Mode = getNewMode benderState.Mode}
    | INVERTER ->
        benderState <- {benderState with Inversion = getNewInversion benderState.Inversion}
    | TELEPORTER ->
        benderState <- {benderState with Position = getOtherTeleporterPositionFn benderState.Position}
    | OBSTACLE ->
        updateCellAtPositionFn benderState.Position EMPTY
    |_ -> ()

// Remove bender from the map
updateCellAtPositionFn benderInitialPosition EMPTY

// Game loop
while benderState.Position <> suicideBoothPosition && not isLooping do
    // Do the special action of the cell
    let currentCellType = getCellAtPositionFn benderState.Position

    if currentCellType = OBSTACLE then statesSinceLastBreak.Clear() else ()

    updateBenderAndMapFn currentCellType

    // Check if Bender can continue in the same direction
    let nextPosition = getNewPositionForDirection benderState.Direction benderState.Position
    let nextCellType = getCellAtPositionFn nextPosition
    let mustChangeDirection = isBlockedByCell benderState.Mode nextCellType

    // If Bender can continue, then continue; otherwise get the next possible direction
    let newDirection = 
        if mustChangeDirection = false then
            benderState.Direction
        else
            getNextDirectionFn()

    // Update Bender's direction
    updateBenderDirectionFn newDirection

    // Check if Bender is looping
    if hasAlreadyBeenThere statesSinceLastBreak benderState then
        isLooping <- true
    else
        // Save bender current state
        states.Add benderState
        statesSinceLastBreak.Add benderState

        // Update Bender to its new position
        updateBenderPositionFn benderState.Direction

if isLooping then
    printfn "LOOP"
else
    // Print Bender every directions taken
    states |> Seq.iter (fun s -> printDirection s.Direction)