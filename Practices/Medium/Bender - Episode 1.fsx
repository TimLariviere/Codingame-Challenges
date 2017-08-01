// Types declaration
type Direction = NORTH | EAST | SOUTH | WEST 
type CellType = EMPTY | BENDER | SUICIDE_BOOTH | TELEPORTER | UNBREAKABLE_OBSTACLE | OBSTACLE | DIRECTION_MODIFIER of Direction
type InversionType = NOT_INVERTED | INVERTED
type BenderMode = NORMAL_MODE | BREAKER_MODE
type Position = { X: int; Y: int }
type BenderState = { Position: Position; Direction: Direction; Inversion: InversionType; Mode: BenderMode }

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
    | 'N' -> DIRECTION_MODIFIER NORTH
    | 'E' -> DIRECTION_MODIFIER EAST
    | 'S' -> DIRECTION_MODIFIER SOUTH
    | 'W' -> DIRECTION_MODIFIER WEST
    | c -> EMPTY

let parseMapFromStr = Seq.mapi (fun i c -> parseCell c) >> Seq.toArray
let readMap = readLines >> flatMapLines >> parseMapFromStr

let getPositionOfCellType lineSize map cellType =
    map
    |> Array.findIndex (fun c -> c = cellType)
    |> getPositionFromIndex lineSize

let getCellAtPosition lineSize (map: CellType array) position =
    let index = getIndexFromPosition lineSize position
    map.[index]

let changeCellAtPosition lineSize (map: CellType array) position cellType = 
    let index = getIndexFromPosition lineSize position
    map.[index] <- cellType

let updateBenderStateIfOnDirectionModifier benderState cellType =
    match cellType with
    | DIRECTION_MODIFIER direction -> 
        {benderState with Direction = direction}
    | _ -> benderState

let printDirection direction =
    match direction with
    | NORTH -> printfn "NORTH"
    | EAST -> printfn "EAST"
    | SOUTH -> printfn "SOUTH"
    | WEST -> printfn "WEST"

// Input
let token = (System.Console.In.ReadLine()).Split [|' '|]
let linesNumber = token.[0] |> int
let columnsNumber = token.[1] |> int

// Compute initial data
let map = readMap linesNumber
let benderInitialPosition = getPositionOfCellType linesNumber map BENDER
let suicideBoothPosition = getPositionOfCellType linesNumber map SUICIDE_BOOTH

let mutable benderState = { Position = benderInitialPosition; Direction = SOUTH; Inversion = NOT_INVERTED; Mode = NORMAL_MODE }

// Partial apply functions
let changeCellAtPositionFn = changeCellAtPosition columnsNumber map
let getCellAtPositionFn = getCellAtPosition columnsNumber map
let updateBenderStateIfOnDirectionModifierFn cellType = benderState <- updateBenderStateIfOnDirectionModifier benderState cellType

// Remove bender from the map
changeCellAtPositionFn benderInitialPosition EMPTY

while true do
    let currentCellType = getCellAtPositionFn benderState.Position
    updateBenderStateIfOnDirectionModifierFn currentCellType

    printDirection benderState.Direction
    ()