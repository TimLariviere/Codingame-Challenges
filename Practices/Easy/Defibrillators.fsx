// Units

[<Measure>]
type degree

[<Measure>]
type radian

let degrees_to_radians (d: float<degree>) =
    d * System.Math.PI / 180.0<degree/radian>

let float_to_degree x = x * 1.0<degree>

// Distance

type Position = { Longitude: float<radian>; Latitude: float<radian> }

let computeX a b =
    (b.Longitude - a.Longitude) * cos ((a.Latitude + b.Latitude) / 2.0<radian>)
    
let computeY a b =
    (b.Latitude - a.Latitude)
    
let computeDistance a b =
    let pow2 (x: float<radian>) = (pown (float x) 2) * 1.0<radian>
    let x = computeX a b |> pow2
    let y = computeY a b |> pow2
    let squareroot = (x + y) |> float |> sqrt
    squareroot * 6371.0

// Parse

type Defibrillator = { Id: int; Name: string; Address: string; Phone: string; Longitude: float<degree>; Latitude: float<degree> }

let replaceComma (s: string) =
    s.Replace(',', '.')

let readDegree s = s |> replaceComma |> float |> float_to_degree

let readDegreeAsRadian s = readDegree s |> degrees_to_radians

let readDefibrillator (s: string) =
    let props = s.Split [| ';' |]
    let defibrillator = 
        {
            Id = int props.[0];
            Name = props.[1];
            Address = props.[2];
            Phone = props.[3];
            Longitude = readDegree props.[4];
            Latitude = readDegree props.[5]
        }

    defibrillator

let defibrillator_to_position d: Position =
    let longitude = d.Longitude |> degrees_to_radians
    let latitude = d.Latitude |> degrees_to_radians
    { Longitude = longitude; Latitude = latitude }

// Engine

let getDefibrillatorWithDistance (position: Position) d =
    let p = defibrillator_to_position d
    let distance = computeDistance position p
    (d, distance)

let getClosestDefibrillator position defibrillators =
    defibrillators |> Array.map (fun x -> getDefibrillatorWithDistance position x) |> Array.minBy (fun x -> snd x) |> fst


let LON = (System.Console.In.ReadLine()) |> readDegreeAsRadian
let LAT = (System.Console.In.ReadLine()) |> readDegreeAsRadian
let userPos: Position = { Longitude = LON; Latitude = LAT }

let readNLines n = Array.init n (fun _ -> System.Console.ReadLine())


let getName d =
    d.Name
    
System.Console.In.ReadLine() |> int |> readNLines |> Array.map (fun x -> readDefibrillator x) |> (getClosestDefibrillator userPos) |> getName |> printfn "%s"