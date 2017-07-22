type Mime = { Extension: string; MimeType: string }

let readNLines n = List.init n (fun _ -> System.Console.ReadLine())

let parseAssociations =
    List.map (fun (s:string) -> s.Split [| ' ' |]) >> List.map (fun s -> { Extension = s.[0].ToLower(); MimeType = s.[1] })

let getMimeType associationTable filename =
    let extension = System.IO.Path.GetExtension(filename)
    let extensionWithoutPoint = if extension.Length > 1 then extension.Substring(1).ToLower() else ""
    let entry = List.tryFind (fun x -> x.Extension = extensionWithoutPoint) associationTable
    match entry with
    | Some x -> entry.Value.MimeType
    | None -> "UNKNOWN"

let extensionsNumber = System.Console.In.ReadLine() |> int
let numberOfFiles = System.Console.In.ReadLine() |> int

let associationTable = extensionsNumber |> readNLines |> parseAssociations

numberOfFiles |> readNLines |> List.map (fun x -> getMimeType associationTable x) |> List.map (fun x -> printfn "%s" x)