let fizzBuzz n =
    match n with
    | x when n % 3 = 0 && n % 5 = 0 -> "FizzBuzz"
    | x when n % 3 = 0 -> "Fizz"
    | x when n % 5 = 0 -> "Buzz"
    | x -> x.ToString()

[|1..100|] |> Array.map fizzBuzz |> Array.iter (fun s -> printfn "%s" s)

// ----------------------

let tellIf x str (y, s) =
    let newS = if y % x = 0 then s+str else s
    (y, newS)

let tellNumberIfNothing (y, s) =
    match s with
    | "" -> string y
    | x -> x

let init x = (x, "")

let fizz3 = tellIf 3 "Fizz"
let buzz5 = tellIf 5 "Buzz"

let fizzBuzz2 = init >> fizz3 >> buzz5 >> tellNumberIfNothing
 
[|1..100|] |> Array.map fizzBuzz2 |> Array.iter (fun s -> printfn "%s" s)