let fizzBuzz n =
    match n with
    | x when n % 3 = 0 && n % 5 = 0 -> "FizzBuzz"
    | x when n % 3 = 0 -> "Fizz"
    | x when n % 5 = 0 -> "Buzz"
    | x -> x.ToString()

[|1..100|] |> Array.map fizzBuzz |> Array.iter (fun s -> printfn "%s" s)