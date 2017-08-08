let readAsInt = System.Console.ReadLine >> int
let readBudgetsArray n = List.init n (fun _ -> readAsInt())
let sumBudgets = List.fold (fun acc elem -> acc + elem) 0
let printParticipations = List.iter (fun x -> printfn "%i" x)

type State = { AlreadyPaid: int; Remaining: int; RemainingParticipants: int; ParticipationSoFar: int; Participations: int list }

let rec makeThemPay state sortedBudgets =
    match sortedBudgets with
    | x::[] ->
        let lastParticipation = state.ParticipationSoFar + state.Remaining
        lastParticipation::state.Participations
    | x::rest ->
        let potentialAmount = (x - state.ParticipationSoFar) * state.RemainingParticipants
        let correctedAmount = min potentialAmount state.Remaining
        let amountForOne = correctedAmount / state.RemainingParticipants
        let realAmount = amountForOne * state.RemainingParticipants

        let newAlreadyPaid = state.AlreadyPaid + realAmount
        let newRemaining = state.Remaining - realAmount
        let newRemainingParticipants = state.RemainingParticipants - 1
        let newParticipationSoFar = state.ParticipationSoFar + amountForOne
        let newParticipations = newParticipationSoFar::state.Participations

        let newState = { AlreadyPaid = newAlreadyPaid; Remaining = newRemaining; RemainingParticipants = newRemainingParticipants; ParticipationSoFar = newParticipationSoFar; Participations = newParticipations  }
        makeThemPay newState rest
    | [] -> state.Participations

let computeParticipations totalBudget numberOfParticipants giftPrice budgets =
    budgets
    |> List.sort
    |> makeThemPay { AlreadyPaid = 0; Remaining = giftPrice; RemainingParticipants = numberOfParticipants; ParticipationSoFar = 0; Participations = [] }
    |> List.rev


// Input
let numberOfParticipants = readAsInt()
let giftPrice = readAsInt()
let budgets = readBudgetsArray numberOfParticipants

// Output
let totalBudget = sumBudgets budgets

match totalBudget >= giftPrice with
| false -> printfn "IMPOSSIBLE"
| true ->  
    budgets
    |> computeParticipations totalBudget numberOfParticipants giftPrice
    |> printParticipations