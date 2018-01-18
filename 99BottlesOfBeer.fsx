let rec printVerse beers =
    match beers with
    | 0 -> printfn "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall."
    | 1 ->
        printfn "%i bottle of beer on the wall, %i bottle of beer.\nTake one down and pass it around, no more bottles of beer on the wall." 1 1
        printVerse 0
    | x ->
        let remainingBeers = beers - 1
        printfn "%i bottle of beer on the wall, %i bottle of beer.\nTake one down and pass it around, %i bottles of beer on the wall." beers beers remainingBeers
        printVerse remainingBeers
        
        
let printSong() =
    printVerse 99
    
    
printSong()