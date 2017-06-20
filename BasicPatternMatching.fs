//let 1 represent January, 2 february, etc.

// Note: This is a bad pattern match, as the input is not 
// exhaustively matched. That means that there are some cases not covered
// If days < 1 || days > 12, we get an exception. The compiler can discover <i>some</i> 
// non-exhaustive pattern matchings, and will display an error if it does
let DaysInMonth month =
    match month with
    | 1 -> 31
    | 2 -> 28
    | 3 -> 31
    | 4 -> 30
    | 5 -> 31
    | 6 -> 30
    | 7 -> 31
    | 8 -> 31
    | 9 -> 30
    | 10 -> 31
    | 11 -> 30
    | 12 -> 31

//Usage
printfn "The number of days in January is %i, while it is just %i in April" (DaysInMonth 1) (DaysInMonth 4)

//The above can be compressed by using the 'or' pattern.
//Note: new line (and indentation) after '->' for 
//perfect alignment of right hand side expressions
let DaysInMonthCompact month = 
    match month with
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 
        31                            
    | 4 | 6 | 9 | 11 -> 
        30 
    | 2 -> 
        28

printfn "The number of days in January is %i, while it is just %i in April" (DaysInMonthCompact 1) (DaysInMonthCompact 4)


//Using the wildcard pattern, it can be made even more compact. The '_' in the last line matches any 
//value of the input. Note however, that this implementation is no longer equal to the two above.
//This implementation does actually comform to exhausitve pattern matching principle, due to the wildcard.
//While 'DaysInMonth -5' in the first implementation would throw an exception, while this 
//implementation evaluates to 31
let DaysInMonthWildcard month =
    match month with
    | 2 -> 
        28
    | 4 | 6 | 9 | 11 -> 
        30
    | _ -> 
        31

printfn "The number of days in January is %i, while it is just %i in April" (DaysInMonthWildcard 1) (DaysInMonthWildcard 4)