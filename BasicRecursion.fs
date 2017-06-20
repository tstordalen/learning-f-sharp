//Same old boring factorial example

//Interestingly, though, we have not defined any input to the funciton. 
//It is anonymous, and seems to infer that it is in fact integers that it will be receiving. 
//No match [something] with ..... is defined.
//Also, notice how 'n' seems to be macthing everything (except 0, in this case), AND 
//assigning that somethign to n, so that we can use it in the right hand side expression.
let rec fact = function
    | 0 -> 1
    | n -> n * fact(n-1)

printfn "The factorial of 5 is %i" (fact 5);

//The 'rec' keyword seems to be mandatory, as the compiler will complain that no 
// value or constructor 'fact' is defined when we try to use it recursively.

//alternatively:
let rec factAlt n = 
    match n with
    | 0 -> 1
    | x -> x*factAlt(x-1)

printfn "The factorial of 5 is %i" (factAlt 5);

