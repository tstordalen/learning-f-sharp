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


// x^n can be expressed recursively as x * x ^ (n-1)
let rec recursivePower x n = 
    match n with
    | 0 -> 1
    | y -> x * recursivePower x (n-1);

printfn "2 to the power of 3 is %i" (recursivePower 2 3);

//The above problem can also be solved using a tuple 
//The below can represent 2^3 
let twoPowThree = (2,3);

//Crash course on accessing tuple values, commence! 
//The below works for tuples of length two only
printfn "The base is %i and the exponent is %i" (fst twoPowThree) (snd twoPowThree);

//If they're longer, you have to unpack them
let first, second = twoPowThree;
printfn "The base is %i and the exponent is %i" (first) (second);

//Note that in the below, we do exhaustive pattern matching even 
// though we never mention x! Also note that power (5, -1) results in infinite recursion.
let rec power = function
    | (x, 0) -> 1                       
    | (x, n) -> x * power(x, n-1);  

printfn "2 to the power of 3 is %i" (power (2,3) );

//Order matters for the patterns. If the two clauses in the power function are reversed,
//it would yield an infinite recursion for all inputs. This is because (x, n) matches anything, 
//and the (x, 0) clause is never reached.