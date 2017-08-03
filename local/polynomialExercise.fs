// Michael R. Hansen, August 31, 2011
// Program skeleton to be use when solving the exercise on polynomials

// An infix function +. for adding polynomials.
// Has the same preceedence as +.
let rec (+.) p q =
    match (p,q) with
       | ([] , _ )      -> q
       | ( _ , [])      -> p
       | (a::p',b::q')  -> List.append [a+b] <| (+.) p' q';



// multiply a polynimial by a constant
let rec multC c list = 
    match list with
    |  [] -> []
    |  head::tail -> List.append [head*c] <| multC c tail;



// multiply P(x) by x
let multX p = 0::p;;

// An infix function *. for mytiplying polynomials
// Has the same preceedence as *
let rec ( *.) p q =
   match p with
    | []   -> []
    | head::tail   -> (+.) (multC head q)  <| tail *. (multX q);;


// convert a polynomial to a string representation
// you may use an auxiliary function 
let toString p = sprintf "%A" <| p ;;

// examples
let p1 = [1; 2; 3];;

let p2 = [1; 2];;

let p3 = [1; 2; 3; 4];

let p4 = p1 +. p2 *. p3;;

let st = toString p4;;

printfn "%s" st;
