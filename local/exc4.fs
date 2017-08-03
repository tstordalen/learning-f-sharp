open System;

type TestResult = 
    | Success
    | Failure of string * string

let SummarizeTest testResult =
    match testResult with
        | Success -> ();
        | Failure (functionName, message) -> printfn "!!FAILED!!Test '%s' failed. Error: %s" functionName message;


let TestAreEqualWithoutStringifier (stringifier : 'a -> string) testName (expected : 'a) (actual : 'a) = 
    let testResult = 
        if expected = actual then Success
        else 
            let failureMessage = String.Format("Expected '{0}' but got '{1}'", stringifier expected, stringifier actual);
            Failure (testName, failureMessage);
   
    SummarizeTest testResult;

let TestAreEqual testName expected actual = 
    TestAreEqualWithoutStringifier (fun a -> sprintf "%A" a) testName expected actual;

let TestIsTrue testName actual =
    TestAreEqual testName true actual;

let TestIsFalse testName actual = 
    TestAreEqual testName false actual;


//Exercise 4.1
//Declare a function uptoN n, such that it's result is the list [1; 2; ...; n]. I assume without doing let upto n = [1..n];
let rec upToFrom limit from = 
    if from <= limit then
        from :: upToFrom limit (from + 1);
    else [];

let upto n = upToFrom n 1;
        
for i = -100 to 100 do 
    let str = sprintf "Test upto  with i = %i" i;
    TestAreEqual str [1..i] (upto i);


//Exercise 4.2
//Declare a function downFrom n, such that the result is [n; n-1; ... ; 2; 1]
let downFrom n = upto n |> List.rev;


//Exercise 4.3 
//Declare a function EvenN such that the result is the list of the first N even numbers
let EvenN n = 
    match n with
    | x when x < 0 -> 
        failwith "n in EvenN should be positive"
    | _ -> 
        [2 .. 2 .. 2 * n];

//Exercise 4.9
//Create a function zip such that zip([1;3;5], [2;4;6]) = [(1,2); (3,4); (5,6)];
let rec zip = function
    | (x::xs, y::ys) -> 
        (x,y) :: zip (xs, ys)
    | ([], []) -> []
    | _ -> failwith "Lists in function zip are of unequal length";
    

printfn "%A" <| zip ([1;2;3], [4;5;6]);

//Exercise 4.11
//Create a function count int list * int -> int that counts the occurences of an integer in a 
//weakling ascending list
let rec count list intToCount = 
    match list with 
    | [] -> 0
    | x::xs -> 
        if x = intToCount then 1 + count xs intToCount
        else count xs intToCount;


//Create a function that given a weakly ascended list and an integer i returns the weakly ascending list the results from inserting
// i into the original list
let rec insert list i = 
    match list with
    | [] -> [i]
    | x::xs -> 
        if i <= x then i :: x :: xs
        else x :: insert xs i

//Create a function intersect that accepts two weakly ascending lists as parameters and returns one weakly ascending list 
//containing the common elements of the two lists. (reallt shitty explanation in the book aswell)
let rec intersect list1 list2 = 
    match (list1, list2) with
    | ([], _) | (_, []) -> [] //No intersect
    | (x::xs, y::ys) when x = y -> 
        x :: intersect xs ys; //One intersected, advance both
    //Next two, no intersect, advance in list with smallest head
    | (x::xs, y::ys) when x < y -> 
        intersect xs list2;
    | _ -> 
        intersect <| list1 <| List.tail list2;


//Create a function plus list1 list2 which takes two weakly ascending lists and returns the union of the two, which should also 
//be weakly ascending
let rec plus list1 list2 = 
    match (list1, list2) with 
    | (_, []) -> list1
    | ([], _) -> list2
    | (x::xs, y::ys) -> 
        if x <= y then x :: plus xs list2
        else y :: plus list1 ys;

let rec minus list1 list2 = 
    match (list1, list2) with 
    | ([], _) | (_, []) -> list1
    | (x::xs, y::ys) when x = y -> minus xs ys
    | (x::xs, y::ys) when x < y -> x :: minus xs list2
    | (x::xs, y::ys) when x > y -> minus list1 ys
    | _ -> [] //Won't happen
