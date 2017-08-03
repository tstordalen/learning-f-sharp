//2.1: divisibe by 2 and 3, but not 5
let isDivisibleBy divisor dividend = 
    dividend % divisor = 0;

let aNumber = 3; 


let isDivisibleBy2 = isDivisibleBy 2;
let isDivisibleBy3 = isDivisibleBy 3;
let isDivisibleBy5 = isDivisibleBy 5;

let matches number = 
    let result = 
        isDivisibleBy2 number && 
        isDivisibleBy3 number &&
        not <| isDivisibleBy5 number;
    result;

printfn "matches(24) is %b" (matches 24);
printfn "matches(27) is %b" (matches 27);
printfn "matches(29) is %b" (matches 29);
printfn "matches(30) is %b" (matches 30);

//2.2: 
let rec pow (str : string) repeat =
    match repeat with
    | 0 -> ""
    | n -> str + (n-1 |> pow str);

printfn "pow 'hello ' 3 is %s" <| pow "hello " 3;

//Easier:
let rec powAlt str repeat = String.replicate repeat str;
printfn "powAlt 'hello ' 3 is %s" <| powAlt "hello " 3;

//2.3: isIthChar: string * int * char -> bool, where evaluates to true if 
// the i'th char of string is equal to the supplied char
let isIthChar ( tuple: string * int * char )  = 
    let str, i, char = tuple;
    str.[i] = char;

printfn "b is 2nd char of abc: %b" <| isIthChar ("abc", 1, 'b');
printfn "a is 3rd char of abc: %b" <| isIthChar ("abc", 2, 'a');

//2.4: eoccFromIth: string * int * char -> int
// Number of times the character appears in the string
//starting at the index supplied by the integer
let rec occFromIth (tuple : string * int * char ) = 
    let str, i , char = tuple;
    match i with 
    | n when n >= String.length str -> 
        0
    | _ -> 
        let toAdd = if(str.[i] = char) then 1 else 0;
        toAdd + occFromIth (str, i + 1, char);

printfn "Occurences of a from and including index 3 in aaaaabcfahefeaa should be 5 and is %i" 
    <| occFromIth ("aaaaabcfahefeaa", 3, 'a');

let rec occInString string char = occFromIth (string, 0, char);

printfn "Occurences of a in aaaaabcfahefeaa should be 8 and is %i" 
    <| occInString "aaaaabcfahefeaa" 'a';

let notDivisible (divisor, dividend) =
    not <| isDivisibleBy divisor dividend;

printfn "is 5 not divisible by 2? %b" <| notDivisible (2, 5);
printfn "is 9 not divisible by 3? %b" <| notDivisible (3, 9);

let rec test (a,b,c) =
    match a with
    | n when n > b -> true
    | n when notDivisible (n,c) -> test (n+1, b, c)
    | _ -> false;

let rec isPrime n =
    if n < 2 then false 
    else test (2, n-1, n);

let list1 = [1 .. 100];
let primes = List.filter isPrime list1;

printfn "Primes: %A" primes;

let rec nextPrime n = 
    if (isPrime <| n+1) then n+1
    else nextPrime <| n+1;


//let testNextPrime =
//    for i in primes do
//        printfn "The next prime after %i is %i" i  (nextPrime i);

//testNextPrime

//Week two exercise: Declare a function to generate the list of the prime numbers between m and n
let rec printPrimes intList = 
    match intList with
    | head :: tail -> 
        if (isPrime head) then head :: printPrimes tail     
        else printPrimes tail;
    | [] -> [];

let printPrimesBetween m n = 
    printPrimes [m .. n];

printfn "Printing using printPrimes: %A" <| printPrimesBetween 1 100;

//much easier solution though, is: 
let printPrimesBetweenEasy m n = [ m .. n ] |> List.filter isPrime;
printfn "Primes between 1 and 100 using the easy solution are: \n %A" <| printPrimesBetweenEasy 1 100;


//Generate a list of the first n prime numbers;
let rec printNPrimeNumbersFrom from n = 
    match n with 
    | 0 -> [];
    | _ -> 
        let next = nextPrime from;
        next :: printNPrimeNumbersFrom next (n-1);

let printFirstNPrimeNumbers n = 
    printNPrimeNumbersFrom 1 n;

printfn "The 25 first prime numbers are: \n %A" <| printFirstNPrimeNumbers 25;


// mrh 13/9 2012
// randomList n range generates a random list of length n containing integers between 0 and range.


let randomArray n range = let rand = let gen = new System.Random()
                                     (fun max -> gen.Next(max))
                          Array.init n (fun _ -> rand range);;
let randomList n range = List.ofArray(randomArray n range);;


let splitList (intList : int list) = 
    let length = intList.Length;
    let half = length / 2;
    let firstHalf = List.toSeq intList |> Seq.take half |> Seq.toList;
    let secondHalf = List.toSeq intList |> Seq.skip half |> Seq.take (length - half) |> Seq.toList;
    (firstHalf, secondHalf);


let rec merge list1 list2 = 
    match (list1, list2) with 
    | ([], _) -> list2
    | (_, []) -> list1
    | (h1::t1, h2::t2) -> 
        if(h1 < h2) then 
            h1 :: merge t1 list2
        else h2 :: merge list1 t2

let rec mergeSort list = 
    match list with
    | [_] -> list //If empty or of size one
    |  _  -> 
        let (left, right) = splitList list;
        merge <| mergeSort left <| mergeSort right;


let rec isSorted list = 
    let sortedList = List.sort list;
    list = sortedList;


printfn "Doing some tests now";
for i in [1..20] do
    let list = randomList 5000 10000;
    let sortedList = mergeSort list;
    if( not (isSorted sortedList)) then 
        printfn "Does not work for %A!!!!! Got %A" list sortedList;
    else ();