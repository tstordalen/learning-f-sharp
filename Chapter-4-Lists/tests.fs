let list1 = [ 1 .. 10];
printfn "%A" list1;

let list2 = [1 .. 2 .. 10];
printfn "%A" list2;

let list3 = [1.1 .. 2.2 .. 10.0];
printfn "%A" list3;

let emptyList = [];

// O(n) reversing 
let rec reverseHelper yetToReverse alreadyReversed = 
    match yetToReverse with
    | [] -> alreadyReversed
    | x::xs -> reverseHelper xs (x::alreadyReversed);
let reverse list = reverseHelper list [];