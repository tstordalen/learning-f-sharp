let IsDivisibleBy dividend divisor = dividend % divisor = 0;

let IsPrime n = 
    let IsNDivisibleBy = IsDivisibleBy n;
    let numbers = [ 2 .. n-1];
    let numbersNIsDivisibleBy = [2..n-1] |> List.filter IsNDivisibleBy;
    (List.length numbersNIsDivisibleBy) = 0;
    










let printList = [2 .. 100]; 

let print n = (sprintf "%i is prime: %b") n (IsPrime n);

let toPrint = List.filter IsPrime printList;

printfn "%A" toPrint;