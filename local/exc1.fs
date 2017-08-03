
let hypothenuse catheti = 
    let cathetus1 = fst catheti;
    let cathetus2 = snd catheti;
    let hypothenuseSquared = 
        System.Math.Pow(cathetus1,2.0) + 
        System.Math.Pow(cathetus2, 2.0);

    System.Math.Sqrt(hypothenuseSquared);

let testCase1 = (3.0, 4.0);
printfn "Hypothenuse when catheti are 3 and 4 is %f" (hypothenuse testCase1);

let rec sum1ToN = function
    | 0 -> 0
    | n -> n + sum1ToN(n-1);

printfn "Sum of number from 1 through 10 is %i" (sum1ToN 10 );

let rec fib = function 
    | 0 | 1 -> 1
    | n -> fib(n-1) + fib(n-2);

printfn "The 10th fibonacci number is %i" (fib 10);


let a = 5;
let f a = a + 1;  //This should increment the input
let g b = (f b) + a; //This should add 6 to the input.a

printfn "f 20 is %i" (f 20);
printfn "g 0 is %i " (g 0);

//It does 