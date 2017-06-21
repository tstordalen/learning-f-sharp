//A free standing program contains a main-function with 
//the type string[] -> int.   string[] is array of strings, 
//accessed by ar.[0], ar.[1], etc.
//The main function has to be preceded by the attribute [<EntryPoint>]

[<EntryPoint>]
let main (param: string[]) =
    printfn "Hello, %s" (param.[0]);
    0;