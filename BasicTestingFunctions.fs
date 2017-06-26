//Some functions that are useful for writing quick and dirty tests 

open System;

type TestResult = 
    | Success
    | Failure of string * string

//If the test succeded, do nothing, else print error message to console
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

//Test examples
//TestAreEqual "Test 2+2 is 4" (2+2) 4;
//TestAreEqual "Test true is true" true true;
//TestIsTrue "True is true:" false;
//TestAreEqual "Tuple equal" (1,1,1) (2,2,2);