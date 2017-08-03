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

//Test examples
//TestAreEqual "Test 2+2 is 4" (2+2) 4;
//TestAreEqual "Test true is true" true true;
//TestIsTrue "True is true:" false;
//TestAreEqual "Tuple equal" (1,1,1) (2,2,2);

// 3.1 
let MinutesSinceMidnight time = 
    let (hours, minutes, amOrPm) = time;
    let minutesDueToPm = 
        match amOrPm with
        | "PM" -> 12*60;
        | _ -> 0;
    
    minutesDueToPm + hours * 60 + minutes;

let IsBefore time1 time2  =
    MinutesSinceMidnight time1 < MinutesSinceMidnight time2;

let (.<) time1 time2 = 
    MinutesSinceMidnight time1 < MinutesSinceMidnight time2;

let time1 = (10, 30, "AM");
let time2 = (10, 30, "PM");
let time3 = (1, 15, "PM")

//TODO CHANGE THIS BACK TO TRUE
TestIsTrue "10:30 AM is before 10:30 PM" (time1 .< time2);
TestIsFalse "10:30 PM is before 10:30 AM" (time2 .< time1);

TestIsTrue "10:30 AM is before 1:15 PM" (time1 .< time3);
TestIsFalse "1:15 PM is before 10:30 AM" (time3 .< time1);

TestIsTrue "1:15 PM is before 10:30 PM" (time3 .< time2);
TestIsFalse "10:30 PM is before 1.15 PM" (time2 .< time3);


//This is not necesarily a good idea. Comparison is implemented for tuples, if it is implemented for its components
//Since all AM times are before all PM times, we can just switch the representation in the tuple to get 
//free comparison

let tt1 = ("AM", 10, 30);
let tt2 = ("PM", 10, 30);
let tt3 = ("PM", 1, 15);

TestIsTrue "10:30 AM is before 10:30 PM" (tt1 < tt2);
TestIsFalse "10:30 PM is before 10:30 AM" (tt2 < tt1);

TestIsTrue "10:30 AM is before 1:15 PM" (tt1 < tt3);
TestIsFalse "1:15 PM is before 10:30 AM" (tt3 < tt1);

TestIsTrue "1:15 PM is before 10:30 PM" (tt3 < tt2);
TestIsFalse "10:30 PM is before 1.15 PM" (tt2 < tt3);


type TimeOfDay = {period : string; hour : int; minutes : int}
let t1 = {period = "AM"; hour = 10; minutes = 30};
let t2 = {period = "PM"; hour = 10; minutes = 30};
let t3 = {period = "PM"; hour = 1; minutes = 15};

TestIsTrue "10:30 AM is before 10:30 PM" (t1 < t2);
TestIsFalse "10:30 PM is before 10:30 AM" (t2 < t1);

TestIsTrue "10:30 AM is before 1:15 PM" (t1 < t3);
TestIsFalse "1:15 PM is before 10:30 AM" (t3 < t1);

TestIsTrue "1:15 PM is before 10:30 PM" (t3 < t2);
TestIsFalse "10:30 PM is before 1.15 PM" (t2 < t3);


//3.2 currency addition and subraction. 12 pence to a shilling, 20 shillings to a pound.
type Amount = {pounds : int; shillings : int; pence : int}
let amount pounds shillings pence = {pounds=pounds; shillings=shillings; pence=pence};

let QuotientAndRemaiderFromDivision dividend divisor = (dividend / divisor, dividend % divisor);

let normalizeAmount currency =  
    let {pounds = pounds; shillings = shillings; pence = pence} = currency; 
    let extraShillings, newPennies = QuotientAndRemaiderFromDivision pence 12;
    let extraPounds, newShillings  = QuotientAndRemaiderFromDivision (shillings + extraShillings) 20;
    let newPounds = pounds + extraPounds;
    {pounds = newPounds; shillings = newShillings; pence = newPennies};
    

TestAreEqual "Pennies convert correctly to shillings"
    (amount 0 10 0)  (normalizeAmount <| amount 0 0 120);

TestAreEqual "Partial convertion from shillings with leftover pennies" 
    (amount 0 1 5) (normalizeAmount <| amount 0 0 17);

TestAreEqual "Pounds are not converted to anything" 
    (amount 150 0 0 ) (normalizeAmount <| amount 150 0 0);

TestAreEqual "Pennies will be converted to pounds if enough pennies"
    (amount 1 0 0) (normalizeAmount <| amount 0 0 240);

TestAreEqual "Partial conversion to Pounds and Shillings with leftover pennies" 
    (amount 1 2 5) (normalizeAmount <| amount 0 0 269);

TestAreEqual "Convert pennies and shillings to pounds, shillings and pennies" 
    (amount 2 3 3)  (normalizeAmount <| amount 0 42 15);

TestAreEqual "Convert shillings to pounds"
    (amount 3 5 0) (normalizeAmount <| amount 0 65 0);

TestAreEqual "Pennies not converted if not enough to get to shillings"
    (amount 0 0 11) (normalizeAmount <| amount 0 0 11);

TestAreEqual "Shillings not converted if not enough to make a pound"
    (amount 0 2 0) (normalizeAmount <| amount 0 2 0);


let (.+.) left right =
    normalizeAmount {pounds=left.pounds + right.pounds; 
                     shillings = left.shillings + right.shillings; 
                     pence = left.pence + right.pence};

TestAreEqual "Add pennies that convert to shillings and pennies"
    (amount 0 1 1) 
    ( (amount 0 0 6) .+. (amount 0 0 7) );

TestAreEqual "Add pennies that converts to pennies"
    (amount 0 0 3)
    ((amount 0 0 2) .+. (amount 0 0 1));

TestAreEqual "Add pennies and shillings that converts to pounds, shillings and pennies"
    (amount 1 2 3)
    ((amount 0 18 2 ) .+. (amount 0 1 37));