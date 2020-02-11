module Continuations

// NOTE: Continuation means 'what happens next' is controlled by caller (CSP style)
let inline divide ifSuccess ifDivideByZero x y =
    if x = LanguagePrimitives.GenericZero then
        ifDivideByZero()
    else
        ifSuccess (x / y)

let ifZero1() = printfn "Divide by zero"
let ifSuccess1 result = printfn "result = %A" result

// int -> int -> unit (side effects)
let divide1 = divide ifSuccess1 ifZero1

// NOTE: Unit values are defined
let good1 = divide1 3 6
let bad1 = divide1 3 0

let ifZero2() = None
let ifSuccess2 result = Some result

// int -> int -> option<int> (no side effects)
let divide2 = divide ifSuccess2 ifZero2

let good2 = divide2 3 6
let bad2 = divide2 3 0

let ifZero3() = failwith "Divide by zero"
let ifSuccess3 result = result

let divide3 = divide ifSuccess3 ifZero3
