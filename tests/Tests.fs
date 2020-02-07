module Tests

// Use FABLE_COMPILER directive to between .NET Core and Fable
#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open YourLibrary

let arithmeticTests =
    testList "Arithmetic tests" [

        testCase "randomFeature works" <| fun () ->
            let result = randomFeature()
            Expect.equal result [1..3] "range"

        testCase "plus works" <| fun () ->
            Expect.equal (1 + 1) 2 "plus"

        testCase "Test for falsehood" <| fun () ->
            Expect.isFalse (1 = 2) "false"

        testCaseAsync "Test async code" <|
            async {
                let! x = async { return 21 }
                let answer = x * 2
                Expect.equal 42 answer "async"
            }

        // pending tests are ignored
        ptestCase "Test ACTUAL before EXPECTED" <| fun () ->
            Expect.equal "ACTUALVALUE" "EXPECTEDVALUE" ""
    ]

//Mocha.runTests arithmeticTests |> ignore

[<EntryPoint>]
let main args =
#if FABLE_COMPILER
    // Use Mocha (NodeJS or Browser)
    Mocha.runTests arithmeticTests
#else
    // Use the Expecto test runner (.NET Core via `dotnet run`)
    runTestsWithArgs defaultConfig args arithmeticTests
#endif