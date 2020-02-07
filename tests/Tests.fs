module Tests

// Use FABLE_COMPILER directive to between .NET Core and Fable
#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open YourLibrary

let treeTests =
    testList "Tree tests" [

        testCase "size works" <| fun () ->
            let sut = Node (2, Leaf 3, Leaf 8)
            let size = treeSize sut
            Expect.equal 3 size "wrong size"
    ]

let arithmeticTests =
    testList "Arithmetic tests" [

        testCase "Euclids GCD works" <| fun () ->
            Expect.equal (gcd 12 27) 3 "gcd of 12 and 27 is 3"
            Expect.equal (gcdInt 12 27) 3 "gcd of 12 and 27 is 3"
            Expect.equal (hcf 12 27) 3 "gcd of 12 and 27 is 3"
            Expect.equal (hcfInt 12 27) 3 "gcd of 12 and 27 is 3"
            Expect.equal (hcfFloat 12. 27.) 3. "gcd of 12 and 27 is 3"
            Expect.equal (hcfDecimal 12M 27M) 3M "gcd of 12 and 27 is 3"

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


let allTests =
    testList "all-tests" [
        arithmeticTests
        treeTests
    ]

//Mocha.runTests arithmeticTests |> ignore

[<EntryPoint>]
let main args =
#if FABLE_COMPILER
    // Use Mocha (NodeJS or Browser)
    Mocha.runTests allTests
#else
    // Use the Expecto test runner (.NET Core via `dotnet run`)
    runTestsWithArgs defaultConfig args allTests
#endif