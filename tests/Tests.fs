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

let polymorphicFunctionTests =
    testList "Polymorphic Function Tests" [
        testList "Type Constraint Tests" [

            testCase "Enum values" <| fun () ->
                let values = getValues2<System.DayOfWeek, int>()
                //let values = getValues<System.DateTime>()
                let expectedValues =
                    [| System.DayOfWeek.Sunday;
                       System.DayOfWeek.Monday;
                       System.DayOfWeek.Tuesday;
                       System.DayOfWeek.Wednesday;
                       System.DayOfWeek.Thursday;
                       System.DayOfWeek.Friday;
                       System.DayOfWeek.Saturday; |]
                Expect.equal values expectedValues "wrong DayOfWeek values"
        ]

        testList "SRTP: Statically Resolved Type Paramaters" [

            testCase "implicit conversion operator" <| fun () ->
                let (!!) : string -> System.Xml.Linq.XNamespace = implicit
                let foo = !!"" + "foo"
                Expect.equal (foo.GetType().Name) "XName" "empty XNamespace and appending our 'foo' gives XName"

            testCase "op_Addition" <| fun () ->
                Expect.equal (add 1 2) 3 "add works"
                Expect.equal (add 1. 2.) 3. "add works"
                Expect.equal (add 1M 2M) 3M "add works"
        ]
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
        polymorphicFunctionTests
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