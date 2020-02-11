namespace YourLibrary.Collections

open System.Diagnostics
open System.Diagnostics.CodeAnalysis

// ==============================================
// Types
// ==============================================

[<DebuggerDisplay("{DebugDisplay,nq}")>]
type Stack<'T> =
    | StackContents of 'T list

// ==============================================
// Stack primitives
// ==============================================

module Stack =

    let Empty = StackContents []

    let push x (StackContents xs) =
        StackContents (x::xs)

    let pop (StackContents xs) =
        match xs with
        | (h :: t) ->
            let newStatck = StackContents t
            (h, newStatck)
        | [] -> failwith "The stack is empty"

    /// <summary>Duplicate top element on the stack.</summary>
    let dup (stack : Stack<'T>) =
        let x, _ = pop stack
        push x stack

    let swap (stack: Stack<'T>) =
        let x, s = pop stack
        let y, s2 = pop s
        s2 |> push x |> push y

    let private binaryOp op (stack : Stack<'T>) =
        let x, s1 = pop stack
        let y, s2 = pop s1
        let result = op x y
        push result s2

    let inline add stack = binaryOp (+) stack

    let inline sub stack = binaryOp (-) stack

    let inline mul stack = binaryOp (*) stack

    let inline div stack = binaryOp (/) stack

    let private unaryOp op (stack : Stack<'T>) =
        let x, s = pop stack
        push (op x) s

    let inline neg stack = unaryOp (~-) stack

    let inline square stack = unaryOp (fun x -> x * x) stack

// ==============================================
// Fluent operations (for float)
// ==============================================

[<AutoOpen>]
module FloatHelpers =
    let EMPTY = Stack.Empty
    let ZERO = Stack.push 0.
    let ONE = Stack.push 1.
    let TWO = Stack.push 2.
    let THREE = Stack.push 3.

    let ADD (s: Stack<float>) = Stack.add s
    let MUL (s: Stack<float>) = Stack.mul s

    // All functions have same input and output types
    // This is called an Endomorphism
    // Function Composition on Endomorphims is a Monoid!!!!
    // We can compose easily because we have a Monoid!!!

    [<SuppressMessage("*", "PublicValuesNames")>]
    let ONE_TWO_ADD = ONE >> TWO >> ADD

    // Side effect here
    let SHOW stack =
        let x,_ = Stack.pop stack
        printfn "The answer is %f" x
        stack  // keep going with same/unchanged stack

// Example
// EMPTY |> ONE |> THREE |> ADD |> TWO |> MUL |> SHOW // (1+3)*2 = 8

// ==============================================
// Stack Extensions
// ==============================================

type Stack<'T> with

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
        member s.DebugDisplay =
            let (StackContents l) = s
            let n = l.Length
            let txt =
                if n > 1000 then "Length > 1000"
                else System.String.Concat( [| "Length = "; n.ToString() |])
            txt
