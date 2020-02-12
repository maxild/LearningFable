module FSharpPlusSnippet

//
// FSharpPlus technique/hacks to simulate type classes
//

// Notes
// 1. '``Functor<'T>`` is a single type parameter with a long name that documents the HKT in Haskell
// 2. What does 'inherit Default1' accomlish? (I removed it here)
// type Map =
//     inherit Default1
type Map =
    static member inline Invoke (mapping: 'T->'U) (source: '``Functor<'T>``) : '``Functor<'U>`` =
        let inline call (mthd: ^M, source: ^I, _output: ^R) = ((^M or ^I or ^R) : (static member Map : (_*_)*_ -> _) (source, mapping), mthd)
        call (Unchecked.defaultof<Map>, source, Unchecked.defaultof<'``Functor<'U>``>)

    static member Map ((x: option<_>           , f: 'T->'U), _mthd: Map) = Option.map  f x
    static member Map ((x: list<_>             , f: 'T->'U), _mthd: Map) = List.map    f x : list<'U>
    static member Map ((g: 'R->'T              , f: 'T->'U), _mthd: Map) = (>>) g f

// Ad hoc polymorphism (generic map)
let inline map f x = Map.Invoke f x
// val inline map :
//   f:('a -> 'b) -> x: ^c ->  ^d
//     when (Map or  ^c or  ^d) : (static member Map :  ^c * ('a -> 'b) * Map -> ^d)

// test
let listOfStrings = map string [1..3]

let foo = map ((+)3) ((+)2)
let fooResult = foo 10

// Compile error when functions have not been defined ad functor instances
//
// Type constraint mismatch when applying the default type 'obj' for a type inference variable.
// No overloads match for method 'Map'. The available overloads are shown below. Consider adding further type constraints
// Possible overload: 'static member Map.Map : ('T list * ('T -> 'U)) * _mthd:Map -> 'U list'. Type constraint mismatch. The type
//     '(int -> int) * (int -> int)'
// is not compatible with type
//     ''a list * ('a -> 'b)'
// .
// Possible overload: 'static member Map.Map : ('T option * ('T -> 'U)) * _mthd:Map -> 'U option'. Type constraint mismatch. The type
//     '(int -> int) * (int -> int)'
// is not compatible with type
//     ''a option * ('a -> 'b)'