module YourLibrary // top-level module: first line in file, no equal sign, no indentation below

#nowarn "77" // Member constraints with the name 'Exp' are given special status by the F# compiler as certain .NET types are implicitly augmented with this member. This may result in compilation failures if you attempt to invoke the member constraint from your own code.module

let randomFeature() = [1;2;3]

// The F# language supports quite a few ways that we can constrain generics:
//
// * Type constraint
// * Null constraint
// * Explicit member constraint (SRTP) - a.k.a. "method constraints"
// * Constructor constraint (when 'a : (new : unit -> 'a))
// * Reference type constraint (when 'a : not struct)
// * Enum type constraint ('a, b' when 'a :> Enum and 'a :> ValueType and 'a : enum<'b>)
// * Delegate constraint

// type constraint (is a ValueType and derives from System.Enum)
let getValues<'a when 'a : struct and 'a :> System.Enum>() =
  System.Enum.GetValues(typeof<'a>) :?> 'a array

// using F# enum constraint
let getValues2<'a,'b when 'a : struct and 'a : enum<'b> and 'a :> System.Enum>() =
  System.Enum.GetValues(typeof<'a>) :?> 'a array

//
// SRTP: Compile-Time duck typing (tedious to write)
//
//      A statically resolved type parameter (SRTP) is a type parameter that is replaced with
//      an actual type at compile time instead of at run time. They are preceded by a caret (^) symbol.
//
//      SRTP are primarily useful in conjunction with member constraints, which are constraints that allow
//      you to specify that a type argument must have a particular member or members in order to be used.
//
//      SRTP are always inlined by the compiler (code bloat)
//
//      SRTP cannot be used on types (no HKT in F#)
//

// Explicitly specify that my ^a type must support a static method called op_Implicit,
// simply known as the implicit operator.
let inline implicit< ^a, ^b when ^a : (static member op_Implicit : ^b ->  ^a)> arg =
    (^a : (static member op_Implicit : ^b -> ^a) arg)

// Use type inference, and avoid specifying type parameters in the signature
let inline implicit2 arg =
    (^a : (static member op_Implicit : ^b -> ^a) arg)

// Op<T> logic possible to implement with inline functions

// Simply write
let inline add x y = x + y
// =>
//    val inline add :
//      x: ^a -> y: ^b ->  ^c
//        when ( ^a or  ^b) : (static member ( + ) :  ^a *  ^b ->  ^c)
// this is not possible in C#!!!!
//    public static T Add<T>(T x, T y)
//    {
//        return x + y;
//    }

// The reason why the add functions below will trigger compiler warnings and is not
// recommended is that a lot of primitive types are implicitly augmented with this
// operator such as int, float, etc, so this would not work for those plus not work
// on string as well since dynamic invocation is not allowed on this particular instance.

// works for decimal
let inline stupid_add< ^a when ^a : (static member op_Addition : ^a * ^a ->  ^a)> x y =
    (^a : (static member op_Addition : ^a * ^a -> ^a) (x, y))

// =>
// warning FS0077: Member constraints with the name 'op_Addition' are given special status
// by the F# compiler as certain .NET types are implicitly augmented with this member. This
// may result in runtime failures if you attempt to invoke the member constraint from your own code.

// works for DateTime + TimeSpan
let inline stupid_add2 arg1 arg2 =
    ( ^a : (static member op_Addition : ^a * ^b -> ^a) (arg1, arg2))

// =>
// warning FS0077: Member constraints with the name 'op_Addition' are given special status
// by the F# compiler as certain .NET types are implicitly augmented with this member. This
// may result in runtime failures if you attempt to invoke the member constraint from your own code.

// TODO: Try convert this to using recursion and state monad
let inline average (xs:'a list) =
    let mutable amount = 0
    let mutable sum    = LanguagePrimitives.GenericZero<'a>
    for x in xs do
        sum    <- sum + x
        amount <- amount + 1
    LanguagePrimitives.DivideByInt sum amount
// =>
// val inline average :
//   xs: ^a list ->  ^a
//     when  ^a : (static member get_Zero : ->  ^a) and
//           ^a : (static member ( + ) :  ^a *  ^a ->  ^a) and
//           ^a : (static member DivideByInt :  ^a * int ->  ^a)

open Microsoft.FSharp.Core

// module BasicInlinedOperations =
//     let inline unboxPrim<'T>(x:obj) = (# "unbox.any !0" type ('T) x : 'T #)
//     let inline box     (x:'T) = (# "box !0" type ('T) x : obj #)

//[<CodeAnalysis.SuppressMessage("Microsoft.Performance","CA1812:AvoidUninstantiatedInternalClasses")>]
// type GenericZeroDynamicImplTable<'T>() =
//     static let result : 'T =
//         // The dynamic implementation
//         let aty = typeof<'T>
//         if   aty.Equals(typeof<sbyte>)      then unbox<'T> (box 0y)
//         elif aty.Equals(typeof<int16>)      then unbox<'T> (box 0s)
//         elif aty.Equals(typeof<int32>)      then unbox<'T> (box 0)
//         elif aty.Equals(typeof<int64>)      then unbox<'T> (box 0L)
//         elif aty.Equals(typeof<nativeint>)  then unbox<'T> (box 0n)
//         elif aty.Equals(typeof<byte>)       then unbox<'T> (box 0uy)
//         elif aty.Equals(typeof<uint16>)     then unbox<'T> (box 0us)
//         elif aty.Equals(typeof<uint32>)     then unbox<'T> (box 0u)
//         elif aty.Equals(typeof<uint64>)     then unbox<'T> (box 0UL)
//         elif aty.Equals(typeof<unativeint>) then unbox<'T> (box 0un)
//         elif aty.Equals(typeof<decimal>)    then unbox<'T> (box 0M)
//         elif aty.Equals(typeof<float>)      then unbox<'T> (box 0.0)
//         elif aty.Equals(typeof<float32>)    then unbox<'T> (box 0.0f)
//         else
//            let pinfo = aty.GetProperty("Zero")
//            unbox<'T> (pinfo.GetValue(null,null))
//     static member Result : 'T = result

// let GenericZeroDynamic< 'T >() : 'T = GenericZeroDynamicImplTable<'T>.Result

// TODO: This is not possible...what is the alternative
// let inline GenericZero< ^T when ^T : (static member Zero : ^T) > : ^T =
//     GenericZeroDynamic<(^T)>()
//     when ^T : int32       = 0
//     when ^T : float       = 0.0
//     when ^T : float32     = 0.0f
//     when ^T : int64       = 0L
//     when ^T : uint64      = 0UL
//     when ^T : uint32      = 0ul
//     when ^T : nativeint   = 0n
//     when ^T : unativeint  = 0un
//     when ^T : int16       = 0s
//     when ^T : uint16      = 0us
//     when ^T : sbyte       = 0y
//     when ^T : byte        = 0uy
//     when ^T : decimal     = 0M
//      // According to the somewhat subtle rules of static optimizations,
//      // this condition is used whenever ^T is resolved to a nominal type
//     when ^T : ^T = (^T : (static member Zero : ^T) ())

let disposeMany (xs : seq<#System.IDisposable>) =
    for x in xs do x.Dispose()

let disposeMany2 (xs : seq<'T :> System.IDisposable> ) =
    for x in xs do x.Dispose()

//
// Starting with F# 4.1, you can also specify concrete type names in statically resolved type parameter signatures.
//

// const from Haskell (const is a keyword in F#)
let inline konst x _ = x

// SRTP example for overloaded fmap
type Functor() =
    // Functor fmap from Haskell
    static member inline fmap (f: ^a -> ^b, a: ^a list) = List.map f a
    static member inline fmap (f: ^a -> ^b, a: ^a option) =
        match a with
        | None -> None
        | Some x -> Some (f x)

    // default implementation of replace (^a is a Functor)
    static member inline replace< ^a, ^b, ^c, ^d, ^e when ^a :> Functor and (^a or ^d): (static member fmap: (^b -> ^c) * ^d -> ^e) >
        (a, f) =
            ((^a or ^d) : (static member fmap : (^b -> ^c) * ^d -> ^e) (konst a, f))

    // call overridden replace if present
    static member inline replace< ^a, ^b, ^c when ^b: (static member replace: ^a * ^b -> ^c)>(a: ^a, f: ^b) =
        (^b : (static member replace: ^a * ^b -> ^c) (a, f))

let inline replace_instance< ^a, ^b, ^c, ^d when (^a or ^c): (static member replace: ^b * ^c -> ^d)> (a: ^b, f: ^c) =
        ((^a or ^c): (static member replace: ^b * ^c -> ^d) (a, f))

// Note the concrete type 'Functor' specified in the signature
let inline replace (a: ^a) (f: ^b): ^a0 when (Functor or  ^b): (static member replace: ^a *  ^b ->  ^a0) =
    replace_instance<Functor, _, _, _> (a, f)

open System.Xml.Linq

// XElement and XAttribute do not share a common ancestor which supports a Name and Value property
let inline getNameValue< ^a when ^a : (member get_Name  : unit -> XName ) and
                                 ^a : (member get_Value : unit -> string)>
    xItem =
        let name =  (^a : (member get_Name  : unit -> XName ) xItem)
        let value = (^a : (member get_Value : unit -> string) xItem)
        (name, value)

// Type Inference can add the compile time type constraints
let inline getNameValue2 xItem =
  let name =  (^a : (member get_Name  : unit -> XName ) xItem)
  let value = (^a : (member get_Value : unit -> string) xItem)
  name, value

type Tree<'T>  =
    | Node of 'T * Tree<'T>  * Tree<'T>
    | Leaf of 'T

let rec treeSize = function
    | Leaf _ -> 1
    | Node(_, l, r) -> 1 + treeSize l + treeSize r

type Vector3D = Vector3D of float * float * float

let length (Vector3D(dx, dy, dz)) = sqrt (dx * dx + dy * dy + dz * dz)

// parametric polymorphism!!!!! parametricity!!!!!

//let getFirst (a,b,c) = a

//let mapPair f g (x,y) = (f x, g y)

// Haskell has type classes
//   Eq a   : Equality comparison
//   Ord a  : Ordered comparison


// F# has generic operators (constraints)
//  when 'T : equality
//  when 'T : comparion
//      - generic comparison (aka structural comparison)
//          * compare
//          * hash
//          * (=)
//          * (<>)
//          * (<)
//          * (<=)
//          * (>)
//          * (>=)
//          * min
//          * max

// Euclids greatest common divisor (gcd)

// int * int -> int
let rec gcd a b =
    if a = 0 then b
    else gcd (b % a) a

let gcdGeneric zero modulus =
    let rec gcd a b =
        if a = zero then b
        else gcd (modulus b a) a
    gcd

let gcdInt = gcdGeneric 0 (%)
let gcdDec = gcdGeneric 0M (%)
let gcdFloat = gcdGeneric 0. (%)

// Alternative (less efficient) definition
let rec hcf a b =
    if a = 0 then b
    elif a < b then hcf a (b - a)
    else hcf (a - b) b

let hcfGeneric zero lessThan subtract =
    // when 'a : equality, because we use still use (=)
    let rec hcf a b =
        if a = zero then b
        elif lessThan a b then hcf a (subtract b a)
        else hcf (subtract a b) b
    hcf

//let hcfInt = hcfGeneric 0 (<) (-)
//let hcfDec = hcfGeneric 0M (<) (-)
let hcfFloat = hcfGeneric 0. (<) (-)

// This dictionary of operations is similar to VTables in OO languages, and
// the compiled form of Type Classes in Haskell!!!!!!!!!!!!!!
type Op<'T> =
    { Zero: 'T
      LessThan: 'T -> 'T -> bool
      Subtract: 'T -> 'T -> 'T }

let intOps = { Zero = 0; LessThan = (<); Subtract = (-)}

// For larger frameworks (libs) object interface types are often used instead of record
type IOp<'T> =
    abstract Zero : 'T
    abstract LessThan: 'T * 'T -> bool
    abstract Subtract: 'T * 'T -> 'T

let decimalOps =
    { new IOp<decimal> with
        member __.Zero = 0M
        member __.LessThan(x, y) = x < y
        member __.Subtract(x, y) = x - y}

let hcfGeneric2 (op : Op<'T>) =
    // when 'a : equality, because we use still use (=)
    let rec hcf a b =
        if a = op.Zero then b
        elif op.LessThan a b then hcf a (op.Subtract b a)
        else hcf (op.Subtract a  b) b
    hcf

let hcfInt = hcfGeneric2 intOps

let hcfGeneric3 (op : IOp<'T>) =
    // when 'a : equality, because we use still use (=)
    let rec hcf a b =
        if a = op.Zero then b
        elif op.LessThan(a, b) then hcf a (op.Subtract(b, a))
        else hcf (op.Subtract(a, b)) b
    hcf

let hcfDecimal = hcfGeneric3 decimalOps