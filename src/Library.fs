module YourLibrary

let randomFeature() = [1;2;3]

// type Tree<'T>  =
//     | Node of 'T * Tree<'T>  * Tree<'T>
//     | Leaf of 'T

// 'T Tree == Tree<'T>, I prefer the latter!
type 'T Tree  =
    | Node of 'T * 'T Tree  * 'T Tree
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