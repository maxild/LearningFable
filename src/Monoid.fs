module TypeClasses

// these are inferred to 'a option and 'a list (not _a option and _a list, because of 'Value Restriction')
let emptyOption = None
let emptyList = []

// Value Restriction problems here, because result of empty needs type annotation
type Empty() =
    static member (?) (_:Empty, m:'a option) = None
    static member (?) (_:Empty, m:'a list) = List.Empty


let inline empty m = (?) (Empty()) m

// > let emptyOption : int option  = empty (Some 3);;
// val emptyOption : int option = None

// > let emptyOption = empty (Some 3);;
// error FS0030: Value restriction.

// See also http://www.fssnip.net/9B
// 'dictionary of operations' through overloaded static member functions (can only overload a single function...Monoid has two)
type Fmap() =
  static member (?) (_:Fmap , m:_ option) = fun f -> Option.map f m
  static member (?) (_:Fmap , m:_ list)   = fun f -> List.map f m

// <$> is not permitted in F#, we use <%> instead
// The type signature is weird!!!!
let inline fmap f m = f |> Fmap() ? (m) // =>
let inline (<%>) f m = fmap f m // =>
// val inline ( <%> ) :
//   f:'a -> x: ^b -> 'c
//     when (Fmap or  ^b) : (static member ( ? ) : Fmap *  ^b -> 'a -> 'c)

// > sin <%> [0.1..0.2..4.];;
//      [0.09983341665; 0.2955202067; 0.4794255386; 0.6442176872; 0.7833269096;
//       0.8912073601; 0.9635581854; 0.9974949866; 0.9916648105; 0.9463000877;
//       0.8632093666; 0.7457052122; 0.5984721441; 0.4273798802; 0.2392493292;
//       0.04158066243; -0.1577456941; -0.3507832277; -0.5298361409; -0.6877661592]

type Apply() =
    static member (?) (_:Apply , mf:_ option) = fun (m:_ option) ->
        match mf , m with
        | Some f , Some x -> Some (f x)
        | _ -> None
    static member (?) (_:Apply , mf:_ list) = fun (m:_ list) ->
        [ for f in mf do for x in m -> f x] // cartesian product (ziplist is another possibility)

let inline (<*>) mf m = Apply() ? (mf) m

// > (+) <%> ["a";"b"] <*> ["x";"y";"z"]  |> printfn "%A";;
//      ["ax"; "ay"; "az"; "bx"; "by"; "bz"]

// > (fun a b c -> a + b + c) <%> [100;200] <*> [10;20] <*> [1;2] |> printfn "%A";;
//      [111; 112; 121; 122; 211; 212; 221; 222]

// > printf "%s, %s" <%> Some "hello" <*> Some "applicative" |> printfn "%A"
//      hello, applicative

// Faking type classes in F#

// TODO: Make Semigroup and Monoid, where Monoid is-a Semigroup (use interfaces for 'dictionary')

// type class
type Monoid<'a> =
    { empty: 'a
      append: 'a -> 'a -> 'a }

// Lets define 'dictionary of operations' (a.k.a. instances) for different combinations of ('a, empty, append)

let listMonoid =
    { empty = []
      append = List.append }

let stringMonoid =
    { empty = ""
      append = (+) }

let productIntMonoid =
    { empty = 1
      append = (*) }

let sumIntMonoid =
    { empty = 0
      append = (+) }

let productRealMonoid =
    { empty = 1.
      append = (*) }

let sumRealMonoid =
    { empty = 0.
      append = (+) }

// all
let andMonoid =
    { empty = true
      append = (&&) }

// any
let orMonoid =
    { empty = false
      append = (||) }

// Generic function using Monoid abstraction
let appendIfGeneric (m: Monoid<'a>) =
    let appendIf condition (x: 'a) (y: 'a) =
        if condition
        then m.append x y
        else m.empty
    appendIf

// TODO: We need MyLanguagePrimitives.GenericMonoid<'a> to resolve instances...what about more than one monoid per type??? wrap the type

// ^m : Monoid<^a>
// we cannot resolve the type into monoid at compile-time, and therefore we need to pass it explicitly at the call site...BAD!!!
let sum = appendIfGeneric sumRealMonoid true 4. 6.

// This cannot be done because Monoid has kind :: * -> * and
// empty/append cannot be statically resolved for each concrete Monoid
//    let inline appendIf x y =
//       appendIfGeneric ....

// we have to explicitly pass the Monoid instance (type class instance) to resolve the
// polymorphic overload of empty and append (very much ad hoc faking of type classes)
//let sumOfEvenNumbers = [1..6] |> List.fold (fun acc x -> appendIf addIntMonoid (x % 2 = 0) acc x) addIntMonoid.empty
