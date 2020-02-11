module Functor

let add42 x = x + 42

// ugly way
let add42ToOpt opt =
    match opt with
    | Some x -> Some (add42 x)
    | None -> None


//
// Functor
//

// idiomatic way
let add42ToOpt2 = Option.map add42

// Option.map
let map g fa =
    match fa with
    | Some x -> Some (g x)
    | None -> None

// fmap infix operator for fmap (only on option...see FSharpPlus!!!)
let (<%>) = map

// Stay up in the world of option<int>...this is called applicative style programming
let x = add42 <%> Some 3

//
// Applicative
//

// TODO: What about 2,3,4,etc parameter functions...apply

let apply f ma =
    match f with
    | Some f -> match ma with
                | Some a -> Some (f a)
                | None -> None
    | None -> None

let (<*>) = apply

let add x y = x + y

let yApp = add <%> Some 5 <*> Some 7

//
// Monad
//

let bind (f: 'a -> 'b option) (ma: 'a option) =
    match ma with
    | Some x -> f x
    | None -> None

let return' x = Some x
//let (>>=) = bind
let (>>=) x f = bind f x

// We can see that (>>) is a specialized version of (>>=), with a default implementation given.
// The intention is that m >> n ignores the result of m, but not its effects.
//let (>>) m n = m >>= fun _ -> n

// using (>>=) from Haskell
let yMon =
    Some 5 >>= (fun x' ->
    Some 7 >>= (fun y' ->
    return' <| add x' y') )

// using bind
let yMon2 =
    Some 5 |> bind (fun x' ->
    Some 7 |> bind (fun y' ->
    return' <| add x' y') )

// BAD: Not readable!!!
let yMon3 =
    bind (fun x' ->
    bind (fun y' -> return' <| add x' y') (Some 7)
    ) (Some 5)

let add8 =
    // make composable functions
    let add5 = bind (return' << (add 5))
    let add3 = bind (return' << (add 3))
    add5 << add3 // compose

let z = Some 7 |> add8

// NOTE: A Monad is a monoid under bind composition
//
//    input >>= ... >>= ... >>= output
//
// How do you lift a function like add into the pipeline???
//
// A monad is a Monoid in the category of endofunctors!!!
// Monadic function: 'a -> option 'b
// Kleisli composition >=> for composing/chaining monadic functions
