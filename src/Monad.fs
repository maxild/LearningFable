module Monad

//
// Monad example: One-Way Wrapper
//

// one-way wrapper (i.e. We can wrap things but we can't unwrap them)
// Idea: wrapped data is tainted data (forurenet data).
// Any time we try to do anything with tainted data the result is also tainted, exactly as we might expect.
type W<'T> =
    | W of 'T
// NOTE: We still want to be able to do whatever we like to the wrapped data,
// but anything that depends on anything wrapped should itself be wrapped.

//[<RequireQualifiedAccess>]]
module W =
    // both 'return' and 'pure' are reserved words in F#
    let return' x = W x
    // generally called map, (but called fmap in Haskell 98)
    let fmap f (W x) = W (f x)
    // join flattens a wrapper (container)
    let join (W (W x)) = W x
    // ('a -> W<'b>) -> (W<'a> -> W<'b>)
    let bind (f : 'a -> W<'b>) (W x) = f x
    // NOTE: bind is even simpler than fmap!!!
    // NOTE: (>>=) has params flipped, that is first monad instance, then monadic function

    // Notice how bind is more general than fmap. In fact, fmap f = bind (return . f).

    // What bind does is to take a container of type (m a) and a function of type (a -> m b).
    // It first maps the function over the container, (which would give an m (m b)) and then
    // applies join to the result to get a container of type (m b)
    let bind' (f : 'a -> W<'b>) wx = join (fmap f wx)

// In Haskell, it is more usual to use the operator >>= instead of bind where bind f x = x >>= f.
// (>>=) :: W 'a -> ('a -> W 'b) -> W 'b
let (>>=) x f = W.bind f x

// Kleisli operator
// composing two monadic functions to single composite monadic function
let (>=>) g h =
    g >> (W.bind h)
// NOTE: As fans of category theory will note, these laws say precisely that monadic functions
// of type a -> m b are the arrows of a category with (>=>) as composition!
// Indeed, this is known as the Kleisli category of the monad m.
// Alternative formulation
let (>==>) g h =
    fun x -> g x >>= h

// Recall that (>=>) “composes” two functions of type a -> m b and b -> m c. You can think of
// something of type a -> m b (roughly) as a function from a to b which may also have some sort
// of effect in the context corresponding to m. (>=>) lets us compose these “effectful functions”.

// The monad laws reformulated in terms of (>=>) are:
//     return >=> g  =  g
//     g >=> return  =  g
//     (g >=> h) >=> k  =  g >=> (h >=> k)
// That is also the monoid laws, and therefore monadic functions are a monoid ('id' = return, compose = '>=>')

// Usage
let a = W 3
let b = W.fmap ((+) 3) a

// This works, but there is something we can't do, and that is use monadic functions...
let monadicAdd x = W (x + 1)

// we need a higher-order function that can apply monadicAdd, ie. unwrap and apply function
// As long as this function always gives us back something that is wrapped, our end users will
// never be able to unwrap anything.
let liftedAdd = W.bind monadicAdd
let c = monadicAdd 3 |> (liftedAdd >> liftedAdd) |> liftedAdd

// Using return and bind we have achieved our goal of wrapping objects and freely manipulating
// wrapped objects while keeping them wrapped. What's more, we can chain functions that wrap
// without getting bogged down in multiple layers of wrapping. And that, really, sums up what
// a Monad is all about.

// -------------------------------
// IMPORTANT!!!!!!!!!!
// fmap lifts (normal) functions
// bind lifts monadic function
// -------------------------------
//
// Exercise 1
//

// define a function g : int -> W int -> W int so that g x (W y) = W (x+y).
let g (x: int) (y: W<int>) =
    y |> W.bind (W.return' << ((+) x))

let g' x y = y >>= (W.return' << ((+) x))
// g x y = y >>= (return . (+x))

//
// Exercise 2
//

// define a function h :: W Int -> W Int -> W Int so that h (W x) (W y) = W (x+y).
let h (x: W<int>) (y: W<int>) =
    x |> W.bind (fun x' -> y |> W.bind (fun y' -> W.return' (x' + y')))

// h x y = x >>= (\x -> g x y)
let h'' x y = x >>= (fun x -> g x y)

// computation expressions can make some kind of do-notation possible in F#???
let h' (x: W<int>) (y: W<int>) =
    x >>= (fun x' ->
    y >>= (fun y' -> W.return' (x' + y')) )

//
// Exercise 3
//
// Prove the three monad laws for W. This should be almost trivial.



//
// Exercise 4
//
// We can't completely unwrap things using the monad API. But we can unwrap one layer from things that
// are wrapped twice. So here's a nice puzzle:
//   define a function join :: W (W a) -> W a using the Monad API and no explicit unwrapping.

//
// Monad Example: Tree
//

type Tree<'T> =
    | Leaf of 'T
    | Branch of Tree<'T> * Tree<'T>

