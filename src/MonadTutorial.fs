module MonadTutorial

// TODO: Move to fsx file somewhere...

// Investigate F# Libraries that does Typeclassopedia stuff
//  See overview here: https://github.com/fsprojects/FSharpPlus/issues/50#issuecomment-354663121
//                     https://github.com/fsprojects/FSharpPlus/issues/50#issuecomment-354675468
//    FSharPlus
//          https://github.com/fsprojects/FSharpPlus/issues/50#issuecomment-344546543
//    FSharpX (Extra)
//    ExtCore

// Ad C# Interop: It's really a nice to have, but there are many problems, some of them don't exists anymore with current state of things, but the main issue is that inlined stuff doesn't work from C#.

// See http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html
// See also http://blog.sigfpe.com/2006/06/monads-kleisli-arrows-comonads-and.html


// SRTP: We can simulate Type Classes this way and get 'Ad Hoc Polymorphism'

// This will not compile
//let inline mapIt f x = map f x

// F# Compiler deisgned this way to facilitate trait calls to math operators (BakedInTraitConstraintNames in ConstraintResolver.fs)
// This will compile, because of implicitly inferred trait constraint (a.k.a member constraint)
// (trait call to an operator ||> -- static member constraint)
let inline mapIt f x = f |>> x
// val inline mapIt :
//   f: ^a -> x: ^b ->  ^_arg3
//     when ( ^a or  ^b) : (static member ( |>> ) :  ^a *  ^b ->  ^_arg3)


//
// Side Effects: Debugging Pure Functions (Writer monad)
//
// NOTE: In Haskell the Writer monad is generalised to monoids instead of lists/strings etc...

let f (x: float) = 2. * x
let g (x: float) = x / 2.

let id = f << g

// Lets make some _pure_ traceable/debuggable functions (with no side-effects)

let f' (x: float) =
    let msg = sprintf "f'(%f) was called.\n" x
    (f x, msg)

let g' (x: float) =
    let msg = sprintf "g'(%f) was called.\n" x
    (g x, msg)

// Cumbersome (non idiomatic) way to compose f' . g'
let id' (x : float) =
    let gx, msg1 = g' x
    let res, msg2 = f' gx
    (res, msg1 + "\n" + msg2)

// We need some way to 'upgrade' (lift) the last function f' such that it takes
// input on the float * string. Therefore we wrap this in a new generic type
// and the lifting/transformer function must have the signature

let bind (f': float -> float * string) ((gx, gs): float * string) : float * string =
    let fx, fs = f' gx
    (fx, gs + fs)

// bind f' (gx,gs) = let (fx,fs) = f' gx in (fx,gs++fs)

// NOTE: bind f' has type (float * string -> float * string)
//       bind therefore lifts a monadic function to the elevated world

// F# pipeline style
let id'' (x : float) = g' x |> bind f'

// bind f' . g' (point free style)
let id''' = bind f' << g'

// Haskell operator
let (>>=) m f = bind f m
let (<<=) f m = bind f m

//let id''' (x : float) = x |> g' >>= f'
let id'''' (x : float) = g' x >>= f'

let id''''' x = f' <<= g' x

// Super Question: is there an 'identity' debuggable function?

// Kleisli operator
let (>=>) (m: float -> float * string) (n: float -> float * string) : float -> float * string =
    fun x -> let mx, sx = m x
             let nx, ns = n mx
             (nx, sx + ns)

// So we're looking for a debuggable function, call it unit,
// such that unit * f = f * unit = f.
//let unit = fun (x: float) -> (x, "")
let unit (x: float) = (x,"")

// laws
let g'' = g' >=> unit
let g''' = unit >=> g'

// The unit allows us to 'lift' any function into a debuggable one.
// In fact, define (a.k.a pure, return)
let lift f x = (f x,"")
let lift' f = unit << f

// Exercise
// Show that lift f * lift g = lift (f.g)

// The type 'm to append messages to is monoid
// Do we need input and output type?
type Writer<'a, 'm> = Writer of 'a * 'm

//------------------------------------------------------------------------------------------------------------

//
// A Container: Multivalued Functions (List monad)
//

// real part
// imaginary part
// ToString == 9+4i
type Complex< ^T when ^T : (static member Zero : ^T) > =
    | Complex of Real: ^T * Imaginary: ^T
        with
            static member inline Zero = Complex(LanguagePrimitives.GenericZero<'T>, LanguagePrimitives.GenericZero<'T>)
            static member inline (+) (Complex (r1, i1), Complex (r2, i2)) = Complex(r1 + r2, i1 + i2)
            static member inline (-) (Complex (r1, i1), Complex (r2, i2)) = Complex(r1 - r2, i1 - i2)
            static member inline (*) (Complex (r1, i1), Complex (r2, i2)) = Complex(r1 * r2 - i1 * i2, i1 * r2 + r1 * i2)
            //static member inline (/) (Complex (r1, i1), Complex (r2, i2)) =


// let div z1 z2 =
//     let Complex(Real=a; Imaginary=b) = z1
//     let Complex(Real=c; Imaginary=d) = z2
//     let real = (a * c + b * d) / (Math.Pow(c, 2.) + Math.Pow(d, 2.))
//     let imaginary = (b * c - a * d) / (Math.Pow(c, 2.) + Math.Pow(d, 2.))
//     Complex(Real=real, Imaginary=imaginary)

// let abs (Complex(Real=a; Imaginary=b)) =
//     Math.Sqrt(Math.Pow(a, 2.) + Math.Pow(b, 2.))

// let conjugate (Complex(Real=a; Imaginary=b)) =
//     Complex(Real=a, Imaginary=(-b))

open System

let sqrt x = Math.Sqrt x
let cbrt x = Math.Pow(x, 1.0 / 3.0)

// Fundamental theorem of algebra
//      * Every complex number, besides zero, has two square roots.
//      * Similarly, every non-zero complex number has three cube roots.

module Complex =
    /// Creates a complex number of a real part a and an imaginary part b
    let mkRect(real, imaginary) = Complex(real, imaginary)

    /// Creates a complex number from its polar form where a is the radius and b is the angle
    let mkPolar(radius, angle) = mkRect (radius * Math.Cos(angle), radius * Math.Sin(angle))

    let magnitude (Complex (r, i)) = sqrt(r * r + i * i)

    let phase (Complex (r, i)) = Math.Atan2(i , r)

    // De Moivre's formula: Convert your number into polar form
    //    r (cos(θ) + i sin(θ)),
    // and then you will get
    //    sqrt ( r (cos(θ)+isin(θ)) ) = ± sqrt(r) (cos(θ/2) + i sin(θ/2))
    let sqrt x = mkPolar (sqrt(magnitude x), phase(x) / 2.0)

// helper
let c real imaginary = Complex.mkRect(real, imaginary)

// We'll call these 'multivalued' functions.

let sqrt' (x : Complex<float>) : Complex<float> list =
    [Complex.sqrt x; Complex.sqrt x] // TODO: This is wrong result

let cbrt' (x : Complex<float>) : Complex<float> list =
    [Complex.sqrt x; Complex.sqrt x; Complex.sqrt x] // TODO: This is wrong result

// Suppose we want to find the sixth root of a real number. We can just concatenate the cube root
// and square root functions. In other words we can define
//
//  sixthroot :: Float -> [Complex Float]
//  sixthroot x = sqrt (cbrt x)

// What we need is a function, called bind say, to compose these functions, with declaration
//
// bind :: (Complex Double -> [Complex Double]) -> ([Complex Double] -> [Complex Double])
let bindList (f' : Complex<float> -> Complex<float> list) (xs : Complex<float> list) : Complex<float> list =
    List.collect f' xs // = List.concat <| List.map f' xs (cartesion cross product)


// Same procedure: Lift the monadic function that comes last in the pipeline
let sixthroot (x: Complex<float>) = bindList sqrt' (cbrt' x)

let unitList x = [x]


// Again, define
//      f >=> g = bind f . g
// NOTE: >=> defined above, we use >==> for list monad here (F# lacks polymorphic operators at the global level without complex SRTP inlining!!!)
let (>==>) f g = bindList f << g
// and
//      lift :: (a -> b) -> (a -> b list)
//      lift f = unit . f       (a.k.a return, pure)
let liftList f = unitList << f
// lift does exactly what you might expect. It turns an ordinary function into a multivalued one in the obvious way.

// Exercise ((return, >=>, monadic function) is a monoid)
// Show the Monad Laws (in the Kleisli Category, a socalled 'Kleisli arrow' is a monadic function)
//     (f >=> return) = (return >=> f) = f                  (return is identity)
// and
//      (return f) >=> (return g) = return (f.g)            (>=> is a monoid)


// What monads allow you to do is compose monad function.
// So given f :: a → m b and g :: b → m c there is a Kleisli arrow a → m c.

//------------------------------------------------------------------------------------------------------------

//
// A more complex side effect: Random Numbers (State Monad)
//
// To generate a random number we need a StdGen value
// And we get a new StgGen value as paret of a pair each time, that needs to be passed in on the next call...

// The Haskell random function looks like this
//    random :: StdGen -> (a, StdGen)

// type StdGen  -- a type for a "standard" random number generator.
//
// -- | Construct a generator from a given seed. Distinct arguments
// -- are likely to produce distinct generators.
// mkStdGen :: Int -> StdGen
//
// -- | Returns an Int that is uniformly distributed in a range of at least 30 bits.
// next :: StdGen -> (Int, StdGen)

// So a function that is conceptually a randomised function a -> b can be written as a function
//     a -> StdGen -> (b, StdGen)
// where StdGen is the type of the seed.