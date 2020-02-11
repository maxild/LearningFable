module Reader
// There is some environment that is passed around as an extra parameter to all your functions
// How can we compose functions when the extra environment parameter keeps getting in the way?

// You might think of it like this: I have an input right now, but I won’t to inject an actual environment
// value later, so let me use the input to create an environment-consuming function that I can glue together
// in various ways right now, without needing an environment value at all.

// In Haskell:
//      type Reader e = (->) e
// In Haskell (->) has kind * -> * -> *, and therefore Reader e has kind * -> *.

// BUT F# does not have HKT. But we can construct a type for any function that takes a parameterized (generic)
// environment as input, and produces any other type.


// Reader :: ('env -> 'result) -> Reader<'env,'result> "wraps" a partial application,
// where only environment input is missing/delayed

// single case union
type ReaderM<'e,'a> =
    | ReaderM of ('e -> 'a)

// type alias
type Reader<'e,'a> = 'e -> 'a

// Functor

/// ('a -> 'b) -> ReaderM<'e,'a> -> ReaderM<'e,'b>
let mapM f (ReaderM g) = ReaderM (f << g)
// (a -> b) -> (e -> a) -> (e -> b)
// fmap f xs = f . xs

/// ('a -> 'b) -> ('e -> 'a) -> ('e -> 'b)
let map (f : 'a -> 'b)
        (g : Reader<'e, 'a>)
        : Reader<'e,'b> =
    f << g

// <$> is Haskell, but $ ise reserved in $
let (<%>) = map

// Applicative

// The return/pure does not read any environment state
// return/pure are reserved words in F#
// const/konst in Haskell
/// 'a -> Reader<'env,'a> implemented as const/K combinator
let pureM x = ReaderM (fun _ -> x)

let pure' (x: 'c) : Reader<_,'c> =
    fun _ -> x

/// ('env -> 'env -> 'a) -> ('env -> 'a) flatten the wrapper/container
//let join (xss : ) = (fun env -> xss env env)

/// ReaderM<'e, ('a -> 'b)> -> ReaderM<'e,'a> -> ReaderM<'e,'b>
let applyM (ReaderM f) (ReaderM x) =
    ReaderM (fun env -> (f env) (x env))

/// ('e -> 'a -> 'b ) -> ('e -> 'a) -> ('e -> 'b)
let apply (f : Reader<'e, 'a -> 'b>)
          (g : Reader<'e, 'a>)
          : Reader<'e, 'b> =
    fun env ->
        let f' = f env
        let g'  = g env
        f' g'

let (<*>) = apply

// (a -> b) -> (R a -> R b)
//
// lift1 ==

// (a -> b -> c) -> (R a -> R b -> R c)
// lift2 ==
// lift3 ==

// A functor can only lift functions of exactly one variable, but we want to lift other functions, therefore we define Applicative

// Applicative lifting


// Applicative (Monad) Lifting function from "normal" to "effects" world:
//
// lift0 :: a -> m a                (pure a.k.a return)
//
// lift1 :: (a -> b) -> m a -> m b  (map a.k.a fmap)
//
// lift2 :: (a1 -> a2 -> r) -> m a1 -> m a2 -> m r   (we need 'apply' here)
//

// Helpers making environment the last formal parameter

/// ('a1 -> 'e -> 'r) -> ('e -> 'a1 -> 'r)
let flip1 (f : 'a1 -> 'e -> 'r)
          : 'e -> Reader<'a1, 'r> =
    fun a dep -> f dep a

/// ('a1 -> 'a2 -> 'e -> 'r) -> ('e -> 'a1 -> 'a2 -> 'r)
let flip2 (f : 'a1 -> 'a2 -> 'e -> 'r)
          : 'a2 -> 'e -> Reader<'a1, 'r> =
    fun a b dep -> f dep a b

/// ('a1 -> 'a2 -> 'a3 -> 'e -> 'r) -> ('e -> 'a1 -> 'a2 -> 'a3 -> 'r)
let flip3 (f : 'a1 -> 'a2 -> 'a3 -> 'e -> 'r)
          : 'a2 -> 'a3 -> 'e -> Reader<'a1, 'r> =
    fun a b c dep -> f dep a b c


// Monad

let returnM = pureM
let return' = pure'

/// ('a -> Reader<'env,'b>) -> Reader<'env,'a> -> Reader<'env,'b>
let bindM f (ReaderM g) =
    ReaderM (fun env -> env |> f (g env))

/// Note: not >>=...params changed
/// ('a -> ('e -> 'b)) -> ('e -> 'a) -> ('e -> 'b)
let bind (f : 'a -> Reader<'d,'b>)
         (g : Reader<'d, 'a>)
         : Reader<'d, 'b> =
    fun env ->
        let f' = (f << g) env
        f' env

// ('a -> 'b) -> ('e -> 'a) -> ('e -> 'b)
// map == ReaderM (f << g)

/// ('e -> 'a) -> ('a -> ('e -> 'b)) -> ('e -> 'b)
let (>>=) f g = bind g f

// (e -> a) -> (a -> (e -> b)) -> (e -> b)
// Convention bind anf (>>=) have params flipped????!!!!????
//let (>>=) = bind

// Computation Expression

type ReaderBuilder() =
  member x.Return(v) = Reader.return' v
  member x.Bind(v, f) = Reader.bind f v
//   member x.Delay(f) = Reader (fun s ->
//     let (Reader h) = f ()
//     h s )

let reader = ReaderBuilder()


// This is our environment type
type Dependency =
    { Username: string
      Password: string }

module Example =

    type Env = string

    let core (name : Env) =
        " I wish my name was " + name

    let business (name : Env) =
        (core name) + "!" // this layer doesn't use/read "the shared state" but need to pass along to core layer

    let validation (name : Env) =
        "Welcome" + name + "!"

    let client (name : Env) =
        validation name + business name

    // The Reader monad is useful when you have a long chain of function invocations
    // where only some of the functions in the chain make direct use of a piece of
    // “shared environmental state”.

    type Html = string
    type Email = string

    // intercalate xs xss is equivalent to (concat (intersperse xs xss)).
    // It inserts the list xs in between the lists in xss and concatenates the result.
    //  >>> intercalate ", " ["Lorem"; "ipsum"; "dolor"]
    //     "Lorem, ipsum, dolor"
    let intercalate (sep: string) (xs : Html list) =
        List.reduce (fun acc s -> acc + sep + s) xs

    // combine :: [Html] -> Html
    let combine = intercalate ""

    let p (children : Html list) : Html =
      "<p>" + combine children + "</p>"

    let div (children: Html list) : Html =
        "<div>" + combine children + "</div>"

    let h1 (children: Html list) : Html =
      "<h1>" + combine children + "</h1>"

    // This is the page layout (for our Elm style view)
    //  view
    //      page
    //      topnav
    //      content (needs email)
    //          left
    //          right
    //              article
    //                  widget (needs email)

    let widget (email: Email) =
        div
            [ p [ "Hey " + email + ", we've got a great offer for you!" ]
            ]

    let article (email: Email) =
        div
            [ p [ "this is an article" ]
            ; widget email
            ]

    let left =
        div
            [ p [ "this is the left side" ]
            ]

    let right (email: Email) =
        div
            [ article email
            ]

    let content (email : Email) =
        div
            [ h1 [ "Custom Content for " + email ]
            ; left
            ; right email
            ]

    let topNav =
        div
            [ h1 [ "OurSite.com" ]
            ]

    let page (email : Email) =
        div
            [ topNav
            ; content email
            ]

    let view email =
        div
            [ page email
            ]

    // The only view functions that actually need the Email directly are content and widget!
    // All the others just pass it along faithfully without using it.
    // Wouldn’t it be nice to avoid all the take-and-pass boilerplate? Enter the Reader monad.

    // newtype Reader e a = Reader { runReader :: e -> a }
    // Problem: (Reader a) er en functor, applivative og monad!!!! DVS 'e -> ?', hvor ? er udfyldt!!!!

    type ReaderM<'e,'a> =
        | ReaderM of ('e -> 'a)

    // No HKT...we have to make a concrete reader
    //type EmailReader<'a> = ReaderM<Email, 'a> // THIS type is a Functor etc...

    // newtype ReaderM e a = ReaderM { runReader :: e -> a }
    let runReader (ReaderM g) = g

    // instance Functor (ReaderM a) where
    let fmap f (ReaderM g) = ReaderM <| (f << g)
    let (<%>) = fmap

    //instance Applicative (ReaderM a) where
    let pure' x = ReaderM <| fun _ -> x

    // let applyM (ReaderM f) (ReaderM x) =
    //     ReaderM (fun env -> (f env) (x env))

    let (<*>) m n = ReaderM <| fun e -> (runReader m e) (runReader n e)

    //instance Monad (Reader a) where
    let return' = pure'

    // ('a -> Reader<'e, 'b>) -> Reader<'e, 'a> -> Reader<'e, 'b>
    let bind (f: 'a->_ ) (ReaderM m) =
        ReaderM (fun env -> runReader (f (m env)) env)   : ReaderM<'e,'b>

    let (>>=) (m: ReaderM<'e, 'a>) (f : ('a -> ReaderM<'e, 'b>)) =
        bind f m

    // Just pass on the environment
    // ask :: Reader a a
    /// Retrieves the monad environment.
    let ask = ReaderM id

    // asks :: (e -> a) -> Reader e a
    let asks = ReaderM

    // See also https://engineering.dollarshaveclub.com/the-reader-monad-example-motivation-542c54ccfaa8

    // If we take the Reader monad as granted we can update the type signatures of many of our view
    // functions from Email -> Html to Reader Email Html.
    // This is to say, we have a Reader whose “shared environmental state” is of type Email and when
    // “run” with such a value, it will return a value of type Html.

    // Therfore we transform all view functions into monadic functions....

    // Looking at the transformed code (below), we see that all of the intermediate Readers that don’t
    // make direct use of the Email value don’t even mention it. They just go about their business
    // ignoring it but allowing descendant Readers to access it should they so desire. Boilerplate sniped.

    // Advantages
    //      Only contentM and rightM mention the email
    // Disadvanteges
    //      >>= boilerplate lambda code...we need some sort of 'do-notation' ('computation expression' sugar in F#)

    // widget Reader uses ask Reader
    let widgetM =
        ask >>= (fun (email : Email) ->
            return' <| div
                  [ p [ "Hey " + email + ", we've got a great offer for you!" ]
                  ])

    // The article function does not need the Email value itself so we don’t ask for it.
    // It does, however, need the markup provided by widgetM. Since widgetM is no longer
    // a function and is not of type Html but rather of type Reader Email Html, we need a
    // way to access the returned Html since the div expects a list of Html values. Luckily,
    // this is exactly what >>= as defined for the Reader monad does! So we feed widget
    // into it, and the value bound to the parameter in the lambda (widget’) is the eventual
    // Html returned from widget. Now within the lambda we can treat it like a good old Html
    // value and include it in the list of Html values we’re giving div here.
    let articleM =
        widgetM >>= fun widget' ->
            return' <| div
                [ p [ "this is an article" ]
                ; widget'
                ]

    let rightM =
        articleM >>= fun article' ->
            return' <| div
                  [ article'
                  ]

    // This is not monadic, because it does not need the shared environment (email)
    let leftM =
        div
            [ p [ "this is the left side" ]
            ]

    // The contentM Reader is the most interesting Reader, it needs both the Email itself and the
    // rendered Html from descendent Readers. To get the Email value, just ask for it, passing
    // ask through >>=. Then within that lambda grab the rightM Reader and pass it through >>=.
    // Now you have access to both bound values via the closure scope. Proceed as usual.
    let contentM =
        ask >>= fun email ->
        rightM >>= fun right' ->
            return' <| div
                [ h1 [ "Custom Content for " + email ]
                ; left
                ; right'
                ]

    let topNavM =
          div
            [ h1 [ "OurSite.com" ]
            ]

    let pageM =
        contentM >>= fun content' ->
            return' <| div
                  [ topNavM
                  ; content'
                  ]

    let viewM =
        pageM >>= fun page' ->
            return' <| div
                [ page'
                ]

    // we can run the reader monad like this
    // Shared view envorinment..think props in React!!!
    let email = "mmaxild@gmail.com"
    printfn "%s" <| runReader viewM email

    //----------------

    // let run dep (rm : Reader<_,_>) =
    //     rm dep

    // ('e -> 'a) -> 'e -> 'a
    let runReader (r: Reader<_,_>) = r

    let rDepEnv : (Dependency -> int) = return' 5
    let rStringEnv : (string -> int) = return' 5

    let result = "This is your string environment" |> (runReader rStringEnv)

    // So instead of threading the config to each function, we can rewrite this using MonadReader
    // and the configuration will get passed implicitly. To retrieve the configuration, we call ask:

    // tom :: Reader string string
    let tom r = r >>= (fun env ->
        let value = env
        return' <| sprintf "This is Jerry %d\n" value)

    // jerry :: Reader string string
    let jerry env = env >>= (fun env ->
        let value = env
        return' <| sprintf "This is Tom %d\n" value)


    // tomAndJerry :: Reader string string
    // tomAndJerry = do
    //     t <- tom
    //     j <- jerry
    //     return (t ++ "\n" ++ j)
    // string -> string
    let tomAndJerry =
        tom >>= (fun env ->
        let t = env
        jerry >>= (fun env ->
        let j = env
        return' (t + "\n" + j)))

    let runJerryRun = (runReader tom) "Who is this?"

    //
    //
    //

    let getName (e : Dependency) = e.Username

    let env = { Username = "Morten Maxild"; Password = "SoVerySecret" }

    let getPassword (e : Dependency) = e.Password

    let execute (programId: int) (e : Dependency) =
        let user = getName e
        let pwd = getPassword e
        let desc = sprintf "User %s with password '%s' is running program #%d" user pwd programId
        desc

    let exec = execute