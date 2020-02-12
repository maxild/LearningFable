module Maybe

open System
//open System.Diagnostics
// [<DebuggerStepThrough>]

[<Sealed>]
type MaybeBuilder () =
    member inline _.Return value: 'T option =
        Some value

    member inline _.ReturnFrom value: 'T option =
        value

    member inline _.Zero (): unit option =
        Some ()

    member _.Delay (f: unit -> 'T option): 'T option =
        f ()

    member inline _.Combine (r1, r2: 'T option): 'T option =
        match r1 with
        | None -> None
        | Some () -> r2

    member _.Bind (value, f: 'T -> 'U option): 'U option =
        Option.bind f value

    member _.Using (resource: ('T :> System.IDisposable), body: _ -> _ option): _ option =
        try body resource
        finally
            if not <| obj.ReferenceEquals (null, box resource) then
                resource.Dispose ()

    member x.While (guard, body: _ option): _ option =
        if guard () then
            // OPTIMIZE: This could be simplified so we don't need to make calls to Bind and While.
            x.Bind (body, (fun () -> x.While (guard, body)))
        else
            x.Zero ()

    member x.For (sequence: seq<_>, body: 'T -> unit option): _ option =
        // OPTIMIZE: This could be simplified so we don't need to make calls to Using, While, Delay.
        x.Using (sequence.GetEnumerator (), fun enum ->
            x.While (
                enum.MoveNext,
                x.Delay (fun () ->
                    body enum.Current)))

[<AutoOpen>]
module Maybe =
    let maybe = MaybeBuilder()

[<EntryPoint>]
let main argv =
    let x =
        maybe {
            let! x = Some 1
            let! y = Some 2
            return x + y
        }

    printfn "%A" x
    Console.ReadKey() |> ignore
    0