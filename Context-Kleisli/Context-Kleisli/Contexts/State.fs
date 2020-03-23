namespace Rogz.Context.Kleisli.State

open Rogz.Context.Data.State


[<Struct; NoComparison; NoEquality>]
type StateK<'S, 'X, 'T> = StateK of ('X -> State<'S, 'T>)
with

    member inline s.Invoke(input) = let (StateK k) = s in k input
    
    member inline s.Select(f: System.Func< ^T, ^U>) =
        let (StateK k) = s in StateK (fun x -> (k x).Select(f))

    member inline s.Zip(other: StateK< ^S, ^X, ^U>, f: System.Func< ^T, ^U, ^V>) =
        let (StateK k1), (StateK k2) = s, other in StateK (fun x -> (k1 x).Zip(k2 x, f))

    member inline s.Join
        (other: StateK< ^S, ^X, ^U>,
        _: System.Func< ^T, int>,
        _: System.Func< ^U, int>,
        f: System.Func< ^T, ^U, ^V>) =
            s.Zip(other, f)

    member inline s.SelectMany(f: System.Func< ^T, StateK< ^S, ^X, ^U>>) =
        let (StateK k) = s
        StateK (fun x -> (k x).SelectMany(fun a -> f.Invoke(a).Invoke(x)))

    member inline s.SelectMany(f: System.Func< ^T, StateK< ^S, ^X, ^U>>, g: System.Func< ^T, ^U, ^V>) =
        let (StateK k) = s
        StateK (fun x -> (k x).SelectMany((fun a -> f.Invoke(a).Invoke(x)), g))


module StateK =

// Primitives

    let inline runKleisli (input: ^x) (StateK k) : State< ^s, ^a> = k input


// Functor

    let inline map (f: ^a -> ^b) (StateK k) = StateK (fun (x: ^x) -> State.map f (k x))


// Profunctor

    let inline dimap (f: ^c -> ^a) (g: ^b -> ^d) (StateK k) : StateK< ^s, ^c, ^d> =
        StateK (fun c -> State.map g (k (f c)))

    let inline mapl (f: ^c -> ^a) pf : StateK< ^s, ^c, ^b> = dimap f id pf


// Applicative

    let unit (value: 'a) : StateK<'s, 'x, ^a> = StateK (fun _ -> State.unit value)

    let inline map2 (f: ^a -> ^b -> ^c) (StateK ka) (StateK kb) : StateK< ^s, ^x, ^c> =
        StateK (fun x -> State.map2 f (ka x) (kb x))


// Monad

    let inline bind f (StateK k) : StateK< ^s, ^x, ^b> =
        StateK (fun (x: ^x) ->
            let (State g) = k x
            State (fun (s: ^s) ->
                let st = g s
                State.runState st.State <| runKleisli x (f (st.Value: ^a))))


    [<RequireQualifiedAccess>]
    module Workflow =

        type StateKBuilder() =
            member _.Return(x) : StateK<'s, 'x, 'a> = unit x
            member _.ReturnFrom(m) : StateK<'s, 'x, 'a> = m
            member _.Zero() : StateK<'s, 'x, unit> = unit ()
            member inline _.Bind(m: StateK< ^s, ^x, ^a>, f) : StateK< ^s, ^x, ^b> = bind f m            

    
    let stateK = Workflow.StateKBuilder()


// Kleisli

    let lift m : StateK<'s, '``_``, 'a> = StateK (fun _ -> m)
    
    let inline hoist (f: State< ^s, ^a> -> State< ^s, ^b>) (StateK k) : StateK< ^s, ^x, ^b> =
        StateK (k >> f)

    let inline mapM f (StateK k) : StateK< ^s, ^x, ^b> =
        StateK (fun x -> State.bind f (k x))

    let inline bindM f m : StateK< ^s, ^x, ^b> =
        StateK (fun x -> State.bind (fun a -> runKleisli x (f a)) m)


// Category

    let identity<'s, 'a> : StateK< ^s, ^a, ^a> = StateK State.unit

    let inline compose (StateK kbc) (StateK (kab: ^a -> State< ^s, ^b>)) : StateK< ^s, ^a, ^c> =
        StateK (fun a ->
            let (State g) = kab a
            State (fun s ->
                let sa = g s
                State.runState sa.State (kbc sa.Value)))


// Arrow
    
    let inline arr f : StateK< ^s, ^a, ^b> = StateK (fun a -> State.unit (f a))

    let inline first (StateK k) : StateK< ^s, ^a * ^c, ^b * ^c> =
        StateK (fun (a, c) ->
            let (State g) = k a
            State (fun s ->
                let r = g s
                { SVPair.State = r.State
                ; Value = r.Value, c }))

    let inline second (StateK k) : StateK< ^s, ^c * ^a, ^c * ^b> =
        StateK (fun (c, a) ->
            let (State g) = k a
            State (fun s ->
                let r = g s
                { SVPair.State = r.State
                ; Value = c, r.Value }))

    let inline split (StateK cd) (StateK ab) : StateK< ^s, ^a * ^c, ^b * ^d> =
        StateK (fun (a, c) ->
            let (State ga), (State gb) = ab a, cd c
            State (fun s ->
                let ra = ga s
                let rb = gb ra.State
                { SVPair.State = rb.State
                ; Value = ra.Value, rb.Value }))

    let inline fanout (StateK ac) (StateK ab) : StateK< ^s, ^a, ^b * ^c> =
        StateK (fun a ->
            let (State ga), (State gb) = ab a, ac a
            State (fun s ->
                let ra = ga s
                let rb = gb ra.State
                { SVPair.State = rb.State
                ; Value = ra.Value, rb.Value }))


// ArrowChoice

    open Rogz.Context.Data.Either

    let inline feedl (StateK ab) : StateK< ^s, Either< ^a, ^c>, Either< ^b, ^c>> =
        StateK (function Left a  -> State.map Left (ab a)
                       | Right c -> State.unit (Right c))

    let inline feedr (StateK ab) : StateK< ^s, Either< ^c, ^a>, Either< ^c, ^b>> =
        StateK (function Left c  -> State.unit (Left c)
                       | Right a -> State.map Right (ab a))

    let inline merge (StateK cd) (StateK ab) : StateK< ^s, Either< ^a, ^c>, Either< ^b, ^d>> =
        StateK (function Left a  -> State.map Left (ab a)
                       | Right c -> State.map Right (cd c))

    let inline fanin (StateK cb) (StateK ab) : StateK< ^s, Either< ^a, ^c>, ^b> =
        StateK (function Left a  -> ab a
                       | Right c -> cb c)