namespace Rogz.Context.Kleisli.Reader

open Rogz.Context.Data.Reader


[<Struct; NoComparison; NoEquality>]
type ReaderK<'E, 'X, 'T> = ReaderK of ('X -> Reader<'E, 'T>)
with

    member inline s.Invoke(input) = let (ReaderK k) = s in k input
    
    member inline s.Select(f: System.Func< ^T, ^U>) =
        let (ReaderK k) = s in ReaderK (fun x -> (k x).Select(f))

    member inline s.Zip(other: ReaderK< ^E, ^X, ^U>, f: System.Func< ^T, ^U, ^V>) =
        let (ReaderK k1), (ReaderK k2) = s, other in ReaderK (fun x -> (k1 x).Zip(k2 x, f))

    member inline s.Join
        (other: ReaderK< ^E, ^X, ^U>,
        _: System.Func< ^T, int>,
        _: System.Func< ^U, int>,
        f: System.Func< ^T, ^U, ^V>) =
            s.Zip(other, f)

    member inline s.SelectMany(f: System.Func< ^T, ReaderK< ^E, ^X, ^U>>) =
        let (ReaderK k) = s
        ReaderK (fun x -> (k x).SelectMany(fun a -> f.Invoke(a).Invoke(x)))

    member inline s.SelectMany(f: System.Func< ^T, ReaderK< ^E, ^X, ^U>>, g: System.Func< ^T, ^U, ^V>) =
        let (ReaderK k) = s
        ReaderK (fun x -> (k x).SelectMany((fun a -> f.Invoke(a).Invoke(x)), g))


module ReaderK =

// Primitives

    let inline runKleisli (input: ^x) (ReaderK k) : Reader< ^e, ^a> = k input


// Functor

    let inline map (f: ^a -> ^b) (ReaderK k) = ReaderK (fun (x: ^x) -> Reader.map f (k x))


// Profunctor

    let inline dimap (f: ^c -> ^a) (g: ^b -> ^d) (ReaderK k) : ReaderK< ^e, ^c, ^d> =
        ReaderK (fun c -> Reader.map g (k (f c)))

    let inline mapl (f: ^c -> ^a) pf : ReaderK< ^e, ^c, ^b> = dimap f id pf


// Applicative

    let unit (value: 'a) : ReaderK<'e, 'x, ^a> = ReaderK (fun _ -> Reader.unit value)

    let inline map2 (f: ^a -> ^b -> ^c) (ReaderK ka) (ReaderK kb) : ReaderK< ^e, ^x, ^c> =
        ReaderK (fun x -> Reader.map2 f (ka x) (kb x))


// Monad

    let inline bind f (ReaderK k) : ReaderK< ^e, ^x, ^b> =
        ReaderK (fun (x: ^x) ->
            let (Reader r) = k x
            Reader (fun (e: ^e) ->
                Reader.runReader e (runKleisli x (f ((r e): ^a)))))


    [<RequireQualifiedAccess>]
    module Workflow =

        type ReaderKBuilder() =
            member _.Return(x) : ReaderK<'e, 'x, 'a> = unit x
            member _.ReturnFrom(m) : ReaderK<'e, 'x, 'a> = m
            member _.Zero() : ReaderK<'e, 'x, unit> = unit ()
            member inline _.Bind(m: ReaderK< ^e, ^x, ^a>, f) : ReaderK< ^e, ^x, ^b> = bind f m            

    
    let readerK = Workflow.ReaderKBuilder()


// Kleisli

    let lift m : ReaderK<'e, '``_``, 'a> = ReaderK (fun _ -> m)
    
    let inline hoist (f: Reader< ^e, ^a> -> Reader< ^e, ^b>) (ReaderK k) : ReaderK< ^e, ^x, ^b> =
        ReaderK (k >> f)

    let inline mapM f (ReaderK k) : ReaderK< ^e, ^x, ^b> =
        ReaderK (fun x -> Reader.bind f (k x))

    let inline bindM f m : ReaderK< ^e, ^x, ^b> =
        ReaderK (fun x -> Reader.bind (fun a -> runKleisli x (f a)) m)


// Category

    let identity<'e, 'a> : ReaderK< ^e, ^a, ^a> = ReaderK Reader.unit

    let inline compose (ReaderK kbc) (ReaderK (kab: ^a -> Reader< ^e, ^b>)) : ReaderK< ^e, ^a, ^c> =
        ReaderK (fun a ->
            let (Reader r) = kab a
            Reader (fun e -> Reader.runReader e (kbc (r e))))


// Arrow
    
    let inline arr f : ReaderK< ^e, ^a, ^b> = ReaderK (fun a -> Reader.unit (f a))

    let inline first (ReaderK k) : ReaderK< ^e, ^a * ^c, ^b * ^c> =
        ReaderK (fun (a, c) ->
            let (Reader r) = k a in Reader (fun e -> r e, c))

    let inline second (ReaderK k) : ReaderK< ^e, ^c * ^a, ^c * ^b> =
        ReaderK (fun (c, a) ->
            let (Reader r) = k a in Reader (fun e -> c, r e))

    let inline split (ReaderK cd) (ReaderK ab) : ReaderK< ^e, ^a * ^c, ^b * ^d> =
        ReaderK (fun (a, c) ->
            let (Reader ra), (Reader rb) = ab a, cd c
            Reader (fun e -> ra e, rb e))

    let inline fanout (ReaderK ac) (ReaderK ab) : ReaderK< ^e, ^a, ^b * ^c> =
        ReaderK (fun a ->
            let (Reader ra), (Reader rb) = ab a, ac a
            Reader (fun e -> ra e, rb e))


// ArrowChoice

    open Rogz.Context.Data.Either

    let inline feedl (ReaderK ab) : ReaderK< ^e, Either< ^a, ^c>, Either< ^b, ^c>> =
        ReaderK (function Left a  -> Reader.map Left (ab a)
                        | Right c -> Reader.unit (Right c))

    let inline feedr (ReaderK ab) : ReaderK< ^e, Either< ^c, ^a>, Either< ^c, ^b>> =
        ReaderK (function Left c  -> Reader.unit (Left c)
                        | Right a -> Reader.map Right (ab a))

    let inline merge (ReaderK cd) (ReaderK ab) : ReaderK< ^e, Either< ^a, ^c>, Either< ^b, ^d>> =
        ReaderK (function Left a  -> Reader.map Left (ab a)
                        | Right c -> Reader.map Right (cd c))

    let inline fanin (ReaderK cb) (ReaderK ab) : ReaderK< ^e, Either< ^a, ^c>, ^b> =
        ReaderK (function Left a  -> ab a
                        | Right c -> cb c)