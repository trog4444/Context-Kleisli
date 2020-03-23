namespace Rogz.Context.Kleisli.Cont

open Rogz.Context.Data.Cont


[<Struct; NoComparison; NoEquality>]
type ContK<'R, 'X, 'T> = ContK of ('X -> Cont<'R, 'T>)
with

    member inline s.Invoke(input) = let (ContK k) = s in k input
    
    member inline s.Select(f: System.Func< ^T, ^U>) =
        let (ContK k) = s in ContK (fun x -> (k x).Select(f))

    member inline s.Zip(other: ContK< ^R, ^X, ^U>, f: System.Func< ^T, ^U, ^V>) =
        let (ContK k1), (ContK k2) = s, other in ContK (fun x -> (k1 x).Zip(k2 x, f))

    member inline s.Join
        (other: ContK< ^R, ^X, ^U>,
        _: System.Func< ^T, int>,
        _: System.Func< ^U, int>,
        f: System.Func< ^T, ^U, ^V>) =
            s.Zip(other, f)

    member inline s.SelectMany(f: System.Func< ^T, ContK< ^R, ^X, ^U>>) =
        let (ContK k) = s
        ContK (fun x -> (k x).SelectMany(fun a -> f.Invoke(a).Invoke(x)))

    member inline s.SelectMany(f: System.Func< ^T, ContK< ^R, ^X, ^U>>, g: System.Func< ^T, ^U, ^V>) =
        let (ContK k) = s
        ContK (fun x -> (k x).SelectMany((fun a -> f.Invoke(a).Invoke(x)), g))


module ContK =

// Primitives

    let inline runKleisli (input: ^x) (ContK k) : Cont< ^r, ^a> = k input


// Functor

    let inline map (f: ^a -> ^b) (ContK k) = ContK (fun (x: ^x) -> Cont.map f (k x))


// Profunctor

    let inline dimap (f: ^c -> ^a) (g: ^b -> ^d) (ContK k) : ContK< ^r, ^c, ^d> =
        ContK (fun c -> Cont.map g (k (f c)))

    let inline mapl (f: ^c -> ^a) pf : ContK< ^r, ^c, ^b> = dimap f id pf


// Applicative

    let unit (value: 'a) : ContK<'r, 'x, ^a> = ContK (fun _ -> Cont.unit value)

    let inline map2 (f: ^a -> ^b -> ^c) (ContK ka) (ContK kb) : ContK< ^r, ^x, ^c> =
        ContK (fun x -> Cont.map2 f (ka x) (kb x))


// Monad

    let inline bind (f: ^a -> ContK< ^r, ^x, ^b>) (ContK (k: ^x -> Cont< ^r, ^a>)) : ContK< ^r, ^x, ^b> =
        ContK (fun x -> let (Cont cc) = k x in Cont (fun k' -> cc (fun a -> Cont.runCont k' (runKleisli x (f a)))))


    [<RequireQualifiedAccess>]
    module Workflow =

        type ContKBuilder() =
            member _.Return(x) : ContK<'r, 'x, 'a> = unit x
            member _.ReturnFrom(m) : ContK<'r, 'x, 'a> = m
            member _.Zero() : ContK<'r, 'x, unit> = unit ()
            member inline _.Bind(m: ContK< ^r, ^x, ^a>, f) : ContK< ^r, ^x, ^b> = bind f m            

    
    let contK = Workflow.ContKBuilder()


// Kleisli

    let lift m : ContK<'r, '``_``, 'a> = ContK (fun _ -> m)
    
    let inline hoist (f: Cont< ^r, ^a> -> Cont< ^r, ^b>) (ContK k) : ContK< ^r, ^x, ^b> =
        ContK (k >> f)

    let inline mapM f (ContK k) : ContK< ^r, ^x, ^b> =
        ContK (fun x -> Cont.bind f (k x))

    let inline bindM f m : ContK< ^r, ^x, ^b> =
        ContK (fun x -> Cont.bind (fun a -> runKleisli x (f a)) m)


// Category

    let identity<'r, 'a> : ContK< ^r, ^a, ^a> = ContK (fun a -> Cont (fun k -> k a))

    let inline compose (ContK kbc) (ContK kab) : ContK< ^r, ^a, ^c> =
        ContK (fun a ->
            let (Cont ca) = kab a
            Cont (fun k -> ca (fun (b: ^b) -> Cont.runCont k (kbc b))))


// Arrow
    
    let inline arr f : ContK< ^r, ^a, ^b> = ContK (fun a -> Cont.unit (f a))

    let inline first (ContK k) : ContK< ^r, ^a * ^c, ^b * ^c> =
        ContK (fun (a, c) ->
            let (Cont cc) = k a
            Cont (fun k -> cc (fun b -> k (b, c))))

    let inline second (ContK k) : ContK< ^r, ^c * ^a, ^c * ^b> =
        ContK (fun (c, a) ->
            let (Cont cc) = k a
            Cont (fun k -> cc (fun b -> k (c, b))))

    let inline split (ContK cd) (ContK ab) : ContK< ^r, ^a * ^c, ^b * ^d> =
        ContK (fun (a, c) ->
            let (Cont ca), (Cont cb) = ab a, cd c
            Cont (fun k -> ca (fun b -> cb (fun d -> k (b, d)))))

    let inline fanout (ContK ac) (ContK ab) : ContK< ^r, ^a, ^b * ^c> =
        ContK (fun a ->
            let (Cont ca), (Cont cb) = ab a, ac a
            Cont (fun k -> ca (fun b -> cb (fun c -> k (b, c)))))


// ArrowChoice

    open Rogz.Context.Data.Either

    let inline feedl (ContK ab) : ContK< ^r, Either< ^a, ^c>, Either< ^b, ^c>> =
        ContK (function Left a  -> Cont.map Left (ab a)
                      | Right c -> Cont.unit (Right c))

    let inline feedr (ContK ab) : ContK< ^r, Either< ^c, ^a>, Either< ^c, ^b>> =
        ContK (function Left c  -> Cont.unit (Left c)
                      | Right a -> Cont.map Right (ab a))

    let inline merge (ContK cd) (ContK ab) : ContK< ^r, Either< ^a, ^c>, Either< ^b, ^d>> =
        ContK (function Left a  -> Cont.map Left (ab a)
                      | Right c -> Cont.map Right (cd c))

    let inline fanin (ContK cb) (ContK ab) : ContK< ^r, Either< ^a, ^c>, ^b> =
        ContK (function Left a  -> ab a
                      | Right c -> cb c)