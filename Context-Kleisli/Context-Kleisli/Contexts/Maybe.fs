namespace Rogz.Context.Kleisli.Maybe

open Rogz.Context.Data.Maybe


[<Struct; NoComparison; NoEquality>]
type MaybeK<'X, 'T> = MaybeK of ('X -> Maybe<'T>)
with

    member inline s.Invoke(input) = let (MaybeK k) = s in k input
    
    member inline s.Select(f: System.Func< ^T, ^U>) =
        let (MaybeK k) = s in MaybeK (fun x -> (k x).Select(f))

    member inline s.Zip(other: MaybeK< ^X, ^U>, f: System.Func< ^T, ^U, ^V>) =
        let (MaybeK k1), (MaybeK k2) = s, other in MaybeK (fun x -> (k1 x).Zip(k2 x, f))

    member inline s.Join
        (other: MaybeK< ^X, ^U>,
        _: System.Func< ^T, int>,
        _: System.Func< ^U, int>,
        f: System.Func< ^T, ^U, ^V>) =
            s.Zip(other, f)

    member inline s.SelectMany(f: System.Func< ^T, MaybeK< ^X, ^U>>) =
        let (MaybeK k) = s
        MaybeK (fun x ->
            match k x with
            | Nothing -> Nothing
            | Just a  -> f.Invoke(a).Invoke(x))

    member inline s.SelectMany(f: System.Func< ^T, MaybeK< ^X, ^U>>, g: System.Func< ^T, ^U, ^V>) =
        let (MaybeK k) = s
        MaybeK (fun x ->
            match k x with
            | Nothing -> Nothing
            | Just a  ->
                match f.Invoke(a).Invoke(x) with
                | Nothing -> Nothing
                | Just b  -> Just (g.Invoke(a, b)))


module MaybeK =

// Primitives

    let inline runKleisli (input: ^x) (MaybeK k) : Maybe< ^a> = k input


// Functor

    let inline map (f: ^a -> ^b) (MaybeK k) =
        MaybeK (fun (x: ^x) ->
            match k x with
            | Nothing -> Nothing
            | Just a  -> Just (f a))


// Profunctor

    let inline dimap (f: ^c -> ^a) (g: ^b -> ^d) (MaybeK k) =
        MaybeK (fun (c: ^c) ->
            match k (f c) with
            | Nothing -> Nothing
            | Just b  -> Just (g b))

    let inline mapl (f: ^c -> ^a) (MaybeK k) : MaybeK< ^c, ^b> = MaybeK (fun x -> k (f x))


// Applicative

    let unit (value: 'a) : MaybeK<'x, ^a> = MaybeK (fun _ -> Just value)

    let inline map2 (f: ^a -> ^b -> ^c) (MaybeK ka) (MaybeK kb) =
        MaybeK (fun (x: ^x) ->
            match ka x, kb x with
            | Nothing, _ | _, Nothing -> Nothing
            | Just a, Just b          -> Just (f a b))


// Monad

    let inline bind f (MaybeK k) : MaybeK< ^x, ^b> =
        MaybeK (fun (x: ^x) ->
            match k x with
            | Nothing      -> Nothing
            | Just (a: ^a) -> let (MaybeK k) = f a in k x)


    [<RequireQualifiedAccess>]
    module Workflow =

        type MaybeKBuilder() =
            member _.Return(x) : MaybeK<'x, 'a> = unit x
            member _.ReturnFrom(m) : MaybeK<'x, 'a> = m
            member _.Zero() : MaybeK<'x, unit> = unit ()
            member inline _.Bind(m: MaybeK< ^x, ^a>, f) : MaybeK< ^x, ^b> = bind f m            

    
    let maybeK = Workflow.MaybeKBuilder()


// Kleisli

    let lift m : MaybeK<'``_``, 'a> = MaybeK (fun _ -> m)
    
    let inline hoist (f: Maybe< ^a> -> Maybe< ^b>) (MaybeK k) : MaybeK< ^x, ^b> = MaybeK (k >> f)

    let inline mapM f (MaybeK k) : MaybeK< ^x, ^b> = MaybeK (fun x -> Maybe.bind f (k x))

    let inline bindM f m : MaybeK< ^x, ^b> =
        MaybeK (fun x -> Maybe.bind (fun a -> runKleisli x (f a)) m)


// Category

    let identity<'a> : MaybeK< ^a, ^a> = MaybeK Just

    let inline compose (MaybeK kbc) (MaybeK kab) : MaybeK< ^a, ^c> =
        MaybeK (fun a ->
            match kab a with
            | Nothing -> Nothing
            | Just b  -> kbc (b: ^b))


// Arrow
    
    let inline arr f : MaybeK< ^a, ^b> = MaybeK (fun a -> Just (f a))


    let inline first (MaybeK k) : MaybeK< ^a * ^c, ^b * ^c> =
        MaybeK (fun (a, c) ->
            match k a with
            | Nothing -> Nothing
            | Just b  -> Just (b, c))

    let inline second (MaybeK k) : MaybeK< ^c * ^a, ^c * ^b> =
        MaybeK (fun (c, a) ->
            match k a with
            | Nothing -> Nothing
            | Just b  -> Just (c, b))

    let inline split (MaybeK cd) (MaybeK ab) : MaybeK< ^a * ^c, ^b * ^d> =
        MaybeK (fun (a, c) ->
            match ab a, cd c with
            | Nothing, _ | _, Nothing -> Nothing
            | Just b, Just d          -> Just (b, d))

    let inline fanout (MaybeK ac) (MaybeK ab) : MaybeK< ^a, ^b * ^c> =
        MaybeK (fun a ->
            match ab a, ac a with
            | Nothing, _ | _, Nothing -> Nothing
            | Just b, Just c          -> Just (b, c))


// ArrowChoice

    open Rogz.Context.Data.Either

    let inline feedl (MaybeK ab) : MaybeK<Either< ^a, ^c>, Either< ^b, ^c>> =
        MaybeK (function Left a  -> Maybe.map Left (ab a)
                       | Right c -> Just (Right c))

    let inline feedr (MaybeK ab) : MaybeK<Either< ^c, ^a>, Either< ^c, ^b>> =
        MaybeK (function Left c  -> Just (Left c)
                       | Right a -> Maybe.map Right (ab a))

    let inline merge (MaybeK cd) (MaybeK ab) : MaybeK<Either< ^a, ^c>, Either< ^b, ^d>> =
        MaybeK (function Left a  -> Maybe.map Left (ab a)
                       | Right c -> Maybe.map Right (cd c))

    let inline fanin (MaybeK cb) (MaybeK ab) : MaybeK<Either< ^a, ^c>, ^b> =
        MaybeK (function Left a  -> ab a
                       | Right c -> cb c)