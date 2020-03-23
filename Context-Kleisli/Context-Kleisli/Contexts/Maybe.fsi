namespace Rogz.Context.Kleisli.Maybe

open Rogz.Context.Data.Maybe


/// <summary>The Maybe type lifted into Kleisli computations.</summary>
[<Struct; NoComparison; NoEquality>]
type MaybeK<'X, 'T> = MaybeK of ('X -> Maybe<'T>)
with

    /// <summary>Run the function with the supplied input.</summary>
    member inline Invoke: input: ^X -> Maybe< ^T>

    /// <summary>Lift a function onto a context.</summary>
    member inline Select: f: System.Func< ^T, ^U> -> MaybeK< ^X, ^U>

    /// <summary>Lift a binary function onto contexts.</summary>
    member inline Zip: other: MaybeK< ^X, ^U> * mapping: System.Func< ^T, ^U, ^V> -> MaybeK< ^X, ^V>

    /// <summary>Lift a binary function onto contexts.</summary>
    member inline Join:
        other: MaybeK< ^X, ^U> *
        kt: System.Func< ^T, int> *
        ku: System.Func< ^U, int> *
        mapping: System.Func< ^T, ^U, ^V> -> MaybeK< ^X, ^V>

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    member inline SelectMany: projection: System.Func< ^T, MaybeK< ^X, ^U>> -> MaybeK< ^X, ^U>

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    member inline SelectMany:
        projection: System.Func< ^T, MaybeK< ^X, ^U>> *
        mapping: System.Func< ^T, ^U, ^V> -> MaybeK< ^X, ^V>


/// <summary>Operations on MaybeK's.</summary>
module MaybeK =

// Primitives

    /// <summary>Run the function with the supplied input.</summary>
    val inline runKleisli: input: ^x -> k: MaybeK< ^x, ^a> -> Maybe< ^a>


// Functor

    /// <summary>Lift a function onto a context.</summary>
    val inline map: f: (^a -> ^b) -> fa: MaybeK< ^x, ^a> -> MaybeK< ^x, ^b>


// Profunctor

    /// <summary>Map over both arguments at the same time, the first (i.e. 'left') contravariantly and the second (i.e. 'right') covariantly.</summary>
    val inline dimap: f: (^c -> ^a) -> g: (^b -> ^d) -> pf: MaybeK< ^a, ^b> -> MaybeK< ^c, ^d>

    /// <summary>Map the first (i.e. 'left') argument contravariantly.</summary>
    val inline mapl: f: (^c -> ^a) -> pf: MaybeK< ^a, ^b> -> MaybeK< ^c, ^b>


// Applicative

    /// <summary>Lift a value into a context.</summary>
    val unit: value: 'a -> MaybeK<'x, ^a>

    /// <summary>Lift a binary function onto contexts.</summary>
    val inline map2: f: (^a -> ^b -> ^c) -> fa: MaybeK< ^x, ^a> -> fb: MaybeK< ^x, ^b> -> MaybeK< ^x, ^c>


// Monad

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline bind: f: (^a -> MaybeK< ^x, ^b>) -> m: MaybeK< ^x, ^a> -> MaybeK< ^x, ^b>


    /// <summary>Computation expression / monadic-workflow type and operations for the given context.</summary>
    [<RequireQualifiedAccess>]
    module Workflow =

        /// <summary>Computation expression for the given monadic type.</summary>
        type MaybeKBuilder =
            new: unit -> MaybeKBuilder
            member Return: x: 'a -> MaybeK<'x, ^a>
            member ReturnFrom: m: MaybeK<'x, 'a> -> MaybeK< ^x, ^a>
            member Zero: unit -> MaybeK<'x, unit>
            member inline Bind: m: MaybeK< ^x, ^a> * f: (^a -> MaybeK< ^x, ^b>) -> MaybeK< ^x, ^b>            


    /// <summary>Computation expression instance for the given type.</summary>
    val maybeK: Workflow.MaybeKBuilder


// Kleisli

    /// <summary>Lift a value of the underlying monad into its Kleisli form.</summary>
    val lift: m: Maybe<'a> -> MaybeK<'``_``, ^a>
    
    /// <summary>Lift a monad momorphism of the base monad into its Kleisli form.</summary>
    val inline hoist: trans: (Maybe< ^a> -> Maybe< ^b>) -> m: MaybeK< ^x, ^a> -> MaybeK< ^x, ^b>

    /// <summary>Map a monad producing function across an existing monad in its own Kleisli category.</summary>
    val inline mapM: f: (^a -> Maybe< ^b>) -> m: MaybeK< ^x, ^a> -> MaybeK< ^x, ^b>

    /// <summary>Acts as a monadic bind on the Kleisli type using the value(s) from the underlying monad.</summary>
    val inline bindM: f: (^a -> MaybeK< ^x, ^b>) -> m: Maybe< ^a> -> MaybeK< ^x, ^b>


// Cat

    /// <summary>Identity element of a category.</summary>
    val identity<'a> : MaybeK< ^a, ^a>

    /// <summary>Compose two members of a category together.</summary>
    val inline compose: o2: MaybeK< ^b, ^c> -> o1: MaybeK< ^a, ^b> -> MaybeK< ^a, ^c>


// Arrow

    /// <summary>Lift a function to an arrow.</summary>
    val inline arr: f: (^a -> ^b) -> MaybeK< ^a, ^b>

    /// <summary>Send the first component of the input through the argument arrow, and copy the rest unchanged to the output.</summary>
    val inline first: ar: MaybeK< ^a, ^b> -> MaybeK< ^a * ^c, ^b * ^c>

    /// <summary>Send the second component of the input through the argument arrow, and copy the rest unchanged to the output.</summary>
    val inline second: ar: MaybeK< ^a, ^b> -> MaybeK< ^c * ^a, ^c * ^b>

    /// <summary>Split the input between the two argument arrows and combine their output.</summary>
    val inline split: a2: MaybeK< ^c, ^d> -> a1: MaybeK< ^a, ^b> -> MaybeK< ^a * ^c, ^b * ^d>

    /// <summary>Fanout: send the input to both argument arrows and combine their output.</summary>
    val inline fanout: a2: MaybeK< ^a, ^c> -> a1: MaybeK< ^a, ^b> -> MaybeK< ^a, ^b * ^c>


// Arrow.Choice

    open Rogz.Context.Data.Either

    /// <summary>Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. A mirror of 'feedr'.</summary>
    val inline feedl: ar: MaybeK< ^a, ^b> -> MaybeK<Either< ^a, ^c>, Either< ^b, ^c>>

    /// <summary>Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. A mirror of 'feedl'.</summary>
    val inline feedr: ar: MaybeK< ^a, ^b> -> MaybeK<Either< ^c, ^a>, Either< ^c, ^b>>

    /// <summary>Split the input between the two argument arrows, retagging and merging their outputs.</summary>
    val inline merge: a2: MaybeK< ^c, ^d> -> a1: MaybeK< ^a, ^b> -> MaybeK<Either< ^a, ^c>, Either< ^b, ^d>>

    /// <summary>Split the input between the two argument arrows and merge their outputs.</summary>
    val inline fanin: a2: MaybeK< ^c, ^b> -> a1: MaybeK< ^a, ^b> -> MaybeK<Either< ^a, ^c>, ^b>