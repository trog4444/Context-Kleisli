namespace Rogz.Context.Kleisli.Reader

open Rogz.Context.Data.Reader


/// <summary>The Reader type lifted into Kleisli computations.</summary>
[<Struct; NoComparison; NoEquality>]
type ReaderK<'E, 'X, 'T> = ReaderK of ('X -> Reader<'E, 'T>)
with

    /// <summary>Run the function with the supplied input.</summary>
    member inline Invoke: input: ^X -> Reader< ^E, ^T>

    /// <summary>Lift a function onto a context.</summary>
    member inline Select: f: System.Func< ^T, ^U> -> ReaderK< ^E, ^X, ^U>

    /// <summary>Lift a binary function onto contexts.</summary>
    member inline Zip: other: ReaderK< ^E, ^X, ^U> * mapping: System.Func< ^T, ^U, ^V> -> ReaderK< ^E, ^X, ^V>

    /// <summary>Lift a binary function onto contexts.</summary>
    member inline Join:
        other: ReaderK< ^E, ^X, ^U> *
        kt: System.Func< ^T, int> *
        ku: System.Func< ^U, int> *
        mapping: System.Func< ^T, ^U, ^V> -> ReaderK< ^E, ^X, ^V>

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    member inline SelectMany: projection: System.Func< ^T, ReaderK< ^E, ^X, ^U>> -> ReaderK< ^E, ^X, ^U>

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    member inline SelectMany:
        projection: System.Func< ^T, ReaderK< ^E, ^X, ^U>> *
        mapping: System.Func< ^T, ^U, ^V> -> ReaderK< ^E, ^X, ^V>


/// <summary>Operations on ReaderK's.</summary>
module ReaderK =

// Primitives

    /// <summary>Run the function with the supplied input.</summary>
    val inline runKleisli: input: ^x -> k: ReaderK< ^e, ^x, ^a> -> Reader< ^e, ^a>


// Functor

    /// <summary>Lift a function onto a context.</summary>
    val inline map: f: (^a -> ^b) -> fa: ReaderK< ^e, ^x, ^a> -> ReaderK< ^e, ^x, ^b>


// Profunctor

    /// <summary>Map over both arguments at the same time, the first (i.e. 'left') contravariantly and the second (i.e. 'eight') covariantly.</summary>
    val inline dimap: f: (^c -> ^a) -> g: (^b -> ^d) -> pf: ReaderK< ^e, ^a, ^b> -> ReaderK< ^e, ^c, ^d>

    /// <summary>Map the first (i.e. 'left') argument contravariantly.</summary>
    val inline mapl: f: (^c -> ^a) -> pf: ReaderK< ^e, ^a, ^b> -> ReaderK< ^e, ^c, ^b>


// Applicative

    /// <summary>Lift a value into a context.</summary>
    val unit: value: 'a -> ReaderK<'e, 'x, ^a>

    /// <summary>Lift a binary function onto contexts.</summary>
    val inline map2: f: (^a -> ^b -> ^c) -> fa: ReaderK< ^e, ^x, ^a> -> fb: ReaderK< ^e, ^x, ^b> -> ReaderK< ^e, ^x, ^c>


// Monad

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline bind: f: (^a -> ReaderK< ^e, ^x, ^b>) -> m: ReaderK< ^e, ^x, ^a> -> ReaderK< ^e, ^x, ^b>


    /// <summary>Computation expression / monadic-workflow type and operations for the given context.</summary>
    [<RequireQualifiedAccess>]
    module Workflow =

        /// <summary>Computation expression for the given monadic type.</summary>
        type ReaderKBuilder =
            new: unit -> ReaderKBuilder
            member Return: x: 'a -> ReaderK<'e, 'x, ^a>
            member ReturnFrom: m: ReaderK<'e, 'x, 'a> -> ReaderK< ^e, ^x, ^a>
            member Zero: unit -> ReaderK<'e, 'x, unit>
            member inline Bind: m: ReaderK< ^e, ^x, ^a> * f: (^a -> ReaderK< ^e, ^x, ^b>) -> ReaderK< ^e, ^x, ^b>            


    /// <summary>Computation expression instance for the given type.</summary>
    val readerK: Workflow.ReaderKBuilder


// Kleisli

    /// <summary>Lift a value of the underlying monad into its Kleisli form.</summary>
    val lift: m: Reader<'e, 'a> -> ReaderK< ^e, '``_``, ^a>
    
    /// <summary>Lift a monad momorphism of the base monad into its Kleisli form.</summary>
    val inline hoist: trans: (Reader< ^e, ^a> -> Reader< ^e, ^b>) -> m: ReaderK< ^e, ^x, ^a> -> ReaderK< ^e, ^x, ^b>

    /// <summary>Map a monad producing function across an existing monad in its own Kleisli category.</summary>
    val inline mapM: f: (^a -> Reader< ^e, ^b>) -> m: ReaderK< ^e, ^x, ^a> -> ReaderK< ^e, ^x, ^b>

    /// <summary>Acts as a monadic bind on the Kleisli type using the value(s) from the underlying monad.</summary>
    val inline bindM: f: (^a -> ReaderK< ^e, ^x, ^b>) -> m: Reader< ^e, ^a> -> ReaderK< ^e, ^x, ^b>


// Cat

    /// <summary>Identity element of a category.</summary>
    val identity<'e, 'a> : ReaderK< ^e, ^a, ^a>

    /// <summary>Compose two members of a category together.</summary>
    val inline compose: o2: ReaderK< ^e, ^b, ^c> -> o1: ReaderK< ^e, ^a, ^b> -> ReaderK< ^e, ^a, ^c>


// Arrow

    /// <summary>Lift a function to an arrow.</summary>
    val inline arr: f: (^a -> ^b) -> ReaderK< ^e, ^a, ^b>

    /// <summary>Send the first component of the input through the argument arrow, and copy the rest unchanged to the output.</summary>
    val inline first: ar: ReaderK< ^e, ^a, ^b> -> ReaderK< ^e, ^a * ^c, ^b * ^c>

    /// <summary>Send the second component of the input through the argument arrow, and copy the rest unchanged to the output.</summary>
    val inline second: ar: ReaderK< ^e, ^a, ^b> -> ReaderK< ^e, ^c * ^a, ^c * ^b>

    /// <summary>Split the input between the two argument arrows and combine their output.</summary>
    val inline split: a2: ReaderK< ^e, ^c, ^d> -> a1: ReaderK< ^e, ^a, ^b> -> ReaderK< ^e, ^a * ^c, ^b * ^d>

    /// <summary>Fanout: send the input to both argument arrows and combine their output.</summary>
    val inline fanout: a2: ReaderK< ^e, ^a, ^c> -> a1: ReaderK< ^e, ^a, ^b> -> ReaderK< ^e, ^a, ^b * ^c>


// Arrow.Choice

    open Rogz.Context.Data.Either

    /// <summary>Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. A mirror of 'feedr'.</summary>
    val inline feedl: ar: ReaderK< ^e, ^a, ^b> -> ReaderK< ^e, Either< ^a, ^c>, Either< ^b, ^c>>

    /// <summary>Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. A mirror of 'feedl'.</summary>
    val inline feedr: ar: ReaderK< ^e, ^a, ^b> -> ReaderK< ^e, Either< ^c, ^a>, Either< ^c, ^b>>

    /// <summary>Split the input between the two argument arrows, retagging and merging their outputs.</summary>
    val inline merge: a2: ReaderK< ^e, ^c, ^d> -> a1: ReaderK< ^e, ^a, ^b> -> ReaderK< ^e, Either< ^a, ^c>, Either< ^b, ^d>>

    /// <summary>Split the input between the two argument arrows and merge their outputs.</summary>
    val inline fanin: a2: ReaderK< ^e, ^c, ^b> -> a1: ReaderK< ^e, ^a, ^b> -> ReaderK< ^e, Either< ^a, ^c>, ^b>