namespace Rogz.Context.Kleisli.Cont

open Rogz.Context.Data.Cont


/// <summary>The Cont type lifted into Kleisli computations.</summary>
[<Struct; NoComparison; NoEquality>]
type ContK<'R, 'X, 'T> = ContK of ('X -> Cont<'R, 'T>)
with

    /// <summary>Run the function with the supplied input.</summary>
    member inline Invoke: input: ^X -> Cont< ^R, ^T>

    /// <summary>Lift a function onto a context.</summary>
    member inline Select: f: System.Func< ^T, ^U> -> ContK< ^R, ^X, ^U>

    /// <summary>Lift a binary function onto contexts.</summary>
    member inline Zip: other: ContK< ^R, ^X, ^U> * mapping: System.Func< ^T, ^U, ^V> -> ContK< ^R, ^X, ^V>

    /// <summary>Lift a binary function onto contexts.</summary>
    member inline Join:
        other: ContK< ^R, ^X, ^U> *
        kt: System.Func< ^T, int> *
        ku: System.Func< ^U, int> *
        mapping: System.Func< ^T, ^U, ^V> -> ContK< ^R, ^X, ^V>

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    member inline SelectMany: projection: System.Func< ^T, ContK< ^R, ^X, ^U>> -> ContK< ^R, ^X, ^U>

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    member inline SelectMany:
        projection: System.Func< ^T, ContK< ^R, ^X, ^U>> *
        mapping: System.Func< ^T, ^U, ^V> -> ContK< ^R, ^X, ^V>


/// <summary>Operations on ContK's.</summary>
module ContK =

// Primitives

    /// <summary>Run the function with the supplied input.</summary>
    val inline runKleisli: input: ^x -> k: ContK< ^r, ^x, ^a> -> Cont< ^r, ^a>


// Functor

    /// <summary>Lift a function onto a context.</summary>
    val inline map: f: (^a -> ^b) -> fa: ContK< ^r, ^x, ^a> -> ContK< ^r, ^x, ^b>


// Profunctor

    /// <summary>Map over both arguments at the same time, the first (i.e. 'left') contravariantly and the second (i.e. 'right') covariantly.</summary>
    val inline dimap: f: (^c -> ^a) -> g: (^b -> ^d) -> pf: ContK< ^r, ^a, ^b> -> ContK< ^r, ^c, ^d>

    /// <summary>Map the first (i.e. 'left') argument contravariantly.</summary>
    val inline mapl: f: (^c -> ^a) -> pf: ContK< ^r, ^a, ^b> -> ContK< ^r, ^c, ^b>


// Applicative

    /// <summary>Lift a value into a context.</summary>
    val unit: value: 'a -> ContK<'r, 'x, ^a>

    /// <summary>Lift a binary function onto contexts.</summary>
    val inline map2: f: (^a -> ^b -> ^c) -> fa: ContK< ^r, ^x, ^a> -> fb: ContK< ^r, ^x, ^b> -> ContK< ^r, ^x, ^c>


// Monad

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline bind: f: (^a -> ContK< ^r, ^x, ^b>) -> m: ContK< ^r, ^x, ^a> -> ContK< ^r, ^x, ^b>


    /// <summary>Computation expression / monadic-workflow type and operations for the given context.</summary>
    [<RequireQualifiedAccess>]
    module Workflow =

        /// <summary>Computation expression for the given monadic type.</summary>
        type ContKBuilder =
            new: unit -> ContKBuilder
            member Return: x: 'a -> ContK<'r, 'x, ^a>
            member ReturnFrom: m: ContK<'r, 'x, 'a> -> ContK< ^r, ^x, ^a>
            member Zero: unit -> ContK<'r, 'x, unit>
            member inline Bind: m: ContK< ^r, ^x, ^a> * f: (^a -> ContK< ^r, ^x, ^b>) -> ContK< ^r, ^x, ^b>            


    /// <summary>Computation expression instance for the given type.</summary>
    val contK: Workflow.ContKBuilder


// Kleisli

    /// <summary>Lift a value of the underlying monad into its Kleisli form.</summary>
    val lift: m: Cont<'r, 'a> -> ContK< ^r, '``_``, ^a>
    
    /// <summary>Lift a monad momorphism of the base monad into its Kleisli form.</summary>
    val inline hoist: trans: (Cont< ^r, ^a> -> Cont< ^r, ^b>) -> m: ContK< ^r, ^x, ^a> -> ContK< ^r, ^x, ^b>

    /// <summary>Map a monad producing function across an existing monad in its own Kleisli category.</summary>
    val inline mapM: f: (^a -> Cont< ^r, ^b>) -> m: ContK< ^r, ^x, ^a> -> ContK< ^r, ^x, ^b>

    /// <summary>Acts as a monadic bind on the Kleisli type using the value(s) from the underlying monad.</summary>
    val inline bindM: f: (^a -> ContK< ^r, ^x, ^b>) -> m: Cont< ^r, ^a> -> ContK< ^r, ^x, ^b>


// Cat

    /// <summary>Identity element of a category.</summary>
    val identity<'r, 'a> : ContK< ^r, ^a, ^a>

    /// <summary>Compose two members of a category together.</summary>
    val inline compose: o2: ContK< ^r, ^b, ^c> -> o1: ContK< ^r, ^a, ^b> -> ContK< ^r, ^a, ^c>


// Arrow

    /// <summary>Lift a function to an arrow.</summary>
    val inline arr: f: (^a -> ^b) -> ContK< ^r, ^a, ^b>

    /// <summary>Send the first component of the input through the argument arrow, and copy the rest unchanged to the output.</summary>
    val inline first: ar: ContK< ^r, ^a, ^b> -> ContK< ^r, ^a * ^c, ^b * ^c>

    /// <summary>Send the second component of the input through the argument arrow, and copy the rest unchanged to the output.</summary>
    val inline second: ar: ContK< ^r, ^a, ^b> -> ContK< ^r, ^c * ^a, ^c * ^b>

    /// <summary>Split the input between the two argument arrows and combine their output.</summary>
    val inline split: a2: ContK< ^r, ^c, ^d> -> a1: ContK< ^r, ^a, ^b> -> ContK< ^r, ^a * ^c, ^b * ^d>

    /// <summary>Fanout: send the input to both argument arrows and combine their output.</summary>
    val inline fanout: a2: ContK< ^r, ^a, ^c> -> a1: ContK< ^r, ^a, ^b> -> ContK< ^r, ^a, ^b * ^c>


// Arrow.Choice

    open Rogz.Context.Data.Either

    /// <summary>Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. A mirror of 'feedr'.</summary>
    val inline feedl: ar: ContK< ^r, ^a, ^b> -> ContK< ^r, Either< ^a, ^c>, Either< ^b, ^c>>

    /// <summary>Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. A mirror of 'feedl'.</summary>
    val inline feedr: ar: ContK< ^r, ^a, ^b> -> ContK< ^r, Either< ^c, ^a>, Either< ^c, ^b>>

    /// <summary>Split the input between the two argument arrows, retagging and merging their outputs.</summary>
    val inline merge: a2: ContK< ^r, ^c, ^d> -> a1: ContK< ^r, ^a, ^b> -> ContK< ^r, Either< ^a, ^c>, Either< ^b, ^d>>

    /// <summary>Split the input between the two argument arrows and merge their outputs.</summary>
    val inline fanin: a2: ContK< ^r, ^c, ^b> -> a1: ContK< ^r, ^a, ^b> -> ContK< ^r, Either< ^a, ^c>, ^b>