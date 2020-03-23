namespace Rogz.Context.Kleisli.State

open Rogz.Context.Data.State


/// <summary>The State type lifted into Kleisli computations.</summary>
[<Struct; NoComparison; NoEquality>]
type StateK<'S, 'X, 'T> = StateK of ('X -> State<'S, 'T>)
with

    /// <summary>Run the function with the supplied input.</summary>
    member inline Invoke: input: ^X -> State< ^S, ^T>

    /// <summary>Lift a function onto a context.</summary>
    member inline Select: f: System.Func< ^T, ^U> -> StateK< ^S, ^X, ^U>

    /// <summary>Lift a binary function onto contexts.</summary>
    member inline Zip: other: StateK< ^S, ^X, ^U> * mapping: System.Func< ^T, ^U, ^V> -> StateK< ^S, ^X, ^V>

    /// <summary>Lift a binary function onto contexts.</summary>
    member inline Join:
        other: StateK< ^S, ^X, ^U> *
        kt: System.Func< ^T, int> *
        ku: System.Func< ^U, int> *
        mapping: System.Func< ^T, ^U, ^V> -> StateK< ^S, ^X, ^V>

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    member inline SelectMany: projection: System.Func< ^T, StateK< ^S, ^X, ^U>> -> StateK< ^S, ^X, ^U>

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    member inline SelectMany:
        projection: System.Func< ^T, StateK< ^S, ^X, ^U>> *
        mapping: System.Func< ^T, ^U, ^V> -> StateK< ^S, ^X, ^V>


/// <summary>Operations on StateK's.</summary>
module StateK =

// Primitives

    /// <summary>Run the function with the supplied input.</summary>
    val inline runKleisli: input: ^x -> k: StateK< ^s, ^x, ^a> -> State< ^s, ^a>


// Functor

    /// <summary>Lift a function onto a context.</summary>
    val inline map: f: (^a -> ^b) -> fa: StateK< ^s, ^x, ^a> -> StateK< ^s, ^x, ^b>


// Profunctor

    /// <summary>Map over both arguments at the same time, the first (i.e. 'left') contravariantly and the second (i.e. 'sight') covariantly.</summary>
    val inline dimap: f: (^c -> ^a) -> g: (^b -> ^d) -> pf: StateK< ^s, ^a, ^b> -> StateK< ^s, ^c, ^d>

    /// <summary>Map the first (i.e. 'left') argument contravariantly.</summary>
    val inline mapl: f: (^c -> ^a) -> pf: StateK< ^s, ^a, ^b> -> StateK< ^s, ^c, ^b>


// Applicative

    /// <summary>Lift a value into a context.</summary>
    val unit: value: 'a -> StateK<'s, 'x, ^a>

    /// <summary>Lift a binary function onto contexts.</summary>
    val inline map2: f: (^a -> ^b -> ^c) -> fa: StateK< ^s, ^x, ^a> -> fb: StateK< ^s, ^x, ^b> -> StateK< ^s, ^x, ^c>


// Monad

    /// <summary>Sequentially compose two contexts, passing any value produced by the first as an argument to the second.</summary>
    val inline bind: f: (^a -> StateK< ^s, ^x, ^b>) -> m: StateK< ^s, ^x, ^a> -> StateK< ^s, ^x, ^b>


    /// <summary>Computation expression / monadic-workflow type and operations for the given context.</summary>
    [<RequireQualifiedAccess>]
    module Workflow =

        /// <summary>Computation expression for the given monadic type.</summary>
        type StateKBuilder =
            new: unit -> StateKBuilder
            member Return: x: 'a -> StateK<'s, 'x, ^a>
            member ReturnFrom: m: StateK<'s, 'x, 'a> -> StateK< ^s, ^x, ^a>
            member Zero: unit -> StateK<'s, 'x, unit>
            member inline Bind: m: StateK< ^s, ^x, ^a> * f: (^a -> StateK< ^s, ^x, ^b>) -> StateK< ^s, ^x, ^b>


    /// <summary>Computation expression instance for the given type.</summary>
    val stateK: Workflow.StateKBuilder


// Kleisli

    /// <summary>Lift a value of the underlying monad into its Kleisli form.</summary>
    val lift: m: State<'s, 'a> -> StateK< ^s, '``_``, ^a>
    
    /// <summary>Lift a monad momorphism of the base monad into its Kleisli form.</summary>
    val inline hoist: trans: (State< ^s, ^a> -> State< ^s, ^b>) -> m: StateK< ^s, ^x, ^a> -> StateK< ^s, ^x, ^b>

    /// <summary>Map a monad producing function across an existing monad in its own Kleisli category.</summary>
    val inline mapM: f: (^a -> State< ^s, ^b>) -> m: StateK< ^s, ^x, ^a> -> StateK< ^s, ^x, ^b>

    /// <summary>Acts as a monadic bind on the Kleisli type using the value(s) from the underlying monad.</summary>
    val inline bindM: f: (^a -> StateK< ^s, ^x, ^b>) -> m: State< ^s, ^a> -> StateK< ^s, ^x, ^b>


// Cat

    /// <summary>Identity element of a category.</summary>
    val identity<'s, 'a> : StateK< ^s, ^a, ^a>

    /// <summary>Compose two members of a category together.</summary>
    val inline compose: o2: StateK< ^s, ^b, ^c> -> o1: StateK< ^s, ^a, ^b> -> StateK< ^s, ^a, ^c>


// Arrow

    /// <summary>Lift a function to an arrow.</summary>
    val inline arr: f: (^a -> ^b) -> StateK< ^s, ^a, ^b>

    /// <summary>Send the first component of the input through the argument arrow, and copy the rest unchanged to the output.</summary>
    val inline first: ar: StateK< ^s, ^a, ^b> -> StateK< ^s, ^a * ^c, ^b * ^c>

    /// <summary>Send the second component of the input through the argument arrow, and copy the rest unchanged to the output.</summary>
    val inline second: ar: StateK< ^s, ^a, ^b> -> StateK< ^s, ^c * ^a, ^c * ^b>

    /// <summary>Split the input between the two argument arrows and combine their output.</summary>
    val inline split: a2: StateK< ^s, ^c, ^d> -> a1: StateK< ^s, ^a, ^b> -> StateK< ^s, ^a * ^c, ^b * ^d>

    /// <summary>Fanout: send the input to both argument arrows and combine their output.</summary>
    val inline fanout: a2: StateK< ^s, ^a, ^c> -> a1: StateK< ^s, ^a, ^b> -> StateK< ^s, ^a, ^b * ^c>


// Arrow.Choice

    open Rogz.Context.Data.Either

    /// <summary>Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. A mirror of 'feedr'.</summary>
    val inline feedl: ar: StateK< ^s, ^a, ^b> -> StateK< ^s, Either< ^a, ^c>, Either< ^b, ^c>>

    /// <summary>Feed marked inputs through the argument arrow, passing the rest through unchanged to the output. A mirror of 'feedl'.</summary>
    val inline feedr: ar: StateK< ^s, ^a, ^b> -> StateK< ^s, Either< ^c, ^a>, Either< ^c, ^b>>

    /// <summary>Split the input between the two argument arrows, retagging and merging their outputs.</summary>
    val inline merge: a2: StateK< ^s, ^c, ^d> -> a1: StateK< ^s, ^a, ^b> -> StateK< ^s, Either< ^a, ^c>, Either< ^b, ^d>>

    /// <summary>Split the input between the two argument arrows and merge their outputs.</summary>
    val inline fanin: a2: StateK< ^s, ^c, ^b> -> a1: StateK< ^s, ^a, ^b> -> StateK< ^s, Either< ^a, ^c>, ^b>