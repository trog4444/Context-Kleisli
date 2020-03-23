namespace Rogz.Context.Kleisli.Either

open Rogz.Context.Data.Either


[<Struct; NoComparison; NoEquality>]
type EitherK<'E, 'X, 'T> = EitherK of ('X -> Either<'E, 'T>)