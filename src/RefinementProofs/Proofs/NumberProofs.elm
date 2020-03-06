module RefinementProofs.Proofs.NumberProofs exposing
    ( Even
    , Negative
    , Odd
    , Positive
    , Zero
    , ZeroOrGreater
    , allPositivesAreZeroOrGreater
    , proveEven
    , proveNegative
    , proveOdd
    , provePositive
    , proveZeroOrGreater
    )

{-| Some basic number proofs

# Definition
@docs Even
    , Negative
    , Odd
    , Positive
    , Zero
    , ZeroOrGreater

# Proofs
@docs proveEven
    , proveNegative
    , proveOdd
    , provePositive
    , proveZeroOrGreater

# Implications
@docs allPositivesAreZeroOrGreater
-}
import RefinementProofs.Theory
    exposing
        ( Implies
        , Not
        , Or
        , Proven(..)
        , axiom
        , inverse
        , makeOr
        , imply
        , or
        )


{-| A positive number
-}
type Positive
    = Positive


{-| A number that is Zero
-}
type Zero
    = Zero


{-| type alias for `Or Zero Positive`. If used with an Int this the number is 0,1,2,3,....
-}
type alias ZeroOrGreater =
    Or Zero Positive


{-| A negative number. Alias for `Not ZeroOrGreater`
-}
type alias Negative =
    Not ZeroOrGreater


{-| An even Int
-}
type Even
    = Even


{-| alias for `Not Even`
-}
type alias Odd =
    Not Even


{-| Prove that a number equals to zero
-}
proveZero : number -> Maybe (Proven number Zero)
proveZero x =
    if x == 0 then
        Just <| axiom Zero x

    else
        Nothing


{-| Prove that a number is positive (>0)
-}
provePositive : number -> Maybe (Proven number Positive)
provePositive x =
    if x > 0 then
        Just <| axiom Positive x

    else
        Nothing


{-| Prove that a number is either zero or greater
-}
proveZeroOrGreater : number -> Maybe (Proven number ZeroOrGreater)
proveZeroOrGreater =
    makeOr proveZero provePositive


{-| Prove a number is negative
-}
proveNegative : number -> Maybe (Proven number Negative)
proveNegative =
    inverse (or Zero Positive) proveZeroOrGreater


{-| Prove that a number is even
-}
proveEven : Int -> Maybe (Proven Int Even)
proveEven x =
    if modBy 2 x == 0 then
        Just <| axiom Even x

    else
        Nothing


{-| Prove that a number is odd
-}
proveOdd : Int -> Maybe (Proven Int Odd)
proveOdd =
    inverse Even proveEven


{-| A simple implication.
    Note this is strictly not needed since this implication is clear in the types - `ZeroOrGreater = Or Zero Positive`.
    An alternative is to use `introOrL` or `or`.
-}
allPositivesAreZeroOrGreater : Implies Positive ZeroOrGreater
allPositivesAreZeroOrGreater =
    imply Positive (or Zero Positive)
