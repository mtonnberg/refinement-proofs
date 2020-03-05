module BasicTheories.NumberTheory exposing
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

# Functions
@docs head, nonEmptyListMap

-}
import RefinementProofs
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


{-| Basic logic
-}
type Positive
    = Positive


{-| Basic logic
-}
type Zero
    = Zero


{-| Basic logic
-}
type alias ZeroOrGreater =
    Or Zero Positive


{-| Basic logic
-}
type alias Negative =
    Not ZeroOrGreater


{-| Basic logic
-}
type Even
    = Even


{-| Basic logic
-}
type alias Odd =
    Not Even


{-| Basic logic
-}
proveZero : number -> Maybe (Proven number Zero)
proveZero x =
    if x == 0 then
        Just <| axiom Zero x

    else
        Nothing


{-| Basic logic
-}
provePositive : number -> Maybe (Proven number Positive)
provePositive x =
    if x > 0 then
        Just <| axiom Positive x

    else
        Nothing


{-| Basic logic
-}
proveZeroOrGreater : number -> Maybe (Proven number ZeroOrGreater)
proveZeroOrGreater =
    makeOr proveZero provePositive


{-| Basic logic
-}
proveNegative : number -> Maybe (Proven number Negative)
proveNegative =
    inverse proveZeroOrGreater


{-| Basic logic
-}
proveEven : Int -> Maybe (Proven Int Even)
proveEven x =
    if modBy 2 x == 0 then
        Just <| axiom Even x

    else
        Nothing


{-| Basic logic
-}
proveOdd : Int -> Maybe (Proven Int Odd)
proveOdd =
    inverse proveEven


{-| Basic logic
-}
allPositivesAreZeroOrGreater : Implies Positive ZeroOrGreater
allPositivesAreZeroOrGreater =
    imply Positive (or Zero Positive)
