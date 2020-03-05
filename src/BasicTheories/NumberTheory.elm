module BasicTheories.NumberTheory exposing
    ( Even
    , Negative
    , Odd
    , Positive
    , Zero
    , ZeroOrGreater
    , allPositiveAreNatural
    , proveEven
    , proveNegative
    , proveOdd
    , provePositive
    , proveZeroOrGreater
    )

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


type Positive
    = Positive


type Zero
    = Zero


type alias ZeroOrGreater =
    Or Zero Positive


type alias Negative =
    Not ZeroOrGreater


type Even
    = Even


type alias Odd =
    Not Even


proveZero : number -> Maybe (Proven number Zero)
proveZero x =
    if x == 0 then
        Just <| axiom Zero x

    else
        Nothing


provePositive : number -> Maybe (Proven number Positive)
provePositive x =
    if x > 0 then
        Just <| axiom Positive x

    else
        Nothing


proveZeroOrGreater : number -> Maybe (Proven number ZeroOrGreater)
proveZeroOrGreater =
    makeOr proveZero provePositive


proveNegative : number -> Maybe (Proven number Negative)
proveNegative =
    inverse proveZeroOrGreater


proveEven : Int -> Maybe (Proven Int Even)
proveEven x =
    if modBy 2 x == 0 then
        Just <| axiom Even x

    else
        Nothing


proveOdd : Int -> Maybe (Proven Int Odd)
proveOdd =
    inverse proveEven


allPositiveAreNatural : Implies Positive ZeroOrGreater
allPositiveAreNatural =
    imply Positive (or Zero Positive)
