module BasicTheories.StringTheory exposing
    ( NonEmptyString
    , NonEmptyTrimmedString
    , lengthOfNonEmptyString
    , mkTrimmedString
    , proveNonEmptyString
    , proveNonEmptyTrimmedString
    , proveTrimmedString
    )

import BasicTheories.NumberTheory
    exposing
        ( Positive
        , provePositive
        )
import RefinementProofs
    exposing
        ( And
        , Proven(..)
        , absurd
        , axiom
        , exorcise
        , makeAnd
        )


type NonEmptyString
    = NonEmptyString


type TrimmedString
    = TrimmedString


type alias NonEmptyTrimmedString =
    And NonEmptyString TrimmedString


proveNonEmptyString : String -> Maybe (Proven String NonEmptyString)
proveNonEmptyString x =
    if String.length x > 0 then
        Just <| axiom NonEmptyString x

    else
        Nothing


mkTrimmedString : String -> Proven String TrimmedString
mkTrimmedString =
    axiom TrimmedString << String.trim


proveTrimmedString : String -> Maybe (Proven String TrimmedString)
proveTrimmedString x =
    if String.trim x == x then
        Just <| axiom TrimmedString x

    else
        Nothing


proveNonEmptyTrimmedString : String -> Maybe (Proven String NonEmptyTrimmedString)
proveNonEmptyTrimmedString =
    makeAnd proveNonEmptyString proveTrimmedString


lengthOfNonEmptyString : Proven String NonEmptyString -> Proven Int Positive
lengthOfNonEmptyString =
    Maybe.withDefault absurd << provePositive << String.length << exorcise
