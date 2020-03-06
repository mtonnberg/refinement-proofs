module BasicTheories.StringTheory exposing
    ( NonEmptyString
    , NonEmptyTrimmedString
    , lengthOfNonEmptyString
    , mkTrimmedString
    , proveNonEmptyString
    , proveNonEmptyTrimmedString
    , proveTrimmedString
    )

{-| Some basic string proofs

# Definition
@docs NonEmptyString
    , NonEmptyTrimmedString

# Proofs
@docs mkTrimmedString
    , proveNonEmptyString
    , proveNonEmptyTrimmedString
    , proveTrimmedString

# Functions
@docs lengthOfNonEmptyString

-}
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


{-| A non-empty string
-}
type NonEmptyString
    = NonEmptyString


{-| A Trimmed string
-}
type TrimmedString
    = TrimmedString


{-| A string that are both nonEmpty and trimmed, alias for "And NonEmptyString TrimmedString"
-}
type alias NonEmptyTrimmedString =
    And NonEmptyString TrimmedString


{-| Prove that a string is non-empty
-}
proveNonEmptyString : String -> Maybe (Proven String NonEmptyString)
proveNonEmptyString x =
    if String.length x > 0 then
        Just <| axiom NonEmptyString x

    else
        Nothing


{-| Make a trimmed string from a string. Note: will trim a non-trimmed string
-}
mkTrimmedString : String -> Proven String TrimmedString
mkTrimmedString =
    axiom TrimmedString << String.trim


{-| Prove that a string is trimmed
-}
proveTrimmedString : String -> Maybe (Proven String TrimmedString)
proveTrimmedString x =
    if String.trim x == x then
        Just <| axiom TrimmedString x

    else
        Nothing


{-| Prove that a string is both non-empty and trimmed
-}
proveNonEmptyTrimmedString : String -> Maybe (Proven String NonEmptyTrimmedString)
proveNonEmptyTrimmedString =
    makeAnd proveNonEmptyString proveTrimmedString


{-| Get the length of a non-empty string
-}
lengthOfNonEmptyString : Proven String NonEmptyString -> Proven Int Positive
lengthOfNonEmptyString =
    Maybe.withDefault absurd << provePositive << String.length << exorcise
