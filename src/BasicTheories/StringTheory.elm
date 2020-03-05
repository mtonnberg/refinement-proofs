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
@docs lengthOfNonEmptyString, nonEmptyListMap

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


{-| Basic logic
-}
type NonEmptyString
    = NonEmptyString


{-| Basic logic
-}
type TrimmedString
    = TrimmedString


{-| Basic logic
-}
type alias NonEmptyTrimmedString =
    And NonEmptyString TrimmedString


{-| Basic logic
-}
proveNonEmptyString : String -> Maybe (Proven String NonEmptyString)
proveNonEmptyString x =
    if String.length x > 0 then
        Just <| axiom NonEmptyString x

    else
        Nothing


{-| Basic logic
-}
mkTrimmedString : String -> Proven String TrimmedString
mkTrimmedString =
    axiom TrimmedString << String.trim


{-| Basic logic
-}
proveTrimmedString : String -> Maybe (Proven String TrimmedString)
proveTrimmedString x =
    if String.trim x == x then
        Just <| axiom TrimmedString x

    else
        Nothing


{-| Basic logic
-}
proveNonEmptyTrimmedString : String -> Maybe (Proven String NonEmptyTrimmedString)
proveNonEmptyTrimmedString =
    makeAnd proveNonEmptyString proveTrimmedString


{-| Basic logic
-}
lengthOfNonEmptyString : Proven String NonEmptyString -> Proven Int Positive
lengthOfNonEmptyString =
    Maybe.withDefault absurd << provePositive << String.length << exorcise
