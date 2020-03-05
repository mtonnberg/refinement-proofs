module BasicTheories.ListTheory exposing
    ( NonEmptyList, SortedList
    , proveNonEmptyList, mkSortedList
    , head, nonEmptyListMap
    , lengthOfNonEmptyList
    )

{-| Some basic list proofs


# Definition

@docs NonEmptyList, SortedList


# Proofs

@docs proveNonEmptyList, mkSortedList


# Functions

@docs head, nonEmptyListMap, lengthOfNonEmptyList

-}

import BasicTheories.NumberTheory
    exposing
        ( Positive
        , provePositive
        )
import RefinementProofs
    exposing
        ( Proven(..)
        , absurd
        , axiom
        , exorcise
        )


{-| Basic logic
-}
type NonEmptyList
    = NonEmptyList


{-| Basic logic
-}
type SortedList
    = SortedList


{-| Basic logic
-}
proveNonEmptyList : List a -> Maybe (Proven (List a) NonEmptyList)
proveNonEmptyList x =
    if List.length x > 0 then
        Just <| axiom NonEmptyList x

    else
        Nothing


{-| Basic logic
-}
mkSortedList : List comparable -> Proven (List comparable) SortedList
mkSortedList =
    axiom SortedList << List.sort


{-| Basic logic
-}
head : Proven (List a) NonEmptyList -> a
head xs =
    case exorcise xs of
        [] ->
            absurd

        x :: _ ->
            x


{-| Basic logic
-}
nonEmptyListMap : (a -> b) -> Proven (List a) NonEmptyList -> Proven (List b) NonEmptyList
nonEmptyListMap f xs =
    axiom NonEmptyList <| List.map f <| exorcise xs


{-| Basic logic
-}
lengthOfNonEmptyList : Proven (List a) NonEmptyList -> Proven Int Positive
lengthOfNonEmptyList =
    Maybe.withDefault absurd << provePositive << List.length << exorcise
