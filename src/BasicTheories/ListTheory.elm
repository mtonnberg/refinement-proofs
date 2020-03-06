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


{-| A non-empty list
-}
type NonEmptyList
    = NonEmptyList


{-| A sorted list
-}
type SortedList
    = SortedList


{-| Prove that a list is non-empty
-}
proveNonEmptyList : List a -> Maybe (Proven (List a) NonEmptyList)
proveNonEmptyList x =
    if List.length x > 0 then
        Just <| axiom NonEmptyList x

    else
        Nothing


{-| Make a sorted list.
-}
mkSortedList : List comparable -> Proven (List comparable) SortedList
mkSortedList =
    axiom SortedList << List.sort


{-| Safely get a head of a non-empty list
-}
head : Proven (List a) NonEmptyList -> a
head xs =
    case exorcise xs of
        [] ->
            absurd

        x :: _ ->
            x


{-| Map over a non-empty list
-}
nonEmptyListMap : (a -> b) -> Proven (List a) NonEmptyList -> Proven (List b) NonEmptyList
nonEmptyListMap f xs =
    axiom NonEmptyList <| List.map f <| exorcise xs


{-| Get the length of a non-empty list
-}
lengthOfNonEmptyList : Proven (List a) NonEmptyList -> Proven Int Positive
lengthOfNonEmptyList =
    Maybe.withDefault absurd << provePositive << List.length << exorcise
