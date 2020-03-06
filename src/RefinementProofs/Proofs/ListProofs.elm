module RefinementProofs.Proofs.ListProofs exposing
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

import RefinementProofs.Proofs.NumberProofs
    exposing
        ( Positive
        , provePositive
        )
import RefinementProofs.Theory
    exposing
        ( Proven(..)
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
            Debug.todo "absurd" 

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
lengthOfNonEmptyList x =
    case provePositive << List.length <| exorcise x of
        Just p -> p
        Nothing -> Debug.todo "absurd" 
