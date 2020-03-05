module BasicTheories.ListTheory exposing
    ( NonEmptyList
    , SortedList
    , head
    , proveNonEmptyList
    , lengthOfNonEmptyList
    , mkSortedList
    , nonEmptyListMap
    )

{-| Some basic list proofs

# Definition
@docs NonEmptyList, SortedList

# Proofs
@docs proveNonEmptyList,mkSortedList

# Functions
@docs head, nonEmptyListMap

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


type NonEmptyList
    = NonEmptyList


type SortedList
    = SortedList


proveNonEmptyList : List a -> Maybe (Proven (List a) NonEmptyList)
proveNonEmptyList x =
    if List.length x > 0 then
        Just <| axiom NonEmptyList x

    else
        Nothing


mkSortedList : List comparable -> Proven (List comparable) SortedList
mkSortedList =
    axiom SortedList << List.sort


head : Proven (List a) NonEmptyList -> a
head xs =
    case exorcise xs of
        [] ->
            absurd

        x :: _ ->
            x

nonEmptyListMap : (a -> b) -> Proven (List a) NonEmptyList -> Proven (List b) NonEmptyList
nonEmptyListMap f xs = axiom NonEmptyList <| List.map f <| exorcise xs

lengthOfNonEmptyList : Proven (List a) NonEmptyList -> Proven Int Positive
lengthOfNonEmptyList =
    Maybe.withDefault absurd << provePositive << List.length << exorcise
