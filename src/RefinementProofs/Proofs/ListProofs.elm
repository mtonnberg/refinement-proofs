module RefinementProofs.Proofs.ListProofs exposing
    ( NonEmptyList, SortedList
    , proveNonEmptyList, mkSortedList
    , head, nonEmptyListMap, lengthOfNonEmptyList
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
import RefinementProofs.WithKnowledge
    exposing
        ( A
        , NoKnowledge
        , Proof
        , WithKnowledge(..)
        , axiomaticValueKnowledge
        , forget
        , raw
        , the
        )


{-| A named non-empty list
-}
type IsNonEmptyList list
    = IsNonEmptyList


{-| An unnamed non-empty list
-}
type NonEmptyList
    = NonEmptyList


{-| An unnamed sorted list
-}
type SortedList
    = SortedList


{-| Prove that a list is non-empty
-}
proveNonEmptyList : List a -> Maybe (WithKnowledge (List a) NonEmptyList NoKnowledge NoKnowledge)
proveNonEmptyList x =
    if List.length x > 0 then
        Just <| axiomaticValueKnowledge NonEmptyList x

    else
        Nothing


{-| Make a sorted list.
-}
mkSortedList : List comparable -> WithKnowledge (List comparable) SortedList NoKnowledge NoKnowledge
mkSortedList =
    axiomaticValueKnowledge SortedList << List.sort


{-| Safely get a head of a non-empty list
-}
head : WithKnowledge (List a) NonEmptyList d n -> a
head xs =
    case forget xs of
        [] ->
            Debug.todo "absurd"

        x :: _ ->
            x


{-| Map over a non-empty list
-}
nonEmptyListMap : (a -> b) -> WithKnowledge (List a) NonEmptyList NoKnowledge NoKnowledge -> WithKnowledge (List b) NonEmptyList NoKnowledge NoKnowledge
nonEmptyListMap f xs =
    axiomaticValueKnowledge NonEmptyList <| List.map f <| forget xs


{-| Get the length of a non-empty list
-}
lengthOfNonEmptyList : WithKnowledge (List a) NonEmptyList d n -> WithKnowledge Int Positive NoKnowledge NoKnowledge
lengthOfNonEmptyList x =
    case provePositive << List.length <| forget x of
        Just p ->
            p

        Nothing ->
            Debug.todo "absurd"
