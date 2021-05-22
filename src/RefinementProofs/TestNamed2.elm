module RefinementProofs.TestNamed2 exposing (getBrandToPromote)

import Dict exposing (Dict)
import RefinementProofs.Proofs.StringProofs exposing (NonEmptyString, TrimmedString, proveNonEmptyString, proveTrimmedString)
import RefinementProofs.TestNamed exposing (IsInDict, Positive, proveKeyIsInDict, provePositive2, takeValueFromDict2)
import RefinementProofs.WithKnowledge
    exposing
        ( A
        , And
        , NoDomainKnowledge
        , NoNamedKnowledge
        , Proof
        , WithKnowledge
        , axiomaticallyAddDomainKnowledge
        , attachNamedKnowledge
        , axiomaticValueKnowledge
        , d_andIsFlippable
        , d_since
        , forget
        , forgetNamedKnowledge
        , name2
        , raw
        , axiomaticallySetDomainKnowledge
        , setNamedKnowledge
        , the
        , v_makeAnd
        , withName
        )


getBrandToPromote :
    Dict Int String
    -> Int
    -> Maybe (WithKnowledge String (And TrimmedString NonEmptyString) (And ASupportedCarBrand ATopTierBrandToday) NoNamedKnowledge)
getBrandToPromote dic k =
    name2 dic k handle


type ASupportedCarBrand
    = ASupportedCarBrand


type ATopTierBrandToday
    = ATopTierBrandToday


carDictToDomainKnowledge :
    A (Dict Int String) dict
    -> A Int key
    -> WithKnowledge String v d (IsInDict key dict)
    -> WithKnowledge String v (And d ASupportedCarBrand) NoNamedKnowledge
carDictToDomainKnowledge _ _ r =
    axiomaticallyAddDomainKnowledge ASupportedCarBrand <|
        forgetNamedKnowledge r


handle :
    A (Dict Int String) dict
    -> A Int key
    -> Maybe (WithKnowledge String (And TrimmedString NonEmptyString) (And ASupportedCarBrand ATopTierBrandToday) NoNamedKnowledge)
handle namedDict namedWantedKey =
    Maybe.map (d_since d_andIsFlippable << carDictToDomainKnowledge namedDict namedWantedKey) <| handleInner namedDict namedWantedKey


handleInner :
    A (Dict Int String) dict
    -> A Int key
    -> Maybe (WithKnowledge String (And TrimmedString NonEmptyString) ATopTierBrandToday (IsInDict key dict))
handleInner namedDict namedWantedKey =
    case withName provePositive2 namedWantedKey of
        Nothing ->
            Nothing

        Just namedPositive ->
            case proveKeyIsInDict (forget namedPositive) namedDict of
                Nothing ->
                    Nothing

                Just isInDictProof ->
                    let
                        keyWithAllNeededProofs : WithKnowledge (A Int key) Positive NoDomainKnowledge (IsInDict key dict)
                        keyWithAllNeededProofs =
                            setNamedKnowledge namedPositive isInDictProof

                        stringCandidate =
                            takeValueFromDict2 keyWithAllNeededProofs namedDict
                    in
                    case v_makeAnd proveTrimmedString proveNonEmptyString stringCandidate of
                        Nothing ->
                            Nothing

                        Just s ->
                            let
                                topTierBrands =
                                    [ "Toyota", "CoolCar 3000" ]
                            in
                            if List.member stringCandidate topTierBrands then
                                Just <|
                                    axiomaticallySetDomainKnowledge ATopTierBrandToday <|
                                        setNamedKnowledge s isInDictProof

                            else
                                Nothing
