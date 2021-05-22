module RefinementProofs.Temp.TestNamed3 exposing (getBrandToPromote)

import Dict exposing (Dict)
import RefinementProofs.Knowledge
    exposing
        ( A
        , And
        , NoDomainKnowledge
        , NoNamedKnowledge
        , NoValueKnowledge
        , Not
        , Proof
        , WithKnowledge
        , andIsFlippable
        , attachNamedKnowledge
        , axiomaticValueKnowledge
        , axiomaticallyAddDomainKnowledge
        , axiomaticallySetDomainKnowledge
        , d_since
        , forget
        , forgetNamedKnowledge
        , name
        , name2
        , raw
        , setNamedKnowledge
        , the
        , v_introAnd
        , v_makeAnd
        , withName
        , withNoKnowledge
        )
import RefinementProofs.Proofs.DictProofs exposing (IsInDict, proveKeyIsInDict, takeValueFromDict)
import RefinementProofs.Proofs.NumberProofs exposing (Positive, provePositive)
import RefinementProofs.Proofs.StringProofs exposing (NonEmptyString, TrimmedString, proveNonEmptyString, proveTrimmedString)


testNaming1 : number
testNaming1 =
    let
        testDict : Dict String Int
        testDict =
            Dict.fromList [ ( "apa", 2 ), ( "bepa", 5 ) ]

        showCaseDicts : A (Dict String Int) dictName -> A String keyName -> Int
        showCaseDicts d1 v2 =
            let
                handleIntruder : A (Dict String Int) dictName -> A String keyName -> A String keyNameIntruder -> Int
                -- This will break the type safety! handleIntruder : A (Dict String Int) dictName -> A String keyName -> A String keyName -> Int
                handleIntruder d v vI =
                    case proveKeyIsInDict v d of
                        Just proofOfMembership ->
                            Debug.log "hurra" <| takeValueFromDict (setNamedKnowledge proofOfMembership <| withNoKnowledge v) d

                        -- Debug.log "protected by the typesystem :)" <| takeValueFromDict (setNamedKnowledge proofOfMembership <| withNoKnowledge vI) d --will not compile :)
                        Nothing ->
                            Debug.log "Could not prove" <| 1
            in
            -- Using lamdas here will break the type safety!
            name "asdf" (handleIntruder d1 v2)

        runDictTest =
            name2 testDict "bepa" showCaseDicts
    in
    2


testNaming2 : number
testNaming2 =
    let
        testDict : Dict String Int
        testDict =
            Dict.fromList [ ( "apa", 2 ), ( "bepa", 5 ) ]

        showCaseDicts : A (Dict String Int) dictName -> A String keyName -> Int
        showCaseDicts d1 v2 =
            let
                handleIntruder : A (Dict String Int) dictName -> A String keyName -> A String keyNameIntruder -> Int
                -- As long as this is correct we will catch errors deeper down
                handleIntruder d v vI =
                    mistakeDeepInTheCallChain d v vI
            in
            -- Using lamdas here will break the type safety!
            name "asdf" (handleIntruder d1 v2)

        runDictTest =
            name2 testDict "bepa" showCaseDicts
    in
    2



-- mistakeDeepInTheCallChain : A (Dict String Int) dictName -> A String keyName -> A String keyName -> Int -- This will not compile :)


mistakeDeepInTheCallChain : A (Dict String Int) dictName -> A String keyName -> A String keyNameIntruder -> Int
mistakeDeepInTheCallChain d v vI =
    case proveKeyIsInDict v d of
        Just proofOfMembership ->
            Debug.log "hurra" <| takeValueFromDict (setNamedKnowledge proofOfMembership <| withNoKnowledge v) d

        -- Debug.log "protected by the typesystem :)" <| takeValueFromDict (setNamedKnowledge proofOfMembership <| withNoKnowledge vI) d --will not compile :)
        Nothing ->
            Debug.log "Could not prove" <| 1


getBrandToPromote :
    Dict Int String
    -> Int
    -> Maybe (WithKnowledge String (And (And ContainsNoCurseWords TrimmedString) NonEmptyString) (And ASupportedCarBrand ATopTierBrandToday) NoNamedKnowledge)
getBrandToPromote dic k =
    name2 dic k handle


{-| The car brand is one of our supported ones
-}
type ASupportedCarBrand
    = ASupportedCarBrand


{-| The car is part of one of the top tier brands. Not that this will differ depending on the loaded cars, the user and the time of day
-}
type ATopTierBrandToday
    = ATopTierBrandToday


type ContainsNoCurseWords
    = ContainsNoCurseWords


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
    -> Maybe (WithKnowledge String (And (And ContainsNoCurseWords TrimmedString) NonEmptyString) (And ASupportedCarBrand ATopTierBrandToday) NoNamedKnowledge)
handle namedDict namedWantedKey =
    Maybe.map (d_since andIsFlippable << carDictToDomainKnowledge namedDict namedWantedKey) <| handleInner namedDict namedWantedKey


handleInner :
    A (Dict Int String) dict
    -> A Int key
    -> Maybe (WithKnowledge String (And (And ContainsNoCurseWords TrimmedString) NonEmptyString) ATopTierBrandToday (IsInDict key dict))
handleInner namedDict namedWantedKey =
    case ( withName provePositive namedWantedKey, proveKeyIsInDict namedWantedKey namedDict, withName proveNoCurseWords namedDict ) of
        ( Just namedPositive, Just isInDictProof, Just kidFriendlyDict ) ->
            let
                keyWithAllNeededProofs : WithKnowledge (A Int key) Positive NoDomainKnowledge (IsInDict key dict)
                keyWithAllNeededProofs =
                    setNamedKnowledge isInDictProof namedPositive

                stringCandidate =
                    takePositiveValueFromKidFriendlyDict keyWithAllNeededProofs kidFriendlyDict
            in
            case v_introAnd proveTrimmedString stringCandidate |> Maybe.andThen (v_introAnd proveNonEmptyString) of
                Nothing ->
                    Nothing

                Just s ->
                    let
                        topTierBrands =
                            [ "Toyota", "CoolCar 3000" ]
                    in
                    if List.member (forget stringCandidate) topTierBrands && (the namedWantedKey) < 6 then
                        Just <|
                            axiomaticallySetDomainKnowledge ATopTierBrandToday <|
                                setNamedKnowledge isInDictProof s

                    else
                        Nothing

        _ ->
            Nothing


{-| Dummy function to just test the concepts
-}
takePositiveValueFromDict :
    WithKnowledge (A comparable keyName) Positive anyDomainKnowledge (IsInDict keyName dictName)
    -> A (Dict comparable v) dictName
    -> v
takePositiveValueFromDict =
    takeValueFromDict


{-| Dummy function to just test the concepts. Here we axiomatically transfer a proof.
-- If the dict contains no curse words then a member of that dictionary cannot contain any curse words either
-}
takePositiveValueFromKidFriendlyDict :
    WithKnowledge (A comparable keyName) Positive anyDomainKnowledge (IsInDict keyName dictName)
    -> WithKnowledge (A (Dict comparable v) dictName) ContainsNoCurseWords anyDomainKnowledge2 anyNamedKnowledge2
    -> WithKnowledge v ContainsNoCurseWords NoDomainKnowledge NoNamedKnowledge
takePositiveValueFromKidFriendlyDict v d =
    axiomaticValueKnowledge ContainsNoCurseWords <| takeValueFromDict v <| forget d


{-| Dummy function to just test the concepts. Make sure that no curse words exists in the dictionary
-}
proveNoCurseWords : Dict comparable String -> Maybe (WithKnowledge (Dict comparable String) ContainsNoCurseWords NoDomainKnowledge NoNamedKnowledge)
proveNoCurseWords d =
    --A dummy implementation since it is not important for the testing
    Just <| axiomaticValueKnowledge ContainsNoCurseWords d
