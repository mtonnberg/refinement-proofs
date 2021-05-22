module RefinementProofs.ContextTests exposing (suite)

import Expect
import RefinementProofs.Context exposing (..)
import RefinementProofs.Proofs.NumberProofs exposing (..)
import RefinementProofs.Theory as RefinementTheory
    exposing
        ( Not
        , WithKnowledge(..)
        , axiom
        , forget
        )
import Test exposing (Test, describe, fuzz, test)


type State1
    = State1


type Red
    = Red


type CarId
    = CarId Int


type BackendResult
    = BackendResult


type GivenCarId
    = GivenCarId


type alias Color =
    String


proveColorNotRed : Color -> Maybe (WithKnowledge Color (Not Red))
proveColorNotRed =
    Just << axiom (RefinementTheory.not Red)


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just _ ->
            True

        Nothing ->
            False


suite : Test
suite =
    describe "Contexts module"
        [ test "Compare different versions" <|
            \_ ->
                let
                    posInt : WithKnowledge Int Positive
                    posInt =
                        case RefinementProofs.Proofs.NumberProofs.provePositive 1 of
                            Just i ->
                                i

                            Nothing ->
                                Debug.todo "absurd"

                    step1 : ForVersionOf State1 (WithKnowledge Int Positive)
                    step1 =
                        provenForVersion State1 (initialVersion State1) posInt

                    versionUsedInStep1 : Versioned State1
                    versionUsedInStep1 =
                        extractVersion step1

                    nextStepVersion : Versioned State1
                    nextStepVersion =
                        incVersion State1 versionUsedInStep1

                    step2 : ForVersionOf State1 (WithKnowledge Int Positive)
                    step2 =
                        provenForVersion State1 nextStepVersion posInt
                in
                Expect.true "version tests" <|
                    isJust (proveSameVersion step1 step1)
                        && not (isJust <| proveSameVersion step1 step2)
                        && ((forget <| forgetVersion step2) == 1)
        , test "Compare different contexts, more complex" <|
            \_ ->
                let
                    carOne : CarId
                    carOne =
                        CarId 2

                    backendResult =
                        initialVersion BackendResult

                    colorForCarOne2 : For GivenCarId ( Versioned BackendResult, CarId ) (WithKnowledge Color (Not Red))
                    colorForCarOne2 =
                        case proveColorNotRed "Blue" of
                            Just p ->
                                provenForContextValue GivenCarId ( backendResult, carOne ) p

                            Nothing ->
                                Debug.todo "absurd"

                    noOfWheelsForCarOne2 : For GivenCarId ( Versioned BackendResult, CarId ) (WithKnowledge Int ZeroOrGreater)
                    noOfWheelsForCarOne2 =
                        case proveZeroOrGreater 4 of
                            Just p ->
                                provenForContextValue GivenCarId ( backendResult, carOne ) p

                            Nothing ->
                                Debug.todo "absurd"
                in
                Expect.true "contexts, tuple" <|
                    isJust (proveSameContext colorForCarOne2 noOfWheelsForCarOne2)
        , test "Compare different contexts, simple" <|
            \_ ->
                let
                    carOne : CarId
                    carOne =
                        CarId 2

                    carTwo : CarId
                    carTwo =
                        CarId 33

                    colorForCarOne : For GivenCarId CarId (WithKnowledge Color (Not Red))
                    colorForCarOne =
                        case proveColorNotRed "Blue" of
                            Just p ->
                                provenForContextValue GivenCarId carOne p

                            Nothing ->
                                Debug.todo "absurd"

                    noOfWheelsForCarOne : For GivenCarId CarId (WithKnowledge Int ZeroOrGreater)
                    noOfWheelsForCarOne =
                        case proveZeroOrGreater 4 of
                            Just p ->
                                provenForContextValue GivenCarId carOne p

                            Nothing ->
                                Debug.todo "absurd"

                    noOfWheelsForCarTwo : For GivenCarId CarId (WithKnowledge Int ZeroOrGreater)
                    noOfWheelsForCarTwo =
                        case proveZeroOrGreater 3 of
                            Just p ->
                                provenForContextValue GivenCarId carTwo p

                            Nothing ->
                                Debug.todo "absurd"

                    r2 =
                        proveSameContext colorForCarOne noOfWheelsForCarOne

                    r3 =
                        proveSameContext colorForCarOne noOfWheelsForCarTwo
                in
                Expect.true "context checks" <|
                    (not <| isJust r3)
                        && isJust r2
        ]
