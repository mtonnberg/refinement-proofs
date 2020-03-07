module RefinementProofs.Test exposing
    ( foo
    )

import RefinementProofs.Theory as RefinementTheory exposing (Proven, exorcise, Not, axiom)
import RefinementProofs.Context exposing (..)
import RefinementProofs.Proofs.NumberProofs exposing (..)


type State1 = State1
type Red = Red
type CarId = CarId Int
type BackendResult = BackendResult 

type GivenCarId = GivenCarId

type alias Color = String

foo : Int
foo =
    let
        posInt : Proven Int Positive
        posInt =
            case RefinementProofs.Proofs.NumberProofs.provePositive 1 of
                Just i -> i
                Nothing ->  Debug.todo "absurd"

        step1 : ForVersionOf State1 (Proven Int Positive)
        step1 =
            provenForVersion State1 (initialVersion State1) posInt

        versionUsedInStep1 : Versioned State1
        versionUsedInStep1 =
            extractVersion step1

        nextStepVersion : Versioned State1
        nextStepVersion =
            incVersion State1 versionUsedInStep1

        step2 : ForVersionOf State1 (Proven Int Positive)
        step2 =
            provenForVersion State1 nextStepVersion posInt

        fstep2 : Int
        fstep2 = exorcise <| forgetVersion step2
        
        --r == Nothing
        r =
            proveSameVersion step1 step2


        
        carOne : CarId
        carOne = CarId 2

        carTwo : CarId
        carTwo = CarId 33


        proveColorNotRed : Color -> Maybe (Proven Color (Not Red))
        proveColorNotRed = Just << axiom (RefinementTheory.not Red)


        colorForCarOne : For GivenCarId CarId (Proven Color (Not Red))
        colorForCarOne = 
            case proveColorNotRed "Blue" of
                Just p -> provenForContextValue GivenCarId carOne p
                Nothing -> Debug.todo "absurd for the demo."


        noOfWheelsForCarOne : For GivenCarId CarId (Proven Int ZeroOrGreater)
        noOfWheelsForCarOne = 
            case proveZeroOrGreater 4 of
                Just p -> provenForContextValue GivenCarId carOne p
                Nothing -> Debug.todo "absurd for the demo."

        

        noOfWheelsForCarTwo : For GivenCarId CarId (Proven Int ZeroOrGreater)
        noOfWheelsForCarTwo = 
            case proveZeroOrGreater 3 of
                Just p -> provenForContextValue GivenCarId carTwo p
                Nothing -> Debug.todo "absurd for the demo."


        -- Just
        r2 =
            proveSameContext colorForCarOne noOfWheelsForCarOne

        -- Nothing
        r3 =
            proveSameContext colorForCarOne noOfWheelsForCarTwo



        --- Complex context value

        backendResult = initialVersion BackendResult

        
        colorForCarOne2 : For GivenCarId (Versioned BackendResult, CarId) (Proven Color (Not Red))
        colorForCarOne2 = 
            case proveColorNotRed "Blue" of
                Just p -> provenForContextValue GivenCarId (backendResult, carOne) p
                Nothing -> Debug.todo "absurd for the demo."


        noOfWheelsForCarOne2 : For GivenCarId (Versioned BackendResult, CarId) (Proven Int ZeroOrGreater)
        noOfWheelsForCarOne2 = 
            case proveZeroOrGreater 4 of
                Just p -> provenForContextValue GivenCarId (backendResult, carOne) p
                Nothing -> Debug.todo "absurd for the demo."


        r4 =
            proveSameContext colorForCarOne2 noOfWheelsForCarOne2
        
    in
        2