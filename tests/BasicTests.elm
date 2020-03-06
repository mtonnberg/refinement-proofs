module BasicTests exposing (suite)

import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)

import BasicTheories.NumberTheory
    exposing
        ( Positive
        , provePositive
        )

import BasicTheories.ListTheory
    exposing
        ( proveNonEmptyList
        )
import RefinementProofs
    exposing
        ( Proven(..)
        , axiom
        , exorcise
        , evaluate
        )

suite : Test
suite =
    describe "The RefinementProofs module"
        [
            test "head of nonemptylist should work" <|
                \_ ->
                    Expect.equal 1 <|
                    Maybe.withDefault -1 <| Maybe.map (\x -> BasicTheories.ListTheory.head x) <| proveNonEmptyList [1,2,3]
        ]