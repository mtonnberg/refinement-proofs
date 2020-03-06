module RefinementProofs.BasicTests exposing (suite)

import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)

import RefinementProofs.Proofs.NumberProofs
    exposing
        ( Positive
        , provePositive
        )

import RefinementProofs.Proofs.ListProofs
    exposing
        ( proveNonEmptyList
        )
import RefinementProofs.Theory
    exposing
        ( Proven(..)
        )

suite : Test
suite =
    describe "The RefinementProofs module"
        [
            test "head of nonemptylist should work" <|
                \_ ->
                    Expect.equal 1 <|
                    Maybe.withDefault -1 <| Maybe.map (\x -> RefinementProofs.Proofs.ListProofs.head x) <| proveNonEmptyList [1,2,3]
        ]