module RefinementProofs.ListProofsTests exposing (suite)

import Expect
import RefinementProofs.Proofs.ListProofs
    exposing
        ( proveNonEmptyList
        )
import RefinementProofs.Proofs.NumberProofs exposing (..)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Basic wiring"
        [ test "head of nonemptylist should work" <|
            \_ ->
                Expect.equal 1 <|
                    Maybe.withDefault -1 <|
                        Maybe.map (\x -> RefinementProofs.Proofs.ListProofs.head x) <|
                            proveNonEmptyList [ 1, 2, 3 ]
        ]
