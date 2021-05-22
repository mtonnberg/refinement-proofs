module RefinementProofs.Temp.TestNamed exposing
    ( IsInDict
    , Positive
    , divide2
    , proveKeyIsInDict
    , provePositive2
    , takeValueFromDict
    , takeValueFromDict2
    )

import Dict exposing (Dict)
import RefinementProofs.Knowledge
    exposing
        ( A
        , Proof
        , WithKnowledge
        , axiomaticValueKnowledge
        , forget
        , makeProof
        , raw
        , the
        )
import RefinementProofs.Knowledge exposing (NoDomainKnowledge)
import RefinementProofs.Knowledge exposing (NoNamedKnowledge)


type Positive
    = Positive


type IsInDict key dict
    = IsInDict


divide2 : Int -> WithKnowledge Int Positive d n -> Float
divide2 x ty =
    toFloat x / (toFloat <| forget ty)


provePositive2 : number -> Maybe (WithKnowledge number Positive NoDomainKnowledge NoNamedKnowledge)
provePositive2 tx =
    if tx > 0 then
        Just (axiomaticValueKnowledge Positive tx)

    else
        Nothing


proveKeyIsInDict : A comparable keyName -> A (Dict comparable v) dictName -> Maybe (Proof (IsInDict keyName dictName))
proveKeyIsInDict tk tdict =
    if Dict.member (the tk) (the tdict) then
        Just (makeProof IsInDict)

    else
        Nothing


takeValueFromDict :
    WithKnowledge (A comparable keyName) v d (IsInDict keyName dictName)
    -> A (Dict comparable v) dictName
    -> v
takeValueFromDict ptkey tdict =
    case Dict.get (raw ptkey) (the tdict) of
        Nothing ->
            Debug.todo "absurd"

        Just v ->
            v

takeValueFromDict2 :
    WithKnowledge (A comparable keyName) Positive d (IsInDict keyName dictName)
    -> A (Dict comparable v) dictName
    -> v
takeValueFromDict2 ptkey tdict =
    case Dict.get (raw ptkey) (the tdict) of
        Nothing ->
            Debug.todo "absurd"

        Just v ->
            v
