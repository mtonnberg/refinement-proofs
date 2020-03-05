module BasicTheories.FootballTheory exposing
    ( 
        FootballTeam
        , FootballField
        , Judge
    )

import RefinementProofs
    exposing
        ( Proven(..)
        , axiom
        , absurd
        , And
        , exorcise
        , makeAnd
        )
    
import BasicTheories.NumberTheory exposing
    ( Positive
    , provePositive
    )
import BasicTheories.StringTheory exposing
    ( NonEmptyTrimmedString
    )
import BasicTheories.ListTheory exposing
    ( NonEmptyList
    )

type alias FootballTeam = {
    teamName : Proven String NonEmptyTrimmedString
    , teamManager : Proven String  NonEmptyTrimmedString
    , players : Proven (List (Proven String NonEmptyTrimmedString))
    (And NonEmptyList ValidComposition)
    }

type ValidComposition = ValidComposition
type ConcatenatedFirstAndLastName = ConcatenatedFirstAndLastName
type VerifiedTeam = VerifiedTeam
type VerifiedJudge = VerifiedJudge

type alias Judge = {
    name : Proven String  (And NonEmptyTrimmedString ConcatenatedFirstAndLastName)
    }

type alias FootballField =
    { teamA : Proven FootballTeam VerifiedTeam
    , teamB : Proven FootballTeam VerifiedTeam
    , judge : Proven Judge VerifiedJudge
    }

proveVerifiedTeam : FootballTeam -> Maybe (Proven FootballTeam VerifiedTeam)
proveVerifiedTeam team =
    case exorcise team.teamName of
        "Liverpool" -> Just <| axiom VerifiedTeam team
        "Barcelona" -> Just <| axiom VerifiedTeam team
        "Göteborgs IF" -> Just <| axiom VerifiedTeam team
        _ -> Nothing
