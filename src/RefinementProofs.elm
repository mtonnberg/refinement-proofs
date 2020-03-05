module RefinementProofs exposing
    ( And
    , Implies
    , Not
    , Or
    , Proven
    , XOr
    , absurd
    , and
    , axiom
    , elimAndL
    , elimAndR
    , elimOrL
    , elimOrR
    , exorcise
    , imply
    , introOrL
    , introOrR
    , inverse
    , makeAnd
    , makeOr
    , modusPonens
    , modusTollens
    , not
    , or
    , provenOrInversed
    , since
    , sinceNot
    )

{-| This library allows for stronger types in Elm for both library writer and end users

# Definition
@docs Proven

# Logic operator types
@docs And, Or, Not, XOr, Implies

# Basic function
@docs axiom, or, and, not

# Logic operators
@docs introOrR, introOrL, elimOrR, elimOrL, makeOr, makeAnd, provenOrInversed, inverse, elimAndL, elimAndR, exorcise, modusPonens, since, modusTollens, sinceNot, imply, absurd

-}


{-| A value "a" that is proven to hold the property "b"
-}
type Proven a b
    = Proven a


{-| A negation of a proof
-}
type Not a
    = Not


{-| Exlusive or
-}
type alias XOr a b =
    Or (And a (Not b)) (And (Not a) b)


{-| Implies that if "a" holds than "b" holds
-}
type Implies a b
    = Implies


{-| Both "a" and "b" holds
-}
type And a b
    = And


{-| "a" or "b" holds
-}
type Or a b
    = Or


{-| Used by library writers to create axioms with non-exported constructors
-}
axiom : p -> a -> Proven a p
axiom _ x =
    Proven x


axiomInternal : a -> Proven a b
axiomInternal x =
    Proven x


{-| Used by library writers to create build a Or proof
-}
or : p1 -> p2 -> Or p1 p2
or _ _ =
    Or


{-| Used by library writers to create build a And proof
-}
and : p1 -> p2 -> And p1 p2
and _ _ =
    And


{-| Used by library writers to create build a Not proof
-}
not : p1 -> Not p1
not _ =
    Not


{-| Basic logic
-}
introOrR : Proven a p1 -> Proven a (Or p1 p2)
introOrR (Proven x) =
    axiomInternal x


{-| Basic logic
-}
introOrL : Proven a p1 -> Proven a (Or p2 p1)
introOrL =
    axiomInternal << exorcise


{-| Basic logic
-}
elimOrR : Proven a (Or b c) -> Proven a b
elimOrR =
    axiomInternal << exorcise


{-| Basic logic
-}
elimOrL : Proven a (Or b c) -> Proven a c
elimOrL =
    axiomInternal << exorcise


{-| A convience method to check two proofs at once
-}
makeAnd : (a -> Maybe (Proven a b)) -> (a -> Maybe (Proven a c)) -> a -> Maybe (Proven a (And b c))
makeAnd f g x =
    case ( f x, g x ) of
        ( Just _, Just _ ) ->
            Just <| axiomInternal x

        _ ->
            Nothing



{-| A convience method to check two proofs
-}
makeOr : (a -> Maybe (Proven a b)) -> (a -> Maybe (Proven a c)) -> a -> Maybe (Proven a (Or b c))
makeOr f g x =
    case ( f x, g x ) of
        ( Just _, _ ) ->
            Just <| axiomInternal x

        ( _, Just _ ) ->
            Just <| axiomInternal x

        _ ->
            Nothing


{-| Basic logic
-}
provenOrInversed : (a -> Maybe (Proven a p1)) -> a -> Result (Proven a (Not p1)) (Proven a p1)
provenOrInversed f x =
    case f x of
        Just r ->
            Ok r

        Nothing ->
            Err <| axiomInternal x


{-| Basic logic
-}
inverse : (a -> Maybe (Proven a p1)) -> a -> Maybe (Proven a (Not p1))
inverse f x =
    case f x of
        Just _ ->
            Nothing

        Nothing ->
            Just <| axiomInternal x


{-| Basic logic
-}
elimAndL : Proven a (And b c) -> Proven a b
elimAndL =
    axiomInternal << exorcise


{-| Basic logic
-}
elimAndR : Proven a (And b c) -> Proven a c
elimAndR =
    axiomInternal << exorcise

{-| Extract the actual value and remove the proof
-}
exorcise : Proven a b -> a
exorcise (Proven x) =
    x


{-| Basic logic
-}
modusPonens : Implies p1 p2 -> Proven a p1 -> Proven a p2
modusPonens _ =
    axiomInternal << exorcise


{-| Synonym for "modus ponens"
-}
since : Implies p1 p2 -> Proven a p1 -> Proven a p2
since =
    modusPonens


{-| Basic logic
-}
modusTollens : Implies p1 p2 -> Proven a (Not p1) -> Proven a (Not p2)
modusTollens _ =
    axiomInternal << exorcise


{-| Synonym for "modus tollens" 
-}
sinceNot : Implies p1 p2 -> Proven a (Not p1) -> Proven a (Not p2)
sinceNot =
    modusTollens


{-| Used by library writers to imply proof relations
-}
imply : p1 -> p2 -> Implies p1 p2
imply _ _ =
    Implies


{-| Used by library writers to carefully ignore "impossible" states
-}
absurd : a
absurd =
    Debug.todo "absurd, this should never happen. If it does the library used is doing something horribly wrong. Please check your proofs"
