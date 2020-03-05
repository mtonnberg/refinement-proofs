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
    , introOrL
    , introOrR
    , inverse
    , makeAnd
    , makeOr
    , imply
    , modusPonens
    , modusTollens
    , or
    , provenOrInversed
    , since
    , sinceNot
    )


type Proven a b
    = Proven a


type Not a
    = Not


type alias XOr a b =
    Or (And a (Not b)) (And (Not a) b)


type Implies a b
    = Implies


type And a b
    = And


type Or a b
    = Or


axiom : p -> a -> Proven a p
axiom _ x =
    Proven x


axiomInternal : a -> Proven a b
axiomInternal x =
    Proven x


or : p1 -> p2 -> Or p1 p2
or _ _ =
    Or


and : p1 -> p2 -> And p1 p2
and _ _ =
    And


introOrR : Proven a p1 -> Proven a (Or p1 p2)
introOrR (Proven x) =
    axiomInternal x


introOrL : Proven a p1 -> Proven a (Or p2 p1)
introOrL =
    axiomInternal << exorcise


elimOrR : Proven a (Or b c) -> Proven a b
elimOrR =
    axiomInternal << exorcise


elimOrL : Proven a (Or b c) -> Proven a c
elimOrL =
    axiomInternal << exorcise


makeAnd : (a -> Maybe (Proven a b)) -> (a -> Maybe (Proven a c)) -> a -> Maybe (Proven a (And b c))
makeAnd f g x =
    case ( f x, g x ) of
        ( Just _, Just _ ) ->
            Just <| axiomInternal x

        _ ->
            Nothing


makeOr : (a -> Maybe (Proven a b)) -> (a -> Maybe (Proven a c)) -> a -> Maybe (Proven a (Or b c))
makeOr f g x =
    case ( f x, g x ) of
        ( Just _, _ ) ->
            Just <| axiomInternal x

        ( _, Just _ ) ->
            Just <| axiomInternal x

        _ ->
            Nothing


provenOrInversed : (a -> Maybe (Proven a p1)) -> a -> Result (Proven a (Not p1)) (Proven a p1)
provenOrInversed f x =
    case f x of
        Just r ->
            Ok r

        Nothing ->
            Err <| axiomInternal x


inverse : (a -> Maybe (Proven a p1)) -> a -> Maybe (Proven a (Not p1))
inverse f x =
    case f x of
        Just _ ->
            Nothing

        Nothing ->
            Just <| axiomInternal x


elimAndL : Proven a (And b c) -> Proven a b
elimAndL =
    axiomInternal << exorcise


elimAndR : Proven a (And b c) -> Proven a c
elimAndR =
    axiomInternal << exorcise


exorcise : Proven a b -> a
exorcise (Proven x) =
    x


modusPonens : Implies p1 p2 -> Proven a p1 -> Proven a p2
modusPonens _ =
    axiomInternal << exorcise


since : Implies p1 p2 -> Proven a p1 -> Proven a p2
since =
    modusPonens


modusTollens : Implies p1 p2 -> Proven a (Not p1) -> Proven a (Not p2)
modusTollens _ =
    axiomInternal << exorcise


sinceNot : Implies p1 p2 -> Proven a (Not p1) -> Proven a (Not p2)
sinceNot =
    modusTollens


imply : p1 -> p2 -> Implies p1 p2
imply _ _ =
    Implies


absurd : a
absurd =
    Debug.todo "absurd, this should never happen. If it does the library used is doing something horribly wrong. Please check your proofs"
