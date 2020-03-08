module RefinementProofs.Theory exposing
    ( And
    , Implies
    , Not
    , Or
    , Proven
    , XOr
    , and
    , axiom
    , elimAndL
    , elimAndR
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
    , since
    , sinceNot
    )

{-| This library allows for stronger and more refined types in Elm for both library writer and end users.


Sometimes we want to create "proofs" that is dependent other other data or other systems - for example a API backend.
We can use a "Context" to describe under which scenario the proof holds. 
Note, there is value to describe the context even if other proofs is not used.

For example:

    cozyAnimals : ForVersionOf BackendResponse (Proved (List CozyAnimals) NonEmptyList)

Would mean that for a given backendresponse we have proved that the list of cozy animals is non empty
In a different part of the code we could have this type:

    animal : ForVersionOf BackendResponse (Proved Animal CozyAnimal)
        
Which would mean that for a given backend response we have proved that the animal in question is a cozy animal
Then in a third part of the code we have both the cozyAnimals and the animal value
To see if the proofs are based on the same backend response we would
        
    firstCheck : Maybe (ForVersionOf BackendResponse (Proved (List CozyAnimals) NonEmptyList, Proved Animal CozyAnimal)
    firstCheck = proveSameVersion cozyAnimals animal
    -- Continue here, deciding what to do if the proofs came from two different API responses 

# Context
Context is more generalized data type that `Version` is based upon. This is useful if a specific v
is prefered or needed instead of just a 'random' number.

For example: 

    type alias Ears = Int
    type AnimalName = AnimalName String 
    p : For AnimalName (Proven Ears Positive)
    p = ...
    -- For: is the name for a Contexbased expression
    -- AnimalName: is the constructor that is *not* exported by the Animal-module RefinementProofs.to ensure that no one
    -- else can create an AnimalName context and rewire the proofs
    -- Proven Ears Positive: The actual expression that is in the described context


# Definition
@docs Proven

# Logic operator types
@docs And, Or, Not, XOr, Implies

# Basic function
@docs axiom, or, and, not

# Logic operators
@docs introOrR, introOrL, makeOr, makeAnd, inverse, elimAndL, elimAndR, exorcise, modusPonens, since, modusTollens, sinceNot, imply

-}

{-| A value "a" that is proven to hold the property "b"
-}
type Proven a p
    = Proven a


{-| A negation of a proof
-}
type Not p
    = Not


{-| Exlusive or
-}
type alias XOr p1 p2 =
    Or (And p1 (Not p2)) (And (Not p1) p2)


{-| Implies that if "p1" holds than "p2" holds
-}
type Implies p1 p2
    = Implies


{-| Both "p1" and "p2" holds
-}
type And p1 p2
    = And


{-| "p1" or "p2" holds
-}
type Or p1 p2
    = Or


{-| Used by library writers to create axioms with non-exported constructors.
    Remember, the library/module RefinementProofs.constructors must not be exported!
-}
axiom : p -> a -> Proven a p
axiom _ x =
    Proven x


{-| DANGER ZONE!
    Used internally by this module RefinementProofs.only to prove general logic. Must never be exported and used with extreme care!
-}
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


{-| If a is proven to hold property p1 then classic logic gives that a holds either p1 or p2
-}
introOrR : Proven a p1 -> Proven a (Or p1 p2)
introOrR (Proven x) =
    axiomInternal x


{-| Same as introOrR but introduces the p2 on the other side
-}
introOrL : Proven a p1 -> Proven a (Or p2 p1)
introOrL =
    axiomInternal << exorcise


{-| A convience method to check two proofs at once for And
-}
makeAnd : (a -> Maybe (Proven a b)) -> (a -> Maybe (Proven a c)) -> a -> Maybe (Proven a (And b c))
makeAnd f g x =
    case ( f x, g x ) of
        ( Just _, Just _ ) ->
            Just <| axiomInternal x

        _ ->
            Nothing



{-| A convience method to check two proofs for Or
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


{-| Used by library writers to show that when `p1` does not hold for a then `Not p1` holds.
-}
inverse : p1 -> (a -> Maybe (Proven a p1)) -> a -> Maybe (Proven a (Not p1))
inverse _ f x =
    case f x of
        Just _ ->
            Nothing

        Nothing ->
            Just <| axiomInternal x


{-| Remove the left hand side of an And proof
-}
elimAndL : Proven a (And b c) -> Proven a b
elimAndL =
    axiomInternal << exorcise


{-| Remove the right hand side of an And proof
-}
elimAndR : Proven a (And b c) -> Proven a c
elimAndR =
    axiomInternal << exorcise

{-| Extract the actual value and remove the proof. This is always safe and a library writer should always expect and allow its users to do this.
-}
exorcise : Proven a b -> a
exorcise (Proven x) =
    x


{-| If a library implies that when p1 holds the p2 holds then you have proven p2 by prove p1
-}
modusPonens : Implies p1 p2 -> Proven a p1 -> Proven a p2
modusPonens _ =
    axiomInternal << exorcise


{-| Synonym for "modus ponens". If a library implies that when p1 holds the p2 holds then you have proven p2 by prove p1
-}
since : Implies p1 p2 -> Proven a p1 -> Proven a p2
since =
    modusPonens


{-| If a library implies that when p1 holds the p2 holds then you have proven (Not p2) by prove (Not p1)
-}
modusTollens : Implies p1 p2 -> Proven a (Not p1) -> Proven a (Not p2)
modusTollens _ =
    axiomInternal << exorcise


{-| Synonym for "modus tollens". If a library implies that when p1 holds the p2 holds then you have proven (Not p2) by prove (Not p1)
-}
sinceNot : Implies p1 p2 -> Proven a (Not p1) -> Proven a (Not p2)
sinceNot =
    modusTollens


{-| Used by library writers to imply proof relations
-}
imply : p1 -> p2 -> Implies p1 p2
imply _ _ =
    Implies
