module RefinementProofs.WithKnowledge exposing
    ( WithKnowledge
    , And, Or, Not, XOr, Implies
    , or, and, not
    , forget, imply
    , forgetNamedKnowledge
    , setNamedKnowledge
    , axiomaticallySetDomainKnowledge
    , NoValueKnowledge
    , NoDomainKnowledge
    , axiomaticallyAddDomainKnowledge
    , d_andIsFlippable, d_modusPonens, d_since
    , NoNamedKnowledge
    , A, NoKnowledge, Proof, attachNamedKnowledge, axiomaticDomainKnowledge, axiomaticNamedKnowledge, axiomaticValueKnowledge, detachNamedKnowledge, makeProof, n_elimAndL, n_elimAndR, n_introOrL, n_introOrR, n_inverse, n_makeAnd, n_makeOr, n_modusPonens, n_modusTollens, n_since, n_sinceNot, name, name2, raw, the, v_elimAndL, v_elimAndR, v_introOrL, v_introOrR, v_inverse, v_makeAnd, v_makeOr, v_modusPonens, v_modusTollens, v_since, v_sinceNot, withName
    )

{-| This library allows for stronger and more refined types in Elm for both library writer and end users.

Sometimes we want to create "proofs" that is dependent other other data or other systems - for example a API backend.
We can use a "Context" to describe under which scenario the proof holds.
Note, there is value to describe the domain even if other proofs is not used.

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
    p : For AnimalName (WithKnowledge Ears Positive)
    p = ...
    -- For: is the name for a Contexbased expression
    -- AnimalName: is the constructor that is *not* exported by the Animal-module RefinementProofs.to ensure that no one
    -- else can create an AnimalName domain and rewire the proofs
    -- WithKnowledge Ears Positive: The actual expression that is in the described domain


# Definition

@docs WithKnowledge


# Logic operator types

@docs And, Or, Not, XOr, Implies


# Basic function

@docs axiom, or, and, not


# Logic operators

@docs introOrR, introOrL, makeOr, makeAnd, inverse, elimAndL, elimAndR, forget, modusPonens, since, modusTollens, sinceNot, imply

-}


{-| A value "a" we know stuff about:
-- we know that the value a satisfy what is described in valueKnowledge
-- we know that we have checked something in the domain
-- or we can have named knowledge that can be detached from this value, since the type system keeps track of it
-}
type WithKnowledge a valueKnowledge domainKnowledge namedKnowledge
    = WithKnowledge a


type NoKnowledge
    = NoKnowledge

type alias NoValueKnowledge = NoKnowledge
type alias NoDomainKnowledge = NoKnowledge
type alias NoNamedKnowledge = NoKnowledge

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


type Proof p
    = Proof


makeProof : p -> Proof p
makeProof _ =
    Proof


detachNamedKnowledge : WithKnowledge a s c p -> Proof p
detachNamedKnowledge _ =
    Proof


attachNamedKnowledge : WithKnowledge a s c n -> Proof n1 -> WithKnowledge a s c (And n n1)
attachNamedKnowledge x _ =
    axiomInternal <| forget x
setNamedKnowledge : WithKnowledge a s d n1 -> Proof n2 -> WithKnowledge a s d n2
setNamedKnowledge x _ =
    axiomInternal <| forget x

axiomaticallyAddDomainKnowledge :  d2  -> WithKnowledge a s d1 n -> WithKnowledge a s (And d1 d2) n
axiomaticallyAddDomainKnowledge _ x =
    axiomInternal <| forget x

axiomaticallySetDomainKnowledge :  d2 -> WithKnowledge a s d1 n -> WithKnowledge a s d2 n
axiomaticallySetDomainKnowledge _ x =
    axiomInternal <| forget x


type A a name
    = A a


the : A a name -> a
the (A x) =
    x


raw : WithKnowledge (A a name) v d n -> a
raw =
    the << forget


name : a -> (A a c -> b) -> b
name x f =
    f (A x)


name2 : a1 -> a2 -> (A a1 c1 -> A a2 c2 -> b) -> b
name2 x y f =
    name x
        (\named1 ->
            name y
                (\named2 ->
                    f named1 named2
                )
        )


withName : (a -> Maybe (WithKnowledge a v d n)) -> A a name -> Maybe (WithKnowledge (A a name) v d n)
withName f x =
    Maybe.map (WithKnowledge << A << forget) <| f (the x)


{-| Used by library writers to create axioms with non-exported constructors.
Remember, the library/module RefinementProofs.constructors must not be exported!
-}
axiomaticValueKnowledge : v -> a -> WithKnowledge a v NoKnowledge NoKnowledge
axiomaticValueKnowledge _ x =
    WithKnowledge x


{-| Used by library writers to create axioms with non-exported constructors.
Remember, the library/module RefinementProofs.constructors must not be exported!
-}
axiomaticDomainKnowledge : c -> a -> WithKnowledge a NoKnowledge c NoKnowledge
axiomaticDomainKnowledge _ x =
    WithKnowledge x


{-| Used by library writers to create axioms with non-exported constructors.
Remember, the library/module RefinementProofs.constructors must not be exported!
-}
axiomaticNamedKnowledge : n -> a -> WithKnowledge a NoKnowledge NoKnowledge n
axiomaticNamedKnowledge _ x =
    WithKnowledge x


{-| DANGER ZONE!
Used internally by this module RefinementProofs.only to prove general logic. Must never be exported and used with extreme care!
-}
axiomInternal : a -> WithKnowledge a s c n
axiomInternal x =
    WithKnowledge x


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


{-| For named knowledge. If a is proven to hold property p1 then classic logic gives that a holds either p1 or p2
-}
n_introOrR : WithKnowledge a v c p1 -> WithKnowledge a v c (Or p1 p2)
n_introOrR (WithKnowledge x) =
    axiomInternal x


{-| Same as introOrR but introduces the p2 on the other side
-}
n_introOrL : WithKnowledge a v c p1 -> WithKnowledge a v c (Or p2 p1)
n_introOrL =
    axiomInternal << forget


{-| A convience method to check two proofs at once for And
-}
n_makeAnd : (a -> Maybe (WithKnowledge a v d b)) -> (a -> Maybe (WithKnowledge a v d c)) -> a -> Maybe (WithKnowledge a v d (And b c))
n_makeAnd f g x =
    case ( f x, g x ) of
        ( Just _, Just _ ) ->
            Just <| axiomInternal x

        _ ->
            Nothing


{-| A convience method to check two proofs for Or
-}
n_makeOr : (a -> Maybe (WithKnowledge a v d b)) -> (a -> Maybe (WithKnowledge a v d c)) -> a -> Maybe (WithKnowledge a v d (Or b c))
n_makeOr f g x =
    case ( f x, g x ) of
        ( Just _, _ ) ->
            Just <| axiomInternal x

        ( _, Just _ ) ->
            Just <| axiomInternal x

        _ ->
            Nothing


{-| Used by library writers to show that when `p1` does not hold for a then `Not p1` holds.
-}
n_inverse : p1 -> (a -> Maybe (WithKnowledge a v c p1)) -> a -> Maybe (WithKnowledge a v c (Not p1))
n_inverse _ f x =
    case f x of
        Just _ ->
            Nothing

        Nothing ->
            Just <| axiomInternal x


{-| Remove the left hand side of an And proof
-}
n_elimAndL : WithKnowledge a v c (And b1 b2) -> WithKnowledge a v c b1
n_elimAndL =
    axiomInternal << forget


{-| Remove the right hand side of an And proof
-}
n_elimAndR : WithKnowledge a v c (And b1 b2) -> WithKnowledge a v c b2
n_elimAndR =
    axiomInternal << forget


{-| If a library implies that when p1 holds the p2 holds then you have proven p2 by prove p1
-}
n_modusPonens : Implies p1 p2 -> WithKnowledge a v c p1 -> WithKnowledge a v c p2
n_modusPonens _ =
    axiomInternal << forget


{-| Synonym for "modus ponens". If a library implies that when p1 holds the p2 holds then you have proven p2 by prove p1
-}
n_since : Implies p1 p2 -> WithKnowledge a v c p1 -> WithKnowledge a v c p2
n_since =
    n_modusPonens


{-| If a library implies that when p1 holds the p2 holds then you have proven (Not p2) by prove (Not p1)
-}
n_modusTollens : Implies p1 p2 -> WithKnowledge a v c (Not p1) -> WithKnowledge a v c (Not p2)
n_modusTollens _ =
    axiomInternal << forget


{-| Synonym for "modus tollens". If a library implies that when p1 holds the p2 holds then you have proven (Not p2) by prove (Not p1)
-}
n_sinceNot : Implies p1 p2 -> WithKnowledge a v c (Not p1) -> WithKnowledge a v c (Not p2)
n_sinceNot =
    n_modusTollens


{-| Used by library writers to imply proof relations
-}
imply : p1 -> p2 -> Implies p1 p2
imply _ _ =
    Implies


{-| Extract the actual value and remove the proof. This is always safe and a library writer should always expect and allow its users to do this.
-}
forget : WithKnowledge a v c n -> a
forget (WithKnowledge x) =
    x

{-| Remove named knowledge. This is always safe and a library writer should always expect and allow its users to do this.
-}
forgetNamedKnowledge : WithKnowledge a v c n -> WithKnowledge a v c NoKnowledge
forgetNamedKnowledge (WithKnowledge x) =
    WithKnowledge x



{-| For named knowledge. If a is proven to hold property p1 then classic logic gives that a holds either p1 or p2
-}
v_introOrR : WithKnowledge a v1 c n -> WithKnowledge a (Or v1 v2) c n
v_introOrR (WithKnowledge x) =
    axiomInternal x


{-| Same as introOrR but introduces the p2 on the other side
-}
v_introOrL : WithKnowledge a v1 c p -> WithKnowledge a (Or v2 v1) c p
v_introOrL =
    axiomInternal << forget


{-| A convience method to check two proofs at once for And
-}
v_makeAnd : (a -> Maybe (WithKnowledge a v1 d b)) -> (a -> Maybe (WithKnowledge a v2 d b)) -> a -> Maybe (WithKnowledge a (And v1 v2) d b)
v_makeAnd f g x =
    case ( f x, g x ) of
        ( Just _, Just _ ) ->
            Just <| axiomInternal x

        _ ->
            Nothing


{-| A convience method to check two proofs for Or
-}
v_makeOr : (a -> Maybe (WithKnowledge a v1 d b)) -> (a -> Maybe (WithKnowledge a v2 d b)) -> a -> Maybe (WithKnowledge a (Or v1 v2) d b)
v_makeOr f g x =
    case ( f x, g x ) of
        ( Just _, _ ) ->
            Just <| axiomInternal x

        ( _, Just _ ) ->
            Just <| axiomInternal x

        _ ->
            Nothing


{-| Used by library writers to show that when `p1` does not hold for a then `Not p1` holds.
-}
v_inverse : v -> (a -> Maybe (WithKnowledge a v c p1)) -> a -> Maybe (WithKnowledge a (Not v) c p1)
v_inverse _ f x =
    case f x of
        Just _ ->
            Nothing

        Nothing ->
            Just <| axiomInternal x


{-| Remove the left hand side of an And proof
-}
v_elimAndL : WithKnowledge a (And b1 b2) c n -> WithKnowledge a b1 c n
v_elimAndL =
    axiomInternal << forget


{-| Remove the right hand side of an And proof
-}
v_elimAndR : WithKnowledge a (And b1 b2) c n -> WithKnowledge a b2 c n
v_elimAndR =
    axiomInternal << forget


{-| If a library implies that when p1 holds the p2 holds then you have proven p2 by prove p1
-}
v_modusPonens : Implies v1 v2 -> WithKnowledge a v1 c p -> WithKnowledge a v2 c p
v_modusPonens _ =
    axiomInternal << forget


{-| Synonym for "modus ponens". If a library implies that when p1 holds the p2 holds then you have proven p2 by prove p1
-}
v_since : Implies v1 v2 -> WithKnowledge a v1 c p -> WithKnowledge a v2 c p
v_since =
    v_modusPonens


{-| If a library implies that when p1 holds the p2 holds then you have proven (Not p2) by prove (Not p1)
-}
v_modusTollens : Implies v1 v2 -> WithKnowledge a (Not v1) c p -> WithKnowledge a (Not v2) c p
v_modusTollens _ =
    axiomInternal << forget


{-| Synonym for "modus tollens". If a library implies that when p1 holds the p2 holds then you have proven (Not p2) by prove (Not p1)
-}
v_sinceNot : Implies v1 v2 -> WithKnowledge a (Not v1) c p -> WithKnowledge a (Not v2) c p
v_sinceNot =
    v_modusTollens



{-| If a library implies that when p1 holds the p2 holds then you have proven p2 by prove p1
-}
d_modusPonens : Implies d1 d2 -> WithKnowledge a v d1 p -> WithKnowledge a v d2 p
d_modusPonens _ =
    axiomInternal << forget


{-| Synonym for "modus ponens". If a library implies that when p1 holds the p2 holds then you have proven p2 by prove p1
-}
d_since : Implies d1 d2 -> WithKnowledge a v d1 p -> WithKnowledge a v d2 p
d_since =
    d_modusPonens

d_andIsFlippable : Implies (And a b) (And b a)
d_andIsFlippable = Implies