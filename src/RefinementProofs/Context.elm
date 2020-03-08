module RefinementProofs.Context exposing
    ( Context
    , For
    , ForVersionOf
    , Versioned
    , extractVersion
    , compareVersions
    , compareContexts
    , extractContext
    , forgetVersion
    , provenForVersion
    , incVersion
    , initialVersion
    , unwrapContext
    , proveSameContext
    , provenForContext
    , provenForContextValue
    , specificContext
    , proveSameVersion
    )

{-|| 
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

# Version
@docs ForVersionOf
    , Versioned
    , extractVersion
    , compareVersions
    , forgetVersion
    , provenForVersion
    , incVersion
    , initialVersion
    , proveSameVersion

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

Which would mean that for a specific AnimalName it is proven that that animal has a positive number of ears.

@docs Context
    , For
    , compareContexts
    , extractContext
    , proveSameContext
    , provenForContext
    , specificContext
    , provenForContextValue
    , unwrapContext
-}
import RefinementProofs.Theory exposing (Proven)


{-|
Describes a context in which something can be put.

The `key` is used to ensure that only the correct module/library can create contexts. 
Remember to keep the constructors private! 
-}
type Context key contextId
    = Context contextId


{-|
Describes a context and what is in that context.

The `key` is used to ensure that only the correct module/library can create contexts. 

The `contextId` is the value used to check if two contexts describe the same thing

The `a` is the actual value in the context 
-}
type For key contextId a
    = For (Context key contextId) a



{-|
A specialized case of `For` that covers a lot of the basic cases and is less verbose to work with.

The `key` is used to ensure that only the correct module/library can create contexts. 

The `a` is the actual value in the context 
-}
type ForVersionOf key a =
    ForVersionOf (Versioned key) a


{-|
A specialized case of `Context` that is used with `ForVersionOf`.

The `key` is used to ensure that only the correct module/library can create contexts. 
-}
type Versioned key =
    Versioned Int



{-|
Forget the context, analogous to exorcise for proofs.
-}
forgetContext : For key contextId a -> a
forgetContext (For _ x) =
    x

{-|
Put a proof into a context
-}
provenForContext : key -> Context key contextId -> Proven a p -> For key contextId (Proven a p)
provenForContext _ (Context v) =
    For (Context v)


{-|
Put a proof into a context
-}
provenForContextValue : key -> contextId -> Proven a p -> For key contextId (Proven a p)
provenForContextValue _ v =
    For (Context v)



{-|
Merge two `For`s if they describe values in the same context
-}
proveSameContext : For key contextId a -> For key contextId b -> Maybe (For key contextId ( a, b ))
proveSameContext ((For c1 _) as x) ((For c2 _) as y) =
    if compareContexts c1 c2 then
        Just <| For c1 ( forgetContext x, forgetContext y )

    else
        Nothing

{-|
Create a context with a specific value, for example an id.
-}
specificContext : key -> contextId -> Context key contextId
specificContext _ c =
    Context c




{-|
Extract the context for comparision
-}
extractContext : For key contextId a -> Context key contextId
extractContext (For v _) =
    v


{-|
Used by library writers to get the context's value
-}
unwrapContext : key -> Context key contextId -> contextId
unwrapContext _ (Context v1) = v1


{-|
Compare two contexts for equality, if equal they describe the same context
-}
compareContexts : Context key contextId -> Context key contextId -> Bool
compareContexts (Context v1) (Context v2) =
    v1 == v2


--------------------- Version


{-|
Get a initial version.
If you are interested of the acutal value then you should use `Context` and `For`
-}
initialVersion : key -> Versioned key
initialVersion _ =
    Versioned 0


{-|
Get a new version.
If you are interested of the acutal value then you should use `Context` and `For`
-}
incVersion : key -> Versioned key -> Versioned key
incVersion _ (Versioned x) =
    Versioned (x+1)

{-|
Extract the version for comparision
-}
extractVersion : ForVersionOf key a -> Versioned key
extractVersion (ForVersionOf v _) = 
    v


{-|
Compare two contexts for equality
-}
compareVersions : Versioned key -> Versioned key -> Bool
compareVersions (Versioned v1) (Versioned v2) =
    v1 == v2



{-|
Forget the version, analogous to exorcise for proofs.
-}
forgetVersion : ForVersionOf key a -> a
forgetVersion (ForVersionOf _ x) =
    x


{-|
    Put a proof into a context
-}
provenForVersion : key -> Versioned key -> Proven a p -> ForVersionOf key (Proven a p)
provenForVersion _ (Versioned x) =
    ForVersionOf (Versioned x)


{-|
Merge two `For`s if they describe values in the same context
-}
proveSameVersion : ForVersionOf key a -> ForVersionOf key b -> Maybe (ForVersionOf key ( a, b ))
proveSameVersion ((ForVersionOf c1 _) as x) ((ForVersionOf c2 _) as y) =
    if compareVersions c1 c2 then
        Just <| ForVersionOf c1 ( forgetVersion x, forgetVersion y )

    else
        Nothing
