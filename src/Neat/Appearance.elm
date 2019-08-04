module Neat.Appearance exposing
    ( Appearance
    , fromAttributes
    , toAttributes
    , batch
    , none
    , fromAttribute
    , attribute
    , class
    , id
    )

{-| A brief module for Appearances.


# Core

@docs Appearance
@docs fromAttributes
@docs toAttributes
@docs batch
@docs none


# Attributes

@docs fromAttribute


## Common attributes

@docs attribute
@docs class
@docs id

-}

import Html exposing (Attribute)
import Html.Attributes as Attributes



-- Core


{-| Similar to `Html.Attribute msg` but more flexible and reusable.
-}
type Appearance msg
    = Appearance (List (Attribute msg))


{-| -}
fromAttributes : List (Attribute msg) -> Appearance msg
fromAttributes =
    Appearance


{-| -}
toAttributes : Appearance msg -> List (Attribute msg)
toAttributes (Appearance attrs) =
    attrs


{-| -}
batch : List (Appearance msg) -> Appearance msg
batch ls =
    fromAttributes <| List.concatMap toAttributes ls


{-| -}
none : Appearance msg
none =
    Appearance []



-- Attributes


{-| -}
fromAttribute : Attribute msg -> Appearance msg
fromAttribute attr =
    Appearance [ attr ]


{-| -}
attribute : String -> String -> Appearance msg
attribute name val =
    fromAttribute <|
        Attributes.attribute name val


{-| -}
class : String -> Appearance msg
class =
    fromAttribute << Attributes.class


{-| -}
id : String -> Appearance msg
id =
    fromAttribute << Attributes.id
