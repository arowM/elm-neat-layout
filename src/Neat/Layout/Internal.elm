module Neat.Layout.Internal exposing
    ( Layout
    , fromAttributes
    , toAttributes
    , batch
    , none
    , isNone
    , fromAttribute
    , style
    , attribute
    )

{-| A brief module for Layouts.


# Core

@docs Layout
@docs fromAttributes
@docs toAttributes
@docs batch
@docs none
@docs isNone


# Attributes

@docs fromAttribute


## Common attributes

@docs style

-}

import Html exposing (Attribute)
import Html.Attributes as Attributes



-- Core


{-| Similar to `Html.Attribute msg` but more flexible and reusable.
-}
type Layout msg
    = Layout (List (Attribute msg))


{-| -}
fromAttributes : List (Attribute msg) -> Layout msg
fromAttributes =
    Layout


{-| -}
toAttributes : Layout msg -> List (Attribute msg)
toAttributes (Layout attrs) =
    attrs


{-| -}
batch : List (Layout msg) -> Layout msg
batch ls =
    fromAttributes <| List.concatMap toAttributes ls


{-| -}
none : Layout msg
none =
    Layout []


{-| -}
isNone : Layout msg -> Bool
isNone (Layout ls) =
    List.isEmpty ls



-- Attributes


{-| -}
fromAttribute : Attribute msg -> Layout msg
fromAttribute attr =
    Layout [ attr ]


{-| -}
style : String -> String -> Layout msg
style name val =
    fromAttribute <|
        Attributes.style name val


{-| -}
attribute : String -> String -> Layout msg
attribute name val =
    fromAttribute <|
        Attributes.attribute name val
