module Neat.Text exposing
    ( Text
    , map
    , fromString
    , setMixin
    , setMixins
    , setAttribute
    , setAttributes
    , setClass
    , setId
    , setRole
    , setAria
    , setBoolAria
    , none
    , when
    , unless
    , withMaybe
    , setNodeName
    )

{-| Module for inline texts.
This module is for creating special texts. e.g., text that has links only to parts of it.


# Core

@docs Text
@docs map


# Primitive constructors

@docs fromString


# Attributes

@docs setMixin
@docs setMixins
@docs setAttribute
@docs setAttributes
@docs setClass
@docs setId


# WAI-ARIA

@docs setRole
@docs setAria
@docs setBoolAria


# Handle conditions

@docs none
@docs when
@docs unless
@docs withMaybe


# Lower level functions for HTML

@docs setNodeName

-}

import Html exposing (Attribute)
import Mixin exposing (Mixin)
import Neat.Internal as Internal


{-| Representing a text.
-}
type alias Text msg =
    Internal.Text msg



-- Primitive constructors


{-| Generate a `Text` with given text.
-}
fromString : String -> Text msg
fromString str =
    case str of
        "" ->
            none

        _ ->
            { mixin = Mixin.none
            , nodeName = Nothing
            , text = str
            }


{-| Generate Nothing.
It can be used to realize `Text`s only displayed under certain conditions, as shown below.
In most cases, a function such as `when` will suffice, though.

    fromTexts
        [ case mtext of
            Just t ->
                text t

            Nothing ->
                none
        ]

-}
none : Text msg
none =
    { mixin = Mixin.none
    , nodeName = Nothing
    , text = ""
    }



-- Mixin


{-| -}
setMixin : Mixin msg -> Text msg -> Text msg
setMixin mixin node =
    { node
        | mixin = Mixin.batch [ node.mixin, mixin ]
    }


{-| -}
setMixins : List (Mixin msg) -> Text msg -> Text msg
setMixins mixins node =
    { node
        | mixin = Mixin.batch <| node.mixin :: mixins
    }


{-| -}
setAttribute : Attribute msg -> Text msg -> Text msg
setAttribute attr =
    setMixin <| Mixin.fromAttributes [ attr ]


{-| -}
setAttributes : List (Attribute msg) -> Text msg -> Text msg
setAttributes =
    setMixin << Mixin.fromAttributes


{-| Append `class` attribute.
-}
setClass : String -> Text msg -> Text msg
setClass =
    setMixin << Mixin.class


{-| Append `id` attribute.
-}
setId : String -> Text msg -> Text msg
setId =
    setMixin << Mixin.id


{-| Set "role" value for WAI-ARIA.
-}
setRole : String -> Text msg -> Text msg
setRole str =
    setMixin <| Mixin.attribute "role" str


{-| Set "aria-\*" value for WAI-ARIA.

e.g., `setAria "required" "true"` stands for "aria-required" is "true".

-}
setAria : String -> String -> Text msg -> Text msg
setAria name v =
    setMixin <| Mixin.attribute ("aria-" ++ name) v


{-| Set boolean "aria-\*" value for WAI-ARIA.

i.e.,

  - `setBoolAria name True` is equal to `setAria name "true"`
  - `setBoolAria name False` is equal to `setAria name "false"`

-}
setBoolAria : String -> Bool -> Text msg -> Text msg
setBoolAria name g =
    setMixin <| Mixin.boolAttribute ("aria-" ++ name) g



-- Handle conditions


{-| -}
when : Bool -> Text msg -> Text msg
when p v =
    if p then
        v

    else
        none


{-| -}
unless : Bool -> Text msg -> Text msg
unless =
    when << not


{-| -}
withMaybe : Maybe a -> (a -> Text msg) -> Text msg
withMaybe ma f =
    case ma of
        Just a ->
            f a

        Nothing ->
            none



-- Lower level functions for HTML


{-| -}
setNodeName : String -> Text msg -> Text msg
setNodeName str text =
    { text
        | nodeName = Just str
    }


{-| -}
map : (a -> b) -> Text a -> Text b
map =
    Internal.mapText
