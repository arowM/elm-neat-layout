module Neat.Flow exposing
    ( Flow
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

{-| Module for [flow contents](https://developer.mozilla.org/en-US/docs/Web/HTML/Content_categories#flow_content).

You will use this module for two main purposes:

  - To add attributes or styles within a series of strings.

        import Html.Attributes exposing (class, href)
        import Neat.Flow as Flow
        import Neat.View as View

        sample : View SampleGap msg
        sample =
            View.fromFlows sampleGap
                [ Flow.fromString "Please "
                , Flow.fromString "contact us"
                    |> Flow.setNodeName "a"
                    |> Flow.setAttribute
                        (href "https://example.com/contact")
                , Flow.fromString " if you receive this "
                , Flow.fromString "error message"
                    |> Flow.setAttribute (class "error")
                , Flow.fromString " multiple times."
                ]

        -- <div>Please <a href="https://example.com/contact">contact us</a> if you receive this <span class="error">error message</span> multiple times.</div>

  - To generate HTML elements that are allowed to contain only flow content.

        import Html.Attributes exposing (attribute, selected, value)
        import Html.Events as Events
        import Neat.Boundary as Boundary
        import Neat.Flow as Flow
        import Neat.View as View

        sampleSelect : Model -> Boundary Msg
        sampleSelect model =
            View.fromFlows sampleGap
                [ Flow.fromString "-- Select one --"
                    |> Flow.setNodeName "option"
                    |> Flow.setAttributes
                        [ attribute "value" ""
                        , selected <| model.selected == ""
                        ]
                , Flow.fromString "Goat"
                    |> Flow.setNodeName "option"
                    |> Flow.setAttributes
                        [ attribute "value" "goat"
                        , selected <| model.selected == "goat"
                        ]
                , Flow.fromString "Dog"
                    |> Flow.setNodeName "option"
                    |> Flow.setAttributes
                        [ attribute "value" "dog"
                        , selected <| model.selected == "dog"
                        ]
                ]
                |> View.setBoundary
                |> Boundary.setNodeName "select"
                |> Boundary.setAttributes
                    [ value model.selected
                    , Events.onInput ChangeChoice
                    ]


# Core

@docs Flow
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
type alias Flow msg =
    Internal.Flow msg



-- Primitive constructors


{-| Generate a `Flow` with given text content.
-}
fromString : String -> Flow msg
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
It can be used to realize `Flow`s only displayed under certain conditions, as shown below.

    fromFlows
        [ case mtext of
            Just t ->
                text t

            Nothing ->
                none
        ]

In most cases, however, a function like `when` will suffice.

-}
none : Flow msg
none =
    { mixin = Mixin.none
    , nodeName = Nothing
    , text = ""
    }



-- Mixin


{-| -}
setMixin : Mixin msg -> Flow msg -> Flow msg
setMixin mixin node =
    { node
        | mixin = Mixin.batch [ node.mixin, mixin ]
    }


{-| -}
setMixins : List (Mixin msg) -> Flow msg -> Flow msg
setMixins mixins node =
    { node
        | mixin = Mixin.batch <| node.mixin :: mixins
    }


{-| -}
setAttribute : Attribute msg -> Flow msg -> Flow msg
setAttribute attr =
    setMixin <| Mixin.fromAttributes [ attr ]


{-| -}
setAttributes : List (Attribute msg) -> Flow msg -> Flow msg
setAttributes =
    setMixin << Mixin.fromAttributes


{-| Append `class` attribute.
-}
setClass : String -> Flow msg -> Flow msg
setClass =
    setMixin << Mixin.class


{-| Append `id` attribute.
-}
setId : String -> Flow msg -> Flow msg
setId =
    setMixin << Mixin.id


{-| Set "role" value for WAI-ARIA.
-}
setRole : String -> Flow msg -> Flow msg
setRole str =
    setMixin <| Mixin.attribute "role" str


{-| Set "aria-\*" value for WAI-ARIA.

e.g., `setAria "required" "true"` stands for "aria-required" is "true".

-}
setAria : String -> String -> Flow msg -> Flow msg
setAria name v =
    setMixin <| Mixin.attribute ("aria-" ++ name) v


{-| Set boolean "aria-\*" value for WAI-ARIA.

i.e.,

  - `setBoolAria name True` is equal to `setAria name "true"`
  - `setBoolAria name False` is equal to `setAria name "false"`

-}
setBoolAria : String -> Bool -> Flow msg -> Flow msg
setBoolAria name g =
    setMixin <| Mixin.boolAttribute ("aria-" ++ name) g



-- Handle conditions


{-| -}
when : Bool -> Flow msg -> Flow msg
when p v =
    if p then
        v

    else
        none


{-| -}
unless : Bool -> Flow msg -> Flow msg
unless =
    when << not


{-| -}
withMaybe : Maybe a -> (a -> Flow msg) -> Flow msg
withMaybe ma f =
    case ma of
        Just a ->
            f a

        Nothing ->
            none



-- Lower level functions for HTML


{-| -}
setNodeName : String -> Flow msg -> Flow msg
setNodeName str text =
    { text
        | nodeName = Just str
    }


{-| -}
map : (a -> b) -> Flow a -> Flow b
map =
    Internal.mapFlow
