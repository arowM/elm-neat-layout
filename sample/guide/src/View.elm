module View exposing
    ( emptyLine
    , textInput
    , TextInput
    )

{-| Utility view functions

@docs emptyLine
@docs textInput
@docs TextInput

-}

import Html.Attributes as Attributes
import Html.Attributes.Classname exposing (classMixinWith)
import Html.Events as Events
import Html.Events.Extra as Events
import Mixin exposing (Mixin)
import Neat exposing (NoGap, View, setAttribute, setMixin)


{-| Empty line for columns.
-}
emptyLine : View NoGap msg
emptyLine =
    Neat.empty
        |> setClass "emptyLine"


{-| Input box for text entry
-}
textInput : TextInput msg -> String -> View NoGap msg
textInput o value =
    Neat.input
        |> Neat.setAttributes
            [ Attributes.type_ "text"
            , Events.onChange o.onChange
            ]
        |> setValue value
        |> setClass "input"


{-| Configuration for `textInput`.
-}
type alias TextInput msg =
    { onChange : String -> msg
    }



{-| Select box
-}
select : Select msg -> String -> View NoGap msg
select o value =
    Neat.select []
        (List.map
            (\( label, value ) ->
                Neat.textNode Html.option label
                    |> setValue value
            )
            (( "", "" ) :: o.options)
        )
        |> setClass "select"
        |> Neat.setAttributes
            [ Events.onChange o.onChange
            ]
        |> setValue value


type alias Select msg =
    { options : List (String, String)
    , onChange : String -> msg
    }


-- Helper functions


class : String -> Mixin msg
class =
    classMixinWith <| \name -> "view__" ++ name


setClass : String -> View NoGap msg -> View NoGap msg
setClass =
    setMixin << class


setValue : String -> View NoGap msg -> View NoGap msg
setValue v =
    setAttribute (Attributes.value v)
