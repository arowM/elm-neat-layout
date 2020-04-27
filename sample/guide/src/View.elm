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
import Mixin exposing (Mixin)
import Neat exposing (NoGap, View, setAttribute, setMixin)
import Reference exposing (Reference)


{-| Empty line for columns.
-}
emptyLine : View NoGap msg
emptyLine =
    Neat.empty
        |> setClass "emptyLine"


{-| Input box of text
-}
textInput : TextInput t p msg -> String -> View NoGap msg
textInput o value =
    Neat.input
        |> Neat.setAttribute (Attributes.type_ "text")
        |> setValue value
        |> updateWithRefOnInput o.toMsg o.ref o.onInput
        |> setClass "input"


{-| Configuration for `textInput`.
-}
type alias TextInput t p msg =
    { ref : Reference t p
    , onInput : String -> t
    , toMsg : p -> msg
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


updateWithRef : (p -> msg) -> Reference a p -> a -> msg
updateWithRef toMsg ref v =
    toMsg <| Reference.root <| Reference.modify (\_ -> v) ref


updateWithRefOnInput : (p -> msg) -> Reference a p -> (String -> a) -> View NoGap msg -> View NoGap msg
updateWithRefOnInput toMsg ref f =
    setAttribute <| Events.onInput <| \a -> updateWithRef toMsg ref <| f a
