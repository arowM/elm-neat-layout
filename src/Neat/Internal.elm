module Neat.Internal exposing
    ( View(..)
    , coerce
    , fromHtml
    , lift
    , setMixin
    , toHtml
    )

import Html exposing (Attribute, Html, div)
import Mixin exposing (Mixin)


type View padding msg
    = View (Mixin msg -> Html msg)


setMixin : Mixin msg -> View p msg -> View p msg
setMixin mixin (View f) =
    View <| \extra -> f <| Mixin.batch [ mixin, extra ]


toHtml : List (Attribute msg) -> View padding msg -> Html msg
toHtml attrs (View html) =
    html <| Mixin.fromAttributes attrs


fromHtml : (List (Attribute msg) -> Html msg) -> View padding msg
fromHtml f =
    View <|
        \mixin ->
            f <| Mixin.toAttributes mixin


coerce : View p1 a -> View p2 a
coerce (View f) =
    View f


lift : (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Mixin msg) -> List (View p msg) -> View p msg
lift node mixins children =
    fromHtml <|
        \extra ->
            node (List.concatMap Mixin.toAttributes mixins ++ extra) <|
                List.map (toHtml []) children
