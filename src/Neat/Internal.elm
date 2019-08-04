module Neat.Internal exposing
    ( View(..)
    , batch
    , coerce
    , div
    , fromHtml
    , lift
    , setLayout
    , setMixin
    , toHtml
    , wrapper
    )

import Html exposing (Attribute, Html, div)
import Mixin exposing (Mixin)
import Neat.Layout.Internal as Layout exposing (Layout)


type View padding msg
    = View (Layout msg -> Mixin msg -> Html msg)


setMixin : Mixin msg -> View p msg -> View p msg
setMixin appearance (View f) =
    View <|
        \layout extra ->
            f layout (Mixin.batch [ appearance, extra ])


setLayout : Layout msg -> View p msg -> View p msg
setLayout layout (View f) =
    View <|
        \extra appearance ->
            f (Layout.batch [ layout, extra ]) appearance


toHtml : Layout msg -> Mixin msg -> View padding msg -> Html msg
toHtml layout appearance (View f) =
    f layout appearance


fromHtml : (Layout msg -> Mixin msg -> Html msg) -> View padding msg
fromHtml =
    View


coerce : View p1 a -> View p2 a
coerce (View f) =
    View f


lift : (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Mixin msg) -> List (View p msg) -> View p msg
lift node appearances children =
    fromHtml <|
        \layout extra ->
            wrapper layout <|
                node
                    (Mixin.toAttributes <|
                        Mixin.batch
                            (appearances ++ [ extra ])
                    )
                <|
                    List.map (toHtml Layout.none Mixin.none) children


wrapper : Layout msg -> Html msg -> Html msg
wrapper layout =
    if Layout.isNone layout then
        identity

    else
        Html.div (Layout.toAttributes layout) << List.singleton


div : List (Mixin msg) -> List (View p msg) -> View p msg
div =
    lift Html.div


batch : List (View p a) -> View p a
batch =
    div []
