module Neat.Internal exposing
    ( View(..)
    , batch
    , coerce
    , div
    , fromHtml
    , lift
    , setAppearance
    , setLayout
    , toHtml
    , wrapper
    )

import Html exposing (Attribute, Html, div)
import Neat.Appearance as Appearance exposing (Appearance)
import Neat.Layout.Internal as Layout exposing (Layout)


type View padding msg
    = View (Layout msg -> Appearance msg -> Html msg)


setAppearance : Appearance msg -> View p msg -> View p msg
setAppearance appearance (View f) =
    View <|
        \layout extra ->
            f layout (Appearance.batch [ appearance, extra ])


setLayout : Layout msg -> View p msg -> View p msg
setLayout layout (View f) =
    View <|
        \extra appearance ->
            f (Layout.batch [ layout, extra ]) appearance


toHtml : Layout msg -> Appearance msg -> View padding msg -> Html msg
toHtml layout appearance (View f) =
    f layout appearance


fromHtml : (Layout msg -> Appearance msg -> Html msg) -> View padding msg
fromHtml =
    View


coerce : View p1 a -> View p2 a
coerce (View f) =
    View f


lift : (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Appearance msg) -> List (View p msg) -> View p msg
lift node appearances children =
    fromHtml <|
        \layout extra ->
            wrapper layout <|
                node
                    (Appearance.toAttributes <|
                        Appearance.batch
                            (appearances ++ [ extra ])
                    )
                <|
                    List.map (toHtml Layout.none Appearance.none) children


wrapper : Layout msg -> Html msg -> Html msg
wrapper layout =
    if Layout.isNone layout then
        identity

    else
        Html.div (Layout.toAttributes layout) << List.singleton


div : List (Appearance msg) -> List (View p msg) -> View p msg
div =
    lift Html.div


batch : List (View p a) -> View p a
batch =
    div []
