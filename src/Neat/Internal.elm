module Neat.Internal exposing
    ( View(..)
    , coerce
    , div
    , fromHtml
    , keyedLazy
    , lift
    , setLayout
    , setMixin
    , toHtml
    )

import Html exposing (Attribute, Html, div)
import Html.Attributes as Attributes
import Html.Keyed as Keyed
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
    liftHelper node appearances <|
        List.map (toHtml Layout.none Mixin.none) children


keyedLazy :
    String
    -> List (Mixin msg)
    -> List ( String, Html msg )
    -> View p msg
keyedLazy tag =
    liftHelper (Keyed.node tag)


liftHelper : (List (Attribute msg) -> List a -> Html msg) -> List (Mixin msg) -> List a -> View p msg
liftHelper node appearances children =
    fromHtml <|
        \layout extra ->
            wrapHtml (Layout.toOuter layout) <|
                \extraAttrs ->
                    node
                        (Mixin.toAttributes <|
                            Mixin.batch
                                [ Mixin.batch appearances
                                , extra
                                , Layout.toInner layout
                                , Mixin.fromAttributes extraAttrs
                                ]
                        )
                        children


wrapHtml : Mixin msg -> (List (Attribute msg) -> Html msg) -> Html msg
wrapHtml outer f =
    if outer == Mixin.none then
        f []

    else
        Html.div (Mixin.toAttributes outer)
            [ f
                [ Attributes.attribute "style" <| "width: 100%; height: 100%;"
                ]
            ]


div : List (Mixin msg) -> List (View p msg) -> View p msg
div =
    lift Html.div
