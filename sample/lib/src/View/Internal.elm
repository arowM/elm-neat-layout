module View.Internal exposing
    ( View(..)
    , apply
    , class
    , convert
    , convertRaw
    , fromHtml
    , fullPadding
    , keyed
    , lift
    , middlePadding
    , narrowPadding
    , noPadding
    , setMixin
    , toHtml
    )

import Css
import Html exposing (Attribute, Html, div)
import Html.Keyed as Keyed
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


convert : ( String, String ) -> View p1 msg -> View p2 msg
convert fromTo (View html) =
    fromHtml <|
        convertRaw fromTo (html << Mixin.fromAttributes)


convertRaw : ( String, String ) -> (List (Attribute msg) -> Html msg) -> List (Attribute msg) -> Html msg
convertRaw ( from, to ) html attrs =
    div
        ([ class "convert"
         , class from
         , class to
         ]
            ++ attrs
        )
        [ html []
        ]


noPadding : String
noPadding =
    "noPadding"


narrowPadding : String
narrowPadding =
    "narrowPadding"


middlePadding : String
middlePadding =
    "middlePadding"


fullPadding : String
fullPadding =
    "fullPadding"


lift : (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Mixin msg) -> List (View p msg) -> View p msg
lift node mixins children =
    fromHtml <|
        \extra ->
            node (List.concatMap Mixin.toAttributes mixins ++ extra) <|
                List.map (toHtml []) children


apply : (Html a -> Html a) -> View p a -> View p a
apply f (View html) =
    fromHtml <|
        \attrs ->
            f <| html <| Mixin.fromAttributes attrs


class : String -> Attribute msg
class =
    Css.classWithPrefix "view__"



-- Keyed


{-| -}
keyed :
    String
    -> List (Mixin msg)
    -> List ( String, View p msg )
    -> View p msg
keyed tag mixin children =
    fromHtml <|
        \extra ->
            Keyed.node tag (List.concatMap Mixin.toAttributes mixin ++ extra) <|
                List.map (Tuple.mapSecond (toHtml [])) children
