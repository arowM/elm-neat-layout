module Neat.Internal exposing
    ( Gap
    , Renderer
    , View
    , fromHtml
    , toHtml
    , unsafeMap
    , unwrap
    )

import Html exposing (Html)
import Mixin exposing (Mixin)
import Neat.Layout.Internal as Layout exposing (Layout(..))


type View gap msg
    = View (Renderer -> Gap -> Layout msg -> Mixin msg -> Html msg)


type alias Gap =
    { width : Float
    , height : Float
    }


type alias Renderer =
    { baseGapSize : String
    }


toHtml : Renderer -> Gap -> Layout msg -> Mixin msg -> View gap msg -> Html msg
toHtml renderer gap layout appearance (View f) =
    f renderer gap layout appearance


unwrap : View gap msg -> Renderer -> Gap -> Layout msg -> Mixin msg -> Html msg
unwrap (View f) =
    f


fromHtml : (Renderer -> Gap -> Layout msg -> Mixin msg -> Html msg) -> View gap msg
fromHtml =
    View


unsafeMap : (a -> b) -> View g a -> View g b
unsafeMap f view =
    fromHtml <|
        \r g _ _ ->
            Html.map f <| toHtml r g Layout.none Mixin.none view
