module Neat.NoPadding exposing
    ( NoPadding
    , Atom
    , toPage
    , PageConfig
    , defaultPageConfig
    , ratio
    , text
    )

{-|


# Core

@docs NoPadding
@docs Atom
@docs toPage
@docs PageConfig
@docs defaultPageConfig
@docs ratio


# Helper functions for Html

@docs text

-}

import Html exposing (Html)
import Html.Attributes exposing (style)
import Mixin exposing (Mixin)
import Neat exposing (View)
import Neat.Internal as Internal



-- Core


{-| A type that indecates a view has no padding.
-}
type NoPadding
    = NoPadding


{-| An alias for convenience.
-}
type alias Atom msg =
    View NoPadding msg


{-| Call this function **only once** on root view function.
Make sure that your own CSS is overwritten by following CSS.

    *,
    *::before,
    *::after {
      margin: 0;
      border: 0;
      padding: 0;
      box-sizing: border-box;
    }

-}
toPage : PageConfig msg -> View NoPadding msg -> Html msg
toPage setting v =
    Html.div []
        [ resetCss
        , Internal.toHtml (Mixin.toAttributes setting.mixin) v
        ]


{-| Configuration for `toPage`.
-}
type alias PageConfig msg =
    { mixin : Mixin msg
    }


{-| Default value for `PageConfig`.

    { mixin = Mixin.none
    }

-}
defaultPageConfig : PageConfig msg
defaultPageConfig =
    { mixin = Mixin.none
    }


resetCss : Html msg
resetCss =
    Html.node "style"
        []
        [ Html.text <|
            """
            *,
            *::before,
            *::after {
              margin: 0;
              border: 0;
              padding: 0;
              box-sizing: border-box;
            }
            """
        ]


{-| -}
ratio : Neat.Ratio NoPadding
ratio =
    Neat.ratio 0



-- Helper functions for Html


{-| `View` version of `Html.text`.
-}
text : String -> View NoPadding msg
text str =
    Internal.fromHtml <| \_ -> Html.text str
