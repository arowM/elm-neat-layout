module Neat.NoPadding exposing
    ( NoPadding
    , Atom
    , toPage
    , ratio
    , text
    )

{-|


# Core

@docs NoPadding
@docs Atom
@docs toPage
@docs ratio


# Helper functions for Html

@docs text

-}

import Html exposing (Html)
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
-}
toPage : View NoPadding msg -> Html msg
toPage =
    Internal.toHtml []


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
