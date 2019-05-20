module Neat.NoPadding exposing
    ( NoPadding
    , Atom
    , toPage
    , text
    )

{-|


# Core

@docs NoPadding
@docs Atom
@docs toPage


# Helper functions for Html

@docs text

-}

import Html exposing (Html)
import Neat exposing (View)



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
    Neat.unsafeToHtml



-- Helper functions for Html


{-| `View` version of `Html.text`. -}
text : String -> View NoPadding msg
text =
    Neat.unsafeFromHtml << Html.text
