module Neat.FullPadding exposing
    ( FullPadding
    , setBoundary
    , fromNoPadding
    , ratio
    )

{-|


# Core

@docs FullPadding
@docs setBoundary
@docs fromNoPadding
@docs ratio

-}

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Mixin exposing (Mixin)
import Neat exposing (View)
import Neat.Internal as Internal
import Neat.NoPadding as NoPadding exposing (NoPadding)



-- Core


{-| A type that indecates a view has full padding.
-}
type FullPadding
    = FullPadding


{-| Convert to NoPadding by setting boundary.
-}
setBoundary : List (Mixin msg) -> List (View FullPadding msg) -> View NoPadding msg
setBoundary mixins children =
    Internal.coerce <|
        Neat.lift Html.div
            mixins
            [ Neat.lift Html.div
                [ fullPadding
                ]
                children
            ]


fullPadding : Mixin msg
fullPadding =
    Mixin.fromAttribute <|
        Attributes.style "padding" "0.5rem"


{-| -}
fromNoPadding : View NoPadding msg -> View FullPadding msg
fromNoPadding =
    Neat.convert NoPadding.ratio ratio


{-| -}
ratio : Neat.Ratio FullPadding
ratio =
    Neat.ratio 1
