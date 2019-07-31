module Neat.Padding.MiddlePadding exposing
    ( MiddlePadding
    , fromNoPadding
    , toFullPadding
    , ratio
    )

{-| Main framework for managing paddings.


# Core

@docs MiddlePadding
@docs fromNoPadding
@docs toFullPadding
@docs ratio

-}

import Neat exposing (View)
import Neat.Padding.Helper.Ratio as Ratio exposing (Ratio)
import Neat.Padding.FullPadding as FullPadding exposing (FullPadding)
import Neat.Padding.NoPadding as NoPadding exposing (NoPadding)


{-| Type level implementation of middle padding.
-}
type MiddlePadding
    = MiddlePadding


{-| -}
fromNoPadding : View NoPadding msg -> View MiddlePadding msg
fromNoPadding =
    Ratio.convert NoPadding.ratio ratio


{-| -}
toFullPadding : View MiddlePadding msg -> View FullPadding msg
toFullPadding =
    Ratio.convert ratio FullPadding.ratio


{-| -}
ratio : Ratio MiddlePadding
ratio =
    Ratio.fromFloat 0.5
