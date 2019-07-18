module Neat.FullPadding exposing
    ( FullPadding
    , fromNoPadding
    , ratio
    )

{-| Functions for View with full paddings.

@docs FullPadding
@docs fromNoPadding
@docs ratio

-}

import Neat.Helper.Ratio as Ratio exposing (Ratio)
import Neat.Internal exposing (View)
import Neat.NoPadding as NoPadding exposing (NoPadding)



-- Core


{-| A type that indecates a view has full padding.
-}
type FullPadding
    = FullPadding


{-| -}
fromNoPadding : View NoPadding msg -> View FullPadding msg
fromNoPadding =
    Ratio.convert NoPadding.ratio ratio


{-| -}
ratio : Ratio FullPadding
ratio =
    Ratio.fromFloat 1
