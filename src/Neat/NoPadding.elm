module Neat.NoPadding exposing
    ( NoPadding
    , Atom
    , ratio
    )

{-| Functions for View without paddings.


# Core

@docs NoPadding
@docs Atom
@docs ratio

-}

import Neat.Helper.Ratio as Ratio exposing (Ratio)
import Neat.Internal exposing (View)



-- Core


{-| A type that indecates a view has no padding.
-}
type NoPadding
    = NoPadding


{-| An alias for convenience.
-}
type alias Atom msg =
    View NoPadding msg


{-| -}
ratio : Ratio NoPadding
ratio =
    Ratio.fromFloat 0
