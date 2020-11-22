module Neat.Size.Internal exposing (Size(..))

{-| A brief module for Size.


# Core

@docs Size

-}

import Neat.Internal exposing (Gap, Renderer)
import Neat.Layout.Internal exposing (Layout)



-- Core


type Size msg
    = Size (Renderer -> Gap -> Layout msg)
