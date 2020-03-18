module Neat.Boundary exposing
    ( Boundary
    , Vertical(..)
    , Horizontal(..)
    , defaultBoundary
    )

{-|


# Core

@docs Boundary
@docs defaultBoudary
@docs Vertical
@docs Horizontal

-}


{-| Configuration about boundary.
-}
type alias Boundary =
    { vertical : Vertical
    , horizontal : Horizontal
    , nodeName : String
    }


{-| Default `Boundary` configuration.

    { vertical = VStretch
    , horizontal = HStretch
    , nodeName = "div"
    }

-}
defaultBoundary : Boundary
defaultBoundary =
    { vertical = VStretch
    , horizontal = HStretch
    , nodeName = "div"
    }



-- Horizontal alignment


{-| -}
type Horizontal
    = Left
    | Right
    | HCenter
    | HStretch



-- Vertical alignment


{-| -}
type Vertical
    = Top
    | Bottom
    | VCenter
    | VStretch
