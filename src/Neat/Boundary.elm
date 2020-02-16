module Neat.Boundary exposing
    ( Boundary
    , defaultBoundary
    , Vertical(..)
    , Horizontal(..)
    )

{-|


# Core

@docs Boundary
@docs defaultBoudary
@docs Vertical
@docs Horizontal

-}


import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Mixin exposing (Mixin)



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
