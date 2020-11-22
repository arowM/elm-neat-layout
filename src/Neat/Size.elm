module Neat.Size exposing
    ( Size
    , width
    -- , minWidth
    -- , maxWidth
    -- , height
    -- , minHeight
    -- , maxHeight
    )

{-| Size functions.


# Core

@docs Size

# Width

@docs width
-- @docs minWidth
-- @docs maxWidth
-- 
-- # Height
-- 
-- @docs height
-- @docs minHeight
-- @docs maxHeight

-}

import Html.Attributes as Attributes
import Mixin exposing (Mixin)
import Neat exposing (View)
import Neat.Layout.Internal as Layout
import Neat.Size.Internal as Size



-- Core


{-| -}
type alias Size msg =
    Size.Size msg



-- Width


{-| -}
width : String -> Size msg
width v = Size.Size <| \renderer gap ->
    Layout.fromRecord
        { outer =
            Mixin.batch
                [ style "width" <|
                    "calc(" ++ v ++ " + " ++ String.fromFloat gap.width ++ " * " ++ renderer.baseGapSize ++ ")"
                ]
        , inner = Mixin.batch
            [ style "width" "100%"
            ]
        }
        |> Layout.makeImportant


{-
{-| -}
minWidth : String -> Size msg
minWidth v =
    Layout.fromRecord
        { outer =
            Mixin.batch
                [ style "min-width" v
                ]
        , inner = Mixin.none
        }
        |> Layout.makeImportant
        |> Size

{-| -}
maxWidth : String -> Size msg
maxWidth v =
    Layout.fromRecord
        { outer =
            Mixin.batch
                [ style "max-width" v
                ]
        , inner = Mixin.none
        }
        |> Layout.makeImportant
        |> Size


-- Height


{-| -}
height : String -> Size msg
height v =
    Layout.fromRecord
        { outer =
            Mixin.batch
                [ style "height" v
                ]
        , inner = Mixin.batch
            [ style "height" "100%"
            ]
        }
        |> Layout.makeImportant
        |> Size


{-| -}
minHeight : String -> Size msg
minHeight v =
    Layout.fromRecord
        { outer =
            Mixin.batch
                [ style "min-height" v
                ]
        , inner = Mixin.none
        }
        |> Layout.makeImportant
        |> Size

{-| -}
maxHeight : String -> Size msg
maxHeight v =
    Layout.fromRecord
        { outer =
            Mixin.batch
                [ style "max-height" v
                ]
        , inner = Mixin.none
        }
        |> Layout.makeImportant
        |> Size
-}


-- Helper functions


style : String -> String -> Mixin msg
style k v =
    Mixin.fromAttribute <| Attributes.style k v

