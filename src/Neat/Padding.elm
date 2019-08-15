module Neat.Padding exposing
    ( NoPadding
    , IsPadding(..)
    , fromNoPadding
    , expand
    , setBoundary
    )

{-|


# Primitive

@docs NoPadding


# Custom paddings

You can introduce custom paddings by just declaring their types and `IsPadding` values.

    type MyPadding
        = MyPadding

    myPadding : IsPadding MyPadding
    myPadding =
        IsPadding
            { rem = 0.6
            }

@docs IsPadding


# Convert between paddings

@docs fromNoPadding
@docs expand
@docs setBoundary

-}

import Html exposing (Html)
import Mixin exposing (Mixin)
import Neat.Internal as Internal exposing (View, div)
import Neat.Layout.Internal as Layout exposing (Layout)



-- Primitives


{-| Primitive type representing no padding on a view.
-}
type NoPadding
    = NoPadding


{-| Information about your custom paddings.

  - rem : padding width in units of `rem`

-}
type IsPadding p
    = IsPadding
        { rem : Float
        }



-- Convert between paddings


{-| -}
fromNoPadding : IsPadding p -> View NoPadding msg -> View p msg
fromNoPadding =
    expand noPadding


{-| Expand padding from `p1` to `p2`.
The width of `p2` is supposed to be greater than `p1`.
-}
expand : IsPadding p1 -> IsPadding p2 -> View p1 msg -> View p2 msg
expand p1 p2 child =
    Internal.setLayout (expandPadding p1 p2) child
        |> Internal.coerce


expandPadding : IsPadding p1 -> IsPadding p2 -> Layout msg
expandPadding (IsPadding c1) (IsPadding c2) =
    Layout.none
        |> Layout.setOuter
            (Mixin.attribute "style" <|
                "padding:"
                    ++ String.fromFloat ((c2.rem - c1.rem) / 2)
                    ++ "rem"
            )



-- Boundary


{-| Set boundary to convert into `NoPadding`.
-}
setBoundary : IsPadding p -> View p msg -> View NoPadding msg
setBoundary config child =
    Internal.coerce <|
        Internal.div
            [ innerPadding config
            ]
            [ child
            ]


innerPadding : IsPadding p -> Mixin msg
innerPadding config =
    Mixin.attribute "style" <|
        "padding: "
            ++ innerPaddingValue config


innerPaddingValue : IsPadding p -> String
innerPaddingValue (IsPadding { rem }) =
    String.fromFloat (rem / 2) ++ "rem"



-- Helper functions


noPadding : IsPadding NoPadding
noPadding =
    IsPadding
        { rem = 0
        }
