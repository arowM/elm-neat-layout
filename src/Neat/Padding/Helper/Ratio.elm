module Neat.Padding.Helper.Ratio exposing
    ( Ratio
    , fromFloat
    , convert
    )

{-| Helper module to create additional paddings.

@docs Ratio
@docs fromFloat
@docs convert

-}

import Html
import Html.Attributes as Attributes
import Mixin exposing (Mixin)
import Neat.Internal as Internal exposing (View)



-- Core


{-| Padding ratio to `FullPadding`.
-}
type Ratio p
    = Ratio Float


{-| Constructor for `Ratio`.
Takes float value of `"Width of p" / "Width of FullPadding"`.
-}
fromFloat : Float -> Ratio p
fromFloat =
    Ratio


{-| Convert padding from `p1` to `p2`.
The width of `p1` is supposed to be less than `p2`.
-}
convert : Ratio p1 -> Ratio p2 -> View p1 msg -> View p2 msg
convert (Ratio r1) (Ratio r2) child =
    Internal.coerce <|
        Internal.lift Html.div
            [ Mixin.fromAttribute <|
                Attributes.style "padding" <|
                    String.fromFloat ((r2 - r1) / 2)
                        ++ "rem"
            ]
            [ child
            ]
