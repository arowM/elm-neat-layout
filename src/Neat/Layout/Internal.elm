module Neat.Layout.Internal exposing
    ( Layout
    , setInner
    , setOuter
    , fromInner
    , fromOuter
    , toInner
    , toOuter
    , batch
    , none
    , isNone
    )

{-| A brief module for Layouts.


# Core

@docs Layout
@docs setInner
@docs setOuter
@docs fromInner
@docs fromOuter
@docs toInner
@docs toOuter
@docs batch
@docs none
@docs isNone

-}

import Html exposing (Attribute)
import Html.Attributes as Attributes
import Mixin exposing (Mixin)



-- Core


type Layout msg
    = Layout (Mixin msg) (Mixin msg)


setOuter : Mixin msg -> Layout msg -> Layout msg
setOuter extra (Layout i o) =
    Layout i (Mixin.batch [ o, extra ])


setInner : Mixin msg -> Layout msg -> Layout msg
setInner extra (Layout i o) =
    Layout (Mixin.batch [ i, extra ]) o


fromInner : Mixin msg -> Layout msg
fromInner i =
    Layout i Mixin.none


fromOuter : Mixin msg -> Layout msg
fromOuter o =
    Layout Mixin.none o


toInner : Layout msg -> Mixin msg
toInner (Layout i o) =
    i


toOuter : Layout msg -> Mixin msg
toOuter (Layout i o) =
    o


{-| -}
batch : List (Layout msg) -> Layout msg
batch ls =
    let
        inner =
            Mixin.batch <| List.map toInner ls

        outer =
            Mixin.batch <| List.map toOuter ls
    in
    Layout inner outer


{-| -}
none : Layout msg
none =
    Layout Mixin.none Mixin.none


{-| -}
isNone : Layout msg -> Bool
isNone (Layout i o) =
    i == Mixin.none && o == Mixin.none
