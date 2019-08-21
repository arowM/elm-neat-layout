module Neat.Layout.Internal exposing
    ( Layout(..)
    , Layout_
    , setInner
    , setOuter
    , fromRecord
    , toRecord
    , batch
    , none
    )

{-| A brief module for Layouts.


# Core

@docs Layout
@docs Layout_
@docs setInner
@docs setOuter
@docs fromInner
@docs fromOuter
@docs fromRecord
@docs toRecord
@docs batch
@docs none

-}

import Mixin exposing (Mixin)



-- Core


type Layout msg
    = Layout (Layout_ msg)


type alias Layout_ msg =
    { inner : Mixin msg
    , outer : Mixin msg
    }


setOuter : Mixin msg -> Layout msg -> Layout msg
setOuter extra (Layout layout_) =
    Layout <|
        { layout_
            | outer =
                Mixin.batch
                    [ layout_.outer
                    , extra
                    ]
        }


setInner : Mixin msg -> Layout msg -> Layout msg
setInner extra (Layout layout_) =
    Layout <|
        { layout_
            | inner =
                Mixin.batch
                    [ layout_.inner
                    , extra
                    ]
        }


fromRecord : Layout_ msg -> Layout msg
fromRecord =
    Layout


toRecord : Layout msg -> Layout_ msg
toRecord (Layout layout_) =
    layout_


{-| -}
batch : List (Layout msg) -> Layout msg
batch ls =
    fromRecord <|
        { inner =
            Mixin.batch <| List.map (.inner << toRecord) ls
        , outer = Mixin.batch <| List.map (.outer << toRecord) ls
        }


{-| -}
none : Layout msg
none =
    Layout <|
        { inner = Mixin.none
        , outer = Mixin.none
        }
