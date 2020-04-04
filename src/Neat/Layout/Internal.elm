module Neat.Layout.Internal exposing
    ( Layout(..)
    , Layout_
    , batch
    , fromRecord
    , isImportant
    , makeImportant
    , map
    , none
    , setInner
    , setOuter
    , toRecord
    )

{-| A brief module for Layouts.
-}

import Mixin exposing (Mixin)



-- Core


type Layout msg
    = Layout IsImportant (Layout_ msg)


{-| Overwrite system layouts if True.
See `setLayout` in `Neat` module.
-}
type alias IsImportant =
    Bool


type alias Layout_ msg =
    { inner : Mixin msg
    , outer : Mixin msg
    }


map : (a -> b) -> Layout a -> Layout b
map f (Layout p { inner, outer }) =
    Layout p
        { inner = Mixin.map f inner
        , outer = Mixin.map f outer
        }


setOuter : Mixin msg -> Layout msg -> Layout msg
setOuter extra (Layout p layout_) =
    Layout p <|
        { layout_
            | outer =
                Mixin.batch
                    [ layout_.outer
                    , extra
                    ]
        }


setInner : Mixin msg -> Layout msg -> Layout msg
setInner extra (Layout p layout_) =
    Layout p <|
        { layout_
            | inner =
                Mixin.batch
                    [ layout_.inner
                    , extra
                    ]
        }


fromRecord : Layout_ msg -> Layout msg
fromRecord =
    Layout False


toRecord : Layout msg -> Layout_ msg
toRecord (Layout _ layout_) =
    layout_


isImportant : Layout msg -> Bool
isImportant (Layout p _) =
    p


makeImportant : Layout msg -> Layout msg
makeImportant (Layout _ layout_) =
    Layout True layout_


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
    Layout False <|
        { inner = Mixin.none
        , outer = Mixin.none
        }
