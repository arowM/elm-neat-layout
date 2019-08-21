module Neat.Layout exposing
    ( Layout
    , row
    , rowWith
    , column
    , columnWith
    , fill
    , fillBy
    , noShrink
    , shrinkBy
    )

{-| Alignment functions.


# Core

@docs Layout


# Rows and Columns

@docs row
@docs rowWith
@docs column
@docs columnWith


# Fill items in Rows and Columns

@docs fill
@docs fillBy
@docs noShrink
@docs shrinkBy

-}

import Html.Attributes as Attributes
import Mixin exposing (Mixin)
import Neat exposing (View)
import Neat.Layout.Column as Column exposing (Column)
import Neat.Layout.Internal as Layout
import Neat.Layout.Row as Row exposing (Row)



-- Core


{-| -}
type alias Layout msg =
    Layout.Layout msg



-- Rows and Columns


{-| Re-exposing of `Neat.Alignment.Row.row`.
-}
row : List (View p msg) -> View p msg
row =
    Row.row


{-| Re-exposing of `Neat.Alignment.Row.rowWith`.

An alias for `rowWith Neat.Alignment.Row.default`.

-}
rowWith : Row -> List (View p msg) -> View p msg
rowWith =
    Row.rowWith


{-| Re-exposing of `Neat.Alignment.Column.column`.
-}
column : List (View p msg) -> View p msg
column =
    Column.column


{-| Re-exposing of `Neat.Alignment.Column.columnWith`.

An alias for `columnWith Neat.Alignment.Column.default`.

-}
columnWith : Column -> List (View p msg) -> View p msg
columnWith =
    Column.columnWith



-- Fill items in Rows and Columns


{-| Expand the item in row/column.

Shorthands for `fillBy 1`.

-}
fill : Layout msg
fill =
    fillBy 1


{-| Specifies how much of the remaining space in the row/column should be assigned to the item.
-}
fillBy : Int -> Layout msg
fillBy n =
    Layout.fromRecord
        { outer =
            Mixin.batch
                [ style "-ms-flex-positive" <| String.fromInt n
                , style "flex-grow" <| String.fromInt n
                ]
        , inner =
            Mixin.batch
                [ style "width" "100%"
                , style "height" "100%"
                ]
        }


{-| Prohibit to shrink the item in row/column.

Shorthands for `fillBy 0`.

-}
noShrink : Layout msg
noShrink =
    shrinkBy 0


{-| If the size of all flex items in row/column is larger than the container, items shrink to fit according to `shrinkBy`.
-}
shrinkBy : Int -> Layout msg
shrinkBy n =
    Layout.fromRecord
        { outer =
            Mixin.batch
                [ style "-ms-flex-negative" <| String.fromInt n
                , style "flex-shrink" <| String.fromInt n
                ]
        , inner = Mixin.none
        }



-- Helper functions


style : String -> String -> Mixin msg
style k v =
    Mixin.fromAttribute <| Attributes.style k v
