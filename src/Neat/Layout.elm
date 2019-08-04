module Neat.Layout exposing
    ( Layout
    , row
    , rowWith
    , column
    , columnWith
    , fill
    , fillBy
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

-}

import Html.Attributes as Attributes
import Neat.Internal exposing (View)
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
    Layout.batch
        [ style "-webkit-box-flex" <| String.fromInt n
        , style "-ms-flex-positive" <| String.fromInt n
        , style "flex-grow" <| String.fromInt n
        ]



-- Helper functions


style : String -> String -> Layout msg
style name val =
    Layout.fromAttribute <| Attributes.style name val
