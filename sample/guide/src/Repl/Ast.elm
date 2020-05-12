module Repl.Ast exposing
    ( Ast(..)
    , Modifier(..)
    )

{-| Simplified AST for elm-neat-layout

To avoid type constrains, this module uses **bad** hack which assumes every gap as a `NoGap`.
Usually, you should define and use the appropriate type for each gap.

@docs Ast
@docs Modifier

-}

import Ast.Gap as AstGap exposing (AstGap, GapSize)
import Dict
import Gap
import Html
import Html.Attributes as Attributes
import Html.Attributes.Classname exposing (classMixinWith)
import Html.Events as Events
import Html.Events.Extra as Events
import Json.Encode as Json
import List.Extra as List
import Maybe.Extra as Maybe
import Mixin exposing (Mixin)
import Neat exposing (IsGap(..), NoGap, View, empty, fromNoGap, setAttribute, setLayout, setMixin, setMixins, textBlock)
import Neat.Layout as Layout
import Neat.Layout.Column as Column exposing (Column, column, columnWith, defaultColumn)
import Neat.Layout.Row as Row exposing (Row, defaultRow, row, rowWith, rowWithMap)
import Reference exposing (Reference)
import Reference.List



-- AST Core


{-| -}
type Ast
    = TextBlock String (List Modifier)
    | Empty (List Modifier)
    | RowWith (Maybe Row) (List Ast) (List Modifier)
    | ColumnWith (Maybe Column) (List Ast) (List Modifier)
    | Unselected





{-| Modifiers for View
-}
type Modifier
    = SetClass String
    | SetLayoutFill
    | SetLayoutFillBy Int
    | SetLayoutNoShrink
    | SetLayoutShrinkBy Int
    | ExpandTo (Maybe AstGap)




