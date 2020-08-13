module Repl.Ast exposing
    ( Ast(..)
    , resultingGap
    , unmodifiedGap
    , Modifier(..)
    , gapOfModifier
    , modifiersOf
    , setAccumulatedGaps
    )

{-| Simplified AST for elm-neat-layout

To avoid type constrains, this module uses **bad** hack which assumes every gap as a `NoGap`.
Usually, you should define and use the appropriate type for each gap.

@docs Ast
@docs resultingGap
@docs unmodifiedGap
@docs Modifier
@docs gapOfModifier
@docs modifiersOf
@docs setAccumulatedGaps

-}

import Dict
import EList exposing (EList)
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
import Repl.Ast.Gap as AstGap exposing (AstGap, GapSize)



-- AST Core


{-| -}
type Ast
    = TextBlock String (EList Modifier)
    | Empty (EList Modifier)
    | RowWith (Maybe Row) (EList Ast) (EList Modifier)
    | ColumnWith (Maybe Column) (EList Ast) (EList Modifier)
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



-- Gap calculations


modifiedGap : AstGap -> EList Modifier -> AstGap
modifiedGap init =
    List.foldl (\mod acc -> AstGap.mappend acc <| gapOfModifier mod) init << EList.toList


{-| -}
gapOfModifier : Modifier -> AstGap
gapOfModifier mod =
    case mod of
        ExpandTo (Just gap) ->
            gap

        _ ->
            AstGap.Undetermined


{-| -}
unmodifiedGap : Ast -> AstGap
unmodifiedGap ast =
    case ast of
        TextBlock _ _ ->
            AstGap.NoGap

        Empty _ ->
            AstGap.NoGap

        RowWith _ children _ ->
            EList.toList children
                |> List.map resultingGap
                |> AstGap.reduceChildGaps

        ColumnWith _ children _ ->
            EList.toList children
                |> List.map resultingGap
                |> AstGap.reduceChildGaps

        Unselected ->
            AstGap.Undetermined


{-| -}
resultingGap : Ast -> AstGap
resultingGap ast =
    modifiedGap (unmodifiedGap ast) (modifiersOf ast)


{-| -}
modifiersOf : Ast -> EList Modifier
modifiersOf ast =
    case ast of
        TextBlock _ mods ->
            mods

        Empty mods ->
            mods

        RowWith _ _ mods ->
            mods

        ColumnWith _ _ mods ->
            mods

        Unselected ->
            EList.empty


{-| -}
setAccumulatedGaps : AstGap -> EList Modifier -> EList ( AstGap, Modifier )
setAccumulatedGaps init mods =
    EList.zipWithList
        (List.scanl (\mod acc -> AstGap.mappend acc <| gapOfModifier mod) init <| EList.toList mods)
        mods
