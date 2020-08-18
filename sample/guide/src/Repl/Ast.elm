module Repl.Ast exposing
    ( Ast
    , accumGap
    , Tree(..)
    , unmodifiedGap
    , Modifier(..)
    , textBlock
    , empty
    , row
    , column
    , none
    , accumulatedGaps
    , gapOfModifier
    )

{-| Simplified AST for elm-neat-layout

To avoid type constrains, this module uses **bad** hack which assumes every gap as a `NoGap`.
Usually, you should define and use the appropriate type for each gap.

@docs Ast
@docs accumGap
@docs Tree
@docs Modifier


# Constructors

@docs textBlock
@docs empty
@docs row
@docs column
@docs none


# Calculators

@docs accumulatedGaps
@docs unmodifiedGap
@docs gapOfModifier

-}

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
import Repl.AccumGap as AccumGap exposing (AccumGap)
import Repl.Gap exposing (Gap)



-- AST Core


{-| -}
type Ast
    = Ast Model


type alias Model =
    { tree : Tree
    , modifier : List Modifier
    , resultingGap : AccumGap -- For memoization
    }

accumGap : Ast -> AccumGap
accumGap (Ast model) = model.resultingGap

unAst : Ast -> Model
unAst (Ast model) =
    model


{-| -}
type Tree
    = TextBlock String
    | Empty
    | Row Row (List Ast)
    | Column Column (List Ast)
    | None


{-| Modifiers for View
-}
type Modifier
    = SetClass String
    | SetLayoutFill
    | SetLayoutFillBy Float
    | SetLayoutNoShrink
    | SetLayoutShrinkBy Float
    | ExpandTo (Maybe Gap)



-- Constructors


textBlock : String -> List Modifier -> Ast
textBlock name mods =
    Ast
        { tree = TextBlock name
        , modifier = mods
        , resultingGap = resultingGap (TextBlock name) mods
        }


empty : List Modifier -> Ast
empty mods =
    Ast
        { tree = Empty
        , modifier = mods
        , resultingGap = resultingGap Empty mods
        }


row : Row -> List Ast -> List Modifier -> Ast
row align children mods =
    Ast
        { tree = Row align children
        , modifier = mods
        , resultingGap = resultingGap (Row align children) mods
        }


column : Column -> List Ast -> List Modifier -> Ast
column align children mods =
    Ast
        { tree = Column align children
        , modifier = mods
        , resultingGap = resultingGap (Column align children) mods
        }


none : Ast
none =
    Ast
        { tree = None
        , resultingGap = AccumGap.Undetermined
        , modifier = []
        }



-- Gap calculations


modifiedGap : AccumGap -> List Modifier -> AccumGap
modifiedGap init =
    List.foldl (\mod acc -> AccumGap.mappend acc <| gapOfModifier mod) init


gapOfModifier : Modifier -> AccumGap
gapOfModifier mod =
    case mod of
        ExpandTo (Just gap) ->
            AccumGap.Gap gap

        _ ->
            AccumGap.Undetermined


unmodifiedGap : Tree -> AccumGap
unmodifiedGap tree =
    case tree of
        TextBlock _ ->
            AccumGap.NoGap

        Empty ->
            AccumGap.NoGap

        Row _ children ->
            children
                |> List.map (.resultingGap << unAst)
                |> AccumGap.reduceChildGaps

        Column _ children ->
            children
                |> List.map (.resultingGap << unAst)
                |> AccumGap.reduceChildGaps

        None ->
            AccumGap.Undetermined


resultingGap : Tree -> List Modifier -> AccumGap
resultingGap tree mods =
    modifiedGap (unmodifiedGap tree) mods


{-| -}
accumulatedGaps : AccumGap -> List Modifier -> List AccumGap
accumulatedGaps init mods =
    List.scanl (\mod acc -> AccumGap.mappend acc <| gapOfModifier mod) init mods
