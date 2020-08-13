module Repl.Preview exposing
    ( preview
    , Config
    )

{-| Preview window for REPL.

@docs preview
@docs Config

-}

import Gap
import Html.Attributes.Classname exposing (classMixinWith)
import Mixin exposing (Mixin)
import Neat exposing (IsGap(..), NoGap, View, empty, fromNoGap, setAttribute, setLayout, setMixin, setMixins, textBlock)
import Neat.Layout as Layout
import Neat.Layout.Column as Column exposing (Column, column, columnWith, defaultColumn)
import Neat.Layout.Row as Row exposing (Row, defaultRow, row, rowWith, rowWithMap)
import Repl.Ast as Ast exposing (Ast(..), Modifier(..), modifiersOf, resultingGap, setAccumulatedGaps, unmodifiedGap)
import Repl.Ast.Gap as AstGap exposing (AstGap)
import Repl.ViewEditor as ViewEditor exposing (ViewEditor)


type alias Config =
    { viewEditor : ViewEditor
    }


{-| View for preview window
-}
preview : Config -> View NoGap msg
preview config =
    let
        ast =
            ViewEditor.toAst config.viewEditor
    in
    column
        [ textBlock "Preview"
            |> Neat.fromNoGap Gap.preview
            |> Neat.setBoundary Gap.preview
            |> setClass "preview-title"
        , row
            [ previewCore ast
            ]
            |> (case resultingGap ast of
                    AstGap.Gap name size ->
                        Neat.setBoundary (IsGap size)

                    AstGap.NoGap ->
                        identity

                    AstGap.Invalid _ ->
                        setClass "invalid"

                    AstGap.Undetermined ->
                        identity
               )
            |> setClass "preview-body-inner"
            |> Neat.fromNoGap Gap.preview
            |> Neat.setBoundary Gap.preview
            |> setClass "preview-body"
            |> setLayout Layout.fill
        ]
        |> setClass "preview"


previewCore : Ast -> View NoGap msg
previewCore ast =
    previewUnmodified ast
        |> applyModifiers ast


previewUnmodified : Ast -> View NoGap msg
previewUnmodified ast =
    case ast of
        TextBlock str mods ->
            textBlock str

        Empty mods ->
            empty

        RowWith align asts mods ->
            rowWith (Maybe.withDefault defaultRow align)
                (List.map previewCore asts)

        ColumnWith align asts mods ->
            columnWith (Maybe.withDefault defaultColumn align)
                (List.map previewCore asts)

        _ ->
            Neat.none


applyModifiers : Ast -> View NoGap msg -> View NoGap msg
applyModifiers ast =
    List.foldr (>>)
        identity
        (List.map previewMod <| setAccumulatedGaps (unmodifiedGap ast) (modifiersOf ast))


previewMod : ( AstGap, Modifier ) -> View NoGap msg -> View NoGap msg
previewMod ( accumGap, mod ) =
    case mod of
        SetClass str ->
            setClass str

        SetLayoutFill ->
            setLayout <| Layout.fillBy 1

        SetLayoutFillBy n ->
            setLayout <| Layout.fillBy n

        SetLayoutNoShrink ->
            setLayout <| Layout.shrinkBy 0

        SetLayoutShrinkBy n ->
            setLayout <| Layout.shrinkBy n

        ExpandTo mgap2 ->
            case ( mgap2, accumGap ) of
                ( Just (AstGap.Gap _ gap2), AstGap.Gap _ gap1 ) ->
                    Neat.expand (IsGap gap1) (IsGap gap2)

                ( Just (AstGap.Gap _ gap2), AstGap.NoGap ) ->
                    Neat.expand (IsGap { width = 0, height = 0 }) (IsGap gap2)

                _ ->
                    identity



-- Helper functions


class : String -> Mixin msg
class =
    classMixinWith <| \name -> "repl_-preview__" ++ name


setClass : String -> View NoGap msg -> View NoGap msg
setClass =
    setMixin << class
