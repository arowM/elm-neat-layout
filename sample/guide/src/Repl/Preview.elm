module Repl.Preview exposing
    ( view
    , Config
    )

{-| Preview window for REPL.

@docs view
@docs Config

-}

import Gap
import Html.Attributes.Classname exposing (classMixinWith)
import Mixin exposing (Mixin)
import Neat exposing (IsGap(..), NoGap, View, empty, fromNoGap, setAttribute, setLayout, setMixin, setMixins, textBlock)
import Neat.Layout as Layout
import Neat.Layout.Column as Column exposing (Column, column, columnWith, defaultColumn)
import Neat.Layout.Row as Row exposing (Row, defaultRow, row, rowWith, rowWithMap)
import Repl.AccumGap as AccumGap exposing (AccumGap)
import Repl.Ast as Ast exposing (Ast(..), Modifier(..), unmodifiedGap)
import Repl.Gap as Gap
import Repl.ViewEditor as ViewEditor exposing (ViewEditor)


type alias Config =
    { viewEditor : ViewEditor
    }


{-| View for preview window
-}
view : Config -> View NoGap msg
view config =
    column
        [ textBlock "Preview"
            |> Neat.fromNoGap Gap.preview
            |> Neat.setBoundary Gap.preview
            |> setClass "preview-title"
        , (case ViewEditor.toAst config.viewEditor of
            Nothing ->
                Neat.none

            Just ast ->
                row
                    [ previewCore ast
                        |> setLayout Layout.fill
                    ]
                    |> (case Ast.accumGap ast of
                            AccumGap.Gap gap ->
                                Neat.setBoundary (Gap.toIsGap gap)

                            AccumGap.NoGap ->
                                identity

                            AccumGap.Invalid _ ->
                                setClass "invalid"

                            AccumGap.Undetermined ->
                                identity
                       )
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
    case Ast.tree ast of
        Ast.TextBlock str ->
            textBlock str

        Ast.Empty ->
            empty

        Ast.Row align asts ->
            rowWith align
                (List.map previewCore asts)

        Ast.Column align asts ->
            columnWith align
                (List.map previewCore asts)

        _ ->
            Neat.none


applyModifiers : Ast -> View NoGap msg -> View NoGap msg
applyModifiers ast =
    let
        mods =
            Ast.modifiers ast
    in
    List.foldr (>>)
        identity
        (List.map2 previewMod
            (Ast.accumulatedGaps (unmodifiedGap <| Ast.tree ast) mods)
            mods
        )


previewMod : AccumGap -> Modifier -> View NoGap msg -> View NoGap msg
previewMod accumGap mod =
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
                ( Just gap2, AccumGap.Gap gap1 ) ->
                    Neat.expand (Gap.toIsGap gap1) (Gap.toIsGap gap2)

                ( Just gap2, AccumGap.NoGap ) ->
                    Neat.expand (IsGap { width = 0, height = 0 }) (Gap.toIsGap gap2)

                _ ->
                    identity



-- Helper functions


class : String -> Mixin msg
class =
    classMixinWith <| \name -> "repl_-preview__" ++ name


setClass : String -> View NoGap msg -> View NoGap msg
setClass =
    setMixin << class
