module Repl.Preview exposing
    ( Preview
    , preview
    )


{-| Preview window for REPL.

# Model

@docs Preview

# View

@docs preview

-}


type Preview = Preview Model


type alias Model =
    { gapEditor : GapEditor
    , ast : A

{-| View for preview window
-}
preview : Ast -> View NoGap Msg
preview ast =
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


previewCore : Ast -> View NoGap Msg
previewCore ast =
    previewUnmodified ast
        |> applyModifiers ast


previewUnmodified : Ast -> View NoGap Msg
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


applyModifiers : Ast -> View NoGap Msg -> View NoGap Msg
applyModifiers ast =
    List.foldr (>>)
        identity
        (List.map previewMod <| setAccumulatedGaps (unmodifiedGap ast) (modifiersOf ast))


previewMod : ( AstGap, Modifier ) -> View NoGap Msg -> View NoGap Msg
previewMod ( accumGap, mod ) =
    case mod of
        SetClass str ->
            setClass str

        SetLayoutFill ->
            setLayoutFillBy 1

        SetLayoutFillBy n ->
            setLayoutFillBy n

        SetLayoutNoShrink ->
            setLayoutShrinkBy 0

        SetLayoutShrinkBy n ->
            setLayoutShrinkBy n

        ExpandTo mgap2 ->
            case ( mgap2, accumGap ) of
                ( Just (AstGap.Gap _ gap2), AstGap.Gap _ gap1 ) ->
                    Neat.expand (IsGap gap1) (IsGap gap2)

                ( Just (AstGap.Gap _ gap2), AstGap.NoGap ) ->
                    Neat.expand (IsGap { width = 0, height = 0 }) (IsGap gap2)

                _ ->
                    identity


