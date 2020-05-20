module Repl exposing
    ( Repl
    , fromRecord
    , Msg
    , update
    , repl
    )

{-| Repl for the guide.

@docs Repl
@docs fromRecord
@docs Msg
@docs update
@docs repl

-}

import Gap
import Html.Attributes.Classname exposing (classMixinWith)
import Mixin exposing (Mixin)
import Neat exposing (IsGap(..), NoGap, View, empty, fromNoGap, setAttribute, setLayout, setMixin, setMixins, textBlock)
import Neat.Layout as Layout
import Neat.Layout.Column as Column exposing (Column, column, columnWith, columnWithMap, defaultColumn)
import Neat.Layout.Row as Row exposing (Row, defaultRow, row, rowWith, rowWithMap)
import Reference exposing (Reference)
import Repl.Ast as Ast exposing (Ast)
import Repl.GapEditor as GapEditor exposing (GapEditor, gapEditor)
import Repl.Preview as Preview exposing (preview)
import Repl.ViewEditor as ViewEditor exposing (ViewEditor, viewEditor)


{-| State of REPL.
-}
type Repl
    = Repl Model


type alias Model =
    { gapEditor : GapEditor
    , viewEditor : ViewEditor
    }


{-| Msg for REPL.
-}
type Msg
    = UpdateGapEditor GapEditor.Msg
    | UpdateViewEditor ViewEditor.Msg


{-| Constructor for `Repl`.
-}
fromRecord : { gapEditor : GapEditor, viewEditor : ViewEditor } -> Repl
fromRecord =
    Repl


update : Msg -> Repl -> ( Repl, Cmd Msg )
update msg (Repl model) =
    case msg of
        UpdateGapEditor new ->
            GapEditor.update new model.gapEditor
                |> Tuple.mapFirst (\m -> Repl { model | gapEditor = m })
                |> Tuple.mapSecond (Cmd.map UpdateGapEditor)

        UpdateViewEditor new ->
            ViewEditor.update new model.viewEditor
                |> Tuple.mapFirst (\m -> Repl { model | viewEditor = m })
                |> Tuple.mapSecond (Cmd.map UpdateViewEditor)


{-| View for REPL
-}
repl : (Msg -> msg) -> Repl -> View Gap.Repl msg
repl f (Repl model) =
    repl_ f model


repl_ : (Msg -> msg) -> Model -> View Gap.Repl msg
repl_ f model =
    rowWithMap
        f
        { defaultRow
            | wrap = Row.Wrap
        }
        [ editor model
            |> fromNoGap Gap.repl
            |> setLayout Layout.fill
        , preview
            { viewEditor = model.viewEditor }
            |> fromNoGap Gap.repl
            |> setLayout Layout.fill
        ]


editor : Model -> View NoGap Msg
editor model =
    column
        [ textBlock "Editor"
            |> Neat.fromNoGap Gap.editor
            |> Neat.setBoundary Gap.editor
            |> setClass "editor-title"
        , columnWithMap
            UpdateViewEditor
            defaultColumn
            [ viewEditor { gapEditor = model.gapEditor } model.viewEditor
            ]
            |> Neat.fromNoGap Gap.editor
            |> Neat.setBoundary Gap.editor
            |> setClass "editor-body"
            |> setLayout Layout.fill
        ]
        |> setClass "editor"



-- Helper functions


class : String -> Mixin msg
class =
    classMixinWith <| \name -> "repl__" ++ name


setClass : String -> View NoGap msg -> View NoGap msg
setClass =
    setMixin << class
