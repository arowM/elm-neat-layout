module Repl exposing
    ( Repl
    , default
    , Msg
    , update
    , repl
    , setViewEditor
    , setGapEditor
    , showViewEditor
    , showGapEditor
    )

{-| Repl for the guide.

# Core

@docs Repl
@docs Msg
@docs update
@docs repl

# Repl builders

@docs default
@docs setViewEditor
@docs setGapEditor
@docs showViewEditor
@docs showGapEditor

-}

import Gap
import Html.Attributes as Attributes
import Html.Attributes.Classname exposing (classMixinWith)
import Html.Events as Events
import Mixin exposing (Mixin)
import Neat exposing (IsGap(..), NoGap, View, empty, emptyNode, fromNoGap, setAttribute, setLayout, setMixin, setMixins, textBlock)
import Neat.Boundary as Boundary exposing (Boundary, defaultBoundary)
import Neat.Layout as Layout
import Neat.Layout.Column as Column exposing (Column, column, columnWith, columnWithMap, defaultColumn)
import Neat.Layout.Row as Row exposing (Row, defaultRow, row, rowWith, rowWithMap)
import Reference exposing (Reference)
import Repl.Ast as Ast exposing (Ast)
import Repl.GapEditor as GapEditor exposing (GapEditor, gapEditor)
import Repl.Preview as Preview
import Repl.ViewEditor as ViewEditor exposing (ViewEditor)


{-| State of REPL.
-}
type Repl
    = Repl Model


type alias Model =
    { gapEditor : GapEditor
    , viewEditor : ViewEditor
    , showGapEditor : Bool
    , showViewEditor : Bool
    }


{-| Msg for REPL.
-}
type Msg
    = UpdateGapEditor GapEditor.Msg
    | UpdateViewEditor ViewEditor.Msg
    | ToggleShowViewEditor
    | ToggleShowGapEditor


{-| Constructor for `Repl`.
-}
default : Repl
default =
    Repl
    { gapEditor = GapEditor.fromList []
    , viewEditor = ViewEditor.fromAst <| Ast.none
    , showGapEditor = False
    , showViewEditor = False
    }



-- Setters


setGapEditor : GapEditor -> Repl -> Repl
setGapEditor v (Repl o) =
    Repl { o | gapEditor = v }


setViewEditor : ViewEditor -> Repl -> Repl
setViewEditor v (Repl o) =
    Repl { o | viewEditor = v }


showGapEditor : Repl -> Repl
showGapEditor (Repl o) =
    Repl { o | showGapEditor = True }


showViewEditor : Repl -> Repl
showViewEditor (Repl o) =
    Repl { o | showViewEditor = True }


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

        ToggleShowViewEditor ->
            ( Repl
                { model |
                    showViewEditor = not model.showViewEditor
                }
            , Cmd.none
            )

        ToggleShowGapEditor ->
            ( Repl
                { model |
                    showGapEditor = not model.showGapEditor
                }
            , Cmd.none
            )


{-| View for REPL
-}
repl : (Msg -> msg) -> Repl -> View NoGap msg
repl f (Repl model) =
    repl_ f model


repl_ : (Msg -> msg) -> Model -> View NoGap msg
repl_ f model =
    columnWithMap
        f
        defaultColumn
        [ rowWith
            { defaultRow
                | horizontal = Row.Right
            }
            [ editorTabButton
                { onClick = ToggleShowViewEditor
                , label = "View"
                }
                model.showViewEditor
            , editorTabButton
                { onClick = ToggleShowGapEditor
                , label = "Gap"
                }
                model.showGapEditor
            ]
            |> setLayout Layout.fill
            |> Neat.setBoundary Gap.editor
            |> setClass "repl_title"
        , column
            [ Preview.view
                { viewEditor = model.viewEditor }
                |> fromNoGap Gap.repl
                |> setLayout Layout.fill

            , editor
                { toMsg = UpdateViewEditor
                , title = "View"
                }
                [ ViewEditor.view
                    { gapEditor = model.gapEditor
                    }
                    model.viewEditor
                ]
                |> fromNoGap Gap.repl
                |> setLayout Layout.fill
                |> Neat.when model.showViewEditor
            , editor
                { toMsg = UpdateGapEditor
                , title = "Gap"
                }
                [ gapEditor model.gapEditor
                ]
                |> fromNoGap Gap.repl
                |> setLayout Layout.fill
                |> Neat.when model.showGapEditor
            ]
            |> Neat.fromNoGap Gap.editor
            |> Neat.setBoundary Gap.editor
            |> setClass "repl_body"
            |> setLayout Layout.fill
        ]
        |> setClass "window"


editor : { toMsg : msg -> Msg, title : String } -> List (View NoGap msg) -> View NoGap Msg
editor { title, toMsg } view =
    column
        [ row
            [ textBlock title
                |> Neat.fromNoGap Gap.editor
            ]
            |> Neat.setBoundary Gap.editor
            |> setClass "window_title"
        , columnWithMap
            toMsg
            defaultColumn
            view
            |> Neat.fromNoGap Gap.editor
            |> Neat.setBoundary Gap.editor
            |> setClass "window_body"
            |> setLayout Layout.fill
        ]
        |> setClass "window"


editorTabButton : { onClick : Msg, label : String } -> Bool -> View Gap.Editor Msg
editorTabButton { onClick, label } show =
                textBlock label
                  |> Neat.fromNoGap Gap.editor
                  |> Neat.setBoundaryWith
                    { defaultBoundary
                        | nodeName = "button"
                        , vertical = Boundary.VCenter
                        , horizontal = Boundary.HCenter
                    }
                    Gap.editor
                    |> Neat.setAttributes
                        [ Attributes.type_ "button"
                        , Events.onClick onClick
                        ]
                    |> setClass "repl_tabButton"
                    |> Neat.setBoolAria "checked" show
                    |> Neat.fromNoGap Gap.editor

-- Helper functions


class : String -> Mixin msg
class =
    classMixinWith <| \name -> "repl__" ++ name


setClass : String -> View NoGap msg -> View NoGap msg
setClass =
    setMixin << class
