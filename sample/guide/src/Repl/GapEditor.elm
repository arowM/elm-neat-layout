module Repl.GapEditor exposing
    ( GapEditor
    , Msg
    , update
    , gapEditor
    , capitalizeHead
    , fromList
    , toList
    , find
    )

{-| Gap editor for REPL.


# Core

@docs GapEditor
@docs Msg
@docs update
@docs gapEditor
@docs capitalizeHead
@docs fromList
@docs toList
@docs find

-}

import Form.Decoder as FD exposing (Decoder)
import Gap
import Html.Attributes.Classname exposing (classMixinWith)
import Html.Events as Events
import Mixin exposing (Mixin)
import Neat exposing (NoGap, View, fromNoGap, setBoundary, setBoundaryWith, setMixin, textBlock)
import Neat.Boundary as Boundary exposing (Boundary, defaultBoundary)
import Neat.Layout.Column exposing (column)
import Neat.Layout.Row as Row exposing (Row, defaultRow, row, rowWith)
import Repl.Gap exposing (Gap)
import Repl.GapEditor.CreateForm as CreateForm exposing (CreateForm)
import View exposing (emptyLine)



-- Core


{-| Main type
-}
type GapEditor
    = GapEditor Model


{-| Construct `GapEditor` from list of gaps.
-}
fromList : List Gap -> GapEditor
fromList ls =
    GapEditor
        { createForm = CreateForm.init
        , showError = False
        , gaps = ls
        }


{-| Extract a list of Gaps from the GapEditor.
-}
toList : GapEditor -> List Gap
toList (GapEditor o) =
    o.gaps


{-| Search an available Gap by its name.
-}
find : String -> GapEditor -> Maybe Gap
find str (GapEditor { gaps }) =
    gaps
        |> List.filter (\g -> str == g.name)
        |> List.head


type alias Model =
    { createForm : CreateForm
    , showError : Bool
    , gaps : List Gap
    }


modelDecoder : Decoder Model CreateForm.Error Gap
modelDecoder =
    FD.lift .createForm CreateForm.decoder


{-|
-}
type Msg
    = PrependGap
    | UpdateCreateFormName String
    | UpdateCreateFormHeight String
    | UpdateCreateFormWidth String


{-|
-}
update : Msg -> GapEditor -> ( GapEditor, Cmd Msg )
update msg (GapEditor model) =
    update_ msg model
        |> Tuple.mapFirst GapEditor


update_ : Msg -> Model -> ( Model, Cmd Msg )
update_ msg model =
    case msg of
        PrependGap ->
            ( case FD.run CreateForm.decoder model.createForm of
                Ok gap ->
                    { model
                        | gaps = gap :: model.gaps
                        , showError = False
                    }

                Err _ ->
                    { model
                        | showError = True
                    }
            , Cmd.none
            )

        UpdateCreateFormName name ->
            updateCreateFormField model <|
                \gf ->
                    { gf | name = capitalizeHead name }

        UpdateCreateFormHeight height ->
            updateCreateFormField model <|
                \gf ->
                    { gf | height = height }

        UpdateCreateFormWidth width ->
            updateCreateFormField model <|
                \gf ->
                    { gf | width = width }


updateCreateFormField : Model -> (CreateForm -> CreateForm) -> ( Model, Cmd none )
updateCreateFormField model f =
    ( { model
        | createForm = f model.createForm
      }
    , Cmd.none
    )


{-| View for Gap editor window
-}
gapEditor : GapEditor -> View Gap.Editor Msg
gapEditor (GapEditor model) =
    column
        (createForm model.showError model.createForm
            :: List.map renderGap model.gaps
        )


createForm : Bool -> CreateForm -> View Gap.Editor Msg
createForm showError model =
    column
        [ row
            [ textBlock "Gap name: "
                |> setClass "formLabel"
                |> fromNoGap Gap.editor
            , View.textInput
                { onChange = UpdateCreateFormName
                }
                model.name
                |> fromNoGap Gap.editor
            ]
        , row
            [ textBlock "Width: "
                |> setClass "formLabel"
                |> fromNoGap Gap.editor
            , View.textInput
                { onChange = UpdateCreateFormWidth
                }
                model.width
                |> fromNoGap Gap.editor
            ]
        , row
            [ textBlock "Height: "
                |> setClass "formLabel"
                |> fromNoGap Gap.editor
            , View.textInput
                { onChange = UpdateCreateFormHeight
                }
                model.height
                |> fromNoGap Gap.editor
            ]
        , rowWith
            { defaultRow
                | horizontal = Row.Right
            }
            [ textBlock "Add"
                |> fromNoGap Gap.editor
                |> setBoundaryWith
                    { defaultBoundary
                        | nodeName = "button"
                        , horizontal = Boundary.HCenter
                        , vertical = Boundary.VCenter
                    }
                    Gap.editor
                |> Neat.setAttributes
                    [ Events.onClick PrependGap
                    ]
                |> setClass "formButton"
                |> fromNoGap Gap.editor
            ]
        ]
        |> setBoundary Gap.editor
        |> setClass "innerForm"
        |> fromNoGap Gap.editor


renderGap : Gap -> View Gap.Editor msg
renderGap gap =
    column
        [ textBlock <| gap.name ++ " : IsGap " ++ capitalizeHead gap.name
        , textBlock <| gap.name ++ " ="
        , textBlock <| "    IsGap"
        , textBlock <| "        { width = " ++ String.fromFloat gap.width
        , textBlock <| "        , height = " ++ String.fromFloat gap.height
        , textBlock <| "        }"
        , View.emptyLine
        , column
            [ textBlock <| "type " ++ capitalizeHead gap.name ++ " = " ++ capitalizeHead gap.name
            , View.emptyLine
            , View.emptyLine
            ]
        ]
        |> fromNoGap Gap.editor
        |> setBoundary Gap.editor
        |> setClass "innerForm"
        |> setClass "innerForm-code"
        |> fromNoGap Gap.editor


{-| Convert gap function name into gap type name.
-}
capitalizeHead : String -> String
capitalizeHead name =
    name
        |> String.uncons
        |> Maybe.map
            (\( c, str ) -> String.cons (Char.toUpper c) str)
        |> Maybe.withDefault ""



-- Helper functions


class : String -> Mixin msg
class =
    classMixinWith <| \name -> "repl_-gapEditor__" ++ name


setClass : String -> View NoGap msg -> View NoGap msg
setClass =
    setMixin << class
