module Repl.GapEditor exposing
    ( GapEditor
    , decoder
    , Msg
    , update
    , gapEditor
    , capitalizeHead
    , fromList
    )

{-| Gap editor for REPL.


# Core

@docs GapEditor
@docs decoder
@docs Msg
@docs update
@docs gapEditor
@docs capitalizeHead

-}

import Html.Attributes.Classname exposing (classMixinWith)
import Form.Decoder as FD exposing (Decoder)
import Gap
import Mixin exposing (Mixin)
import Neat exposing (NoGap, View, fromNoGap, setBoundary, setBoundaryWith, setMixin, textBlock)
import Neat.Boundary as Boundary exposing (Boundary, defaultBoundary)
import Neat.Layout.Column exposing (column)
import Neat.Layout.Row as Row exposing (Row, defaultRow, row, rowWith)
import Reference exposing (Reference)
import Reference.List
import Repl.Ast.Gap exposing (GapSize)
import Repl.GapEditor.GapForm as GapForm exposing (GapForm)
import View exposing (emptyLine)



-- Core


{-| Main type
-}
type GapEditor
    = GapEditor Model


fromList : List ( String, GapSize ) -> GapEditor
fromList ls =
    GapEditor
        { createForm = GapForm.init
        , gaps = ls
        }


type alias Model =
    { createForm : GapForm
    , gaps : List (String, GapSize)
    }


{-| Decoder that produces list of (gap name, gap size).
-}
decoder : Decoder GapEditor ( Int, GapForm.Error ) (List ( String, GapSize ))
decoder =
    FD.lift (\(GapEditor model) -> model) modelDecoder


modelDecoder : Decoder Model ( Int, GapForm.Error ) (List ( String, GapSize ))
modelDecoder =
    FD.lift .forms formsDecoder


formsDecoder : Decoder (List GapForm) ( Int, GapForm.Error ) (List ( String, GapSize ))
formsDecoder =
    FD.listOf GapForm.decoder


type Msg
    = UpdateForms (List GapForm)
    | UpdateCreateFormName String
    | UpdateCreateFormHeight String
    | UpdateCreateFormWidth String


update : Msg -> GapEditor -> ( GapEditor, Cmd Msg )
update msg (GapEditor model) =
    update_ msg model
        |> Tuple.mapFirst GapEditor


update_ : Msg -> Model -> ( Model, Cmd Msg)
update_ msg model =
    case msg of
        UpdateForms new ->
            ( { model
                    | forms = new
              }
            , Cmd.none
            )

        UpdateCreateFormName name ->
            updateCreateFormField
                (\gf -> { gf | name = capitalizeHead name })
                model

        UpdateCreateFormHeight height ->
            updateCreateFormField
                (\gf -> { gf | height = height })
                model

        UpdateCreateFormWidth width ->
            updateCreateFormField
                (\gf -> { gf | width = width })
                model



updateCreateFormField : (GapForm -> GapForm) -> Model -> (Model, Cmd none)
updateCreateFormField f model =
    ( { model
          | createForm = f model.createForm
      }
    , Cmd.none
    )

{-| View for Gap editor window
-}
gapEditor : GapEditor -> View NoGap Msg
gapEditor (GapEditor model) =
    column
        (Reference.List.unwrap formEditor <| Reference.top model.forms)


formEditor : Reference GapForm (List GapForm) -> View NoGap Msg
formEditor ref =
    let
        this =
            Reference.this ref
    in
    column
        [ row
            [ textInput
                { onInput = \a -> { this | name = a }
                , toMsg = UpdateForms
                }
                this.name
                    |> setClass "input"
            , textBlock <| " : IsGap " ++ capitalizeHead this.name
            ]
        , textBlock <| this.name ++ " ="
        , textBlock <| "    IsGap"
        , row
            [ textBlock <| "        { width = "
            , textInput
                { onInput = \a -> { this | width = a }
                , toMsg = UpdateForms
                }
                this.width
                    |> setClass "input"
            ]
        , row
            [ textBlock <| "        { height = "
            , textInput
                { onInput = \a -> { this | height = a }
                , toMsg = UpdateForms
                }
                this.height
                    |> setClass "input"
            ]
        , textBlock <| "        }"
        , View.emptyLine
        , View.emptyLine
        , Neat.unless (this.name == "") <|
            column
                [ textBlock <| "type " ++ capitalizeHead this.name ++ " = " ++ capitalizeHead this.name
                , View.emptyLine
                , View.emptyLine
                ]
        ]


createForm : GapForm -> View NoGap Msg
createForm model =
    column
        [ row
            [ textBlock "Gap name"
                    |> fromNoGap Gap.editor
            , textInput
                { onInput = \a -> { model | name = a }
                , toMsg = UpdateCreateFormName
                }
                model.name
                    |> setClass "input"
                    |> fromNoGap Gap.editor
            ]
        , row
            [ textBlock "Width"
                    |> fromNoGap Gap.editor
            , textInput
                { onInput = \a -> { model | width = a }
                , toMsg = UpdateCreateFormWidth
                }
                model.width
                    |> setClass "input"
                    |> fromNoGap Gap.editor
            ]
        , row
            [ textBlock "Height"
                |> fromNoGap Gap.editor
            , textInput
                { onInput = \a -> { model | height = a }
                , toMsg = UpdateCreateFormHeight
                }
                model.height
                    |> setClass "input"
                    |> fromNoGap Gap.editor
            ]
        , rowWith
            { defaultRow
                | horizontal = Row.Right
            }
            [ textBlock "Add"
                |> fromNoGap Gap.editor
                |> setBoundaryWith
                    Gap.editor
                    { defaultBoundary
                        | nodeName = "button"
                        , horizontal = Boundary.HCenter
                        , vertical = Boundary.VCenter
                    }
                |> fromNoGap Gap.editor
            ]
        ]
                |> setBoundary Gap.editor
                |> setClass "innerForm"



{-| Convert gap function name into gap type name.
-}
capitalizeHead : String -> String
capitalizeHead name =
    name
        |> String.uncons
        |> Maybe.map
            (\( c, str ) -> String.cons (Char.toUpper c) str)
        |> Maybe.withDefault ""


{-| Configuration for `textInput`.
-}
type alias TextInput t msg =
    { onInput : String -> t
    , toMsg : t -> msg
    }

textInput : TextInput t msg -> String -> View NoGap msg
textInput o value =
    View.textInput
        { ref = Reference.identity
        , onInput = o.onInput
        , toMsg = o.toMsg
        }


-- Helper functions


class : String -> Mixin msg
class =
    classMixinWith <| \name -> "view" ++ name


setClass : String -> View NoGap msg -> View NoGap msg
setClass =
    setMixin << class
