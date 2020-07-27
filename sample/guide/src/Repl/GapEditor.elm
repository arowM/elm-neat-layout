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
import Mixin exposing (Mixin)
import Neat exposing (NoGap, View, setMixin, textBlock)
import Neat.Layout.Column exposing (column)
import Neat.Layout.Row exposing (row)
import Reference exposing (Reference)
import Reference.List
import Repl.Ast.Gap exposing (GapSize)
import Repl.GapEditor.GapForm as GapForm exposing (GapForm)
import View exposing (emptyLine, textInput)



-- Core


{-| Main type
-}
type GapEditor
    = GapEditor Model


fromList : List ( String, GapSize ) -> GapEditor
fromList ls =
    ls
        |> List.map
            (\( name, size ) ->
                { name = name
                , width = String.fromFloat size.width
                , height = String.fromFloat size.height
                }
            )
        |> (\fs -> GapEditor { forms = fs })


type alias Model =
    { forms : List GapForm
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


update : Msg -> GapEditor -> ( GapEditor, Cmd Msg )
update msg (GapEditor model) =
    case msg of
        UpdateForms new ->
            ( GapEditor
                { model
                    | forms = new
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
                { ref = ref
                , onInput = \a -> { this | name = a }
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
                { ref = ref
                , onInput = \a -> { this | width = a }
                , toMsg = UpdateForms
                }
                this.width
                    |> setClass "input"
            ]
        , row
            [ textBlock <| "        { height = "
            , textInput
                { ref = ref
                , onInput = \a -> { this | height = a }
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
