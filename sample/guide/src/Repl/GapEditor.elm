module Repl.GapEditor exposing
    ( GapEditor
    , toList
    , Msg
    , update
    , editor
    , capitalizeHead
    )

{-| Gap editor for REPL.


# Core

@docs GapEditor
@docs toList
@docs Msg
@docs update
@docs editor
@docs capitalizeHead

-}

import Ast.Gap exposing (GapSize)
import Neat exposing (NoGap, View, textBlock)
import Neat.Layout.Column exposing (column)
import Neat.Layout.Row exposing (row)
import Reference exposing (Reference)
import Reference.List
import View exposing (emptyLine, textInput)



-- Core


{-| Main type
-}
type GapEditor
    = GapEditor Model


type alias Model =
    { forms : List GapForm
    }


type alias GapForm =
    { name : String
    , width : String
    , height : String
    }


toList : GapEditor -> List ( String, GapSize )
toList (GapEditor model) =
    Debug.todo "formDecoder"


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
editor : GapEditor -> View NoGap Msg
editor (GapEditor model) =
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
            ]
        , row
            [ textBlock <| "        { height = "
            , textInput
                { ref = ref
                , onInput = \a -> { this | height = a }
                , toMsg = UpdateForms
                }
                this.height
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
