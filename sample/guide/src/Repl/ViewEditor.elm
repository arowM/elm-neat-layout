module Repl.ViewEditor exposing
    ( ViewEditor
    , fromAst
    , toAst
    , Msg
    , update
    , view
    , Config
    )

{-| Editor window for Repl

@docs ViewEditor
@docs fromAst
@docs toAst
@docs Msg
@docs update
@docs viewEditor
@docs Config

-}

import Neat exposing (NoGap, View)
import Neat.Layout.Column as Column exposing (defaultColumn)
import Repl.Ast exposing (Ast)
import Repl.ViewEditor.Form as Form exposing (Form)


type ViewEditor
    = ViewEditor Model


type alias Model =
    { form : Form
    }


{-| -}
fromAst : Ast -> ViewEditor
fromAst ast =
    ViewEditor { form = Form.fromAst ast }


{-| -}
toAst : ViewEditor -> Maybe Ast
toAst (ViewEditor { form }) =
    Form.toAst form


{-| -}
type Msg
    = FormMsg Form.Msg


{-| -}
update : Msg -> ViewEditor -> ( ViewEditor, Cmd Msg )
update message (ViewEditor model) =
    case message of
        FormMsg msg ->
            Form.update msg model.form
                |> Tuple.mapFirst
                    (\form -> ViewEditor <| { model | form = form })
                |> Tuple.mapSecond
                    (Cmd.map FormMsg)


{-| Reexport `Config` -}
type alias Config = Form.Config


{-| -}
view : Form.Config -> ViewEditor -> View NoGap Msg
view config (ViewEditor { form }) =
    Column.columnWithMap FormMsg defaultColumn
        [ Form.view config form
        ]
