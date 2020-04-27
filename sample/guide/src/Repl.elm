module Repl exposing
    ( Msg
    , Repl
    , init
    , update
    , repl
    )

{-| Repl for the guide.

@docs Msg
@docs init
@docs update
@docs repl

-}

import Gap
import Mixin exposing (Mixin)
import Neat exposing (NoGap, View, setAttribute, setMixin)
import Neat.Layout as Layout
import Reference exposing (Reference)
import Repl.GapEditor as GapEditor exposing (GapEditor)
-- TODO move to Repl.Ast
import Ast as Ast exposing (Ast)


{-| State of REPL.
-}
type Repl
    = Repl Model


type alias Model =
    { gapEditor : GapEditor
    , ast : Ast
    }

{-| Msg for REPL. -}
type Msg
    = UpdateGapEditor GapEditor.Msg
    | UpdateAst Ast.Msg


{-| Constructor for `Repl`.
-}
fromRecord : { gapEditor : GapEditor, ast : Ast } -> Repl
fromRecord = Repl

update : Msg -> Repl -> (Repl, Cmd Msg)
update msg (Repl model) =
    case msg of
        UpdateGapEditor new ->
            ( Repl
                { model
                    | gapEditor = new
                }
            , Cmd.none
            )

        UpdateAst new ->
            ( Repl
                { model
                    | ast = new
                }
            , Cmd.none
            )


{-| View for REPL
-}
repl : (Msg -> msg) -> Ast -> View Gap.Repl msg
repl f ast =
    rowWithMap
        f
        { defaultRow
            | wrap = Row.Wrap
        }
        [ editor ast
            |> fromNoGap Gap.repl
            |> setLayout Layout.fill
        , preview ast
            |> fromNoGap Gap.repl
            |> setLayout Layout.fill
        ]


editor : Repl -> View NoGap Msg
editor (Repl model) =
    column
        [ textBlock "Editor"
            |> Neat.fromNoGap Gap.editor
            |> Neat.setBoundary Gap.editor
            |> setClass "editor-title"
        , column
            [ Ast.editor <| Reference.top model.ast
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
    classMixinWith <| \name -> "view__" ++ name


setClass : String -> View NoGap msg -> View NoGap msg
setClass =
    setMixin << class
