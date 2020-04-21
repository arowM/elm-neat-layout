module Ast exposing
    ( Ast(..)
    , Modifier(..)
    , Msg
    , update
    , repl
    , editor
    , preview
    )

{-| Simplified AST for elm-neat-layout

To avoid type constrains, this module uses **bad** hack which assumes every gap as a `NoGap`.
Usually, you should define and use the appropriate type for each gap.

@docs Ast
@docs Modifier
@docs Msg
@docs update
@docs repl
@docs editor
@docs preview

-}

import Ast.Gap as AstGap exposing (AstGap, GapSize)
import Dict
import Gap
import Html
import Html.Attributes as Attributes
import Html.Attributes.Classname exposing (classMixinWith)
import Html.Events as Events
import Html.Events.Extra as Events
import Json.Encode as Json
import List.Extra as List
import Maybe.Extra as Maybe
import Mixin exposing (Mixin)
import Neat exposing (IsGap(..), NoGap, View, empty, fromNoGap, setAttribute, setLayout, setMixin, setMixins, textBlock)
import Neat.Layout as Layout
import Neat.Layout.Column exposing (column)
import Neat.Layout.Row as Row exposing (Row, defaultRow, row, rowWith, rowWithMap)
import Reference exposing (Reference)
import Reference.List


-- AST Core

{-| -}
type Ast
    = TextBlock String (List Modifier)
    | Empty (List Modifier)
    | RowWith Row (List Ast) (List Modifier)
    | Column (List Ast) (List Modifier)
    | Unselected


modifiersOf : Ast -> List Modifier
modifiersOf ast =
    case ast of
        TextBlock _ mods ->
            mods

        Empty mods ->
            mods

        RowWith _ _ mods ->
            mods

        Column _ mods ->
            mods

        Unselected ->
            []


setModifiersOf : Ast -> List Modifier -> Ast
setModifiersOf ast mods =
    case ast of
        TextBlock str _ ->
            TextBlock str mods

        Empty _ ->
            Empty mods

        RowWith align children _ ->
            RowWith align children mods

        Column children _ ->
            Column children mods

        Unselected ->
            Unselected


setChildrenOf : Ast -> List Ast -> Ast
setChildrenOf ast children =
    case ast of
        RowWith align _ mods ->
            RowWith align children mods

        Column _ mods ->
            Column children mods

        _ ->
            ast


{-| Modifiers for View
-}
type Modifier
    = SetClass String
    | SetLayoutFill
    | SetLayoutFillBy Int
    | SetLayoutNoShrink
    | SetLayoutShrinkBy Int
    | ExpandTo (Maybe AstGap)


-- Msg

{-| -}
type Msg
    = UpdateAst Ast


{-| -}
update : Msg -> Ast -> ( Ast, Cmd Msg )
update msg _ =
    case msg of
        UpdateAst ast ->
            ( ast, Cmd.none )


-- View

{-| View for REPL
-}
repl : (Msg -> msg) -> Int -> Ast -> View Gap.Repl msg
repl f n ast =
    rowWithMap
        f
        { defaultRow
            | wrap = Row.WrapInto n
        }
        [ editor ast
            |> fromNoGap Gap.repl
            |> setLayout Layout.fill
        , preview ast
            |> fromNoGap Gap.repl
            |> setLayout Layout.fill
        ]


-- -- Editor

{-| View for editor window
-}
editor : Ast -> View NoGap Msg
editor ast =
    column
        [ textBlock "Editor"
            |> Neat.fromNoGap Gap.editor
            |> Neat.setBoundary Gap.editor
            |> setClass "editor-title"
        , column
            [ editorCore <| Reference.top ast
            ]
            |> Neat.fromNoGap Gap.editor
            |> Neat.setBoundary Gap.editor
            |> setClass "editor-body"
            |> setLayout Layout.fill
        ]
        |> setClass "editor"


editorCore : Reference Ast Ast -> View NoGap Msg
editorCore ref =
    case Reference.this ref of
        TextBlock str mods ->
            textBlockEditor ref str mods

        Empty mods ->
            emptyEditor ref mods

        RowWith align children mods ->
            rowEditor ref align children mods

        Column children mods ->
            columnEditor ref children mods

        Unselected ->
            unselectedEditor ref


textBlockEditor : Reference Ast Ast -> String -> List Modifier -> View NoGap Msg
textBlockEditor ref str mods =
    column
        [ row
            [ textBlock "textBlock \""
            , Neat.input
                |> setAttribute (Attributes.type_ "text")
                |> setAttribute (Attributes.value str)
                |> updateWithRefOnInput ref (\a _ -> TextBlock a mods)
                |> setClass "input"
            , row
                [ textBlock "\""
                , gapAnnotation <| AstGap.mappend (unmodifiedGap <| Reference.this ref) AstGap.Undetermined
                ]
            ]
        , editMods ref mods
        , appendMods ref mods
        ]


gapAnnotation : AstGap -> View NoGap Msg
gapAnnotation gap =
    case gap of
        AstGap.NoGap ->
            annotationBlock "NoGap"

        AstGap.Gap name _ ->
            name
                |> String.uncons
                |> Maybe.map
                    (\( c, str ) -> "Gap." ++ String.cons (Char.toUpper c) str)
                |> Maybe.withDefault ""
                |> annotationBlock

        AstGap.Undetermined ->
            Neat.none

        AstGap.Invalid AstGap.ChildGapMismatch ->
            annotationBlock "All children must be of the same type"
                |> setClass "annotation-error"

        AstGap.Invalid AstGap.InheritedError ->
            Neat.none


annotationBlock : String -> View NoGap Msg
annotationBlock msg =
            textBlock ("  -- " ++ msg)
                |> setClass "annotation"


emptyEditor : Reference Ast Ast -> List Modifier -> View NoGap Msg
emptyEditor ref mods =
    column
        [ row
            [ textBlock "empty"
            , gapAnnotation <| AstGap.mappend (unmodifiedGap <| Reference.this ref) AstGap.Undetermined
            ]
        , editMods ref mods
        , appendMods ref mods
        ]


rowEditor : Reference Ast Ast -> Row -> List Ast -> List Modifier -> View NoGap Msg
rowEditor ref align asts mods =
    column
        [ textBlock "row"
        , Neat.when (List.length asts == 0) <|
            textBlock "    ["
        , Reference.fromRecord
            { rootWith = rootWithChildren ref
            , this = asts
            }
            |> Reference.List.unwrap editorCore
            |> List.indexedMap
                (\n x ->
                    if n == 0 then
                        row [ textBlock "    [ ", x ]

                    else
                        row [ textBlock "    , ", x ]
                )
            |> column
        , rowWith
            { defaultRow
                | horizontal = Row.Left
            }
            [ indent
            , unselectedEditor <|
                Reference.fromRecord
                    { rootWith = \child -> rootWithChildren ref (asts ++ [ child ])
                    , this = Unselected
                    }
            ]
        , row
            [ textBlock "    ]"
            , gapAnnotation (unmodifiedGap <| Reference.this ref)
            ]
        , editMods ref mods
        , appendMods ref mods
        ]


columnEditor : Reference Ast Ast -> List Ast -> List Modifier -> View NoGap Msg
columnEditor ref asts mods =
    column
        [ textBlock "column"
        , Neat.when (List.length asts == 0) <|
            textBlock "    ["
        , Reference.fromRecord
            { rootWith = rootWithChildren ref
            , this = asts
            }
            |> Reference.List.unwrap editorCore
            |> List.indexedMap
                (\n x ->
                    if n == 0 then
                        row [ textBlock "    [ ", x ]

                    else
                        row [ textBlock "    , ", x ]
                )
            |> column
        , rowWith
            { defaultRow
                | horizontal = Row.Left
            }
            [ indent
            , unselectedEditor <|
                Reference.fromRecord
                    { rootWith = \child -> rootWithChildren ref (asts ++ [ child ])
                    , this = Unselected
                    }
            ]
        , row
            [ textBlock "    ]"
            , gapAnnotation (unmodifiedGap <| Reference.this ref)
            ]
        , editMods ref mods
        , appendMods ref mods
        ]


unselectedEditor : Reference Ast Ast -> View NoGap Msg
unselectedEditor ref =
    row
        [ Neat.select []
            [ Neat.textNode Html.option ""
                |> setMixins
                    [ Mixin.attribute "value" ""
                    ]
            , Neat.textNode Html.option "empty"
                |> setMixins
                    [ Mixin.attribute "value" "empty"
                    ]
            , Neat.textNode Html.option "textBlock \"\""
                |> setMixins
                    [ Mixin.attribute "value" "textBlock"
                    ]
            , Neat.textNode Html.option "row []"
                |> setMixins
                    [ Mixin.attribute "value" "row"
                    ]
            , Neat.textNode Html.option "column []"
                |> setMixins
                    [ Mixin.attribute "value" "column"
                    ]
            ]
            |> setClass "select"
            |> updateWithRefOnInput ref (\v _ -> parseAstNode v)
            |> setAttribute (Attributes.value "")
        ]


editorMod : Reference ( AstGap, Modifier ) Ast -> View NoGap Msg
editorMod ref =
    let
        ( accumGap, mod ) =
            Reference.this ref
    in
    case mod of
        SetClass str ->
            editorModSetClass ref str

        SetLayoutFill ->
            row
                [ textBlock "|> setLayout Layout.fill"
                , gapAnnotation <| AstGap.mappend accumGap AstGap.Undetermined
                ]

        SetLayoutFillBy n ->
            row
                [ textBlock "|> setLayout (Layout.fillBy "
                , Neat.input
                    |> setAttribute (Attributes.type_ "number")
                    |> setAttribute (Attributes.value <| String.fromInt n)
                    |> setClass "input"
                    |> updateWithRefOnInput ref
                        (\v ( acc, _ ) ->
                            ( acc
                            , v
                                |> String.toInt
                                |> Maybe.withDefault 0
                                |> SetLayoutFillBy
                            )
                        )
                , textBlock ")"
                , gapAnnotation <| AstGap.mappend accumGap AstGap.Undetermined
                ]

        SetLayoutNoShrink ->
            row
                [ textBlock "|> setLayout Layout.noShrink"
                , gapAnnotation <| AstGap.mappend accumGap AstGap.Undetermined
                ]

        SetLayoutShrinkBy n ->
            row
                [ textBlock "|> setLayout (Layout.shrinkBy "
                , Neat.input
                    |> setAttribute (Attributes.type_ "number")
                    |> setAttribute (Attributes.value <| String.fromInt n)
                    |> setClass "input"
                    |> updateWithRefOnInput ref
                        (\v ( acc, _ ) ->
                            ( acc
                            , v
                                |> String.toInt
                                |> Maybe.withDefault 1
                                |> SetLayoutShrinkBy
                            )
                        )
                , textBlock ")"
                , gapAnnotation <| AstGap.mappend accumGap AstGap.Undetermined
                ]

        ExpandTo mgap2 ->
            editorModExpandTo accumGap ref mgap2


editorModSetClass : Reference ( AstGap, Modifier ) Ast -> String -> View NoGap Msg
editorModSetClass ref str =
    let
        (accumGap, _) = Reference.this ref
    in
    row
        [ textBlock "|> setAttribute (class \""
        , Neat.select []
            (List.map
                (\k ->
                    Neat.textNode Html.option k
                        |> setMixins
                            [ Mixin.attribute "value" k
                            ]
                )
                ("" :: List.map Tuple.first classes)
            )
            |> setClass "select"
            |> updateWithRefOnInput ref (\v ( acc, _ ) -> ( acc, SetClass v ))
            |> setAttribute (Attributes.value str)
        , textBlock "\")"
        , gapAnnotation <| AstGap.mappend accumGap AstGap.Undetermined
        ]


editorModExpandTo : AstGap -> Reference ( AstGap, Modifier ) Ast -> Maybe AstGap -> View NoGap Msg
editorModExpandTo accumGap ref mgap2 =
    row
        [ textBlock <| labelExpandTo accumGap
        , Neat.select
            []
            (List.map
                (\k ->
                    Neat.textNode Html.option k
                        |> setMixins
                            [ Mixin.attribute "value" k
                            ]
                )
                ("" :: List.map Tuple.first gaps)
            )
            |> setClass "select"
            |> updateWithRefOnInput ref
                (\a ( acc, _ ) ->
                    ( acc
                    , Dict.get a (Dict.fromList gaps)
                        |> Maybe.map (AstGap.Gap a)
                        |> ExpandTo
                    )
                )
            |> setAttribute
                (Attributes.value <|
                    case mgap2 of
                        Just (AstGap.Gap name _) ->
                            name

                        _ ->
                            ""
                )
        , gapAnnotation <| AstGap.mappend accumGap (Maybe.withDefault AstGap.Undetermined mgap2)
        ]


labelExpandTo : AstGap -> String
labelExpandTo gap =
    case gap of
        AstGap.NoGap ->
            "|> fromNoGap Gap."

        AstGap.Gap name _ ->
            "|> expand Gap." ++ name ++ " Gap."

        _ ->
            ""

-- -- Preview

{-| View for preview window -}
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
            rowWith align
                (List.map previewCore asts)

        Column asts mods ->
            column
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


-- Data

classes : List ( String, String )
classes =
    [ ( "background-red"
      , ""
      )
    , ( "background-blue"
      , ""
      )
    , ( "border-red"
      , ""
      )
    , ( "border-blue"
      , ""
      )
    , ( "block"
      , ""
      )
    ]



gaps : List ( String, GapSize )
gaps =
    [ ( "narrow"
      , { width = 0.4
        , height = 0.4
        }
      )
    , ( "wide"
      , { width = 2
        , height = 2
        }
      )
    ]



-- Parsers for select tag


parseAstNode : String -> Ast
parseAstNode v =
    case v of
        "empty" ->
            Empty []

        "textBlock" ->
            TextBlock "" []

        "row" ->
            RowWith defaultRow [] []

        "column" ->
            Column [] []

        _ ->
            Unselected


parseModifier : String -> Maybe Modifier
parseModifier v =
    case v of
        "setClass" ->
            Just <| SetClass ""

        "setLayoutFill" ->
            Just SetLayoutFill

        "setLayoutFillBy" ->
            Just <| SetLayoutFillBy 1

        "setLayoutNoShrink" ->
            Just SetLayoutNoShrink

        "setLayoutShrinkBy" ->
            Just <| SetLayoutShrinkBy 1

        "expandTo" ->
            Just <| ExpandTo Nothing

        _ ->
            Nothing


indent : View NoGap msg
indent =
    textBlock "    "


appendMods : Reference Ast Ast -> List Modifier -> View NoGap Msg
appendMods ref mods =
    rowWith
        { defaultRow
            | horizontal = Row.Left
        }
        [ indent
        , Neat.select []
            [ Neat.textNode Html.option ""
                |> setMixins
                    [ Mixin.attribute "value" ""
                    ]
            , Neat.textNode Html.option "|> setAttribute (class \"\")"
                |> setMixins
                    [ Mixin.attribute "value" "setClass"
                    ]
            , Neat.textNode Html.option "|> setLayout Layout.fill"
                |> setMixins
                    [ Mixin.attribute "value" "setLayoutFill"
                    ]
            , Neat.textNode Html.option "|> setLayout (Layout.fillBy n)"
                |> setMixins
                    [ Mixin.attribute "value" "setLayoutFillBy"
                    ]
            , Neat.textNode Html.option "|> setLayout Layout.noShrink"
                |> setMixins
                    [ Mixin.attribute "value" "setLayoutNoShrink"
                    ]
            , Neat.textNode Html.option "|> setLayout (Layout.shrinkBy n)"
                |> setMixins
                    [ Mixin.attribute "value" "setLayoutShrinkBy"
                    ]
            , Neat.textNode Html.option (labelExpandTo (resultingGap <| Reference.this ref) ++ "_")
                |> setMixins
                    [ Mixin.attribute "value" "expandTo"
                    ]
            ]
            |> setClass "select"
            |> updateWithRefOnInput ref
                (\v ast -> setModifiersOf ast <| mods ++ Maybe.toList (parseModifier v))
            |> setAttribute (Attributes.value "")
        ]


editMods : Reference Ast Ast -> List Modifier -> View NoGap Msg
editMods ref mods =
    column <|
        Reference.List.unwrap
            (identity
                >> editorMod
                >> (\v ->
                        row
                            [ indent
                            , v
                            ]
                   )
            )
        <|
            Reference.fromRecord
                { rootWith = List.map Tuple.second >> rootWithModifiers ref
                , this = setAccumulatedGaps (unmodifiedGap <| Reference.this ref) mods
                }



-- Gap calculations


modifiedGap : AstGap -> List Modifier -> AstGap
modifiedGap init =
    List.foldl (\mod acc -> AstGap.mappend acc <| gapOfModifier mod) init


setAccumulatedGaps : AstGap -> List Modifier -> List ( AstGap, Modifier )
setAccumulatedGaps init mods =
    List.zip
        (List.scanl (\mod acc -> AstGap.mappend acc <| gapOfModifier mod) init mods)
        mods


gapOfModifier : Modifier -> AstGap
gapOfModifier mod =
    case mod of
        ExpandTo (Just gap) ->
            gap

        _ ->
            AstGap.Undetermined


unmodifiedGap : Ast -> AstGap
unmodifiedGap ast =
    case ast of
        TextBlock _ _ ->
            AstGap.NoGap

        Empty _ ->
            AstGap.NoGap

        RowWith _ children _ ->
            AstGap.reduceChildGaps <|
                List.map resultingGap children

        Column children _ ->
            AstGap.reduceChildGaps <|
                List.map resultingGap children

        Unselected ->
            AstGap.Undetermined


resultingGap : Ast -> AstGap
resultingGap ast =
    modifiedGap (unmodifiedGap ast) (modifiersOf ast)



-- Helper functions


class : String -> Mixin msg
class =
    classMixinWith <| \name -> "ast__" ++ name


setClass : String -> View NoGap msg -> View NoGap msg
setClass =
    setMixin << class


setLayoutFillBy : Int -> View NoGap msg -> View NoGap msg
setLayoutFillBy =
    setLayout << Layout.fillBy


setLayoutShrinkBy : Int -> View NoGap msg -> View NoGap msg
setLayoutShrinkBy =
    setLayout << Layout.shrinkBy


rootWithModifiers : Reference Ast Ast -> List Modifier -> Ast
rootWithModifiers ref mods =
    Reference.root <| Reference.modify (\ast -> setModifiersOf ast mods) ref


rootWithChildren : Reference Ast Ast -> List Ast -> Ast
rootWithChildren ref children =
    Reference.root <| Reference.modify (\ast -> setChildrenOf ast children) ref


updateWithRef : Reference a Ast -> (a -> a) -> Msg
updateWithRef ref f =
    UpdateAst <| Reference.root <| Reference.modify f ref


updateWithRefOnInput : Reference a Ast -> (String -> a -> a) -> View NoGap Msg -> View NoGap Msg
updateWithRefOnInput ref f =
    setAttribute <| Events.onInput <| \a -> updateWithRef ref <| f a
