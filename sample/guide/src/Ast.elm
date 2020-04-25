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
import Neat.Layout.Column as Column exposing (Column, column, columnWith, defaultColumn)
import Neat.Layout.Row as Row exposing (Row, defaultRow, row, rowWith, rowWithMap)
import Reference exposing (Reference)
import Reference.List



-- AST Core


{-| -}
type Ast
    = TextBlock String (List Modifier)
    | Empty (List Modifier)
    | RowWith (Maybe Row) (List Ast) (List Modifier)
    | ColumnWith (Maybe Column) (List Ast) (List Modifier)
    | Unselected


type alias ShowAlignmentEditor =
    Bool


modifiersOf : Ast -> List Modifier
modifiersOf ast =
    case ast of
        TextBlock _ mods ->
            mods

        Empty mods ->
            mods

        RowWith _ _ mods ->
            mods

        ColumnWith _ _ mods ->
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

        ColumnWith align children _ ->
            ColumnWith align children mods

        Unselected ->
            Unselected


setChildrenOf : Ast -> List Ast -> Ast
setChildrenOf ast children =
    case ast of
        RowWith align _ mods ->
            RowWith align children mods

        ColumnWith align _ mods ->
            ColumnWith align children mods

        _ ->
            ast


setRowAlignment : Ast -> Maybe Row -> Ast
setRowAlignment ast malign =
    case ast of
        RowWith _ children mods ->
            RowWith malign children mods

        _ ->
            ast


rowAlignmentOf : Ast -> Maybe Row
rowAlignmentOf ast =
    case ast of
        RowWith malign _ _ ->
            malign

        _ ->
            Nothing


setColumnAlignment : Ast -> Maybe Column -> Ast
setColumnAlignment ast malign =
    case ast of
        ColumnWith _ children mods ->
            ColumnWith malign children mods

        _ ->
            ast


columnAlignmentOf : Ast -> Maybe Column
columnAlignmentOf ast =
    case ast of
        ColumnWith malign _ _ ->
            malign

        _ ->
            Nothing


editAlignment : Ast -> Bool
editAlignment ast =
    case ast of
        RowWith (Just _) _ _ ->
            True

        ColumnWith (Just _) _ _ ->
            True

        _ ->
            False


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

        RowWith _ children mods ->
            rowEditor ref children mods

        ColumnWith _ children mods ->
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
                |> setValue str
                |> updateWithRefOnInput ref (\a -> TextBlock a mods)
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


rowEditor : Reference Ast Ast -> List Ast -> List Modifier -> View NoGap Msg
rowEditor ref asts mods =
    let
        ast =
            Reference.this ref

        useRowWith =
            editAlignment ast

        rowValue =
            if useRowWith then
                "rowWith"

            else
                "row"

        thisAlign =
            Maybe.withDefault defaultRow <| rowAlignmentOf ast
    in
    column
        [ row
            [ selectWith
                [ ( "row", "row" )
                , ( "rowWith", "rowWith" )
                ]
                |> updateWithRefOnInput ref
                    (\v ->
                        setRowAlignment ast
                            (if v == "rowWith" then
                                Just thisAlign

                             else
                                Nothing
                            )
                    )
                |> setValue rowValue
            ]
        , Neat.when useRowWith <|
            row
                [ indent
                , rowAlignmentEditor ref
                ]
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


rowAlignmentEditor : Reference Ast Ast -> View NoGap Msg
rowAlignmentEditor ref =
    let
        ast =
            Reference.this ref

        thisAlign =
            Maybe.withDefault defaultRow <| rowAlignmentOf <| ast
    in
    column
        [ textBlock "{ defaultRow"
        , row
            [ textBlock "  | vertical = "
            , selectForEnum rowVerticals
                |> updateWithRefOnInput ref
                    (\v ->
                        setRowAlignment ast <|
                            Just
                                { thisAlign
                                    | vertical = lookupWithDefault defaultRow.vertical v rowVerticals
                                }
                    )
                |> setValue (encodeRowVertical <| thisAlign.vertical)
            ]
        , row
            [ textBlock "  , horizontal = "
            , selectForEnum rowHorizontals
                |> updateWithRefOnInput ref
                    (\v ->
                        setRowAlignment ast <|
                            Just
                                { thisAlign
                                    | horizontal = lookupWithDefault defaultRow.horizontal v rowHorizontals
                                }
                    )
                |> setValue (encodeRowHorizontal <| thisAlign.horizontal)
            ]
        , row
            [ textBlock "  , wrap = "
            , selectForEnum rowWraps
                |> updateWithRefOnInput ref
                    (\v ->
                        setRowAlignment ast <|
                            Just
                                { thisAlign
                                    | wrap = lookupWithDefault defaultRow.wrap v rowWraps
                                }
                    )
                |> setValue (encodeRowWrap <| thisAlign.wrap)
            ]
        , textBlock "}"
        ]


columnEditor : Reference Ast Ast -> List Ast -> List Modifier -> View NoGap Msg
columnEditor ref asts mods =
    let
        ast =
            Reference.this ref

        useColumnWith =
            editAlignment ast

        columnValue =
            if useColumnWith then
                "columnWith"

            else
                "column"

        thisAlign =
            Maybe.withDefault defaultColumn <| columnAlignmentOf ast
    in
    column
        [ row
            [ selectWith
                [ ( "column", "column" )
                , ( "columnWith", "columnWith" )
                ]
                |> updateWithRefOnInput ref
                    (\v ->
                        setColumnAlignment ast
                            (if v == "columnWith" then
                                Just thisAlign

                             else
                                Nothing
                            )
                    )
                |> setValue columnValue
            ]
        , Neat.when useColumnWith <|
            row
                [ indent
                , columnAlignmentEditor ref
                ]
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


columnAlignmentEditor : Reference Ast Ast -> View NoGap Msg
columnAlignmentEditor ref =
    let
        ast =
            Reference.this ref

        thisAlign =
            Maybe.withDefault defaultColumn <| columnAlignmentOf <| ast
    in
    column
        [ textBlock "{ defaultColumn"
        , row
            [ textBlock "  | vertical = "
            , selectForEnum columnVerticals
                |> updateWithRefOnInput ref
                    (\v ->
                        setColumnAlignment ast <|
                            Just
                                { thisAlign
                                    | vertical = lookupWithDefault defaultColumn.vertical v columnVerticals
                                }
                    )
                |> setValue (encodeColumnVertical <| thisAlign.vertical)
            ]
        , row
            [ textBlock "  , horizontal = "
            , selectForEnum columnHorizontals
                |> updateWithRefOnInput ref
                    (\v ->
                        setColumnAlignment ast <|
                            Just
                                { thisAlign
                                    | horizontal = lookupWithDefault defaultColumn.horizontal v columnHorizontals
                                }
                    )
                |> setValue (encodeColumnHorizontal <| thisAlign.horizontal)
            ]
        , row
            [ textBlock "  , wrap = "
            , selectForEnum columnWraps
                |> updateWithRefOnInput ref
                    (\v ->
                        setColumnAlignment ast <|
                            Just
                                { thisAlign
                                    | wrap = lookupWithDefault defaultColumn.wrap v columnWraps
                                }
                    )
                |> setValue (encodeColumnWrap <| thisAlign.wrap)
            ]
        , textBlock "}"
        ]


unselectedEditor : Reference Ast Ast -> View NoGap Msg
unselectedEditor ref =
    row
        [ selectWith
            [ ( "empty", "empty" )
            , ( "textBlock \"\"", "textBlock" )
            , ( "row []", "row" )
            , ( "column []", "column" )
            ]
            |> updateWithRefOnInput ref parseAstNode
            |> setValue ""
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
                , numberInput n
                    |> updateWithRefOnInput ref
                        (\v ->
                            ( accumGap
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
                , numberInput n
                    |> updateWithRefOnInput ref
                        (\v ->
                            ( accumGap
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
        ( accumGap, _ ) =
            Reference.this ref
    in
    row
        [ textBlock "|> setAttribute (class \""
        , selectForEnum classes
            |> updateWithRefOnInput ref (\v -> ( accumGap, SetClass v ))
            |> setValue str
        , textBlock "\")"
        , gapAnnotation <| AstGap.mappend accumGap AstGap.Undetermined
        ]


editorModExpandTo : AstGap -> Reference ( AstGap, Modifier ) Ast -> Maybe AstGap -> View NoGap Msg
editorModExpandTo accumGap ref mgap2 =
    row
        [ textBlock <| labelExpandTo accumGap
        , selectForEnum gaps
            |> updateWithRefOnInput ref
                (\a ->
                    ( accumGap
                    , Dict.get a (Dict.fromList gaps)
                        |> Maybe.map (AstGap.Gap a)
                        |> ExpandTo
                    )
                )
            |> setValue
                (case mgap2 of
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


indent : View NoGap msg
indent =
    textBlock "    "


appendMods : Reference Ast Ast -> List Modifier -> View NoGap Msg
appendMods ref mods =
    let
        ast =
            Reference.this ref
    in
    rowWith
        { defaultRow
            | horizontal = Row.Left
        }
        [ indent
        , selectWith
            [ ( "|> setAttribute (class \"\")", "setClass" )
            , ( "|> setLayout Layout.fill", "setLayoutFill" )
            , ( "|> setLayout (Layout.fillBy n)", "setLayoutFillBy" )
            , ( "|> setLayout Layout.noShrink", "setLayoutNoShrink" )
            , ( "|> setLayout (Layout.shrinkBy n)", "setLayoutShrinkBy" )
            , ( labelExpandTo (resultingGap <| Reference.this ref) ++ "_", "expandTo" )
            ]
            |> updateWithRefOnInput ref
                (\v -> setModifiersOf ast <| mods ++ Maybe.toList (parseModifier v))
            |> setValue ""
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


rowVerticals : List ( String, Row.Vertical )
rowVerticals =
    [ ( "Stretch", Row.Stretch )
    , ( "Top", Row.Top )
    , ( "Bottom", Row.Bottom )
    , ( "VCenter", Row.VCenter )
    ]


rowHorizontals : List ( String, Row.Horizontal )
rowHorizontals =
    [ ( "Left", Row.Left )
    , ( "Right", Row.Right )
    , ( "HCenter", Row.HCenter )
    , ( "SpaceBetween", Row.SpaceBetween )
    , ( "SpaceAround", Row.SpaceAround )
    ]


rowWraps : List ( String, Row.Wrap )
rowWraps =
    [ ( "NoWrap", Row.NoWrap )
    , ( "Wrap", Row.Wrap )
    ]


columnVerticals : List ( String, Column.Vertical )
columnVerticals =
    [ ( "Top", Column.Top )
    , ( "Bottom", Column.Bottom )
    , ( "VCenter", Column.VCenter )
    , ( "SpaceBetween", Column.SpaceBetween )
    , ( "SpaceAround", Column.SpaceAround )
    ]


columnHorizontals : List ( String, Column.Horizontal )
columnHorizontals =
    [ ( "Stretch", Column.Stretch )
    , ( "Left", Column.Left )
    , ( "Right", Column.Right )
    , ( "HCenter", Column.HCenter )
    ]


columnWraps : List ( String, Column.Wrap )
columnWraps =
    [ ( "NoWrap", Column.NoWrap )
    , ( "Wrap", Column.Wrap )
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
            RowWith Nothing [] []

        "column" ->
            ColumnWith Nothing [] []

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



-- Encoders for select tag value


encodeRowVertical : Row.Vertical -> String
encodeRowVertical v =
    case v of
        Row.Stretch ->
            "Stretch"

        Row.Top ->
            "Top"

        Row.Bottom ->
            "Bottom"

        Row.VCenter ->
            "VCenter"


encodeRowHorizontal : Row.Horizontal -> String
encodeRowHorizontal v =
    case v of
        Row.Left ->
            "Left"

        Row.Right ->
            "Right"

        Row.HCenter ->
            "HCenter"

        Row.SpaceBetween ->
            "SpaceBetween"

        Row.SpaceAround ->
            "SpaceAround"


encodeRowWrap : Row.Wrap -> String
encodeRowWrap v =
    case v of
        Row.NoWrap ->
            "NoWrap"

        Row.Wrap ->
            "Wrap"

        Row.WrapInto _ ->
            "Wrap"


encodeColumnVertical : Column.Vertical -> String
encodeColumnVertical v =
    case v of
        Column.Top ->
            "Top"

        Column.Bottom ->
            "Bottom"

        Column.VCenter ->
            "VCenter"

        Column.SpaceBetween ->
            "SpaceBetween"

        Column.SpaceAround ->
            "SpaceAround"


encodeColumnHorizontal : Column.Horizontal -> String
encodeColumnHorizontal v =
    case v of
        Column.Stretch ->
            "Stretch"

        Column.Left ->
            "Left"

        Column.Right ->
            "Right"

        Column.HCenter ->
            "HCenter"


encodeColumnWrap : Column.Wrap -> String
encodeColumnWrap v =
    case v of
        Column.NoWrap ->
            "NoWrap"

        Column.Wrap ->
            "Wrap"

        Column.WrapInto _ ->
            "Wrap"



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

        ColumnWith _ children _ ->
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


setValue : String -> View NoGap msg -> View NoGap msg
setValue v =
    setAttribute (Attributes.value v)


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


updateWithRef : Reference a Ast -> a -> Msg
updateWithRef ref v =
    UpdateAst <| Reference.root <| Reference.modify (\_ -> v) ref


updateWithRefOnInput : Reference a Ast -> (String -> a) -> View NoGap Msg -> View NoGap Msg
updateWithRefOnInput ref f =
    setAttribute <| Events.onInput <| \a -> updateWithRef ref <| f a


lookup : comparable -> List ( comparable, v ) -> Maybe v
lookup k ls =
    Dict.get k <| Dict.fromList ls


lookupWithDefault : v -> comparable -> List ( comparable, v ) -> v
lookupWithDefault def k ls =
    lookup k ls
        |> Maybe.withDefault def


selectForEnum : List ( String, v ) -> View NoGap Msg
selectForEnum kvs =
    selectWith <| List.map (\( k, _ ) -> ( k, k )) kvs


selectWith : List ( String, String ) -> View NoGap Msg
selectWith ps =
    Neat.select []
        (List.map
            (\( label, value ) ->
                Neat.textNode Html.option label
                    |> setValue value
            )
            (( "", "" ) :: ps)
        )
        |> setClass "select"


numberInput : Int -> View NoGap msg
numberInput n =
    Neat.input
        |> setAttribute (Attributes.type_ "number")
        |> setAttribute (Attributes.value <| String.fromInt n)
        |> setClass "input"
