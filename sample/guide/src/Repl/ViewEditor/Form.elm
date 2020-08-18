module Repl.ViewEditor.Form exposing
    ( Form
    , view
    , update
    , fromAst
    )

{-| Form for ViewForm

@docs Form
@docs view
@docs update

# Constructor
@docs fromAst

-}

import Dict
import EList exposing (EList)
import Form.Decoder as FD exposing (Decoder)
import Gap
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Attributes.Classname exposing (classMixinWith)
import Html.Events as Events
import Html.Events.Extra as Events
import Html.Lazy as Html
import Mixin exposing (Mixin)
import Neat exposing (IsGap(..), NoGap, Protected, Renderer, View, empty, fromNoGap, setAttribute, setBoundaryWith, setLayout, setMixin, setMixins, textBlock)
import Neat.Boundary exposing (defaultBoundary)
import Neat.Layout as Layout
import Neat.Layout.Column as Column exposing (Column, column, columnWith, defaultColumn)
import Neat.Layout.Row as Row exposing (Row, defaultRow, row, rowWith, rowWithMap)
import Repl.AccumGap as AccumGap exposing (AccumGap)
import Repl.Ast as Ast exposing (Ast)
import Repl.Gap exposing (Gap)
import Repl.GapEditor as GapEditor exposing (GapEditor)
import View exposing (emptyLine)



-- MODEL


{-| -}
type Form
    = Form Model


unForm : Form -> Model
unForm (Form model) =
    model


type alias Model =
    { element : ElementForm
    , ast : ResAst -- For memoization
    }


type alias ResAst =
    Result (List Error) Ast


type ElementForm
    = TextBlock String (EList ModifierForm)
    | Empty (EList ModifierForm)
    | Row ShowAlignment RowForm (EList Form) (EList ModifierForm)
    | Column ShowAlignment ColumnForm (EList Form) (EList ModifierForm)
    | Unselected


type alias ShowAlignment =
    Bool


{-| Modifiers for View
-}
type ModifierForm
    = SetClass String
    | SetLayoutFill
    | SetLayoutFillBy String
    | SetLayoutNoShrink
    | SetLayoutShrinkBy String
    | ExpandTo (Maybe Gap)


type alias RowForm =
    Row


type alias ColumnForm =
    Column



-- CONSTRUCTOR


fromAst : Ast -> Form
fromAst = Debug.todo

-- NODE


{-| An type alias representing the root of Form.
-}
type alias Root =
    ElementForm


type alias Node a =
    { current : a
    , set : (a -> a) -> Root -> Root
    }


sequence : Node (EList a) -> EList (Node a)
sequence node =
    node.current
        |> EList.keyedMap
            (\key a ->
                { current = a
                , set =
                    \f ->
                        node.set <|
                            EList.modify key f
                }
            )



-- UPDATE


{-| -}
type Msg
    = ModifyForm (Root -> Root)


{-| -}
update : Msg -> Form -> ( Form, Cmd Msg )
update msg (Form model) =
    update_ msg model.element
        |> Tuple.mapFirst
            (\elem ->
                Form
                    { element = elem
                    , ast = FD.run decoder elem
                    }
            )


update_ : Msg -> ElementForm -> ( ElementForm, Cmd Msg )
update_ msg elem =
    case msg of
        ModifyForm modifier ->
            ( modifier elem
            , Cmd.none
            )



-- MODIFIER


modifyTextBlockName : (String -> String) -> ElementForm -> ElementForm
modifyTextBlockName f form =
    case form of
        TextBlock s ms ->
            TextBlock (f s) ms

        a ->
            a


modifyTextBlockMods : (EList ModifierForm -> EList ModifierForm) -> ElementForm -> ElementForm
modifyTextBlockMods f form =
    case form of
        TextBlock s ms ->
            TextBlock s (f ms)

        a ->
            a


modifyEmptyMods : (EList ModifierForm -> EList ModifierForm) -> ElementForm -> ElementForm
modifyEmptyMods f form =
    case form of
        Empty ms ->
            Empty (f ms)

        a ->
            a


modifyRowShowAlign : (Bool -> Bool) -> ElementForm -> ElementForm
modifyRowShowAlign f form =
    case form of
        Row showAlign align children mods ->
            Row (f showAlign) align children mods

        a ->
            a


modifyRowAlign : (Row -> Row) -> ElementForm -> ElementForm
modifyRowAlign f form =
    case form of
        Row showAlign align children mods ->
            Row showAlign (f align) children mods

        a ->
            a


modifyRowChildren : (EList Form -> EList Form) -> ElementForm -> ElementForm
modifyRowChildren f form =
    case form of
        Row showAlign align children mods ->
            Row showAlign align (f children) mods

        a ->
            a


modifyRowMods : (EList ModifierForm -> EList ModifierForm) -> ElementForm -> ElementForm
modifyRowMods f form =
    case form of
        Row showAlign align children mods ->
            Row showAlign align children (f mods)

        a ->
            a


modifyColumnShowAlign : (Bool -> Bool) -> ElementForm -> ElementForm
modifyColumnShowAlign f form =
    case form of
        Column showAlign align children mods ->
            Column (f showAlign) align children mods

        a ->
            a


modifyColumnAlign : (Column -> Column) -> ElementForm -> ElementForm
modifyColumnAlign f form =
    case form of
        Column showAlign align children mods ->
            Column showAlign (f align) children mods

        a ->
            a


modifyColumnChildren : (EList Form -> EList Form) -> ElementForm -> ElementForm
modifyColumnChildren f form =
    case form of
        Column showAlign align children mods ->
            Column showAlign align (f children) mods

        a ->
            a


modifyColumnMods : (EList ModifierForm -> EList ModifierForm) -> ElementForm -> ElementForm
modifyColumnMods f form =
    case form of
        Column showAlign align children mods ->
            Column showAlign align children (f mods)

        a ->
            a


modifySetClass : (String -> String) -> ModifierForm -> ModifierForm
modifySetClass f mod =
    case mod of
        SetClass s ->
            SetClass (f s)

        a ->
            a


modifySetLayoutFillBy : (String -> String) -> ModifierForm -> ModifierForm
modifySetLayoutFillBy f mod =
    case mod of
        SetLayoutFillBy s ->
            SetLayoutFillBy (f s)

        a ->
            a


modifySetLayoutShrinkBy : (String -> String) -> ModifierForm -> ModifierForm
modifySetLayoutShrinkBy f mod =
    case mod of
        SetLayoutShrinkBy s ->
            SetLayoutShrinkBy (f s)

        a ->
            a


modifyExpandTo : (Maybe Gap -> Maybe Gap) -> ModifierForm -> ModifierForm
modifyExpandTo f mod =
    case mod of
        ExpandTo g ->
            ExpandTo (f g)

        a ->
            a



-- DECODER


type Error
    = SetLayoutFillByNaNError
    | SetLayoutShrinkByNaNError


decoder : Decoder ElementForm Error Ast
decoder =
    FD.custom <|
        \form ->
            case form of
                TextBlock name mods ->
                    decodeTextBlock name mods

                Empty mods ->
                    decodeEmptyBlock mods

                Row _ align children mods ->
                    decodeRow align children mods

                Column _ align children mods ->
                    decodeColumn align children mods

                Unselected ->
                    Ok Ast.none


decodeTextBlock : String -> EList ModifierForm -> ResAst
decodeTextBlock name mods =
    mods
        |> EList.toList
        |> FD.run (FD.list modifierDecoder)
        |> Result.map (Ast.textBlock name)


decodeEmptyBlock : EList ModifierForm -> ResAst
decodeEmptyBlock mods =
    mods
        |> EList.toList
        |> FD.run (FD.list modifierDecoder)
        |> Result.map Ast.empty


decodeRow : RowForm -> EList Form -> EList ModifierForm -> ResAst
decodeRow align children mods =
    mods
        |> EList.toList
        |> FD.run (FD.list modifierDecoder)
        |> Result.andThen
            (\ms ->
                children
                    |> EList.toList
                    |> List.map (unForm >> .ast)
                    |> catResults
                    |> Result.map (\cs -> Ast.row align cs ms)
            )


decodeColumn : ColumnForm -> EList Form -> EList ModifierForm -> ResAst
decodeColumn align children mods =
    mods
        |> EList.toList
        |> FD.run (FD.list modifierDecoder)
        |> Result.andThen
            (\ms ->
                children
                    |> EList.toList
                    |> List.map (unForm >> .ast)
                    |> catResults
                    |> Result.map (\cs -> Ast.column align cs ms)
            )


modifierDecoder : Decoder ModifierForm Error Ast.Modifier
modifierDecoder =
    FD.custom <|
        \form ->
            case form of
                SetClass str ->
                    Ok <| Ast.SetClass str

                SetLayoutFill ->
                    Ok <| Ast.SetLayoutFill

                SetLayoutFillBy s ->
                    s
                        |> String.toFloat
                        |> Maybe.map Ast.SetLayoutFillBy
                        |> Result.fromMaybe [ SetLayoutFillByNaNError ]

                SetLayoutNoShrink ->
                    Ok <| Ast.SetLayoutNoShrink

                SetLayoutShrinkBy s ->
                    s
                        |> String.toFloat
                        |> Maybe.map Ast.SetLayoutShrinkBy
                        |> Result.fromMaybe [ SetLayoutShrinkByNaNError ]

                ExpandTo mgap ->
                    Ok <| Ast.ExpandTo mgap



-- VIEW


type alias Config =
    { gapEditor : GapEditor
    }


view : Config -> Form -> View NoGap Msg
view config (Form { ast, element }) =
    formNode config
        ast
        { current = element
        , set = identity
        }


formNode : Config -> ResAst -> Node ElementForm -> View NoGap Msg
formNode config res node =
    case node.current of
        TextBlock str mods ->
            textBlockNode config
                res
                { current = str
                , set = node.set << modifyTextBlockName
                }
                { current = mods
                , set = node.set << modifyTextBlockMods
                }

        Empty mods ->
            emptyNode config
                res
                { current = mods
                , set = node.set << modifyEmptyMods
                }

        Row showAlign align children mods ->
            rowNode config
                res
                { current = showAlign
                , set = node.set << modifyRowShowAlign
                }
                { current = align
                , set = node.set << modifyRowAlign
                }
                { current = children
                , set = node.set << modifyRowChildren
                }
                { current = mods
                , set = node.set << modifyRowMods
                }

        Column showAlign align children mods ->
            columnNode config
                res
                { current = showAlign
                , set = node.set << modifyColumnShowAlign
                }
                { current = align
                , set = node.set << modifyColumnAlign
                }
                { current = children
                , set = node.set << modifyColumnChildren
                }
                { current = mods
                , set = node.set << modifyColumnMods
                }

        Unselected ->
            unselectedNode node


textBlockNode : Config -> ResAst -> Node String -> Node (EList ModifierForm) -> View NoGap Msg
textBlockNode config res str mods =
    let
        unmodifiedGap =
            Ast.unmodifiedGap <| Ast.TextBlock str.current
    in
    column
        [ row
            [ textBlock "textBlock \""
            , View.textInput
                { onChange = ModifyForm << str.set << always
                }
                str.current
            , row
                [ textBlock "\""
                , withAccumGap gapAnnotation res
                ]
            ]
        , modsNode config unmodifiedGap mods
        , appendModNode unmodifiedGap mods
        ]


emptyNode : Config -> ResAst -> Node (EList ModifierForm) -> View NoGap Msg
emptyNode config res mods =
    let
        unmodifiedGap =
            Ast.unmodifiedGap Ast.Empty
    in
    column
        [ row
            [ textBlock "empty"
            , withAccumGap gapAnnotation res
            ]
        , modsNode config unmodifiedGap mods
        , appendModNode unmodifiedGap mods
        ]


appendChildNode : Node (EList Form) -> Node ElementForm
appendChildNode children =
    { current = Unselected
    , set =
        \f ->
            children.set <|
                \els ->
                    EList.append els
                        (EList.singleton <|
                            let
                                new =
                                    f Unselected
                            in
                            Form
                                { element = new
                                , ast = FD.run decoder new
                                }
                        )
    }


childNode_ : Config -> String -> Node Form -> Column -> Renderer -> Html (Protected NoGap Msg)
childNode_ config char form =
    Column.toProtected <|
        row
            [ textBlock <| "    " ++ char ++ " "
            , childNode config form
            ]


childNode : Config -> Node Form -> View NoGap Msg
childNode config node =
    formNode config
        (node.current
            |> unForm
            |> .ast
        )
        { current = node.current |> unForm |> .element
        , set =
            \f ->
                node.set
                    (\(Form model) ->
                        let
                            new =
                                f model.element
                        in
                        Form
                            { element = new
                            , ast = FD.run decoder new
                            }
                    )
        }



-- row


rowNode : Config -> ResAst -> Node ShowAlignment -> Node Row -> Node (EList Form) -> Node (EList ModifierForm) -> View NoGap Msg
rowNode config res show align children mods =
    let
        unmodifiedGap =
            children.current
                |> EList.toList
                |> List.map (.ast << unForm)
                |> catResults
                |> Result.map (Ast.unmodifiedGap << Ast.Row align.current)
                |> Result.withDefault AccumGap.inheritedError
    in
    column
        [ textBlock "row"
        , textBlock "..."
            |> fromNoGap Gap.editor
            |> setBoundaryWith
                { defaultBoundary
                    | nodeName = "button"
                }
                Gap.editor
            |> Neat.setAttributes
                [ Events.onClick <| ModifyForm <| show.set <| always True
                ]
        , Neat.when show.current <|
            row
                [ indent
                , rowAlignmentNode align
                ]
        , Neat.when (EList.length children.current == 0) <|
            textBlock "    ["
        , children
            |> sequence
            |> EList.zipWithList ("[" :: List.repeat (EList.length children.current) ",")
            |> EList.toKeyedList
            |> Column.optimized
                (\( k, _ ) -> EList.uniqueString k)
                (\( k, ( char, v ) ) ->
                    Html.lazy5 childNode_ config char v
                )
                defaultColumn
        , rowWith
            { defaultRow
                | horizontal = Row.Left
            }
            [ indent
            , unselectedNode (appendChildNode children)
            ]
        , row
            [ textBlock "    ]"
            , withAccumGap gapAnnotation res
            ]
        , modsNode config unmodifiedGap mods
        , appendModNode unmodifiedGap mods
        ]


catResults : List (Result (List err) a) -> Result (List err) (List a)
catResults =
    List.foldr
        (\x acc ->
            case ( x, acc ) of
                ( Ok a, Ok ls ) ->
                    Ok (a :: ls)

                ( Ok _, Err ls ) ->
                    Err ls

                ( Err a, Ok ls ) ->
                    Err a

                ( Err a, Err ls ) ->
                    Err (a ++ ls)
        )
        (Ok [])


rowAlignmentNode : Node Row -> View NoGap Msg
rowAlignmentNode ({ current } as align) =
    column
        [ textBlock "{ defaultRow"
        , row
            [ textBlock "  | vertical = "
            , rowVerticalSelector align
            ]
        , row
            [ textBlock "  , horizontal = "
            , rowHorizontalSelector align
            ]
        , row
            [ textBlock "  , wrap = "
            , rowWrapSelector align
            ]
        , textBlock "}"
        ]


rowVerticalSelector : Node Row -> View NoGap Msg
rowVerticalSelector align =
    View.select
        { options = List.map (\v -> ( encodeRowVertical v, encodeRowVertical v )) rowVerticals
        , onChange =
            \v ->
                ModifyForm <|
                    align.set <|
                        \curr ->
                            { curr
                                | vertical = Maybe.withDefault defaultRow.vertical (decodeRowVertical v)
                            }
        }
        (encodeRowVertical align.current.vertical)


rowVerticals : List Row.Vertical
rowVerticals =
    [ Row.Stretch
    , Row.Top
    , Row.Bottom
    , Row.VCenter
    ]


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


decodeRowVertical : String -> Maybe Row.Vertical
decodeRowVertical str =
    case str of
        "Stretch" ->
            Just Row.Stretch

        "Top" ->
            Just Row.Top

        "Bottom" ->
            Just Row.Bottom

        "VCenter" ->
            Just Row.VCenter

        _ ->
            Nothing


rowHorizontalSelector : Node Row -> View NoGap Msg
rowHorizontalSelector align =
    View.select
        { options = List.map (\v -> ( encodeRowHorizontal v, encodeRowHorizontal v )) rowHorizontals
        , onChange =
            \v ->
                ModifyForm <|
                    align.set <|
                        \curr ->
                            { curr
                                | horizontal = Maybe.withDefault defaultRow.horizontal (decodeRowHorizontal v)
                            }
        }
        (encodeRowHorizontal align.current.horizontal)


rowHorizontals : List Row.Horizontal
rowHorizontals =
    [ Row.Left
    , Row.Right
    , Row.HCenter
    , Row.SpaceBetween
    , Row.SpaceAround
    ]


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


decodeRowHorizontal : String -> Maybe Row.Horizontal
decodeRowHorizontal str =
    case str of
        "Left" ->
            Just Row.Left

        "Right" ->
            Just Row.Right

        "HCenter" ->
            Just Row.HCenter

        "SpaceBetween" ->
            Just Row.SpaceBetween

        "SpaceAround" ->
            Just Row.SpaceAround

        _ ->
            Nothing


rowWrapSelector : Node Row -> View NoGap Msg
rowWrapSelector align =
    View.select
        { options = List.map (\v -> ( encodeRowWrap v, encodeRowWrap v )) rowWraps
        , onChange =
            \v ->
                ModifyForm <|
                    align.set <|
                        \curr ->
                            { curr
                                | wrap = Maybe.withDefault defaultRow.wrap (decodeRowWrap v)
                            }
        }
        (encodeRowWrap align.current.wrap)


rowWraps : List Row.Wrap
rowWraps =
    [ Row.Wrap
    , Row.NoWrap
    ]


encodeRowWrap : Row.Wrap -> String
encodeRowWrap v =
    case v of
        Row.Wrap ->
            "Wrap"

        Row.NoWrap ->
            "NoWrap"


decodeRowWrap : String -> Maybe Row.Wrap
decodeRowWrap str =
    case str of
        "Wrap" ->
            Just Row.Wrap

        "NoWrap" ->
            Just Row.NoWrap

        _ ->
            Nothing



-- column


columnNode : Config -> ResAst -> Node ShowAlignment -> Node Column -> Node (EList Form) -> Node (EList ModifierForm) -> View NoGap Msg
columnNode config res show align children mods =
    let
        unmodifiedGap =
            children.current
                |> EList.toList
                |> List.map (.ast << unForm)
                |> catResults
                |> Result.map (Ast.unmodifiedGap << Ast.Column align.current)
                |> Result.withDefault AccumGap.inheritedError
    in
    column
        [ textBlock "column"
        , textBlock "..."
            |> fromNoGap Gap.editor
            |> setBoundaryWith
                { defaultBoundary
                    | nodeName = "button"
                }
                Gap.editor
            |> Neat.setAttributes
                [ Events.onClick <| ModifyForm <| show.set <| always True
                ]
        , Neat.when show.current <|
            column
                [ indent
                , columnAlignmentNode align
                ]
        , Neat.when (EList.length children.current == 0) <|
            textBlock "    ["
        , children
            |> sequence
            |> EList.zipWithList ("[" :: List.repeat (EList.length children.current) ",")
            |> EList.toKeyedList
            |> Column.optimized
                (\( k, _ ) -> EList.uniqueString k)
                (\( k, ( char, v ) ) ->
                    Html.lazy5 childNode_ config char v
                )
                defaultColumn
        , columnWith
            { defaultColumn
                | horizontal = Column.Left
            }
            [ indent
            , unselectedNode (appendChildNode children)
            ]
        , column
            [ textBlock "    ]"
            , withAccumGap gapAnnotation res
            ]
        , modsNode config unmodifiedGap mods
        , appendModNode unmodifiedGap mods
        ]


columnAlignmentNode : Node Column -> View NoGap Msg
columnAlignmentNode ({ current } as align) =
    column
        [ textBlock "{ defaultColumn"
        , column
            [ textBlock "  | vertical = "
            , columnVerticalSelector align
            ]
        , column
            [ textBlock "  , horizontal = "
            , columnHorizontalSelector align
            ]
        , column
            [ textBlock "  , wrap = "
            , columnWrapSelector align
            ]
        , textBlock "}"
        ]


columnVerticalSelector : Node Column -> View NoGap Msg
columnVerticalSelector align =
    View.select
        { options = List.map (\v -> ( encodeColumnVertical v, encodeColumnVertical v )) columnVerticals
        , onChange =
            \v ->
                ModifyForm <|
                    align.set <|
                        \curr ->
                            { curr
                                | vertical = Maybe.withDefault defaultColumn.vertical (decodeColumnVertical v)
                            }
        }
        (encodeColumnVertical align.current.vertical)


columnVerticals : List Column.Vertical
columnVerticals =
    [ Column.Top
    , Column.Bottom
    , Column.VCenter
    , Column.SpaceBetween
    , Column.SpaceAround
    ]


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


decodeColumnVertical : String -> Maybe Column.Vertical
decodeColumnVertical str =
    case str of
        "Top" ->
            Just Column.Top

        "Bottom" ->
            Just Column.Bottom

        "VCenter" ->
            Just Column.VCenter

        "SpaceBetween" ->
            Just Column.SpaceBetween

        "SpaceAround" ->
            Just Column.SpaceAround

        _ ->
            Nothing


columnHorizontalSelector : Node Column -> View NoGap Msg
columnHorizontalSelector align =
    View.select
        { options = List.map (\v -> ( encodeColumnHorizontal v, encodeColumnHorizontal v )) columnHorizontals
        , onChange =
            \v ->
                ModifyForm <|
                    align.set <|
                        \curr ->
                            { curr
                                | horizontal = Maybe.withDefault defaultColumn.horizontal (decodeColumnHorizontal v)
                            }
        }
        (encodeColumnHorizontal align.current.horizontal)


columnHorizontals : List Column.Horizontal
columnHorizontals =
    [ Column.Stretch
    , Column.Left
    , Column.Right
    , Column.HCenter
    ]


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


decodeColumnHorizontal : String -> Maybe Column.Horizontal
decodeColumnHorizontal str =
    case str of
        "Stretch" ->
            Just Column.Stretch

        "Left" ->
            Just Column.Left

        "Right" ->
            Just Column.Right

        "HCenter" ->
            Just Column.HCenter

        _ ->
            Nothing


columnWrapSelector : Node Column -> View NoGap Msg
columnWrapSelector align =
    View.select
        { options = List.map (\v -> ( encodeColumnWrap v, encodeColumnWrap v )) columnWraps
        , onChange =
            \v ->
                ModifyForm <|
                    align.set <|
                        \curr ->
                            { curr
                                | wrap = Maybe.withDefault defaultColumn.wrap (decodeColumnWrap v)
                            }
        }
        (encodeColumnWrap align.current.wrap)


columnWraps : List Column.Wrap
columnWraps =
    [ Column.Wrap
    , Column.NoWrap
    ]


encodeColumnWrap : Column.Wrap -> String
encodeColumnWrap v =
    case v of
        Column.Wrap ->
            "Wrap"

        Column.NoWrap ->
            "NoWrap"


decodeColumnWrap : String -> Maybe Column.Wrap
decodeColumnWrap str =
    case str of
        "Wrap" ->
            Just Column.Wrap

        "NoWrap" ->
            Just Column.NoWrap

        _ ->
            Nothing


unselectedNode : Node ElementForm -> View NoGap Msg
unselectedNode elem =
    row
        [ View.select
            { options =
                [ ( "empty", "empty" )
                , ( "textBlock \"\"", "textBlock" )
                , ( "row []", "row" )
                , ( "column []", "column" )
                ]
            , onChange = ModifyForm << elem.set << always << decodeAstNode
            }
            ""
        ]


decodeAstNode : String -> ElementForm
decodeAstNode v =
    case v of
        "empty" ->
            Empty EList.empty

        "textBlock" ->
            TextBlock "" EList.empty

        "row" ->
            Row False defaultRow EList.empty EList.empty

        "column" ->
            Column False defaultColumn EList.empty EList.empty

        _ ->
            Unselected


modsNode : Config -> AccumGap -> Node (EList ModifierForm) -> View NoGap Msg
modsNode config unmodifiedGap mods =
    mods
        |> sequence
        |> EList.zipWithList
            (mods.current
                |> EList.toList
                |> FD.run (FD.list modifierDecoder)
                |> Result.map
                    (Ast.accumulatedGaps unmodifiedGap)
                |> Result.withDefault
                    (List.repeat (EList.length mods.current) AccumGap.Undetermined)
            )
        |> EList.toKeyedList
        |> Column.optimized
            (\( k, _ ) -> EList.uniqueString k)
            (\( k, ( gap, node ) ) ->
                Html.lazy5 modNode_ config gap node
            )
            defaultColumn


modNode_ : Config -> AccumGap -> Node ModifierForm -> Column -> Renderer -> Html (Protected NoGap Msg)
modNode_ config gap mods =
    Column.toProtected <|
        row
            [ indent
            , modNode config gap mods
            ]


modNode : Config -> AccumGap -> Node ModifierForm -> View NoGap Msg
modNode config gap mod =
    case mod.current of
        SetClass str ->
            setClassNode gap
                { current = str
                , set = mod.set << modifySetClass
                }

        SetLayoutFill ->
            row
                [ textBlock "|> setLayout Layout.fill"
                , gapAnnotation gap
                ]

        SetLayoutFillBy n ->
            setLayoutFillByNode gap
                { current = n
                , set = mod.set << modifySetLayoutFillBy
                }

        SetLayoutNoShrink ->
            row
                [ textBlock "|> setLayout Layout.noShrink"
                , gapAnnotation gap
                ]

        SetLayoutShrinkBy n ->
            setLayoutShrinkByNode gap
                { current = n
                , set = mod.set << modifySetLayoutShrinkBy
                }

        ExpandTo mgap ->
            expandToNode config
                gap
                { current = mgap
                , set = mod.set << modifyExpandTo
                }


setClassNode : AccumGap -> Node String -> View NoGap Msg
setClassNode gap str =
    row
        [ textBlock "|> setAttribute (class \""
        , View.select
            { options = List.map (\v -> ( v, v )) classNames
            , onChange = ModifyForm << str.set << always
            }
            str.current
        , textBlock "\")"
        , gapAnnotation gap
        ]


classNames : List String
classNames =
    [ "background-red"
    , "background-blue"
    , "border-red"
    , "border-blue"
    , "block"
    ]


setLayoutFillByNode : AccumGap -> Node String -> View NoGap Msg
setLayoutFillByNode gap str =
    row
        [ textBlock "|> setLayout (Layout.fillBy "
        , View.textInput
            { onChange = ModifyForm << str.set << always
            }
            str.current
        , textBlock ")"
        , gapAnnotation gap
        ]


setLayoutShrinkByNode : AccumGap -> Node String -> View NoGap Msg
setLayoutShrinkByNode gap str =
    row
        [ textBlock "|> setLayout (Layout.shrinkeBy "
        , View.textInput
            { onChange = ModifyForm << str.set << always
            }
            str.current
        , textBlock ")"
        , gapAnnotation gap
        ]


expandToNode : Config -> AccumGap -> Node (Maybe Gap) -> View NoGap Msg
expandToNode { gapEditor } gap mgap =
    row
        [ textBlock <| labelExpandTo gap
        , View.select
            { options = List.map (\v -> ( v.name, v.name )) (GapEditor.toList gapEditor)
            , onChange = \v -> ModifyForm <| mgap.set <| always <| GapEditor.find v gapEditor
            }
            (mgap.current
                |> Maybe.map .name
                |> Maybe.withDefault ""
            )
        , gapAnnotation gap
        ]


labelExpandTo : AccumGap -> String
labelExpandTo gap =
    case gap of
        AccumGap.NoGap ->
            "|> fromNoGap Gap."

        AccumGap.Gap g ->
            "|> expand Gap." ++ g.name ++ " Gap."

        _ ->
            ""


appendModNode : AccumGap -> Node (EList ModifierForm) -> View NoGap Msg
appendModNode unmodifiedGap mods =
    rowWith
        { defaultRow
            | horizontal = Row.Left
        }
        [ indent
        , View.select
            { options =
                [ ( "|> setAttribute (class \"\")", "setClass" )
                , ( "|> setLayout Layout.fill", "setLayoutFill" )
                , ( "|> setLayout (Layout.fillBy n)", "setLayoutFillBy" )
                , ( "|> setLayout Layout.noShrink", "setLayoutNoShrink" )
                , ( "|> setLayout (Layout.shrinkBy n)", "setLayoutShrinkBy" )
                , ( labelExpandTo
                        (mods.current
                            |> EList.toList
                            |> FD.run (FD.list modifierDecoder)
                            |> Result.map
                                (List.foldl (\a acc -> AccumGap.mappend acc <| Ast.gapOfModifier a) unmodifiedGap)
                            |> Result.withDefault AccumGap.inheritedError
                        )
                        ++ " _"
                  , "expandTo"
                  )
                ]
            , onChange =
                \v ->
                    ModifyForm <|
                        mods.set <|
                            \ms ->
                                case decodeModifier v of
                                    Just m ->
                                        EList.append ms (EList.singleton m)

                                    Nothing ->
                                        ms
            }
            ""
        ]


decodeModifier : String -> Maybe ModifierForm
decodeModifier v =
    case v of
        "setClass" ->
            Just <| SetClass ""

        "setLayoutFill" ->
            Just SetLayoutFill

        "setLayoutFillBy" ->
            Just <| SetLayoutFillBy ""

        "setLayoutNoShrink" ->
            Just SetLayoutNoShrink

        "setLayoutShrinkBy" ->
            Just <| SetLayoutShrinkBy ""

        "expandTo" ->
            Just <| ExpandTo Nothing

        _ ->
            Nothing



-- Helper functions


withAccumGap : (AccumGap -> View NoGap Msg) -> ResAst -> View NoGap Msg
withAccumGap f res =
    case res of
        Err _ ->
            Neat.none

        Ok ast ->
            f <| Ast.accumGap ast


gapAnnotation : AccumGap -> View NoGap Msg
gapAnnotation gap =
    case gap of
        AccumGap.NoGap ->
            annotationBlock "NoGap"

        AccumGap.Gap g ->
            g.name
                |> GapEditor.capitalizeHead
                |> (\str ->
                        "Gap."
                            ++ str
                            |> annotationBlock
                   )

        AccumGap.Undetermined ->
            Neat.none

        AccumGap.Invalid AccumGap.ChildGapMismatch ->
            annotationBlock "All children must be of the same type"
                |> setClass "annotation-error"

        AccumGap.Invalid AccumGap.InheritedError ->
            Neat.none


annotationBlock : String -> View NoGap Msg
annotationBlock msg =
    textBlock ("  -- " ++ msg)
        |> setClass "annotation"


indent : View NoGap msg
indent =
    textBlock "    "


class : String -> Mixin msg
class =
    classMixinWith <| \name -> "repl_-viewEditor__" ++ name


setClass : String -> View NoGap msg -> View NoGap msg
setClass =
    setMixin << class
