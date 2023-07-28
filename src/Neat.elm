module Neat exposing
    ( View
    , Boundary
    , render
    , Renderer
    , defaultRenderer
    , setBaseSizeInRem
    , when
    , unless
    , withMaybe
    , IsGap
    , customGap
    , Gap
    )

{-| Main module for elm-neat-layout.


# Core

@docs View
@docs Boundary


# Render

@docs render
@docs Renderer
@docs defaultRenderer
@docs setBaseSizeInRem


# Handle conditions

@docs when
@docs unless
@docs withMaybe


# Custom gaps

You can use custom gaps just by declaring new types and `IsGap` values for them.

    import Neat exposing (IsGap)

    type ButtonGroup
        = ButtonGroup Never

    buttonGroup : IsGap ButtonGroup
    buttonGroup =
        IsGap
            { horizontal = 0.6
            , vertical = 0.6
            }

@docs IsGap
@docs customGap
@docs Gap

-}

import Mixin exposing (Mixin)
import Mixin.Html as Html exposing (Html)
import Neat.Boundary
import Neat.Internal as Internal
    exposing
        ( Alignment(..)
        , BaseSize(..)
        , Boundary(..)
        , BoundaryProps
        , Boundary_(..)
        , ChildBoundaries(..)
        , Children(..)
        , ColumnBoundary_
        , Column_
        , Flows_
        , Gap
        , IsGap(..)
        , Justify(..)
        , MaxHeight(..)
        , MaxWidth(..)
        , MinHeight(..)
        , MinWidth(..)
        , Overlay(..)
        , Renderer(..)
        , Renderer_
        , RowBoundary_
        , Row_
        , Size(..)
        , View_(..)
        )
import Neat.View



-- Core


{-| A gap-sensible `Html msg` alternative.
-}
type alias View gap msg =
    Neat.View.View gap msg


{-| A bounded View without gap.

Convert to/from View by `setGap`/`setBoundary`.

-}
type alias Boundary msg =
    Neat.Boundary.Boundary msg



-- Custom gaps


{-| Information about your custom gaps.

  - horizontal : horizontal gap relative to _base size_
  - vertical : vertical gap relative to _base size_

e.g., If the _base size_ is `2rem`, `IsGap { horizontal = 1.2, vertical = 2 }` becomes gap with `"2.4rem"` horizontally and `"4rem"` vertically.

-}
type alias IsGap p =
    Internal.IsGap p


{-| -}
customGap : Gap -> IsGap p
customGap =
    IsGap


{-| -}
type alias Gap =
    { horizontal : Float
    , vertical : Float
    }



-- `Browser.*` alternatives


{-| Settings for rendering `View`.
-}
type alias Renderer =
    Internal.Renderer


mapBaseSize : (Float -> Float) -> BaseSize -> BaseSize
mapBaseSize f (BaseSize unit a) =
    BaseSize unit (f a)


multipleBaseSize : Float -> BaseSize -> BaseSize
multipleBaseSize a =
    mapBaseSize (\s -> a * s)


renderBaseSize : BaseSize -> String
renderBaseSize (BaseSize unit a) =
    String.concat
        [ String.fromFloat a
        , unit
        ]


{-|

  - base size: 1rem

-}
defaultRenderer : Renderer
defaultRenderer =
    Renderer defaultRenderer_


{-| Set the base size in [rem](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem).

> Represents the font-size of the root element (typically <html>).

All gap sizes are determined relative to this value.

-}
setBaseSizeInRem : Float -> Renderer -> Renderer
setBaseSizeInRem a (Renderer renderer) =
    Renderer <|
        { renderer
            | baseSize = BaseSize "rem" a
        }


defaultRenderer_ : Renderer_
defaultRenderer_ =
    { baseSize = BaseSize "rem" 1
    , debug = False
    }



-- Sizing


minWidthZero : MinWidth -> Bool
minWidthZero minWidth =
    case minWidth of
        MinWidthInBs a ->
            a == 0

        MinWidthInUnit _ a ->
            a == 0


minHeightZero : MinHeight -> Bool
minHeightZero minHeight =
    case minHeight of
        MinHeightInBs a ->
            a == 0

        MinHeightInUnit _ a ->
            a == 0



-- Render


{-| Render the `View` into `Html` so that it spreads across the screen.
-}
render : Renderer -> Boundary msg -> Html msg
render (Renderer renderer) boundary =
    Html.div
        []
        [ Html.node "style"
            [ Mixin.style "display" "none"
            ]
            [ Html.text neatLayoutStyle
            ]
        , case boundary of
            Boundary props boundary_ ->
                renderBoundary_
                    renderer
                    (class "top")
                    { props
                        | height = FlexSize
                        , width = FlexSize
                    }
                    boundary_

            NoneBoundary ->
                Html.text ""
        ]


renderView_ :
    Renderer_
    -> Mixin msg
    -> View_ msg
    -> Html msg
renderView_ renderer extraMixin view =
    case view of
        FromBoundary _ props boundary_ ->
            renderBoundary_
                renderer
                extraMixin
                props
                boundary_

        FromRow o ->
            renderRow renderer extraMixin o

        FromColumn o ->
            renderColumn renderer extraMixin o

        FromFlows o ->
            renderFlows renderer extraMixin o


renderBoundary_ :
    Renderer_
    -> Mixin msg
    -> BoundaryProps msg
    -> Boundary_ msg
    -> Html msg
renderBoundary_ renderer extraMixin props boundary_ =
    let
        wrapper =
            Mixin.batch
                [ boundaryCustomProperty renderer props
                , extraMixin
                , class "boundary"
                , case props.height of
                    MinSize ->
                        class "heightMinSize"

                    FlexSize ->
                        class "heightFlex"
                , case props.width of
                    MinSize ->
                        class "widthMinSize"

                    FlexSize ->
                        class "widthFlex"
                , Mixin.when (hasMaxHeight props.maxHeight) <|
                    class "boundary-hasMaxHeight"
                , Mixin.when (hasMaxWidth props.maxWidth) <|
                    class "boundary-hasMaxWidth"
                , Mixin.unless (minHeightZero props.minHeight) <|
                    class "boundary-hasMinHeight"
                , Mixin.unless (minWidthZero props.minWidth) <|
                    class "boundary-hasMinWidth"
                , Mixin.unless (List.isEmpty props.overlays) <|
                    class "boundary-hasOverlays"
                , Mixin.when props.enforcePointerEvent <|
                    class "boundary-enforcePointerEvent"
                ]

        overlays =
            List.reverse props.overlays

        uniqueKey =
            List.map (\(Overlay { name }) -> name) overlays
                |> uniqueKeyFrom
    in
    case boundary_ of
        FromView param ->
            let
                nominalGap =
                    extractNominalGap param.content

                innerGap =
                    Mixin.batch
                        [ variable "inner-gap-x"
                            (multipleBaseSize nominalGap.horizontal renderer.baseSize
                                |> renderBaseSize
                            )
                        , variable "inner-gap-y"
                            (multipleBaseSize nominalGap.vertical renderer.baseSize
                                |> renderBaseSize
                            )
                        ]

                childMixin =
                    class "boundaryChild"
            in
            if overlays == [] then
                Html.node props.nodeName
                    [ props.mixin
                    , innerGap
                    , wrapper
                    , class "boundary_content"
                    ]
                    [ renderView_ renderer childMixin param.content
                    ]

            else
                -- Some elements are only allowed to contain text in their content.
                -- This is a way to prevent Overlay from being included in such elements.
                Html.keyed "div"
                    [ wrapper
                    ]
                    (( uniqueKey
                     , Html.node props.nodeName
                        [ props.mixin
                        , innerGap
                        , class "boundary_content"
                        ]
                        [ renderView_ renderer childMixin param.content
                        ]
                     )
                        :: List.map (renderOverlay renderer) overlays
                    )

        EmptyBoundary ->
            let
                innerGap =
                    Mixin.batch
                        [ variable "inner-gap-x" "0"
                        , variable "inner-gap-y" "0"
                        ]
            in
            if overlays == [] then
                Html.node props.nodeName
                    [ props.mixin
                    , innerGap
                    , wrapper
                    , class "boundary_content"
                    , class "boundary_content-empty"
                    ]
                    []

            else
                -- Some elements are not allowed to contain any contents.
                -- This is a way to prevent Overlay from being included in such elements.
                Html.keyed "div"
                    [ wrapper
                    ]
                    (( uniqueKey
                     , Html.node props.nodeName
                        [ props.mixin
                        , innerGap
                        , class "boundary_content"
                        , class "boundary_content-empty"
                        ]
                        []
                     )
                        :: List.map (renderOverlay renderer) overlays
                    )

        FlowsBoundary param ->
            let
                consFlows ( t, ts ) =
                    t :: ts

                flowHtmls =
                    renderFlowList
                        (class "inline")
                        (consFlows param.texts)

                innerGap =
                    Mixin.batch
                        [ variable "inner-gap-x"
                            (multipleBaseSize param.contentGap.horizontal renderer.baseSize
                                |> renderBaseSize
                            )
                        , variable "inner-gap-y"
                            (multipleBaseSize param.contentGap.vertical renderer.baseSize
                                |> renderBaseSize
                            )
                        ]

                alignments =
                    Mixin.batch
                        [ Mixin.unless param.wrap
                            (class "flows-nowrap")
                        , Mixin.when param.ellipsis
                            (class "flows-ellipsis")
                        , Mixin.when param.preserveWhiteSpace
                            (class "flows-preserveWhiteSpace")
                        , case param.justify of
                            JustifyStart ->
                                class "flows-justifyStart"

                            JustifyCenter ->
                                class "flows-justifyCenter"

                            JustifyEnd ->
                                class "flows-justifyEnd"
                        ]
            in
            if overlays == [] then
                Html.node props.nodeName
                    [ props.mixin
                    , innerGap
                    , wrapper
                    , class "boundary_content"
                    , class "textBoundary"
                    , alignments
                    , variable "content-gap-y"
                        (multipleBaseSize param.contentGap.vertical renderer.baseSize
                            |> renderBaseSize
                        )
                    ]
                    flowHtmls

            else
                Html.keyed "div"
                    [ wrapper
                    ]
                    (( uniqueKey
                     , Html.node props.nodeName
                        [ props.mixin
                        , innerGap
                        , class "boundary_content"
                        , class "textBoundary"
                        , alignments
                        , variable "content-gap-y"
                            (multipleBaseSize param.contentGap.vertical renderer.baseSize
                                |> renderBaseSize
                            )
                        ]
                        flowHtmls
                     )
                        :: List.map (renderOverlay renderer) overlays
                    )

        RowBoundary param ->
            renderRowBoundary
                renderer
                extraMixin
                props
                param

        ColumnBoundary param ->
            renderColumnBoundary
                renderer
                extraMixin
                props
                param


renderRowBoundary :
    Renderer_
    -> Mixin msg
    -> BoundaryProps msg
    -> RowBoundary_ msg
    -> Html msg
renderRowBoundary renderer extraMixin props o =
    let
        childMixin item =
            Mixin.batch
                [ class "rowChild"
                , if item.grow then
                    class "rowChild-grow"

                  else
                    Mixin.none
                , case item.alignSelf of
                    AlignStart ->
                        class "rowChild-alignStart"

                    AlignCenter ->
                        class "rowChild-alignCenter"

                    AlignEnd ->
                        class "rowChild-alignEnd"

                    AlignStretch ->
                        class "rowChild-alignStretch"
                ]

        wrapper =
            Mixin.batch
                [ rowBoundaryCustomProperty renderer props
                , extraMixin
                , class "rowBoundary"
                , Mixin.when o.wrap <|
                    class "rowBoundary-wrap"
                , case o.justifyContent of
                    JustifyStart ->
                        class "rowBoundary-justifyStart"

                    JustifyCenter ->
                        class "rowBoundary-justifyCenter"

                    JustifyEnd ->
                        class "rowBoundary-justifyEnd"
                , case props.height of
                    MinSize ->
                        class "heightMinSize"

                    FlexSize ->
                        class "heightFlex"
                , case props.width of
                    MinSize ->
                        class "widthMinSize"

                    FlexSize ->
                        class "widthFlex"
                , Mixin.when o.scrollable <|
                    class "rowBoundary-horizontalOverflow"
                , Mixin.when (hasMaxHeight props.maxHeight) <|
                    class "rowBoundary-hasMaxHeight"
                , Mixin.when (hasMaxWidth props.maxWidth) <|
                    class "rowBoundary-hasMaxWidth"
                , Mixin.unless (minHeightZero props.minHeight) <|
                    class "rowBoundary-hasMinHeight"
                , Mixin.unless (minWidthZero props.minWidth) <|
                    class "rowBoundary-hasMinWidth"
                , Mixin.unless (List.isEmpty props.overlays) <|
                    class "rowBoundary-hasOverlays"
                , Mixin.when props.enforcePointerEvent <|
                    class "rowBoundary-enforcePointerEvent"
                ]

        overlays =
            List.reverse props.overlays

        (ChildBoundaries children) =
            o.children

        contents =
            (children.head :: children.tail)
                |> List.map
                    (\item ->
                        ( item.key
                        , renderBoundary_
                            renderer
                            (childMixin item)
                            item.props
                            item.content
                        )
                    )

        uniqueKey =
            List.map (\(Overlay { name }) -> name) overlays
                |> uniqueKeyFrom
    in
    if overlays /= [] && o.scrollable then
        -- Prevent overlay elements from moving together when scrolling.
        Html.keyed "div"
            [ wrapper
            ]
            (( uniqueKey
             , Html.keyed props.nodeName
                [ props.mixin
                , class "rowBoundary_content"
                , Mixin.when o.scrollable <|
                    class "rowBoundary_content-horizontalOverflow"
                ]
                contents
             )
                :: List.map (renderOverlay renderer) overlays
            )

    else
        Html.keyed props.nodeName
            [ props.mixin
            , wrapper
            , class "rowBoundary_content"
            , Mixin.when o.scrollable <|
                class "rowBoundary_content-horizontalOverflow"
            ]
            (List.map
                (\( key, c ) -> ( uniqueKey ++ key, c ))
                contents
                ++ List.map (renderOverlay renderer) overlays
            )


uniqueKeyFrom : List String -> String
uniqueKeyFrom =
    List.foldl
        (\str ( length, acc ) ->
            let
                strLength =
                    String.length str
            in
            if strLength > length then
                ( strLength, str )

            else
                ( length, acc )
        )
        ( 0, "" )
        >> Tuple.second
        >> String.append "Unique"


renderColumnBoundary :
    Renderer_
    -> Mixin msg
    -> BoundaryProps msg
    -> ColumnBoundary_ msg
    -> Html msg
renderColumnBoundary renderer extraMixin props o =
    let
        childMixin item =
            Mixin.batch
                [ class "columnChild"
                , Mixin.when item.grow <|
                    class "columnChild-grow"
                , case item.alignSelf of
                    AlignStart ->
                        class "columnChild-alignStart"

                    AlignCenter ->
                        class "columnChild-alignCenter"

                    AlignEnd ->
                        class "columnChild-alignEnd"

                    AlignStretch ->
                        class "columnChild-alignStretch"
                ]

        wrapper =
            Mixin.batch
                [ columnBoundaryCustomProperty renderer props
                , extraMixin
                , class "columnBoundary"
                , case o.justifyContent of
                    JustifyStart ->
                        class "columnBoundary-justifyStart"

                    JustifyCenter ->
                        class "columnBoundary-justifyCenter"

                    JustifyEnd ->
                        class "columnBoundary-justifyEnd"
                , Mixin.when o.scrollable <|
                    class "columnBoundary-verticalOverflow"
                , Mixin.when (hasMaxHeight props.maxHeight) <|
                    class "columnBoundary-hasMaxHeight"
                , Mixin.when (hasMaxWidth props.maxWidth) <|
                    class "columnBoundary-hasMaxWidth"
                , Mixin.unless (minHeightZero props.minHeight) <|
                    class "columnBoundary-hasMinHeight"
                , Mixin.unless (minWidthZero props.minWidth) <|
                    class "columnBoundary-hasMinWidth"
                , case props.height of
                    MinSize ->
                        class "heightMinSize"

                    FlexSize ->
                        class "heightFlex"
                , case props.width of
                    MinSize ->
                        class "widthMinSize"

                    FlexSize ->
                        class "widthFlex"
                , Mixin.unless (List.isEmpty props.overlays) <|
                    class "columnBoundary-hasOverlays"
                , Mixin.when props.enforcePointerEvent <|
                    class "columnBoundary-enforcePointerEvent"
                ]

        overlays =
            List.reverse props.overlays

        (ChildBoundaries children) =
            o.children

        contents =
            (children.head :: children.tail)
                |> List.map
                    (\item ->
                        ( item.key
                        , renderBoundary_
                            renderer
                            (childMixin item)
                            item.props
                            item.content
                        )
                    )

        uniqueKey =
            List.map (\(Overlay { name }) -> name) overlays
                |> uniqueKeyFrom
    in
    if overlays /= [] && o.scrollable then
        -- Prevent overlay elements from moving together when scrolling.
        Html.keyed "div"
            [ wrapper
            ]
            (( uniqueKey
             , Html.keyed props.nodeName
                [ props.mixin
                , class "columnBoundary_content"
                , Mixin.when o.scrollable <|
                    class "columnBoundary_content-verticalOverflow"
                ]
                contents
             )
                :: List.map (renderOverlay renderer) overlays
            )

    else
        Html.keyed props.nodeName
            [ props.mixin
            , wrapper
            , class "columnBoundary_content"
            , Mixin.when o.scrollable <|
                class "columnBoundary_content-verticalOverflow"
            ]
            (List.map
                (\( key, c ) -> ( uniqueKey ++ key, c ))
                contents
                ++ List.map (renderOverlay renderer) overlays
            )


hasMaxHeight : MaxHeight -> Bool
hasMaxHeight mh =
    case mh of
        MaxHeightInBs _ ->
            True

        MaxHeightInUnit _ _ ->
            True

        _ ->
            False


hasMaxWidth : MaxWidth -> Bool
hasMaxWidth mw =
    case mw of
        MaxWidthInBs _ ->
            True

        MaxWidthInUnit _ _ ->
            True

        _ ->
            False


extractNominalGap : View_ msg -> Gap
extractNominalGap view_ =
    case view_ of
        FromBoundary g _ _ ->
            g

        FromRow row_ ->
            row_.nominalGap

        FromColumn column_ ->
            column_.nominalGap

        FromFlows texts_ ->
            texts_.nominalGap


boundaryCustomProperty :
    Renderer_
    -> BoundaryProps msg
    -> Mixin msg
boundaryCustomProperty renderer o =
    Mixin.batch
        [ case o.minWidth of
            MinWidthInBs bs ->
                variable "min-width"
                    (multipleBaseSize bs renderer.baseSize
                        |> renderBaseSize
                    )

            MinWidthInUnit unit v ->
                variable "min-width"
                    (String.fromFloat v ++ unit)
        , case o.maxWidth of
            MaxWidthInBs bs ->
                variable "max-width"
                    (multipleBaseSize bs renderer.baseSize
                        |> renderBaseSize
                    )

            MaxWidthInUnit unit v ->
                variable "max-width"
                    (String.fromFloat v ++ unit)

            _ ->
                Mixin.none
        , case o.minHeight of
            MinHeightInBs bs ->
                variable "min-height"
                    (multipleBaseSize bs renderer.baseSize
                        |> renderBaseSize
                    )

            MinHeightInUnit unit v ->
                variable "min-height"
                    (String.fromFloat v ++ unit)
        , case o.maxHeight of
            MaxHeightInBs bs ->
                variable "max-height"
                    (multipleBaseSize bs renderer.baseSize
                        |> renderBaseSize
                    )

            MaxHeightInUnit unit v ->
                variable "max-height"
                    (String.fromFloat v ++ unit)

            _ ->
                Mixin.none
        ]


rowBoundaryCustomProperty :
    Renderer_
    -> BoundaryProps msg
    -> Mixin msg
rowBoundaryCustomProperty renderer o =
    Mixin.batch
        [ case o.minWidth of
            MinWidthInBs bs ->
                variable "min-width"
                    (multipleBaseSize bs renderer.baseSize
                        |> renderBaseSize
                    )

            MinWidthInUnit unit v ->
                variable "min-width"
                    (String.fromFloat v ++ unit)
        , case o.maxWidth of
            MaxWidthInBs bs ->
                variable "max-width"
                    (multipleBaseSize bs renderer.baseSize
                        |> renderBaseSize
                    )

            MaxWidthInUnit unit v ->
                variable "max-width"
                    (String.fromFloat v ++ unit)

            _ ->
                Mixin.none
        , case o.minHeight of
            MinHeightInBs bs ->
                variable "min-height"
                    (multipleBaseSize bs renderer.baseSize
                        |> renderBaseSize
                    )

            MinHeightInUnit unit v ->
                variable "min-height"
                    (String.fromFloat v ++ unit)
        , case o.maxHeight of
            MaxHeightInBs bs ->
                variable "max-height"
                    (multipleBaseSize bs renderer.baseSize
                        |> renderBaseSize
                    )

            MaxHeightInUnit unit v ->
                variable "max-height"
                    (String.fromFloat v ++ unit)

            _ ->
                Mixin.none
        ]


columnBoundaryCustomProperty :
    Renderer_
    -> BoundaryProps msg
    -> Mixin msg
columnBoundaryCustomProperty renderer o =
    Mixin.batch
        [ case o.minWidth of
            MinWidthInBs bs ->
                variable "min-width"
                    (multipleBaseSize bs renderer.baseSize
                        |> renderBaseSize
                    )

            MinWidthInUnit unit v ->
                variable "min-width"
                    (String.fromFloat v ++ unit)
        , case o.maxWidth of
            MaxWidthInBs bs ->
                variable "max-width"
                    (multipleBaseSize bs renderer.baseSize
                        |> renderBaseSize
                    )

            MaxWidthInUnit unit v ->
                variable "max-width"
                    (String.fromFloat v ++ unit)

            _ ->
                Mixin.none
        , case o.minHeight of
            MinHeightInBs bs ->
                variable "min-height"
                    (multipleBaseSize bs renderer.baseSize
                        |> renderBaseSize
                    )

            MinHeightInUnit unit v ->
                variable "min-height"
                    (String.fromFloat v ++ unit)
        , case o.maxHeight of
            MaxHeightInBs bs ->
                variable "max-height"
                    (multipleBaseSize bs renderer.baseSize
                        |> renderBaseSize
                    )

            MaxHeightInUnit unit v ->
                variable "max-height"
                    (String.fromFloat v ++ unit)

            _ ->
                Mixin.none
        ]


renderRow : Renderer_ -> Mixin msg -> Row_ msg -> Html msg
renderRow renderer extraMixin o =
    let
        base =
            Mixin.batch
                [ o.mixin
                , extraMixin
                , class "row"
                , variable "content-gap-x"
                    (multipleBaseSize o.contentGap.horizontal renderer.baseSize
                        |> renderBaseSize
                    )
                , variable "content-gap-y"
                    (multipleBaseSize o.contentGap.vertical renderer.baseSize
                        |> renderBaseSize
                    )
                , Mixin.when o.wrap <|
                    class "row-wrap"
                , case o.justifyContent of
                    JustifyStart ->
                        class "row-justifyStart"

                    JustifyCenter ->
                        class "row-justifyCenter"

                    JustifyEnd ->
                        class "row-justifyEnd"
                ]

        childMixin item =
            Mixin.batch
                [ class "rowChild"
                , Mixin.when item.grow <|
                    class "rowChild-grow"
                , case item.alignSelf of
                    AlignStart ->
                        class "rowChild-alignStart"

                    AlignCenter ->
                        class "rowChild-alignCenter"

                    AlignEnd ->
                        class "rowChild-alignEnd"

                    AlignStretch ->
                        class "rowChild-alignStretch"
                ]
    in
    case o.children of
        Children item0 items ->
            (item0 :: items)
                |> List.map
                    (\item ->
                        ( item.key, renderView_ renderer (childMixin item) item.content )
                    )
                |> Html.keyed o.nodeName
                    [ base
                    ]


renderColumn : Renderer_ -> Mixin msg -> Column_ msg -> Html msg
renderColumn renderer extraMixin o =
    let
        base =
            Mixin.batch
                [ o.mixin
                , extraMixin
                , class "column"
                , variable "content-gap-x"
                    (multipleBaseSize o.contentGap.horizontal renderer.baseSize
                        |> renderBaseSize
                    )
                , variable "content-gap-y"
                    (multipleBaseSize o.contentGap.vertical renderer.baseSize
                        |> renderBaseSize
                    )
                , case o.justifyContent of
                    JustifyStart ->
                        class "column-justifyStart"

                    JustifyCenter ->
                        class "column-justifyCenter"

                    JustifyEnd ->
                        class "column-justifyEnd"
                ]

        childMixin item =
            Mixin.batch
                [ class "columnChild"
                , Mixin.when item.grow <|
                    class "columnChild-grow"
                , case item.alignSelf of
                    AlignStart ->
                        class "columnChild-alignStart"

                    AlignCenter ->
                        class "columnChild-alignCenter"

                    AlignEnd ->
                        class "columnChild-alignEnd"

                    AlignStretch ->
                        class "columnChild-alignStretch"
                ]
    in
    case o.children of
        Children item0 items ->
            (item0 :: items)
                |> List.map
                    (\item ->
                        ( item.key, renderView_ renderer (childMixin item) item.content )
                    )
                |> Html.keyed o.nodeName
                    [ base
                    ]


renderFlows : Renderer_ -> Mixin msg -> Flows_ msg -> Html msg
renderFlows renderer extraMixin o =
    let
        consFlows ( t, ts ) =
            t :: ts

        base =
            Mixin.batch
                [ o.mixin
                , extraMixin
                , class "textView"
                , Mixin.unless o.wrap
                    (class "flows-nowrap")
                , Mixin.when o.ellipsis
                    (class "flows-ellipsis")
                , Mixin.when o.preserveWhiteSpace
                    (class "flows-preserveWhiteSpace")
                , case o.justify of
                    JustifyStart ->
                        class "flows-justifyStart"

                    JustifyCenter ->
                        class "flows-justifyCenter"

                    JustifyEnd ->
                        class "flows-justifyEnd"
                , variable "content-gap-y"
                    (multipleBaseSize o.contentGap.vertical renderer.baseSize
                        |> renderBaseSize
                    )
                ]

        textHtmls =
            renderFlowList
                (class "inline")
                (consFlows o.texts)
    in
    Html.node
        (o.nodeName
            |> Maybe.withDefault "div"
        )
        [ base
        ]
        textHtmls


renderFlowList : Mixin msg -> List (Internal.Flow msg) -> List (Html msg)
renderFlowList mixin =
    List.filterMap
        (\inline ->
            case inline.nodeName of
                Nothing ->
                    if inline.mixin == Mixin.none then
                        if inline.text == "" then
                            Nothing

                        else
                            -- Some elements are only allowed to contain text in their content.
                            -- For such cases, we convert the expression here to be as simple as possible.
                            Html.text inline.text
                                |> Just

                    else
                        Html.span
                            [ inline.mixin
                            , mixin
                            ]
                            [ Html.text inline.text
                            ]
                            |> Just

                Just nodeName ->
                    Html.node nodeName
                        [ inline.mixin
                        , mixin
                        ]
                        [ Html.text inline.text
                        ]
                        |> Just
        )


renderOverlay : Renderer_ -> Overlay msg -> ( String, Html msg )
renderOverlay renderer (Overlay overlay) =
    let
        childMixin =
            Mixin.batch
                [ class "overlay"
                , variable "overlay-top" (String.fromFloat overlay.area.top ++ "%")
                , variable "overlay-bottom" (String.fromFloat overlay.area.bottom ++ "%")
                , variable "overlay-left" (String.fromFloat overlay.area.left ++ "%")
                , variable "overlay-right" (String.fromFloat overlay.area.right ++ "%")
                , variable "overlay-priority"
                    (overlay.area.priority
                        |> Maybe.map String.fromInt
                        |> Maybe.withDefault "auto"
                    )
                ]
    in
    ( "overlay-" ++ overlay.name
    , renderBoundary_ renderer childMixin overlay.props overlay.boundary_
    )


class : String -> Mixin msg
class str =
    Mixin.class <| "elmNeatLayout--" ++ str


variable : String -> String -> Mixin msg
variable prop val =
    Mixin.style ("--elmneatlayout--" ++ prop) val



-- Handle conditions


{-| Apply a modifier only when a condition is met.
-}
when : Bool -> (a -> a) -> a -> a
when p f =
    if p then
        f

    else
        identity


{-| Apply a modifier unless a condition is met.
-}
unless : Bool -> (a -> a) -> a -> a
unless p =
    when (not p)


{-| Apply a modifier only if the given value is `Just`.
-}
withMaybe : Maybe a -> (a -> b -> b) -> b -> b
withMaybe ma f =
    case ma of
        Just a ->
            f a

        Nothing ->
            identity


neatLayoutStyle : String
neatLayoutStyle =
    """.elmNeatLayout,.elmNeatLayout:before,.elmNeatLayout:after{box-sizing:border-box;margin:0;padding:0}.elmNeatLayout--top{display:block;position:fixed;inset:0;overflow:hidden}.elmNeatLayout--overlay{pointer-events:none;top:var(--elmneatlayout--overlay-top);bottom:var(--elmneatlayout--overlay-bottom);left:var(--elmneatlayout--overlay-left);right:var(--elmneatlayout--overlay-right);z-index:var(--elmneatlayout--overlay-priority);display:block;position:absolute;overflow:hidden}.elmNeatLayout--boundary_content{padding:var(--elmneatlayout--inner-gap-y)var(--elmneatlayout--inner-gap-x);display:block;overflow-x:clip;overflow-y:visible}.elmNeatLayout--boundary-hasMaxHeight{max-height:var(--elmneatlayout--max-height)}.elmNeatLayout--boundary-hasMaxWidth{max-width:var(--elmneatlayout--max-width)}.elmNeatLayout--boundary-hasMinHeight{min-height:var(--elmneatlayout--min-height)}.elmNeatLayout--boundary-hasMinWidth{min-width:var(--elmneatlayout--min-width)}.elmNeatLayout--boundary-hasOverlays:not(.elmNeatLayout--top){position:relative}.elmNeatLayout--boundary-enforcePointerEvent{pointer-events:auto}.elmNeatLayout--boundary:not(.elmNeatLayout--boundary_content){flex-direction:row;align-items:stretch;display:flex}.elmNeatLayout--boundary:not(.elmNeatLayout--boundary_content)>.elmNeatLayout--boundary_content{height:auto;flex-grow:1}.elmNeatLayout--boundary.elmNeatLayout--rowChild{width:auto;flex-shrink:1}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--heightMinSize{height:auto}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--heightFlex{height:100%}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--heightFlex.elmNeatLayout--rowChild-alignStretch{height:auto}.elmNeatLayout--boundary.elmNeatLayout--columnChild{height:auto;flex-shrink:1}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--widthMinSize{width:auto}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--widthFlex{width:100%}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--widthFlex.elmNeatLayout--columnChild-alignStretch{width:auto}.elmNeatLayout--rowBoundary_content{flex-flow:row;display:flex;overflow:hidden}.elmNeatLayout--rowBoundary-wrap{flex-wrap:wrap}.elmNeatLayout--rowBoundary-justifyStart{justify-content:flex-start}.elmNeatLayout--rowBoundary-justifyCenter{justify-content:center}.elmNeatLayout--rowBoundary-justifyEnd{justify-content:flex-end}.elmNeatLayout--rowBoundary-hasMaxHeight{max-height:var(--elmneatlayout--max-height)}.elmNeatLayout--rowBoundary-hasMaxWidth{max-width:var(--elmneatlayout--max-width)}.elmNeatLayout--rowBoundary-hasMinHeight{min-height:var(--elmneatlayout--min-height)}.elmNeatLayout--rowBoundary-hasMinWidth{min-width:var(--elmneatlayout--min-width)}.elmNeatLayout--rowBoundary-hasOverlays:not(.elmNeatLayout--top){position:relative}.elmNeatLayout--rowBoundary-enforcePointerEvent{pointer-events:auto}.elmNeatLayout--rowBoundary:not(.elmNeatLayout--rowBoundary_content){flex-direction:row;align-items:stretch;display:flex}.elmNeatLayout--rowBoundary:not(.elmNeatLayout--rowBoundary_content)>.elmNeatLayout--rowBoundary_content{height:auto;flex-grow:1}.elmNeatLayout--rowBoundary.elmNeatLayout--rowChild{flex-shrink:1}.elmNeatLayout--rowBoundary.elmNeatLayout--rowChild:not(.elmNeatLayout--rowBoundary-horizontalOverflow){width:auto}.elmNeatLayout--rowBoundary.elmNeatLayout--rowChild.elmNeatLayout--rowBoundary-horizontalOverflow:not(.elmNeatLayout--rowChild-grow){width:0}.elmNeatLayout--rowBoundary.elmNeatLayout--rowChild.elmNeatLayout--rowBoundary-horizontalOverflow.elmNeatLayout--rowChild-grow{width:auto}.elmNeatLayout--rowBoundary.elmNeatLayout--rowChild.elmNeatLayout--heightMinSize{height:auto}.elmNeatLayout--rowBoundary.elmNeatLayout--rowChild.elmNeatLayout--heightFlex{height:100%}.elmNeatLayout--rowBoundary.elmNeatLayout--rowChild.elmNeatLayout--heightFlex.elmNeatLayout--rowChild-alignStretch{height:auto}.elmNeatLayout--rowBoundary.elmNeatLayout--columnChild{height:auto;flex-shrink:1}.elmNeatLayout--rowBoundary.elmNeatLayout--columnChild.elmNeatLayout--widthMinSize{width:auto}.elmNeatLayout--rowBoundary.elmNeatLayout--columnChild.elmNeatLayout--widthFlex{width:100%}.elmNeatLayout--rowBoundary.elmNeatLayout--columnChild.elmNeatLayout--widthFlex.elmNeatLayout--columnChild-alignStretch{width:auto}.elmNeatLayout--rowBoundary_content-horizontalOverflow{overflow-x:auto;overflow-y:hidden}.elmNeatLayout--rowBoundary_content-horizontalOverflow>.elmNeatLayout--rowChild{flex-shrink:0}.elmNeatLayout--rowBoundary_content>.elmNeatLayout--rowChild{flex-grow:0}.elmNeatLayout--rowBoundary_content>.elmNeatLayout--rowChild-grow{flex-grow:1}.elmNeatLayout--rowBoundary_content>.elmNeatLayout--rowChild-alignStart{align-self:flex-start}.elmNeatLayout--rowBoundary_content>.elmNeatLayout--rowChild-alignCenter{align-self:center}.elmNeatLayout--rowBoundary_content>.elmNeatLayout--rowChild-alignEnd{align-self:flex-end}.elmNeatLayout--rowBoundary_content>.elmNeatLayout--rowChild-alignStretch{align-self:stretch}.elmNeatLayout--columnBoundary_content{flex-flow:column;display:flex;overflow:hidden}.elmNeatLayout--columnBoundary-justifyStart{justify-content:flex-start}.elmNeatLayout--columnBoundary-justifyCenter{justify-content:center}.elmNeatLayout--columnBoundary-justifyEnd{justify-content:flex-end}.elmNeatLayout--columnBoundary-hasMaxHeight{max-height:var(--elmneatlayout--max-height)}.elmNeatLayout--columnBoundary-hasMaxWidth{max-width:var(--elmneatlayout--max-width)}.elmNeatLayout--columnBoundary-hasMinHeight{min-height:var(--elmneatlayout--min-height)}.elmNeatLayout--columnBoundary-hasMinWidth{min-width:var(--elmneatlayout--min-width)}.elmNeatLayout--columnBoundary-hasOverlays:not(.elmNeatLayout--top){position:relative}.elmNeatLayout--columnBoundary-enforcePointerEvent{pointer-events:auto}.elmNeatLayout--columnBoundary:not(.elmNeatLayout--columnBoundary_content){flex-direction:row;align-items:stretch;display:flex}.elmNeatLayout--columnBoundary:not(.elmNeatLayout--columnBoundary_content)>.elmNeatLayout--columnBoundary_content{height:auto;flex-grow:1}.elmNeatLayout--columnBoundary.elmNeatLayout--rowChild{width:auto;flex-shrink:1}.elmNeatLayout--columnBoundary.elmNeatLayout--rowChild.elmNeatLayout--heightMinSize{height:auto}.elmNeatLayout--columnBoundary.elmNeatLayout--rowChild.elmNeatLayout--heightFlex{height:100%}.elmNeatLayout--columnBoundary.elmNeatLayout--rowChild.elmNeatLayout--heightFlex.elmNeatLayout--rowChild-alignStretch{height:auto}.elmNeatLayout--columnBoundary.elmNeatLayout--columnChild{flex-shrink:1}.elmNeatLayout--columnBoundary.elmNeatLayout--columnChild:not(.elmNeatLayout--columnBoundary-verticalOverflow){height:auto}.elmNeatLayout--columnBoundary.elmNeatLayout--columnChild.elmNeatLayout--columnBoundary-verticalOverflow:not(.elmNeatLayout--columnChild-grow){height:0}.elmNeatLayout--columnBoundary.elmNeatLayout--columnChild.elmNeatLayout--columnBoundary-verticalOverflow:not(.elmNeatLayout--columnBoundary_content){overflow-y:hidden}.elmNeatLayout--columnBoundary.elmNeatLayout--columnChild.elmNeatLayout--columnBoundary-verticalOverflow.elmNeatLayout--columnChild-grow{height:100%}.elmNeatLayout--columnBoundary.elmNeatLayout--columnChild.elmNeatLayout--widthMinSize{width:auto}.elmNeatLayout--columnBoundary.elmNeatLayout--columnChild.elmNeatLayout--widthFlex{width:100%}.elmNeatLayout--columnBoundary.elmNeatLayout--columnChild.elmNeatLayout--widthFlex.elmNeatLayout--columnChild-alignStretch{width:auto}.elmNeatLayout--columnBoundary_content-verticalOverflow{overflow-x:hidden;overflow-y:auto}.elmNeatLayout--columnBoundary_content-verticalOverflow>.elmNeatLayout--columnChild{flex-shrink:0}.elmNeatLayout--columnBoundary_content>.elmNeatLayout--columnChild{flex-grow:0}.elmNeatLayout--columnBoundary_content>.elmNeatLayout--columnChild-grow{flex-grow:1}.elmNeatLayout--columnBoundary_content>.elmNeatLayout--columnChild-alignStart{align-self:flex-start}.elmNeatLayout--columnBoundary_content>.elmNeatLayout--columnChild-alignCenter{align-self:center}.elmNeatLayout--columnBoundary_content>.elmNeatLayout--columnChild-alignEnd{align-self:flex-end}.elmNeatLayout--columnBoundary_content>.elmNeatLayout--columnChild-alignStretch{align-self:stretch}.elmNeatLayout--row{gap:var(--elmneatlayout--content-gap-y)var(--elmneatlayout--content-gap-x);flex-flow:row;display:flex}.elmNeatLayout--row-wrap{flex-wrap:wrap}.elmNeatLayout--row-justifyStart{justify-content:flex-start}.elmNeatLayout--row-justifyCenter{justify-content:center}.elmNeatLayout--row-justifyEnd{justify-content:flex-end}.elmNeatLayout--row>.elmNeatLayout--rowChild{flex-grow:0;flex-shrink:1}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-grow{flex-grow:1}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignStart{align-self:flex-start}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignCenter{align-self:center}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignEnd{align-self:flex-end}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignStretch{align-self:stretch}.elmNeatLayout--row.elmNeatLayout--rowChild{width:auto;height:auto;flex-shrink:1}.elmNeatLayout--row.elmNeatLayout--columnChild{height:auto;width:100%;flex-shrink:1}.elmNeatLayout--row.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignStretch{width:auto}.elmNeatLayout--row.elmNeatLayout--boundaryChild{width:100%;height:100%}.elmNeatLayout--column{gap:var(--elmneatlayout--content-gap-y)var(--elmneatlayout--content-gap-x);flex-flow:column;display:flex}.elmNeatLayout--column-justifyStart{justify-content:flex-start}.elmNeatLayout--column-justifyCenter{justify-content:center}.elmNeatLayout--column-justifyEnd{justify-content:flex-end}.elmNeatLayout--column>.elmNeatLayout--columnChild{flex-grow:0;flex-shrink:1}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-grow{flex-grow:1}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignStart{align-self:flex-start}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignCenter{align-self:center}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignEnd{align-self:flex-end}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignStretch{align-self:stretch}.elmNeatLayout--column.elmNeatLayout--rowChild{width:auto;height:100%;flex-shrink:1}.elmNeatLayout--column.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignStretch{height:auto}.elmNeatLayout--column.elmNeatLayout--columnChild{height:auto;width:auto;flex-shrink:1}.elmNeatLayout--column.elmNeatLayout--boundaryChild{width:100%;height:100%}.elmNeatLayout--textView{margin:calc(var(--elmneatlayout--content-gap-y)/-2)0;display:block;overflow-x:clip;overflow-y:visible}.elmNeatLayout--textView,.elmNeatLayout--textBoundary{line-height:calc(1em + var(--elmneatlayout--content-gap-y))}.elmNeatLayout--flows-justifyStart{text-align:start}.elmNeatLayout--flows-justifyCenter{text-align:center}.elmNeatLayout--flows-justifyEnd{text-align:end}.elmNeatLayout--flows-ellipsis{text-overflow:ellipsis}.elmNeatLayout--flows:not(.elmNeatLayout--flows-nowrap):not(.elmNeatLayout--flows-preserveWhiteSpace){white-space:normal}.elmNeatLayout--flows:not(.elmNeatLayout--flows-nowrap).elmNeatLayout--flows-preserveWhiteSpace{white-space:break-spaces}.elmNeatLayout--flows-nowrap:not(.elmNeatLayout--flows-preserveWhiteSpace){white-space:nowrap}.elmNeatLayout--flows-nowrap.elmNeatLayout--flows-preserveWhiteSpace{white-space:pre}.elmNeatLayout--inline{line-height:calc(1em + var(--elmneatlayout--content-gap-y));display:inline}"""
