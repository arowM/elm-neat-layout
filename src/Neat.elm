module Neat exposing
    ( View
    , Boundary
    , NoGap
    , noGap
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
@docs NoGap
@docs noGap


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
import Neat.Internal as Internal
    exposing
        ( Alignment(..)
        , BaseSize(..)
        , Boundary(..)
        , Boundary_
        , Children(..)
        , Column(..)
        , ColumnItem(..)
        , Column_
        , Content(..)
        , Gap
        , IsGap(..)
        , Item_
        , Layered(..)
        , MaxHeight(..)
        , MaxWidth(..)
        , MinHeight(..)
        , MinWidth(..)
        , Overlay
        , Renderer(..)
        , Renderer_
        , Row(..)
        , RowItem(..)
        , Row_
        , Size(..)
        , View(..)
        , View_(..)
        )
import Neat.Text as Text



-- Core


{-| A gap-sensible `Html msg` alternative.
-}
type alias View gap msg =
    Internal.View gap msg


{-| A bounded View without gap.

Convert to/from View by `setGap`/`setBoundary`.

-}
type alias Boundary msg =
    Internal.Boundary msg


mapOverlays : (a -> b) -> List (Overlay a) -> List (Overlay b)
mapOverlays f =
    List.map
        (\o ->
            { name = o.name
            , area = o.area
            , boundary = mapBoundary f o.boundary
            }
        )


{-| -}
type alias RowItem gap msg =
    Internal.RowItem gap msg


{-| -}
type alias ColumnItem gap msg =
    Internal.ColumnItem gap msg


modifyChild : (View_ a -> View_ b) -> Children a -> Children b
modifyChild f (Children item0 items) =
    let
        modifyContent : Item_ a -> Item_ b
        modifyContent item =
            { alignSelf = item.alignSelf
            , grow = item.grow
            , key = item.key
            , content = f item.content
            }
    in
    List.map modifyContent items
        |> Children (modifyContent item0)


map_ : (a -> b) -> View_ a -> View_ b
map_ f view =
    case view of
        FromBoundary boundary ->
            FromBoundary <| mapBoundary_ f boundary

        FromRow o ->
            FromRow
                { mixin = Mixin.map f o.mixin
                , gap = o.gap
                , nodeName = o.nodeName
                , justifyContent = o.justifyContent
                , children = modifyChild (map_ f) o.children
                , wrap = o.wrap
                }

        FromColumn o ->
            FromColumn
                { mixin = Mixin.map f o.mixin
                , gap = o.gap
                , nodeName = o.nodeName
                , justifyContent = o.justifyContent
                , children = modifyChild (map_ f) o.children
                }

        None ->
            None


{-| -}
mapBoundary : (a -> b) -> Boundary a -> Boundary b
mapBoundary f (Boundary boundary) =
    mapBoundary_ f boundary
        |> Boundary


mapBoundary_ : (a -> b) -> Boundary_ a -> Boundary_ b
mapBoundary_ f o =
    { mixin = Mixin.map f o.mixin
    , gap = o.gap
    , nodeName = o.nodeName
    , innerGap = o.innerGap
    , overlays = mapOverlays f o.overlays
    , width = o.width
    , minWidth = o.minWidth
    , maxWidth = o.maxWidth
    , horizontalOverflow = o.horizontalOverflow
    , height = o.height
    , minHeight = o.minHeight
    , maxHeight = o.maxHeight
    , verticalOverflow = o.verticalOverflow
    , content =
        case o.content of
            TextsContent texts ->
                TextsContent <| List.map (Text.map f) texts

            ViewContent view ->
                ViewContent <| map_ f view

            HtmlContent children ->
                HtmlContent <|
                    List.map
                        (\( k, b ) ->
                            ( k, mapBoundary_ f b )
                        )
                        children

            StringContent str ->
                StringContent str

            NoContent ->
                NoContent
    , enforcePointerEvent = o.enforcePointerEvent
    }


{-| A primitive type that represents that there is no Gap

For custom gaps, see [Custom gaps](#custom-gaps).

-}
type NoGap
    = NoGap Never


{-| -}
noGap : IsGap NoGap
noGap =
    IsGap emptyGap


emptyGap : Gap
emptyGap =
    { vertical = 0
    , horizontal = 0
    }



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



-- Primitive nodes


{-| -}
type alias Row =
    Internal.Row


{-| -}
type alias Column =
    Internal.Column



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
render (Renderer renderer) (Boundary boundary) =
    let
        childMixin =
            { inherit =
                Mixin.batch
                    [ class "heightFlex"
                    , class "widthFlex"
                    ]
            , self = class "top"
            }
    in
    Html.div
        []
        [ Html.node "style"
            [ Mixin.style "display" "none"
            ]
            [ Html.text neatLayoutStyle
            ]
        , { boundary
            | height = FlexSize
            , width = FlexSize
          }
            |> renderBoundary renderer childMixin
        ]


type alias ChildMixin msg =
    { inherit : Mixin msg
    , self : Mixin msg
    }


render_ :
    Renderer_
    -> ChildMixin msg
    -> View_ msg
    -> Html msg
render_ renderer childMixin view =
    case view of
        FromBoundary o ->
            renderBoundary renderer childMixin o

        FromRow o ->
            renderRow renderer childMixin o

        FromColumn o ->
            renderColumn renderer childMixin o

        None ->
            Html.text ""


renderBoundary : Renderer_ -> ChildMixin msg -> Boundary_ msg -> Html msg
renderBoundary renderer { self } o =
    let
        childMixin =
            { inherit =
                Mixin.batch
                    [ case o.height of
                        MinSize ->
                            class "heightMinSize"

                        FlexSize ->
                            class "heightFlex"
                    , case o.width of
                        MinSize ->
                            class "widthMinSize"

                        FlexSize ->
                            class "widthFlex"
                    ]
            , self = class "boundaryContent"
            }

        base =
            Mixin.batch
                [ o.mixin
                , boundaryCustomProperty renderer o
                , childMixin.inherit
                , self
                , class "boundary"
                , if o.horizontalOverflow then
                    class "boundary-horizontalOverflow"

                  else
                    Mixin.none
                , if o.verticalOverflow then
                    class "boundary-verticalOverflow"

                  else
                    Mixin.none
                , if hasMaxHeight o.maxHeight then
                    class "boundary-hasMaxHeight"

                  else
                    Mixin.none
                , if hasMaxWidth o.maxWidth then
                    class "boundary-hasMaxWidth"

                  else
                    Mixin.none
                , if not (minHeightZero o.minHeight) then
                    class "boundary-hasMinHeight"

                  else
                    Mixin.none
                , if not (minWidthZero o.minWidth) then
                    class "boundary-hasMinWidth"

                  else
                    Mixin.none
                , if not <| List.isEmpty o.overlays then
                    class "boundary-hasOverlays"

                  else
                    Mixin.none
                , if o.enforcePointerEvent then
                    class "boundary-enforcePointerEvent"

                  else
                    Mixin.none
                ]

        overlays =
            List.reverse o.overlays
    in
    case o.content of
        NoContent ->
            Html.text ""

        ViewContent content ->
            if o.verticalOverflow && o.innerGap.vertical /= 0 then
                Html.node o.nodeName
                    [ base
                    ]
                    [ Html.keyed "div"
                        [ class "boundary_scroller"
                        , class "boundary_scroller-verticalScroll"
                        ]
                        (( "content", render_ renderer childMixin content )
                            :: List.map (renderOverlay renderer) overlays
                        )
                    ]

            else if content == None then
                Html.keyed o.nodeName
                    [ base
                    , class "boundary-view"
                    , class "boundary-view-noContent"
                    ]
                    (List.map (renderOverlay renderer) overlays)

            else
                Html.keyed o.nodeName
                    [ base
                    , class "boundary-view"
                    , class "boundary-view-hasContent"
                    ]
                    (( "content", render_ renderer childMixin content )
                        :: List.map (renderOverlay renderer) overlays
                    )

        HtmlContent children ->
            children
                |> List.map
                    (\( k, b ) ->
                        ( k
                        , renderBoundary renderer childMixin b
                        )
                    )
                |> (\cs ->
                        cs
                            ++ List.map (renderOverlay renderer) overlays
                   )
                |> Html.keyed o.nodeName
                    [ base
                    , class "boundary-html"
                    ]

        TextsContent texts ->
            Html.node o.nodeName
                [ base
                , class "boundary-text"
                ]
                [ texts
                    |> List.indexedMap
                        (\n inline ->
                            ( "content" ++ String.fromInt n
                            , Html.node inline.nodeName
                                [ inline.mixin
                                , class "boundary_text"
                                ]
                                [ Html.text inline.text
                                ]
                            )
                        )
                    |> (\children ->
                            children
                                ++ List.map (renderOverlay renderer) overlays
                       )
                    |> Html.keyed "div"
                        [ class "boundary_textMargin"
                        ]
                ]

        StringContent str ->
            Html.node o.nodeName
                [ base
                ]
                [ Html.text str
                ]


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


boundaryCustomProperty : Renderer_ -> Boundary_ msg -> Mixin msg
boundaryCustomProperty renderer o =
    Mixin.batch
        [ Mixin.style "--outer-gap-x"
            (multipleBaseSize o.gap.horizontal renderer.baseSize
                |> renderBaseSize
            )
        , Mixin.style "--outer-gap-y"
            (multipleBaseSize o.gap.vertical renderer.baseSize
                |> renderBaseSize
            )
        , Mixin.style "--inner-gap-x"
            (multipleBaseSize o.innerGap.horizontal renderer.baseSize
                |> renderBaseSize
            )
        , Mixin.style "--inner-gap-y"
            (multipleBaseSize o.innerGap.vertical renderer.baseSize
                |> renderBaseSize
            )
        , case o.minWidth of
            MinWidthInBs bs ->
                Mixin.style "--min-width"
                    (multipleBaseSize bs renderer.baseSize
                        |> renderBaseSize
                    )

            MinWidthInUnit unit v ->
                Mixin.style "--min-width"
                    (String.fromFloat v ++ unit)
        , case o.maxWidth of
            MaxWidthInBs bs ->
                Mixin.style "--max-width"
                    (multipleBaseSize bs renderer.baseSize
                        |> renderBaseSize
                    )

            MaxWidthInUnit unit v ->
                Mixin.style "--max-width"
                    (String.fromFloat v ++ unit)

            _ ->
                Mixin.none
        , case o.minHeight of
            MinHeightInBs bs ->
                Mixin.style "--min-height"
                    (multipleBaseSize bs renderer.baseSize
                        |> renderBaseSize
                    )

            MinHeightInUnit unit v ->
                Mixin.style "--min-height"
                    (String.fromFloat v ++ unit)
        , case o.maxHeight of
            MaxHeightInBs bs ->
                Mixin.style "--max-height"
                    (multipleBaseSize bs renderer.baseSize
                        |> renderBaseSize
                    )

            MaxHeightInUnit unit v ->
                Mixin.style "--max-height"
                    (String.fromFloat v ++ unit)

            _ ->
                Mixin.none
        ]


renderRow : Renderer_ -> ChildMixin msg -> Row_ msg -> Html msg
renderRow renderer { inherit, self } o =
    let
        base =
            Mixin.batch
                [ o.mixin
                , self
                , class "row"
                , if o.wrap then
                    class "row-wrap"

                  else
                    Mixin.none
                , case o.justifyContent of
                    AlignStart ->
                        class "row-justifyStart"

                    AlignCenter ->
                        class "row-justifyCenter"

                    AlignEnd ->
                        class "row-justifyEnd"

                    AlignStretch ->
                        class "row-justifyStretch"
                ]

        childMixin item =
            { inherit = inherit
            , self =
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
            }
    in
    case o.children of
        Children item [] ->
            Html.keyed o.nodeName
                [ base
                , class "row-single"
                ]
                [ ( item.key, render_ renderer (childMixin item) item.content )
                ]

        Children item0 items ->
            (item0 :: items)
                |> List.map
                    (\item ->
                        ( item.key, render_ renderer (childMixin item) item.content )
                    )
                |> Html.keyed o.nodeName
                    [ base
                    , class "row-multi"
                    ]


renderColumn : Renderer_ -> ChildMixin msg -> Column_ msg -> Html msg
renderColumn renderer { inherit, self } o =
    let
        base =
            Mixin.batch
                [ o.mixin
                , self
                , class "column"
                , case o.justifyContent of
                    AlignStart ->
                        class "column-justifyStart"

                    AlignCenter ->
                        class "column-justifyCenter"

                    AlignEnd ->
                        class "column-justifyEnd"

                    AlignStretch ->
                        class "column-justifyStretch"
                ]

        childMixin item =
            { inherit = inherit
            , self =
                Mixin.batch
                    [ class "columnChild"
                    , if item.grow then
                        class "columnChild-grow"

                      else
                        Mixin.none
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
            }
    in
    case o.children of
        Children item [] ->
            Html.keyed o.nodeName
                [ base
                , class "column-single"
                ]
                [ ( item.key, render_ renderer (childMixin item) item.content )
                ]

        Children item0 items ->
            (item0 :: items)
                |> List.map
                    (\item ->
                        ( item.key, render_ renderer (childMixin item) item.content )
                    )
                |> Html.keyed o.nodeName
                    [ base
                    , class "column-multi"
                    ]


renderOverlay : Renderer_ -> Overlay msg -> ( String, Html msg )
renderOverlay renderer overlay =
    let
        (Boundary boundary) =
            overlay.boundary

        childMixin =
            { inherit =
                Mixin.batch
                    [ class "heightFlex"
                    , class "widthFlex"
                    ]
            , self =
                Mixin.batch
                    [ class "overlay"
                    , Mixin.style "--overlay-top" (String.fromFloat overlay.area.top ++ "%")
                    , Mixin.style "--overlay-bottom" (String.fromFloat overlay.area.bottom ++ "%")
                    , Mixin.style "--overlay-left" (String.fromFloat overlay.area.left ++ "%")
                    , Mixin.style "--overlay-right" (String.fromFloat overlay.area.right ++ "%")
                    , Mixin.style "--overlay-priority"
                        (overlay.area.priority
                            |> Maybe.map String.fromInt
                            |> Maybe.withDefault "auto"
                        )
                    ]
            }
    in
    ( "overlay-" ++ overlay.name
    , { boundary
        | height = FlexSize
        , width = FlexSize
      }
        |> renderBoundary renderer childMixin
    )


class : String -> Mixin msg
class str =
    Mixin.class <| "elmNeatLayout--" ++ str



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
    """.elmNeatLayout,.elmNeatLayout:before,.elmNeatLayout:after{box-sizing:border-box;margin:0;padding:0}.elmNeatLayout--top{display:block;position:fixed;inset:0;overflow:hidden}.elmNeatLayout--overlay{pointer-events:none;top:var(--overlay-top);bottom:var(--overlay-bottom);left:var(--overlay-left);right:var(--overlay-right);z-index:var(--overlay-priority);display:block;position:absolute;overflow:hidden}.elmNeatLayout--boundary{overflow:hidden}.elmNeatLayout--boundary-hasOverlays:not(.elmNeatLayout--top){position:relative}.elmNeatLayout--boundary-enforcePointerEvent{pointer-events:auto}.elmNeatLayout--boundary>.elmNeatLayout--boundary_scroller{height:100%;width:100%}.elmNeatLayout--boundary>.elmNeatLayout--boundary_scroller-verticalScroll>.elmNeatLayout--boundaryContent{height:auto;min-height:100%}.elmNeatLayout--boundary>.elmNeatLayout--boundary_scroller-horizontalScroll>.elmNeatLayout--boundaryContent{width:auto;min-width:100%}.elmNeatLayout--boundary-view-hasContent,.elmNeatLayout--boundary>.elmNeatLayout--boundary_scroller{padding:var(--inner-gap-y)var(--inner-gap-x)}.elmNeatLayout--boundary-text{overflow:visible}.elmNeatLayout--boundary-text>.elmNeatLayout--boundary_textMargin{height:auto;width:100%}.elmNeatLayout--boundary-text>.elmNeatLayout--boundary_textMargin:before{width:0;height:0;margin-top:calc(var(--outer-gap-y)/-2);content:"";display:block}.elmNeatLayout--boundary-text>.elmNeatLayout--boundary_textMargin:after{width:0;height:0;margin-bottom:calc(var(--outer-gap-y)/-2);content:"";display:block}.elmNeatLayout--boundary-text>.elmNeatLayout--boundary_textMargin>.elmNeatLayout--boundary_text{line-height:calc(1em + var(--outer-gap-y));display:inline}.elmNeatLayout--boundary-horizontalOverflow,.elmNeatLayout--boundary-horizontalOverflow>.elmNeatLayout--boundary_scroller{overflow-x:auto}.elmNeatLayout--boundary-verticalOverflow,.elmNeatLayout--boundary-verticalOverflow>.elmNeatLayout--boundary_scroller{overflow-y:auto}.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--boundary-text>.elmNeatLayout--boundary_textMargin{overflow-y:hidden}.elmNeatLayout--boundary-hasMinHeight{min-height:var(--min-height)}.elmNeatLayout--boundary-hasMaxHeight:not(.elmNeatLayout--boundary-verticalOverflow){max-height:var(--max-height)}.elmNeatLayout--boundary-hasMinWidth{min-width:var(--min-width)}.elmNeatLayout--boundary-hasMaxWidth:not(.elmNeatLayout--boundary-horizontalOverflow){max-width:var(--max-width)}.elmNeatLayout--boundary.elmNeatLayout--rowChild{flex-shrink:1}.elmNeatLayout--boundary.elmNeatLayout--rowChild:not(.elmNeatLayout--boundary-horizontalOverflow){width:auto}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--heightFlex:not(.elmNeatLayout--boundary-verticalOverflow){height:100%}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--heightFlex:not(.elmNeatLayout--boundary-verticalOverflow).elmNeatLayout--rowChild-alignStretch,.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--heightMinSize{height:auto}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--heightFlex{height:100%}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--heightFlex.elmNeatLayout--rowChild-alignStretch{height:auto}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--heightFlex.elmNeatLayout--boundary-hasMaxHeight{max-height:var(--max-height)}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--heightMinSize{height:auto;max-height:100%}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--heightMinSize.elmNeatLayout--boundary-hasMaxHeight{max-height:min(var(--max-height),100%)}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-horizontalOverflow{width:0;flex-shrink:1}.elmNeatLayout--boundary.elmNeatLayout--rowChild.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--boundary-hasMaxWidth{max-width:var(--max-width)}.elmNeatLayout--boundary.elmNeatLayout--columnChild{flex-shrink:0}.elmNeatLayout--boundary.elmNeatLayout--columnChild:not(.elmNeatLayout--boundary-verticalOverflow){height:auto}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--widthFlex:not(.elmNeatLayout--boundary-horizontalOverflow){width:100%}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--widthFlex:not(.elmNeatLayout--boundary-horizontalOverflow).elmNeatLayout--columnChild-alignStretch,.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--widthMinSize{width:auto}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--widthFlex{width:100%}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--widthFlex.elmNeatLayout--columnChild-alignStretch{width:auto}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--widthFlex.elmNeatLayout--boundary-hasMaxWidth{max-width:var(--max-width)}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--widthMinSize{width:auto;max-width:100%}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--widthMinSize.elmNeatLayout--boundary-hasMaxWidth{max-width:min(var(--max-width),100%)}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-verticalOverflow{height:0;flex-shrink:1}.elmNeatLayout--boundary.elmNeatLayout--columnChild.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--boundary-hasMaxHeight{max-height:var(--max-height)}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--heightFlex{height:100%}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--heightMinSize{height:auto}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--widthFlex{width:100%}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--widthMinSize{width:auto}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--boundary-verticalOverflow{height:auto;max-height:100%}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--boundary-verticalOverflow.elmNeatLayout--boundary-hasMaxHeight{max-height:min(var(--max-height),100%)}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--boundary-horizontalOverflow{width:auto;max-width:100%}.elmNeatLayout--boundary.elmNeatLayout--boundaryContent.elmNeatLayout--boundary-horizontalOverflow.elmNeatLayout--boundary-hasMaxWidth{max-width:min(var(--max-width),100%)}.elmNeatLayout--row{gap:var(--inner-gap-y)var(--inner-gap-x);flex-flow:row;display:flex}.elmNeatLayout--row.elmNeatLayout--row-wrap{flex-wrap:wrap}.elmNeatLayout--row.elmNeatLayout--row-justifyStart{justify-content:flex-start}.elmNeatLayout--row.elmNeatLayout--row-justifyCenter{justify-content:center}.elmNeatLayout--row.elmNeatLayout--row-justifyEnd{justify-content:flex-end}.elmNeatLayout--row>.elmNeatLayout--rowChild{flex-grow:0}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-grow{flex-grow:1}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignStart{align-self:flex-start}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignCenter{align-self:center}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignEnd{align-self:flex-end}.elmNeatLayout--row>.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignStretch{align-self:stretch}.elmNeatLayout--row.elmNeatLayout--rowChild{width:auto;height:auto;flex-shrink:1}.elmNeatLayout--row.elmNeatLayout--columnChild{height:auto;width:100%;flex-shrink:0}.elmNeatLayout--row.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignStretch{width:auto}.elmNeatLayout--row.elmNeatLayout--boundaryContent{width:100%;height:100%}.elmNeatLayout--column{gap:var(--inner-gap-y)var(--inner-gap-x);flex-flow:column;display:flex}.elmNeatLayout--column.elmNeatLayout--column-justifyStart{justify-content:flex-start}.elmNeatLayout--column.elmNeatLayout--column-justifyCenter{justify-content:center}.elmNeatLayout--column.elmNeatLayout--column-justifyEnd{justify-content:flex-end}.elmNeatLayout--column>.elmNeatLayout--columnChild{flex-grow:0}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-grow{flex-grow:1}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignStart{align-self:flex-start}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignCenter{align-self:center}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignEnd{align-self:flex-end}.elmNeatLayout--column>.elmNeatLayout--columnChild.elmNeatLayout--columnChild-alignStretch{align-self:stretch}.elmNeatLayout--column.elmNeatLayout--rowChild{width:auto;height:100%;flex-shrink:1}.elmNeatLayout--column.elmNeatLayout--rowChild.elmNeatLayout--rowChild-alignStretch{height:auto}.elmNeatLayout--column.elmNeatLayout--columnChild{height:auto;width:auto;flex-shrink:0}.elmNeatLayout--column.elmNeatLayout--boundaryContent{width:100%;height:100%}"""
