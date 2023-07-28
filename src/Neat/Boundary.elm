module Neat.Boundary exposing
    ( Boundary
    , map
    , empty
    , setMixin
    , setMixins
    , setAttribute
    , setAttributes
    , setClass
    , setId
    , setRole
    , setAria
    , setBoolAria
    , setMinWidthInBs
    , setMinWidthInEm
    , setMinWidthInRem
    , setMinHeightInBs
    , setMinHeightInEm
    , setMinHeightInRem
    , setMaxWidthInBs
    , setMaxWidthInEm
    , setMaxWidthInRem
    , setMaxHeightInBs
    , setMaxHeightInEm
    , setMaxHeightInRem
    , row
    , center
    , Row
    , defaultRow
    , disableWrap
    , enableHScroll
    , RowItem
    , alignCenter
    , alignRight
    , rowItem
    , topItem
    , middleItem
    , bottomItem
    , grownRowItem
    , grownTopItem
    , grownMiddleItem
    , grownBottomItem
    , column
    , Column
    , defaultColumn
    , enableVScroll
    , setVerticalScroller
    , ColumnItem
    , alignMiddle
    , alignBottom
    , columnItem
    , leftItem
    , centerItem
    , rightItem
    , grownColumnItem
    , grownLeftItem
    , grownCenterItem
    , grownRightItem
    , putLayer
    , Layer
    , defaultLayer
    , Layered
    , mapLayered
    , toLayered
    , none
    , when
    , unless
    , withMaybe
    , setGap
    , setNodeName
    )

{-| Module for building `Boundary`.


# Core

@docs Boundary
@docs map
@docs empty


# Attributes

Functions to set arbitrary attributes.
The following styles are not reflected by the `style` attribute or CSS files.

  - padding / margin
      - Use functions for gaps.
  - min-width / max-width / min-height / max-height / line-height
      - Use functions for sizes.
  - width / height
      - Use min-\* and max-\*.
  - white-space
      - Determined by wrapping style
  - display / position
      - Determined automatically by its context
  - z-index
      - Enforced to be "auto"
  - box-sizing
      - Determined by each type of view.
          - `scalableBlock` is "content-box"
          - otherwise, "border-box"
  - overflow
      - Boundaries automatically handles overflow.
  - position
      - Use putLayer
  - line-height
      - Specify with `Neat.View.textBlock` of `Neat.View.fromFlows`

@docs setMixin
@docs setMixins
@docs setAttribute
@docs setAttributes
@docs setClass
@docs setId


# WAI-ARIA

@docs setRole
@docs setAria
@docs setBoolAria


# Sizes

You can only use a limited kind of units.
This may seem inconvenient, but it prevents you to build unmaintainable broken views.


## Minimum width

The default _minimum width_ is zero without enabling child overflow.

@docs setMinWidthInBs
@docs setMinWidthInEm
@docs setMinWidthInRem


## Minimum height

The default _minimum height_ is zero without enabling child overflow.

@docs setMinHeightInBs
@docs setMinHeightInEm
@docs setMinHeightInRem


## Maximum width

The default value for maximum width is _fit_, which shrinks as much as its children do not overhang it.

@docs setMaxWidthInBs
@docs setMaxWidthInEm
@docs setMaxWidthInRem


## Maximum height

The default value for maximum height is _fit_, which shrinks as much as its children do not overhang it.

@docs setMaxHeightInBs
@docs setMaxHeightInEm
@docs setMaxHeightInRem


# Row

@docs row
@docs center


## Config

@docs Row
@docs defaultRow
@docs disableWrap
@docs enableHScroll
@docs RowItem
@docs alignCenter
@docs alignRight


## Item

Each function has the `String` argument, which helps make the DOM modifications more efficient. It must be unique among items in the same row.

@docs rowItem
@docs topItem
@docs middleItem
@docs bottomItem
@docs grownRowItem
@docs grownTopItem
@docs grownMiddleItem
@docs grownBottomItem


# Column

@docs column


## Config

@docs Column
@docs defaultColumn
@docs enableVScroll
@docs setVerticalScroller
@docs ColumnItem
@docs alignMiddle
@docs alignBottom


## Item

Each function has the `String` argument, which helps make the DOM modifications more efficient. It must be unique among items in the same row.

@docs columnItem
@docs leftItem
@docs centerItem
@docs rightItem
@docs grownColumnItem
@docs grownLeftItem
@docs grownCenterItem
@docs grownRightItem


# Overlay

@docs putLayer
@docs Layer
@docs defaultLayer
@docs Layered
@docs mapLayered
@docs toLayered


# Handle Conditions

@docs none
@docs when
@docs unless
@docs withMaybe


# Convert to `View`

@docs setGap


# Lower level functions for HTML

@docs setNodeName

-}

import Html exposing (Attribute)
import Mixin exposing (Mixin)
import Neat.Internal as Internal
    exposing
        ( Alignment(..)
        , Boundary(..)
        , BoundaryProps
        , Boundary_(..)
        , ChildBoundaries(..)
        , ColumnBoundary_
        , IsGap(..)
        , ItemBoundary_
        , Justify(..)
        , Layered(..)
        , MaxHeight(..)
        , MaxWidth(..)
        , MinHeight(..)
        , MinWidth(..)
        , Overlay(..)
        , RowBoundary_
        , Size(..)
        , View(..)
        , View_(..)
        , defaultBoundaryProps
        , mapBoundary
        )



-- Core


{-| A bounded View without gap.

Convert to/from View by `setGap`/`setBoundary`.

-}
type alias Boundary msg =
    Internal.Boundary msg


{-| -}
map : (a -> b) -> Boundary a -> Boundary b
map =
    Internal.mapBoundary



-- Primitive Constructors


{-| An empty block.

In HTML terms, it generates an HTML element without content.

    -- Exposed by `elm-community/html-extra`


    import Html.Attributes exposing (type_, value)
    import Html.Events.Extra exposing (onChange)
    import Neat.Boundary as Boundary

    input : Model -> Boundary msg
    input model =
        Boundary.empty
            |> Boundary.setNodeName "input"
            |> Boundary.setAttributes
                [ type_ "text"
                , value model.input
                , onChange ChangeInputValue
                ]

-}
empty : Boundary msg
empty =
    Boundary
        defaultBoundaryProps
        EmptyBoundary



-- Flow contents
-- Attributes


{-| Append `Mixin` on boundaries.
-}
setMixin : Mixin msg -> Boundary msg -> Boundary msg
setMixin new =
    modifyProps <|
        \props ->
            { props
                | mixin =
                    Mixin.batch
                        [ props.mixin
                        , new
                        ]
            }


{-| Same as `setMixin` but takes a list of `Mixin`s.
-}
setMixins : List (Mixin msg) -> Boundary msg -> Boundary msg
setMixins ls =
    setMixin <| Mixin.batch ls


{-| Append `Attribute` on boundaries.
-}
setAttribute : Attribute msg -> Boundary msg -> Boundary msg
setAttribute attr =
    setMixin <| Mixin.fromAttributes [ attr ]


{-| Same as `setAttribute` but takes a list of `Attribute`s.
-}
setAttributes : List (Attribute msg) -> Boundary msg -> Boundary msg
setAttributes attrs =
    setMixin <| Mixin.fromAttributes attrs


{-| Append `class` attribute.
-}
setClass : String -> Boundary msg -> Boundary msg
setClass =
    setMixin << Mixin.class


{-| Append `id` attribute.
-}
setId : String -> Boundary msg -> Boundary msg
setId =
    setMixin << Mixin.id


{-| Set "role" value for WAI-ARIA.
-}
setRole : String -> Boundary msg -> Boundary msg
setRole str =
    setMixin <| Mixin.attribute "role" str


{-| Set "aria-\*" value for WAI-ARIA.

e.g., `setAria "required" "true"` stands for "aria-required" is "true".

-}
setAria : String -> String -> Boundary msg -> Boundary msg
setAria name v =
    setMixin <| Mixin.attribute ("aria-" ++ name) v


{-| Set boolean "aria-\*" value for WAI-ARIA.

i.e.,

  - `setBoolAria name True` is equal to `setAria name "true"`
  - `setBoolAria name False` is equal to `setAria name "false"`

-}
setBoolAria : String -> Bool -> Boundary msg -> Boundary msg
setBoolAria name g =
    setMixin <| Mixin.boolAttribute ("aria-" ++ name) g



-- Sizing


{-| Set the minimum width as a percentage of the _base size_.
e.g., `setMinWidthInBs 100` set the _minimum width_ the same length as the _base size_.
-}
setMinWidthInBs : Float -> Boundary msg -> Boundary msg
setMinWidthInBs =
    setMinWidth << MinWidthInBs


{-| Set the minimum width in [em](https://developer.mozilla.org/en-US/docs/Web/CSS/length#em).

> Represents the calculated font-size of the element.

-}
setMinWidthInEm : Float -> Boundary msg -> Boundary msg
setMinWidthInEm =
    setMinWidth << MinWidthInUnit "em"


{-| Set the minimum width in [rem](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem).

> Represents the font-size of the root element (typically <html>).

-}
setMinWidthInRem : Float -> Boundary msg -> Boundary msg
setMinWidthInRem =
    setMinWidth << MinWidthInUnit "rem"


modifyProps : (BoundaryProps msg -> BoundaryProps msg) -> Boundary msg -> Boundary msg
modifyProps f boundary =
    case boundary of
        NoneBoundary ->
            NoneBoundary

        Boundary props content ->
            Boundary (f props) content


setMinWidth : MinWidth -> Boundary msg -> Boundary msg
setMinWidth length =
    modifyProps <|
        \props ->
            { props | minWidth = length }


{-| Set the minimum height as a percentage of the _base size_.
e.g., `setMinHeightInBs 100` set the _minimum height_ the same length as the _base size_.
-}
setMinHeightInBs : Float -> Boundary msg -> Boundary msg
setMinHeightInBs =
    setMinHeight << MinHeightInBs


{-| Set the minimum height in [em](https://developer.mozilla.org/en-US/docs/Web/CSS/length#em).

> Represents the calculated font-size of the element.

-}
setMinHeightInEm : Float -> Boundary msg -> Boundary msg
setMinHeightInEm =
    setMinHeight << MinHeightInUnit "em"


{-| Set the minimum height in [rem](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem).

> Represents the font-size of the root element (typically <html>).

-}
setMinHeightInRem : Float -> Boundary msg -> Boundary msg
setMinHeightInRem =
    setMinHeight << MinHeightInUnit "rem"


setMinHeight : MinHeight -> Boundary msg -> Boundary msg
setMinHeight length =
    modifyProps <|
        \props ->
            { props | minHeight = length }


{-| Set the maximum width as a percentage of the _base size_.
e.g., `setMaxWidthInBs 100` set the _maximum width_ the same length as the _base size_.
-}
setMaxWidthInBs : Float -> Boundary msg -> Boundary msg
setMaxWidthInBs =
    setMaxWidth << MaxWidthInBs


{-| Set the maximum width in [em](https://developer.mozilla.org/en-US/docs/Web/CSS/length#em).

> Represents the calculated font-size of the element.

-}
setMaxWidthInEm : Float -> Boundary msg -> Boundary msg
setMaxWidthInEm =
    setMaxWidth << MaxWidthInUnit "em"


{-| Set the maximum width in [rem](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem).

> Represents the font-size of the root element (typically <html>).

-}
setMaxWidthInRem : Float -> Boundary msg -> Boundary msg
setMaxWidthInRem =
    setMaxWidth << MaxWidthInUnit "rem"


setMaxWidth : MaxWidth -> Boundary msg -> Boundary msg
setMaxWidth length =
    modifyProps <|
        \props ->
            { props
                | maxWidth = length
                , width = FlexSize
            }


{-| Set the maximum height as a percentage of the _base size_.
e.g., `setMaxHeightInBs 100` set the _maximum height_ the same length as the _base size_.
-}
setMaxHeightInBs : Float -> Boundary msg -> Boundary msg
setMaxHeightInBs =
    setMaxHeight << MaxHeightInBs


{-| Set the maximum height in [em](https://developer.mozilla.org/en-US/docs/Web/CSS/length#em).

> Represents the calculated font-size of the element.

-}
setMaxHeightInEm : Float -> Boundary msg -> Boundary msg
setMaxHeightInEm =
    setMaxHeight << MaxHeightInUnit "em"


{-| Set the maximum height in [rem](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem).

> Represents the font-size of the root element (typically <html>).

-}
setMaxHeightInRem : Float -> Boundary msg -> Boundary msg
setMaxHeightInRem =
    setMaxHeight << MaxHeightInUnit "rem"


setMaxHeight : MaxHeight -> Boundary msg -> Boundary msg
setMaxHeight length =
    modifyProps <|
        \props ->
            { props
                | maxHeight = length
                , height = FlexSize
            }



-- Row


{-| Align children horizontally.
-}
row : Row -> List (RowItem msg) -> Boundary msg
row (Row { justify, wrap, scrollable }) children_ =
    let
        children =
            children_
                |> List.filterMap
                    (\item ->
                        case item of
                            NoneRowItem ->
                                Nothing

                            RowItem item_ ->
                                Just item_
                    )
    in
    case children of
        [] ->
            NoneBoundary

        item :: items ->
            let
                row_ =
                    defaultRowBoundary_ <|
                        ChildBoundaries
                            { head = item
                            , tail = items
                            }
            in
            Boundary defaultBoundaryProps <|
                RowBoundary
                    { row_
                        | justifyContent = justify
                        , wrap = wrap
                        , scrollable = scrollable
                    }


defaultRowBoundary_ : ChildBoundaries msg -> RowBoundary_ msg
defaultRowBoundary_ children =
    { children = children
    , justifyContent = JustifyStart
    , wrap = True
    , scrollable = False
    }


{-| An alias for `row` with a centered element.
-}
center : Boundary msg -> Boundary msg
center v =
    row
        (defaultRow
            |> alignCenter
        )
        [ v
            |> middleItem "content"
        ]


{-| -}
type Row
    = Row RowConfig


type alias RowConfig =
    { justify : Justify
    , wrap : Bool
    , scrollable : Bool
    }


{-| Default setting for rows.

  - horizontal alignment: left
  - wrapping: enabled
  - horizontal scroll: disabled

-}
defaultRow : Row
defaultRow =
    Row
        { justify = JustifyStart
        , wrap = True
        , scrollable = False
        }


{-| -}
disableWrap : Row -> Row
disableWrap (Row config) =
    Row
        { config | wrap = False }


{-| -}
enableHScroll : Row -> Row
enableHScroll (Row config) =
    Row
        { config | scrollable = True }


{-| -}
type RowItem msg
    = RowItem (ItemBoundary_ msg)
    | NoneRowItem


{-| -}
alignCenter : Row -> Row
alignCenter (Row config) =
    Row
        { config | justify = JustifyCenter }


{-| -}
alignRight : Row -> Row
alignRight (Row config) =
    Row
        { config | justify = JustifyEnd }


rowItem_ : (BoundaryProps msg -> Boundary_ msg -> ItemBoundary_ msg) -> Boundary msg -> RowItem msg
rowItem_ f boundary =
    case boundary of
        NoneBoundary ->
            NoneRowItem

        Boundary props boundary_ ->
            RowItem <| f props boundary_


{-| Row item with stretched height.
-}
rowItem : String -> Boundary msg -> RowItem msg
rowItem key =
    rowItem_ <|
        \props boundary_ ->
            { alignSelf = AlignStretch
            , grow = False
            , key = key
            , props = props
            , content = boundary_
            }


{-| Row item with stretched height and width.
-}
grownRowItem : String -> Boundary msg -> RowItem msg
grownRowItem key =
    rowItem_ <|
        \props boundary_ ->
            { alignSelf = AlignStretch
            , grow = True
            , key = key
            , props = props
            , content = boundary_
            }


{-| Top-aligned item.
-}
topItem : String -> Boundary msg -> RowItem msg
topItem key =
    rowItem_ <|
        \props boundary_ ->
            { alignSelf = AlignStart
            , grow = False
            , key = key
            , props = props
            , content = boundary_
            }


{-| Top-aligned item which grows its width as much as possible.
-}
grownTopItem : String -> Boundary msg -> RowItem msg
grownTopItem key =
    rowItem_ <|
        \props boundary_ ->
            { alignSelf = AlignStart
            , grow = True
            , key = key
            , props = props
            , content = boundary_
            }


{-| Vertically centered item.
-}
middleItem : String -> Boundary msg -> RowItem msg
middleItem key =
    rowItem_ <|
        \props boundary_ ->
            { alignSelf = AlignCenter
            , grow = False
            , key = key
            , props = props
            , content = boundary_
            }


{-| Vertically centered item which grows its width as much as possible.
-}
grownMiddleItem : String -> Boundary msg -> RowItem msg
grownMiddleItem key =
    rowItem_ <|
        \props boundary_ ->
            { alignSelf = AlignCenter
            , grow = True
            , key = key
            , props = props
            , content = boundary_
            }


{-| Bottom-aligned item.
-}
bottomItem : String -> Boundary msg -> RowItem msg
bottomItem key =
    rowItem_ <|
        \props boundary_ ->
            { alignSelf = AlignEnd
            , grow = False
            , key = key
            , props = props
            , content = boundary_
            }


{-| Bottom-aligned item which grows its width as much as possible.
-}
grownBottomItem : String -> Boundary msg -> RowItem msg
grownBottomItem key =
    rowItem_ <|
        \props boundary_ ->
            { alignSelf = AlignEnd
            , grow = True
            , key = key
            , props = props
            , content = boundary_
            }



-- Column


{-| Align children horizontally.
-}
column : Column -> List (ColumnItem msg) -> Boundary msg
column (Column { justify, scrollable }) children_ =
    let
        children =
            children_
                |> List.filterMap
                    (\item ->
                        case item of
                            NoneColumnItem ->
                                Nothing

                            ColumnItem item_ ->
                                Just item_
                    )
    in
    case children of
        [] ->
            NoneBoundary

        item :: items ->
            let
                column_ =
                    defaultColumnBoundary_ <|
                        ChildBoundaries
                            { head = item
                            , tail = items
                            }
            in
            Boundary defaultBoundaryProps <|
                ColumnBoundary
                    { column_
                        | justifyContent = justify
                        , scrollable = scrollable
                    }


defaultColumnBoundary_ : ChildBoundaries msg -> ColumnBoundary_ msg
defaultColumnBoundary_ children =
    { children = children
    , justifyContent = JustifyStart
    , scrollable = False
    }


{-| -}
type Column
    = Column ColumnConfig


type alias ColumnConfig =
    { justify : Justify
    , scrollable : Bool
    }


{-| Default setting for columns.

  - vertical alignment: top
  - vertical scroll: disabled

-}
defaultColumn : Column
defaultColumn =
    Column
        { justify = JustifyStart
        , scrollable = False
        }


{-| Enable vertical scroll.
-}
enableVScroll : Column -> Column
enableVScroll (Column config) =
    Column
        { config | scrollable = True }


{-| Just for convenience.

    setVerticalScroller boundary =
        column
            (defaultColumn
                |> enableVScroll
            )
            [ grownColumnItem "content" boundary
            ]

-}
setVerticalScroller : Boundary msg -> Boundary msg
setVerticalScroller boundary =
    column
        (defaultColumn
            |> enableVScroll
        )
        [ grownColumnItem "content" boundary
        ]


{-| -}
type ColumnItem msg
    = ColumnItem (ItemBoundary_ msg)
    | NoneColumnItem


{-| -}
alignMiddle : Column -> Column
alignMiddle (Column config) =
    Column
        { config | justify = JustifyCenter }


{-| -}
alignBottom : Column -> Column
alignBottom (Column config) =
    Column
        { config | justify = JustifyEnd }


columnItem_ : (BoundaryProps msg -> Boundary_ msg -> ItemBoundary_ msg) -> Boundary msg -> ColumnItem msg
columnItem_ f boundary =
    case boundary of
        NoneBoundary ->
            NoneColumnItem

        Boundary props boundary_ ->
            ColumnItem <| f props boundary_


{-| Column item with stretched height.
-}
columnItem : String -> Boundary msg -> ColumnItem msg
columnItem key =
    columnItem_ <|
        \props boundary_ ->
            { alignSelf = AlignStretch
            , grow = False
            , key = key
            , props = props
            , content = boundary_
            }


{-| Column item with stretched height and width.
-}
grownColumnItem : String -> Boundary msg -> ColumnItem msg
grownColumnItem key =
    columnItem_ <|
        \props boundary_ ->
            { alignSelf = AlignStretch
            , grow = True
            , key = key
            , props = props
            , content = boundary_
            }


{-| Left-aligned item.
-}
leftItem : String -> Boundary msg -> ColumnItem msg
leftItem key =
    columnItem_ <|
        \props boundary_ ->
            { alignSelf = AlignStart
            , grow = False
            , key = key
            , props = props
            , content = boundary_
            }


{-| Left-aligned item which grows its width as much as possible.
-}
grownLeftItem : String -> Boundary msg -> ColumnItem msg
grownLeftItem key =
    columnItem_ <|
        \props boundary_ ->
            { alignSelf = AlignStart
            , grow = True
            , key = key
            , props = props
            , content = boundary_
            }


{-| Horizontally centered item.
-}
centerItem : String -> Boundary msg -> ColumnItem msg
centerItem key =
    columnItem_ <|
        \props boundary_ ->
            { alignSelf = AlignCenter
            , grow = False
            , key = key
            , props = props
            , content = boundary_
            }


{-| Horizontally centered item which grows its width as much as possible.
-}
grownCenterItem : String -> Boundary msg -> ColumnItem msg
grownCenterItem key =
    columnItem_ <|
        \props boundary_ ->
            { alignSelf = AlignCenter
            , grow = True
            , key = key
            , props = props
            , content = boundary_
            }


{-| Right-aligned item.
-}
rightItem : String -> Boundary msg -> ColumnItem msg
rightItem key =
    columnItem_ <|
        \props boundary_ ->
            { alignSelf = AlignEnd
            , grow = False
            , key = key
            , props = props
            , content = boundary_
            }


{-| Right-aligned item which grows its width as much as possible.
-}
grownRightItem : String -> Boundary msg -> ColumnItem msg
grownRightItem key =
    columnItem_ <|
        \props boundary_ ->
            { alignSelf = AlignEnd
            , grow = True
            , key = key
            , props = props
            , content = boundary_
            }



-- Overlay


{-| Set the position of each edge of the overlay layer as a percentage of the base view.

The `priority` field specifies how much the element is superimposed on the front side in preference to other elements. If given `Nothing`, it is set equivalent priority comparing to other elements.

-}
type alias Layer =
    { top : Float
    , bottom : Float
    , left : Float
    , right : Float
    , priority : Maybe Int
    }


{-|

    defaultLayer
    --> { top = 0
    --> , bottom = 0
    --> , left = 0
    --> , right = 0
    --> , priority = Nothing
    --> }

-}
defaultLayer : Layer
defaultLayer =
    { top = 0
    , bottom = 0
    , left = 0
    , right = 0
    , priority = Nothing
    }


{-| Put overlay layer on the parent view.
-}
putLayer : String -> ( Layer, Boundary (Layered msg) ) -> Boundary msg -> Boundary msg
putLayer name ( area, layered ) =
    modifyProps <|
        \props ->
            { props
                | overlays =
                    case mapBoundary (\(Layered a) -> a) layered of
                        NoneBoundary ->
                            props.overlays

                        Boundary props_ boundary_ ->
                            Overlay
                                { name = name
                                , area = area
                                , props = props_
                                , boundary_ = boundary_
                                }
                                :: props.overlays
            }


{-| -}
type alias Layered msg =
    Internal.Layered msg


{-| -}
mapLayered : (a -> b) -> Boundary (Layered a) -> Boundary (Layered b)
mapLayered f =
    mapBoundary (\(Layered a) -> Layered <| f a)


{-| Convert `Boundary` for `putLayer`. The `Boundary (Layered msg)` ignores pointer events; this feature is especially helpfull for realizing popups with clickable background.
-}
toLayered : Boundary msg -> Boundary (Layered msg)
toLayered boundary =
    modifyProps
        (\props ->
            { props
                | enforcePointerEvent = True
            }
        )
        boundary
        |> mapBoundary Layered



-- Handle conditions


{-| Generates no HTML nodes.
This is useful for handling elements which only appears under certain conditions.

    when p v =
        if p then
            v

        else
            none

-}
none : Boundary a
none =
    NoneBoundary


{-| Insert a view only when a condition is met.
-}
when : Bool -> Boundary msg -> Boundary msg
when p v =
    if p then
        v

    else
        none


{-| Insert a view unless a condition is met.
-}
unless : Bool -> Boundary msg -> Boundary msg
unless p =
    when <| not p


{-| Insert a view only if the given value is `Just`.
-}
withMaybe : Maybe a -> (a -> Boundary msg) -> Boundary msg
withMaybe ma f =
    case ma of
        Just a ->
            f a

        Nothing ->
            none



-- Convert to `View`


{-| Set Gap around a view.
-}
setGap : IsGap gap -> Boundary msg -> View gap msg
setGap (IsGap gap) boundary =
    case boundary of
        NoneBoundary ->
            None

        Boundary props boundary_ ->
            View <| FromBoundary gap props boundary_



-- Low level function for HTML


{-| Set HTML node name on `Boundary`.
-}
setNodeName : String -> Boundary msg -> Boundary msg
setNodeName str =
    modifyProps <|
        \props ->
            { props | nodeName = str }
