module Neat.View exposing
    ( View
    , map
    , setRole
    , setAria
    , setBoolAria
    , row
    , Row
    , defaultRow
    , enableWrap
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
    , none
    , when
    , unless
    , withMaybe
    , setBoundary
    , setNodeName
    )

{-| Module for building `View`.


# Core

@docs View
@docs map


# WAI-ARIA

@docs setRole
@docs setAria
@docs setBoolAria


# Row

@docs row


## Config

@docs Row
@docs defaultRow
@docs enableWrap
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


# Handle Conditions

@docs none
@docs when
@docs unless
@docs withMaybe


# Convert to `Boundary`

@docs setBoundary


# Lower level functions for HTML

@docs setNodeName

-}

import Mixin exposing (Mixin)
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
        , MaxHeight(..)
        , MaxWidth(..)
        , MinHeight(..)
        , MinWidth(..)
        , Overlay
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


{-| -}
map : (a -> b) -> View gap a -> View gap b
map f =
    liftInternal (map_ f)


liftInternal : (View_ a -> View_ b) -> View g1 a -> View g2 b
liftInternal f (View view) =
    View <| f view


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

            NoContent ->
                NoContent

            StringContent str ->
                StringContent str
    , enforcePointerEvent = o.enforcePointerEvent
    }


defaultBoundary : Boundary_ msg
defaultBoundary =
    { mixin = Mixin.none
    , gap = emptyGap
    , nodeName = "div"
    , innerGap = emptyGap
    , overlays = []
    , width = MinSize
    , minWidth = MinWidthInUnit "" 0
    , maxWidth = MaxWidthFit
    , horizontalOverflow = False
    , height = MinSize
    , minHeight = MinHeightInUnit "" 0
    , maxHeight = MaxHeightFit
    , verticalOverflow = False
    , content = NoContent
    , enforcePointerEvent = False
    }


mapOverlays : (a -> b) -> List (Overlay a) -> List (Overlay b)
mapOverlays f =
    List.map
        (\o ->
            { name = o.name
            , area = o.area
            , boundary = mapBoundary f o.boundary
            }
        )


emptyGap : Gap
emptyGap =
    { vertical = 0
    , horizontal = 0
    }


extractGap : View_ msg -> Gap
extractGap view =
    case view of
        FromBoundary o ->
            o.gap

        FromRow o ->
            o.gap

        FromColumn o ->
            o.gap

        None ->
            emptyGap


defaultRow_ : Children msg -> Row_ msg
defaultRow_ children =
    { mixin = Mixin.none
    , gap = emptyGap
    , nodeName = "div"
    , justifyContent = AlignStart
    , children = children
    , wrap = True
    }


defaultColumn_ : Children msg -> Column_ msg
defaultColumn_ children =
    { mixin = Mixin.none
    , gap = emptyGap
    , nodeName = "div"
    , justifyContent = AlignStart
    , children = children
    }



-- WAI-ARIA


{-| Set "role" value for WAI-ARIA.
-}
setRole : String -> View g msg -> View g msg
setRole str =
    setViewMixin (Mixin.attribute "role" str)


setViewMixin : Mixin msg -> View g msg -> View g msg
setViewMixin new (View view) =
    View <|
        case view of
            FromBoundary boundary ->
                FromBoundary { boundary | mixin = Mixin.batch [ boundary.mixin, new ] }

            FromRow row_ ->
                FromRow { row_ | mixin = Mixin.batch [ row_.mixin, new ] }

            FromColumn column_ ->
                FromColumn { column_ | mixin = Mixin.batch [ column_.mixin, new ] }

            None ->
                None


{-| Set "aria-\*" value for WAI-ARIA.

e.g., `setAria "required" "true"` stands for "aria-required" is "true".

-}
setAria : String -> String -> View g msg -> View g msg
setAria name v =
    setViewMixin (Mixin.attribute ("aria-" ++ name) v)


{-| Set boolean "aria-\*" value for WAI-ARIA.

i.e.,

  - `setBoolAria name True` is equal to `setAria name "true"`
  - `setBoolAria name False` is equal to `setAria name "false"`

-}
setBoolAria : String -> Bool -> View g msg -> View g msg
setBoolAria name g =
    setViewMixin (Mixin.boolAttribute ("aria-" ++ name) g)



-- Row


{-| Align children horizontally.
-}
row : Row -> List (RowItem gap msg) -> View gap msg
row (Row { justify, wrap }) children_ =
    let
        children =
            children_
                |> List.filterMap
                    (\(RowItem item) ->
                        if item.content == None then
                            Nothing

                        else
                            Just item
                    )
    in
    View <|
        case children of
            [] ->
                None

            item :: items ->
                let
                    row_ =
                        defaultRow_ (Children item items)
                in
                FromRow
                    { row_
                        | gap = extractGap item.content
                        , justifyContent = justify
                        , wrap = wrap
                    }


{-| -}
type alias Row =
    Internal.Row


{-| Default setting for rows.

  - horizontal alignment: left
  - wrapping: disabled

-}
defaultRow : Row
defaultRow =
    Row
        { justify = AlignStart
        , wrap = False
        }


{-| -}
enableWrap : Row -> Row
enableWrap (Row config) =
    Row
        { config | wrap = True }


{-| -}
type alias RowItem gap msg =
    Internal.RowItem gap msg


{-| -}
alignCenter : Row -> Row
alignCenter (Row config) =
    Row
        { config | justify = AlignCenter }


{-| -}
alignRight : Row -> Row
alignRight (Row config) =
    Row
        { config | justify = AlignEnd }


{-| Row item with stretched height.
-}
rowItem : String -> View gap msg -> RowItem gap msg
rowItem key (View content) =
    RowItem
        { alignSelf = AlignStretch
        , grow = False
        , key = key
        , content = content
        }


{-| Row item with stretched height and width.
-}
grownRowItem : String -> View gap msg -> RowItem gap msg
grownRowItem key (View content) =
    RowItem
        { alignSelf = AlignStretch
        , grow = True
        , key = key
        , content = content
        }


{-| Top-aligned item.
-}
topItem : String -> View gap msg -> RowItem gap msg
topItem key (View content) =
    RowItem
        { alignSelf = AlignStart
        , grow = False
        , key = key
        , content = content
        }


{-| Top-aligned item which grows its width as much as possible.
-}
grownTopItem : String -> View gap msg -> RowItem gap msg
grownTopItem key (View content) =
    RowItem
        { alignSelf = AlignStart
        , grow = True
        , key = key
        , content = content
        }


{-| Vertically centered item.
-}
middleItem : String -> View gap msg -> RowItem gap msg
middleItem key (View content) =
    RowItem
        { alignSelf = AlignCenter
        , grow = False
        , key = key
        , content = content
        }


{-| Vertically centered item which grows its width as much as possible.
-}
grownMiddleItem : String -> View gap msg -> RowItem gap msg
grownMiddleItem key (View content) =
    RowItem
        { alignSelf = AlignCenter
        , grow = True
        , key = key
        , content = content
        }


{-| Bottom-aligned item.
-}
bottomItem : String -> View gap msg -> RowItem gap msg
bottomItem key (View content) =
    RowItem
        { alignSelf = AlignEnd
        , grow = False
        , key = key
        , content = content
        }


{-| Bottom-aligned item which grows its width as much as possible.
-}
grownBottomItem : String -> View gap msg -> RowItem gap msg
grownBottomItem key (View content) =
    RowItem
        { alignSelf = AlignEnd
        , grow = True
        , key = key
        , content = content
        }



-- Column


{-| Align children vertically.
-}
column : Column -> List (ColumnItem gap msg) -> View gap msg
column (Column { justify }) children_ =
    let
        children =
            children_
                |> List.filterMap
                    (\(ColumnItem item) ->
                        if item.content == None then
                            Nothing

                        else
                            Just item
                    )
    in
    View <|
        case children of
            [] ->
                None

            item :: items ->
                let
                    column_ =
                        defaultColumn_ <| Children item items
                in
                FromColumn
                    { column_
                        | gap = extractGap item.content
                        , justifyContent = justify
                    }


{-| -}
type alias Column =
    Internal.Column


{-| Default setting for columns.

  - vertical alignment: top

-}
defaultColumn : Column
defaultColumn =
    Column
        { justify = AlignStart
        }


{-| -}
type alias ColumnItem gap msg =
    Internal.ColumnItem gap msg


{-| -}
alignMiddle : Column -> Column
alignMiddle (Column config) =
    Column
        { config | justify = AlignCenter }


{-| -}
alignBottom : Column -> Column
alignBottom (Column config) =
    Column
        { config | justify = AlignEnd }


{-| Column item with stretched width.
-}
columnItem : String -> View gap msg -> ColumnItem gap msg
columnItem key (View content) =
    ColumnItem
        { alignSelf = AlignStretch
        , grow = False
        , key = key
        , content = content
        }


{-| Column item with stretched width and height.
-}
grownColumnItem : String -> View gap msg -> ColumnItem gap msg
grownColumnItem key (View content) =
    ColumnItem
        { alignSelf = AlignStretch
        , grow = True
        , key = key
        , content = content
        }


{-| Left-aligned item.
-}
leftItem : String -> View gap msg -> ColumnItem gap msg
leftItem key (View content) =
    ColumnItem
        { alignSelf = AlignStart
        , grow = False
        , key = key
        , content = content
        }


{-| Left-aligned item which grows its height as much as possible.
-}
grownLeftItem : String -> View gap msg -> ColumnItem gap msg
grownLeftItem key (View content) =
    ColumnItem
        { alignSelf = AlignStart
        , grow = True
        , key = key
        , content = content
        }


{-| Horizontally centered item.
-}
centerItem : String -> View gap msg -> ColumnItem gap msg
centerItem key (View content) =
    ColumnItem
        { alignSelf = AlignCenter
        , grow = False
        , key = key
        , content = content
        }


{-| Horizontally centered item which grows its height as much as possible.
-}
grownCenterItem : String -> View gap msg -> ColumnItem gap msg
grownCenterItem key (View content) =
    ColumnItem
        { alignSelf = AlignCenter
        , grow = True
        , key = key
        , content = content
        }


{-| Right-aligned item.
-}
rightItem : String -> View gap msg -> ColumnItem gap msg
rightItem key (View content) =
    ColumnItem
        { alignSelf = AlignEnd
        , grow = False
        , key = key
        , content = content
        }


{-| Right-aligned item which grows its height as much as possible.
-}
grownRightItem : String -> View gap msg -> ColumnItem gap msg
grownRightItem key (View content) =
    ColumnItem
        { alignSelf = AlignEnd
        , grow = True
        , key = key
        , content = content
        }


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



-- Handle conditions


{-| Generates no HTML nodes.
This is useful for handling elements which only appears under certain conditions.

    when p v =
        if p then
            v

        else
            none

-}
none : View g a
none =
    View None


{-| Insert a view only when a condition is met.
-}
when : Bool -> View gap msg -> View gap msg
when p v =
    if p then
        v

    else
        none


{-| Insert a view unless a condition is met.
-}
unless : Bool -> View gap msg -> View gap msg
unless p =
    when <| not p


{-| Insert a view only if the given value is `Just`.
-}
withMaybe : Maybe a -> (a -> View gap msg) -> View gap msg
withMaybe ma f =
    case ma of
        Just a ->
            f a

        Nothing ->
            none



-- Convert to `Boundary`


{-| Wrap a view with boundary without gap.
This is the only way to reset view gaps.
-}
setBoundary : View gap msg -> Boundary msg
setBoundary (View view) =
    Boundary
        { defaultBoundary
            | innerGap = extractGap view
            , content =
                if view == None then
                    NoContent

                else
                    ViewContent view
        }



-- Low level function for HTML


{-| -}
setNodeName : String -> View gap msg -> View gap msg
setNodeName str (View view) =
    View <|
        case view of
            FromBoundary boundary ->
                FromBoundary { boundary | nodeName = str }

            FromRow row_ ->
                FromRow { row_ | nodeName = str }

            FromColumn column_ ->
                FromColumn { column_ | nodeName = str }

            None ->
                None
