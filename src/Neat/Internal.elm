module Neat.Internal exposing
    ( Alignment(..)
    , BaseSize(..)
    , Boundary(..)
    , BoundaryProps
    , Boundary_(..)
    , ChildBoundaries(..)
    , Children(..)
    , Column(..)
    , ColumnBoundary_
    , ColumnConfig
    , ColumnItem(..)
    , Column_
    , Flow
    , Flows_
    , Gap
    , IsGap(..)
    , ItemBoundary_
    , Item_
    , Justify(..)
    , Layer
    , Layered(..)
    , MaxHeight(..)
    , MaxWidth(..)
    , MinHeight(..)
    , MinWidth(..)
    , Overlay(..)
    , Renderer(..)
    , Renderer_
    , Row(..)
    , RowBoundary_
    , RowConfig
    , RowItem(..)
    , Row_
    , Size(..)
    , View(..)
    , View_(..)
    , defaultBoundaryProps
    , mapBoundary
    , mapBoundary_
    , mapFlow
    , mapOverlays
    , mapView_
    )

import Mixin exposing (Mixin)


{-| A gap-sensible `Html msg` alternative.
-}
type View gap msg
    = View (View_ msg)
    | None


{-| Internal view.
Some variants have gaps for caching purpose.
-}
type View_ msg
    = FromBoundary
        -- Gap that will be set around Boundary
        Gap
        (BoundaryProps msg)
        (Boundary_ msg)
    | FromRow (Row_ msg)
    | FromColumn (Column_ msg)
    | FromFlows (Flows_ msg)


type alias Row_ msg =
    { mixin : Mixin msg
    , nominalGap : Gap
    , contentGap : Gap
    , nodeName : String
    , justifyContent : Justify
    , children : Children msg
    , wrap : Bool
    }


type alias Column_ msg =
    { mixin : Mixin msg
    , nominalGap : Gap
    , contentGap : Gap
    , nodeName : String
    , justifyContent : Justify
    , children : Children msg
    }


type alias Flows_ msg =
    { mixin : Mixin msg
    , nominalGap : Gap
    , contentGap : Gap
    , nodeName : Maybe String
    , texts : ( Flow msg, List (Flow msg) )
    }


type Children msg
    = Children (Item_ msg) (List (Item_ msg))


type RowItem gap msg
    = RowItem (Item_ msg)
    | NoneRowItem


type ColumnItem gap msg
    = ColumnItem (Item_ msg)
    | NoneColumnItem


type alias Item_ msg =
    { alignSelf : Alignment
    , grow : Bool
    , key : String
    , content : View_ msg
    }


mapView_ : (a -> b) -> View_ a -> View_ b
mapView_ f view =
    case view of
        FromBoundary g props boundary_ ->
            FromBoundary g
                (mapBoundaryProps f props)
                (mapBoundary_ f boundary_)

        FromRow o ->
            FromRow
                { mixin = Mixin.map f o.mixin
                , nominalGap = o.nominalGap
                , contentGap = o.contentGap
                , nodeName = o.nodeName
                , justifyContent = o.justifyContent
                , children = modifyChild (mapView_ f) o.children
                , wrap = o.wrap
                }

        FromColumn o ->
            FromColumn
                { mixin = Mixin.map f o.mixin
                , nominalGap = o.nominalGap
                , contentGap = o.contentGap
                , nodeName = o.nodeName
                , justifyContent = o.justifyContent
                , children = modifyChild (mapView_ f) o.children
                }

        FromFlows o ->
            FromFlows
                { mixin = Mixin.map f o.mixin
                , nominalGap = o.nominalGap
                , contentGap = o.contentGap
                , nodeName = o.nodeName
                , texts =
                    let
                        ( head, tail ) =
                            o.texts
                    in
                    ( mapFlow f head
                    , List.map (mapFlow f) tail
                    )
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


{-| A bounded View without gap.

Convert to/from View by `setGap`/`setBoundary`.

-}
type Boundary msg
    = Boundary (BoundaryProps msg) (Boundary_ msg)
    | NoneBoundary


type Boundary_ msg
    = FromView (FromView_ msg)
    | RowBoundary (RowBoundary_ msg)
    | ColumnBoundary (ColumnBoundary_ msg)
    | FlowsBoundary (FlowsBoundary_ msg)
      -- e.g., `input`
    | EmptyBoundary


mapBoundary : (a -> b) -> Boundary a -> Boundary b
mapBoundary f boundary =
    case boundary of
        Boundary props boundary_ ->
            Boundary
                (mapBoundaryProps f props)
                (mapBoundary_ f boundary_)

        NoneBoundary ->
            NoneBoundary


mapBoundary_ : (a -> b) -> Boundary_ a -> Boundary_ b
mapBoundary_ f boundary_ =
    case boundary_ of
        FromView props ->
            mapFromView_ f props
                |> FromView

        RowBoundary props ->
            mapRowBoundary_ f props
                |> RowBoundary

        ColumnBoundary props ->
            mapColumnBoundary_ f props
                |> ColumnBoundary

        FlowsBoundary props ->
            mapFlowsBoundary_ f props
                |> FlowsBoundary

        EmptyBoundary ->
            EmptyBoundary


mapFromView_ : (a -> b) -> FromView_ a -> FromView_ b
mapFromView_ f props =
    { content =
        mapView_ f props.content
    }


{-| Property about all types of boundaries.
-}
type alias BoundaryProps msg =
    { mixin : Mixin msg
    , nodeName : String
    , overlays : List (Overlay msg)
    , width : Size
    , minWidth : MinWidth
    , maxWidth : MaxWidth
    , height : Size
    , minHeight : MinHeight
    , maxHeight : MaxHeight
    , enforcePointerEvent : Bool
    }


type Size
    = MinSize
    | FlexSize


defaultBoundaryProps : BoundaryProps msg
defaultBoundaryProps =
    { mixin = Mixin.none
    , nodeName = "div"
    , overlays = []
    , width = MinSize
    , minWidth = MinWidthInUnit "" 0
    , maxWidth = MaxWidthFit
    , height = MinSize
    , minHeight = MinHeightInUnit "" 0
    , maxHeight = MaxHeightFit
    , enforcePointerEvent = False
    }


mapBoundaryProps : (a -> b) -> BoundaryProps a -> BoundaryProps b
mapBoundaryProps f o =
    { mixin = Mixin.map f o.mixin
    , nodeName = o.nodeName
    , overlays = mapOverlays f o.overlays
    , width = o.width
    , minWidth = o.minWidth
    , maxWidth = o.maxWidth
    , height = o.height
    , minHeight = o.minHeight
    , maxHeight = o.maxHeight
    , enforcePointerEvent = o.enforcePointerEvent
    }


type alias FromView_ msg =
    { content : View_ msg
    }


type alias RowBoundary_ msg =
    { children : ChildBoundaries msg
    , justifyContent : Justify
    , wrap : Bool
    , scrollable : Bool
    }


mapRowBoundary_ : (a -> b) -> RowBoundary_ a -> RowBoundary_ b
mapRowBoundary_ f o =
    { children = mapChildBoundaries f o.children
    , justifyContent = o.justifyContent
    , wrap = o.wrap
    , scrollable = o.scrollable
    }


type alias ColumnBoundary_ msg =
    { justifyContent : Justify
    , children : ChildBoundaries msg
    , scrollable : Bool
    }


mapColumnBoundary_ : (a -> b) -> ColumnBoundary_ a -> ColumnBoundary_ b
mapColumnBoundary_ f o =
    { children = mapChildBoundaries f o.children
    , justifyContent = o.justifyContent
    , scrollable = o.scrollable
    }


type alias FlowsBoundary_ msg =
    { contentGap : Gap
    , texts : ( Flow msg, List (Flow msg) )
    }


mapFlowsBoundary_ : (a -> b) -> FlowsBoundary_ a -> FlowsBoundary_ b
mapFlowsBoundary_ f o =
    { contentGap = o.contentGap
    , texts =
        let
            ( t, ts ) =
                o.texts
        in
        ( mapFlow f t, List.map (mapFlow f) ts )
    }


type ChildBoundaries msg
    = ChildBoundaries
        { head : ItemBoundary_ msg
        , tail : List (ItemBoundary_ msg)
        }


mapChildBoundaries : (a -> b) -> ChildBoundaries a -> ChildBoundaries b
mapChildBoundaries f (ChildBoundaries param) =
    ChildBoundaries
        { head = mapItemBoundary_ f param.head
        , tail =
            List.map (mapItemBoundary_ f) param.tail
        }


type alias ItemBoundary_ msg =
    { alignSelf : Alignment
    , grow : Bool
    , key : String
    , props : BoundaryProps msg
    , content : Boundary_ msg
    }


mapItemBoundary_ : (a -> b) -> ItemBoundary_ a -> ItemBoundary_ b
mapItemBoundary_ f boundary_ =
    { alignSelf = boundary_.alignSelf
    , grow = boundary_.grow
    , key = boundary_.key
    , props = mapBoundaryProps f boundary_.props
    , content = mapBoundary_ f boundary_.content
    }


type Justify
    = JustifyStart
    | JustifyCenter
    | JustifyEnd


type Alignment
    = AlignStart
    | AlignCenter
    | AlignEnd
    | AlignStretch


type Overlay msg
    = Overlay
        { name : String
        , area : Layer
        , props : BoundaryProps msg
        , boundary_ : Boundary_ msg
        }


mapOverlays : (a -> b) -> List (Overlay a) -> List (Overlay b)
mapOverlays f =
    List.map
        (\(Overlay o) ->
            Overlay
                { name = o.name
                , area = o.area
                , props = mapBoundaryProps f o.props
                , boundary_ = mapBoundary_ f o.boundary_
                }
        )


type IsGap p
    = IsGap Gap


type alias Gap =
    { horizontal : Float
    , vertical : Float
    }


type Renderer
    = Renderer Renderer_


type alias Renderer_ =
    { baseSize : BaseSize
    , debug : Bool
    }


type BaseSize
    = BaseSize String Float


type Row
    = Row RowConfig


type alias RowConfig =
    { nodeName : String
    , justify : Justify
    , wrap : Bool
    }


type Column
    = Column ColumnConfig


type alias ColumnConfig =
    { nodeName : String
    , justify : Justify
    }


type MinWidth
    = MinWidthInBs Float
    | MinWidthInUnit String Float


type MaxWidth
    = MaxWidthFit
    | MaxWidthInBs Float
    | MaxWidthInUnit String Float


type MinHeight
    = MinHeightInBs Float
    | MinHeightInUnit String Float


type MaxHeight
    = MaxHeightFit
    | MaxHeightInBs Float
    | MaxHeightInUnit String Float


type alias Layer =
    { top : Float
    , bottom : Float
    , left : Float
    , right : Float
    , priority : Maybe Int
    }


type Layered msg
    = Layered msg


type alias Flow msg =
    { mixin : Mixin msg
    , nodeName : Maybe String
    , text : String
    }


mapFlow : (a -> b) -> Flow a -> Flow b
mapFlow f text =
    { mixin = Mixin.map f text.mixin
    , nodeName = text.nodeName
    , text = text.text
    }
