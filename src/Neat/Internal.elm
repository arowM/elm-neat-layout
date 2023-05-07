module Neat.Internal exposing
    ( Alignment(..)
    , BaseSize(..)
    , Boundary(..)
    , Boundary_
    , Children(..)
    , Column(..)
    , ColumnConfig
    , ColumnItem(..)
    , Column_
    , Content(..)
    , Gap
    , IsGap(..)
    , Item_
    , Layer
    , Layered(..)
    , MaxHeight(..)
    , MaxWidth(..)
    , MinHeight(..)
    , MinWidth(..)
    , Overlay
    , Renderer(..)
    , Renderer_
    , Row(..)
    , RowConfig
    , RowItem(..)
    , Row_
    , Size(..)
    , Text
    , View(..)
    , View_(..)
    )

import Mixin exposing (Mixin)


{-| A gap-sensible `Html msg` alternative.
-}
type View gap msg
    = View (View_ msg)


{-| Internal view.
Some variants have gaps for caching purpose.
-}
type View_ msg
    = FromBoundary
        -- Gap that will be set around Boundary
        Gap
        (Boundary_ msg)
    | FromRow (Row_ msg)
    | FromColumn (Column_ msg)
    | None


{-| A bounded View without gap.

Convert to/from View by `setGap`/`setBoundary`.

-}
type Boundary msg
    = Boundary (Boundary_ msg)


type alias Boundary_ msg =
    { mixin : Mixin msg
    , nodeName : String
    , padding : Gap
    , overlays : List (Overlay msg)
    , width : Size
    , minWidth : MinWidth
    , maxWidth : MaxWidth
    , horizontalOverflow : Bool
    , height : Size
    , minHeight : MinHeight
    , maxHeight : MaxHeight
    , verticalOverflow : Bool
    , content : Content msg
    , enforcePointerEvent : Bool
    }


type Content msg
    = TextsContent (List (Text msg))
    | ViewContent (View_ msg)
    | HtmlContent (List ( String, Boundary_ msg ))
    | StringContent String
    | NoContent


type Size
    = MinSize
    | FlexSize


type alias Row_ msg =
    { mixin : Mixin msg
    , nominalGap : Gap
    , contentGap : Gap
    , nodeName : String
    , justifyContent : Alignment
    , children : Children msg
    , wrap : Bool
    }


type Alignment
    = AlignStart
    | AlignCenter
    | AlignEnd
    | AlignStretch


type alias Column_ msg =
    { mixin : Mixin msg
    , nominalGap : Gap
    , contentGap : Gap
    , nodeName : String
    , justifyContent : Alignment
    , children : Children msg
    }


type alias Overlay msg =
    { name : String
    , area : Layer
    , boundary : Boundary msg
    }


type Children msg
    = Children (Item_ msg) (List (Item_ msg))


type RowItem gap msg
    = RowItem (Item_ msg)


type ColumnItem gap msg
    = ColumnItem (Item_ msg)


type alias Item_ msg =
    { alignSelf : Alignment
    , grow : Bool
    , key : String
    , content : View_ msg
    }


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
    { justify : Alignment
    , wrap : Bool
    }


type Column
    = Column ColumnConfig


type alias ColumnConfig =
    { justify : Alignment
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


type alias Text msg =
    { mixin : Mixin msg
    , nodeName : String
    , text : String
    }
