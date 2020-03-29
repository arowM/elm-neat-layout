module Neat.Flex exposing
    ( Flex
    , Horizontal(..)
    , Vertical(..)
    , Wrap(..)
    , childLayout
    , flex
    , flexWrap
    , setChildBasis
    , rowMixins
    )

{-| Internal module exposing helper functions for flexbox.
-}

import Html.Attributes as Attributes
import Mixin exposing (Mixin)
import Neat.Layout.Internal as Layout exposing (Layout)


rowMixins : Flex -> List (Mixin msg)
rowMixins align =
    [ flex
    , flexDirection
    , horizontal align.horizontal
    , vertical align.vertical
    , flexWrap align.wrap
    ]


childLayout : Flex -> Layout msg
childLayout align =
    Layout.batch
        [ childLayoutV align.vertical
        , childLayoutH align.horizontal
        ]


childLayoutH : Horizontal -> Layout msg
childLayoutH h =
    case h of
        HStretch ->
            Layout.fromRecord
                { inner = style "width" "100%"
                , outer = style "width" "100%"
                }

        _ ->
            Layout.none


childLayoutV : Vertical -> Layout msg
childLayoutV v =
    case v of
        VStretch ->
            Layout.fromRecord <|
                { inner = style "height" "100%"
                , outer = Mixin.none
                }

        _ ->
            Layout.none


{-| Configuration about flex alignment.
-}
type alias Flex =
    { vertical : Vertical
    , horizontal : Horizontal
    , wrap : Wrap
    }

{-| Configuration about wrapping.
-}
type Wrap
    = NoWrap
    | Wrap
    | WrapInto Int


-- Mixins


flex : Mixin msg
flex =
    Mixin.fromAttribute <|
        Attributes.attribute "data-elm-neat-layout" "flex"


flexDirection : Mixin msg
flexDirection =
    Mixin.batch
        [ style "-ms-flex-direction" "row"
        , style "flex-direction" "row"
        ]


flexWrap : Wrap -> Mixin msg
flexWrap wrap =
    case wrap of
        NoWrap ->
            Mixin.batch
                [ style "-ms-flex-wrap" "nowrap"
                , style "flex-wrap" "nowrap"
                ]
        Wrap ->
            Mixin.batch
                [ style "-ms-flex-wrap" "wrap"
                , style "flex-wrap" "wrap"
                ]
        WrapInto _ ->
            Mixin.batch
                [ style "-ms-flex-wrap" "wrap"
                , style "flex-wrap" "wrap"
                ]


setChildBasis : Wrap -> Layout msg
setChildBasis wrap =
    case wrap of
        WrapInto n ->
            (basis <| String.fromFloat (100 / toFloat n) ++ "%")

        _ ->
            Layout.none

{-| basis
-}
basis : String -> Layout msg
basis v =
    Layout.fromRecord
        { outer =
            Mixin.batch
                [ style "-ms-flex-preferred-size" v
                , style "flex-basis" v
                ]
        , inner = Mixin.none
        }
        |> Layout.makeImportant



-- Horizontal alignment


{-| -}
type Horizontal
    = Left
    | Right
    | HCenter
    | HSpaceBetween
    | HSpaceAround
      -- | HSpaceEvenly {- Not supported on IE and Edge -}
      {- Not supported as flex layout on IE and Edge, but works well on them. -}
    | HStretch


horizontal : Horizontal -> Mixin msg
horizontal hor =
    Mixin.batch <|
        case hor of
            Left ->
                [ style "-ms-flex-pack" "start"
                , style "justify-content" "flex-start"
                ]

            Right ->
                [ style "-ms-flex-pack" "end"
                , style "justify-content" "flex-end"
                ]

            HCenter ->
                [ style "-ms-flex-pack" "center"
                , style "justify-content" "center"
                ]

            HSpaceBetween ->
                [ style "-ms-flex-pack" "justify"
                , style "justify-content" "space-between"
                ]

            HSpaceAround ->
                [ style "-ms-flex-pack" "distribute"
                , style "justify-content" "space-around"
                ]

            HStretch ->
                [ style "-ms-flex-pack" "start"
                , style "justify-content" "flex-start"
                ]



-- Vertical alignment


{-| -}
type Vertical
    = Top
    | Bottom
    | VCenter
    | VSpaceBetween
    | VSpaceAround
      -- | VSpaceEvenly {- Not supported on IE and Edge -}
      {- Not supported as flex layout on IE and Edge, but works well on them. -}
    | VStretch


vertical : Vertical -> Mixin msg
vertical ver =
    Mixin.batch <|
        case ver of
            Top ->
                [ style "-ms-flex-align" "start"
                , style "align-items" "flex-start"
                ]

            Bottom ->
                [ style "-ms-flex-align" "end"
                , style "align-items" "flex-end"
                ]

            VCenter ->
                [ style "-ms-flex-align" "center"
                , style "align-items" "center"
                ]

            VStretch ->
                [ style "-ms-flex-align" "stretch"
                , style "align-items" "stretch"
                ]

            VSpaceBetween ->
                []

            VSpaceAround ->
                []



-- Helper functions


style : String -> String -> Mixin msg
style k v =
    Mixin.fromAttribute <| Attributes.style k v
