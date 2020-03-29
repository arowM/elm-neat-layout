module Neat.Layout.Column exposing
    ( columnWith
    , Column
    , defaultColumn
    , column
    , Vertical(..)
    , Horizontal(..)
    , Wrap(..)
    , optimized
    , toProtected
    )

{-|


# Columns

@docs columnWith
@docs Column
@docs Wrap
@docs defaultColumn
@docs column
@docs Vertical
@docs Horizontal


# Optimization

@docs optimized
@docs toProtected

-}

import Html exposing (Html)
import Html.Attributes as Attributes
import Mixin exposing (Mixin)
import Neat exposing (Protected, Renderer, View)
import Neat.Flex as Flex exposing (Flex, flex, flexWrap)



-- Core


{-| -}
columnWith : Column -> List (View p msg) -> View p msg
columnWith align children =
    wrapper align (columnMixins align) <|
        List.map (expandChild align) children


wrapper : Column -> List (Mixin msg) -> List (View p msg) -> View p msg
wrapper align =
    case align.nodeName of
        "div" ->
            Neat.lift Html.div

        a ->
            Neat.lift (Html.node a)


columnMixins : Column -> List (Mixin msg)
columnMixins align =
    [ flex
    , flexDirection
    , horizontal align.horizontal
    , vertical align.vertical
    , flexWrap <| toFlexWrap align.wrap
    ]


expandChild : Column -> View p msg -> View p msg
expandChild align =
    Neat.setLayout <| Flex.childLayout <| toFlex align


{-| An alias for `columnWith defaultColumn`.
-}
column : List (View p msg) -> View p msg
column =
    columnWith defaultColumn


{-| Configuration about column alignment.
-}
type alias Column =
    { vertical : Vertical
    , horizontal : Horizontal
    , wrap : Wrap
    , nodeName : String
    }


{-| Configuration about wrapping.
-}
type Wrap
    = NoWrap
    | Wrap
    | WrapInto Int


toFlex : Column -> Flex
toFlex align =
    { vertical = toFlexVertical align.vertical
    , horizontal = toFlexHorizontal align.horizontal
    , wrap = toFlexWrap align.wrap
    }


toFlexWrap : Wrap -> Flex.Wrap
toFlexWrap wrap =
    case wrap of
        NoWrap ->
            Flex.NoWrap
        Wrap ->
            Flex.Wrap
        WrapInto n ->
            Flex.WrapInto n




{-| Default `Column` configuration.

    { vertical = Top
    , horizontal = Stretch
    , wrap = NoWrap
    , nodeName = "div"
    }

-}
defaultColumn : Column
defaultColumn =
    { vertical = Top
    , horizontal = Stretch
    , wrap = NoWrap
    , nodeName = "div"
    }



-- Mixins


flexDirection : Mixin msg
flexDirection =
    Mixin.batch
        [ style "-ms-flex-direction" "column"
        , style "flex-direction" "column"
        ]



-- Horizontal alignment


{-| -}
type Horizontal
    = Left
    | Right
    | HCenter
    | Stretch


toFlexHorizontal : Horizontal -> Flex.Horizontal
toFlexHorizontal h =
    case h of
        Left ->
            Flex.Left

        Right ->
            Flex.Right

        HCenter ->
            Flex.HCenter

        Stretch ->
            Flex.HStretch


horizontal : Horizontal -> Mixin msg
horizontal hor =
    Mixin.batch <|
        case hor of
            Left ->
                [ style "-ms-flex-align" "start"
                , style "align-items" "flex-start"
                ]

            Right ->
                [ style "-ms-flex-align" "end"
                , style "align-items" "flex-end"
                ]

            HCenter ->
                [ style "-ms-flex-align" "center"
                , style "align-items" "center"
                ]

            Stretch ->
                [ style "-ms-flex-align" "stretch"
                , style "align-items" "stretch"
                ]



-- Vertical alignment


{-| -}
type Vertical
    = Top
    | Bottom
    | VCenter
    | SpaceBetween
    | SpaceAround


toFlexVertical : Vertical -> Flex.Vertical
toFlexVertical v =
    case v of
        Top ->
            Flex.Top

        Bottom ->
            Flex.Bottom

        VCenter ->
            Flex.VCenter

        SpaceBetween ->
            Flex.VSpaceBetween

        SpaceAround ->
            Flex.VSpaceAround


vertical : Vertical -> Mixin msg
vertical ver =
    Mixin.batch <|
        case ver of
            Top ->
                [ style "-ms-flex-pack" "start"
                , style "justify-content" "flex-start"
                ]

            Bottom ->
                [ style "-ms-flex-pack" "end"
                , style "justify-content" "flex-end"
                ]

            VCenter ->
                [ style "-ms-flex-pack" "center"
                , style "justify-content" "center"
                ]

            SpaceBetween ->
                [ style "-ms-flex-pack" "justify"
                , style "justify-content" "space-between"
                ]

            SpaceAround ->
                [ style "-ms-flex-pack" "distribute"
                , style "justify-content" "space-around"
                ]



-- Optimization


{-| Optimized column.

  - First argument: identifier for each element
      - the return value must be unique among all elements

  - Second argument: apply `lazyN` on the resulting function of `toProtected`.

```
v1 : View p msg
v1 =
    Debug.todo "v1"

v2 : View p msg
v2 =
    Debug.todo "v2"

child : Int -> View p msg
child _ =
    Debug.todo "child"

-- Make sure to declare this top level in order to `lazy` works well.
child_ : Int -> Column -> Html (Protected p msg)
child_ n =
    toProtected <| child n

v : List Int -> View p msg
v =
    optimized
        String.fromInt
        (\n -> lazy2 child_ n)
        defaultColumn
```

-}
optimized :
    (x -> String)
    -> (x -> Column -> Protected p Renderer -> Html (Protected p msg))
    -> Column
    -> List x
    -> View p msg
optimized identifier f align =
    Neat.optimized
        identifier
        (\x -> f x align)
        align.nodeName
        (columnMixins align)


{-| This is supposed to be used in order to make `Html.lazy.lazyN` work.
See `optimized` for real usage.
-}
toProtected : View p a -> Column -> Protected p Renderer -> Html (Protected p a)
toProtected v align =
    Neat.toProtected <|
        expandChild align v



-- Helper functions


style : String -> String -> Mixin msg
style k v =
    Mixin.fromAttribute <| Attributes.style k v
