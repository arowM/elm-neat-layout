module Neat.Layout.Row exposing
    ( rowWith
    , rowWithMap
    , Row
    , Wrap(..)
    , defaultRow
    , row
    , Vertical(..)
    , Horizontal(..)
    , optimized
    , toProtected
    )

{-|


# Row

@docs rowWith
@docs rowWithMap
@docs Row
@docs Wrap
@docs defaultRow
@docs row
@docs Vertical
@docs Horizontal


# Optimization

@docs optimized
@docs toProtected

-}

import Html exposing (Html)
import Mixin exposing (Mixin)
import Neat exposing (Protected, Renderer, View)
import Neat.Flex as Flex exposing (Flex)
import Neat.Internal exposing (unsafeMap)



-- Core


{-| Align Views horizontally.

Alias for `rowWithMap`.

-}
rowWith : Row -> List (View p msg) -> View p msg
rowWith align children =
    wrapper align (rowMixins align) <|
        List.map (expandChild align) <|
            List.map
                (Neat.setLayout
                    (Flex.setChildBasis <| toFlexWrap align.wrap)
                )
                children


{-| Align Views horizontally.
In addition, `rowWithMap` can convert msg type of a View.
-}
rowWithMap : (a -> b) -> Row -> List (View p a) -> View p b
rowWithMap f align children =
    wrapper align (rowMixins align) <|
        List.map
            (identity
                >> Neat.setLayout
                    (Flex.setChildBasis <| toFlexWrap align.wrap)
                >> expandChild align
                >> unsafeMap f
            )
            children


wrapper : Row -> List (Mixin msg) -> List (View p msg) -> View p msg
wrapper align =
    case align.nodeName of
        "div" ->
            Neat.lift Html.div

        a ->
            Neat.lift (Html.node a)


rowMixins : Row -> List (Mixin msg)
rowMixins align =
    Flex.rowMixins <| toFlex align


expandChild : Row -> View p msg -> View p msg
expandChild align =
    Neat.setLayout <| Flex.childLayout <| toFlex align


{-| An alias for `rowWith defaultRow`.
-}
row : List (View p msg) -> View p msg
row =
    rowWith defaultRow


{-| Configuration about row alignment.
-}
type alias Row =
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


toFlex : Row -> Flex
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


{-| Default `Row` configuration.

    { vertical = Stretch
    , horizontal = Left
    , wrap = NoWrap
    , nodeName = "div"
    }

-}
defaultRow : Row
defaultRow =
    { vertical = Stretch
    , horizontal = Left
    , wrap = NoWrap
    , nodeName = "div"
    }



-- Horizontal alignment


{-| -}
type Horizontal
    = Left
    | Right
    | HCenter
    | SpaceBetween
    | SpaceAround


toFlexHorizontal : Horizontal -> Flex.Horizontal
toFlexHorizontal h =
    case h of
        Left ->
            Flex.Left

        Right ->
            Flex.Right

        HCenter ->
            Flex.HCenter

        SpaceBetween ->
            Flex.HSpaceBetween

        SpaceAround ->
            Flex.HSpaceAround



-- Vertical alignment


{-| -}
type Vertical
    = Top
    | Bottom
    | VCenter
    | Stretch


toFlexVertical : Vertical -> Flex.Vertical
toFlexVertical v =
    case v of
        Top ->
            Flex.Top

        Bottom ->
            Flex.Bottom

        VCenter ->
            Flex.VCenter

        Stretch ->
            Flex.VStretch



-- Optimization


{-| Optimized row.

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
child_ : Int -> Row -> Html (Protected p msg)
child_ n =
    toProtected <| child n

v : List Int -> View p msg
v =
    optimized
        String.fromInt
        (\n -> lazy2 child_ n)
        defaultRow
```

-}
optimized :
    (x -> String)
    -> (x -> Row -> Protected p Renderer -> Html (Protected p msg))
    -> Row
    -> List x
    -> View p msg
optimized identifier f align =
    Neat.optimized
        identifier
        (\x -> f x align)
        align.nodeName
        (rowMixins align)


{-| This is supposed to be used in order to make `Html.lazy.lazyN` work.
See `optimized` for real usage.
-}
toProtected : View p a -> Row -> Protected p Renderer -> Html (Protected p a)
toProtected v vert =
    Neat.toProtected <|
        expandChild vert v
