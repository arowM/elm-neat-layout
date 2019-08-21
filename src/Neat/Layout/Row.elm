module Neat.Layout.Row exposing
    ( rowWith
    , Row
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
@docs Row
@docs defaultRow
@docs row
@docs Vertical
@docs Horizontal


# Optimization

@docs optimized
@docs toProtected

-}

import Html exposing (Html)
import Html.Attributes as Attributes
import Mixin exposing (Mixin)
import Neat exposing (Protected, View)
import Neat.Layout.Internal as Layout



-- Core


{-| -}
rowWith : Row -> List (View p msg) -> View p msg
rowWith align children =
    Neat.div (rowMixins align) <|
        List.map (expandV align.vertical) children


rowMixins : Row -> List (Mixin msg)
rowMixins align =
    [ flex
    , flexDirection
    , horizontal align.horizontal
    , vertical align.vertical
    , flexWrap align.wrap
    ]


expandV : Vertical -> View p msg -> View p msg
expandV v =
    case v of
        Stretch ->
            Neat.setLayout <|
                Layout.fromRecord <|
                    { inner = style "height" "100%"
                    , outer = Mixin.none
                    }

        _ ->
            identity


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
    , wrap : Bool
    }


{-| Default `Row` configuration.

    { vertical = Top
    , horizontal = Left
    , wrap = False
    }

-}
defaultRow : Row
defaultRow =
    { vertical = Top
    , horizontal = Left
    , wrap = False
    }



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


flexWrap : Bool -> Mixin msg
flexWrap b =
    if b then
        Mixin.batch
            [ style "-ms-flex-wrap" "wrap"
            , style "flex-wrap" "wrap"
            ]

    else
        Mixin.batch
            [ style "-ms-flex-wrap" "nowrap"
            , style "flex-wrap" "nowrap"
            ]



-- Horizontal alignment


{-| -}
type Horizontal
    = Left
    | Right
    | HCenter
    | SpaceBetween
    | SpaceAround


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

            SpaceBetween ->
                [ style "-ms-flex-pack" "justify"
                , style "justify-content" "space-between"
                ]

            SpaceAround ->
                [ style "-ms-flex-pack" "distribute"
                , style "justify-content" "space-around"
                ]



-- Vertical alignment


{-| -}
type Vertical
    = Top
    | Bottom
    | VCenter
    | Stretch


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

            Stretch ->
                [ style "-ms-flex-align" "stretch"
                , style "align-items" "stretch"
                ]



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
child_ : Int -> Vertical -> Html (Protected p msg)
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
    -> (x -> Vertical -> Html (Protected p msg))
    -> Row
    -> List x
    -> View p msg
optimized identifier f align =
    Neat.optimized
        identifier
        (\x -> f x align.vertical)
        "div"
        (rowMixins align)


{-| This is supposed to be used in order to make `Html.lazy.lazyN` work.
See `optimized` for real usage.
-}
toProtected : View p a -> Vertical -> Html (Protected p a)
toProtected v vert =
    Neat.toProtected <|
        expandV vert v



-- Helper functions


style : String -> String -> Mixin msg
style k v =
    Mixin.fromAttribute <| Attributes.style k v
