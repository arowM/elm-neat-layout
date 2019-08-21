module Neat.Layout.Column exposing
    ( columnWith
    , Column
    , defaultColumn
    , column
    , Vertical(..)
    , Horizontal(..)
    , optimized
    , toProtected
    )

{-|


# Columns

@docs columnWith
@docs Column
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
import Neat exposing (Protected, View)
import Neat.Layout.Internal as Layout



-- Core


{-| -}
columnWith : Column -> List (View p msg) -> View p msg
columnWith align children =
    Neat.div
        (columnMixins align)
    <|
        List.map (expandH align.horizontal) children


columnMixins : Column -> List (Mixin msg)
columnMixins align =
    [ flex
    , flexDirection
    , horizontal align.horizontal
    , vertical align.vertical
    , flexWrap align.wrap
    ]


expandH : Horizontal -> View p msg -> View p msg
expandH h =
    case h of
        Stretch ->
            Neat.setLayout <|
                Layout.fromRecord
                    { inner = style "width" "100%"
                    , outer = Mixin.none
                    }

        _ ->
            identity


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
    , wrap : Bool
    }


{-| Default `Column` configuration.

    { vertical = Top
    , horizontal = Left
    , wrap = False
    }

-}
defaultColumn : Column
defaultColumn =
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
        [ style "-ms-flex-direction" "column"
        , style "flex-direction" "column"
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
    | Stretch


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
child_ : Int -> Horizontal -> Html (Protected p msg)
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
    -> (x -> Horizontal -> Html (Protected p msg))
    -> Column
    -> List x
    -> View p msg
optimized identifier f align =
    Neat.optimized
        identifier
        (\x -> f x align.horizontal)
        "div"
        (columnMixins align)


{-| This is supposed to be used in order to make `Html.lazy.lazyN` work.
See `optimized` for real usage.
-}
toProtected : View p a -> Horizontal -> Html (Protected p a)
toProtected v hor =
    Neat.toProtected <|
        expandH hor v



-- Helper functions


style : String -> String -> Mixin msg
style k v =
    Mixin.fromAttribute <| Attributes.style k v
