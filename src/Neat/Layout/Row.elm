module Neat.Layout.Row exposing
    ( rowWith
    , Row
    , defaultRow
    , row
    , Vertical(..)
    , Horizontal(..)
    )

{-| Rows

@docs rowWith
@docs Row
@docs defaultRow
@docs row
@docs Vertical
@docs Horizontal

-}

import Html.Attributes as Attributes
import Mixin exposing (Mixin)
import Neat exposing (View)
import Neat.Layout.Internal as Layout



-- Core


{-| -}
rowWith : Row -> List (View p msg) -> View p msg
rowWith align children =
    Neat.div
        []
        [ Neat.div
            [ flex
            , flexDirection
            , horizontal align.horizontal
            , vertical align.vertical
            , flexWrap align.wrap
            ]
          <|
            List.map (expandV align.vertical) children
        ]


expandV : Vertical -> View p msg -> View p msg
expandV v =
    case v of
        Stretch ->
            Neat.setLayout <|
                Layout.fromInner <|
                    style "height" "100%"

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
    , wrap = True
    }

-}
defaultRow : Row
defaultRow =
    { vertical = Top
    , horizontal = Left
    , wrap = True
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



-- Helper functions


style : String -> String -> Mixin msg
style k v =
    Mixin.fromAttribute <| Attributes.style k v
