module Neat.Layout.Column exposing
    ( columnWith
    , Column
    , defaultColumn
    , column
    , Vertical(..)
    , Horizontal(..)
    )

{-| Columns

@docs columnWith
@docs Column
@docs defaultColumn
@docs column
@docs Vertical
@docs Horizontal

-}

import Mixin exposing (Mixin)
import Neat exposing (View)
import Neat.Layout.Internal as Layout



-- Core


{-| -}
columnWith : Column -> List (View p msg) -> View p msg
columnWith align children =
    Neat.div
        []
        [ Neat.div
            [ inlineStyle <|
                List.concat
                    [ flex
                    , flexDirection
                    , horizontal align.horizontal
                    , vertical align.vertical
                    , flexWrap align.wrap
                    ]
            ]
          <|
            List.map (expandH align.horizontal) children
        ]


expandH : Horizontal -> View p msg -> View p msg
expandH h =
    case h of
        Stretch ->
            Neat.setLayout <|
                Layout.fromOuter <|
                    Mixin.attribute "style" "width: 100%"

        _ ->
            identity


inlineStyle : List ( String, String ) -> Mixin msg
inlineStyle ls =
    Mixin.attribute "style" <|
        String.join ";" <|
            List.map (\( key, val ) -> key ++ ": " ++ val) ls


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
    , wrap = True
    }

-}
defaultColumn : Column
defaultColumn =
    { vertical = Top
    , horizontal = Left
    , wrap = True
    }



-- Mixins


flex : List ( String, String )
flex =
    [ ( "display", "flex" )
    , ( "display", "-webkit-box" )
    , ( "display", "-ms-flexbox" )
    ]


flexDirection : List ( String, String )
flexDirection =
    [ ( "-webkit-box-orient", "vertical" )
    , ( "-webkit-box-direction", "normal" )
    , ( "-ms-flex-direction", "column" )
    , ( "flex-direction", "column" )
    ]


flexWrap : Bool -> List ( String, String )
flexWrap b =
    if b then
        [ ( "-ms-flex-wrap", "wrap" )
        , ( "flex-wrap", "wrap" )
        ]

    else
        [ ( "-ms-flex-wrap", "nowrap" )
        , ( "flex-wrap", "nowrap" )
        ]



-- Horizontal alignment


{-| -}
type Horizontal
    = Left
    | Right
    | HCenter
    | Stretch


horizontal : Horizontal -> List ( String, String )
horizontal hor =
    case hor of
        Left ->
            [ ( "-webkit-box-align", "start" )
            , ( "-ms-flex-align", "start" )
            , ( "align-items", "flex-start" )
            ]

        Right ->
            [ ( "-webkit-box-align", "end" )
            , ( "-ms-flex-align", "end" )
            , ( "align-items", "flex-end" )
            ]

        HCenter ->
            [ ( "-webkit-box-align", "center" )
            , ( "-ms-flex-align", "center" )
            , ( "align-items", "center" )
            ]

        Stretch ->
            [ ( "-webkit-box-align", "stretch" )
            , ( "-ms-flex-align", "stretch" )
            , ( "align-items", "stretch" )
            ]



-- Vertical alignment


{-| -}
type Vertical
    = Top
    | Bottom
    | VCenter
    | SpaceBetween
    | SpaceAround


vertical : Vertical -> List ( String, String )
vertical ver =
    case ver of
        Top ->
            [ ( "-webkit-box-pack", "start" )
            , ( "-ms-flex-pack", "start" )
            , ( "justify-content", "flex-start" )
            ]

        Bottom ->
            [ ( "-webkit-box-pack", "end" )
            , ( "-ms-flex-pack", "end" )
            , ( "justify-content", "flex-end" )
            ]

        VCenter ->
            [ ( "-webkit-box-pack", "center" )
            , ( "-ms-flex-pack", "center" )
            , ( "justify-content", "center" )
            ]

        SpaceBetween ->
            [ ( "-webkit-box-pack", "justify" )
            , ( "-ms-flex-pack", "justify" )
            , ( "justify-content", "space-between" )
            ]

        SpaceAround ->
            [ ( "-ms-flex-pack", "distribute" )
            , ( "justify-content", "space-around" )
            ]
