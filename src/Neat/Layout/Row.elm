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

import Mixin exposing (Mixin)
import Neat exposing (View)



-- Core


{-| -}
rowWith : Row -> List (View p msg) -> View p msg
rowWith align children =
    Neat.div
        []
        [ Neat.div
            [ inlineStyle <|
                List.concat
                    [ flex
                    , horizontal align.horizontal
                    , vertical align.vertical
                    , flexWrap align.wrap
                    ]
            ]
            children
        ]


inlineStyle : List ( String, String ) -> Mixin msg
inlineStyle ls =
    Mixin.attribute "style" <|
        String.join ";" <|
            List.map (\( key, val ) -> key ++ ": " ++ val) ls


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


flex : List ( String, String )
flex =
    [ ( "display", "flex" )
    , ( "display", "-webkit-box" )
    , ( "display", "-ms-flexbox" )
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
    | SpaceBetween
    | SpaceAround
    | HStretch


horizontal : Horizontal -> List ( String, String )
horizontal hor =
    case hor of
        Left ->
            [ ( "-webkit-box-pack", "start" )
            , ( "-ms-flex-pack", "start" )
            , ( "justify-content", "flex-start" )
            ]

        Right ->
            [ ( "-webkit-box-pack", "end" )
            , ( "-ms-flex-pack", "end" )
            , ( "justify-content", "flex-end" )
            ]

        HCenter ->
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

        HStretch ->
            [ ( "-webkit-box-pack", "stretch" )
            , ( "-ms-flex-pack", "stretch" )
            , ( "justify-content", "stretch" )
            ]



-- Vertical alignment


{-| -}
type Vertical
    = Top
    | Bottom
    | VCenter
    | VStretch


vertical : Vertical -> List ( String, String )
vertical ver =
    case ver of
        Top ->
            [ ( "-webkit-box-align", "start" )
            , ( "-ms-flex-align", "start" )
            , ( "align-items", "flex-start" )
            ]

        Bottom ->
            [ ( "-webkit-box-align", "end" )
            , ( "-ms-flex-align", "end" )
            , ( "align-items", "flex-end" )
            ]

        VCenter ->
            [ ( "-webkit-box-align", "center" )
            , ( "-ms-flex-align", "center" )
            , ( "align-items", "center" )
            ]

        VStretch ->
            [ ( "-webkit-box-align", "stretch" )
            , ( "-ms-flex-align", "stretch" )
            , ( "align-items", "stretch" )
            ]
