module Neat.Alignment.Row exposing
    ( rowWith
    , Row
    , defaultRow
    , row
    , Vertical
    , top
    , bottom
    , vcenter
    , vstretch
    , Horizontal
    , left
    , right
    , hcenter
    , spaceBetween
    , spaceAround
    , hstretch
    )

{-| Rows


# Core

@docs rowWith
@docs Row
@docs defaultRow
@docs row


# Vertical constructors

@docs Vertical
@docs top
@docs bottom
@docs vcenter
@docs vstretch


# Horizontal constructors

@docs Horizontal
@docs left
@docs right
@docs hcenter
@docs spaceBetween
@docs spaceAround
@docs hstretch

-}

import Mixin exposing (Mixin, style)
import Neat.Internal as Internal exposing (View)



-- Core


{-| -}
rowWith : Row -> List (View p msg) -> View p msg
rowWith align =
    Internal.div
        [ inlineStyle <|
            List.concat
                [ flex
                , horizontal align.horizontal
                , vertical align.vertical
                , flexWrap align.wrap
                ]
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

    { vertical = top
    , horizontal = left
    , wrap = True
    }

-}
defaultRow : Row
defaultRow =
    { vertical = top
    , horizontal = left
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


type Horizontal
    = Left
    | Right
    | HCenter
    | SpaceBetween
    | SpaceAround
    | HStretch


left : Horizontal
left =
    Left


right : Horizontal
right =
    Right


hcenter : Horizontal
hcenter =
    HCenter


spaceBetween : Horizontal
spaceBetween =
    SpaceBetween


spaceAround : Horizontal
spaceAround =
    SpaceAround


hstretch : Horizontal
hstretch =
    HStretch


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


type Vertical
    = Top
    | Bottom
    | VCenter
    | VStretch


top : Vertical
top =
    Top


bottom : Vertical
bottom =
    Bottom


vcenter : Vertical
vcenter =
    VCenter


vstretch : Vertical
vstretch =
    VStretch


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
