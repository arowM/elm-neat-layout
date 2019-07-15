module Neat.FullPadding exposing
    ( FullPadding
    , fromNoPadding
    , ratio
    , setBoundary
    , Boundary
    , defaultBoundary
    )

{-|


# Core

@docs FullPadding
@docs fromNoPadding
@docs ratio


# Boundary

@docs setBoundary
@docs Boundary
@docs defaultBoundary

-}

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Mixin exposing (Mixin)
import Neat exposing (View)
import Neat.Internal as Internal
import Neat.NoPadding as NoPadding exposing (NoPadding)



-- Core


{-| A type that indecates a view has full padding.
-}
type FullPadding
    = FullPadding


{-| Convert to NoPadding by setting boundary.
-}
setBoundary : Boundary msg -> List (View FullPadding msg) -> View NoPadding msg
setBoundary boundary children =
    Internal.coerce <|
        setOffset boundary.outerOffset <|
            Neat.div
                [ boundary.mixin
                ]
                [ Neat.div
                    [ fullPaddingWithOffset boundary.innerOffset
                    ]
                    children
                ]


{-| Boundary settings.
Available value for `innerOffset` and `outerOffset` is CSS value for length.
e.g., `Just "2px"`, `Just "-3em"`,...
-}
type alias Boundary msg =
    { innerOffset : Maybe String
    , outerOffset : Maybe String
    , mixin : Mixin msg
    }


{-| Default boundary setting.

    { innerOffset = Nothing
    , outerOffset = Nothing
    , mixin = Mixin.none
    }

-}
defaultBoundary : Boundary msg
defaultBoundary =
    { innerOffset = Nothing
    , outerOffset = Nothing
    , mixin = Mixin.none
    }


fullPadding : Mixin msg
fullPadding =
    Mixin.fromAttribute <|
        Attributes.style "padding" fullPaddingValue


fullPaddingWithOffset : Maybe String -> Mixin msg
fullPaddingWithOffset moffset =
    case moffset of
        Nothing ->
            fullPadding

        Just offset ->
            Mixin.fromAttribute <|
                Attributes.style "padding" <|
                    String.concat
                        [ "calc("
                        , offset
                        , " + "
                        , fullPaddingValue
                        , ")"
                        ]


setOffset : Maybe String -> View p msg -> View p msg
setOffset moffset v =
    case moffset of
        Nothing ->
            v

        Just offset ->
            Neat.div
                [ Mixin.fromAttribute <|
                    Attributes.style "padding" offset
                ]
                [ v
                ]


fullPaddingValue : String
fullPaddingValue =
    "0.5rem"


{-| -}
fromNoPadding : View NoPadding msg -> View FullPadding msg
fromNoPadding =
    Neat.convert NoPadding.ratio ratio


{-| -}
ratio : Neat.Ratio FullPadding
ratio =
    Neat.ratio 1
