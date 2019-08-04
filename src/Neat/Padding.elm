module Neat.Padding exposing
    ( NoPadding
    , IsPadding(..)
    , fromNoPadding
    , expand
    , setBoundaryWith
    , setBoundary
    , Boundary
    , defaultBoundary
    )

{-|


# Primitive

@docs NoPadding


# Custom paddings

You can introduce custom paddings by just declaring their types and `IsPadding` values.

    type MyPadding
        = MyPadding

    myPadding : IsPadding MyPadding
    myPadding =
        IsPadding
            { rem = 0.6
            }

@docs IsPadding


# Convert between paddings

@docs fromNoPadding
@docs expand


# Boundary

@docs setBoundaryWith
@docs setBoundary
@docs Boundary
@docs defaultBoundary

-}

import Html
import Html.Attributes as Attributes
import Neat.Appearance exposing (Appearance)
import Neat.Internal as Internal exposing (View, div)
import Neat.Layout.Internal as Layout exposing (Layout)



-- Primitives


{-| Primitive type representing no padding on a view.
-}
type NoPadding
    = NoPadding


{-| Information about your custom paddings.

  - rem : padding width in units of `rem`

-}
type IsPadding p
    = IsPadding
        { rem : Float
        }



-- Convert between paddings


{-| -}
fromNoPadding : IsPadding p -> View NoPadding msg -> View p msg
fromNoPadding =
    expand noPadding


{-| Expand padding from `p1` to `p2`.
The width of `p2` is supposed to be greater than `p1`.
-}
expand : IsPadding p1 -> IsPadding p2 -> View p1 msg -> View p2 msg
expand (IsPadding c1) (IsPadding c2) child =
    Internal.coerce <|
        Internal.setLayout
            (Layout.style "padding" <|
                String.fromFloat ((c2.rem - c1.rem) / 2)
                    ++ "rem"
            )
            child



-- Boundary


{-| Boundary config.
Available value for `innerOffset` and `outerOffset` is CSS value for length.
e.g., `Just "2px"`, `Just "-3em"`,...
-}
type alias Boundary =
    { innerOffset : Maybe String
    , outerOffset : Maybe String
    }


{-| Default `Boundary`.

    { innerOffset = Nothing
    , outerOffset = Nothing
    }

-}
defaultBoundary : Boundary
defaultBoundary =
    { innerOffset = Nothing
    , outerOffset = Nothing
    }


{-| Set boundary to convert into `NoPadding`.
-}
setBoundaryWith : IsPadding p -> Boundary -> List (Appearance msg) -> List (View p msg) -> View NoPadding msg
setBoundaryWith config boundary appearance children =
    Internal.coerce <|
        Internal.setLayout (outerPadding boundary.outerOffset) <|
            div appearance
                [ Internal.setLayout
                    (innerPaddingWithOffset config boundary.innerOffset)
                  <|
                    Internal.batch children
                ]


{-| Shorthands for `setBoundaryWith` with defaultBoundary.
-}
setBoundary : IsPadding p -> List (Appearance msg) -> List (View p msg) -> View NoPadding msg
setBoundary config =
    setBoundaryWith config defaultBoundary


innerPaddingWithOffset : IsPadding p -> Maybe String -> Layout msg
innerPaddingWithOffset config moffset =
    Layout.style "padding" <|
        String.concat
            [ "calc("
            , Maybe.withDefault "0" moffset
            , " + "
            , innerPaddingValue config
            , ")"
            ]


outerPadding : Maybe String -> Layout msg
outerPadding moffset =
    case moffset of
        Nothing ->
            Layout.none

        Just offset ->
            Layout.style "padding" offset


innerPaddingValue : IsPadding p -> String
innerPaddingValue (IsPadding { rem }) =
    String.fromFloat (rem / 2) ++ "rem"



-- Helper functions


noPadding : IsPadding NoPadding
noPadding =
    IsPadding
        { rem = 0
        }
