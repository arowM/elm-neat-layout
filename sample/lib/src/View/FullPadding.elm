module View.FullPadding exposing
    ( FullPadding
    , setBoundary
    , fromNoPadding
    , fromNarrowPadding
    , fromMiddlePadding
    )

{-|


# Core

@docs FullPadding
@docs setBoundary


# Lift functions

@docs fromNoPadding
@docs fromNarrowPadding
@docs fromMiddlePadding

-}

import Html exposing (Attribute, Html)
import Mixin exposing (Mixin)
import View exposing (View)
import View.Internal as Internal
import View.MiddlePadding exposing (MiddlePadding)
import View.NarrowPadding exposing (NarrowPadding)
import View.NoPadding exposing (NoPadding)



-- Core


{-| A type that indecates a view has full padding.
-}
type FullPadding
    = FullPadding


{-| Takes a function to set boundary.
-}
setBoundary : (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Mixin msg) -> List (View FullPadding msg) -> View NoPadding msg
setBoundary node mixin children =
    Internal.fromHtml <|
        \extra ->
            node (List.concatMap Mixin.toAttributes mixin ++ extra) <|
                List.map (Internal.toHtml []) children



-- Lift functions


{-| -}
fromNoPadding : View NoPadding msg -> View FullPadding msg
fromNoPadding =
    Internal.convert
        ( Internal.noPadding
        , Internal.fullPadding
        )


{-| -}
fromNarrowPadding : View NarrowPadding msg -> View FullPadding msg
fromNarrowPadding =
    Internal.convert
        ( Internal.narrowPadding
        , Internal.fullPadding
        )


{-| -}
fromMiddlePadding : View MiddlePadding msg -> View FullPadding msg
fromMiddlePadding =
    Internal.convert
        ( Internal.middlePadding
        , Internal.fullPadding
        )
