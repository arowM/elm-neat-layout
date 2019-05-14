module View exposing
    ( View
    , lift
    , apply
    , batch
    , none
    , toHtml
    , fromHtml
    , setMixin
    , div
    , row
    , keyed
    )

{-| Main framework for managing paddings.


# Core

@docs View
@docs lift
@docs apply
@docs batch
@docs none
@docs toHtml
@docs fromHtml
@docs setMixin


# Helper functions for Html

@docs div
@docs row


# Keyed

@docs keyed

-}

import Html exposing (Attribute, Html)
import Layout.Mixin as Mixin
import Mixin exposing (Mixin)
import View.Internal as Internal


{-| Html alternative that is aware of padding width in type level.
-}
type alias View padding msg =
    Internal.View padding msg


{-|

    import Html exposing (div)
    import Mixin
    import View.NoPadding as NoPadding exposing (NoPadding)

    atom1 : View NoPadding msg
    atom1 =
        View.div
            []
            [ NoPadding.text "atom1"
            ]

    atom2 : View NoPadding msg
    atom2 =
        View.div
            []
            [ NoPadding.text "atom2"
            ]

    (lift div)
        [ Mixin.class "parent"
        ]
        [ atom1
        , atom2
        ]

-}
lift : (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Mixin msg) -> List (View p msg) -> View p msg
lift =
    Internal.lift


{-| -}
apply : (Html a -> Html a) -> View p a -> View p a
apply =
    Internal.apply


{-| -}
batch : List (View padding a) -> View padding a
batch =
    div []


{-| -}
none : View padding a
none =
    Internal.fromHtml <| \_ -> Html.text ""


{-| DO NOT overuse.
-}
toHtml : View p a -> Html a
toHtml =
    Internal.toHtml []


{-| DO NOT overuse.
-}
fromHtml : Html a -> View p a
fromHtml html =
    Internal.fromHtml <|
        \attrs ->
            Html.div attrs [ html ]


{-| -}
setMixin : Mixin msg -> View p msg -> View p msg
setMixin =
    Internal.setMixin



-- Helper functions for Html


{-| -}
div : List (Mixin msg) -> List (View padding msg) -> View padding msg
div =
    lift Html.div


{-| -}
row : List (View p msg) -> View p msg
row =
    div [ Mixin.row ]



-- Keyed


{-| -}
keyed :
    String
    -> List (Mixin msg)
    -> List ( String, View p msg )
    -> View p msg
keyed =
    Internal.keyed
