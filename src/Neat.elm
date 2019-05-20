module Neat exposing
    ( View
    , lift
    , apply
    , batch
    , none
    , unsafeToHtml
    , unsafeFromHtml
    , setMixin
    , setMixins
    , Ratio
    , ratio
    , convert
    , div
    -- , row
    , keyed
    )

{-| Main framework for managing paddings.


# Core

@docs View
@docs lift
@docs apply
@docs batch
@docs none
@docs unsafeToHtml
@docs unsafeFromHtml
@docs setMixin
@docs setMixins


# Convert to another padding

@docs Ratio
@docs ratio
@docs convert


# Helper functions for Html

@docs div
-- @docs row


# Keyed

@docs keyed

-}

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Keyed as Keyed
import Mixin exposing (Mixin)
import Neat.Internal as Internal


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

    lift div
        [ Mixin.class "parent"
        ]
        [ atom1
        , atom2
        ]

-}
lift : (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Mixin msg) -> List (View p msg) -> View p msg
lift node mixins children =
    Internal.fromHtml <|
        \extra ->
            node (List.concatMap Mixin.toAttributes mixins ++ extra) <|
                List.map (Internal.toHtml []) children


{-| -}
apply : (Html a -> Html a) -> View p a -> View p a
apply f (Internal.View html) =
    Internal.fromHtml <|
        \attrs ->
            f <| html <| Mixin.fromAttributes attrs




{-| -}
batch : List (View padding a) -> View padding a
batch =
    div []


{-| -}
none : View padding a
none =
    Internal.fromHtml <| \_ -> Html.text ""


{-| DO NOT overuse.
    This is only supposed to be used in order to make `Html.Lazy.lazy` works.
-}
unsafeToHtml : View p a -> Html a
unsafeToHtml =
    Internal.toHtml []


{-| DO NOT overuse.
    This is only supposed to be used in order to make `Html.Lazy.lazy` works.
-}
unsafeFromHtml : Html a -> View p a
unsafeFromHtml html =
    Internal.fromHtml <|
        \attrs ->
            Html.div attrs [ html ]


{-| -}
setMixin : Mixin msg -> View p msg -> View p msg
setMixin =
    Internal.setMixin


{-| -}
setMixins : List (Mixin msg) -> View p msg -> View p msg
setMixins mixins =
    Internal.setMixin <| Mixin.batch mixins


-- Convert to another padding


{-| Padding ratio to `FullPadding`.
-}
type Ratio p =
    Ratio Float


{-| Constructor for `Ratio`.
    Takes float value of `"Width of p" / "Width of FullPadding"`.
-}
ratio : Float -> Ratio p
ratio =
    Ratio


{-| Convert padding from `p1` to `p2`.
    It must satisfy condition: `"Width of p1" <= "Width of p2" <= "Width of FullPadding"`.
-}
convert : Ratio p1 -> Ratio p2 -> View p1 msg -> View p2 msg
convert (Ratio r1) (Ratio r2) child =
    Internal.coerce <|
    lift Html.div
        [ Mixin.fromAttribute <|
            Attributes.style "padding" <| String.fromFloat ((r2 - r1) / 2) ++ "rem"
        ]
        [ child
        ]



-- Helper functions for Html


{-| -}
div : List (Mixin msg) -> List (View padding msg) -> View padding msg
div =
    lift Html.div


-- {-| -}
-- row : List (View p msg) -> View p msg
-- row =
--     div [ Mixin.row ]



-- Keyed


{-| -}
keyed :
    String
    -> List (Mixin msg)
    -> List ( String, View p msg )
    -> View p msg
keyed tag mixin children =
    Internal.fromHtml <|
        \extra ->
            Keyed.node tag (List.concatMap Mixin.toAttributes mixin ++ extra) <|
                List.map (Tuple.mapSecond (Internal.toHtml [])) children
