module Neat exposing
    ( View
    , toPage
    , lift
    , batch
    , none
    , setMixin
    , setMixins
    , apply
    , setBoundary
    , setBoundaryWith
    , BoundaryConfig
    , defaultBoundaryConfig
    , div
    , text
    , keyed
    , keyedLazy
    , toHtmlForLazy
    )

{-| Main module for elm-neat-layout.


# Core

@docs View
@docs toPage


# Constructors and Modifiers for View

@docs lift
@docs batch
@docs none
@docs setMixin
@docs setMixins
@docs apply


# Boundary

@docs setBoundary
@docs setBoundaryWith
@docs BoundaryConfig
@docs defaultBoundaryConfig


# Alternatives to Html nodes

@docs div
@docs text


# Keyed

@docs keyed
@docs keyedLazy
@docs toHtmlForLazy

-}

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Keyed as Keyed
import Mixin exposing (Mixin)
import Neat.Internal as Internal
import Neat.Padding.FullPadding exposing (FullPadding)
import Neat.Padding.NoPadding exposing (NoPadding)



-- Core


{-| Html alternative with type level paddings.
-}
type alias View padding msg =
    Internal.View padding msg


{-| Call this function **only once** on root view function.
Make sure that your own CSS is partially overwritten by following CSS.

    *,
    *::before,
    *::after {
      margin: 0;
      border: 0;
      padding: 0;
      box-sizing: border-box;
    }

-}
toPage : View NoPadding msg -> Html msg
toPage v =
    Html.div []
        [ resetCss
        , Internal.toHtml [] v
        ]


resetCss : Html msg
resetCss =
    Html.node "style"
        []
        [ Html.text <|
            """
            *,
            *::before,
            *::after {
              margin: 0;
              border: 0;
              padding: 0;
              box-sizing: border-box;
            }
            """
        ]



-- Constructors and Modifiers for View


{-| Lift `Html` nodes into `View`.

    import Html exposing (div)
    import Mixin
    import View.NoPadding as NoPadding exposing (NoPadding)

    view1 : View NoPadding msg
    view1 =
        View.div
            []
            [ NoPadding.text "view1"
            ]

    view2 : View NoPadding msg
    view2 =
        View.div
            []
            [ NoPadding.text "view2"
            ]

    composed : View NoPadding msg
    composed =
        lift div
            [ Mixin.class "parent"
            ]
            [ view1
            , view2
            ]

-}
lift : (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Mixin msg) -> List (View p msg) -> View p msg
lift =
    Internal.lift


{-| -}
batch : List (View p a) -> View p a
batch =
    div []


{-| Alias for `text ""`.
-}
none : View p a
none =
    Internal.fromHtml <| \_ -> Html.text ""


{-| -}
setMixin : Mixin msg -> View p msg -> View p msg
setMixin =
    Internal.setMixin


{-| -}
setMixins : List (Mixin msg) -> View p msg -> View p msg
setMixins mixins =
    Internal.setMixin <| Mixin.batch mixins


{-| -}
apply : (Html a -> Html a) -> View p a -> View p a
apply f (Internal.View html) =
    Internal.fromHtml <|
        \attrs ->
            f <| html <| Mixin.fromAttributes attrs



-- Boundary


{-| Set boundary to full-padding views.
-}
setBoundaryWith : BoundaryConfig -> List (Mixin msg) -> List (View FullPadding msg) -> View NoPadding msg
setBoundaryWith boundary mixins children =
    Internal.coerce <|
        setOffset boundary.outerOffset <|
            div mixins
                [ div
                    [ fullPaddingWithOffset boundary.innerOffset
                    ]
                    children
                ]


{-| Shorthands for `setBoundaryWith defaultBoundaryConfig`.
-}
setBoundary : List (Mixin msg) -> List (View FullPadding msg) -> View NoPadding msg
setBoundary =
    setBoundaryWith defaultBoundaryConfig


{-| Boundary config.
Available value for `innerOffset` and `outerOffset` is CSS value for length.
e.g., `Just "2px"`, `Just "-3em"`,...
-}
type alias BoundaryConfig =
    { innerOffset : Maybe String
    , outerOffset : Maybe String
    }


{-| Default `BoundaryConfig`.

    { innerOffset = Nothing
    , outerOffset = Nothing
    }

-}
defaultBoundaryConfig : BoundaryConfig
defaultBoundaryConfig =
    { innerOffset = Nothing
    , outerOffset = Nothing
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
            div
                [ Mixin.fromAttribute <|
                    Attributes.style "padding" offset
                ]
                [ v
                ]


fullPaddingValue : String
fullPaddingValue =
    "0.5rem"



-- Alternatives to Html nodes


{-| `View` version of `Html.div`.
-}
div : List (Mixin msg) -> List (View p msg) -> View p msg
div =
    Internal.div


{-| `View` version of `Html.text`.
-}
text : String -> View NoPadding msg
text str =
    Internal.fromHtml <| \_ -> Html.text str


{-| -}
keyed :
    String
    -> List (Mixin msg)
    -> List ( String, View p msg )
    -> View p msg
keyed tag mixin =
    keyedLazy tag mixin << List.map (Tuple.mapSecond (Internal.toHtml []))


{-|

    v1 : View p msg
    v1 =
        Debug.todo "v1"

    v2 : View p msg
    v2 =
        Debug.todo "v2"

    child : Int -> View p msg
    child _ =
        Debug.todo "child"

    child_ : Int -> Html msg
    child_ =
        toHtmlForLazy << child

    v : List Int -> View p msg
    v ns =
        keyedLazy "div"
            []
            (\n -> ( String.fromInt n, lazy child n ))

-}
keyedLazy :
    String
    -> List (Mixin msg)
    -> List ( String, Html msg )
    -> View p msg
keyedLazy tag mixin children =
    Internal.fromHtml <|
        \extra ->
            Keyed.node tag (List.concatMap Mixin.toAttributes mixin ++ extra) <| children


{-| DO NOT overuse.
This is only supposed to be used in order to make `Html.Lazy.lazy` work.
See `keyedLazy` for real usage.
-}
toHtmlForLazy : View p a -> Html a
toHtmlForLazy =
    Internal.toHtml []
