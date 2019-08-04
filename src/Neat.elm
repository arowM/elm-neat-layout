module Neat exposing
    ( View
    , toPage
    , lift
    , batch
    , none
    , setAppearance
    , setAppearances
    , setLayout
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
@docs setAppearance
@docs setAppearances
@docs setLayout


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
import Neat.Appearance as Appearance exposing (Appearance)
import Neat.Internal as Internal
import Neat.Layout.Internal as Layout exposing (Layout)
import Neat.Padding exposing (NoPadding)



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
        , Internal.toHtml Layout.none Appearance.none v
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
lift : (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Appearance msg) -> List (View p msg) -> View p msg
lift =
    Internal.lift


{-| -}
batch : List (View p a) -> View p a
batch =
    Internal.batch


{-| Alias for `text ""`.
-}
none : View NoPadding a
none =
    text ""


{-| -}
setAppearance : Appearance msg -> View p msg -> View p msg
setAppearance =
    Internal.setAppearance


{-| -}
setAppearances : List (Appearance msg) -> View p msg -> View p msg
setAppearances =
    Internal.setAppearance << Appearance.batch


{-| -}
setLayout : Layout msg -> View p msg -> View p msg
setLayout =
    Internal.setLayout



-- Alternatives to Html nodes


{-| `View` version of `Html.div`.
-}
div : List (Appearance msg) -> List (View p msg) -> View p msg
div =
    Internal.div


{-| `View` version of `Html.text`.
-}
text : String -> View NoPadding msg
text str =
    Internal.fromHtml <| \_ _ -> Html.text str


{-| -}
keyed :
    String
    -> List (Appearance msg)
    -> List ( String, View p msg )
    -> View p msg
keyed tag mixin =
    keyedLazy tag mixin << List.map (Tuple.mapSecond (Internal.toHtml Layout.none Appearance.none))


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
    -> List (Appearance msg)
    -> List ( String, Html msg )
    -> View p msg
keyedLazy tag appearances children =
    Internal.fromHtml <|
        \layout extra ->
            Internal.wrapper layout <|
                Keyed.node tag
                    (Appearance.toAttributes <| Appearance.batch (appearances ++ [ extra ]))
                    children


{-| DO NOT overuse. It can break layouts.

This is only supposed to be used in order to make `Html.Lazy.lazy` work.
See `keyedLazy` for real usage.

-}
toHtmlForLazy : View p a -> Html a
toHtmlForLazy =
    Internal.toHtml Layout.none Appearance.none
