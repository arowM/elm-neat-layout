module Neat exposing
    ( View
    , toPage
    , lift
    , none
    , setMixin
    , setMixins
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
@docs none
@docs setMixin
@docs setMixins
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
import Html.Keyed as Keyed
import Mixin exposing (Mixin)
import Neat.Internal as Internal
import Neat.Layout.Internal as Layout exposing (Layout)
import Neat.Padding exposing (NoPadding)



-- Core


{-| Html alternative it can manage paddings in type level.
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
        , Internal.toHtml Layout.none Mixin.none v
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
    import Mixin exposing (Mixin)
    import Neat exposing (View)
    import Neat.Padding as Padding exposing (NoPadding)

    view1 : View NoPadding msg
    view1 =
        Neat.div
            []
            [ Neat.text "view1"
            ]

    view2 : View NoPadding msg
    view2 =
        Neat.div
            []
            [ Neat.text "view2"
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


{-| Alias for `text ""`.
-}
none : View NoPadding a
none =
    text ""


{-| -}
setMixin : Mixin msg -> View NoPadding msg -> View NoPadding msg
setMixin =
    Internal.setMixin


{-| -}
setMixins : List (Mixin msg) -> View NoPadding msg -> View NoPadding msg
setMixins =
    Internal.setMixin << Mixin.batch


{-| -}
setLayout : Layout msg -> View p msg -> View p msg
setLayout =
    Internal.setLayout



-- Alternatives to Html nodes


{-| `View` version of `Html.div`.
-}
div : List (Mixin msg) -> List (View p msg) -> View p msg
div =
    Internal.div


{-| `View` version of `Html.span [] [ Html.text ]`.
-}
text : String -> View NoPadding msg
text str =
    lift Html.span
        []
        [ Internal.fromHtml <| \_ _ -> Html.text str
        ]


{-| -}
keyed :
    String
    -> List (Mixin msg)
    -> List ( String, View p msg )
    -> View p msg
keyed tag mixin =
    keyedLazy tag mixin << List.map (Tuple.mapSecond (Internal.toHtml Layout.none Mixin.none))


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
keyedLazy =
    Internal.keyedLazy


{-| DO NOT overuse. It can break layouts.

This is only supposed to be used in order to make `Html.Lazy.lazy` work.
See `keyedLazy` for real usage.

-}
toHtmlForLazy : View p a -> Html a
toHtmlForLazy =
    Internal.toHtml Layout.none Mixin.none
