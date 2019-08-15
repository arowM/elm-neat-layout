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
    , NoPadding
    , IsPadding(..)
    , fromNoPadding
    , expand
    , setBoundary
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
@docs setBoundary

-}

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Keyed as Keyed
import Mixin exposing (Mixin)
import Neat.Layout.Internal as Layout exposing (Layout)



-- Core


{-| Html alternative it can manage paddings in type level.
-}
type View padding msg
    = View (Layout msg -> Mixin msg -> Html msg)


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
        , toHtml Layout.none Mixin.none v
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
lift node appearances children =
    liftHelper node appearances <|
        List.map (toHtml Layout.none Mixin.none) children


{-| Alias for `text ""`.
-}
none : View NoPadding a
none =
    text ""


{-| -}
setMixin : Mixin msg -> View NoPadding msg -> View NoPadding msg
setMixin appearance (View f) =
    View <|
        \layout extra ->
            f layout (Mixin.batch [ appearance, extra ])


{-| -}
setMixins : List (Mixin msg) -> View NoPadding msg -> View NoPadding msg
setMixins =
    setMixin << Mixin.batch


{-| -}
setLayout : Layout msg -> View p msg -> View p msg
setLayout layout (View f) =
    View <|
        \extra appearance ->
            f (Layout.batch [ layout, extra ]) appearance



-- Alternatives to Html nodes


{-| `View` version of `Html.div`.
-}
div : List (Mixin msg) -> List (View p msg) -> View p msg
div =
    lift Html.div


{-| `View` version of `Html.span [] [ Html.text ]`.
-}
text : String -> View NoPadding msg
text str =
    lift Html.span
        []
        [ fromHtml <| \_ _ -> Html.text str
        ]


{-| -}
keyed :
    String
    -> List (Mixin msg)
    -> List ( String, View p msg )
    -> View p msg
keyed tag mixin =
    keyedLazy tag mixin << List.map (Tuple.mapSecond (toHtml Layout.none Mixin.none))


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
keyedLazy tag =
    liftHelper (Keyed.node tag)


{-| DO NOT overuse. It can break layouts.

This is only supposed to be used in order to make `Html.Lazy.lazy` work.
See `keyedLazy` for real usage.

-}
toHtmlForLazy : View p a -> Html a
toHtmlForLazy =
    toHtml Layout.none Mixin.none



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
expand p1 p2 child =
    setLayout (expandPadding p1 p2) child
        |> coerce


expandPadding : IsPadding p1 -> IsPadding p2 -> Layout msg
expandPadding (IsPadding c1) (IsPadding c2) =
    Layout.none
        |> Layout.setOuter
            (Mixin.attribute "style" <|
                "padding:"
                    ++ String.fromFloat ((c2.rem - c1.rem) / 2)
                    ++ "rem"
            )



-- Boundary


{-| Set boundary to convert into `NoPadding`.
-}
setBoundary : IsPadding p -> View p msg -> View NoPadding msg
setBoundary config child =
    coerce <|
        div
            [ innerPadding config
            ]
            [ child
            ]


innerPadding : IsPadding p -> Mixin msg
innerPadding config =
    Mixin.attribute "style" <|
        "padding: "
            ++ innerPaddingValue config


innerPaddingValue : IsPadding p -> String
innerPaddingValue (IsPadding { rem }) =
    String.fromFloat (rem / 2) ++ "rem"



-- Helper functions


noPadding : IsPadding NoPadding
noPadding =
    IsPadding
        { rem = 0
        }


toHtml : Layout msg -> Mixin msg -> View padding msg -> Html msg
toHtml layout appearance (View f) =
    f layout appearance


fromHtml : (Layout msg -> Mixin msg -> Html msg) -> View padding msg
fromHtml =
    View


coerce : View p1 a -> View p2 a
coerce (View f) =
    View f


liftHelper : (List (Attribute msg) -> List a -> Html msg) -> List (Mixin msg) -> List a -> View p msg
liftHelper node appearances children =
    fromHtml <|
        \layout extra ->
            wrapHtml (Layout.toOuter layout) <|
                \extraAttrs ->
                    node
                        (Mixin.toAttributes <|
                            Mixin.batch
                                [ Mixin.batch appearances
                                , extra
                                , Layout.toInner layout
                                , Mixin.fromAttributes extraAttrs
                                ]
                        )
                        children


wrapHtml : Mixin msg -> (List (Attribute msg) -> Html msg) -> Html msg
wrapHtml outer f =
    if outer == Mixin.none then
        f []

    else
        Html.div (Mixin.toAttributes outer)
            [ f
                [ Attributes.attribute "style" <| "width: 100%; height: 100%;"
                ]
            ]
