module Neat exposing
    ( View
    , toPage
    , lift
    , none
    , empty
    , emptyNode
    , setMixin
    , setMixins
    , setAttribute
    , setAttributes
    , setLayout
    , div
    , text
    , keyed
    , NoPadding
    , IsPadding(..)
    , fromNoPadding
    , expand
    , setBoundary
    , unsafeFromHtml
    , optimized
    , toProtected
    , Protected
    )

{-| Main module for elm-neat-layout.


# Core

@docs View
@docs toPage


# Constructors and Modifiers for View

@docs lift
@docs none
@docs empty
@docs emptyNode
@docs setMixin
@docs setMixins
@docs setAttribute
@docs setAttributes
@docs setLayout


# Alternatives to Html nodes

@docs div
@docs text


# Keyed

@docs keyed


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


# Unsafe functions

These can break neat paddings, so use them carefully.

@docs unsafeFromHtml


# Lower level functions for performance optimization

These are lower level functions, so all you need would be `Neat.Layout.Row.optimized` and `Neat.Layout.Column.optimized`.

Do not worry even if you cannot understand how to use these functions.

@docs optimized
@docs toProtected
@docs Protected

-}

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Keyed as Keyed
import Mixin exposing (Mixin)
import Neat.Layout.Internal as Layout exposing (Layout(..))



-- Core


{-| Html alternative that has type level padding.
-}
type View padding msg
    = View (Float -> Layout msg -> Mixin msg -> Html msg)


{-| Call this function **only once** on root view function.
Make sure that your own CSS is partially overwritten by following CSS.

    *,
    *::before,
    *::after {
      margin: 0;
      padding: 0;
      box-sizing: border-box;
    }
    [data-elm-neat-layout~=flex]{
        display:-ms-flexbox;
        display:flex
    }

-}
toPage : View NoPadding msg -> List (Html msg)
toPage v =
    [ resetCss
    , toHtml 0 Layout.none Mixin.none v
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
              padding: 0;
              box-sizing: border-box;
            }
            [data-elm-neat-layout~=flex]{
                display:-ms-flexbox;
                display:flex
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
    liftHelper Mixin.none node appearances <|
        List.map (toHtml 0 Layout.none Mixin.none) children


{-| View version of `Html.text ""`.
-}
none : View p a
none =
    fromHtml <| \_ _ _ -> Html.text ""


{-| An alias for `div [] []`.
-}
empty : View NoPadding a
empty =
    div [] []


{-| An alias for `\node -> lift node [] []`.
-}
emptyNode : (List (Attribute msg) -> List (Html msg) -> Html msg) -> View NoPadding msg
emptyNode node =
    lift node [] []


{-| -}
setMixin : Mixin msg -> View NoPadding msg -> View NoPadding msg
setMixin appearance (View f) =
    View <|
        \p layout extra ->
            f p layout (Mixin.batch [ appearance, extra ])


{-| -}
setMixins : List (Mixin msg) -> View NoPadding msg -> View NoPadding msg
setMixins =
    setMixin << Mixin.batch


{-| -}
setAttribute : Attribute msg -> View NoPadding msg -> View NoPadding msg
setAttribute =
    setMixin << Mixin.fromAttribute


{-| -}
setAttributes : List (Attribute msg) -> View NoPadding msg -> View NoPadding msg
setAttributes =
    setMixin << Mixin.fromAttributes


{-| -}
setLayout : Layout msg -> View p msg -> View p msg
setLayout layout (View f) =
    View <|
        \p extra appearance ->
            f p (Layout.batch [ layout, extra ]) appearance


modifyPadding : (Float -> Float) -> View p1 msg -> View p2 msg
modifyPadding g (View f) =
    View <|
        \p -> f (g p)



-- Alternatives to Html nodes


{-| `View` version of `Html.div`.
-}
div : List (Mixin msg) -> List (View p msg) -> View p msg
div =
    lift Html.div


{-| `View` version of `\str -> Html.div [] [ Html.text str ]`.
-}
text : String -> View NoPadding msg
text str =
    lift Html.div
        []
        [ fromHtml <| \_ _ _ -> Html.text str
        ]


{-| -}
keyed :
    String
    -> List (Mixin msg)
    -> List ( String, View p msg )
    -> View p msg
keyed tag mixins children =
    liftHelper Mixin.none (Keyed.node tag) mixins <|
        List.map (Tuple.mapSecond (toHtml 0 Layout.none Mixin.none)) children


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

    child_ : Int -> Html (Protected p msg)
    child_ =
        toProtected << child

    v : List Int -> View p msg
    v =
        optimized
            String.fromInt
            (lazy child_)
            "div"
            []

-}
optimized :
    (x -> String)
    -> (x -> Html (Protected p msg))
    -> String
    -> List (Mixin msg)
    -> List x
    -> View p msg
optimized identifier f tag mixins ls =
    liftHelper Mixin.none (Keyed.node tag) mixins <|
        List.map (\x -> ( identifier x, Html.map unProtect <| f x )) ls


{-| This is supposed to be used in order to make `Html.Lazy.lazyN` work.
See `optimized` for real usage.
-}
toProtected : View p a -> Html (Protected p a)
toProtected v =
    Html.map Protected <| toHtml 0 Layout.none Mixin.none v


{-| -}
type Protected p a
    = Protected a


unProtect : Protected p a -> a
unProtect (Protected a) =
    a



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
    modifyPadding (newPadding p1 p2) child


newPadding : IsPadding p1 -> IsPadding p2 -> Float -> Float
newPadding (IsPadding c1) (IsPadding c2) curr =
    ((c2.rem - c1.rem) / 2) + curr



-- Boundary


{-| Set boundary to convert into `NoPadding`.
-}
setBoundary : IsPadding p -> View p msg -> View NoPadding msg
setBoundary config child =
    liftHelper (innerPadding config)
        Html.div
        []
        [ toHtml 0
            (Layout.fromRecord
                { inner = Mixin.none
                , outer = Mixin.fromAttribute <| Attributes.style "height" "100%"
                }
            )
            Mixin.none
            child
        ]


innerPadding : IsPadding p -> Mixin msg
innerPadding (IsPadding { rem }) =
    Mixin.fromAttribute <|
        Attributes.style "padding" <|
            String.fromFloat (rem / 2)
                ++ "rem"



-- Unsafe functions


{-| -}
unsafeFromHtml : Html msg -> View p msg
unsafeFromHtml h =
    div []
        [ fromHtml <| \_ _ _ -> h
        ]



-- Helper functions


noPadding : IsPadding NoPadding
noPadding =
    IsPadding
        { rem = 0
        }


toHtml : Float -> Layout msg -> Mixin msg -> View padding msg -> Html msg
toHtml p layout appearance (View f) =
    f p layout appearance


fromHtml : (Float -> Layout msg -> Mixin msg -> Html msg) -> View padding msg
fromHtml =
    View


liftHelper : Mixin msg -> (List (Attribute msg) -> List a -> Html msg) -> List (Mixin msg) -> List a -> View p msg
liftHelper special node appearances children =
    fromHtml <|
        \pad (Layout layout_) extra ->
            node
                (Mixin.toAttributes <|
                    Mixin.batch
                        [ Mixin.batch appearances
                        , extra
                        , prohibited
                        , special
                        , if pad == 0 then
                            layout_.outer

                          else
                            layout_.inner
                        ]
                )
                children
                |> wrapHtml pad layout_.outer


prohibited : Mixin mgs
prohibited =
    Mixin.fromAttributes
        [ Attributes.style "padding" "0"
        , Attributes.style "margin" "0"
        ]


wrapHtml : Float -> Mixin msg -> Html msg -> Html msg
wrapHtml pad outer c =
    if pad == 0 then
        c

    else
        Html.div
            ((Attributes.style "padding" <| String.fromFloat pad ++ "rem") :: Mixin.toAttributes outer)
            [ c
            ]
