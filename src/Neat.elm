module Neat exposing
    ( View
    , sandbox
    , element
    , document
    , Document
    , application
    , lift
    , textBlock
    , textNode
    , none
    , empty
    , emptyNode
    , input
    , textarea
    , select
    , setMixin
    , setMixins
    , setAttribute
    , setAttributes
    , setLayout
    , keyed
    , NoPadding
    , IsPadding(..)
    , fromNoPadding
    , expand
    , setBoundary
    , setBoundaryWith
    , setRole
    , setAria
    , setBoolAria
    , unsafeFromHtml
    , optimized
    , toProtected
    , Protected
    )

{-| Main module for elm-neat-layout.


# Core

@docs View


# `Browser.*` alternatives

@docs sandbox
@docs element
@docs document
@docs Document
@docs application


# Constructors

@docs lift
@docs textBlock
@docs textNode
@docs none
@docs empty
@docs emptyNode


# Special nodes

@docs input
@docs textarea
@docs select


# Modifiers

@docs setMixin
@docs setMixins
@docs setAttribute
@docs setAttributes
@docs setLayout


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
@docs setBoundaryWith


# Accessibility

@docs setRole
@docs setAria
@docs setBoolAria


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

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Keyed as Keyed
import Mixin exposing (Mixin)
import Neat.Boundary as Boundary exposing (Boundary)
import Neat.Flex as Flex exposing (Flex)
import Neat.Layout.Internal as Layout exposing (Layout(..))
import Url exposing (Url)



-- Core


{-| Html alternative that has type level padding.
-}
type View padding msg
    = View (Float -> Layout msg -> Mixin msg -> Html msg)



-- `Browser.*` alternatives


{-| Alternative to `Browser.sandbox`.
This inserts following CSS.

    <style>
    .elm-neat-layout-root,
    .elm-neat-layout-root::before,
    .elm-neat-layout-root::after,
    .elm-neat-layout-root *,
    .elm-neat-layout-root *::before,
    .elm-neat-layout-root *::after {
      margin: 0;
      padding: 0;
      box-sizing: border-box;
    }
    [data-elm-neat-layout~=flex]{
        display:-ms-flexbox;
        display:flex
    }
    </style>

-}
sandbox :
    { init : model
    , view : model -> View NoPadding msg
    , update : msg -> model -> model
    }
    -> Program () model msg
sandbox o =
    Browser.sandbox
        { init = o.init
        , update = o.update
        , view =
            \model ->
                Html.div
                    [ Attributes.class "elm-neat-layout-root" ]
                    [ resetCssScoped
                    , toHtml 0 Layout.none Mixin.none (o.view model)
                    ]
        }


{-| Alternative to `Browser.element`.
This also inserts following style tag.

    <style>
    .elm-neat-layout-root,
    .elm-neat-layout-root::before,
    .elm-neat-layout-root::after,
    .elm-neat-layout-root *,
    .elm-neat-layout-root *::before,
    .elm-neat-layout-root *::after {
      margin: 0;
      padding: 0;
      box-sizing: border-box;
    }
    [data-elm-neat-layout~=flex]{
        display:-ms-flexbox;
        display:flex
    }
    </style>

-}
element :
    { init : flags -> ( model, Cmd msg )
    , view : model -> View NoPadding msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> Program flags model msg
element o =
    Browser.element
        { init = o.init
        , view =
            \model ->
                Html.div
                    [ Attributes.class "elm-neat-layout-root" ]
                    [ resetCssScoped
                    , toHtml 0 Layout.none Mixin.none (o.view model)
                    ]
        , update = o.update
        , subscriptions = o.subscriptions
        }


{-| Alternative to `Browser.document`.
This also inserts following style tag.

    <style>
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
    </style>

-}
document :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> Program flags model msg
document o =
    Browser.document
        { init = o.init
        , view =
            \model ->
                let
                    view =
                        o.view model
                in
                { title = view.title
                , body =
                    [ resetCss
                    , toHtml 0 Layout.none Mixin.none view.body
                    ]
                }
        , update = o.update
        , subscriptions = o.subscriptions
        }


type alias Document msg =
    { title : String
    , body : View NoPadding msg
    }


{-| Alternative to `Browser.application`.
This also inserts following style tag.

    <style>
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
    </style>

-}
application :
    { init : flags -> Url -> Key -> ( model, Cmd msg )
    , view : model -> Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , onUrlRequest : UrlRequest -> msg
    , onUrlChange : Url -> msg
    }
    -> Program flags model msg
application o =
    Browser.application
        { init = o.init
        , view =
            \model ->
                let
                    view =
                        o.view model
                in
                { title = view.title
                , body =
                    [ resetCss
                    , toHtml 0 Layout.none Mixin.none view.body
                    ]
                }
        , update = o.update
        , subscriptions = o.subscriptions
        , onUrlRequest = o.onUrlRequest
        , onUrlChange = o.onUrlChange
        }


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


resetCssScoped : Html msg
resetCssScoped =
    Html.node "style"
        []
        [ Html.text <|
            """
elm-neat-layout-root,
elm-neat-layout-root::before,
elm-neat-layout-root::after,
elm-neat-layout-root *,
elm-neat-layout-root *::before,
elm-neat-layout-root *::after {
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
lift =
    lift_ False


lift_ : Bool -> (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Mixin msg) -> List (View p msg) -> View p msg
lift_ allowPadding node appearances children =
    liftHelper allowPadding Mixin.none node appearances <|
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
setMixin =
    setMixinHelper


setMixinHelper : Mixin msg -> View p msg -> View p msg
setMixinHelper appearance (View f) =
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



-- Accessibility


{-| Set "role" value for WAI-ARIA.
-}
setRole : String -> View p msg -> View p msg
setRole v =
    setMixinHelper <| Mixin.attribute "role" v


{-| Set "aria-\*" value for WAI-ARIA.

e.g., `setAria "required" "true"` stands for "aria-required" is "true".

-}
setAria : String -> String -> View p msg -> View p msg
setAria name v =
    setMixinHelper <| Mixin.attribute ("aria-" ++ name) v


{-| Set boolean "aria-\*" value for WAI-ARIA.

i.e.,

  - `setBoolAria name True` is equal to `setAria name "true"`
  - `setBoolAria name False` is equal to `setAria name "false"`

-}
setBoolAria : String -> Bool -> View p msg -> View p msg
setBoolAria name p =
    setAria name <|
        if p then
            "true"

        else
            "false"


modifyPadding : (Float -> Float) -> View p1 msg -> View p2 msg
modifyPadding g (View f) =
    View <|
        \p -> f (g p)



-- Special Html nodes


{-| `View` version of `Html.select`.
Differ from `lift Html.select`, it can accept "padding" style property.
-}
select : List (Mixin msg) -> List (View p msg) -> View p msg
select =
    lift_ True Html.select


{-| `View` version of `Html.input`.
Differ from `lift Html.input [] []`, it can accept "padding" style property.
-}
input : View p msg
input =
    lift_ True Html.input [] []


{-| `View` version of `Html.textarea`.
Differ from `lift Html.textarea [] []`, it can accept "padding" style property.
-}
textarea : View p msg
textarea =
    lift_ True Html.textarea [] []


{-| Create a View which only contains a text node.

`textNode Html.option "Item1"` is equivalent to `Html.option [] [ Html.text "Item1" ]` in Html world.

-}
textNode : (List (Attribute msg) -> List (Html msg) -> Html msg) -> String -> View NoPadding msg
textNode f str =
    lift f
        []
        [ fromHtml <| \_ _ _ -> Html.text str
        ]


{-| Create a text block.

`textBlock "foo"` is equivalent to `Html.div [] [ Html.text "foo" ]` in Html world.

    textBlock =
        textNode Html.div

-}
textBlock : String -> View NoPadding msg
textBlock =
    textNode Html.div


{-| -}
keyed :
    String
    -> List (Mixin msg)
    -> List ( String, View p msg )
    -> View p msg
keyed tag mixins children =
    liftHelper False Mixin.none (Keyed.node tag) mixins <|
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
    liftHelper False Mixin.none (Keyed.node tag) mixins <|
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


{-| Wrap a view with boundary without padding.

    This is an alias for `setBoundary defaultBoundary`.

-}
setBoundary : IsPadding p -> View p msg -> View NoPadding msg
setBoundary =
    setBoundaryWith Boundary.defaultBoundary


{-| Set boundary to convert into `NoPadding`.
-}
setBoundaryWith : Boundary -> IsPadding p -> View p msg -> View NoPadding msg
setBoundaryWith align config child =
    liftHelper False
        (innerPadding config)
        (nodeNameToNode align.nodeName)
        (Flex.rowMixins <| toFlex align)
        [ toHtml 0 (Flex.childLayout <| toFlex align) Mixin.none child
        ]


nodeNameToNode : String -> List (Attribute msg) -> List (Html msg) -> Html msg
nodeNameToNode name =
    if name == "div" then
        Html.div

    else
        Html.node name


expandChild : Boundary -> View p msg -> View p msg
expandChild align =
    setLayout <| Flex.childLayout <| toFlex align


toFlex : Boundary -> Flex
toFlex align =
    { vertical = toFlexVertical align.vertical
    , horizontal = toFlexHorizontal align.horizontal
    , wrap = False
    }


toFlexHorizontal : Boundary.Horizontal -> Flex.Horizontal
toFlexHorizontal align =
    case align of
        Boundary.Left ->
            Flex.Left

        Boundary.Right ->
            Flex.Right

        Boundary.HCenter ->
            Flex.HCenter

        Boundary.HStretch ->
            Flex.HStretch


toFlexVertical : Boundary.Vertical -> Flex.Vertical
toFlexVertical align =
    case align of
        Boundary.Top ->
            Flex.Top

        Boundary.Bottom ->
            Flex.Bottom

        Boundary.VCenter ->
            Flex.VCenter

        Boundary.VStretch ->
            Flex.VStretch


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


div : List (Mixin msg) -> List (View p msg) -> View p msg
div =
    lift Html.div


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


liftHelper : Bool -> Mixin msg -> (List (Attribute msg) -> List a -> Html msg) -> List (Mixin msg) -> List a -> View p msg
liftHelper allowPadding special node appearances children =
    fromHtml <|
        \pad (Layout layout_) extra ->
            node
                (Mixin.toAttributes <|
                    Mixin.batch
                        [ Mixin.batch appearances
                        , extra
                        , prohibited allowPadding
                        , special
                        , if pad == 0 then
                            layout_.outer

                          else
                            layout_.inner
                        ]
                )
                children
                |> wrapHtml pad layout_.outer


prohibited : Bool -> Mixin mgs
prohibited allowPadding =
    Mixin.fromAttributes <|
        if allowPadding then
            [ Attributes.style "margin" "0"
            ]

        else
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
