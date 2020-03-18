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
    , NoGap
    , IsGap(..)
    , fromNoGap
    , expand
    , setBoundary
    , setBoundaryWith
    , setRole
    , setAria
    , setBoolAria
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

@docs NoGap


# Custom gaps

You can introduce custom gaps by just declaring their types and `IsGap` values.

    type MyGap
        = MyGap

    myGap : IsGap MyGap
    myGap =
        IsGap
            { rem = 0.6
            }

@docs IsGap


# Convert between gaps

@docs fromNoGap
@docs expand
@docs setBoundary
@docs setBoundaryWith


# Accessibility

@docs setRole
@docs setAria
@docs setBoolAria


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


{-| Html alternative that has type level gap.
-}
type View gap msg
    = View (Float -> Layout msg -> Mixin msg -> Html msg)



-- `Browser.*` alternatives


{-| Alternative to `Browser.sandbox`.
This inserts following CSS.

    <style>
    *,
    *::before,
    *::after {
      box-sizing: border-box;
    }
    [data-elm-neat-layout~=flex] {
      display:-ms-flexbox;
      display:flex
    }
    </style>

-}
sandbox :
    { init : model
    , view : model -> View NoGap msg
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
                    [ Attributes.style "padding" "0"
                    , Attributes.style "margin" "0"
                    ]
                    [ resetCss
                    , toHtml 0 Layout.none Mixin.none (o.view model)
                    ]
        }


{-| Alternative to `Browser.element`.
This also inserts following style tag.

    <style>
    *,
    *::before,
    *::after {
      box-sizing: border-box;
    }
    [data-elm-neat-layout~=flex] {
      display:-ms-flexbox;
      display:flex
    }
    </style>

-}
element :
    { init : flags -> ( model, Cmd msg )
    , view : model -> View NoGap msg
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
                    [ Attributes.style "padding" "0"
                    , Attributes.style "margin" "0"
                    ]
                    [ resetCss
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
      box-sizing: border-box;
    }
    [data-elm-neat-layout~=flex] {
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


{-| Alternative to `Browser.Document`.
-}
type alias Document msg =
    { title : String
    , body : View NoGap msg
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


resetCss : Html msg
resetCss =
    Html.node "style"
        []
        [ Html.text <|
            """
*,
*::before,
*::after {
  box-sizing: border-box;
}
[data-elm-neat-layout~=flex] {
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
    import Neat.Gap as Gap exposing (NoGap)

    view1 : View NoGap msg
    view1 =
        Neat.div
            []
            [ Neat.text "view1"
            ]

    view2 : View NoGap msg
    view2 =
        Neat.div
            []
            [ Neat.text "view2"
            ]

    composed : View NoGap msg
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
lift_ allowGap node appearances children =
    liftHelper allowGap Mixin.none node appearances <|
        List.map (toHtml 0 Layout.none Mixin.none) children


{-| View version of `Html.text ""`.
-}
none : View p a
none =
    fromHtml <| \_ _ _ -> Html.text ""


{-| An alias for `div [] []`.
-}
empty : View NoGap a
empty =
    div [] []


{-| An alias for `\node -> lift node [] []`.
-}
emptyNode : (List (Attribute msg) -> List (Html msg) -> Html msg) -> View NoGap msg
emptyNode node =
    lift node [] []


{-| -}
setMixin : Mixin msg -> View NoGap msg -> View NoGap msg
setMixin =
    setMixinHelper


setMixinHelper : Mixin msg -> View p msg -> View p msg
setMixinHelper appearance (View f) =
    View <|
        \p layout extra ->
            f p layout (Mixin.batch [ appearance, extra ])


{-| -}
setMixins : List (Mixin msg) -> View NoGap msg -> View NoGap msg
setMixins =
    setMixin << Mixin.batch


{-| -}
setAttribute : Attribute msg -> View NoGap msg -> View NoGap msg
setAttribute =
    setMixin << Mixin.fromAttribute


{-| -}
setAttributes : List (Attribute msg) -> View NoGap msg -> View NoGap msg
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


modifyGap : (Float -> Float) -> View p1 msg -> View p2 msg
modifyGap g (View f) =
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
textNode : (List (Attribute msg) -> List (Html msg) -> Html msg) -> String -> View NoGap msg
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
textBlock : String -> View NoGap msg
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


{-| Primitive type representing no gap on a view.
-}
type NoGap
    = NoGap


{-| Information about your custom gaps.

  - rem : gap width in units of `rem`

-}
type IsGap p
    = IsGap
        { rem : Float
        }



-- Convert between gaps


{-| -}
fromNoGap : IsGap p -> View NoGap msg -> View p msg
fromNoGap =
    expand noGap


{-| Expand gap from `p1` to `p2`.
The width of `p2` is supposed to be greater than `p1`.
-}
expand : IsGap p1 -> IsGap p2 -> View p1 msg -> View p2 msg
expand p1 p2 child =
    modifyGap (newGap p1 p2) child


newGap : IsGap p1 -> IsGap p2 -> Float -> Float
newGap (IsGap c1) (IsGap c2) curr =
    ((c2.rem - c1.rem) / 2) + curr



-- Boundary


{-| Wrap a view with boundary without gap.

    This is an alias for `setBoundary defaultBoundary`.

-}
setBoundary : IsGap p -> View p msg -> View NoGap msg
setBoundary =
    setBoundaryWith Boundary.defaultBoundary


{-| Set boundary to convert into `NoGap`.
-}
setBoundaryWith : Boundary -> IsGap p -> View p msg -> View NoGap msg
setBoundaryWith align config child =
    liftHelper False
        (innerGap config)
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


innerGap : IsGap p -> Mixin msg
innerGap (IsGap { rem }) =
    Mixin.fromAttribute <|
        Attributes.style "padding" <|
            String.fromFloat (rem / 2)
                ++ "rem"



-- Helper functions


div : List (Mixin msg) -> List (View p msg) -> View p msg
div =
    lift Html.div


noGap : IsGap NoGap
noGap =
    IsGap
        { rem = 0
        }


toHtml : Float -> Layout msg -> Mixin msg -> View gap msg -> Html msg
toHtml p layout appearance (View f) =
    f p layout appearance


fromHtml : (Float -> Layout msg -> Mixin msg -> Html msg) -> View gap msg
fromHtml =
    View


liftHelper : Bool -> Mixin msg -> (List (Attribute msg) -> List a -> Html msg) -> List (Mixin msg) -> List a -> View p msg
liftHelper allowGap special node appearances children =
    fromHtml <|
        \pad (Layout layout_) extra ->
            node
                (Mixin.toAttributes <|
                    Mixin.batch
                        [ Mixin.batch appearances
                        , extra
                        , prohibited allowGap
                        , special
                        , if pad == 0 then
                            layout_.outer

                          else
                            layout_.inner
                        ]
                )
                children
                |> wrapHtml pad
                    (Mixin.batch
                        [ prohibited allowGap
                        , layout_.outer
                        ]
                    )


prohibited : Bool -> Mixin mgs
prohibited allowGap =
    Mixin.fromAttributes <|
        if allowGap then
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
            (Mixin.toAttributes outer
                ++ [ Attributes.style "padding" <| String.fromFloat pad ++ "rem" ]
            )
            [ c
            ]
