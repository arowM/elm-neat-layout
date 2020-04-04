module Neat exposing
    ( View
    , sandbox
    , Renderer
    , defaultRenderer
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
    , node
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
    , when
    , unless
    )

{-| Main module for elm-neat-layout.


# Core

@docs View


# `Browser.*` alternatives

@docs sandbox
@docs Renderer
@docs defaultRenderer
@docs element
@docs document
@docs Document
@docs application


# Constructors

@docs node
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

# Handle cases

@docs when
@docs unless

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
            { width = 0.6
            , height = 0.6
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
    = View (Renderer -> Gap -> ViewElem gap msg)

type ViewElem gap msg
    = Node HasInnerPadding NodeName (Layout msg) (Mixin msg) (List (ViewElem gap msg))
    | Text String

type alias NodeName = String

type alias HasInnerPadding = Bool


type alias Gap =
    { width : Float
    , height : Float
    }


emptyGap : Gap
emptyGap =
    { width = 0
    , height = 0
    }


fromViewElem : (Renderer -> Gap -> ViewElem gap msg) -> View gap msg
fromViewElem = View

toHtml : Renderer -> Gap -> View gap msg -> Html msg
toHtml renderer gap (View view) =
    case view renderer gap of
        Node hasInnerPadding nodeName layout appearance children ->
            nodeToHtml hasInnerPadding nodeName layout appearance
                (List.map (toHtml renderer gap) children)
        Text str ->
            Html.text str


nodeToHtml : Bool -> NodeName -> Layout msg -> Mixin msg -> List (Html msg) -> Html msg
nodeToHtml hasInnerPadding nodeName layout appearance children =
            Html.node
                name
                (Mixin.toAttributes <|
                    Mixin.batch
                        [ appearance
                        , prohibited hasInnerPadding
                        , if gap.width == 0 && gap.height == 0 then
                            layout_.outer

                          else
                            layout_.inner
                        ]
                )
                children
                |> wrapHtml renderer
                    gap
                    (Mixin.batch
                        [ prohibited hasInnerPadding
                        , layout_.outer
                        ]
                    )


prohibited : Bool -> Mixin mgs
prohibited hasInnerPadding =
    Mixin.batch
        [ style "margin" "0"
        , Mixin.unless hasInnerPadding <| style "padding" "0"
        ]


wrapHtml : Renderer -> Gap -> Mixin msg -> Html msg -> Html msg
wrapHtml renderer gap outer c =
    if gap.width == 0 && gap.height == 0 then
        c

    else
        Html.div
            (Mixin.toAttributes outer
                ++ [ Attributes.style "padding-right" <|
                        "calc("
                            ++ String.fromFloat gap.width
                            ++ " * "
                            ++ renderer.baseGapSize
                            ++ ")"
                   , Attributes.style "padding-left" <|
                        "calc("
                            ++ String.fromFloat gap.width
                            ++ " * "
                            ++ renderer.baseGapSize
                            ++ ")"
                   , Attributes.style "padding-top" <|
                        "calc("
                            ++ String.fromFloat gap.height
                            ++ " * "
                            ++ renderer.baseGapSize
                            ++ ")"
                   , Attributes.style "padding-bottom" <|
                        "calc("
                            ++ String.fromFloat gap.height
                            ++ " * "
                            ++ renderer.baseGapSize
                            ++ ")"
                   ]
            )
            [ c
            ]


map : (a -> b) -> View g a -> View g b
map f (View view) =
    View <| \r g -> case view r g of
        Node b name renderer gap layout style children ->
            Node b name renderer gap (Layout.map f layout) (Mixin.map f style) (List.map (map f) children)
        Text str ->
            Text str


{-
liftHelper : Bool -> (Renderer -> Mixin msg) -> (List (Attribute msg) -> List a -> Html msg) -> List (Mixin msg) -> (Renderer -> List a) -> View g msg
liftHelper allowGap special node appearances children =
    fromHtml <|
        \renderer gap (Layout _ layout_) extra ->
            node
                (Mixin.toAttributes <|
                    Mixin.batch
                        [ Mixin.batch appearances
                        , extra
                        , prohibited allowGap
                        , special renderer
                        , if gap.width == 0 && gap.height == 0 then
                            layout_.outer

                          else
                            layout_.inner
                        ]
                )
                (children renderer)
                |> wrapHtml renderer
                    gap
                    (Mixin.batch
                        [ prohibited allowGap
                        , layout_.outer
                        ]
                    )
-}







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
    , renderer : Renderer
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
                    , toHtml o.renderer emptyGap Layout.none Mixin.none (o.view model)
                    ]
        }


{-| Settings for rendering `View`.

The `baseGapSize` is base gap size in [<length> CSS data type](https://developer.mozilla.org/en-US/docs/Web/CSS/length).
All gap sizes are determined relative to this value.
e.g., `"16px"`, `"1.2rem"`, ...
The "em" unit is not recommended because it varies with parent node font-size of each gap.

-}
type alias Renderer =
    { baseGapSize : String
    }


{-|

    defaultRenderer =
        { baseGapSize = "1rem"
        }

-}
defaultRenderer : Renderer
defaultRenderer =
    { baseGapSize = "1rem"
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
    , renderer : model -> Renderer
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
                    , toHtml (o.renderer model) emptyGap Layout.none Mixin.none (o.view model)
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
    , renderer : model -> Renderer
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
                    , toHtml (o.renderer model) emptyGap Layout.none Mixin.none view.body
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
    , renderer : model -> Renderer
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
                    , toHtml (o.renderer model) emptyGap Layout.none Mixin.none view.body
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

{-| General way to create `View` nodes. It is equivalent to `Html.node`.

    node "div"
        [ Mixin.id "foo"
        ]
        [ node "button"
            [ Mixin.attribute "type" "button"
            ]
            []
        ]

-}
node : String -> List (Mixin msg) -> List (View gap msg) -> View gap msg
node =
    node_ False


node_ : HasInnerPadding -> NodeName -> List (Mixin msg) -> List (View gap msg) -> View gap msg
node_ hasInnerPadding nodeName mixins children =
    fromViewElem <| \renderer gap ->
        Node hasInnerPadding nodeName Layout.none (Mixin.batch mixins) <| List.map (\(View view) -> fromViewElem view) children


{-| Put plain text wrapped in a node. It will escape the string so that it appears exactly as you specify.

    textNode "span" "foo bar baz"

The example above is equivalent to `Html.span [] [ Html.text "foo bar baz" ]`.

-}
textNode : String -> String -> View NoGap msg
textNode nodeName str =
    node nodeName []
        [ fromViewElem <| \_ _ -> Text ""
        ]


{-| Alias for `textNode "div".
-}
textBlock : String -> View NoGap msg
textBlock =
    textNode "div"

{-| Equivalent to `Html.text ""`.
-}
none : View g a
none =
    fromViewElem <| \_ _ -> Text ""


{-| Alias for `node "div" [] []`.
-}
empty : View NoGap a
empty =
    div [] []


{-| Alias for `\nodeName -> node nodeName [] []`.
-}
emptyNode : NodeName -> View NoGap msg
emptyNode node =
     node [] []


{-| Overwrite `Mixin` for a `View`.
-}
setMixin : Mixin msg -> View NoGap msg -> View NoGap msg
setMixin =
    setMixinHelper


setMixinHelper : Mixin msg -> View g msg -> View g msg
setMixinHelper appearance (View f) =
    View <|
        \renderer gap layout extra ->
            f renderer gap layout (Mixin.batch [ appearance, extra ])


{-| Overwrite `Mixin`s for a `View`. -}
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
setLayout : Layout msg -> View g msg -> View g msg
setLayout layout (View view) =
    fromViewElem <| \renderer gap ->
        case view renderer gap of
            Text str -> Text str
            Node b nodeName layout appearance children ->
                Node b
                    nodeName
                    (if Layout.isImportant layout
                        then
                            Layout.batch [ extra, layout ]
                        else
                            Layout.batch [ layout, extra ]
                    )
                    appearance
                    children



-- Accessibility


{-| Set "role" value for WAI-ARIA.
-}
setRole : String -> View g msg -> View g msg
setRole v =
    setMixinHelper <| Mixin.attribute "role" v


{-| Set "aria-\*" value for WAI-ARIA.

e.g., `setAria "required" "true"` stands for "aria-required" is "true".

-}
setAria : String -> String -> View g msg -> View g msg
setAria name v =
    setMixinHelper <| Mixin.attribute ("aria-" ++ name) v


{-| Set boolean "aria-\*" value for WAI-ARIA.

i.e.,

  - `setBoolAria name True` is equal to `setAria name "true"`
  - `setBoolAria name False` is equal to `setAria name "false"`

-}
setBoolAria : String -> Bool -> View g msg -> View g msg
setBoolAria name g =
    setAria name <|
        if g then
            "true"

        else
            "false"


modifyGap : (Gap -> Gap) -> View g1 msg -> View g2 msg
modifyGap g (View view) =
    fromViewElem <|
        \r gap -> view r (g gap)



-- Special Html nodes


{-| `View` version of `Html.select`.
Differ from `node "select"`, it can accept "padding" property.
-}
select : List (Mixin msg) -> List (View g msg) -> View g msg
select =
    node_ True "select"


{-| `View` version of `Html.input`.
Differ from `node "input" [] []`, it can accept "padding" property.
-}
input : View g msg
input =
    node_ True "input" [] []


{-| `View` version of `Html.textarea`.
Differ from `node "textarea" [] []`, it can accept "padding" property.
-}
textarea : View g msg
textarea =
    node_ True "textarea" [] []


{-| -}
keyed :
    String
    -> List (Mixin msg)
    -> List ( String, View g msg )
    -> View g msg
keyed tag mixins children =
    fromViewElem <| \renderer gap ->

    liftHelper False (\_ -> Mixin.none) (Keyed.node tag) mixins <|
        \r ->
            List.map (Tuple.mapSecond (toHtml r emptyGap Layout.none Mixin.none)) children


type View x gap msg =
    View ((x -> msg) -> Renderer -> Gap -> List (Attribute x) -> List (Html x) -> Html x)

    = View (Renderer -> Gap -> ViewElem gap msg)

type ViewElem gap msg
    = Node HasInnerPadding NodeName (Layout msg) (Mixin msg) (List (ViewElem gap msg))
    | Text String


keyed tag mixins children =
    liftHelper False (\_ -> Mixin.none) (Keyed.node tag) mixins <|
        \r ->
            List.map (Tuple.mapSecond (toHtml r emptyGap Layout.none Mixin.none)) children


{-|

    v1 : View g msg
    v1 =
        Debug.todo "v1"

    v2 : View g msg
    v2 =
        Debug.todo "v2"

    child : Int -> View g msg
    child _ =
        Debug.todo "child"

    child_ : Int -> Html (Protected g msg)
    child_ =
        toProtected << child

    v : List Int -> View g msg
    v =
        optimized
            String.fromInt
            (lazy child_)
            "div"
            []

-}
optimized :
    (x -> String)
    -> (x -> Protected g Renderer -> Html (Protected g msg))
    -> String
    -> List (Mixin msg)
    -> List x
    -> View g msg
optimized identifier f tag mixins ls =
    liftHelper False (\_ -> Mixin.none) (Keyed.node tag) mixins <|
        \r ->
            List.map (\x -> ( identifier x, Html.map unProtect <| f x (Protected r) )) ls


{-| This is supposed to be used in order to make `Html.Lazy.lazyN` work.
See `optimized` for real usage.
-}
toProtected : View g a -> Protected g Renderer -> Html (Protected g a)
toProtected v (Protected renderer) =
    Html.map Protected <| toHtml renderer emptyGap Layout.none Mixin.none v


{-| -}
type Protected g a
    = Protected a


unProtect : Protected g a -> a
unProtect (Protected a) =
    a



-- Primitives


{-| Primitive type representing no gap
-}
type NoGap
    = NoGap


{-| Information about your custom gaps.

  - width : gap width relative to `baseGapSize` of `Renderer`
  - height : gap height relative to `baseGapSize` of `Renderer`

The `Renderer` value is given to `sandbox`, `element`, `document`, `application`.

e.g., When `baseGapSize` is `"2rem"`, `IsGap { width = 1.2, height = 2 }` becomes gap with `"2.4rem"` width and `"4rem"` height.

-}
type IsGap p
    = IsGap
        { width : Float
        , height : Float
        }



-- Convert between gaps


{-| -}
fromNoGap : IsGap g -> View NoGap msg -> View g msg
fromNoGap =
    expand noGap


{-| Expand gap from `g1` to `g2`.
Both the width and the height of `g2` is supposed to be greater than `g1`.
-}
expand : IsGap g1 -> IsGap g2 -> View g1 msg -> View g2 msg
expand g1 g2 child =
    modifyGap (newGap g1 g2) child


newGap : IsGap g1 -> IsGap g2 -> Gap -> Gap
newGap (IsGap c1) (IsGap c2) curr =
    { width = ((c2.width - c1.width) / 2) + curr.width
    , height = ((c2.height - c1.height) / 2) + curr.height
    }



-- Boundary


{-| Wrap a view with boundary without gap.

    This is an alias for `setBoundary defaultBoundary`.

-}
setBoundary : IsGap g -> View g msg -> View NoGap msg
setBoundary =
    setBoundaryWith Boundary.defaultBoundary


{-| Set boundary to convert into `NoGap`.
-}
setBoundaryWith : Boundary -> IsGap g -> View g msg -> View NoGap msg
setBoundaryWith align config child =
    lift (nodeNameToNode align.nodeName)
        []
        [ liftHelper False
            (\r -> innerGap r config)
            Html.div
            -- DO NOT directly put `(nodeNameToNode align.nodeName)` here.
            -- It causes unnatural dashed borders for focused tabindexed nodes such as "button" node.
            [ Mixin.batch <| Flex.rowMixins <| toFlex align
            , style "width" "100%"
            , style "height" "100%"
            ]
          <|
            \r ->
                [ toHtml r emptyGap (Flex.childLayout <| toFlex align) Mixin.none child
                ]
        ]


nodeNameToNode : String -> List (Attribute msg) -> List (Html msg) -> Html msg
nodeNameToNode name =
    if name == "div" then
        Html.div

    else
        Html.node name


toFlex : Boundary -> Flex
toFlex align =
    { vertical = toFlexVertical align.vertical
    , horizontal = toFlexHorizontal align.horizontal
    , wrap = Flex.NoWrap
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


innerGap : Renderer -> IsGap g -> Mixin msg
innerGap renderer (IsGap { width, height }) =
    Mixin.fromAttributes
        [ Attributes.style "padding-right" <|
            "calc("
                ++ String.fromFloat (width / 2)
                ++ " * "
                ++ renderer.baseGapSize
                ++ ")"
        , Attributes.style "padding-left" <|
            "calc("
                ++ String.fromFloat (width / 2)
                ++ " * "
                ++ renderer.baseGapSize
                ++ ")"
        , Attributes.style "padding-top" <|
            "calc("
                ++ String.fromFloat (height / 2)
                ++ " * "
                ++ renderer.baseGapSize
                ++ ")"
        , Attributes.style "padding-bottom" <|
            "calc("
                ++ String.fromFloat (height / 2)
                ++ " * "
                ++ renderer.baseGapSize
                ++ ")"
        ]


-- Handle cases


{-| Insert a `View` only when a condition is met.
-}
when : Bool -> View p msg -> View p msg
when p v =
    if p then v else none


{-| Insert a `View` unless a condition is met.
-}
unless : Bool -> View p msg -> View p msg
unless p =
    when <| not p


-- Helper functions


div : List (Mixin msg) -> List (View g msg) -> View g msg
div =
    lift Html.div


noGap : IsGap NoGap
noGap =
    IsGap
        { width = 0
        , height = 0
        }


style : String -> String -> Mixin msg
style k v =
    Mixin.fromAttribute <| Attributes.style k v
