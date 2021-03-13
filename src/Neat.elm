module Neat exposing
    ( View
    , map
    , NoGap
    , Renderer
    , defaultRenderer
    , setBaseSizeInRem
    , sandbox
    , Model
    , Msg
    , element
    , document
    , Document
    , application
    , textBlock
    , empty
    , none
    , scalableBlock
    , setMixin
    , setMixins
    , setAttribute
    , setAttributes
    , setRole
    , setAria
    , setBoolAria
    , setLineHeight
    , setMinWidthContain
    , setMinWidthInBs
    , setMinWidthInEm
    , setMinWidthInRem
    , setMinHeightContain
    , setMinHeightInBs
    , setMinHeightInEm
    , setMinHeightInRem
    , setMaxWidthInfinite
    , setMaxWidthFit
    , setMaxWidthInBs
    , setMaxWidthInEm
    , setMaxWidthInRem
    , setMaxHeightInfinite
    , setMaxHeightFit
    , setMaxHeightInBs
    , setMaxHeightInEm
    , setMaxHeightInRem
    , enableWrap
    , disableWrap
    , setHorizontalSpaceBehind
    , setHorizontalSpaceForward
    , setHorizontalSpaceBoth
    , setHorizontalSpaceAround
    , setHorizontalSpaceBetween
    , setVerticalSpaceBehind
    , setVerticalSpaceForward
    , setVerticalSpaceBoth
    , setVerticalSpaceAround
    , setVerticalSpaceBetween
    , setGap
    , setBoundary
    , row
    , keyedRow
    , setRow
    , column
    , keyedColumn
    , setColumn
    , putLayer
    , Layer
    , defaultLayer
    , InLayer
    , toLayerView
    , when
    , unless
    , IsGap(..)
    , Gap
    , setNodeName
    , toTemporaryHtml
    , setBoundaryToTemporaryHtml
    )

{-| Main module for elm-neat-layout.


# What's still missing for ver. 3

* add setMaxWidthCover and setMaxHeightCover
* make default width and height of views _cover_
* implement debug mode
* add withWindowSize function
* add functions to handle borders


# Core

@docs View
@docs map
@docs NoGap


# Renderer

@docs Renderer
@docs defaultRenderer
@docs setBaseSizeInRem


# `Browser.*` alternatives

@docs sandbox
@docs Model
@docs Msg
@docs element
@docs document
@docs Document
@docs application


# Primitive constructors

@docs textBlock
@docs empty
@docs none
@docs scalableBlock


# Attributes

Functions to set arbitrary attributes.
The following styles are not reflected by the `style` attribute or CSS files.

  - padding / margin
      - Use functions for gaps.
  - min-width / max-width / min-height / max-height / line-height
      - Use functions for sizes.
  - width / height
      - Use min-\* and max-\*.
  - white-space
      - Determined by wrapping style
  - display / position
      - Determined automatically by its context
  - z-index
      - Enforced to be "auto"
  - box-sizing
      - Determined by each type of view.
          - `scalableBlock` is "content-box"
          - otherwise, "border-box"

@docs setMixin
@docs setMixins
@docs setAttribute
@docs setAttributes


# WAI-ARIA

@docs setRole
@docs setAria
@docs setBoolAria


# Sizes

You can only use a limited kind of units.
This may seem inconvenient, but it prevents you to build unmaintainable broken views.


## line height

@docs setLineHeight


## minimum width

@docs setMinWidthContain
@docs setMinWidthInBs
@docs setMinWidthInEm
@docs setMinWidthInRem


## minimum height

@docs setMinHeightContain
@docs setMinHeightInBs
@docs setMinHeightInEm
@docs setMinHeightInRem


## maximum width

@docs setMaxWidthInfinite
@docs setMaxWidthFit
@docs setMaxWidthInBs
@docs setMaxWidthInEm
@docs setMaxWidthInRem


## maximum height

@docs setMaxHeightInfinite
@docs setMaxHeightFit
@docs setMaxHeightInBs
@docs setMaxHeightInEm
@docs setMaxHeightInRem


# Wrap style

@docs enableWrap
@docs disableWrap


# Spacing

@docs setHorizontalSpaceBehind
@docs setHorizontalSpaceForward
@docs setHorizontalSpaceBoth
@docs setHorizontalSpaceAround
@docs setHorizontalSpaceBetween
@docs setVerticalSpaceBehind
@docs setVerticalSpaceForward
@docs setVerticalSpaceBoth
@docs setVerticalSpaceAround
@docs setVerticalSpaceBetween


# Gaps

@docs setGap
@docs setBoundary


# Rows and Columns

@docs row
@docs keyedRow
@docs setRow
@docs column
@docs keyedColumn
@docs setColumn


# Overlay

@docs putLayer
@docs Layer
@docs defaultLayer
@docs InLayer
@docs toLayerView


# Handle conditions

@docs when
@docs unless


# Custom gaps

You can use custom gaps just by declaring new types and `IsGap` values for them.

    import Neat exposing (IsGap)

    type ButtonGroup
        = ButtonGroup

    buttonGroup : IsGap ButtonGroup
    buttonGroup =
        IsGap
            { horizontal = 0.6
            , vertical = 0.6
            }

@docs IsGap
@docs Gap


# Lower level functions for HTML

@docs setNodeName
@docs toTemporaryHtml
@docs setBoundaryToTemporaryHtml

-}

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Keyed as Keyed
import Mixin exposing (Mixin)
import Mixin.Html as Mixin
import Neat.KeyableList as KeyableList exposing (KeyableList)
import Url exposing (Url)



-- Core


{-| A gap-sensible `Html msg` alternative.
-}
type View gap msg
    = View (View_ msg)


{-| Internal view.
Some variants have gaps for caching purpose.
-}
type View_ msg
    = Row
        { childGap : Gap
        , mixin : Mixin msg
        , layout : Layout
        , overlays : Overlays msg
        , children : KeyableList (View_ msg)
        }
    | Column
        { childGap : Gap
        , mixin : Mixin msg
        , layout : Layout
        , overlays : Overlays msg
        , children : KeyableList (View_ msg)
        }
    | TextNode
        { mixin : Mixin msg
        , layout : Layout
        , overlays : Overlays msg
        , text : String
        }
    | Scalable
        { mixin : Mixin msg
        , layout : Layout
        , overlays : Overlays msg
        , rate : Float
        }
    | None
    | Boundary
        { childGap : Gap
        , mixin : Mixin msg
        , layout : Layout
        , overlays : Overlays msg
        , child : View_ msg
        }
    | Rendered
        { childGap : Gap
        , toHtml : Renderer_ -> Html msg
        }


type alias Overlays msg =
    List ( Layer, View_ msg )


{-| -}
map : (a -> b) -> View gap a -> View gap b
map f =
    liftInternal (map_ f)


liftInternal : (View_ a -> View_ b) -> View g1 a -> View g2 b
liftInternal f (View view) =
    View <| f view


map_ : (a -> b) -> View_ a -> View_ b
map_ f view =
    case view of
        Row o ->
            Row
                { childGap = o.childGap
                , mixin = Mixin.map f o.mixin
                , layout = o.layout
                , overlays = List.map (Tuple.mapSecond (map_ f)) o.overlays
                , children = KeyableList.map (map_ f) o.children
                }

        Column o ->
            Column
                { childGap = o.childGap
                , mixin = Mixin.map f o.mixin
                , layout = o.layout
                , overlays = List.map (Tuple.mapSecond (map_ f)) o.overlays
                , children = KeyableList.map (map_ f) o.children
                }

        TextNode o ->
            TextNode
                { mixin = Mixin.map f o.mixin
                , layout = o.layout
                , overlays = List.map (Tuple.mapSecond (map_ f)) o.overlays
                , text = o.text
                }

        Scalable o ->
            Scalable
                { mixin = Mixin.map f o.mixin
                , layout = o.layout
                , overlays = List.map (Tuple.mapSecond (map_ f)) o.overlays
                , rate = o.rate
                }

        None ->
            None

        Boundary o ->
            Boundary
                { childGap = o.childGap
                , mixin = Mixin.map f o.mixin
                , layout = o.layout
                , overlays = List.map (Tuple.mapSecond (map_ f)) o.overlays
                , child = map_ f o.child
                }

        Rendered o ->
            Rendered
                { childGap = o.childGap
                , toHtml = Html.map f << o.toHtml
                }


type alias Layout =
    { maxWidth : MaxWidth
    , minWidth : MinWidth
    , maxHeight : MaxHeight
    , minHeight : MinHeight
    , wrap : Bool
    , verticalSpace : Space
    , horizontalSpace : Space
    , expandTo : Maybe Gap -- resulting Gap
    , lineHeight : Maybe Float
    , nodeName : String
    , contentBox : Bool
    , enforcePointerEvent : Bool
    }


defaultLayout : Layout
defaultLayout =
    { maxWidth = MaxWidthInfinite
    , minWidth = MinWidthContain
    , maxHeight = MaxHeightInfinite
    , minHeight = MinHeightContain
    , wrap = False
    , verticalSpace = SpaceBehind
    , horizontalSpace = SpaceBehind
    , expandTo = Nothing
    , lineHeight = Nothing
    , nodeName = "div"
    , contentBox = False
    , enforcePointerEvent = False
    }


{-| A primitive type that represents that there is no Gap

For custom gaps, see [Custom gaps](#custom-gaps).

-}
type NoGap
    = NoGap


emptyGap : Gap
emptyGap =
    { vertical = 0
    , horizontal = 0
    }



-- Internal Model


{-| -}
type Model model
    = Model (Model_ model)


type alias Model_ model =
    { userModel : model
    , offsetWidth : Dict TextNodeKey Int
    }


init : ( model, Cmd msg ) -> ( Model model, Cmd (Msg msg) )
init ( userModel, userCmd ) =
    ( Model
        { userModel = userModel
        , offsetWidth = Dict.empty
        }
    , Cmd.map UpdateUserMsg userCmd
    )


type alias TextNodeKey =
    List String



-- Internal Message


{-| -}
type Msg msg
    = UpdateUserMsg msg
    | UpdateWidth


update : (msg -> model -> ( model, Cmd msg )) -> Msg msg -> Model model -> ( Model model, Cmd (Msg msg) )
update userUpdate msg (Model model) =
    update_ userUpdate msg model
        |> (\( newModel, cmd ) ->
                ( Model newModel
                , cmd
                )
           )


update_ : (msg -> model -> ( model, Cmd msg )) -> Msg msg -> Model_ model -> ( Model_ model, Cmd (Msg msg) )
update_ userUpdate msg model =
    case msg of
        UpdateUserMsg userMsg ->
            userUpdate userMsg model.userModel
                |> (\( userModel, userCmd ) ->
                        ( { model
                            | userModel = userModel
                          }
                        , Cmd.map UpdateUserMsg userCmd
                        )
                   )

        UpdateWidth ->
            ( model
            , Cmd.none
            )



-- `Browser.*` alternatives


{-| Settings for rendering `View`.
-}
type Renderer
    = Renderer Renderer_


type alias Renderer_ =
    { baseSize : BaseSize
    , debug : Bool
    , parent : Parent
    , id : TextNodeKey
    }


type alias Parent =
    { direction : Direction
    , lineHeight : Float
    }


type Direction
    = Horizontal
    | Vertical


type BaseSize
    = BaseSize String Float


mapBaseSize : (Float -> Float) -> BaseSize -> BaseSize
mapBaseSize f (BaseSize unit a) =
    BaseSize unit (f a)


multipleBaseSize : Float -> BaseSize -> BaseSize
multipleBaseSize a =
    mapBaseSize (\s -> a * s)


renderBaseSize : BaseSize -> String
renderBaseSize (BaseSize unit a) =
    String.concat
        [ String.fromFloat a
        , unit
        ]


{-|

  - base size: 1rem
  - debug: disabled
  - lineHeight: 1.5

-}
defaultRenderer : Renderer
defaultRenderer =
    Renderer defaultRenderer_


{-| Set the base size in [rem](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem).

> Represents the font-size of the root element (typically <html>).

All gap sizes are determined relative to this value.

-}
setBaseSizeInRem : Float -> Renderer -> Renderer
setBaseSizeInRem a (Renderer renderer) =
    Renderer <|
        { renderer
            | baseSize = BaseSize "rem" a
        }


defaultRenderer_ : Renderer_
defaultRenderer_ =
    { baseSize = BaseSize "rem" 1
    , debug = False
    , parent =
        { direction = Vertical
        , lineHeight = 1.5
        }
    , id = []
    }


{-| Alternative to `Browser.sandbox`.

This applications is rendered as children of the column with following parameters:

  - max width, min width: 100%
  - max height, min height: 100%
  - vertical space: space behind
  - horizontal space: space behind

-}
sandbox :
    { init : model
    , view : model -> View NoGap msg
    , update : msg -> model -> model
    , renderer : Renderer
    }
    -> Program () (Model model) (Msg msg)
sandbox o =
    Browser.element
        { init = \() -> init ( o.init, Cmd.none )
        , view =
            \(Model model) ->
                Mixin.div
                    [ enforcedStyle
                    , flex
                    , flexDirection "column"
                    , style "position" "fixed"
                    , style "top" "0"
                    , style "bottom" "0"
                    , style "left" "0"
                    , style "right" "0"
                    ]
                    [ Mixin.lift (Html.node "style")
                        []
                        [ Html.text """
@keyframes neat-layout-dummy-animation {
  to {
    transform: scaleX(1);
  }
}
                        """
                        ]
                    , render o.renderer (o.view model.userModel)
                        |> Html.map UpdateUserMsg
                    ]
        , update =
            update
                (\userMsg userModel ->
                    o.update userMsg userModel
                        |> (\newUserModel ->
                                ( newUserModel
                                , Cmd.none
                                )
                           )
                )
        , subscriptions = \_ -> Sub.none
        }


{-| Alternative to `Browser.element`.

This applications is rendered as children of the column with following parameters:

  - max width, min width: 100%
  - max height, min height: 100%
  - vertical space: space behind
  - horizontal space: space behind

-}
element :
    { init : flags -> ( model, Cmd msg )
    , view : model -> View NoGap msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , renderer : model -> Renderer
    }
    -> Program flags (Model model) (Msg msg)
element o =
    Browser.element
        { init = o.init >> init
        , view =
            \(Model model) ->
                Mixin.div
                    [ enforcedStyle
                    , flex
                    , flexDirection "column"
                    , style "position" "fixed"
                    , style "top" "0"
                    , style "bottom" "0"
                    , style "left" "0"
                    , style "right" "0"
                    ]
                    [ Mixin.lift (Html.node "style")
                        []
                        [ Html.text """
@keyframes neat-layout-dummy-animation {
  to {
    transform: scaleX(1);
  }
}
                        """
                        ]
                    , render (o.renderer model.userModel) (o.view model.userModel)
                        |> Html.map UpdateUserMsg
                    ]
        , update = update o.update
        , subscriptions =
            \(Model model) ->
                o.subscriptions model.userModel
                    |> Sub.map UpdateUserMsg
        }


{-| Alternative to `Browser.document`.

This applications is rendered as children of the column with following parameters:

  - max width, min width: 100vw
  - max height: infinite
  - min height: 100vh
  - vertical space: space behind
  - horizontal space: space behind

-}
document :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , renderer : model -> Renderer
    }
    -> Program flags (Model model) (Msg msg)
document o =
    Browser.document
        { init = o.init >> init
        , view =
            \(Model model) ->
                let
                    view =
                        o.view model.userModel
                in
                { title = view.title
                , body =
                    [ Mixin.lift (Html.node "style")
                        []
                        [ Html.text """
@keyframes neat-layout-dummy-animation {
  to {
    transform: scaleX(1);
  }
}
                        """
                        ]
                    , Mixin.div
                        [ enforcedStyle
                        , flex
                        , flexDirection "column"
                        , style "position" "fixed"
                        , style "top" "0"
                        , style "bottom" "0"
                        , style "left" "0"
                        , style "right" "0"
                        , style "overflow" "auto"
                        ]
                        [ render (o.renderer model.userModel) view.body
                            |> Html.map UpdateUserMsg
                        ]
                    ]
                }
        , update = update o.update
        , subscriptions =
            \(Model model) ->
                o.subscriptions model.userModel
                    |> Sub.map UpdateUserMsg
        }


{-| Alternative to `Browser.Document`.
-}
type alias Document msg =
    { title : String
    , body : View NoGap msg
    }


{-| Alternative to `Browser.application`.

This applications is rendered as children of the column with following parameters:

  - max width, min width: 100vw
  - max height: infinite
  - min height: 100vh
  - vertical space: space behind
  - horizontal space: space behind

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
    -> Program flags (Model model) (Msg msg)
application o =
    Browser.application
        { init = \flags url key -> o.init flags url key |> init
        , view =
            \(Model model) ->
                let
                    view =
                        o.view model.userModel
                in
                { title = view.title
                , body =
                    [ Mixin.lift (Html.node "style")
                        []
                        [ Html.text """
@keyframes neat-layout-dummy-animation {
  to {
    transform: scaleX(1);
  }
}
                        """
                        ]
                    , Mixin.div
                        [ enforcedStyle
                        , flex
                        , flexDirection "column"
                        , style "position" "fixed"
                        , style "top" "0"
                        , style "bottom" "0"
                        , style "left" "0"
                        , style "right" "0"
                        ]
                        [ render (o.renderer model.userModel) view.body
                            |> Html.map UpdateUserMsg
                        ]
                    ]
                }
        , update = update o.update
        , subscriptions =
            \(Model model) ->
                o.subscriptions model.userModel
                    |> Sub.map UpdateUserMsg
        , onUrlRequest = o.onUrlRequest >> UpdateUserMsg
        , onUrlChange = o.onUrlChange >> UpdateUserMsg
        }



-- Primitive nodes


{-| Generates a view that displays a text.

The default sizes are as follows:

  - max-width: _fit_
  - min-width: _contain_
  - max-height: _fit_
  - min-height: _contain_
  - wrap: disabled
  - vertical space: behind
  - horizontal space: behind

If the argument string is empty, all the sizes are set to zero.
Note that `textBlock ""` is not equivalent to `none` nor `empty`.

-}
textBlock : String -> View NoGap msg
textBlock str =
    TextNode
        { mixin = Mixin.none
        , layout =
            { defaultLayout
                | maxWidth = MaxWidthFit
                , maxHeight = MaxHeightFit
            }
        , overlays = []
        , text = str
        }
        |> View


{-| Generates no HTML nodes.
This is useful for handling elements that disappear under certain conditions.
-}
none : View g a
none =
    View None


{-| Generates an HTML node without text nodes.
Takes HTML node name.

The default sizes are as follows:

  - max-width: _infinite_
  - min-width: _contain_
  - max-height: _infinite_
  - min-height: _contain_
  - wrap: disabled
  - vertical space: behind
  - horizontal space: behind

-}
empty : View NoGap a
empty =
    TextNode
        { mixin = Mixin.none
        , layout = defaultLayout
        , overlays = []
        , text = ""
        }
        |> View
        |> setMaxWidthInfinite
        |> setMaxHeightInfinite


{-| Same as `empty` but scales in a fixed aspect ratio.
Take `(height) / (width)` value as an argument.

It is useful for displaying images in a fixed aspect ratio by combinating with CSS "background-image" property.

  - max-width: _infinite_
  - min-width: _contain_
  - max-height: _infinite_ (unchangeable)
  - min-height: _contain_ (unchangeable)
  - wrap: disabled
  - vertical space: behind
  - horizontal space: behind

The [box-sizing](https://developer.mozilla.org/en-US/docs/Web/CSS/box-sizing) of this view is enforced to be `content-box`.
Also, this view ignores the height specification.

-}
scalableBlock : Float -> View NoGap msg
scalableBlock ratio =
    Scalable
        { mixin = Mixin.none
        , layout = defaultLayout
        , overlays = []
        , rate = ratio
        }
        |> View
        |> setMaxWidthInfinite



-- Handle conditions


{-| Insert a view only when a condition is met.
-}
when : Bool -> View gap msg -> View gap msg
when p v =
    if p then
        v

    else
        none


{-| Insert a view unless a condition is met.
-}
unless : Bool -> View gap msg -> View gap msg
unless p =
    when <| not p



-- Custom gaps


{-| Information about your custom gaps.

  - horizontal : horizontal gap relative to _base size_
  - vertical : vertical gap relative to _base size_

e.g., If the _base size_ is `2rem`, `IsGap { horizontal = 1.2, vertical = 2 }` becomes gap with `"2.4rem"` horizontally and `"4rem"` vertically.

-}
type IsGap p
    = IsGap Gap


{-| -}
type alias Gap =
    { horizontal : Float
    , vertical : Float
    }


subtractGap : Gap -> Gap -> Gap
subtractGap g2 g1 =
    { horizontal = g1.horizontal - g2.horizontal
    , vertical = g1.vertical - g2.vertical
    }



-- Setter


{-| Set `Mixin` on views with no gaps.
-}
setMixin : Mixin msg -> View NoGap msg -> View NoGap msg
setMixin mixin =
    liftInternal (setMixin_ mixin)


setMixin_ : Mixin msg -> View_ msg -> View_ msg
setMixin_ new view =
    case view of
        Row o ->
            Row { o | mixin = Mixin.batch [ o.mixin, new ] }

        Column o ->
            Column { o | mixin = Mixin.batch [ o.mixin, new ] }

        TextNode o ->
            TextNode { o | mixin = Mixin.batch [ o.mixin, new ] }

        Scalable o ->
            Scalable { o | mixin = Mixin.batch [ o.mixin, new ] }

        None ->
            None

        Boundary o ->
            Boundary { o | mixin = Mixin.batch [ o.mixin, new ] }

        Rendered o ->
            Rendered o


{-| Same as `setMixin` but takes list of `Mixin`s.
-}
setMixins : List (Mixin msg) -> View NoGap msg -> View NoGap msg
setMixins ls =
    setMixin <| Mixin.batch ls


{-| Set `Attribute` on views without gaps.
-}
setAttribute : Attribute msg -> View NoGap msg -> View NoGap msg
setAttribute attr =
    setMixin <| Mixin.fromAttribute attr


{-| Same as `setAttribute` but takes list of `Attribute`s.
-}
setAttributes : List (Attribute msg) -> View NoGap msg -> View NoGap msg
setAttributes attrs =
    setMixin <| Mixin.fromAttributes attrs


{-| Set "role" value for WAI-ARIA.
-}
setRole : String -> View g msg -> View g msg
setRole v =
    liftInternal (setMixin_ <| Mixin.attribute "role" v)


{-| Set "aria-\*" value for WAI-ARIA.

e.g., `setAria "required" "true"` stands for "aria-required" is "true".

-}
setAria : String -> String -> View g msg -> View g msg
setAria name v =
    liftInternal (setMixin_ <| Mixin.attribute ("aria-" ++ name) v)


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



-- Size
-- -- line height


{-| Set [line-height](https://developer.mozilla.org/en-US/docs/Web/CSS/line-height).

> Use a minimum value of 1.5 for line-height for main paragraph content. This will help people experiencing low vision conditions, as well as people with cognitive concerns such as Dyslexia.

This line height is inherited to child nodes.

-}
setLineHeight : Float -> View g msg -> View g msg
setLineHeight lineHeight =
    modifyLayout <|
        \layout ->
            { layout
                | lineHeight = Just lineHeight
            }



-- -- minimum width


{-| -}
type MinWidth
    = MinWidthContain
    | MinWidthInBs Float
    | MinWidthInUnit String Float


{-| Set _minimum width_ to the minimum width at which the view does not overflow.
-}
setMinWidthContain : View g msg -> View g msg
setMinWidthContain =
    setMinWidth MinWidthContain


{-| Set the minimum width as a percentage of the _base size_.
e.g., `setMinWidthInBs 100` set the _minimum width_ the same length as the _base size_.

The value is valid only if the container does not overflow.

-}
setMinWidthInBs : Float -> View g msg -> View g msg
setMinWidthInBs =
    setMinWidth << MinWidthInBs


{-| Set the minimum width in [em](https://developer.mozilla.org/en-US/docs/Web/CSS/length#em).

> Represents the calculated font-size of the element.

The value is valid only if the container does not overflow.

-}
setMinWidthInEm : Float -> View g msg -> View g msg
setMinWidthInEm =
    setMinWidth << MinWidthInUnit "em"


{-| Set the minimum width in [rem](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem).

> Represents the font-size of the root element (typically <html>).

The value is valid only if the container does not overflow.

-}
setMinWidthInRem : Float -> View g msg -> View g msg
setMinWidthInRem =
    setMinWidth << MinWidthInUnit "rem"


setMinWidth : MinWidth -> View g msg -> View g msg
setMinWidth length =
    modifyLayout <|
        \layout ->
            { layout
                | minWidth = length
            }



-- -- minimum height


{-| -}
type MinHeight
    = MinHeightContain
    | MinHeightInBs Float
    | MinHeightInUnit String Float


{-| Set _minimum height_ to the minimum height at which the view does not overflow.
-}
setMinHeightContain : View g msg -> View g msg
setMinHeightContain =
    setMinHeight MinHeightContain


{-| Set the minimum height as a percentage of the _base size_.
e.g., `setMinHeightInBs 100` set the _minimum height_ the same length as the _base size_.

The value is valid only if the container does not overflow.

-}
setMinHeightInBs : Float -> View g msg -> View g msg
setMinHeightInBs =
    setMinHeight << MinHeightInBs


{-| Set the minimum height in [em](https://developer.mozilla.org/en-US/docs/Web/CSS/length#em).

> Represents the calculated font-size of the element.

The value is valid only if the container does not overflow.

-}
setMinHeightInEm : Float -> View g msg -> View g msg
setMinHeightInEm =
    setMinHeight << MinHeightInUnit "em"


{-| Set the minimum height in [rem](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem).

> Represents the font-size of the root element (typically <html>).

The value is valid only if the container does not overflow.

-}
setMinHeightInRem : Float -> View g msg -> View g msg
setMinHeightInRem =
    setMinHeight << MinHeightInUnit "rem"


setMinHeight : MinHeight -> View g msg -> View g msg
setMinHeight length =
    modifyLayout <|
        \layout ->
            { layout
                | minHeight = length
            }



-- -- maximum width


{-| -}
type MaxWidth
    = MaxWidthInfinite
    | MaxWidthFit
    | MaxWidthInBs Float
    | MaxWidthInUnit String Float


{-| Set the maximum width to _infinite_.

A view with this function applied will be stretched horizontally as much as possible.

-}
setMaxWidthInfinite : View g msg -> View g msg
setMaxWidthInfinite =
    setMaxWidth MaxWidthInfinite


{-| Set the maximum width to _fit_.

If this is applied, the maximum width of all child views are set to _fit_.

-}
setMaxWidthFit : View g msg -> View g msg
setMaxWidthFit =
    setMaxWidth MaxWidthFit


{-| Set the maximum width as a percentage of the _base size_.
e.g., `setMaxWidthInBs 100` set the _maximum width_ the same length as the _base size_.

The value is valid only if the container does not overflow.

-}
setMaxWidthInBs : Float -> View g msg -> View g msg
setMaxWidthInBs =
    setMaxWidth << MaxWidthInBs


{-| Set the maximum width in [em](https://developer.mozilla.org/en-US/docs/Web/CSS/length#em).

> Represents the calculated font-size of the element.

The value is valid only if the container does not overflow.

-}
setMaxWidthInEm : Float -> View g msg -> View g msg
setMaxWidthInEm =
    setMaxWidth << MaxWidthInUnit "em"


{-| Set the maximum width in [rem](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem).

> Represents the font-size of the root element (typically <html>).

The value is valid only if the container does not overflow.

-}
setMaxWidthInRem : Float -> View g msg -> View g msg
setMaxWidthInRem =
    setMaxWidth << MaxWidthInUnit "rem"


setMaxWidth : MaxWidth -> View g msg -> View g msg
setMaxWidth length =
    modifyLayout <|
        \layout ->
            { layout
                | maxWidth = length
            }



-- -- maximum height


{-| -}
type MaxHeight
    = MaxHeightInfinite
    | MaxHeightFit
    | MaxHeightInBs Float
    | MaxHeightInUnit String Float


{-| Set the maximum height to _infinite_.

A view with this function applied will be stretched horizontally as much as possible.

-}
setMaxHeightInfinite : View g msg -> View g msg
setMaxHeightInfinite =
    setMaxHeight MaxHeightInfinite


{-| Set the maximum height to _fit_.

If this is applied, the maximum height of all child views are set to _fit_.

-}
setMaxHeightFit : View g msg -> View g msg
setMaxHeightFit =
    setMaxHeight MaxHeightFit


{-| Set the maximum height as a percentage of the _base size_.
e.g., `setMaxHeightInBs 100` set the _maximum height_ the same length as the _base size_.

The value is valid only if the container does not overflow.

-}
setMaxHeightInBs : Float -> View g msg -> View g msg
setMaxHeightInBs =
    setMaxHeight << MaxHeightInBs


{-| Set the maximum height in [em](https://developer.mozilla.org/en-US/docs/Web/CSS/length#em).

> Represents the calculated font-size of the element.

The value is valid only if the container does not overflow.

-}
setMaxHeightInEm : Float -> View g msg -> View g msg
setMaxHeightInEm =
    setMaxHeight << MaxHeightInUnit "em"


{-| Set the maximum height in [rem](https://developer.mozilla.org/en-US/docs/Web/CSS/length#rem).

> Represents the font-size of the root element (typically <html>).

The value is valid only if the container does not overflow.

-}
setMaxHeightInRem : Float -> View g msg -> View g msg
setMaxHeightInRem =
    setMaxHeight << MaxHeightInUnit "rem"


setMaxHeight : MaxHeight -> View g msg -> View g msg
setMaxHeight length =
    modifyLayout <|
        \layout ->
            { layout
                | maxHeight = length
            }


{-| Expand the gap.
Both the vertical and horizontal length of new gap is supposed to be greater than or equal to original ones.
-}
setGap : IsGap new -> View old msg -> View new msg
setGap (IsGap g) =
    modifyLayout <|
        \layout ->
            { layout
                | expandTo = Just g
            }


modifyLayout : (Layout -> Layout) -> View g1 msg -> View g2 msg
modifyLayout f =
    liftInternal (modifyLayout_ f)


modifyLayout_ : (Layout -> Layout) -> View_ msg -> View_ msg
modifyLayout_ f view =
    case view of
        Row o ->
            Row { o | layout = f o.layout }

        Column o ->
            Column { o | layout = f o.layout }

        TextNode o ->
            TextNode { o | layout = f o.layout }

        Scalable o ->
            Scalable { o | layout = f o.layout }

        None ->
            None

        Boundary o ->
            Boundary { o | layout = f o.layout }

        Rendered o ->
            Rendered o



-- Wrap style


{-| Set the contents of the view should not be wrapped.
-}
disableWrap : View g msg -> View g msg
disableWrap =
    setWrap False


{-| Set the contents of the view should be wrapped.
-}
enableWrap : View g msg -> View g msg
enableWrap =
    setWrap True


setWrap : Bool -> View g msg -> View g msg
setWrap p =
    modifyLayout <|
        \layout ->
            { layout
                | wrap = p
            }



-- Spaces


{-| It has no effect on _fit_ views.
-}
type Space
    = SpaceBehind
    | SpaceForward
    | SpaceBoth
    | SpaceAround
    | SpaceBetween


{-| -}
setHorizontalSpaceBehind : View gap msg -> View gap msg
setHorizontalSpaceBehind =
    modifyLayout <|
        \layout ->
            { layout
                | horizontalSpace = SpaceBehind
            }


{-| -}
setHorizontalSpaceForward : View gap msg -> View gap msg
setHorizontalSpaceForward =
    modifyLayout <|
        \layout ->
            { layout
                | horizontalSpace = SpaceForward
            }


{-| -}
setHorizontalSpaceBoth : View gap msg -> View gap msg
setHorizontalSpaceBoth =
    modifyLayout <|
        \layout ->
            { layout
                | horizontalSpace = SpaceBoth
            }


{-| -}
setHorizontalSpaceAround : View gap msg -> View gap msg
setHorizontalSpaceAround =
    modifyLayout <|
        \layout ->
            { layout
                | horizontalSpace = SpaceAround
            }


{-| -}
setHorizontalSpaceBetween : View gap msg -> View gap msg
setHorizontalSpaceBetween =
    modifyLayout <|
        \layout ->
            { layout
                | horizontalSpace = SpaceBetween
            }


{-| -}
setVerticalSpaceBehind : View gap msg -> View gap msg
setVerticalSpaceBehind =
    modifyLayout <|
        \layout ->
            { layout
                | verticalSpace = SpaceBehind
            }


{-| -}
setVerticalSpaceForward : View gap msg -> View gap msg
setVerticalSpaceForward =
    modifyLayout <|
        \layout ->
            { layout
                | verticalSpace = SpaceForward
            }


{-| -}
setVerticalSpaceBoth : View gap msg -> View gap msg
setVerticalSpaceBoth =
    modifyLayout <|
        \layout ->
            { layout
                | verticalSpace = SpaceBoth
            }


{-| -}
setVerticalSpaceAround : View gap msg -> View gap msg
setVerticalSpaceAround =
    modifyLayout <|
        \layout ->
            { layout
                | verticalSpace = SpaceAround
            }


{-| -}
setVerticalSpaceBetween : View gap msg -> View gap msg
setVerticalSpaceBetween =
    modifyLayout <|
        \layout ->
            { layout
                | verticalSpace = SpaceBetween
            }



-- Rows and Columns


{-| Align children horizontally.

  - max-width: _infinite_
  - min-width: _contain_
  - max-height: _infinite_
  - min-height: _contain_
  - wrap: disabled
  - vertical space: behind
  - horizontal space: behind
  - vertical overflow: disabled
  - horizontal overflow: disabled

-}
row : List (View gap msg) -> View gap msg
row children_ =
    let
        children =
            children_
                |> List.filterMap
                    (\(View c) ->
                        case c of
                            None ->
                                Nothing

                            _ ->
                                Just c
                    )
    in
    View <|
        case List.head children of
            Nothing ->
                None

            Just v ->
                row_ v (KeyableList.fromList children)


{-| Works just like `row`, but you add a unique identifier to each child node. You want this when you have a list of nodes that is changing: adding nodes, removing nodes, etc. In these cases, the unique identifiers help make the DOM modifications more efficient.
-}
keyedRow : List ( String, View gap msg ) -> View gap msg
keyedRow children_ =
    let
        children =
            children_
                |> List.filterMap
                    (\( str, View c ) ->
                        case c of
                            None ->
                                Nothing

                            _ ->
                                Just ( str, c )
                    )
    in
    View <|
        case Maybe.map Tuple.second (List.head children) of
            Nothing ->
                None

            Just v ->
                row_ v (KeyableList.fromKeyedList children)


{-| Alias for `List.singleton >> row`.

It is usefull for changing main axis of the view.

-}
setRow : View gap msg -> View gap msg
setRow =
    List.singleton >> row


row_ : View_ msg -> KeyableList (View_ msg) -> View_ msg
row_ head =
    let
        helper gap children =
            Row
                { childGap = gap
                , mixin = Mixin.none
                , layout = defaultLayout
                , overlays = []
                , children = children
                }
    in
    case head of
        Row { childGap, layout } ->
            helper <| Maybe.withDefault childGap layout.expandTo

        Column { childGap, layout } ->
            helper <| Maybe.withDefault childGap layout.expandTo

        TextNode { layout } ->
            helper <| Maybe.withDefault emptyGap layout.expandTo

        Scalable { layout } ->
            helper <| Maybe.withDefault emptyGap layout.expandTo

        -- Unreachable
        None ->
            \_ -> None

        Boundary { childGap, layout } ->
            helper <| Maybe.withDefault childGap layout.expandTo

        -- Unreachable
        Rendered { childGap } ->
            helper <| childGap


{-| Align children horizontally.

  - max-width: _infinite_
  - min-width: _contain_
  - max-height: _infinite_
  - min-height: _contain_
  - wrap: disabled
  - vertical space: behind
  - horizontal space: behind
  - vertical overflow: disabled
  - horizontal overflow: disabled

-}
column : List (View p msg) -> View p msg
column children_ =
    let
        children =
            children_
                |> List.filterMap
                    (\(View c) ->
                        case c of
                            None ->
                                Nothing

                            _ ->
                                Just c
                    )
    in
    View <|
        case List.head children of
            Nothing ->
                None

            Just v ->
                column_ v (KeyableList.fromList children)


{-| Works just like `column`, but you add a unique identifier to each child node. You want this when you have a list of nodes that is changing: adding nodes, removing nodes, etc. In these cases, the unique identifiers help make the DOM modifications more efficient.
-}
keyedColumn : List ( String, View gap msg ) -> View gap msg
keyedColumn children_ =
    let
        children =
            children_
                |> List.filterMap
                    (\( str, View c ) ->
                        case c of
                            None ->
                                Nothing

                            _ ->
                                Just ( str, c )
                    )
    in
    View <|
        case Maybe.map Tuple.second (List.head children) of
            Nothing ->
                None

            Just v ->
                column_ v (KeyableList.fromKeyedList children)


{-| Alias for `List.singleton >> column`.

It is usefull for changing main axis of the view.

-}
setColumn : View gap msg -> View gap msg
setColumn =
    List.singleton >> column


column_ : View_ msg -> KeyableList (View_ msg) -> View_ msg
column_ head =
    let
        helper gap children =
            Column
                { childGap = gap
                , mixin = Mixin.none
                , layout = defaultLayout
                , overlays = []
                , children = children
                }
    in
    case head of
        Row { childGap, layout } ->
            helper <| Maybe.withDefault childGap layout.expandTo

        Column { childGap, layout } ->
            helper <| Maybe.withDefault childGap layout.expandTo

        TextNode { layout } ->
            helper <| Maybe.withDefault emptyGap layout.expandTo

        Scalable { layout } ->
            helper <| Maybe.withDefault emptyGap layout.expandTo

        -- Unreachable
        None ->
            \_ -> None

        Boundary { childGap, layout } ->
            helper <| Maybe.withDefault childGap layout.expandTo

        -- Unreachable
        Rendered { childGap } ->
            helper childGap



-- Overlay


{-| Put overlay layer to a view.
This layer is never the target of pointer events; however
pointer events may target its descendant views if those views are converted into `Virtual msg` with `toLayerView` function.
-}
putLayer : ( Layer, View NoGap (InLayer msg) ) -> View NoGap msg -> View NoGap msg
putLayer ( area, overlay_ ) =
    case map (\(InLayer a) -> a) overlay_ of
        View None ->
            identity

        View overlay ->
            modifyOverlays <|
                \overlays -> ( area, overlay ) :: overlays


modifyOverlays : (Overlays msg -> Overlays msg) -> View gap msg -> View gap msg
modifyOverlays f =
    liftInternal (modifyOverlays_ f)


modifyOverlays_ : (Overlays msg -> Overlays msg) -> View_ msg -> View_ msg
modifyOverlays_ f view =
    case view of
        Row o ->
            Row { o | overlays = f o.overlays }

        Column o ->
            Column { o | overlays = f o.overlays }

        TextNode o ->
            TextNode { o | overlays = f o.overlays }

        Scalable o ->
            Scalable { o | overlays = f o.overlays }

        None ->
            None

        Boundary o ->
            Boundary { o | overlays = f o.overlays }

        Rendered o ->
            Rendered o


{-| -}
type InLayer msg
    = InLayer msg


{-| -}
toLayerView : View gap msg -> View gap (InLayer msg)
toLayerView view =
    map InLayer view
        |> modifyLayout
            (\layout ->
                { layout
                    | enforcePointerEvent = True
                }
            )


{-| Set the position of each edge of the overlay layer as a percentage of the base view.
-}
type alias Layer =
    { top : Float
    , bottom : Float
    , left : Float
    , right : Float
    , priority : Maybe Int
    }


{-|

    defaultLayer
    --> { top = 0
    --> , bottom = 0
    --> , left = 0
    --> , right = 0
    --> , priority = Nothing
    --> }

-}
defaultLayer : Layer
defaultLayer =
    { top = 0
    , bottom = 0
    , left = 0
    , right = 0
    , priority = Nothing
    }



-- Boundary


{-| Wrap a view with boundary without gap.
This is the only way to reset view gaps.

  - max-width: _infinite_
  - min-width: _contain_
  - max-height: _infinite_
  - min-height: _contain_
  - wrap: disabled
  - vertical space: behind
  - horizontal space: behind
  - vertical overflow: disabled
  - horizontal overflow: disabled

-}
setBoundary : View g msg -> View NoGap msg
setBoundary =
    liftInternal setBoundary_


setBoundary_ : View_ msg -> View_ msg
setBoundary_ view =
    let
        helper gap =
            Boundary
                { childGap = gap
                , mixin = Mixin.none
                , layout = defaultLayout
                , overlays = []
                , child = view
                }
    in
    case view of
        Row { childGap, layout } ->
            helper <| Maybe.withDefault childGap layout.expandTo

        Column { childGap, layout } ->
            helper <| Maybe.withDefault childGap layout.expandTo

        TextNode { layout } ->
            helper <| Maybe.withDefault emptyGap layout.expandTo

        Scalable { layout } ->
            helper <| Maybe.withDefault emptyGap layout.expandTo

        None ->
            None

        Boundary { childGap, layout } ->
            helper <| Maybe.withDefault childGap layout.expandTo

        Rendered _ ->
            helper emptyGap



-- Low level function for HTML


{-| -}
setNodeName : String -> View gap msg -> View gap msg
setNodeName str =
    modifyLayout <|
        \layout ->
            { layout
                | nodeName = str
            }



-- Temporary HTML


{-| Convert to Temporary HTML.
It is for `Html.Lazy.lazyN` functions.
-}
toTemporaryHtml : View NoGap msg -> (Renderer -> Html msg)
toTemporaryHtml v renderer =
    render renderer v


{-| Similar to `setBoundary`, but for temporary HTML.
-}
setBoundaryToTemporaryHtml : (Renderer -> Html msg) -> View NoGap msg
setBoundaryToTemporaryHtml f =
    Rendered
        { childGap = emptyGap
        , toHtml = f << Renderer
        }
        |> View
        |> setBoundary



-- Render


render : Renderer -> View NoGap a -> Html a
render (Renderer renderer) (View v) =
    render_ v renderer


render_ : View_ a -> Renderer_ -> Html a
render_ view renderer =
    case view of
        Row o ->
            rowNode renderer o

        Column o ->
            columnNode renderer o

        TextNode o ->
            textNode renderer o

        Scalable o ->
            scalableNode renderer o

        None ->
            Html.text ""

        Boundary o ->
            boundaryNode renderer o

        Rendered o ->
            o.toHtml renderer


textNode : Renderer_ -> { mixin : Mixin msg, layout : Layout, overlays : Overlays msg, text : String } -> Html msg
textNode renderer { mixin, layout, overlays, text } =
    let
        lineHeight =
            Maybe.withDefault renderer.parent.lineHeight layout.lineHeight

        outerGap =
            Maybe.withDefault emptyGap layout.expandTo
    in
    Mixin.div
        [ enforcedStyle
        , flex
        , flexDirection "column"
        , style "padding" <|
            gapValue outerGap renderer.baseSize
        , case renderer.parent.direction of
            Horizontal ->
                Mixin.batch
                    [ flexGrow
                        (if layout.maxWidth == MaxWidthFit then
                            0

                         else
                            1
                        )
                        1
                    , maxWidthStyle outerGap.horizontal renderer.baseSize layout.maxWidth
                    ]

            Vertical ->
                Mixin.batch
                    [ flexGrow
                        (if layout.maxHeight == MaxHeightFit then
                            0

                         else
                            1
                        )
                        1
                    , maxHeightStyle outerGap.vertical renderer.baseSize layout.maxHeight
                    , Mixin.when (layout.maxWidth == MaxWidthFit) <|
                        alignStart
                    ]
        ]
        [ Mixin.lift (Html.node layout.nodeName)
            [ mixin
            , enforcedStyle
            , flex
            , flexDirection "row"
            , justifySpace layout.horizontalSpace
            , Mixin.when (layout.maxHeight /= MaxHeightInfinite)
                (alignSpace layout.verticalSpace)
            , sizeStyle renderer layout
            , flexGrow
                (if layout.maxHeight == MaxHeightFit then
                    0

                 else
                    1
                )
                (if layout.minHeight == MinHeightContain then
                    0

                 else
                    1
                )

            -- , flexWrap layout.wrap
            , Mixin.unless layout.wrap <|
                style "white-space" "nowrap"
            , style "line-height" "0"
            , positionStyle overlays
            , Mixin.class "neat-textNode"
            ]
            (croppedTextCore lineHeight layout.wrap text
                :: List.indexedMap
                    (\k ->
                        renderLayer
                            { renderer
                                | id = String.fromInt k :: "overlay" :: renderer.id
                            }
                    )
                    (List.reverse overlays)
            )
        ]


positionStyle : Overlays msg -> Mixin msg
positionStyle overlays =
    if overlays == [] then
        Mixin.none

    else
        style "position" "relative"


renderLayer : Renderer_ -> ( Layer, View_ msg ) -> Html msg
renderLayer renderer ( area, view ) =
    Mixin.div
        [ enforcedStyle
        , flex
        , flexDirection "column"
        , style "position" "absolute"
        , style "top" <| String.fromFloat area.top ++ "%"
        , style "bottom" <| String.fromFloat area.bottom ++ "%"
        , style "left" <| String.fromFloat area.left ++ "%"
        , style "right" <| String.fromFloat area.right ++ "%"
        , style "pointer-events" "none"
        , area.priority
            |> Maybe.map String.fromInt
            |> Maybe.withDefault "auto"
            |> style "z-index"
        ]
        [ render_ view renderer
        ]


croppedTextCore : Float -> Bool -> String -> Html msg
croppedTextCore lineHeight wrap text =
    case text of
        "" ->
            Html.text ""

        _ ->
            Mixin.span
                [ enforcedStyle
                , style "line-height" <| String.fromFloat lineHeight
                , style "margin" <|
                    String.concat
                        [ String.fromFloat <| (1 - lineHeight) / 2
                        , "em 0"
                        ]
                , Mixin.unless wrap <|
                    style "white-space" "nowrap"
                , style "font-style" "inherit"
                , style "font-variant-ligatures" "inherit"
                , style "font-variant-caps" "inherit"
                , style "font-variant-numeric" "inherit"
                , style "font-variant-east-asian" "inherit"
                , style "font-weight" "inherit"
                , style "font-stretch" "inherit"
                , style "font-size" "inherit"
                , style "font-family" "inherit"
                , style "text-decoration-line" "inherit"
                , style "text-decoration-color" "inherit"
                , style "text-decoration-style" "inherit"
                , style "text-decoration-thickness" "inherit"
                , style "text-decoration-skip" "inherit"
                , style "text-emphasis-position" "inherit"
                , style "-webkit-text-emphasis-position" "inherit"
                , style "text-emphasis-color" "inherit"
                , style "-webkit-text-emphasis-color" "inherit"
                , style "text-emphasis-style" "inherit"
                , style "-webkit-text-emphasis-style" "inherit"
                , style "text-shadow" "inherit"
                , style "text-underline-offset" "inherit"
                , style "text-underline-position" "inherit"
                , style "text-indent" "inherit"
                , style "-webkit-text-orientation" "inherit"
                , style "text-orientation" "inherit"
                , style "writing-mode" "inherit"
                , style "text-rendering" "inherit"
                , style "text-shadow" "inherit"
                , style "text-transform" "inherit"
                ]
                [ Html.text text
                ]


rowNode : Renderer_ -> { childGap : Gap, mixin : Mixin msg, layout : Layout, overlays : Overlays msg, children : KeyableList (View_ msg) } -> Html msg
rowNode renderer { childGap, mixin, layout, children, overlays } =
    let
        lineHeight =
            Maybe.withDefault renderer.parent.lineHeight layout.lineHeight

        outerGap =
            layout.expandTo
                |> Maybe.map (subtractGap childGap)
                |> Maybe.withDefault emptyGap

        attrs =
            (Mixin.toAttributes << Mixin.batch)
                [ mixin
                , enforcedStyle
                , flex
                , flexDirection "row"
                , justifySpace layout.horizontalSpace
                , Mixin.when (layout.maxHeight /= MaxHeightInfinite)
                    (alignSpace layout.verticalSpace)
                , sizeStyle renderer layout
                , flexGrow 1 1
                , flexWrap layout.wrap
                , case renderer.parent.direction of
                    Horizontal ->
                        Mixin.class "neat-row-core-in-row"

                    Vertical ->
                        Mixin.class "neat-row-core-in-column"
                , positionStyle overlays
                ]

        overlayChildren =
            List.indexedMap
                (\k ->
                    renderLayer
                        { renderer
                            | id = String.fromInt k :: "overlay" :: renderer.id
                        }
                )
                (List.reverse overlays)
    in
    Mixin.div
        [ enforcedStyle
        , flex
        , flexDirection "column"
        , style "padding" <|
            gapValue outerGap renderer.baseSize
        , case renderer.parent.direction of
            Horizontal ->
                Mixin.batch
                    [ flexGrow
                        (if layout.maxWidth == MaxWidthFit then
                            0

                         else
                            1
                        )
                        1
                    , maxWidthStyle outerGap.horizontal renderer.baseSize layout.maxWidth
                    ]

            Vertical ->
                Mixin.batch
                    [ flexGrow
                        (if layout.maxHeight == MaxHeightFit then
                            0

                         else
                            1
                        )
                        1
                    , maxHeightStyle outerGap.vertical renderer.baseSize layout.maxHeight
                    , Mixin.when (layout.maxWidth == MaxWidthFit) <|
                        alignStart
                    ]
        ]
        [ children
            |> KeyableList.identifiedMap
                (\id c ->
                    render_ c
                        { renderer
                            | parent =
                                { direction = Horizontal
                                , lineHeight = lineHeight
                                }
                            , id = id :: renderer.id
                        }
                )
            |> KeyableList.unwrap
                (\cs ->
                    Html.node layout.nodeName
                        attrs
                        (overlayChildren ++ cs)
                )
                (\cs ->
                    Keyed.node layout.nodeName
                        attrs
                        (List.indexedMap
                            (\n c ->
                                ( "I'm too fool to use this as a key. This is for neat-layout. " ++ String.fromInt n, c )
                            )
                            overlayChildren
                            ++ cs
                        )
                )
        ]


columnNode : Renderer_ -> { childGap : Gap, mixin : Mixin msg, layout : Layout, overlays : Overlays msg, children : KeyableList (View_ msg) } -> Html msg
columnNode renderer { childGap, mixin, layout, children, overlays } =
    let
        lineHeight =
            Maybe.withDefault renderer.parent.lineHeight layout.lineHeight

        outerGap =
            layout.expandTo
                |> Maybe.map (subtractGap childGap)
                |> Maybe.withDefault emptyGap

        attrs =
            (Mixin.toAttributes << Mixin.batch)
                [ mixin
                , enforcedStyle
                , flex
                , flexDirection "column"
                , justifySpace layout.verticalSpace
                , Mixin.when (layout.maxWidth /= MaxWidthInfinite)
                    (alignSpace layout.horizontalSpace)
                , sizeStyle renderer layout
                , flexGrow 1 1
                , flexWrap layout.wrap
                , case renderer.parent.direction of
                    Horizontal ->
                        Mixin.class "neat-column-core-in-row"

                    Vertical ->
                        Mixin.class "neat-column-core-in-column"
                , positionStyle overlays
                ]

        overlayChildren =
            List.map (renderLayer renderer) (List.reverse overlays)
    in
    Mixin.div
        [ enforcedStyle
        , flex
        , flexDirection "row"
        , style "padding" <|
            gapValue outerGap renderer.baseSize
        , case renderer.parent.direction of
            Horizontal ->
                Mixin.batch
                    [ flexGrow
                        (if layout.maxWidth == MaxWidthFit then
                            0

                         else
                            1
                        )
                        1
                    , maxWidthStyle outerGap.horizontal renderer.baseSize layout.maxWidth
                    , Mixin.when (layout.maxHeight == MaxHeightFit) <|
                        alignStart
                    ]

            Vertical ->
                Mixin.batch
                    [ flexGrow
                        (if layout.maxHeight == MaxHeightFit then
                            0

                         else
                            1
                        )
                        1
                    , maxHeightStyle outerGap.vertical renderer.baseSize layout.maxHeight
                    ]
        ]
        [ children
            |> KeyableList.identifiedMap
                (\id c ->
                    render_ c
                        { renderer
                            | parent =
                                { direction = Vertical
                                , lineHeight = lineHeight
                                }
                            , id = id :: renderer.id
                        }
                )
            |> KeyableList.unwrap
                (\cs ->
                    Html.node layout.nodeName
                        attrs
                        (overlayChildren ++ cs)
                )
                (\cs ->
                    Keyed.node layout.nodeName
                        attrs
                        (List.indexedMap
                            (\n c ->
                                ( "I'm too fool to use this as a key. This is for neat-layout. " ++ String.fromInt n, c )
                            )
                            overlayChildren
                            ++ cs
                        )
                )
        ]


boundaryNode : Renderer_ -> { childGap : Gap, mixin : Mixin msg, layout : Layout, overlays : Overlays msg, child : View_ msg } -> Html msg
boundaryNode renderer { childGap, mixin, layout, child, overlays } =
    let
        lineHeight =
            Maybe.withDefault renderer.parent.lineHeight layout.lineHeight

        outerGap =
            layout.expandTo
                |> Maybe.withDefault emptyGap
    in
    Mixin.div
        [ enforcedStyle
        , flex
        , flexDirection "column"
        , style "padding" <|
            gapValue outerGap renderer.baseSize
        , case renderer.parent.direction of
            Horizontal ->
                Mixin.batch
                    [ flexGrow
                        (if layout.maxWidth == MaxWidthFit then
                            0

                         else
                            1
                        )
                        1
                    , maxWidthStyle outerGap.horizontal renderer.baseSize layout.maxWidth
                    ]

            Vertical ->
                Mixin.batch
                    [ flexGrow
                        (if layout.maxHeight == MaxHeightFit then
                            0

                         else
                            1
                        )
                        1
                    , maxHeightStyle outerGap.vertical renderer.baseSize layout.maxHeight
                    , Mixin.when (layout.maxWidth == MaxWidthFit) <|
                        alignStart
                    ]
        ]
        [ Mixin.lift (Html.node layout.nodeName)
            [ mixin
            , enforcedStyle
            , flex
            , flexDirection "row"
            , justifySpace layout.horizontalSpace
            , Mixin.when (layout.maxHeight /= MaxHeightInfinite)
                (alignSpace layout.verticalSpace)
            , sizeStyle renderer layout
            , flexGrow
                (if layout.maxHeight == MaxHeightFit then
                    0

                 else
                    1
                )
                (if layout.minHeight == MinHeightContain then
                    0

                 else
                    1
                )
            , flexWrap layout.wrap
            , style "padding" <|
                gapValue childGap renderer.baseSize
            , Mixin.class "neat-boundary"
            , positionStyle overlays
            ]
            (render_ child
                { renderer
                    | parent =
                        { direction = Horizontal
                        , lineHeight = lineHeight
                        }
                }
                :: List.indexedMap
                    (\k ->
                        renderLayer
                            { renderer
                                | id = String.fromInt k :: "overlay" :: renderer.id
                            }
                    )
                    (List.reverse overlays)
            )
        ]


scalableNode : Renderer_ -> { mixin : Mixin msg, layout : Layout, overlays : Overlays msg, rate : Float } -> Html msg
scalableNode renderer { mixin, layout, overlays, rate } =
    let
        outerGap =
            Maybe.withDefault emptyGap layout.expandTo
    in
    Mixin.div
        [ enforcedStyle
        , flex
        , flexDirection "column"
        , style "padding" <|
            gapValue outerGap renderer.baseSize
        , case renderer.parent.direction of
            Horizontal ->
                Mixin.batch
                    [ flexGrow
                        (if layout.maxWidth == MaxWidthFit then
                            0

                         else
                            1
                        )
                        1
                    , maxWidthStyle outerGap.horizontal renderer.baseSize layout.maxWidth
                    ]

            Vertical ->
                Mixin.batch
                    [ flexGrow 0 1
                    , Mixin.when (layout.maxWidth == MaxWidthFit) <|
                        alignStart
                    ]
        ]
        [ Mixin.lift (Html.node layout.nodeName)
            [ mixin
            , enforcedStyle
            , flex
            , flexDirection "row"
            , justifySpace layout.horizontalSpace
            , Mixin.when (layout.maxHeight /= MaxHeightInfinite)
                (alignSpace layout.verticalSpace)
            , widthStyle renderer layout
            , flexGrow 0 0
            , style "line-height" "0"
            , Mixin.class "neat-scalableNode"
            , style "box-sizing" "content-box"
            , positionStyle overlays
            ]
            (Mixin.div
                [ style "padding-top" <|
                    String.concat
                        [ String.fromFloat <| rate * 100
                        , "%"
                        ]
                , style "width" "100%"
                ]
                []
                :: List.indexedMap
                    (\k ->
                        renderLayer
                            { renderer
                                | id = String.fromInt k :: "overlay" :: renderer.id
                            }
                    )
                    (List.reverse overlays)
            )
        ]



-- Helper functions for styling


gapValue : Gap -> BaseSize -> String
gapValue gap baseSize =
    String.concat
        [ multipleBaseSize (gap.vertical / 2) baseSize
            |> renderBaseSize
        , " "
        , multipleBaseSize (gap.horizontal / 2) baseSize
            |> renderBaseSize
        ]


widthStyle : Renderer_ -> Layout -> Mixin msg
widthStyle renderer layout =
    Mixin.batch
        [ maxWidthStyle 0 renderer.baseSize layout.maxWidth
        , minWidthStyle renderer.baseSize layout.minWidth
        ]


sizeStyle : Renderer_ -> Layout -> Mixin msg
sizeStyle renderer layout =
    Mixin.batch
        [ maxWidthStyle 0 renderer.baseSize layout.maxWidth
        , minWidthStyle renderer.baseSize layout.minWidth
        , maxHeightStyle 0 renderer.baseSize layout.maxHeight
        , minHeightStyle renderer.baseSize layout.minHeight
        ]


maxWidthStyle : Float -> BaseSize -> MaxWidth -> Mixin msg
maxWidthStyle pad baseSize maxWidth =
    case maxWidth of
        MaxWidthInBs a ->
            baseSize
                |> multipleBaseSize (a + pad)
                |> renderBaseSize
                |> style "max-width"

        MaxWidthInUnit unit a ->
            String.concat
                [ String.fromFloat a
                , unit
                ]
                |> (\size ->
                        if pad == 0 then
                            size

                        else
                            String.concat
                                [ "calc("
                                , size
                                , " + "
                                , baseSize
                                    |> multipleBaseSize pad
                                    |> renderBaseSize
                                , ")"
                                ]
                   )
                |> style "max-width"

        _ ->
            Mixin.none


minWidthStyle : BaseSize -> MinWidth -> Mixin msg
minWidthStyle baseSize minWidth =
    case minWidth of
        MinWidthInBs a ->
            baseSize
                |> multipleBaseSize a
                |> renderBaseSize
                |> style "min-width"

        MinWidthInUnit unit a ->
            style "min-width" <|
                String.concat
                    [ String.fromFloat a
                    , unit
                    ]

        _ ->
            Mixin.none


maxHeightStyle : Float -> BaseSize -> MaxHeight -> Mixin msg
maxHeightStyle pad baseSize maxHeight =
    case maxHeight of
        MaxHeightInBs a ->
            baseSize
                |> multipleBaseSize (a + pad)
                |> renderBaseSize
                |> style "max-height"

        MaxHeightInUnit unit a ->
            String.concat
                [ String.fromFloat a
                , unit
                ]
                |> (\size ->
                        if pad == 0 then
                            size

                        else
                            String.concat
                                [ "calc("
                                , size
                                , " + "
                                , baseSize
                                    |> multipleBaseSize pad
                                    |> renderBaseSize
                                , ")"
                                ]
                   )
                |> style "max-height"

        _ ->
            Mixin.none


minHeightStyle : BaseSize -> MinHeight -> Mixin msg
minHeightStyle baseSize minHeight =
    case minHeight of
        MinHeightInBs a ->
            baseSize
                |> multipleBaseSize a
                |> renderBaseSize
                |> style "min-height"

        MinHeightInUnit unit a ->
            style "min-height" <|
                String.concat
                    [ String.fromFloat a
                    , unit
                    ]

        _ ->
            Mixin.none


flexGrow : Float -> Float -> Mixin msg
flexGrow grow shrink =
    let
        value =
            String.join " "
                [ String.fromFloat grow
                , String.fromFloat shrink
                , "auto"
                ]
    in
    Mixin.batch
        [ style "-ms-flex" value
        , style "flex" value
        ]



{-
   debugStyle : Mixin msg
   debugStyle =
       Mixin.batch
           [ style "border-style" "dotted"
           , style "border-width" "3px"
           , style "border-color" "red"
           ]
-}


enforcedStyle : Mixin msg
enforcedStyle =
    Mixin.batch
        [ style "width" "auto"
        , style "height" "auto"
        , style "min-width" "initial"
        , style "max-width" "none"
        , style "min-height" "initial"
        , style "max-height" "none"
        , style "padding" "0"
        , style "margin" "0"
        , style "line-height" "inherit"
        , style "display" "inline-block"
        , style "white-space" "normal"
        , style "box-sizing" "border-box"
        , style "position" "static"
        , style "z-index" "auto"
        , flexWrap False
        , flexDirection "row"
        , flexGrow 0 1
        ]


flex : Mixin msg
flex =
    Mixin.batch
        [ style "display" "flex"
        , justifyStart
        , alignStretch
        ]


flexDirection : String -> Mixin msg
flexDirection dir =
    Mixin.batch
        [ style "-ms-flex-direction" dir
        , style "flex-direction" dir
        ]


flexWrap : Bool -> Mixin msg
flexWrap wrap =
    let
        wrapStr =
            if wrap then
                "wrap"

            else
                "nowrap"
    in
    Mixin.batch
        [ style "-ms-flex-wrap" wrapStr
        , style "flex-wrap" wrapStr
        ]


justifySpace : Space -> Mixin msg
justifySpace space =
    case space of
        SpaceBehind ->
            justifyStart

        SpaceForward ->
            justifyEnd

        SpaceBoth ->
            justifyCenter

        SpaceAround ->
            justifySpaceAround

        SpaceBetween ->
            justifySpaceBetween


justifyStart : Mixin msg
justifyStart =
    Mixin.batch
        [ style "-ms-flex-pack" "start"
        , style "justify-content" "flex-start"
        ]


justifyEnd : Mixin msg
justifyEnd =
    Mixin.batch
        [ style "-ms-flex-pack" "end"
        , style "justify-content" "flex-end"
        ]


justifyCenter : Mixin msg
justifyCenter =
    Mixin.batch
        [ style "-ms-flex-pack" "center"
        , style "justify-content" "center"
        ]


justifySpaceBetween : Mixin msg
justifySpaceBetween =
    Mixin.batch
        [ style "-ms-flex-pack" "justify"
        , style "justify-content" "space-between"
        ]


justifySpaceAround : Mixin msg
justifySpaceAround =
    Mixin.batch
        [ style "-ms-flex-pack" "distribute"
        , style "justify-content" "space-around"
        ]


alignSpace : Space -> Mixin msg
alignSpace space =
    case space of
        SpaceBehind ->
            alignStart

        SpaceForward ->
            alignEnd

        SpaceBoth ->
            alignCenter

        SpaceAround ->
            alignSpaceAround

        SpaceBetween ->
            alignSpaceBetween


alignStretch : Mixin msg
alignStretch =
    Mixin.batch
        [ style "-ms-flex-align" "stretch"
        , style "align-items" "stretch"
        ]


alignStart : Mixin msg
alignStart =
    Mixin.batch
        [ style "-webkit-box-align" "start"
        , style "-ms-flex-align" "start"
        , style "align-items" "flex-start"
        ]


alignEnd : Mixin msg
alignEnd =
    Mixin.batch
        [ style "-webkit-box-align" "end"
        , style "-ms-flex-align" "end"
        , style "align-items" "flex-end"
        ]


alignCenter : Mixin msg
alignCenter =
    Mixin.batch
        [ style "-webkit-box-align" "center"
        , style "-ms-flex-align" "center"
        , style "align-items" "center"
        ]


alignSpaceBetween : Mixin msg
alignSpaceBetween =
    alignStart


alignSpaceAround : Mixin msg
alignSpaceAround =
    alignCenter


style : String -> String -> Mixin msg
style k v =
    Mixin.fromAttribute <| Attributes.style k v
