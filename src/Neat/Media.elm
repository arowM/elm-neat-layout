module Neat.Media exposing
    ( Media
    , element
    , Element
    , Msg
    , init
    , RootElement(..)
    , subscriptions
    , update
    )

{-| Helper module for responsive design.
The `Neat.Media` provides helper functions to handle width of the target element dynamically.


# Core

@docs Media
@docs element
@docs Element


# Functions for TEA

@docs Msg
@docs init
@docs RootElement
@docs subscriptions
@docs update

-}

import Browser.Dom as Dom exposing (Viewport)
import Browser.Events
import Task


{-| Initial state for TEA.

  - root: the target element you want to get its width

-}
init : { root : RootElement } -> ( Media, Cmd Msg )
init { root } =
    ( Media
        { root = root
        , element = Nothing
        }
    , updateRootWidth root
    )


{-| Internal Msg
-}
type Msg
    = ResizeWindow
    | ResizeRootWidth (Result Dom.Error Element)
    | ResizeWindowWidth Viewport


{-| Core data type.
-}
type Media
    = Media Model


type alias Model =
    { element : Maybe Element
    , root : RootElement
    }


{-| Reexport [`Browser.Dom.Element`](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#Element) for convenience.
-}
type alias Element =
    { scene :
        { width : Float
        , height : Float
        }
    , viewport :
        { x : Float
        , y : Float
        , width : Float
        , height : Float
        }
    , element :
        { x : Float
        , y : Float
        , width : Float
        , height : Float
        }
    }


{-| The target element you want to get its width.

  - Window: browser window
  - RootWithId: element specified by `id` attribute value

-}
type RootElement
    = Window
    | RootWithId String


{-| Take `Element` of the target element form `Media`.
-}
element : Media -> Maybe Element
element (Media m) =
    m.element


{-| -}
update : Msg -> Media -> ( Media, Cmd Msg )
update msg (Media model) =
    case update_ msg model of
        ( model_, msg_ ) ->
            ( Media model_, msg_ )


update_ : Msg -> Model -> ( Model, Cmd Msg )
update_ msg model =
    case msg of
        ResizeWindow ->
            ( model
            , updateRootWidth model.root
            )

        ResizeRootWidth relem ->
            case relem of
                Ok elem ->
                    ( { model
                        | element = Just elem
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model
                    , Cmd.none
                    )

        ResizeWindowWidth elem ->
            ( { model
                | element =
                    Just
                        { scene = elem.scene
                        , viewport = elem.viewport
                        , element = elem.viewport
                        }
              }
            , Cmd.none
            )


updateRootWidth : RootElement -> Cmd Msg
updateRootWidth root =
    case root of
        Window ->
            Task.perform ResizeWindowWidth <| Dom.getViewport

        RootWithId rid ->
            Task.attempt ResizeRootWidth <| Dom.getElement rid


{-| -}
subscriptions : Media -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize (\_ _ -> ResizeWindow)
        ]
