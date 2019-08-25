module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Html.Attributes.Classname exposing (classMixinWith)
import Json.Decode as Json
import Mixin exposing (Mixin)
import Neat exposing (NoPadding, View, setMixin)
import Page exposing (Page)
import Url exposing (Url)
import Url.Builder
import Url.Parser as Url



-- App


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , currentPage : Int
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , currentPage =
            parseUrl url
      }
    , Cmd.none
    )


parseUrl : Url -> Int
parseUrl url =
    Url.parse parser url
        |> Maybe.withDefault 0


parser : Url.Parser (Int -> a) a
parser =
    Url.fragment <|
        \ma ->
            ma
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault 0



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | KeyDown String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | currentPage = parseUrl url }
            , Cmd.none
            )

        KeyDown key ->
            case key of
                "ArrowRight" ->
                    ( model
                    , pushPage model.key <|
                        min
                            (List.length Page.pages - 1)
                            (model.currentPage + 1)
                    )

                "ArrowLeft" ->
                    ( model
                    , pushPage model.key <|
                        max 0 (model.currentPage - 1)
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )


pushPage : Nav.Key -> Int -> Cmd Msg
pushPage key n =
    String.fromInt n
        |> Just
        |> Url.Builder.custom Url.Builder.Relative [] []
        |> Nav.pushUrl key


view : Model -> Browser.Document Msg
view model =
    { title = "に〜と のためのキマるスタイリング🐐"
    , body =
        Neat.div []
            (List.reverse <| List.indexedMap (slide model.currentPage) Page.pages)
            |> setClass "app"
            |> Neat.toPage
    }


slide : Int -> Int -> Page Msg -> View NoPadding Msg
slide curr n p =
    p
        |> setClass "slide"
        |> setClass (slideClass curr n)


slideClass : Int -> Int -> String
slideClass curr n =
    if curr > n then
        "slide-gone"

    else
        "slide-left"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown <|
        Json.andThen
            (\key -> Json.succeed (KeyDown key))
            (Json.field "key" Json.string)



-- Helper functions


class : String -> Mixin msg
class =
    classMixinWith <| \name -> "app__" ++ name


setClass : String -> View NoPadding msg -> View NoPadding msg
setClass =
    setMixin << class
