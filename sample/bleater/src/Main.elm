module Main exposing (main)

import Gap
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Attributes.Classname exposing (classMixinWith)
import Html.Events.Extra as Events
import Html.Lazy as Html
import MenuItem exposing (MenuItem)
import Mixin exposing (Mixin)
import Neat exposing (NoGap, Protected, Renderer, View, defaultRenderer, fromNoGap, setBoundary, setLayout, setMixin, setMixins)
import Neat.Layout as Layout
import Neat.Layout.Column as Column exposing (Column, defaultColumn)
import Neat.Layout.Row as Row exposing (defaultRow)



-- App


main : Program () Model Msg
main =
    Neat.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , renderer = \_ -> defaultRenderer
        }


type alias Model =
    { searchText : String
    , messages : List Message
    }


type alias Message =
    { icon : MessageIcon
    , name : String
    , screenName : String
    , id : Int
    }


type MessageIcon
    = MessageIcon1
    | MessageIcon2


messageIconClass : MessageIcon -> String
messageIconClass icon =
    case icon of
        MessageIcon1 ->
            "messageIcon-icon1"

        MessageIcon2 ->
            "messageIcon-icon2"


init : ( Model, Cmd Msg )
init =
    ( { searchText = ""
      , messages =
            [ message1 1
            , message1 2
            , message2 3
            , message1 4
            , message2 5
            , message2 6
            , message1 7
            , message2 8
            , message1 9
            ]
      }
    , Cmd.none
    )


message1 : Int -> Message
message1 n =
    { icon = MessageIcon1
    , name = "Sakura-chan 1st"
    , screenName = "sakura-chan1"
    , id = n
    }


message2 : Int -> Message
message2 n =
    { icon = MessageIcon2
    , name = "Sakura-chan 2nd"
    , screenName = "sakura-chan2"
    , id = n
    }


type Msg
    = ChangeSearchText String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSearchText str ->
            ( { model
                | searchText = str
              }
            , Cmd.none
            )


view : Model -> View NoGap Msg
view model =
    Layout.row
        [ menu
            |> setLayout Layout.noShrink
        , body model
        , rightPane
        ]
        |> setClass "center"
        |> alignHCenter
        |> setClass "wrapper"


alignHCenter : View NoGap msg -> View NoGap msg
alignHCenter v =
    Layout.rowWith
        { defaultRow
            | horizontal = Row.HCenter
        }
        [ v ]



-- Menu


menu : View NoGap msg
menu =
    MenuItem.enum
        |> List.map (fromNoGap Gap.menu << menuItem)
        |> Layout.column
        |> setBoundary Gap.menu
        |> setClass "menu"


menuItem : MenuItem -> View NoGap msg
menuItem m =
    Layout.rowWith
        { defaultRow
            | vertical = Row.Bottom
        }
        [ Neat.empty
            |> setMixins
                [ class <| MenuItem.toClassname m
                , class "menuItem_icon"
                ]
            |> fromNoGap Gap.icon
        , Neat.textBlock (MenuItem.toLabel m)
            |> setMixins
                [ class "menuItem_label"
                ]
            |> fromNoGap Gap.icon
            |> setLayout Layout.fill
        ]
        |> setBoundary Gap.icon
        |> setClass "menuItem"



-- Body


body : Model -> View NoGap msg
body model =
    Layout.column
        [ bodyTitle
        , bodyContent model.messages
            |> setLayout Layout.fill
            |> setClass "scrollAreaY"
        ]


bodyTitle : View NoGap msg
bodyTitle =
    Neat.textBlock "Home"
        |> Neat.fromNoGap Gap.header
        |> setBoundary Gap.header
        |> setClass "bodyTitle"


bodyContent : List Message -> View NoGap msg
bodyContent messages =
    Column.optimized
        (String.fromInt << .id)
        (\m -> Html.lazy5 message_ m.icon m.name m.screenName)
        defaultColumn
        messages
        |> setClass "bodyContent"


message_ : MessageIcon -> String -> String -> Column -> Renderer -> Html (Protected NoGap msg)
message_ icon name screenName =
    Column.toProtected <| message icon name screenName


message : MessageIcon -> String -> String -> View NoGap msg
message icon name screenName =
    Layout.row
        [ Neat.empty
            |> setClass "messageIcon"
            -- |> setClass (Debug.log "This functions is only rendered at first" "messageIcon")
            |> setClass (messageIconClass icon)
            |> fromNoGap Gap.body
        , Layout.column
            [ Layout.row
                [ Neat.textBlock name
                    |> setClass "messageName"
                    |> fromNoGap Gap.message
                , Neat.textBlock ("@" ++ screenName)
                    |> setClass "messageScreenName"
                    |> fromNoGap Gap.message
                ]
            , Layout.column
                [ Neat.textBlock """
                    Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                    """
                , Neat.textBlock """
                    Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
                    """
                ]
                |> setClass "messageBody"
                |> Neat.fromNoGap Gap.message
            ]
            |> Neat.expand Gap.message Gap.body
        ]
        |> setBoundary Gap.body
        |> setClass "message"



-- Right Pane


rightPane : View NoGap Msg
rightPane =
    Layout.column
        [ searchField
        ]
        |> setBoundary Gap.menu
        |> setClass "rightPane"
        |> setLayout Layout.fill


searchField : View Gap.Menu Msg
searchField =
    Layout.rowWith
        { defaultRow
            | vertical = Row.VCenter
        }
        [ Neat.empty
            |> setClass "searchIcon"
            |> fromNoGap Gap.icon
        , Neat.emptyNode Html.input
            |> setClass "searchInput"
            |> setType_ "text"
            |> setMixin
                (Mixin.fromAttribute <| Attributes.placeholder "Search Bleater")
            |> setMixin
                (Mixin.fromAttribute <| Events.onChange ChangeSearchText)
            |> fromNoGap Gap.icon
        ]
        |> setBoundary Gap.icon
        |> setClass "searchField"
        |> fromNoGap Gap.menu


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Helper functions


class : String -> Mixin msg
class =
    classMixinWith <| \name -> "app__" ++ name


setClass : String -> View NoGap msg -> View NoGap msg
setClass =
    setMixin << class


setType_ : String -> View NoGap msg -> View NoGap msg
setType_ =
    setMixin << Mixin.fromAttribute << Attributes.type_
