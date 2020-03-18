module Main exposing (main)

import Browser
import Browser.Navigation
import Html exposing (Html, button, div, text)
import Html.Attributes as Attributes
import Html.Attributes.Classname exposing (classMixinWith)
import Html.Events.Extra as Events
import Html.Lazy as Html
import MenuItem exposing (MenuItem)
import Mixin exposing (Mixin)
import Neat exposing (NoGap, Protected, View, fromNoGap, setBoundary, setLayout, setMixin, setMixins)
import Neat.Layout as Layout
import Neat.Layout.Column as Column exposing (Column, defaultColumn)
import Neat.Layout.Row as Row exposing (defaultRow)
import Gap exposing (BodyGap, HeaderGap, IconGap, MenuGap, MessageGap, bodyGap, headerGap, iconGap, menuGap, messageGap)



-- App


main : Program () Model Msg
main =
    Neat.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
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
    Layout.rowWith
        { defaultRow
            | vertical = Row.Stretch
        }
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
        |> List.map (fromNoGap menuGap << menuItem)
        |> Layout.columnWith
            { defaultColumn
                | horizontal = Column.Stretch
            }
        |> setBoundary menuGap
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
            |> fromNoGap iconGap
        , Neat.textBlock (MenuItem.toLabel m)
            |> setMixins
                [ class "menuItem_label"
                ]
            |> fromNoGap iconGap
            |> setLayout Layout.fill
        ]
        |> setBoundary iconGap
        |> setClass "menuItem"



-- Body


body : Model -> View NoGap msg
body model =
    Layout.columnWith
        { defaultColumn
            | horizontal = Column.Stretch
        }
        [ bodyTitle
        , bodyContent model.messages
            |> setLayout Layout.fill
            |> setClass "scrollAreaY"
        ]


bodyTitle : View NoGap msg
bodyTitle =
    Neat.textBlock "Home"
        |> Neat.fromNoGap headerGap
        |> setBoundary headerGap
        |> setClass "bodyTitle"


bodyContent : List Message -> View NoGap msg
bodyContent messages =
    Column.optimized
        (String.fromInt << .id)
        (\m -> Html.lazy4 message_ m.icon m.name m.screenName)
        { defaultColumn
            | horizontal = Column.Stretch
        }
        messages
        |> setClass "bodyContent"


message_ : MessageIcon -> String -> String -> Column -> Html (Protected NoGap msg)
message_ icon name screenName =
    Column.toProtected <| message icon name screenName


message : MessageIcon -> String -> String -> View NoGap msg
message icon name screenName =
    Layout.row
        [ Neat.empty
            |> setClass "messageIcon"
            -- (Debug.log "This functions is only rendered at first" "messageIcon")
            |> setClass (messageIconClass icon)
            |> fromNoGap bodyGap
        , Layout.column
            [ Layout.row
                [ Neat.textBlock name
                    |> setClass "messageName"
                    |> fromNoGap messageGap
                , Neat.textBlock ("@" ++ screenName)
                    |> setClass "messageScreenName"
                    |> fromNoGap messageGap
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
                |> Neat.fromNoGap messageGap
            ]
            |> Neat.expand messageGap bodyGap
        ]
        |> setBoundary bodyGap
        |> setClass "message"



-- Right Pane


rightPane : View NoGap Msg
rightPane =
    Layout.columnWith
        { defaultColumn
            | horizontal = Column.Stretch
        }
        [ searchField
        ]
        |> setBoundary menuGap
        |> setClass "rightPane"
        |> setLayout Layout.fill


searchField : View MenuGap Msg
searchField =
    Layout.rowWith
        { defaultRow
            | vertical = Row.VCenter
        }
        [ Neat.empty
            |> setClass "searchIcon"
            |> fromNoGap iconGap
        , Neat.emptyNode Html.input
            |> setClass "searchInput"
            |> setType_ "text"
            |> setMixin
                (Mixin.fromAttribute <| Attributes.placeholder "Search Bleater")
            |> setMixin
                (Mixin.fromAttribute <| Events.onChange ChangeSearchText)
            |> fromNoGap iconGap
        ]
        |> setBoundary iconGap
        |> setClass "searchField"
        |> fromNoGap menuGap


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
